//! The picker interface and its implementations.

use std::io;

use crossterm::event::{self, Event};

use std::time::Duration;

use crate::candidate::Candidate;
use crate::confirm;
use crate::filter::filter;
use crate::options::PickerOptions;
use crate::selection::{Selection, SelectionType};
use crate::stream::CandidateStream;
use crate::terminal::TerminalGuard;
use crate::ui::{App, render};

/// Anything that can turn candidates into a selection.
pub trait Picker {
    /// Presents `candidates` and returns what was chosen.
    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>>;

    /// Presents candidates that are still arriving.
    ///
    /// The default collects them and falls back to [`Picker::pick`], which is
    /// right for any picker that cannot draw while it waits.
    fn pick_stream(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<Selection<Candidate>> {
        let candidates = stream.collect();
        self.pick(options, candidates)
    }

    /// Picks, and reports where the toggles ended up.
    ///
    /// The default leaves them as they were given, which is right for every
    /// picker that draws nothing. Every implementation must first honour
    /// [`exact_selection`], so a query that names a candidate outright gets
    /// the same answer whichever picker is in play.
    fn pick_options(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        if let Some(selection) = exact_selection(options, &candidates) {
            return Ok((selection, option_state(options)));
        }
        let selection = self.pick(options, candidates)?;
        Ok((selection, option_state(options)))
    }

    /// The same for a stream.
    fn pick_stream_options(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        let selection = self.pick_stream(options, stream)?;
        Ok((selection, option_state(options)))
    }

    /// Asks only about the toggles: no list, no query.
    ///
    /// `None` means the user cancelled. The default accepts them unchanged,
    /// so a picker that cannot draw runs with the defaults rather than
    /// failing — the same principle as `-1`.
    fn confirm(&mut self, options: &PickerOptions) -> io::Result<Option<Vec<bool>>> {
        Ok(Some(option_state(options)))
    }
}

/// The toggles as the caller set them.
fn option_state(options: &PickerOptions) -> Vec<bool> {
    options.options.iter().map(|option| option.on).collect()
}

/// The real picker: a terminal UI on stderr.
#[derive(Debug, Default)]
pub struct TuiPicker;

impl Picker for TuiPicker {
    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>> {
        Ok(self.pick_options(options, candidates)?.0)
    }

    fn pick_stream(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<Selection<Candidate>> {
        Ok(self.pick_stream_options(options, stream)?.0)
    }

    fn pick_options(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        if let Some(selection) = exact_selection(options, &candidates) {
            return Ok((selection, option_state(options)));
        }
        // `-1` still applies: with a unique match there is nothing to ask,
        // and the toggles stay as they were given.
        if let Some(selection) = short_circuit(options, &candidates) {
            return Ok((selection, option_state(options)));
        }

        let mut app = App::new(candidates, options.clone());
        let selection = run_loop(&mut app, options, None)?;
        Ok((selection, app.option_state()))
    }

    fn pick_stream_options(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        // The picker opens on an empty list and fills as the producer runs.
        let mut app = App::empty(options.clone());
        let selection = run_loop(&mut app, options, Some(stream))?;
        if matches!(selection, Selection::Canceled) {
            stream.cancel();
        }
        Ok((selection, app.option_state()))
    }

    /// Draws the toggles and waits for `Enter` or `Esc`.
    ///
    /// Without a terminal there is nothing to ask with, so the defaults
    /// stand: a command with options is runnable from a script.
    fn confirm(&mut self, options: &PickerOptions) -> io::Result<Option<Vec<bool>>> {
        if options.options.is_empty() {
            return Ok(Some(Vec::new()));
        }
        confirm::run(options)
    }
}

/// The draw/read loop, shared by the ready and streaming entry points.
///
/// With a stream, events are polled rather than waited on, so newly arrived
/// candidates are drawn even while the user types nothing.
fn run_loop(
    app: &mut App,
    options: &PickerOptions,
    mut stream: Option<&mut CandidateStream>,
) -> io::Result<Selection<Candidate>> {
    // Taken lazily. `-1` and streaming pull against each other: `-1` needs the
    // whole list to know a match is unique, and a script piping `nixon run`
    // with an unambiguous command has no terminal to take at all. So while
    // `-1` could still apply, nothing is drawn and no terminal is claimed;
    // the moment a second candidate matches it cannot apply, and the picker
    // opens — which for a command listing thousands of files is at once.
    let mut guard: Option<TerminalGuard> = None;
    // `-1` is decided once, on the query the picker opened with. Once the
    // user has typed, narrowing to a single row must not select it for them.
    let mut untouched = true;

    while !app.is_done() {
        let arrived = stream
            .as_mut()
            .map(|stream| (stream.drain(), stream.is_finished()));
        let streaming = match arrived {
            Some((candidates, finished)) => {
                for candidate in candidates {
                    // An exact answer needs no more of the stream.
                    if untouched && let Some(selection) = exact_match(options, &candidate) {
                        return Ok(selection);
                    }
                    app.push(candidate);
                }
                !finished
            }
            None => false,
        };

        // Let the background matcher make progress, then draw what it has.
        app.tick();

        if untouched && options.select_one && app.matched_count() <= 1 {
            if streaming {
                // Still arriving, and still possibly unique: wait it out.
                std::thread::sleep(Duration::from_millis(10));
                continue;
            }
            // The producer is done, but the matcher may not have caught up
            // with its last candidates; deciding on a partial count would
            // report "no matches" for a list that has one.
            app.tick_until_settled();
            if let Some(selection) = short_circuit_app(app) {
                return Ok(selection);
            }
        }

        let terminal = match guard {
            Some(ref mut guard) => guard.terminal(),
            None => guard.insert(TerminalGuard::new()?).terminal(),
        };
        terminal.draw(|frame| {
            app.set_height(list_height(frame.area().height, options));
            render(app, frame);
        })?;

        if streaming && !event::poll(Duration::from_millis(30))? {
            // Nothing typed; loop round to pick up more candidates.
            continue;
        }

        if let Event::Key(key) = event::read()?
            && key.kind == event::KeyEventKind::Press
        {
            untouched = false;
            app.handle(key);
        }
    }

    Ok(app.outcome.take().unwrap_or(Selection::Empty))
}

/// `-1` applied to what the matcher has settled on.
fn short_circuit_app(app: &App) -> Option<Selection<Candidate>> {
    match app.matched_count() {
        0 => Some(Selection::Empty),
        1 => app.current().map(|candidate| {
            Selection::selected(crate::selection::SelectionType::Default, vec![candidate])
        }),
        _ => None,
    }
}

/// Non-interactive matching, for `--list` and `| list`.
///
/// Returns every candidate that matched, in ranked order, so callers read
/// their `value` — the same field an interactive pick hands back. It never
/// touches the terminal and never returns [`Selection::Canceled`].
#[derive(Debug, Default)]
pub struct FilterPicker;

impl Picker for FilterPicker {
    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>> {
        let query = options.initial_query.as_deref().unwrap_or_default();
        let matched = filter(query, &candidates, options.matching);
        Ok(Selection::selected(
            crate::selection::SelectionType::Default,
            matched,
        ))
    }
}

/// The candidate `select_exact` settles the pick with, if there is one.
///
/// Part of the [`Picker`] contract: every implementation honours it, so the
/// answer does not depend on which picker is in play.
pub fn exact_selection(
    options: &PickerOptions,
    candidates: &[Candidate],
) -> Option<Selection<Candidate>> {
    candidates
        .iter()
        .find_map(|candidate| exact_match(options, candidate))
}

/// A candidate whose value is exactly the query.
///
/// Unlike `-1` this does not need the whole list: nothing later can be a
/// better answer than an exact one, so it settles the pick as soon as the
/// candidate is seen — which is what lets it work on a stream.
fn exact_match(options: &PickerOptions, candidate: &Candidate) -> Option<Selection<Candidate>> {
    if !options.select_exact {
        return None;
    }
    let query = options.initial_query.as_deref()?;
    (candidate.value == query)
        .then(|| Selection::selected(SelectionType::Default, vec![candidate.clone()]))
}

/// fzf's `-1`: a query matching exactly one row selects it without drawing.
fn short_circuit(
    options: &PickerOptions,
    candidates: &[Candidate],
) -> Option<Selection<Candidate>> {
    if !options.select_one {
        return None;
    }
    let query = options.initial_query.as_deref().unwrap_or_default();
    let matched = filter(query, candidates, options.matching);
    match matched.len() {
        0 => Some(Selection::Empty),
        1 => Some(Selection::selected(
            crate::selection::SelectionType::Default,
            matched,
        )),
        _ => None,
    }
}

/// Rows available to the list, after the query line and any header.
fn list_height(total: u16, options: &PickerOptions) -> usize {
    let chrome = if options.header.is_some() { 2 } else { 1 };
    usize::from(total).saturating_sub(chrome).max(1)
}

/// A picker that answers from a queue, for tests.
#[cfg(any(test, feature = "test-util"))]
#[derive(Debug, Default)]
pub struct ScriptedPicker {
    answers: std::collections::VecDeque<Selection<Candidate>>,
    /// Every `(options, candidates)` it was asked, in order.
    pub calls: Vec<(PickerOptions, Vec<Candidate>)>,
    /// Toggles to flip on the next pick, as a key press would.
    pub toggles: std::collections::VecDeque<Vec<usize>>,
    /// Whether `confirm` should cancel rather than accept.
    pub cancel_confirm: bool,
}

#[cfg(any(test, feature = "test-util"))]
impl ScriptedPicker {
    /// Builds a picker that returns `answers` in order.
    pub fn new(answers: Vec<Selection<Candidate>>) -> Self {
        Self {
            answers: answers.into(),
            calls: Vec::new(),
            toggles: std::collections::VecDeque::new(),
            cancel_confirm: false,
        }
    }

    /// Flips these toggles on the next pick, as `Alt-<n>` would.
    #[must_use]
    pub fn toggling(mut self, indices: &[usize]) -> Self {
        self.toggles.push_back(indices.to_vec());
        self
    }

    /// The state after applying whatever this pick was scripted to toggle.
    fn toggled(&mut self, options: &PickerOptions) -> Vec<bool> {
        let mut state = option_state(options);
        for index in self.toggles.pop_front().unwrap_or_default() {
            if let Some(on) = state.get_mut(index) {
                *on = !*on;
            }
        }
        state
    }

    /// Builds a picker that always selects the row at `index`.
    pub fn selecting(indices: &[usize]) -> SelectingPicker {
        SelectingPicker {
            indices: indices.to_vec(),
            calls: Vec::new(),
        }
    }
}

#[cfg(any(test, feature = "test-util"))]
impl Picker for ScriptedPicker {
    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>> {
        self.calls.push((options.clone(), candidates));
        Ok(self.answers.pop_front().unwrap_or(Selection::Empty))
    }

    fn pick_options(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        if let Some(selection) = exact_selection(options, &candidates) {
            return Ok((selection, option_state(options)));
        }
        let state = self.toggled(options);
        let selection = self.pick(options, candidates)?;
        Ok((selection, state))
    }

    fn pick_stream_options(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        let mut candidates = Vec::new();
        loop {
            for candidate in stream.drain() {
                // An exact answer needs no more of the stream, exactly as
                // in the real picker's loop.
                if let Some(selection) = exact_selection(options, std::slice::from_ref(&candidate))
                {
                    return Ok((selection, option_state(options)));
                }
                candidates.push(candidate);
            }
            if stream.is_finished() {
                break;
            }
            std::thread::yield_now();
        }

        let state = self.toggled(options);
        self.calls.push((options.clone(), candidates));
        Ok((self.answers.pop_front().unwrap_or(Selection::Empty), state))
    }

    fn confirm(&mut self, options: &PickerOptions) -> io::Result<Option<Vec<bool>>> {
        if self.cancel_confirm {
            return Ok(None);
        }
        let state = self.toggled(options);
        self.calls.push((options.clone(), Vec::new()));
        Ok(Some(state))
    }

    /// Consumes candidates as they arrive, as the real picker does.
    ///
    /// The default would block on `collect`, which hides every ordering
    /// question the streaming path raises.
    fn pick_stream(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<Selection<Candidate>> {
        let mut candidates = Vec::new();
        loop {
            candidates.extend(stream.drain());
            if stream.is_finished() {
                break;
            }
            std::thread::yield_now();
        }
        self.pick(options, candidates)
    }
}

/// A picker that picks candidates by position, for tests.
#[cfg(any(test, feature = "test-util"))]
#[derive(Debug, Default)]
pub struct SelectingPicker {
    indices: Vec<usize>,
    /// Every `(options, candidates)` it was asked, in order.
    pub calls: Vec<(PickerOptions, Vec<Candidate>)>,
}

#[cfg(any(test, feature = "test-util"))]
impl Picker for SelectingPicker {
    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>> {
        let picked: Vec<Candidate> = self
            .indices
            .iter()
            .filter_map(|i| candidates.get(*i).cloned())
            .collect();
        self.calls.push((options.clone(), candidates));
        Ok(Selection::selected(
            crate::selection::SelectionType::Default,
            picked,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::{FilterPicker, Picker, ScriptedPicker, TuiPicker, list_height};
    use crate::candidate::Candidate;
    use crate::options::PickerOptions;
    use crate::selection::{Selection, SelectionType};

    fn candidates(items: &[&str]) -> Vec<Candidate> {
        items.iter().map(|s| Candidate::identity(*s)).collect()
    }

    fn values(selection: &Selection<Candidate>) -> Vec<String> {
        selection.items().iter().map(|c| c.value.clone()).collect()
    }

    #[test]
    fn the_filter_picker_returns_every_match() {
        let mut picker = FilterPicker;
        let options = PickerOptions::default().query("fil");
        let selection = picker
            .pick(&options, candidates(&["git-files", "rg-files", "deploy"]))
            .unwrap();
        assert_eq!(values(&selection), ["git-files", "rg-files"]);
    }

    #[test]
    fn the_filter_picker_returns_empty_when_nothing_matches() {
        let mut picker = FilterPicker;
        let options = PickerOptions::default().query("zzz");
        let selection = picker.pick(&options, candidates(&["one"])).unwrap();
        assert_eq!(selection, Selection::Empty);
    }

    #[test]
    fn select_one_takes_a_unique_match_without_a_terminal() {
        let mut picker = TuiPicker;
        let options = PickerOptions::default().query("deploy").select_one(true);
        let selection = picker
            .pick(&options, candidates(&["git-files", "deploy"]))
            .unwrap();
        assert_eq!(values(&selection), ["deploy"]);
    }

    #[test]
    fn select_one_is_empty_when_nothing_matches() {
        let mut picker = TuiPicker;
        let options = PickerOptions::default().query("zzz").select_one(true);
        let selection = picker.pick(&options, candidates(&["one", "two"])).unwrap();
        assert_eq!(selection, Selection::Empty);
    }

    #[test]
    fn the_scripted_picker_answers_in_order_and_records_its_calls() {
        let mut picker = ScriptedPicker::new(vec![Selection::selected(
            SelectionType::Default,
            candidates(&["two"]),
        )]);
        let options = PickerOptions::default().header("Select command");
        let selection = picker.pick(&options, candidates(&["one", "two"])).unwrap();

        assert_eq!(values(&selection), ["two"]);
        assert_eq!(picker.calls.len(), 1);
        assert_eq!(picker.calls[0].0.header.as_deref(), Some("Select command"));
        assert_eq!(picker.calls[0].1.len(), 2);
    }

    #[test]
    fn the_scripted_picker_runs_out_as_empty() {
        let mut picker = ScriptedPicker::new(Vec::new());
        let selection = picker
            .pick(&PickerOptions::default(), candidates(&["one"]))
            .unwrap();
        assert_eq!(selection, Selection::Empty);
    }

    #[test]
    fn the_selecting_picker_picks_by_position() {
        let mut picker = ScriptedPicker::selecting(&[1]);
        let selection = picker
            .pick(&PickerOptions::default(), candidates(&["one", "two"]))
            .unwrap();
        assert_eq!(values(&selection), ["two"]);
    }

    #[test]
    fn the_list_height_leaves_room_for_the_query_and_header() {
        let plain = PickerOptions::default();
        assert_eq!(list_height(20, &plain), 19);

        let with_header = PickerOptions::default().header("x");
        assert_eq!(list_height(20, &with_header), 18);
        assert_eq!(list_height(1, &with_header), 1);
    }
}
