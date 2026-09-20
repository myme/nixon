//! The picker interface and its implementations. ENGINEERING §4.1.

use std::io;

use crossterm::event::{self, Event};

use std::time::Duration;

use crate::candidate::Candidate;
use crate::filter::filter;
use crate::options::PickerOptions;
use crate::selection::Selection;
use crate::stream::CandidateStream;
use crate::terminal::TerminalGuard;
use crate::ui::{App, render};

/// Anything that can turn candidates into a selection. ENGINEERING §4.1.
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
}

/// The real picker: a terminal UI on stderr. ENGINEERING §4.1.
#[derive(Debug, Default)]
pub struct TuiPicker;

impl Picker for TuiPicker {
    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>> {
        if let Some(selection) = short_circuit(options, &candidates) {
            return Ok(selection);
        }

        let mut app = App::new(candidates, options.clone());
        run_loop(&mut app, options, None)
    }

    fn pick_stream(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<Selection<Candidate>> {
        // The picker opens on an empty list and fills as the producer runs.
        let mut app = App::empty(options.clone());
        let selection = run_loop(&mut app, options, Some(stream))?;
        if matches!(selection, Selection::Canceled) {
            stream.cancel();
        }
        Ok(selection)
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

/// `-1` applied to what the matcher has settled on. SPEC §8.4.
fn short_circuit_app(app: &App) -> Option<Selection<Candidate>> {
    match app.matched_count() {
        0 => Some(Selection::Empty),
        1 => app.current().map(|candidate| {
            Selection::selected(crate::selection::SelectionType::Default, vec![candidate])
        }),
        _ => None,
    }
}

/// Non-interactive matching, for `--list` and `| list`. SPEC §8.4.
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

/// fzf's `-1`: a query matching exactly one row selects it without drawing.
/// SPEC §8.4.
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

/// A picker that answers from a queue, for tests. ENGINEERING §4.1.
#[cfg(any(test, feature = "test-util"))]
#[derive(Debug, Default)]
pub struct ScriptedPicker {
    answers: std::collections::VecDeque<Selection<Candidate>>,
    /// Every `(options, candidates)` it was asked, in order.
    pub calls: Vec<(PickerOptions, Vec<Candidate>)>,
}

#[cfg(any(test, feature = "test-util"))]
impl ScriptedPicker {
    /// Builds a picker that returns `answers` in order.
    pub fn new(answers: Vec<Selection<Candidate>>) -> Self {
        Self {
            answers: answers.into(),
            calls: Vec::new(),
        }
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
