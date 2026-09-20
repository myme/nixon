//! The picker's state machine and rendering.

pub mod keymap;
pub mod render;

pub use render::render;

use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Duration;

use crossterm::event::KeyEvent;
use nucleo::{Config, Nucleo};
use nucleo_matcher::Matcher;

use crate::candidate::Candidate;
use crate::matcher::match_indices;
use crate::options::PickerOptions;
use crate::selection::{Selection, SelectionType};
use crate::textbuf::TextBuffer;
use keymap::{Action, action_for};

/// One visible row, ready to draw.
///
/// Built for the handful of rows on screen, never for the whole list.
pub struct Row {
    /// The candidate to draw.
    pub candidate: Candidate,
    /// Character positions of the query's matches in its visible text.
    pub indices: Vec<u32>,
    /// Whether the user has marked it.
    pub marked: bool,
    /// Whether the cursor is on it.
    pub is_cursor: bool,
}

/// The picker's state.
///
/// Matching runs on nucleo's background worker: the UI thread reparses the
/// pattern on a keystroke and reads a snapshot per frame, and never makes a
/// pass over the candidates itself.
pub struct App {
    nucleo: Nucleo<Candidate>,
    matcher: Matcher,
    /// The query line, with its own cursor.
    pub query: TextBuffer,
    /// Index into the current matches.
    pub cursor: usize,
    /// Candidates the user has marked, keyed by identity.
    ///
    /// Keyed rather than positional so marks survive a query change, and
    /// ordered by identity so confirming returns them in list order, however
    /// they were marked.
    pub marked: BTreeMap<u32, Candidate>,
    /// First visible row.
    pub offset: usize,
    /// How many rows the list can show.
    pub height: usize,
    /// Set once the pick is over.
    pub outcome: Option<Selection<Candidate>>,
    options: PickerOptions,
    next_id: u32,
}

impl App {
    /// Builds the initial state, applying any pre-filled query.
    pub fn new(candidates: Vec<Candidate>, options: PickerOptions) -> Self {
        let mut app = Self::empty(options);
        for candidate in candidates {
            app.push(candidate);
        }
        app.reparse(true);
        app.tick_until_settled();
        app
    }

    /// An empty picker that candidates can be streamed into.
    ///
    /// Lets the picker open before the command producing its candidates has
    /// finished.
    pub fn empty(options: PickerOptions) -> Self {
        let nucleo = Nucleo::new(Config::DEFAULT, Arc::new(|| {}), None, 1);
        let mut app = Self {
            nucleo,
            matcher: Matcher::new(nucleo_matcher::Config::DEFAULT),
            query: TextBuffer::new(options.initial_query.as_deref().unwrap_or_default()),
            cursor: 0,
            marked: BTreeMap::new(),
            next_id: 0,
            offset: 0,
            height: 10,
            outcome: None,
            options,
        };
        app.reparse(true);
        app
    }

    /// Takes a candidate, giving it the identity its marks are keyed by.
    ///
    /// The matched text is the candidate's visible text, never its escapes.
    pub fn push(&mut self, mut candidate: Candidate) {
        candidate.id = self.next_id;
        self.next_id += 1;
        let plain = candidate.plain();
        self.nucleo
            .injector()
            .push(candidate, |_, columns| columns[0] = plain.as_str().into());
    }

    /// Lets the matcher make progress; called once per frame.
    pub fn tick(&mut self) {
        self.nucleo.tick(10);
        self.clamp();
    }

    /// Runs the matcher to completion. For the non-streaming path and tests.
    pub fn tick_until_settled(&mut self) {
        self.settle(Duration::from_secs(5));
    }

    /// Lets the matcher work for at most `budget`, then returns.
    ///
    /// A keystroke gives it a frame's worth; whatever is left finishes on
    /// later frames, so the UI thread is never blocked by the candidate
    /// count.
    fn settle(&mut self, budget: Duration) {
        let deadline = std::time::Instant::now() + budget;
        while self.nucleo.tick(1).running && std::time::Instant::now() < deadline {}
        self.clamp();
    }

    /// The header shown above the query.
    pub fn header(&self) -> Option<&str> {
        self.options.header.as_deref()
    }

    /// Whether more than one row may be marked.
    pub const fn multi(&self) -> bool {
        self.options.multi
    }

    /// How many candidates matched the query.
    pub fn matched_count(&self) -> u32 {
        self.nucleo.snapshot().matched_item_count()
    }

    /// How many candidates there are in total.
    pub fn total_count(&self) -> u32 {
        self.nucleo.snapshot().item_count()
    }

    /// Tells the state machine how many rows fit, so it can scroll.
    pub fn set_height(&mut self, height: usize) {
        self.height = height.max(1);
        self.scroll_into_view();
    }

    /// The candidate under the cursor.
    pub fn current(&self) -> Option<Candidate> {
        let snapshot = self.nucleo.snapshot();
        let at = u32::try_from(self.cursor).ok()?;
        snapshot.get_matched_item(at).map(|item| item.data.clone())
    }

    /// The rows currently on screen, with their match positions.
    ///
    /// Only the visible window is built, and match indices are computed only
    /// for those rows, so the cost per frame is the screen size rather than
    /// the candidate count.
    pub fn rows(&mut self) -> Vec<Row> {
        let pattern = self.options.matching.pattern(&self.query.text());
        let snapshot = self.nucleo.snapshot();

        let from = u32::try_from(self.offset).unwrap_or(u32::MAX);
        let to = from
            .saturating_add(u32::try_from(self.height).unwrap_or(u32::MAX))
            .min(snapshot.matched_item_count());
        if from >= to {
            return Vec::new();
        }

        let cursor = u32::try_from(self.cursor).unwrap_or(u32::MAX);
        let mut indices = Vec::new();
        snapshot
            .matched_items(from..to)
            .enumerate()
            .map(|(row, item)| {
                let candidate = item.data.clone();
                match_indices(
                    &mut self.matcher,
                    &pattern,
                    &candidate.plain(),
                    &mut indices,
                );
                let at = from + u32::try_from(row).unwrap_or(0);
                Row {
                    marked: self.marked.contains_key(&candidate.id),
                    candidate,
                    indices: indices.clone(),
                    is_cursor: at == cursor,
                }
            })
            .collect()
    }

    /// Whether the pick has finished.
    pub const fn is_done(&self) -> bool {
        self.outcome.is_some()
    }

    /// Applies one key press.
    pub fn handle(&mut self, key: KeyEvent) {
        match action_for(key, &self.options.expect) {
            Action::Edit(edit) => {
                let before = self.query.text();
                self.query.apply(&edit);
                // Only re-match when the text actually changed; a cursor move
                // must not disturb the selection.
                if self.query.text() != before {
                    let appended = self.query.text().starts_with(&before);
                    self.reparse(!appended);
                    self.cursor = 0;
                    self.offset = 0;
                    // Marks are deliberately kept: they are keyed by
                    // candidate, so narrowing away a marked row does not
                    // unmark it.
                    // A frame's worth of matching, no more.
                    self.settle(Duration::from_millis(10));
                }
            }
            Action::MoveUp => self.move_cursor(-1),
            Action::MoveDown => self.move_cursor(1),
            Action::PageUp => self.page(-1, self.height),
            Action::PageDown => self.page(1, self.height),
            Action::HalfPageUp => self.page(-1, self.height / 2),
            Action::HalfPageDown => self.page(1, self.height / 2),
            Action::ToggleMark => self.toggle_mark(1),
            Action::ToggleMarkUp => self.toggle_mark(-1),
            Action::Confirm(kind) => self.confirm(kind),
            Action::Cancel => self.outcome = Some(Selection::Canceled),
            Action::Ignore => {}
        }
    }

    /// Hands the query to the background matcher.
    fn reparse(&mut self, rescore: bool) {
        let opts = self.options.matching;
        // The same query the non-interactive paths parse, so `--list` and
        // the picker agree on what a term means.
        let query = self.query.text();
        self.nucleo.pattern.reparse(
            0,
            &opts.query(&query),
            opts.case_matching(),
            Normalization(),
            // The rewrite `exact` applies is not append-safe: typing a `$`
            // turns `'foo` into `foo$`, which is not an extension of it.
            !rescore && !opts.exact,
        );
    }

    fn clamp(&mut self) {
        let matched = self.matched_count() as usize;
        self.cursor = self.cursor.min(matched.saturating_sub(1));
        self.scroll_into_view();
    }

    fn move_cursor(&mut self, delta: isize) {
        let matched = self.matched_count() as usize;
        if matched == 0 {
            return;
        }
        let last = matched - 1;
        self.cursor = if delta < 0 {
            self.cursor.saturating_sub(1)
        } else {
            (self.cursor + 1).min(last)
        };
        self.scroll_into_view();
    }

    /// Moves `rows` candidates, at least one.
    fn page(&mut self, direction: isize, rows: usize) {
        for _ in 0..rows.max(1) {
            self.move_cursor(direction);
        }
    }

    fn scroll_into_view(&mut self) {
        if self.cursor < self.offset {
            self.offset = self.cursor;
        } else if self.cursor >= self.offset + self.height {
            self.offset = self.cursor + 1 - self.height;
        }
        let matched = self.matched_count() as usize;
        self.offset = self.offset.min(matched.saturating_sub(self.height));
    }

    /// Marks or unmarks the current row, then steps `delta`.
    ///
    /// fzf moves on after marking so a run of rows can be taken without
    /// reaching for the arrows; Shift-Tab does the same upwards.
    fn toggle_mark(&mut self, delta: isize) {
        if !self.options.multi {
            return;
        }
        if let Some(candidate) = self.current() {
            if self.marked.remove(&candidate.id).is_none() {
                self.marked.insert(candidate.id, candidate);
            }
            self.move_cursor(delta);
        }
    }

    /// Marked rows if any, else the row under the cursor.
    fn confirm(&mut self, kind: SelectionType) {
        let items: Vec<Candidate> = if self.marked.is_empty() {
            self.current().into_iter().collect()
        } else {
            // Keyed by identity, so these include rows the current query no
            // longer matches, in list order.
            self.marked.values().cloned().collect()
        };
        self.outcome = Some(Selection::selected(kind, items));
    }
}

/// nucleo's normalization setting, spelled once.
#[expect(non_snake_case, reason = "reads as the enum variant it stands for")]
const fn Normalization() -> nucleo::pattern::Normalization {
    nucleo::pattern::Normalization::Smart
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    use super::App;
    use crate::candidate::Candidate;
    use crate::options::PickerOptions;
    use crate::selection::{Selection, SelectionType};

    fn candidates(items: &[&str]) -> Vec<Candidate> {
        items.iter().map(|s| Candidate::identity(*s)).collect()
    }

    fn app(items: &[&str]) -> App {
        App::new(candidates(items), PickerOptions::default())
    }

    fn multi_app(items: &[&str]) -> App {
        App::new(candidates(items), PickerOptions::default().multi(true))
    }

    fn press(app: &mut App, code: KeyCode) {
        app.handle(KeyEvent::new(code, KeyModifiers::NONE));
        app.tick_until_settled();
    }

    fn ctrl(app: &mut App, c: char) {
        app.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL));
        app.tick_until_settled();
    }

    fn alt(app: &mut App, c: char) {
        app.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::ALT));
        app.tick_until_settled();
    }

    fn type_query(app: &mut App, text: &str) {
        for c in text.chars() {
            press(app, KeyCode::Char(c));
        }
    }

    fn values(selection: &Selection<Candidate>) -> Vec<String> {
        selection.items().iter().map(|c| c.value.clone()).collect()
    }

    fn shown(app: &mut App) -> Vec<String> {
        app.rows()
            .into_iter()
            .map(|row| row.candidate.value)
            .collect()
    }

    #[test]
    fn everything_matches_an_empty_query() {
        let app = app(&["one", "two", "three"]);
        assert_eq!(app.matched_count(), 3);
        assert_eq!(app.total_count(), 3);
        assert_eq!(app.current().map(|c| c.value), Some("one".to_owned()));
    }

    #[test]
    fn an_initial_query_is_applied_before_the_first_draw() {
        let app = App::new(
            candidates(&["git-files", "deploy"]),
            PickerOptions::default().query("git"),
        );
        assert_eq!(app.matched_count(), 1);
        assert_eq!(app.current().map(|c| c.value), Some("git-files".to_owned()));
    }

    #[test]
    fn typing_narrows_the_list() {
        let mut app = app(&["git-files", "rg-files", "deploy"]);
        type_query(&mut app, "dep");
        assert_eq!(app.matched_count(), 1);
        assert_eq!(app.current().map(|c| c.value), Some("deploy".to_owned()));
    }

    #[test]
    fn backspace_widens_it_again() {
        let mut app = app(&["one", "two"]);
        type_query(&mut app, "on");
        assert_eq!(app.matched_count(), 1);
        press(&mut app, KeyCode::Backspace);
        press(&mut app, KeyCode::Backspace);
        assert_eq!(app.matched_count(), 2);
    }

    #[test]
    fn control_u_clears_the_query() {
        let mut app = app(&["one", "two"]);
        type_query(&mut app, "one");
        ctrl(&mut app, 'u');
        assert!(app.query.is_empty());
        assert_eq!(app.matched_count(), 2);
    }

    #[test]
    fn control_w_deletes_the_last_word() {
        let mut app = app(&["one"]);
        type_query(&mut app, "foo bar");
        ctrl(&mut app, 'w');
        assert_eq!(app.query.text(), "foo ");
    }

    #[test]
    fn control_a_and_e_move_within_the_query() {
        let mut app = app(&["one"]);
        type_query(&mut app, "abc");
        ctrl(&mut app, 'a');
        assert_eq!(app.query.cursor(), (0, 0));
        press(&mut app, KeyCode::Char('x'));
        assert_eq!(app.query.text(), "xabc");
        ctrl(&mut app, 'e');
        assert_eq!(app.query.cursor(), (0, 4));
    }

    #[test]
    fn moving_the_query_cursor_does_not_change_the_selection() {
        let mut app = app(&["alpha", "beta", "gamma"]);
        press(&mut app, KeyCode::Down);
        assert_eq!(app.cursor, 1);
        ctrl(&mut app, 'a');
        assert_eq!(app.cursor, 1, "a cursor move must not re-filter");
    }

    #[test]
    fn control_y_yanks_back_what_was_deleted() {
        let mut app = app(&["one"]);
        type_query(&mut app, "foo bar");
        ctrl(&mut app, 'w');
        ctrl(&mut app, 'y');
        assert_eq!(app.query.text(), "foo bar");
    }

    #[test]
    fn the_cursor_clamps_at_both_ends() {
        let mut app = app(&["one", "two", "three"]);
        press(&mut app, KeyCode::Up);
        assert_eq!(app.cursor, 0);
        for _ in 0..10 {
            press(&mut app, KeyCode::Down);
        }
        assert_eq!(app.cursor, 2);
    }

    #[test]
    fn the_cursor_returns_to_the_top_when_the_query_changes() {
        let mut app = app(&["alpha", "beta", "gamma"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Down);
        assert_eq!(app.cursor, 2);
        type_query(&mut app, "alpha");
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn page_keys_move_a_screenful() {
        let mut app = app(&["a", "b", "c", "d", "e", "f"]);
        app.set_height(2);
        press(&mut app, KeyCode::PageDown);
        assert_eq!(app.cursor, 2);
        press(&mut app, KeyCode::PageUp);
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn control_v_and_alt_v_page_like_the_page_keys() {
        let mut app = app(&["a", "b", "c", "d", "e", "f"]);
        app.set_height(2);
        ctrl(&mut app, 'v');
        assert_eq!(app.cursor, 2);
        alt(&mut app, 'v');
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn alt_j_and_alt_k_move_half_a_screenful() {
        let mut app = app(&["a", "b", "c", "d", "e", "f", "g", "h"]);
        app.set_height(4);
        alt(&mut app, 'j');
        assert_eq!(app.cursor, 2);
        alt(&mut app, 'j');
        assert_eq!(app.cursor, 4);
        alt(&mut app, 'k');
        assert_eq!(app.cursor, 2);
    }

    #[test]
    fn a_half_page_still_moves_one_row_on_a_tiny_list() {
        let mut app = app(&["a", "b", "c"]);
        app.set_height(1);
        alt(&mut app, 'j');
        assert_eq!(app.cursor, 1);
    }

    #[test]
    fn enter_confirms_the_row_under_the_cursor() {
        let mut app = app(&["one", "two"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Enter);
        let outcome = app.outcome.clone().unwrap();
        assert!(matches!(
            outcome,
            Selection::Selected {
                kind: SelectionType::Default,
                ..
            }
        ));
        assert_eq!(values(&outcome), ["two"]);
    }

    #[test]
    fn confirming_with_no_matches_is_empty() {
        let mut app = app(&["one", "two"]);
        type_query(&mut app, "zzz");
        press(&mut app, KeyCode::Enter);
        assert_eq!(app.outcome, Some(Selection::Empty));
    }

    #[test]
    fn escape_and_control_c_cancel() {
        let mut escaped = app(&["one"]);
        press(&mut escaped, KeyCode::Esc);
        assert_eq!(escaped.outcome, Some(Selection::Canceled));

        let mut interrupted = app(&["one"]);
        ctrl(&mut interrupted, 'c');
        assert_eq!(interrupted.outcome, Some(Selection::Canceled));
    }

    #[test]
    fn alt_enter_f1_and_f2_confirm_with_their_own_type() {
        let mut edit = app(&["one"]);
        edit.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::ALT));
        assert!(matches!(
            edit.outcome,
            Some(Selection::Selected {
                kind: SelectionType::Edit,
                ..
            })
        ));

        let mut show = app(&["one"]);
        press(&mut show, KeyCode::F(1));
        assert!(matches!(
            show.outcome,
            Some(Selection::Selected {
                kind: SelectionType::Show,
                ..
            })
        ));

        let mut visit = app(&["one"]);
        press(&mut visit, KeyCode::F(2));
        assert!(matches!(
            visit.outcome,
            Some(Selection::Selected {
                kind: SelectionType::Visit,
                ..
            })
        ));
    }

    #[test]
    fn tab_does_nothing_without_multi_select() {
        let mut app = app(&["one", "two"]);
        press(&mut app, KeyCode::Tab);
        assert!(app.marked.is_empty());
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn tab_marks_and_advances_with_multi_select() {
        let mut app = multi_app(&["one", "two", "three"]);
        press(&mut app, KeyCode::Tab);
        assert_eq!(app.marked.len(), 1);
        assert_eq!(app.cursor, 1);
    }

    /// fzf keeps marks across query changes: mark under one search, change
    /// it, mark more, and Enter returns everything.
    #[test]
    fn marks_survive_a_query_change() {
        let mut app = multi_app(&["alpha-one", "beta-two", "alpha-three"]);

        type_query(&mut app, "alpha");
        assert_eq!(app.matched_count(), 2);
        press(&mut app, KeyCode::Tab);
        assert_eq!(app.marked.len(), 1);

        // Narrow to something the marked row does not match.
        ctrl(&mut app, 'u');
        type_query(&mut app, "beta");
        assert_eq!(app.matched_count(), 1);
        assert_eq!(app.marked.len(), 1, "the earlier mark must survive");

        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Enter);

        let picked = values(&app.outcome.clone().unwrap());
        assert_eq!(picked, ["alpha-one", "beta-two"]);
    }

    #[test]
    fn a_marked_row_that_no_longer_matches_is_still_returned() {
        let mut app = multi_app(&["keep-me", "other"]);
        press(&mut app, KeyCode::Tab);
        type_query(&mut app, "other");
        assert_eq!(app.matched_count(), 1);

        press(&mut app, KeyCode::Enter);
        assert_eq!(values(&app.outcome.clone().unwrap()), ["keep-me"]);
    }

    #[test]
    fn unmarking_works_after_a_query_change() {
        let mut app = multi_app(&["alpha", "beta"]);
        press(&mut app, KeyCode::Tab);
        assert_eq!(app.marked.len(), 1);

        type_query(&mut app, "alpha");
        press(&mut app, KeyCode::Tab);
        assert!(app.marked.is_empty(), "the same row unmarks itself");
    }

    #[test]
    fn marks_are_returned_in_list_order_however_they_were_made() {
        let mut app = multi_app(&["one", "two", "three"]);
        // Mark the last, then the first.
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Tab);
        type_query(&mut app, "one");
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Enter);

        assert_eq!(values(&app.outcome.clone().unwrap()), ["one", "three"]);
    }

    #[test]
    fn shift_tab_marks_and_moves_up() {
        let mut app = multi_app(&["one", "two", "three"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Down);
        assert_eq!(app.cursor, 2);

        press(&mut app, KeyCode::BackTab);
        assert_eq!(app.marked.len(), 1);
        assert_eq!(app.cursor, 1);

        press(&mut app, KeyCode::BackTab);
        assert_eq!(app.marked.len(), 2);
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn shift_tab_does_nothing_without_multi_select() {
        let mut app = app(&["one", "two"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::BackTab);
        assert!(app.marked.is_empty());
        assert_eq!(app.cursor, 1);
    }

    #[test]
    fn shift_tab_twice_on_the_same_row_unmarks_it() {
        let mut app = multi_app(&["one", "two"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::BackTab);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::BackTab);
        assert!(app.marked.is_empty());
    }

    #[test]
    fn marking_upwards_returns_the_rows_in_list_order() {
        let mut app = multi_app(&["one", "two", "three"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::BackTab);
        press(&mut app, KeyCode::BackTab);
        press(&mut app, KeyCode::Enter);
        assert_eq!(values(&app.outcome.clone().unwrap()), ["two", "three"]);
    }

    #[test]
    fn tab_twice_on_the_same_row_unmarks_it() {
        let mut app = multi_app(&["one", "two"]);
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Up);
        press(&mut app, KeyCode::Tab);
        assert!(app.marked.is_empty());
    }

    #[test]
    fn confirming_returns_every_marked_row() {
        let mut app = multi_app(&["one", "two", "three"]);
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Enter);
        assert_eq!(values(&app.outcome.clone().unwrap()), ["one", "two"]);
    }

    #[test]
    fn scrolling_follows_the_cursor_down_and_back_up() {
        let mut app = app(&["a", "b", "c", "d", "e"]);
        app.set_height(2);
        assert_eq!(app.offset, 0);

        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Down);
        assert_eq!(app.cursor, 2);
        assert_eq!(app.offset, 1);

        press(&mut app, KeyCode::Up);
        press(&mut app, KeyCode::Up);
        assert_eq!(app.cursor, 0);
        assert_eq!(app.offset, 0);
    }

    #[test]
    fn only_the_visible_window_is_built() {
        let mut app = app(&["a", "b", "c", "d", "e"]);
        app.set_height(2);
        assert_eq!(shown(&mut app), ["a", "b"]);
    }

    #[test]
    fn a_pick_is_not_done_until_it_is_confirmed_or_cancelled() {
        let mut app = app(&["one"]);
        assert!(!app.is_done());
        press(&mut app, KeyCode::Down);
        assert!(!app.is_done());
        press(&mut app, KeyCode::Enter);
        assert!(app.is_done());
    }

    #[test]
    fn an_empty_query_keeps_the_input_order() {
        let mut app = app(&["zzzzzzzz", "a", "mmmm"]);
        assert_eq!(shown(&mut app), ["zzzzzzzz", "a", "mmmm"]);
    }

    #[test]
    fn ties_break_on_length_then_input_order() {
        // All three contain "ab"; fzf shows the shortest first.
        let mut app = app(&["ab-longest-one", "ab", "ab-mid"]);
        type_query(&mut app, "ab");
        assert_eq!(shown(&mut app), ["ab", "ab-mid", "ab-longest-one"]);
    }

    #[test]
    fn matched_characters_are_recorded_for_the_visible_rows() {
        let mut app = app(&["git-files"]);
        type_query(&mut app, "gf");
        let rows = app.rows();
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].indices, [0, 4]);
    }

    #[test]
    fn candidates_can_be_streamed_in_after_the_picker_opens() {
        let mut app = App::empty(PickerOptions::default());
        assert_eq!(app.total_count(), 0);

        for value in ["one", "two", "three"] {
            app.push(Candidate::identity(value));
        }

        app.tick_until_settled();
        assert_eq!(app.total_count(), 3);
        assert_eq!(app.matched_count(), 3);
    }

    /// Guards the fix for the 95k-candidate slowdown: a keystroke must not
    /// cost a pass over the candidates.
    ///
    /// The threshold is deliberately loose. This runs unoptimised and
    /// alongside the other nix checks, where it measured 57ms against a
    /// 50ms line; in release on an idle machine it is ~11ms. What it has to
    /// catch is matching moving back onto the UI thread, which cost ~180ms
    /// per keystroke in release and seconds here — so half a second is a
    /// wide but decisive line, not a performance target.
    #[test]
    fn a_keystroke_stays_within_a_frame_on_a_large_list() {
        let items: Vec<Candidate> = (0..200_000)
            .map(|i| Candidate::identity(format!("src/module{}/file_{i}.rs", i % 97)))
            .collect();
        let mut app = App::new(items, PickerOptions::default());
        app.set_height(40);

        for c in "file_12".chars() {
            let start = std::time::Instant::now();
            app.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE));
            let _ = app.rows();
            let elapsed = start.elapsed();
            assert!(
                elapsed < std::time::Duration::from_millis(500),
                "a keystroke took {elapsed:?}; matching must stay off the UI thread"
            );
        }
    }
}
