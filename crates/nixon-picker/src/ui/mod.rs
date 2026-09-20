//! The picker's state machine and rendering. ENGINEERING §4.1.

pub mod keymap;
pub mod render;

pub use render::render;

use std::collections::BTreeSet;

use crossterm::event::KeyEvent;

use crate::candidate::Candidate;
use crate::matcher::{Match, matches};
use crate::options::PickerOptions;
use crate::selection::{Selection, SelectionType};
use crate::textbuf::TextBuffer;
use keymap::{Action, action_for};

/// The picker's whole state. Pure: no I/O, no terminal. ENGINEERING §4.1.
pub struct App {
    /// Every candidate offered.
    pub candidates: Vec<Candidate>,
    /// The query line, with its own cursor. ENGINEERING §7.2.
    pub query: TextBuffer,
    /// The matches, in display order, each with the characters it matched.
    pub matched: Vec<Match>,
    /// Index into `matched`.
    pub cursor: usize,
    /// Indices into `candidates` the user has marked.
    pub marked: BTreeSet<usize>,
    /// First visible row of `matched`.
    pub offset: usize,
    /// How many rows the list can show.
    pub height: usize,
    /// Set once the pick is over.
    pub outcome: Option<Selection<Candidate>>,
    options: PickerOptions,
}

impl App {
    /// Builds the initial state, applying any pre-filled query.
    pub fn new(candidates: Vec<Candidate>, options: PickerOptions) -> Self {
        let mut app = Self {
            candidates,
            query: TextBuffer::new(options.initial_query.as_deref().unwrap_or_default()),
            matched: Vec::new(),
            cursor: 0,
            marked: BTreeSet::new(),
            offset: 0,
            height: 10,
            outcome: None,
            options,
        };
        app.recompute();
        app
    }

    /// The header shown above the query.
    pub fn header(&self) -> Option<&str> {
        self.options.header.as_deref()
    }

    /// Whether more than one row may be marked.
    pub const fn multi(&self) -> bool {
        self.options.multi
    }

    /// Tells the state machine how many rows fit, so it can scroll.
    pub fn set_height(&mut self, height: usize) {
        self.height = height.max(1);
        self.scroll_into_view();
    }

    /// The candidate under the cursor.
    pub fn current(&self) -> Option<&Candidate> {
        self.matched
            .get(self.cursor)
            .and_then(|m| self.candidates.get(m.index))
    }

    /// The rows currently visible, each with its match. ENGINEERING §7.2.
    pub fn visible(&self) -> impl Iterator<Item = (&Match, &Candidate)> {
        self.matched
            .iter()
            .skip(self.offset)
            .take(self.height)
            .filter_map(|m| self.candidates.get(m.index).map(|c| (m, c)))
    }

    /// Whether the pick has finished.
    pub const fn is_done(&self) -> bool {
        self.outcome.is_some()
    }

    /// Applies one key press. ENGINEERING §7.2.
    pub fn handle(&mut self, key: KeyEvent) {
        match action_for(key, &self.options.expect) {
            Action::Edit(edit) => {
                let before = self.query.text();
                self.query.apply(&edit);
                // Only re-match when the text actually changed; a cursor move
                // must not disturb the selection.
                if self.query.text() != before {
                    self.recompute();
                }
            }
            Action::MoveUp => self.move_cursor(-1),
            Action::MoveDown => self.move_cursor(1),
            Action::PageUp => self.page(-1),
            Action::PageDown => self.page(1),
            Action::ToggleMark => self.toggle_mark(),
            Action::Confirm(kind) => self.confirm(kind),
            Action::Cancel => self.outcome = Some(Selection::Canceled),
            Action::Ignore => {}
        }
    }

    /// Re-runs the query, keeping the cursor in range.
    fn recompute(&mut self) {
        self.matched = matches(&self.query.text(), &self.candidates, self.options.matching);
        self.cursor = self.cursor.min(self.matched.len().saturating_sub(1));
        self.scroll_into_view();
    }

    /// Moves a screenful. ENGINEERING §7.2.
    fn page(&mut self, direction: isize) {
        let page = self.height.max(1);
        for _ in 0..page {
            self.move_cursor(direction);
        }
    }

    fn move_cursor(&mut self, delta: isize) {
        if self.matched.is_empty() {
            return;
        }
        let last = self.matched.len() - 1;
        self.cursor = match delta {
            d if d < 0 => self.cursor.saturating_sub(1),
            _ => (self.cursor + 1).min(last),
        };
        self.scroll_into_view();
    }

    fn scroll_into_view(&mut self) {
        if self.cursor < self.offset {
            self.offset = self.cursor;
        } else if self.cursor >= self.offset + self.height {
            self.offset = self.cursor + 1 - self.height;
        }
        let max_offset = self.matched.len().saturating_sub(self.height);
        self.offset = self.offset.min(max_offset);
    }

    fn toggle_mark(&mut self) {
        if !self.options.multi {
            return;
        }
        if let Some(index) = self.matched.get(self.cursor).map(|m| m.index) {
            if !self.marked.insert(index) {
                self.marked.remove(&index);
            }
            self.move_cursor(1);
        }
    }

    /// Marked rows if any, else the row under the cursor. SPEC §8.2.
    fn confirm(&mut self, kind: SelectionType) {
        let items: Vec<Candidate> = if self.marked.is_empty() {
            self.current().cloned().into_iter().collect()
        } else {
            self.marked
                .iter()
                .filter_map(|index| self.candidates.get(*index).cloned())
                .collect()
        };
        self.outcome = Some(Selection::selected(kind, items));
    }
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
    }

    fn ctrl(app: &mut App, c: char) {
        app.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL));
    }

    fn type_query(app: &mut App, text: &str) {
        for c in text.chars() {
            press(app, KeyCode::Char(c));
        }
    }

    fn values(selection: &Selection<Candidate>) -> Vec<String> {
        selection.items().iter().map(|c| c.value.clone()).collect()
    }

    #[test]
    fn everything_matches_an_empty_query() {
        let app = app(&["one", "two", "three"]);
        assert_eq!(app.matched.len(), 3);
        assert_eq!(app.current().map(|c| c.value.as_str()), Some("one"));
    }

    #[test]
    fn an_initial_query_is_applied_before_the_first_draw() {
        let app = App::new(
            candidates(&["git-files", "deploy"]),
            PickerOptions::default().query("git"),
        );
        assert_eq!(app.matched.len(), 1);
        assert_eq!(app.current().map(|c| c.value.as_str()), Some("git-files"));
    }

    #[test]
    fn typing_narrows_the_list() {
        let mut app = app(&["git-files", "rg-files", "deploy"]);
        type_query(&mut app, "dep");
        assert_eq!(app.matched.len(), 1);
        assert_eq!(app.current().map(|c| c.value.as_str()), Some("deploy"));
    }

    #[test]
    fn backspace_widens_it_again() {
        let mut app = app(&["one", "two"]);
        type_query(&mut app, "on");
        assert_eq!(app.matched.len(), 1);
        press(&mut app, KeyCode::Backspace);
        press(&mut app, KeyCode::Backspace);
        assert_eq!(app.matched.len(), 2);
    }

    #[test]
    fn control_u_clears_the_query() {
        let mut app = app(&["one", "two"]);
        type_query(&mut app, "one");
        ctrl(&mut app, 'u');
        assert!(app.query.is_empty());
        assert_eq!(app.matched.len(), 2);
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
    fn page_keys_move_a_screenful() {
        let mut app = app(&["a", "b", "c", "d", "e", "f"]);
        app.set_height(2);
        press(&mut app, KeyCode::PageDown);
        assert_eq!(app.cursor, 2);
        press(&mut app, KeyCode::PageUp);
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn matched_characters_are_recorded_for_highlighting() {
        let mut app = app(&["git-files"]);
        type_query(&mut app, "gf");
        assert_eq!(app.matched.len(), 1);
        assert!(!app.matched[0].indices.is_empty());
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
    fn the_cursor_stays_in_range_when_the_list_shrinks() {
        let mut app = app(&["alpha", "beta", "gamma"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Down);
        assert_eq!(app.cursor, 2);
        type_query(&mut app, "alpha");
        assert_eq!(app.cursor, 0);
    }

    #[test]
    fn enter_confirms_the_row_under_the_cursor() {
        let mut app = app(&["one", "two"]);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Enter);
        let outcome = app.outcome.unwrap();
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
    fn escape_cancels() {
        let mut app = app(&["one"]);
        press(&mut app, KeyCode::Esc);
        assert_eq!(app.outcome, Some(Selection::Canceled));
    }

    #[test]
    fn control_c_cancels() {
        let mut app = app(&["one"]);
        ctrl(&mut app, 'c');
        assert_eq!(app.outcome, Some(Selection::Canceled));
    }

    #[test]
    fn alt_enter_confirms_as_edit() {
        let mut app = app(&["one"]);
        app.handle(KeyEvent::new(KeyCode::Enter, KeyModifiers::ALT));
        assert!(matches!(
            app.outcome,
            Some(Selection::Selected {
                kind: SelectionType::Edit,
                ..
            })
        ));
    }

    #[test]
    fn f1_shows_and_f2_visits() {
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
        assert_eq!(values(&app.outcome.unwrap()), ["one", "two"]);
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
    fn only_the_visible_window_is_returned() {
        let mut app = app(&["a", "b", "c", "d", "e"]);
        app.set_height(2);
        let shown: Vec<&str> = app.visible().map(|(_, c)| c.value.as_str()).collect();
        assert_eq!(shown, ["a", "b"]);
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
}
