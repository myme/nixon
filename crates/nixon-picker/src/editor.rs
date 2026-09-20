//! The edit-before-run editor. ENGINEERING §7.1 decision 4.
//!
//! Replaces v1's single-line haskeline prompt: `Alt-Enter` in the picker
//! opens the command's source here, multi-line capable.
//!
//! The text area is written out rather than taken from tui-textarea, which
//! ENGINEERING §2.1 names: its latest release builds against ratatui 0.29 and
//! its `Widget` and `Input` types are distinct from 0.30's. The alternatives
//! on crates.io are third-party forks. What is needed here is small and pure,
//! so it is unit- and snapshot-testable like the picker's own state machine.

use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::text::{Line, Text};
use ratatui::widgets::Paragraph;

use crate::terminal::TerminalGuard;

/// What the user did in the editor.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Edit {
    /// Submitted this text.
    Submitted(String),
    /// Cancelled with `Esc` or `Ctrl-C`.
    Canceled,
}

/// One key press, applied to the editor's state. ENGINEERING §7.1 decision 4.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EditAction {
    /// `Enter`: submit.
    Submit,
    /// `Alt-Enter`: insert a newline instead.
    Newline,
    /// `Esc` or `Ctrl-C`: cancel.
    Cancel,
    /// Type a character.
    Insert(char),
    /// Delete backwards, joining lines at a line start.
    DeleteBackward,
    /// Move the cursor.
    Move(Motion),
    /// Nothing.
    Ignore,
}

/// A cursor movement.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Motion {
    /// One character left.
    Left,
    /// One character right.
    Right,
    /// One line up.
    Up,
    /// One line down.
    Down,
    /// Start of the line.
    Home,
    /// End of the line.
    End,
}

/// Maps a key to an editor action. ENGINEERING §7.1 decision 4.
pub const fn action_for(key: KeyEvent) -> EditAction {
    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
    let alt = key.modifiers.contains(KeyModifiers::ALT);
    match (key.code, ctrl, alt) {
        (KeyCode::Enter, _, true) => EditAction::Newline,
        (KeyCode::Enter, _, false) => EditAction::Submit,
        (KeyCode::Esc, _, _) | (KeyCode::Char('c'), true, _) => EditAction::Cancel,
        (KeyCode::Backspace, _, _) | (KeyCode::Char('h'), true, _) => EditAction::DeleteBackward,
        (KeyCode::Left, _, _) | (KeyCode::Char('b'), true, _) => EditAction::Move(Motion::Left),
        (KeyCode::Right, _, _) | (KeyCode::Char('f'), true, _) => EditAction::Move(Motion::Right),
        (KeyCode::Up, _, _) | (KeyCode::Char('p'), true, _) => EditAction::Move(Motion::Up),
        (KeyCode::Down, _, _) | (KeyCode::Char('n'), true, _) => EditAction::Move(Motion::Down),
        (KeyCode::Home, _, _) | (KeyCode::Char('a'), true, _) => EditAction::Move(Motion::Home),
        (KeyCode::End, _, _) | (KeyCode::Char('e'), true, _) => EditAction::Move(Motion::End),
        (KeyCode::Char(c), false, false) => EditAction::Insert(c),
        _ => EditAction::Ignore,
    }
}

/// A multi-line text buffer with a cursor. Pure: no I/O, no terminal.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TextArea {
    lines: Vec<String>,
    row: usize,
    col: usize,
}

impl TextArea {
    /// Opens on `text`, cursor at the end.
    pub fn new(text: &str) -> Self {
        let lines: Vec<String> = if text.is_empty() {
            vec![String::new()]
        } else {
            text.lines().map(ToOwned::to_owned).collect()
        };
        let row = lines.len() - 1;
        let col = lines[row].chars().count();
        Self { lines, row, col }
    }

    /// The buffer's contents.
    pub fn text(&self) -> String {
        self.lines.join("\n")
    }

    /// The buffer's lines.
    pub fn lines(&self) -> &[String] {
        &self.lines
    }

    /// The cursor, as `(row, column)` in characters.
    pub const fn cursor(&self) -> (usize, usize) {
        (self.row, self.col)
    }

    /// Applies an action. `Submit` and `Cancel` are the caller's to handle.
    pub fn apply(&mut self, action: EditAction) {
        match action {
            EditAction::Insert(c) => self.insert(c),
            EditAction::Newline => self.newline(),
            EditAction::DeleteBackward => self.delete_backward(),
            EditAction::Move(motion) => self.move_cursor(motion),
            EditAction::Submit | EditAction::Cancel | EditAction::Ignore => {}
        }
    }

    fn byte_at(&self, row: usize, col: usize) -> usize {
        self.lines[row]
            .char_indices()
            .nth(col)
            .map_or(self.lines[row].len(), |(i, _)| i)
    }

    fn insert(&mut self, c: char) {
        let at = self.byte_at(self.row, self.col);
        self.lines[self.row].insert(at, c);
        self.col += 1;
    }

    fn newline(&mut self) {
        let at = self.byte_at(self.row, self.col);
        let tail = self.lines[self.row].split_off(at);
        self.lines.insert(self.row + 1, tail);
        self.row += 1;
        self.col = 0;
    }

    fn delete_backward(&mut self) {
        if self.col > 0 {
            let at = self.byte_at(self.row, self.col - 1);
            self.lines[self.row].remove(at);
            self.col -= 1;
        } else if self.row > 0 {
            let line = self.lines.remove(self.row);
            self.row -= 1;
            self.col = self.lines[self.row].chars().count();
            self.lines[self.row].push_str(&line);
        }
    }

    fn move_cursor(&mut self, motion: Motion) {
        let width = |line: &String| line.chars().count();
        match motion {
            Motion::Left => {
                if self.col > 0 {
                    self.col -= 1;
                } else if self.row > 0 {
                    self.row -= 1;
                    self.col = width(&self.lines[self.row]);
                }
            }
            Motion::Right => {
                if self.col < width(&self.lines[self.row]) {
                    self.col += 1;
                } else if self.row + 1 < self.lines.len() {
                    self.row += 1;
                    self.col = 0;
                }
            }
            Motion::Up => {
                if self.row > 0 {
                    self.row -= 1;
                    self.col = self.col.min(width(&self.lines[self.row]));
                }
            }
            Motion::Down => {
                if self.row + 1 < self.lines.len() {
                    self.row += 1;
                    self.col = self.col.min(width(&self.lines[self.row]));
                }
            }
            Motion::Home => self.col = 0,
            Motion::End => self.col = width(&self.lines[self.row]),
        }
    }
}

/// Opens the editor on `initial` and returns what the user did.
///
/// An empty submission is still [`Edit::Submitted`]; the caller reports
/// `Empty command.` as SPEC §10.7 does.
pub fn edit_text(initial: &str) -> io::Result<Edit> {
    let mut area = TextArea::new(initial);
    let mut guard = TerminalGuard::new()?;

    loop {
        guard.terminal().draw(|frame| render(&area, frame))?;

        let Event::Key(key) = event::read()? else {
            continue;
        };
        if key.kind != event::KeyEventKind::Press {
            continue;
        }
        match action_for(key) {
            EditAction::Submit => return Ok(Edit::Submitted(area.text())),
            EditAction::Cancel => return Ok(Edit::Canceled),
            other => area.apply(other),
        }
    }
}

/// Draws the prompt and the buffer. Pure in `area`. ENGINEERING §4.1.
pub fn render(area: &TextArea, frame: &mut Frame<'_>) {
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(1), Constraint::Min(0)])
        .split(frame.area());

    frame.render_widget(Paragraph::new(Line::from("> ")), rows[0]);
    let lines: Vec<Line<'_>> = area
        .lines()
        .iter()
        .map(|l| Line::from(l.as_str()))
        .collect();
    frame.render_widget(Paragraph::new(Text::from(lines)), rows[1]);

    let (row, col) = area.cursor();
    let x = rows[1]
        .x
        .saturating_add(u16::try_from(col).unwrap_or(u16::MAX));
    let y = rows[1]
        .y
        .saturating_add(u16::try_from(row).unwrap_or(u16::MAX));
    frame.set_cursor_position((x, y));
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use rstest::rstest;

    use super::{EditAction, Motion, TextArea, action_for, render};

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn ctrl(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    fn type_text(area: &mut TextArea, text: &str) {
        for c in text.chars() {
            area.apply(EditAction::Insert(c));
        }
    }

    #[rstest]
    #[case(KeyCode::Enter, EditAction::Submit)]
    #[case(KeyCode::Esc, EditAction::Cancel)]
    #[case(KeyCode::Backspace, EditAction::DeleteBackward)]
    #[case(KeyCode::Left, EditAction::Move(Motion::Left))]
    #[case(KeyCode::Right, EditAction::Move(Motion::Right))]
    #[case(KeyCode::Up, EditAction::Move(Motion::Up))]
    #[case(KeyCode::Down, EditAction::Move(Motion::Down))]
    #[case(KeyCode::Home, EditAction::Move(Motion::Home))]
    #[case(KeyCode::End, EditAction::Move(Motion::End))]
    #[case(KeyCode::Char('x'), EditAction::Insert('x'))]
    fn plain_keys_map_to_their_action(#[case] code: KeyCode, #[case] expected: EditAction) {
        assert_eq!(action_for(key(code)), expected);
    }

    #[test]
    fn alt_enter_inserts_a_newline_and_enter_submits() {
        let alt_enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::ALT);
        assert_eq!(action_for(alt_enter), EditAction::Newline);
        assert_eq!(action_for(key(KeyCode::Enter)), EditAction::Submit);
    }

    #[test]
    fn control_c_cancels() {
        assert_eq!(action_for(ctrl('c')), EditAction::Cancel);
    }

    #[test]
    fn opens_on_the_initial_text_with_the_cursor_at_the_end() {
        let area = TextArea::new("echo hello");
        assert_eq!(area.text(), "echo hello");
        assert_eq!(area.cursor(), (0, 10));
    }

    #[test]
    fn opens_empty_when_there_is_no_initial_text() {
        let area = TextArea::new("");
        assert_eq!(area.text(), "");
        assert_eq!(area.cursor(), (0, 0));
    }

    #[test]
    fn a_multi_line_source_keeps_its_lines() {
        let area = TextArea::new("echo one\necho two\necho three");
        assert_eq!(area.lines().len(), 3);
        assert_eq!(area.cursor(), (2, 10));
    }

    #[test]
    fn typing_inserts_at_the_cursor() {
        let mut area = TextArea::new("echo");
        type_text(&mut area, " hi");
        assert_eq!(area.text(), "echo hi");
    }

    #[test]
    fn typing_in_the_middle_inserts_there() {
        let mut area = TextArea::new("echo");
        area.apply(EditAction::Move(Motion::Home));
        type_text(&mut area, "# ");
        assert_eq!(area.text(), "# echo");
        assert_eq!(area.cursor(), (0, 2));
    }

    #[test]
    fn a_newline_splits_the_line_at_the_cursor() {
        let mut area = TextArea::new("echo one two");
        area.apply(EditAction::Move(Motion::Home));
        for _ in 0..9 {
            area.apply(EditAction::Move(Motion::Right));
        }
        area.apply(EditAction::Newline);
        assert_eq!(area.text(), "echo one \ntwo");
        assert_eq!(area.cursor(), (1, 0));
    }

    #[test]
    fn backspace_deletes_within_a_line() {
        let mut area = TextArea::new("echo");
        area.apply(EditAction::DeleteBackward);
        assert_eq!(area.text(), "ech");
        assert_eq!(area.cursor(), (0, 3));
    }

    #[test]
    fn backspace_at_a_line_start_joins_the_lines() {
        let mut area = TextArea::new("one\ntwo");
        area.apply(EditAction::Move(Motion::Home));
        area.apply(EditAction::DeleteBackward);
        assert_eq!(area.text(), "onetwo");
        assert_eq!(area.cursor(), (0, 3));
    }

    #[test]
    fn backspace_at_the_very_start_does_nothing() {
        let mut area = TextArea::new("one");
        area.apply(EditAction::Move(Motion::Home));
        area.apply(EditAction::DeleteBackward);
        assert_eq!(area.text(), "one");
        assert_eq!(area.cursor(), (0, 0));
    }

    #[test]
    fn the_cursor_wraps_between_lines() {
        let mut area = TextArea::new("one\ntwo");
        area.apply(EditAction::Move(Motion::Home));
        area.apply(EditAction::Move(Motion::Left));
        assert_eq!(area.cursor(), (0, 3));
        area.apply(EditAction::Move(Motion::Right));
        assert_eq!(area.cursor(), (1, 0));
    }

    #[test]
    fn the_cursor_clamps_at_both_ends() {
        let mut area = TextArea::new("one");
        area.apply(EditAction::Move(Motion::Home));
        area.apply(EditAction::Move(Motion::Up));
        assert_eq!(area.cursor(), (0, 0));
        area.apply(EditAction::Move(Motion::End));
        area.apply(EditAction::Move(Motion::Down));
        assert_eq!(area.cursor(), (0, 3));
    }

    #[test]
    fn moving_to_a_shorter_line_clamps_the_column() {
        let mut area = TextArea::new("a longer line\nshort");
        area.apply(EditAction::Move(Motion::End));
        area.apply(EditAction::Move(Motion::Up));
        assert_eq!(area.cursor(), (0, 5));
    }

    #[test]
    fn multi_byte_characters_move_by_character_not_byte() {
        let mut area = TextArea::new("héllo");
        area.apply(EditAction::DeleteBackward);
        assert_eq!(area.text(), "héll");
        area.apply(EditAction::Move(Motion::Home));
        area.apply(EditAction::Move(Motion::Right));
        area.apply(EditAction::Insert('x'));
        assert_eq!(area.text(), "hxéll");
    }

    fn draw(area: &TextArea) -> String {
        let backend = TestBackend::new(40, 6);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|frame| render(area, frame)).unwrap();
        format!("{}", terminal.backend())
    }

    #[test]
    fn renders_a_single_line_source() {
        insta::assert_snapshot!(draw(&TextArea::new("git ls-files")));
    }

    #[test]
    fn renders_a_multi_line_source() {
        insta::assert_snapshot!(draw(&TextArea::new("set -e\necho one\necho two")));
    }
}
