//! The edit-before-run editor. ENGINEERING §7.1 decision 4, §7.2.
//!
//! Replaces v1's single-line haskeline prompt. It shares the query line's
//! whole readline keymap, adding only multi-line movement: `Up`/`Down`
//! between lines, `Alt-Enter` for a newline, `Enter` to submit, `Esc` to
//! cancel.

use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::text::{Line, Text};
use ratatui::widgets::Paragraph;

use crate::terminal::TerminalGuard;
use crate::textbuf::{Edit, Motion, TextBuffer};
use crate::ui::keymap::line_edit;

/// What the user did in the editor.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Edited {
    /// Submitted this text.
    Submitted(String),
    /// Cancelled with `Esc` or `Ctrl-C`.
    Canceled,
}

/// One key press in the editor. ENGINEERING §7.2.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EditAction {
    /// `Enter`: submit.
    Submit,
    /// `Esc` or `Ctrl-C`: cancel.
    Cancel,
    /// Anything the query line would do, plus the multi-line moves.
    Edit(Edit),
    /// Nothing.
    Ignore,
}

/// Maps a key in the editor. ENGINEERING §7.2.
///
/// The same bindings as the query line; only the keys the picker reserves
/// for the candidate list differ, because here they move between lines.
pub fn action_for(key: KeyEvent) -> EditAction {
    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
    let alt = key.modifiers.contains(KeyModifiers::ALT);

    match (key.code, ctrl, alt) {
        (KeyCode::Enter, _, true) => return EditAction::Edit(Edit::Newline),
        (KeyCode::Enter, _, false) => return EditAction::Submit,
        (KeyCode::Esc, _, _) | (KeyCode::Char('c'), true, _) => return EditAction::Cancel,
        (KeyCode::Up, _, _) | (KeyCode::Char('p'), true, _) => {
            return EditAction::Edit(Edit::Move(Motion::Up));
        }
        (KeyCode::Down, _, _) | (KeyCode::Char('n'), true, _) => {
            return EditAction::Edit(Edit::Move(Motion::Down));
        }
        _ => {}
    }

    line_edit(key).map_or(EditAction::Ignore, EditAction::Edit)
}

/// Opens the editor on `initial` and returns what the user did.
///
/// An empty submission is still [`Edited::Submitted`]; the caller reports
/// `Empty command.` as SPEC §10.7 does.
pub fn edit_text(initial: &str) -> io::Result<Edited> {
    let mut buffer = TextBuffer::new(initial);
    let mut guard = TerminalGuard::new()?;

    loop {
        guard.terminal().draw(|frame| render(&buffer, frame))?;

        let Event::Key(key) = event::read()? else {
            continue;
        };
        if key.kind != event::KeyEventKind::Press {
            continue;
        }
        match action_for(key) {
            EditAction::Submit => return Ok(Edited::Submitted(buffer.text())),
            EditAction::Cancel => return Ok(Edited::Canceled),
            EditAction::Edit(edit) => buffer.apply(&edit),
            EditAction::Ignore => {}
        }
    }
}

/// Draws the prompt and the buffer, with the cursor. ENGINEERING §7.2.
pub fn render(buffer: &TextBuffer, frame: &mut Frame<'_>) {
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(1), Constraint::Min(0)])
        .split(frame.area());

    frame.render_widget(Paragraph::new(Line::from("> ")), rows[0]);
    let lines: Vec<Line<'_>> = buffer
        .lines()
        .iter()
        .map(|line| Line::from(line.as_str()))
        .collect();
    frame.render_widget(Paragraph::new(Text::from(lines)), rows[1]);

    let (row, col) = buffer.cursor();
    frame.set_cursor_position((
        rows[1]
            .x
            .saturating_add(u16::try_from(col).unwrap_or(u16::MAX)),
        rows[1]
            .y
            .saturating_add(u16::try_from(row).unwrap_or(u16::MAX)),
    ));
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use rstest::rstest;

    use super::{EditAction, action_for, render};
    use crate::textbuf::{Edit, Motion, TextBuffer};

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn ctrl(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    fn alt(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::ALT)
    }

    #[rstest]
    #[case(KeyCode::Enter, EditAction::Submit)]
    #[case(KeyCode::Esc, EditAction::Cancel)]
    #[case(KeyCode::Char('x'), EditAction::Edit(Edit::Insert('x')))]
    #[case(KeyCode::Backspace, EditAction::Edit(Edit::DeleteBackward))]
    #[case(KeyCode::Delete, EditAction::Edit(Edit::DeleteForward))]
    #[case(KeyCode::Home, EditAction::Edit(Edit::Move(Motion::Home)))]
    #[case(KeyCode::End, EditAction::Edit(Edit::Move(Motion::End)))]
    fn plain_keys_map_to_their_action(#[case] code: KeyCode, #[case] expected: EditAction) {
        assert_eq!(action_for(key(code)), expected);
    }

    #[test]
    fn alt_enter_inserts_a_newline_and_enter_submits() {
        let alt_enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::ALT);
        assert_eq!(action_for(alt_enter), EditAction::Edit(Edit::Newline));
        assert_eq!(action_for(key(KeyCode::Enter)), EditAction::Submit);
    }

    #[test]
    fn control_c_cancels() {
        assert_eq!(action_for(ctrl('c')), EditAction::Cancel);
    }

    /// The whole query-line keymap, not a subset. ENGINEERING §7.2.
    #[rstest]
    #[case(ctrl('a'), Edit::Move(Motion::Home))]
    #[case(ctrl('e'), Edit::Move(Motion::End))]
    #[case(ctrl('b'), Edit::Move(Motion::Left))]
    #[case(ctrl('f'), Edit::Move(Motion::Right))]
    #[case(ctrl('h'), Edit::DeleteBackward)]
    #[case(ctrl('d'), Edit::DeleteForward)]
    #[case(ctrl('w'), Edit::DeleteWordBackward)]
    #[case(ctrl('u'), Edit::DeleteToStart)]
    #[case(ctrl('y'), Edit::Yank)]
    #[case(alt('b'), Edit::Move(Motion::WordLeft))]
    #[case(alt('f'), Edit::Move(Motion::WordRight))]
    #[case(alt('d'), Edit::DeleteWordForward)]
    fn the_editor_shares_the_query_lines_bindings(#[case] key: KeyEvent, #[case] expected: Edit) {
        assert_eq!(action_for(key), EditAction::Edit(expected));
    }

    /// In the editor these move between lines, where the picker moves in the
    /// candidate list.
    #[rstest]
    #[case(key(KeyCode::Up), Motion::Up)]
    #[case(key(KeyCode::Down), Motion::Down)]
    #[case(ctrl('p'), Motion::Up)]
    #[case(ctrl('n'), Motion::Down)]
    fn arrows_move_between_lines(#[case] key: KeyEvent, #[case] expected: Motion) {
        assert_eq!(action_for(key), EditAction::Edit(Edit::Move(expected)));
    }

    #[test]
    fn a_full_edit_sequence_produces_the_expected_source() {
        let mut buffer = TextBuffer::new("echo ran-alpha");
        for key in [ctrl('a'), key(KeyCode::Char('#')), key(KeyCode::Char(' '))] {
            if let EditAction::Edit(edit) = action_for(key) {
                buffer.apply(&edit);
            }
        }
        assert_eq!(buffer.text(), "# echo ran-alpha");
    }

    fn draw(buffer: &TextBuffer) -> (String, Option<(u16, u16)>) {
        let backend = TestBackend::new(40, 6);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|frame| render(buffer, frame)).unwrap();
        let cursor = terminal.get_cursor_position().ok().map(|p| (p.x, p.y));
        (format!("{}", terminal.backend()), cursor)
    }

    #[test]
    fn renders_a_single_line_source() {
        let (frame, _) = draw(&TextBuffer::new("git ls-files"));
        insta::assert_snapshot!(frame);
    }

    #[test]
    fn renders_a_multi_line_source() {
        let (frame, _) = draw(&TextBuffer::new("set -e\necho one\necho two"));
        insta::assert_snapshot!(frame);
    }

    #[test]
    fn the_cursor_sits_where_the_buffer_says() {
        let (_, cursor) = draw(&TextBuffer::new("echo hi"));
        assert_eq!(cursor, Some((7, 1)));

        let mut buffer = TextBuffer::new("echo hi");
        buffer.apply(&Edit::Move(Motion::Home));
        let (_, cursor) = draw(&buffer);
        assert_eq!(cursor, Some((0, 1)));
    }

    #[test]
    fn the_cursor_follows_the_row_in_a_multi_line_buffer() {
        let (_, cursor) = draw(&TextBuffer::new("one\ntwo"));
        assert_eq!(cursor, Some((3, 2)));
    }
}
