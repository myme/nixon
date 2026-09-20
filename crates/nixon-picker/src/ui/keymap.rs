//! Key bindings. ENGINEERING §7.2.
//!
//! The query line follows readline/fzf defaults. Note that `Ctrl-J` and
//! `Ctrl-K` move in the list, as in fzf, rather than editing the line.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::selection::SelectionType;
use crate::textbuf::{Edit, Motion};

/// What a key press means to the picker.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Action {
    /// Change the query line.
    Edit(Edit),
    /// Move to the previous candidate.
    MoveUp,
    /// Move to the next candidate.
    MoveDown,
    /// Move a screenful towards the start.
    PageUp,
    /// Move a screenful towards the end.
    PageDown,
    /// Move half a screenful towards the start.
    HalfPageUp,
    /// Move half a screenful towards the end.
    HalfPageDown,
    /// Mark or unmark the current row and move down, in multi mode.
    ToggleMark,
    /// Mark or unmark the current row and move up, in multi mode.
    ToggleMarkUp,
    /// Confirm with this selection type.
    Confirm(SelectionType),
    /// Cancel the pick.
    Cancel,
    /// Nothing.
    Ignore,
}

/// Maps a key in the picker, `expect` keys taking precedence. ENGINEERING §7.2.
pub fn action_for(key: KeyEvent, expect: &[(KeyEvent, SelectionType)]) -> Action {
    if let Some((_, kind)) = expect
        .iter()
        .find(|(expected, _)| expected.code == key.code && expected.modifiers == key.modifiers)
    {
        return Action::Confirm(*kind);
    }

    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
    let alt = key.modifiers.contains(KeyModifiers::ALT);

    // Actions first, then list movement, then line editing.
    match (key.code, ctrl, alt) {
        (KeyCode::Enter, _, true) => return Action::Confirm(SelectionType::Edit),
        (KeyCode::Enter, _, false) => return Action::Confirm(SelectionType::Default),
        (KeyCode::F(1), _, _) => return Action::Confirm(SelectionType::Show),
        (KeyCode::F(2), _, _) => return Action::Confirm(SelectionType::Visit),
        (KeyCode::Esc, _, _) | (KeyCode::Char('c' | 'g'), true, _) => return Action::Cancel,
        // Both encodings of Shift-Tab reach us as BackTab: legacy `CSI Z`,
        // and kitty's `\t` with SHIFT set. Neither needs the modifier
        // checked here.
        (KeyCode::Tab, _, _) => return Action::ToggleMark,
        (KeyCode::BackTab, _, _) => return Action::ToggleMarkUp,

        // fzf binds Ctrl-J and Ctrl-K to list movement, not kill-line.
        (KeyCode::Down, _, _) | (KeyCode::Char('n' | 'j'), true, _) => return Action::MoveDown,
        (KeyCode::Up, _, _) | (KeyCode::Char('p' | 'k'), true, _) => return Action::MoveUp,
        // PgUp/PgDn are awkward on some layouts, so the emacs pair works
        // too, and Alt-J/Alt-K mirror Ctrl-J/Ctrl-K a half page at a time.
        // Ctrl-D and Ctrl-U are readline's, not fzf's, in the query line.
        (KeyCode::PageDown, _, _) | (KeyCode::Char('v'), true, _) => return Action::PageDown,
        (KeyCode::PageUp, _, _) | (KeyCode::Char('v'), _, true) => return Action::PageUp,
        (KeyCode::Char('j'), _, true) => return Action::HalfPageDown,
        (KeyCode::Char('k'), _, true) => return Action::HalfPageUp,

        _ => {}
    }

    line_edit(key).map_or(Action::Ignore, Action::Edit)
}

/// The readline bindings shared by the query line and the editor.
/// ENGINEERING §7.2.
pub const fn line_edit(key: KeyEvent) -> Option<Edit> {
    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
    let alt = key.modifiers.contains(KeyModifiers::ALT);

    let edit = match (key.code, ctrl, alt) {
        (KeyCode::Home, _, _) | (KeyCode::Char('a'), true, _) => Edit::Move(Motion::Home),
        (KeyCode::End, _, _) | (KeyCode::Char('e'), true, _) => Edit::Move(Motion::End),
        (KeyCode::Left | KeyCode::Char('b'), _, true) => Edit::Move(Motion::WordLeft),
        (KeyCode::Right | KeyCode::Char('f'), _, true) => Edit::Move(Motion::WordRight),
        (KeyCode::Left, _, _) | (KeyCode::Char('b'), true, _) => Edit::Move(Motion::Left),
        (KeyCode::Right, _, _) | (KeyCode::Char('f'), true, _) => Edit::Move(Motion::Right),

        // Alt-Backspace is the terminal's other spelling of Ctrl-W.
        (KeyCode::Backspace, _, true) | (KeyCode::Char('w'), true, _) => Edit::DeleteWordBackward,
        (KeyCode::Char('d'), _, true) => Edit::DeleteWordForward,
        (KeyCode::Backspace, _, _) | (KeyCode::Char('h'), true, _) => Edit::DeleteBackward,
        (KeyCode::Delete, _, _) | (KeyCode::Char('d'), true, _) => Edit::DeleteForward,
        (KeyCode::Char('u'), true, _) => Edit::DeleteToStart,
        (KeyCode::Char('y'), true, _) => Edit::Yank,

        (KeyCode::Char(c), false, false) => Edit::Insert(c),
        _ => return None,
    };
    Some(edit)
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use rstest::rstest;

    use super::{Action, action_for, line_edit};
    use crate::selection::SelectionType;
    use crate::textbuf::{Edit, Motion};

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
    #[case(KeyCode::Enter, Action::Confirm(SelectionType::Default))]
    #[case(KeyCode::F(1), Action::Confirm(SelectionType::Show))]
    #[case(KeyCode::F(2), Action::Confirm(SelectionType::Visit))]
    #[case(KeyCode::Esc, Action::Cancel)]
    #[case(KeyCode::Tab, Action::ToggleMark)]
    #[case(KeyCode::Up, Action::MoveUp)]
    #[case(KeyCode::Down, Action::MoveDown)]
    #[case(KeyCode::PageUp, Action::PageUp)]
    #[case(KeyCode::PageDown, Action::PageDown)]
    fn plain_keys_map_to_their_action(#[case] code: KeyCode, #[case] expected: Action) {
        assert_eq!(action_for(key(code), &[]), expected);
    }

    #[test]
    fn alt_enter_edits_before_running() {
        let alt_enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::ALT);
        assert_eq!(
            action_for(alt_enter, &[]),
            Action::Confirm(SelectionType::Edit)
        );
    }

    #[rstest]
    #[case('n', Action::MoveDown)]
    #[case('j', Action::MoveDown)]
    #[case('p', Action::MoveUp)]
    #[case('k', Action::MoveUp)]
    #[case('c', Action::Cancel)]
    fn control_keys_move_or_cancel(#[case] c: char, #[case] expected: Action) {
        assert_eq!(action_for(ctrl(c), &[]), expected);
    }

    #[rstest]
    #[case(ctrl('v'), Action::PageDown)]
    #[case(alt('v'), Action::PageUp)]
    #[case(alt('j'), Action::HalfPageDown)]
    #[case(alt('k'), Action::HalfPageUp)]
    fn the_page_keys_have_keyboard_friendly_equivalents(
        #[case] key: KeyEvent,
        #[case] expected: Action,
    ) {
        assert_eq!(action_for(key, &[]), expected);
    }

    #[test]
    fn control_j_and_k_move_the_list_rather_than_editing() {
        assert_eq!(action_for(ctrl('j'), &[]), Action::MoveDown);
        assert_eq!(action_for(ctrl('k'), &[]), Action::MoveUp);
    }

    #[rstest]
    #[case('a', Edit::Move(Motion::Home))]
    #[case('e', Edit::Move(Motion::End))]
    #[case('b', Edit::Move(Motion::Left))]
    #[case('f', Edit::Move(Motion::Right))]
    #[case('h', Edit::DeleteBackward)]
    #[case('d', Edit::DeleteForward)]
    #[case('w', Edit::DeleteWordBackward)]
    #[case('u', Edit::DeleteToStart)]
    #[case('y', Edit::Yank)]
    fn control_keys_edit_the_line(#[case] c: char, #[case] expected: Edit) {
        assert_eq!(line_edit(ctrl(c)), Some(expected.clone()));
        assert_eq!(action_for(ctrl(c), &[]), Action::Edit(expected));
    }

    #[rstest]
    #[case('b', Edit::Move(Motion::WordLeft))]
    #[case('f', Edit::Move(Motion::WordRight))]
    #[case('d', Edit::DeleteWordForward)]
    fn alt_keys_work_on_words(#[case] c: char, #[case] expected: Edit) {
        assert_eq!(line_edit(alt(c)), Some(expected));
    }

    #[rstest]
    #[case(KeyCode::Home, Edit::Move(Motion::Home))]
    #[case(KeyCode::End, Edit::Move(Motion::End))]
    #[case(KeyCode::Left, Edit::Move(Motion::Left))]
    #[case(KeyCode::Right, Edit::Move(Motion::Right))]
    #[case(KeyCode::Backspace, Edit::DeleteBackward)]
    #[case(KeyCode::Delete, Edit::DeleteForward)]
    fn named_keys_edit_the_line(#[case] code: KeyCode, #[case] expected: Edit) {
        assert_eq!(line_edit(key(code)), Some(expected));
    }

    #[test]
    fn alt_backspace_is_the_other_spelling_of_control_w() {
        let alt_backspace = KeyEvent::new(KeyCode::Backspace, KeyModifiers::ALT);
        assert_eq!(line_edit(alt_backspace), Some(Edit::DeleteWordBackward));
    }

    #[test]
    fn alt_arrows_move_by_word() {
        let alt_left = KeyEvent::new(KeyCode::Left, KeyModifiers::ALT);
        let alt_right = KeyEvent::new(KeyCode::Right, KeyModifiers::ALT);
        assert_eq!(line_edit(alt_left), Some(Edit::Move(Motion::WordLeft)));
        assert_eq!(line_edit(alt_right), Some(Edit::Move(Motion::WordRight)));
    }

    #[test]
    fn printable_characters_go_into_the_query() {
        assert_eq!(
            action_for(key(KeyCode::Char('x')), &[]),
            Action::Edit(Edit::Insert('x'))
        );
    }

    #[test]
    fn shift_tab_marks_upwards_in_both_terminal_encodings() {
        // Legacy terminals send `CSI Z`, which crossterm reports as BackTab
        // with SHIFT; the kitty protocol sends `\t` with SHIFT, which
        // crossterm also reports as BackTab.
        let legacy = KeyEvent::new(KeyCode::BackTab, KeyModifiers::SHIFT);
        assert_eq!(action_for(legacy, &[]), Action::ToggleMarkUp);

        let bare = KeyEvent::new(KeyCode::BackTab, KeyModifiers::NONE);
        assert_eq!(action_for(bare, &[]), Action::ToggleMarkUp);
    }

    #[test]
    fn tab_marks_downwards() {
        assert_eq!(action_for(key(KeyCode::Tab), &[]), Action::ToggleMark);
    }

    #[test]
    fn an_expect_key_takes_precedence() {
        let expect = [(key(KeyCode::Tab), SelectionType::Show)];
        assert_eq!(
            action_for(key(KeyCode::Tab), &expect),
            Action::Confirm(SelectionType::Show)
        );
    }

    #[test]
    fn an_unbound_key_is_ignored() {
        assert_eq!(action_for(key(KeyCode::Insert), &[]), Action::Ignore);
        assert_eq!(line_edit(key(KeyCode::Insert)), None);
    }
}
