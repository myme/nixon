//! Key bindings. ENGINEERING §7.2.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::selection::SelectionType;

/// What a key press means to the picker.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Action {
    /// Append a character to the query.
    Insert(char),
    /// Delete the character before the cursor.
    DeleteBackward,
    /// Delete the word before the cursor.
    DeleteWord,
    /// Clear the query.
    Clear,
    /// Move the selection up.
    MoveUp,
    /// Move the selection down.
    MoveDown,
    /// Mark or unmark the current row, when multi-select is on.
    ToggleMark,
    /// Confirm with this selection type.
    Confirm(SelectionType),
    /// Cancel the pick.
    Cancel,
    /// Nothing.
    Ignore,
}

/// Maps a key to an action, `expect` keys taking precedence. ENGINEERING §7.2.
pub fn action_for(key: KeyEvent, expect: &[(KeyEvent, SelectionType)]) -> Action {
    if let Some((_, kind)) = expect
        .iter()
        .find(|(expected, _)| expected.code == key.code && expected.modifiers == key.modifiers)
    {
        return Action::Confirm(*kind);
    }

    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
    let alt = key.modifiers.contains(KeyModifiers::ALT);

    match (key.code, ctrl, alt) {
        (KeyCode::Enter, _, true) => Action::Confirm(SelectionType::Edit),
        (KeyCode::Enter, _, false) => Action::Confirm(SelectionType::Default),
        (KeyCode::F(1), _, _) => Action::Confirm(SelectionType::Show),
        (KeyCode::F(2), _, _) => Action::Confirm(SelectionType::Visit),
        (KeyCode::Esc, _, _) | (KeyCode::Char('c' | 'g'), true, _) => Action::Cancel,
        (KeyCode::Tab, _, _) => Action::ToggleMark,
        (KeyCode::Up, _, _) | (KeyCode::Char('p'), true, _) => Action::MoveUp,
        (KeyCode::Down, _, _) | (KeyCode::Char('n'), true, _) => Action::MoveDown,
        (KeyCode::Backspace, _, _) | (KeyCode::Char('h'), true, _) => Action::DeleteBackward,
        (KeyCode::Char('w'), true, _) => Action::DeleteWord,
        (KeyCode::Char('u'), true, _) => Action::Clear,
        (KeyCode::Char(c), false, false) => Action::Insert(c),
        _ => Action::Ignore,
    }
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use rstest::rstest;

    use super::{Action, action_for};
    use crate::selection::SelectionType;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn ctrl(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    #[rstest]
    #[case(KeyCode::Enter, Action::Confirm(SelectionType::Default))]
    #[case(KeyCode::F(1), Action::Confirm(SelectionType::Show))]
    #[case(KeyCode::F(2), Action::Confirm(SelectionType::Visit))]
    #[case(KeyCode::Esc, Action::Cancel)]
    #[case(KeyCode::Tab, Action::ToggleMark)]
    #[case(KeyCode::Up, Action::MoveUp)]
    #[case(KeyCode::Down, Action::MoveDown)]
    #[case(KeyCode::Backspace, Action::DeleteBackward)]
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
    #[case('c', Action::Cancel)]
    #[case('n', Action::MoveDown)]
    #[case('p', Action::MoveUp)]
    #[case('u', Action::Clear)]
    #[case('w', Action::DeleteWord)]
    fn control_keys_map_to_their_action(#[case] c: char, #[case] expected: Action) {
        assert_eq!(action_for(ctrl(c), &[]), expected);
    }

    #[test]
    fn printable_characters_go_into_the_query() {
        assert_eq!(
            action_for(key(KeyCode::Char('x')), &[]),
            Action::Insert('x')
        );
    }

    #[test]
    fn an_expect_key_takes_precedence() {
        let expect = [(key(KeyCode::Tab), SelectionType::Show)];
        assert_eq!(
            action_for(key(KeyCode::Tab), &expect),
            Action::Confirm(SelectionType::Show)
        );
    }
}
