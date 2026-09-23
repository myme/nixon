//! Pure menu navigation over Nixon's validated launcher configuration.

use nixon::config::launcher::{LauncherAction, LauncherConfig, MenuItem};

/// The widget that owns the incoming key.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputFocus {
    /// Menu mnemonics are active.
    Menu,
    /// A text field handles typing and deletion.
    TextInput,
}

/// A key relevant to the menu layer.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MenuInput {
    /// A printable key, including space.
    Character(char),
    /// The Backspace key.
    Backspace,
    /// Ctrl-H, equivalent to Backspace in a menu.
    CtrlH,
    /// The Escape key.
    Escape,
    /// Ctrl-C closes the launcher.
    CtrlC,
}

/// The result of routing a key through the menu.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MenuOutcome {
    /// The key belongs to another widget or has no matching item.
    Ignored,
    /// A submenu became current.
    EnteredSubmenu,
    /// Navigation returned to the parent menu.
    Back,
    /// The text field should release focus without changing menus.
    LeaveTextInput,
    /// A leaf was chosen; execution belongs to the caller.
    Action(LauncherAction),
    /// The launcher should close.
    Close,
}

/// The current menu and its path from the root.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MenuState {
    root: Vec<MenuItem>,
    path: Vec<usize>,
}

impl MenuState {
    /// Starts at a resolved launcher's root menu.
    #[must_use]
    pub fn new(config: &LauncherConfig) -> Option<Self> {
        let root = config.items.as_ref().filter(|items| !items.is_empty())?;
        Some(Self {
            root: root.clone(),
            path: Vec::new(),
        })
    }

    /// The entries visible at the current menu level.
    #[must_use]
    pub fn items(&self) -> &[MenuItem] {
        let mut items = self.root.as_slice();
        for index in &self.path {
            let Some(MenuItem::Submenu {
                items: children, ..
            }) = items.get(*index)
            else {
                return &[];
            };
            items = children;
        }
        items
    }

    /// The number of submenus entered from the root.
    #[must_use]
    pub const fn depth(&self) -> usize {
        self.path.len()
    }

    /// Routes a key without running the selected action.
    pub fn handle(&mut self, input: MenuInput, focus: InputFocus) -> MenuOutcome {
        if focus == InputFocus::TextInput {
            return match input {
                MenuInput::Escape => MenuOutcome::LeaveTextInput,
                MenuInput::CtrlC => MenuOutcome::Close,
                _ => MenuOutcome::Ignored,
            };
        }

        match input {
            MenuInput::Backspace | MenuInput::CtrlH | MenuInput::Escape => {
                if self.path.pop().is_some() {
                    MenuOutcome::Back
                } else {
                    MenuOutcome::Close
                }
            }
            MenuInput::CtrlC => MenuOutcome::Close,
            MenuInput::Character(key) => self.choose(key),
        }
    }

    fn choose(&mut self, key: char) -> MenuOutcome {
        if !key.is_ascii_alphanumeric() && key != ' ' {
            return MenuOutcome::Ignored;
        }
        let normalized = key.to_ascii_uppercase();
        let Some((index, item)) = self
            .items()
            .iter()
            .enumerate()
            .find(|(_, item)| item.key().normalized() == normalized)
        else {
            return MenuOutcome::Ignored;
        };
        match item {
            MenuItem::Submenu { .. } => {
                self.path.push(index);
                MenuOutcome::EnteredSubmenu
            }
            MenuItem::Action { action, .. } => MenuOutcome::Action(action.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use nixon::config::launcher::{LauncherAction, LauncherConfig, MenuItem, MprisOperation};
    use nixon::config::{Config, parse_block};

    use super::{InputFocus, MenuInput, MenuOutcome, MenuState};

    fn defaults() -> MenuState {
        MenuState::new(&Config::defaults().launcher).unwrap()
    }

    fn press(state: &mut MenuState, input: MenuInput) -> MenuOutcome {
        state.handle(input, InputFocus::Menu)
    }

    #[test]
    fn root_starts_visible_and_leaf_actions_do_not_change_the_path() {
        let mut state = defaults();
        assert_eq!(state.depth(), 0);
        assert_eq!(state.items().len(), 5);
        assert_eq!(
            press(&mut state, MenuInput::Character('c')),
            MenuOutcome::Action(LauncherAction::Commands)
        );
        assert_eq!(state.depth(), 0);
    }

    #[test]
    fn submenu_navigation_and_back_follow_the_stack() {
        let mut state = defaults();
        assert_eq!(
            press(&mut state, MenuInput::Character('w')),
            MenuOutcome::EnteredSubmenu
        );
        assert_eq!(state.depth(), 1);
        assert_eq!(state.items().len(), 1);
        assert_eq!(
            press(&mut state, MenuInput::Character('O')),
            MenuOutcome::Action(LauncherAction::BrowserInput)
        );
        assert_eq!(state.depth(), 1);
        assert_eq!(press(&mut state, MenuInput::Backspace), MenuOutcome::Back);
        assert_eq!(state.depth(), 0);
        assert_eq!(press(&mut state, MenuInput::Backspace), MenuOutcome::Close);
    }

    #[test]
    fn nested_submenus_unwind_one_level_at_a_time() {
        let config = parse_block(
            "yaml",
            "launcher:\n  items:\n    - key: A\n      label: First\n      items:\n        - key: B\n          label: Second\n          items:\n            - key: C\n              label: Run\n              action: commands\n",
        )
        .unwrap();
        let mut state = MenuState::new(&config.launcher).unwrap();
        assert_eq!(
            press(&mut state, MenuInput::Character('a')),
            MenuOutcome::EnteredSubmenu
        );
        assert_eq!(
            press(&mut state, MenuInput::Character('b')),
            MenuOutcome::EnteredSubmenu
        );
        assert_eq!(state.depth(), 2);
        assert_eq!(
            press(&mut state, MenuInput::Character('c')),
            MenuOutcome::Action(LauncherAction::Commands)
        );
        assert_eq!(press(&mut state, MenuInput::Escape), MenuOutcome::Back);
        assert_eq!(state.depth(), 1);
        assert_eq!(press(&mut state, MenuInput::CtrlH), MenuOutcome::Back);
        assert_eq!(state.depth(), 0);
    }

    #[test]
    fn space_and_reused_sibling_keys_resolve_in_the_current_menu() {
        let mut state = defaults();
        assert_eq!(
            press(&mut state, MenuInput::Character('s')),
            MenuOutcome::EnteredSubmenu
        );
        assert_eq!(
            press(&mut state, MenuInput::Character(' ')),
            MenuOutcome::Action(LauncherAction::Mpris {
                operation: MprisOperation::PlayPause,
                player: "spotify".to_owned(),
            })
        );
        assert_eq!(
            press(&mut state, MenuInput::Character('p')),
            MenuOutcome::Action(LauncherAction::Mpris {
                operation: MprisOperation::Previous,
                player: "spotify".to_owned(),
            })
        );
        assert_eq!(press(&mut state, MenuInput::CtrlH), MenuOutcome::Back);
        assert_eq!(
            press(&mut state, MenuInput::Character('P')),
            MenuOutcome::Action(LauncherAction::Projects)
        );
    }

    #[test]
    fn configured_lowercase_letter_and_digit_are_mnemonics() {
        let config = parse_block(
            "yaml",
            "launcher:\n  items:\n    - key: e\n      label: Edit\n      action: { command: edit }\n    - key: 7\n      label: Seven\n      action: history\n",
        )
        .unwrap();
        let mut state = MenuState::new(&config.launcher).unwrap();
        assert_eq!(
            press(&mut state, MenuInput::Character('E')),
            MenuOutcome::Action(LauncherAction::Command {
                name: "edit".to_owned(),
                project: None,
            })
        );
        assert_eq!(
            press(&mut state, MenuInput::Character('7')),
            MenuOutcome::Action(LauncherAction::History)
        );
        assert_eq!(
            press(&mut state, MenuInput::Character('é')),
            MenuOutcome::Ignored
        );
        assert_eq!(
            press(&mut state, MenuInput::Character('q')),
            MenuOutcome::Ignored
        );
    }

    #[test]
    fn text_focus_owns_printable_keys_and_backspace() {
        let mut state = defaults();
        for input in [
            MenuInput::Character('C'),
            MenuInput::Character(' '),
            MenuInput::Backspace,
            MenuInput::CtrlH,
        ] {
            assert_eq!(
                state.handle(input, InputFocus::TextInput),
                MenuOutcome::Ignored
            );
        }
        assert_eq!(state.depth(), 0);
        assert_eq!(
            state.handle(MenuInput::Escape, InputFocus::TextInput),
            MenuOutcome::LeaveTextInput
        );
        assert_eq!(state.depth(), 0);
        assert_eq!(press(&mut state, MenuInput::Escape), MenuOutcome::Close);
        assert_eq!(
            state.handle(MenuInput::CtrlC, InputFocus::TextInput),
            MenuOutcome::Close
        );
    }

    #[test]
    fn escape_backs_out_one_level_and_ctrl_c_closes() {
        let mut state = defaults();
        assert_eq!(
            press(&mut state, MenuInput::Character('W')),
            MenuOutcome::EnteredSubmenu
        );
        assert_eq!(press(&mut state, MenuInput::Escape), MenuOutcome::Back);
        assert_eq!(press(&mut state, MenuInput::CtrlC), MenuOutcome::Close);
    }

    #[test]
    fn unresolved_or_empty_items_cannot_start_a_menu() {
        assert!(MenuState::new(&LauncherConfig::default()).is_none());
        assert!(
            MenuState::new(&LauncherConfig {
                items: Some(Vec::<MenuItem>::new()),
                ..LauncherConfig::default()
            })
            .is_none()
        );
    }
}
