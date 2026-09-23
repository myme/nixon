//! How one pick is configured.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::matcher::MatchOptions;
use crate::selection::SelectionType;

/// A flag the user can toggle while making a selection.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PickerOption {
    /// What the row shows, e.g. `--force`.
    pub label: String,
    /// Shown beside the row when it has focus.
    pub description: Option<String>,
    /// Whether it is currently on.
    pub on: bool,
}

impl PickerOption {
    /// A toggle with no description.
    pub fn new(label: impl Into<String>, on: bool) -> Self {
        Self {
            label: label.into(),
            description: None,
            on,
        }
    }

    /// The same, with a description.
    #[must_use]
    pub fn describe(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }
}

/// One visible confirmation action and the key it sends to the picker.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PickerAction {
    /// Button text.
    pub label: String,
    /// The existing keyboard binding for this action.
    pub key: KeyEvent,
    /// The selection type returned by that binding.
    pub kind: SelectionType,
}

/// Everything that varies between picks.
#[derive(Clone, Debug, Default)]
pub struct PickerOptions {
    /// Shown above the list, e.g. `Select command [nixon] (/home/me/code)`.
    pub header: Option<String>,
    /// Pre-filled query: a command-line argument is a query, not a value.
    pub initial_query: Option<String>,
    /// How the query is matched against candidates.
    pub matching: MatchOptions,
    /// Allow marking more than one row.
    pub multi: bool,
    /// Extra keys that confirm with a particular [`SelectionType`].
    pub expect: Vec<(KeyEvent, SelectionType)>,
    /// Labels for confirmation actions. Availability still comes from `expect`.
    pub action_labels: Vec<(SelectionType, String)>,
    /// fzf's `-1`: a query matching exactly one row selects it without
    /// drawing anything.
    pub select_one: bool,
    /// A query equal to a candidate's value takes that candidate, whatever
    /// else it also matches.
    pub select_exact: bool,
    /// Flags shown in a row of their own, toggled while picking.
    pub options: Vec<PickerOption>,
}

impl PickerOptions {
    /// The header shown above the list.
    #[must_use]
    pub fn header(mut self, header: impl Into<String>) -> Self {
        self.header = Some(header.into());
        self
    }

    /// The query the picker opens with.
    #[must_use]
    pub fn query(mut self, query: impl Into<String>) -> Self {
        self.initial_query = Some(query.into());
        self
    }

    /// Allows marking more than one row.
    #[must_use]
    pub const fn multi(mut self, multi: bool) -> Self {
        self.multi = multi;
        self
    }

    /// Keeps the candidates' own order instead of ranking by score.
    #[must_use]
    pub const fn no_sort(mut self) -> Self {
        self.matching.sort = false;
        self
    }

    /// Adds a key that confirms with its own selection type.
    #[must_use]
    pub fn expect(mut self, key: KeyEvent, kind: SelectionType) -> Self {
        self.expect.push((key, kind));
        self
    }

    /// Names a confirmation action without changing its key or selection type.
    #[must_use]
    pub fn action_label(mut self, kind: SelectionType, label: impl Into<String>) -> Self {
        self.action_labels.push((kind, label.into()));
        self
    }

    /// Actions the GUI can offer as buttons, in keyboard binding order.
    pub fn actions(&self) -> Vec<PickerAction> {
        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE);
        let mut actions = Vec::with_capacity(self.expect.len() + 1);
        if !self
            .expect
            .iter()
            .any(|(key, _)| key.code == enter.code && key.modifiers == enter.modifiers)
        {
            actions.push(self.action(enter, SelectionType::Default));
        }
        actions.extend(
            self.expect
                .iter()
                .map(|(key, kind)| self.action(*key, *kind)),
        );
        actions
    }

    fn action(&self, key: KeyEvent, kind: SelectionType) -> PickerAction {
        let fallback = match kind {
            SelectionType::Default => "Select",
            SelectionType::Edit => "Edit",
            SelectionType::Show => "Show",
            SelectionType::Visit => "Visit",
        };
        let label = self
            .action_labels
            .iter()
            .rev()
            .find(|(type_, _)| *type_ == kind)
            .map_or_else(|| fallback.to_owned(), |(_, label)| label.clone());
        PickerAction { label, key, kind }
    }

    /// The toggles shown below the query.
    #[must_use]
    pub fn options(mut self, options: Vec<PickerOption>) -> Self {
        self.options = options;
        self
    }

    /// Enables fzf's `-1` behaviour.
    #[must_use]
    pub const fn select_one(mut self, select_one: bool) -> Self {
        self.select_one = select_one;
        self
    }

    /// Lets a query equal to a candidate's value settle the pick.
    #[must_use]
    pub const fn select_exact(mut self, select_exact: bool) -> Self {
        self.select_exact = select_exact;
        self
    }
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    use super::PickerOptions;
    use crate::selection::SelectionType;

    #[test]
    fn actions_follow_expect_bindings_and_label_overrides() {
        let options = PickerOptions::default()
            .action_label(SelectionType::Default, "Replay")
            .action_label(SelectionType::Show, "Inspect")
            .expect(
                KeyEvent::new(KeyCode::F(1), KeyModifiers::NONE),
                SelectionType::Show,
            );
        let actions = options.actions();
        assert_eq!(actions.len(), 2);
        assert_eq!(
            (actions[0].label.as_str(), actions[0].kind),
            ("Replay", SelectionType::Default)
        );
        assert_eq!(
            (actions[1].label.as_str(), actions[1].kind),
            ("Inspect", SelectionType::Show)
        );
        assert_eq!(actions[1].key.code, KeyCode::F(1));
    }

    #[test]
    fn expect_can_override_the_default_enter_action() {
        let options = PickerOptions::default().expect(
            KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE),
            SelectionType::Show,
        );
        let actions = options.actions();
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0].kind, SelectionType::Show);
    }
}
