//! How one pick is configured.

use crossterm::event::KeyEvent;

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
    /// fzf's `-1`: a query matching exactly one row selects it without
    /// drawing anything.
    pub select_one: bool,
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
}
