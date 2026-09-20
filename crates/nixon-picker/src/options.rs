//! How one pick is configured. ENGINEERING §4.1.

use crossterm::event::KeyEvent;

use crate::matcher::MatchOptions;
use crate::selection::SelectionType;

/// Everything that varies between picks. ENGINEERING §4.1.
#[derive(Clone, Debug, Default)]
pub struct PickerOptions {
    /// Shown above the list, e.g. `Select command [nixon] (/home/me/code)`.
    pub header: Option<String>,
    /// Pre-filled query. SPEC §5.6: a CLI arg is a query, not a value.
    pub initial_query: Option<String>,
    /// How the query is matched against candidates.
    pub matching: MatchOptions,
    /// Allow marking more than one row.
    pub multi: bool,
    /// Extra keys that confirm with a particular [`SelectionType`].
    pub expect: Vec<(KeyEvent, SelectionType)>,
    /// fzf's `-1`: a query matching exactly one row selects it without
    /// drawing anything. SPEC §8.4.
    pub select_one: bool,
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

    /// Enables fzf's `-1` behaviour.
    #[must_use]
    pub const fn select_one(mut self, select_one: bool) -> Self {
        self.select_one = select_one;
        self
    }
}
