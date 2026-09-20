//! The outcome of a pick. SPEC §8.2, ENGINEERING §4.1.

/// Which binding confirmed the selection. SPEC §8.2.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum SelectionType {
    /// `Enter`: run it.
    #[default]
    Default,
    /// `Alt-Enter`: edit the source before running.
    Edit,
    /// `F1`: print the source.
    Show,
    /// `F2`: open it in `$EDITOR`.
    Visit,
}

/// What the picker returned. SPEC §8.2.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Selection<T> {
    /// Nothing matched.
    Empty,
    /// The user cancelled with `Esc` or `Ctrl-C`. Exits 130. ENGINEERING §7.2.
    Canceled,
    /// One or more rows were chosen.
    Selected {
        /// Which binding confirmed it.
        kind: SelectionType,
        /// The chosen rows, in the order they appear.
        items: Vec<T>,
    },
}

impl<T> Selection<T> {
    /// A selection with no items is [`Selection::Empty`]. SPEC §8.2.
    pub fn selected(kind: SelectionType, items: Vec<T>) -> Self {
        if items.is_empty() {
            Self::Empty
        } else {
            Self::Selected { kind, items }
        }
    }

    /// The chosen rows, or none if empty or cancelled.
    pub fn items(&self) -> &[T] {
        match self {
            Self::Selected { items, .. } => items,
            _ => &[],
        }
    }

    /// Applies `f` to each chosen row.
    pub fn map<U>(self, f: impl FnMut(T) -> U) -> Selection<U> {
        match self {
            Self::Empty => Selection::Empty,
            Self::Canceled => Selection::Canceled,
            Self::Selected { kind, items } => Selection::Selected {
                kind,
                items: items.into_iter().map(f).collect(),
            },
        }
    }
}
