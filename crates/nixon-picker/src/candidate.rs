//! What the picker offers and what selecting it yields. ENGINEERING §4.1.

/// A selectable row.
///
/// `display` is what the user sees and may carry ANSI escapes; `value` is
/// what selection returns, with escapes stripped. SPEC §8.4.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Candidate {
    /// Text shown in the picker. May contain ANSI escapes.
    pub display: String,
    /// Text returned when the row is selected.
    pub value: String,
}

impl Candidate {
    /// A candidate whose displayed text is also its value.
    pub fn identity(text: impl Into<String>) -> Self {
        let text = text.into();
        Self {
            display: text.clone(),
            value: text,
        }
    }

    /// A candidate shown as `display` but returning `value`. SPEC §8.2.
    pub fn with_title(display: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            display: display.into(),
            value: value.into(),
        }
    }
}
