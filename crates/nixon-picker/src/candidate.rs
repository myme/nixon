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
    ///
    /// The value has ANSI escapes stripped, as SPEC §8.4 requires.
    pub fn identity(text: impl Into<String>) -> Self {
        let display = text.into();
        let value = strip_ansi(&display);
        Self { display, value }
    }

    /// The displayed text without ANSI escapes: what the user actually sees.
    ///
    /// Matching and the highlight indices are relative to this, so a query
    /// can never match an escape sequence and the highlight lines up with
    /// the rendered characters.
    pub fn plain(&self) -> String {
        strip_ansi(&self.display)
    }

    /// A candidate shown as `display` but returning `value`. SPEC §8.2.
    pub fn with_title(display: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            display: display.into(),
            value: value.into(),
        }
    }
}

/// Removes ANSI escape sequences from `text`. SPEC §8.4.
fn strip_ansi(text: &str) -> String {
    String::from_utf8(strip_ansi_escapes::strip(text)).unwrap_or_else(|_| text.to_owned())
}

#[cfg(test)]
mod tests {
    use super::Candidate;

    #[test]
    fn an_identity_candidate_displays_its_own_value() {
        let candidate = Candidate::identity("README.md");
        assert_eq!(candidate.display, candidate.value);
        assert_eq!(candidate.value, "README.md");
    }

    #[test]
    fn ansi_is_kept_for_display_but_stripped_from_the_value() {
        let candidate = Candidate::identity("\u{1b}[32mgreen\u{1b}[0m branch");
        assert!(candidate.display.contains('\u{1b}'));
        assert_eq!(candidate.value, "green branch");
    }

    #[test]
    fn plain_text_is_what_the_user_sees() {
        let candidate = Candidate::with_title("\u{1b}[1mbold\u{1b}[0m item", "v");
        assert_eq!(candidate.plain(), "bold item");
        assert_eq!(candidate.value, "v");
    }
}
