//! Generic fuzzy picker: terminal UI and matching.
//!
//! Knows nothing about commands, projects or markdown; nixon bridges its own
//! types to [`Candidate`] in `nixon::select`. See ENGINEERING §4.1 for the
//! module plan this crate grows into.

/// A selectable row: what the user sees, and what selecting it yields.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Candidate {
    /// Text shown in the picker. May contain ANSI escapes.
    pub display: String,
    /// Text returned when the row is selected, with ANSI escapes stripped.
    pub value: String,
}

impl Candidate {
    /// Builds a candidate whose displayed text is also its value.
    pub fn identity(text: impl Into<String>) -> Self {
        let text = text.into();
        Self {
            display: text.clone(),
            value: text,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Candidate;

    #[test]
    fn identity_candidate_displays_its_own_value() {
        let candidate = Candidate::identity("README.md");
        assert_eq!(candidate.display, candidate.value);
        assert_eq!(candidate.value, "README.md");
    }
}
