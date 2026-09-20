//! Bridges nixon's types to the picker's [`Candidate`]. The only module that
//! knows both sides; nothing else in this crate names the picker.

use nixon_picker::Candidate;

/// Builds picker candidates from plain lines, as SPEC §5.6 `Lines` does.
pub fn line_candidates(lines: &[String]) -> Vec<Candidate> {
    lines.iter().map(Candidate::identity).collect()
}

#[cfg(test)]
mod tests {
    use super::line_candidates;

    #[test]
    fn lines_become_candidates_that_are_their_own_value() {
        let lines = vec!["src/main.rs".to_owned(), "README.md".to_owned()];
        let candidates = line_candidates(&lines);
        assert_eq!(candidates.len(), 2);
        assert_eq!(candidates[0].value, "src/main.rs");
        assert_eq!(candidates[1].display, "README.md");
    }
}
