//! Non-interactive matching, for the paths that never draw.

use crate::candidate::Candidate;
use crate::matcher::{MatchOptions, matches};

/// Candidates matching `query`, without a terminal.
///
/// Used by `--list` and by `| list` placeholders, which print matches rather
/// than opening the picker.
pub fn filter(query: &str, candidates: &[Candidate], opts: MatchOptions) -> Vec<Candidate> {
    matches(query, candidates, opts)
        .into_iter()
        .map(|m| candidates[m.index].clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::filter;
    use crate::candidate::Candidate;
    use crate::matcher::MatchOptions;

    fn candidates(items: &[&str]) -> Vec<Candidate> {
        items.iter().map(|s| Candidate::identity(*s)).collect()
    }

    #[test]
    fn an_empty_query_returns_everything() {
        let all = candidates(&["one", "two"]);
        assert_eq!(filter("", &all, MatchOptions::default()), all);
    }

    #[test]
    fn only_matches_are_returned() {
        let all = candidates(&["git-files", "deploy"]);
        let found = filter("git", &all, MatchOptions::default());
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].value, "git-files");
    }

    #[test]
    fn no_matches_is_an_empty_list() {
        let all = candidates(&["one", "two"]);
        assert!(filter("zzz", &all, MatchOptions::default()).is_empty());
    }

    #[test]
    fn values_are_preserved_not_just_titles() {
        let all = vec![Candidate::with_title("README.md  (docs)", "README.md")];
        let found = filter("README", &all, MatchOptions::default());
        assert_eq!(found[0].value, "README.md");
    }
}
