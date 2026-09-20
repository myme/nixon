//! Fuzzy matching. ENGINEERING §2.1, §4.1.
//!
//! Interactive matching runs on nucleo's background worker, so the UI thread
//! never makes an O(n) pass over the candidates. The synchronous entry points
//! here are for the non-interactive paths and for tests.

use nucleo_matcher::pattern::{AtomKind, CaseMatching, Normalization, Pattern};
use nucleo_matcher::{Config, Matcher};

use crate::candidate::Candidate;

/// How a query is interpreted. ENGINEERING §7.1 decision 2.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct MatchOptions {
    /// Substring rather than fuzzy matching.
    pub exact: bool,
    /// Case-insensitive matching.
    pub ignore_case: bool,
    /// Rank by score; false keeps the candidates' own order.
    pub sort: bool,
}

impl Default for MatchOptions {
    fn default() -> Self {
        Self {
            exact: false,
            ignore_case: false,
            sort: true,
        }
    }
}

impl MatchOptions {
    /// How nucleo should treat case.
    pub const fn case_matching(self) -> CaseMatching {
        if self.ignore_case {
            CaseMatching::Ignore
        } else {
            CaseMatching::Respect
        }
    }

    /// Substring or fuzzy.
    pub const fn atom_kind(self) -> AtomKind {
        if self.exact {
            AtomKind::Substring
        } else {
            AtomKind::Fuzzy
        }
    }

    /// The pattern for a query.
    pub fn pattern(self, query: &str) -> Pattern {
        Pattern::new(
            query,
            self.case_matching(),
            Normalization::Smart,
            self.atom_kind(),
        )
    }
}

/// A candidate that matched, with its score and which characters matched.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Match {
    /// Index into the candidates that were searched.
    pub index: usize,
    /// Higher is a better match.
    pub score: u32,
    /// Character positions in the candidate's visible text that matched.
    pub indices: Vec<u32>,
}

/// Matches `query` against `candidates`, synchronously.
///
/// Used by the non-interactive paths, where the whole list is wanted at once
/// and there is no UI to keep responsive.
pub fn matches(query: &str, candidates: &[Candidate], opts: MatchOptions) -> Vec<Match> {
    if query.is_empty() {
        return candidates
            .iter()
            .enumerate()
            .map(|(index, _)| Match {
                index,
                ..Match::default()
            })
            .collect();
    }

    let mut matcher = Matcher::new(Config::DEFAULT);
    let pattern = opts.pattern(query);

    let mut found: Vec<Match> = candidates
        .iter()
        .enumerate()
        .filter_map(|(index, candidate)| {
            let plain = candidate.plain();
            let mut buf = Vec::new();
            let haystack = nucleo_matcher::Utf32Str::new(&plain, &mut buf);
            pattern.score(haystack, &mut matcher).map(|score| Match {
                index,
                score,
                indices: Vec::new(),
            })
        })
        .collect();

    if opts.sort {
        // Stable, so equal scores keep the candidates' own order.
        found.sort_by_key(|m| std::cmp::Reverse(m.score));
    }
    found
}

/// The characters of `text` that `query` matched, for highlighting.
///
/// Computed per visible row rather than for every match: a screenful is a few
/// dozen rows whatever the candidate count.
pub fn match_indices(matcher: &mut Matcher, pattern: &Pattern, text: &str, out: &mut Vec<u32>) {
    out.clear();
    let mut buf = Vec::new();
    let haystack = nucleo_matcher::Utf32Str::new(text, &mut buf);
    pattern.indices(haystack, matcher, out);
    out.sort_unstable();
    out.dedup();
}

#[cfg(test)]
mod tests {
    use super::{Match, MatchOptions, match_indices, matches};
    use crate::candidate::Candidate;
    use nucleo_matcher::{Config, Matcher};

    fn candidates(items: &[&str]) -> Vec<Candidate> {
        items.iter().map(|s| Candidate::identity(*s)).collect()
    }

    fn matched(query: &str, items: &[&str], opts: MatchOptions) -> Vec<String> {
        let candidates = candidates(items);
        matches(query, &candidates, opts)
            .into_iter()
            .map(|m| candidates[m.index].value.clone())
            .collect()
    }

    #[test]
    fn an_empty_query_matches_everything_in_order() {
        assert_eq!(
            matched("", &["one", "two", "three"], MatchOptions::default()),
            ["one", "two", "three"]
        );
    }

    #[test]
    fn a_fuzzy_query_matches_subsequences() {
        let found = matched(
            "gf",
            &["git-files", "rg-files", "deploy"],
            MatchOptions::default(),
        );
        assert!(found.contains(&"git-files".to_owned()));
        assert!(!found.contains(&"deploy".to_owned()));
    }

    #[test]
    fn an_exact_query_matches_substrings_only() {
        let opts = MatchOptions {
            exact: true,
            ..MatchOptions::default()
        };
        let found = matched("files", &["git-files", "fbles", "deploy"], opts);
        assert_eq!(found, ["git-files"]);
    }

    #[test]
    fn case_is_respected_by_default_and_ignored_on_request() {
        let respect = MatchOptions::default();
        assert!(matched("GIT", &["git-files"], respect).is_empty());

        let ignore = MatchOptions {
            ignore_case: true,
            ..MatchOptions::default()
        };
        assert_eq!(matched("GIT", &["git-files"], ignore), ["git-files"]);
    }

    #[test]
    fn without_sorting_matches_keep_their_own_order() {
        let opts = MatchOptions {
            sort: false,
            ..MatchOptions::default()
        };
        assert_eq!(matched("f", &["zf", "af", "mf"], opts), ["zf", "af", "mf"]);
    }

    #[test]
    fn nothing_matches_a_query_with_no_hits() {
        assert!(matched("zzz", &["one", "two"], MatchOptions::default()).is_empty());
    }

    #[test]
    fn match_indices_point_at_the_matched_characters() {
        let mut matcher = Matcher::new(Config::DEFAULT);
        let pattern = MatchOptions::default().pattern("gf");
        let mut out = Vec::new();
        match_indices(&mut matcher, &pattern, "git-files", &mut out);
        assert_eq!(out, [0, 4]);
    }

    #[test]
    fn match_indices_are_empty_when_nothing_matches() {
        let mut matcher = Matcher::new(Config::DEFAULT);
        let pattern = MatchOptions::default().pattern("zzz");
        let mut out = vec![9];
        match_indices(&mut matcher, &pattern, "git-files", &mut out);
        assert!(out.is_empty());
    }

    #[test]
    fn a_default_match_carries_no_indices() {
        assert!(Match::default().indices.is_empty());
    }
}
