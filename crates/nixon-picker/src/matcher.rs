//! Fuzzy matching over candidates. ENGINEERING §2.1, §4.1.

use nucleo_matcher::pattern::{CaseMatching, Normalization, Pattern};
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

/// A candidate that matched, with its score and which characters matched.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Match {
    /// Index into the candidates that were searched.
    pub index: usize,
    /// Higher is a better match.
    pub score: u32,
    /// Character positions in the candidate's display text that matched.
    pub indices: Vec<u32>,
}

/// Matches `query` against `candidates`, by their displayed text.
///
/// An empty query matches everything, in the candidates' own order. When
/// `sort` is set, matches are ranked by score, ties keeping input order.
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
    let case = if opts.ignore_case {
        CaseMatching::Ignore
    } else {
        CaseMatching::Respect
    };
    let kind = if opts.exact {
        nucleo_matcher::pattern::AtomKind::Substring
    } else {
        nucleo_matcher::pattern::AtomKind::Fuzzy
    };
    let pattern = Pattern::new(query, case, Normalization::Smart, kind);

    let mut found: Vec<Match> = candidates
        .iter()
        .enumerate()
        .filter_map(|(index, candidate)| {
            let plain = candidate.plain();
            let mut buf = Vec::new();
            let haystack = nucleo_matcher::Utf32Str::new(&plain, &mut buf);
            let mut indices = Vec::new();
            pattern
                .indices(haystack, &mut matcher, &mut indices)
                .map(|score| {
                    indices.sort_unstable();
                    indices.dedup();
                    Match {
                        index,
                        score,
                        indices: std::mem::take(&mut indices),
                    }
                })
        })
        .collect();

    if opts.sort {
        // Stable, so equal scores keep the candidates' own order.
        found.sort_by_key(|m| std::cmp::Reverse(m.score));
    }
    found
}

#[cfg(test)]
mod tests {
    use super::{MatchOptions, matches};
    use crate::candidate::Candidate;

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
}
