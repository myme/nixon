//! Fuzzy matching.
//!
//! Interactive matching runs on nucleo's background worker, so the UI thread
//! never makes an O(n) pass over the candidates. The synchronous entry points
//! here are for the non-interactive paths and for tests.

use std::borrow::Cow;

use nucleo_matcher::pattern::{CaseMatching, Normalization, Pattern};
use nucleo_matcher::{Config, Matcher};

use crate::candidate::Candidate;

/// How a query is interpreted.
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
    ///
    /// Unset means fzf's smart case: a lowercase query ignores case, one
    /// with a capital in it does not. `Respect` would be the literal reading
    /// of the option, but nucleo 0.3 compares the whole haystack rather than
    /// the matched span when case is respected, so `^src` and `.md$` find
    /// nothing at all under it.
    pub const fn case_matching(self) -> CaseMatching {
        if self.ignore_case {
            CaseMatching::Ignore
        } else {
            CaseMatching::Smart
        }
    }

    /// The query as nucleo should read it, with `exact` applied.
    pub fn query(self, raw: &str) -> Cow<'_, str> {
        if self.exact {
            Cow::Owned(exact_query(raw))
        } else {
            Cow::Borrowed(raw)
        }
    }

    /// The pattern for a query, in fzf's extended syntax.
    pub fn pattern(self, query: &str) -> Pattern {
        Pattern::parse(
            &self.query(query),
            self.case_matching(),
            Normalization::Smart,
        )
    }
}

/// Rewrites a query so that its plain terms match as substrings.
///
/// This is fzf's `--exact`: a bare term matches literally and a leading `'`
/// is the way back to fuzzy. nucleo parses the operators but gives no way to
/// change an atom's kind afterwards, so the switch is made in the query.
/// `^foo` and `foo$` are already literal, so they are left alone, and so is
/// a term whose operator is backslash-escaped.
fn exact_query(query: &str) -> String {
    atoms(query).map(exact_atom).collect::<Vec<_>>().join(" ")
}

/// Terms, split on unescaped spaces, exactly as nucleo splits them.
fn atoms(query: &str) -> impl Iterator<Item = &str> {
    let mut escaped = false;
    query.split(move |c| {
        escaped = match c {
            ' ' if !escaped => return true,
            '\\' => true,
            _ => false,
        };
        false
    })
}

fn exact_atom(atom: &str) -> String {
    let (negate, term) = atom
        .strip_prefix('!')
        .map_or(("", atom), |rest| ("!", rest));

    if let Some(unquoted) = term.strip_prefix('\'') {
        // Quoted: under `exact` this is the escape hatch back to fuzzy.
        return format!("{negate}{unquoted}");
    }
    if term.is_empty() || term.starts_with(['^', '\\']) || ends_with_anchor(term) {
        return atom.to_owned();
    }
    format!("{negate}'{term}")
}

/// Whether a term ends in an unescaped `$`.
fn ends_with_anchor(term: &str) -> bool {
    term.ends_with('$') && !term.ends_with("\\$")
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

#[cfg(test)]
mod extended_syntax {
    use rstest::rstest;

    use super::{MatchOptions, exact_query, matches};
    use crate::candidate::Candidate;

    const ITEMS: [&str; 5] = [
        "src/main.rs",
        "src/matcher.rs",
        "README.md",
        "docs/picker.md",
        "main",
    ];

    fn matched(query: &str, opts: MatchOptions) -> Vec<String> {
        let candidates: Vec<Candidate> = ITEMS.iter().map(|s| Candidate::identity(*s)).collect();
        let mut found: Vec<String> = matches(query, &candidates, opts)
            .into_iter()
            .map(|m| candidates[m.index].value.clone())
            .collect();
        found.sort();
        found
    }

    #[rstest]
    #[case("^src", &["src/main.rs", "src/matcher.rs"])]
    #[case(".md$", &["README.md", "docs/picker.md"])]
    #[case("'main", &["main", "src/main.rs"])]
    #[case("!src", &["README.md", "docs/picker.md", "main"])]
    #[case("^src .rs$", &["src/main.rs", "src/matcher.rs"])]
    #[case("^src !matcher", &["src/main.rs"])]
    #[case("!^src !^docs", &["README.md", "main"])]
    fn operators_narrow_the_list(#[case] query: &str, #[case] expected: &[&str]) {
        let mut want: Vec<String> = expected.iter().map(|s| (*s).to_owned()).collect();
        want.sort();
        assert_eq!(matched(query, MatchOptions::default()), want);
    }

    #[test]
    fn a_suffix_is_a_suffix_not_a_literal_dollar() {
        // A single fuzzy atom matched `$` against the text and found nothing.
        assert_eq!(matched("rs$", MatchOptions::default()).len(), 2);
    }

    #[test]
    fn exact_makes_a_bare_term_a_substring() {
        let exact = MatchOptions {
            exact: true,
            ..MatchOptions::default()
        };
        // Fuzzy matches `s…c…r` as a subsequence; a substring does not.
        assert!(matched("scr", MatchOptions::default()).contains(&"src/matcher.rs".to_owned()));
        assert!(matched("scr", exact).is_empty());
        assert_eq!(
            matched("src", exact),
            ["src/main.rs", "src/matcher.rs"].map(ToOwned::to_owned)
        );
    }

    #[test]
    fn exact_leaves_the_operators_alone() {
        let exact = MatchOptions {
            exact: true,
            ..MatchOptions::default()
        };
        assert_eq!(matched("^src", exact).len(), 2);
        assert_eq!(matched(".md$", exact).len(), 2);
        assert_eq!(matched("!src", exact).len(), 3);
    }

    #[test]
    fn a_quoted_term_is_the_way_back_to_fuzzy_under_exact() {
        let exact = MatchOptions {
            exact: true,
            ..MatchOptions::default()
        };
        assert!(matched("'scr", exact).contains(&"src/matcher.rs".to_owned()));
    }

    #[rstest]
    #[case("foo", "'foo")]
    #[case("foo bar", "'foo 'bar")]
    #[case("^foo", "^foo")]
    #[case("foo$", "foo$")]
    #[case("!foo", "!'foo")]
    #[case("'foo", "foo")]
    #[case("!'foo", "!foo")]
    #[case("a\\ b", "'a\\ b")]
    fn exact_rewrites_only_the_plain_terms(#[case] raw: &str, #[case] expected: &str) {
        assert_eq!(exact_query(raw), expected);
    }
}
