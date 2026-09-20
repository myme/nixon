//! Everything nixon does: config, markdown, projects, commands and
//! evaluation. The binary in `nixon-cli` is a thin shell over this crate.

// Tests assert on known-good values, and placeholder syntax such as
// `${arg:m}` reads to clippy as a stray format argument.
#![cfg_attr(
    test,
    allow(
        clippy::unwrap_used,
        clippy::expect_used,
        clippy::panic,
        clippy::literal_string_with_formatting_args
    )
)]

pub mod app;
pub mod command;
pub mod config;
pub mod discover;
pub mod error;
pub mod eval;
pub mod format;
pub mod fs;
pub mod language;
pub mod markdown;
pub mod output;
pub mod placeholder;
pub mod process;
pub mod project;
pub mod resolve;
pub mod select;

use nixon_picker::matcher::{Case, MatchOptions};

/// Turns the config's matching flags into the picker's.
///
/// `ignore_case` is a tri-state, so all three of its values mean something:
/// unset is smart case, `-i` ignores case, `--no-ignore-case` respects it.
pub fn matcher_options(config: &config::Config) -> MatchOptions {
    MatchOptions {
        exact: config.exact_match.unwrap_or(false),
        case: match config.ignore_case {
            None => Case::Smart,
            Some(true) => Case::Ignore,
            Some(false) => Case::Respect,
        },
        sort: true,
    }
}

#[cfg(test)]
mod tests {
    use nixon_picker::matcher::Case;

    use super::matcher_options;
    use crate::config::Config;

    fn case_for(ignore_case: Option<bool>) -> Case {
        matcher_options(&Config {
            ignore_case,
            ..Config::default()
        })
        .case
    }

    /// All three values mean something: `--no-ignore-case` used to collapse
    /// into the unset case and do nothing at all.
    #[test]
    fn ignore_case_is_a_tri_state() {
        assert_eq!(case_for(None), Case::Smart);
        assert_eq!(case_for(Some(true)), Case::Ignore);
        assert_eq!(case_for(Some(false)), Case::Respect);
    }
}
