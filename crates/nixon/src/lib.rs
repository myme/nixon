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

use std::process::ExitCode;

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

use nixon_picker::matcher::MatchOptions;

/// Turns the config's matching flags into the picker's.
pub fn matcher_options(config: &config::Config) -> MatchOptions {
    MatchOptions {
        exact: config.exact_match.unwrap_or(false),
        ignore_case: config.ignore_case.unwrap_or(false),
        sort: true,
    }
}

/// Runs nixon. Subcommand dispatch lands with
#[expect(
    clippy::missing_const_for_fn,
    reason = "const only because the body is still empty"
)]
pub fn run() -> ExitCode {
    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use std::process::ExitCode;

    use super::run;

    #[test]
    fn run_succeeds_while_there_is_nothing_to_do() {
        assert_eq!(format!("{:?}", run()), format!("{:?}", ExitCode::SUCCESS));
    }
}
