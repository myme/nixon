//! Everything SPEC.md describes: config, markdown, projects, commands and
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

pub mod command;
pub mod config;
pub mod discover;
pub mod error;
pub mod eval;
pub mod format;
pub mod fs;
pub mod language;
pub mod markdown;
pub mod placeholder;
pub mod process;
pub mod project;
pub mod select;

/// Runs nixon. Subcommand dispatch lands with SPEC §10.
#[expect(
    clippy::missing_const_for_fn,
    reason = "const only because the body is still empty; drop this with SPEC §10"
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
