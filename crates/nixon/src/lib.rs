//! Everything SPEC.md describes: config, markdown, projects, commands and
//! evaluation. The binary in `nixon-cli` is a thin shell over this crate.

use std::process::ExitCode;

pub mod language;
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
