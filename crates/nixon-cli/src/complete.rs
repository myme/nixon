//! Shell completion. SPEC §2.4, ENGINEERING §2.1.
//!
//! `clap_complete`'s `CompleteEnv` re-invokes the binary at Tab time, which
//! is what v1's optparse-applicative completer did.

use clap::CommandFactory as _;

use crate::cli::Cli;

/// Answers a completion request and exits, if this is one. SPEC §2.4.
///
/// Must run before anything writes to stdout.
pub fn maybe_complete() {
    clap_complete::CompleteEnv::with_factory(Cli::command).complete();
}
