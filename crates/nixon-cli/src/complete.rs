//! Shell completion.
//!
//! `clap_complete`'s `CompleteEnv` re-invokes the binary at Tab time, which
//! is what v1's optparse-applicative completer did.

use std::ffi::OsStr;
use std::path::PathBuf;

use clap::CommandFactory as _;
use clap_complete::CompletionCandidate;
use nixon::app::{App, Environment};
use nixon::config::{Config, load};
use nixon::discover::find_project_commands;
use nixon::fs::Dirs;
use nixon::process::RealRunner;
use nixon_picker::FilterPicker;

use crate::cli::Cli;

/// Answers a completion request and exits, if this is one.
///
/// Must run before anything writes to stdout.
pub fn maybe_complete() {
    clap_complete::CompleteEnv::with_factory(Cli::command).complete();
}

/// Command names in the project the completion was requested from.
///
/// Completion runs in a fresh process with no state, so this rebuilds just
/// enough config to answer, and answers nothing rather than failing.
pub fn command_names(current: &OsStr) -> Vec<CompletionCandidate> {
    let Some(app) = completion_app() else {
        return Vec::new();
    };
    let project = app.current_project();
    let Ok(config) = app.config_for(&project) else {
        return Vec::new();
    };

    candidates(
        find_project_commands(&config, &project)
            .into_iter()
            .map(|command| command.name),
        current,
    )
}

/// Names of the projects discovery finds.
pub fn project_names(current: &OsStr) -> Vec<CompletionCandidate> {
    let Some(app) = completion_app() else {
        return Vec::new();
    };
    candidates(
        app.projects()
            .into_iter()
            .map(|project| project.name.to_string_lossy().into_owned()),
        current,
    )
}

/// Keeps the names that start with what has been typed.
fn candidates(names: impl Iterator<Item = String>, current: &OsStr) -> Vec<CompletionCandidate> {
    let prefix = current.to_string_lossy();
    names
        .filter(|name| name.starts_with(prefix.as_ref()))
        .map(CompletionCandidate::new)
        .collect()
}

/// A minimal app for answering a completion, or nothing if it cannot be built.
///
/// `-C` and `-p` on the completion line are honoured by re-parsing what the
/// shell passed, so `nixon -C other.md run <TAB>` completes against that
/// file.
fn completion_app() -> Option<App<FilterPicker, RealRunner>> {
    let dirs = Dirs::from_env().ok()?;
    let cli = partial_cli();

    let config_path = cli
        .as_ref()
        .and_then(|cli| cli.global.config.clone())
        .unwrap_or_else(|| dirs.global_config());
    let file_config = load::load_global(&config_path).unwrap_or_default();

    let cli_config = cli.map_or_else(Config::default, |cli| cli.global.to_config());
    let config = Config::defaults().merge(file_config).merge(cli_config);

    let env = Environment {
        cwd: std::env::current_dir().ok()?,
        ..Environment::default()
    };
    Some(App::new(config, dirs, env, FilterPicker, RealRunner))
}

/// Re-parses the words the shell is completing, for `-C` and `-p`.
///
/// The line is normally incomplete, so a parse failure is expected and just
/// means falling back to the defaults.
fn partial_cli() -> Option<Cli> {
    let words: Vec<PathBuf> = std::env::args_os()
        .take_while(|word| word != "--")
        .map(PathBuf::from)
        .collect();
    Cli::try_parse_from_words(&words)
}
