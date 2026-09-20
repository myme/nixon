//! Argument parsing. SPEC §2, ENGINEERING §7.2.

use std::path::PathBuf;

use clap::{ArgAction, Args, Parser, Subcommand};
use nixon::config::{Config, LogLevel};
use nixon::language::Language;
use nixon::placeholder::{Placeholder, parse_one};

/// Command & environment launcher.
#[derive(Debug, Parser)]
#[command(name = "nixon", version, about = "Command & environment launcher")]
pub struct Cli {
    /// Global options, which all come before the subcommand.
    #[command(flatten)]
    pub global: GlobalOpts,

    /// The subcommand; bare arguments are `run`'s. SPEC §2.2.
    #[command(subcommand)]
    pub command: Option<Commands>,
}

/// Options that apply to every subcommand. SPEC §2.1.
///
/// `-b/--backend`, `-t/--terminal` and `-T/--force-tty` went with the backend
/// concept; passing one is an ordinary unexpected-argument error.
/// ENGINEERING §7.2.
#[derive(Debug, Args)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "each flag is a separate command-line option"
)]
pub struct GlobalOpts {
    /// Path to config file.
    #[arg(short = 'C', long, value_name = "CONFIG")]
    pub config: Option<PathBuf>,

    /// Exact match in the selector.
    #[arg(short = 'e', long, action = ArgAction::SetTrue, overrides_with = "no_exact")]
    pub exact: bool,
    /// Turn off exact matching.
    #[arg(long, action = ArgAction::SetTrue, overrides_with = "exact", hide = true)]
    pub no_exact: bool,

    /// Case-insensitive match in the selector.
    #[arg(short = 'i', long, action = ArgAction::SetTrue, overrides_with = "no_ignore_case")]
    pub ignore_case: bool,
    /// Turn off case-insensitive matching.
    #[arg(long, action = ArgAction::SetTrue, overrides_with = "ignore_case", hide = true)]
    pub no_ignore_case: bool,

    /// Project directory, repeatable.
    #[arg(short = 'p', long = "path", value_name = "PATH")]
    pub paths: Vec<PathBuf>,

    /// Run commands through `direnv exec`.
    #[arg(short = 'd', long, action = ArgAction::SetTrue, overrides_with = "no_direnv")]
    pub direnv: bool,
    /// Do not use direnv.
    #[arg(long, action = ArgAction::SetTrue, overrides_with = "direnv", hide = true)]
    pub no_direnv: bool,

    /// Run commands through `nix-shell`.
    #[arg(short = 'n', long, action = ArgAction::SetTrue, overrides_with = "no_nix")]
    pub nix: bool,
    /// Do not use nix.
    #[arg(long, action = ArgAction::SetTrue, overrides_with = "nix", hide = true)]
    pub no_nix: bool,

    /// Log level.
    #[arg(short = 'L', long, value_name = "LOGLEVEL", value_parser = parse_log_level)]
    pub loglevel: Option<LogLevel>,
}

impl GlobalOpts {
    /// The config these options imply, to merge over the file's. SPEC §2.3.
    pub fn to_config(&self) -> Config {
        Config {
            project_dirs: self.paths.clone(),
            exact_match: tri_state(self.exact, self.no_exact),
            ignore_case: tri_state(self.ignore_case, self.no_ignore_case),
            use_direnv: tri_state(self.direnv, self.no_direnv),
            use_nix: tri_state(self.nix, self.no_nix),
            loglevel: self.loglevel,
            ..Config::default()
        }
    }
}

/// `--x` and `--no-x` collapse to a tri-state. SPEC §2.1.
const fn tri_state(yes: bool, no: bool) -> Option<bool> {
    match (yes, no) {
        (true, _) => Some(true),
        (_, true) => Some(false),
        _ => None,
    }
}

/// `debug`, `info`, `warning`/`warn`, `error`. SPEC §2.1.
fn parse_log_level(value: &str) -> Result<LogLevel, String> {
    match value {
        "debug" => Ok(LogLevel::Debug),
        "info" => Ok(LogLevel::Info),
        "warning" | "warn" => Ok(LogLevel::Warning),
        "error" => Ok(LogLevel::Error),
        other => Err(format!("invalid log level: {other}")),
    }
}

/// The subcommands. SPEC §2.2.
#[derive(Debug, Subcommand)]
pub enum Commands {
    /// Edit a command in `$EDITOR`.
    Edit {
        /// Command to edit.
        command: Option<String>,
    },

    /// Evaluate an expression.
    Eval(EvalArgs),

    /// Garbage collect cached scripts.
    Gc {
        /// Print what would be removed without removing it.
        #[arg(short = 'd', long)]
        dry_run: bool,
    },

    /// Insert a new command into a config file.
    New(NewArgs),

    /// Select a project and run a command in it.
    Project(ProjectArgs),

    /// Select and run a command.
    Run(RunArgs),

    /// Bare arguments are the `run` subcommand's. SPEC §2.2.
    #[command(external_subcommand)]
    External(Vec<String>),
}

/// `nixon run`. SPEC §2.2.
#[derive(Debug, Default, Args)]
pub struct RunArgs {
    /// Command to run.
    pub command: Option<String>,
    /// Arguments to the command.
    pub args: Vec<String>,
    /// Select a command and output its source.
    #[arg(short = 'i', long)]
    pub insert: bool,
    /// List commands.
    #[arg(short = 'l', long)]
    pub list: bool,
    /// Output command selection on stdout.
    #[arg(short = 's', long)]
    pub select: bool,
}

/// `nixon project`. SPEC §2.2.
#[derive(Debug, Default, Args)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "each flag is a separate command-line option"
)]
pub struct ProjectArgs {
    /// Project to select.
    pub project: Option<String>,
    /// Command to run in it.
    pub command: Option<String>,
    /// Arguments to the command.
    pub args: Vec<String>,
    /// Select a project command and output its source.
    #[arg(short = 'i', long)]
    pub insert: bool,
    /// Select a project and list some info about it.
    #[arg(short = 'I', long)]
    pub inspect: bool,
    /// List projects.
    #[arg(short = 'l', long)]
    pub list: bool,
    /// Select a project and output on stdout.
    #[arg(short = 's', long)]
    pub select: bool,
}

/// `nixon eval`. SPEC §2.2.
#[derive(Debug, Default, Args)]
pub struct EvalArgs {
    /// The expression to evaluate.
    #[arg(conflicts_with = "file")]
    pub command: Option<String>,
    /// Placeholders, each parsed with the placeholder grammar. SPEC §5.3.
    #[arg(value_parser = parse_placeholder)]
    pub placeholders: Vec<Placeholder>,
    /// Read the expression from a file instead.
    #[arg(short = 'f', long)]
    pub file: Option<PathBuf>,
    /// Language of the expression.
    #[arg(short = 'l', long, value_parser = parse_language)]
    pub language: Option<Language>,
    /// Select a project instead of using the current directory.
    #[arg(short = 'p', long)]
    pub project: bool,
}

/// `nixon new`. SPEC §2.2.
#[derive(Debug, Args)]
pub struct NewArgs {
    /// Name of the command.
    #[arg(short = 'n', long, default_value = "<name>")]
    pub name: String,
    /// Description of the command.
    #[arg(short = 'd', long, default_value = "Description…")]
    pub desc: String,
    /// Language of the command.
    #[arg(short = 'l', long, default_value = "bash", value_parser = parse_language)]
    pub lang: Language,
    /// Source of the command.
    #[arg(short = 's', long, default_value = "")]
    pub src: String,
}

/// Any string is a language; unknown ones simply have no interpreter.
/// SPEC §7.1.
#[expect(
    clippy::unnecessary_wraps,
    reason = "clap's value_parser requires a fallible signature"
)]
fn parse_language(value: &str) -> Result<Language, String> {
    Ok(Language::from(value))
}

/// A placeholder argument, e.g. `'${git-files:m}'`. SPEC §2.2.
fn parse_placeholder(value: &str) -> Result<Placeholder, String> {
    parse_one(value).map_err(|err| err.to_string())
}
