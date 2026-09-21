//! Argument parsing.

use std::path::PathBuf;

use clap::{ArgAction, Args, CommandFactory as _, FromArgMatches as _, Parser, Subcommand};
use nixon::config::{Config, LogLevel};
use nixon::language::Language;
use nixon::placeholder::{Placeholder, parse_one};

/// Command & environment launcher.
#[derive(Debug, Parser)]
#[command(
    name = "nixon",
    version,
    about = "Command & environment launcher",
    after_help = "See nixon.md(5), nixon-picker(7), nixon-shell(7)."
)]
pub struct Cli {
    /// Global options, which all come before the subcommand.
    #[command(flatten)]
    pub global: GlobalOpts,

    /// The subcommand; bare arguments are `run`'s.
    #[command(subcommand)]
    pub command: Option<Commands>,
}

/// Options that apply to every subcommand.
///
/// `-b/--backend`, `-t/--terminal` and `-T/--force-tty` went with the backend
/// concept; passing one is an ordinary unexpected-argument error.
#[derive(Debug, Args)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "each flag is a separate command-line option"
)]
pub struct GlobalOpts {
    /// Path to config file.
    #[arg(short = 'C', long, value_name = "CONFIG", help = config_help())]
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
    /// The config these options imply, to merge over the file's.
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

/// `--x` and `--no-x` collapse to a tri-state.
const fn tri_state(yes: bool, no: bool) -> Option<bool> {
    match (yes, no) {
        (true, _) => Some(true),
        (_, true) => Some(false),
        _ => None,
    }
}

/// The help for `-C`, naming the default path.
///
/// v1 computed it from XDG and `$HOME` at runtime and collapsed `$HOME` to
/// `~`; so does this.
fn config_help() -> String {
    let default = nixon::fs::Dirs::from_env().map_or_else(
        |_| PathBuf::from("$XDG_CONFIG_HOME/nixon.md"),
        |dirs| nixon::fs::implode_home(&dirs.global_config(), &dirs.home),
    );
    format!("Path to config file [default: {}]", default.display())
}

/// `debug`, `info`, `warning`/`warn`, `error`.
fn parse_log_level(value: &str) -> Result<LogLevel, String> {
    match value {
        "debug" => Ok(LogLevel::Debug),
        "info" => Ok(LogLevel::Info),
        "warning" | "warn" => Ok(LogLevel::Warning),
        "error" => Ok(LogLevel::Error),
        other => Err(format!("invalid log level: {other}")),
    }
}

/// The subcommands.
#[derive(Debug, Subcommand)]
pub enum Commands {
    /// Edit a command in `$EDITOR`.
    Edit {
        /// Command to edit.
        #[arg(add = clap_complete::ArgValueCompleter::new(crate::complete::command_names))]
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

    /// Show what has been run, and run it again.
    History(HistoryArgs),

    /// Insert a new command into a config file.
    New(NewArgs),

    /// Select a project and run a command in it.
    Project(ProjectArgs),

    /// Select and run a command.
    Run(RunArgs),

    /// Generators that are nixon's own business, not the user's.
    #[command(hide = true, subcommand)]
    Internal(Internal),

    /// Bare arguments are the `run` subcommand's.
    #[command(external_subcommand)]
    External(Vec<String>),
}

/// Hidden helpers, for packaging rather than for use.
#[derive(Debug, Subcommand)]
pub enum Internal {
    /// Write the `nixon(1)` man page to stdout.
    Mangen,
}

/// `nixon run`.
#[derive(Debug, Default, Args)]
pub struct RunArgs {
    /// The command to run, then its own arguments.
    // One positional rather than two, so everything after the name belongs
    // to the command and `nixon run c -i` means what `nixon c -i` means.
    // nixon's own flags go before the name.
    #[arg(
        value_name = "COMMAND",
        trailing_var_arg = true,
        allow_hyphen_values = true,
        add = clap_complete::ArgValueCompleter::new(crate::complete::run_args)
    )]
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

/// `nixon project`.
#[derive(Debug, Default, Args)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "each flag is a separate command-line option"
)]
pub struct ProjectArgs {
    /// Project to select.
    #[arg(add = clap_complete::ArgValueCompleter::new(crate::complete::project_names))]
    pub project: Option<String>,
    /// The command to run in it, then its own arguments.
    // One positional, as in `run`: everything after the command name is the
    // command's. A flag written straight after the project name is still
    // nixon's, because the command name has not arrived yet.
    #[arg(
        value_name = "COMMAND",
        trailing_var_arg = true,
        allow_hyphen_values = true,
        add = clap_complete::ArgValueCompleter::new(crate::complete::project_command_args)
    )]
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

/// `nixon eval`.
#[derive(Debug, Default, Args)]
pub struct EvalArgs {
    /// The expression to evaluate.
    #[arg(conflicts_with = "file")]
    pub command: Option<String>,
    /// Placeholders, each parsed with the placeholder grammar.
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

/// `nixon history`.
#[derive(Debug, Default, Args)]
pub struct HistoryArgs {
    /// Search query for the picker, or the filter for `--list`.
    pub query: Option<String>,
    /// Print matching invocations.
    #[arg(short = 'l', long)]
    pub list: bool,
    /// Pick one and print it instead of running it.
    #[arg(short = 's', long)]
    pub select: bool,
    /// Keep only the last N entries.
    #[arg(short = 'n', long, value_name = "N")]
    pub limit: Option<usize>,
    /// Empty the log.
    #[arg(long)]
    pub clear: bool,
}

/// `nixon new`.
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
#[expect(
    clippy::unnecessary_wraps,
    reason = "clap's value_parser requires a fallible signature"
)]
fn parse_language(value: &str) -> Result<Language, String> {
    Ok(Language::from(value))
}

/// A placeholder argument, e.g. `'${git-files:m}'`.
fn parse_placeholder(value: &str) -> Result<Placeholder, String> {
    parse_one(value).map_err(|err| err.to_string())
}

impl Cli {
    /// Parses words that may be an incomplete command line, for completion.
    pub fn try_parse_from_words(words: &[std::path::PathBuf]) -> Option<Self> {
        Self::command()
            .try_get_matches_from_mut(words)
            .ok()
            .and_then(|matches| Self::from_arg_matches(&matches).ok())
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use clap::Parser as _;
    use nixon::config::LogLevel;

    use super::{Cli, Commands};

    fn parse(args: &[&str]) -> Cli {
        Cli::try_parse_from(args).unwrap()
    }

    #[test]
    fn tri_state_flags_are_unset_by_default() {
        let config = parse(&["nixon"]).global.to_config();
        assert_eq!(config.exact_match, None);
        assert_eq!(config.ignore_case, None);
        assert_eq!(config.use_direnv, None);
        assert_eq!(config.use_nix, None);
    }

    #[test]
    fn tri_state_flags_can_be_turned_on_and_off() {
        let on = parse(&["nixon", "-e", "-i", "-d", "-n"]).global.to_config();
        assert_eq!(on.exact_match, Some(true));
        assert_eq!(on.ignore_case, Some(true));
        assert_eq!(on.use_direnv, Some(true));
        assert_eq!(on.use_nix, Some(true));

        let off = parse(&[
            "nixon",
            "--no-exact",
            "--no-ignore-case",
            "--no-direnv",
            "--no-nix",
        ])
        .global
        .to_config();
        assert_eq!(off.exact_match, Some(false));
        assert_eq!(off.ignore_case, Some(false));
        assert_eq!(off.use_direnv, Some(false));
        assert_eq!(off.use_nix, Some(false));
    }

    #[test]
    fn the_last_of_a_tri_state_pair_wins() {
        assert_eq!(
            parse(&["nixon", "-e", "--no-exact"])
                .global
                .to_config()
                .exact_match,
            Some(false)
        );
        assert_eq!(
            parse(&["nixon", "--no-exact", "-e"])
                .global
                .to_config()
                .exact_match,
            Some(true)
        );
    }

    #[test]
    fn path_is_repeatable_and_appends_to_project_dirs() {
        let config = parse(&["nixon", "-p", "/one", "--path", "/two"])
            .global
            .to_config();
        assert_eq!(
            config.project_dirs,
            ["/one", "/two"].map(PathBuf::from).to_vec()
        );
    }

    #[test]
    fn log_levels_include_both_spellings_of_warning() {
        for (text, expected) in [
            ("debug", LogLevel::Debug),
            ("info", LogLevel::Info),
            ("warning", LogLevel::Warning),
            ("warn", LogLevel::Warning),
            ("error", LogLevel::Error),
        ] {
            let config = parse(&["nixon", "-L", text]).global.to_config();
            assert_eq!(config.loglevel, Some(expected), "for {text}");
        }
        assert!(Cli::try_parse_from(["nixon", "-L", "nonsense"]).is_err());
    }

    #[test]
    fn bin_dirs_project_types_and_commands_are_not_settable_from_the_cli() {
        let config = parse(&["nixon"]).global.to_config();
        assert!(config.bin_dirs.is_empty());
        assert!(config.project_types.is_empty());
        assert!(config.commands.is_empty());
        for flag in ["--bin-dirs", "--project-types", "--commands"] {
            assert!(Cli::try_parse_from(["nixon", flag, "x"]).is_err());
        }
    }

    #[test]
    fn bare_arguments_become_the_run_subcommand() {
        let parsed = parse(&["nixon", "foo", "bar"]);
        match parsed.command {
            Some(Commands::External(args)) => assert_eq!(args, ["foo", "bar"]),
            other => panic!("expected an external subcommand, got {other:?}"),
        }
    }

    #[test]
    fn a_subcommand_keyword_is_never_a_command_name() {
        assert!(matches!(
            parse(&["nixon", "edit"]).command,
            Some(Commands::Edit { .. })
        ));
        assert!(matches!(
            parse(&["nixon", "gc"]).command,
            Some(Commands::Gc { .. })
        ));
    }

    #[test]
    fn eval_placeholders_are_parsed_with_the_grammar() {
        let parsed = parse(&["nixon", "eval", "vim \"$1\"", "${git-files:m}"]);
        let Some(Commands::Eval(args)) = parsed.command else {
            panic!("expected eval");
        };
        assert_eq!(args.placeholders.len(), 1);
        assert_eq!(args.placeholders[0].name, "git-files");
        assert!(args.placeholders[0].multiple);
    }

    #[test]
    fn an_unparsable_eval_placeholder_is_a_cli_error() {
        assert!(Cli::try_parse_from(["nixon", "eval", "x", "${unterminated"]).is_err());
    }

    #[test]
    fn new_has_the_documented_defaults() {
        let parsed = parse(&["nixon", "new"]);
        let Some(Commands::New(args)) = parsed.command else {
            panic!("expected new");
        };
        assert_eq!(args.name, "<name>");
        assert_eq!(args.desc, "Description…");
        assert_eq!(args.lang.to_string(), "bash");
        assert_eq!(args.src, "");
    }

    #[test]
    fn the_removed_backend_flags_do_not_parse() {
        for args in [
            vec!["nixon", "-b", "fzf"],
            vec!["nixon", "--backend", "rofi"],
            vec!["nixon", "-T"],
            vec!["nixon", "--force-tty"],
            vec!["nixon", "-t", "xterm"],
            vec!["nixon", "--terminal", "xterm"],
        ] {
            assert!(
                Cli::try_parse_from(&args).is_err(),
                "{args:?} should not parse"
            );
        }
    }
}
