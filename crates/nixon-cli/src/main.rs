//! The `nixon` binary. Argument parsing, diagnostics and process exit live
//! here; everything else is in the `nixon` library.

// A binary has no external API, so `pub` on its private modules' items is
// how they are shared between those modules, not an exported surface.
#![allow(unreachable_pub)]
// Tests assert on known-good values.
#![cfg_attr(test, allow(clippy::unwrap_used, clippy::expect_used, clippy::panic))]

mod cli;
mod complete;
mod gui;
mod gui_exec;
mod gui_process;
mod mangen;

use std::process::ExitCode;

use clap::{CommandFactory as _, Parser as _};
use nixon::app::eval::EvalOpts;
use nixon::app::history::{HistoryOpts, Outcome};
use nixon::app::new::NewOpts;
use nixon::app::project::ProjectOpts;
use nixon::app::{App, Environment, RunOpts};
use nixon::config::{Config, LogLevel, load};
use nixon::error::{NixonError, Result};
use nixon::fs::Dirs;
use nixon::process::RealRunner;
use nixon_picker::TuiPicker;

use cli::{Cli, Commands, EvalArgs, HistoryArgs, Internal, Mode, ProjectArgs, RunArgs};

fn main() -> ExitCode {
    // Completion must answer before anything writes to stdout.
    complete::maybe_complete();

    match run() {
        Ok(code) => ExitCode::from(u8::try_from(code).unwrap_or(1)),
        Err(err) => {
            report(&err);
            ExitCode::from(u8::try_from(err.exit_code()).unwrap_or(1))
        }
    }
}

/// Prints an error as plain text on stderr.
fn report(err: &NixonError) {
    match err {
        // A parse error carries a position, so miette can show the source.
        NixonError::Markdown(markdown) => {
            tracing::error!(
                "{}:{} {}",
                markdown.file,
                markdown.line.unwrap_or(0),
                markdown.message
            );
        }
        other => tracing::error!("{other}"),
    }
}

fn run() -> Result<i32> {
    let parsed = Cli::parse();
    let cli_config = parsed.global.to_config();
    init_tracing(cli_config.loglevel);
    if let Some(Commands::Internal(Internal::GuiExec { payload_file })) = parsed.command.as_ref() {
        return gui_exec::run_payload(payload_file);
    }
    if parsed.global.mode == Mode::Gui {
        gui::reject_subcommand(parsed.command.as_ref())?;
    }
    let dirs =
        Dirs::from_env().map_err(|err| NixonError::Io(std::io::Error::other(err.to_string())))?;

    let config_path = parsed
        .global
        .config
        .clone()
        .unwrap_or_else(|| dirs.global_config());
    let file_config = load::load_global(&config_path)?;

    // Defaults, then the file, then the command line.
    let config = Config::defaults().merge(file_config).merge(cli_config);

    let env = Environment {
        cwd: std::env::current_dir()?,
        shell: std::env::var("SHELL").ok(),
        direnv_dir: std::env::var("DIRENV_DIR").ok(),
        editor: std::env::var("VISUAL")
            .or_else(|_| std::env::var("EDITOR"))
            .ok(),
        exe: std::env::current_exe().ok(),
    };

    if parsed.global.mode == Mode::Gui {
        return gui::run(config, dirs, env);
    }

    let mut app = App::new(config, dirs, env, TuiPicker, RealRunner);
    dispatch(&mut app, parsed.command)
}

/// Runs one subcommand.
fn dispatch(app: &mut App<TuiPicker, RealRunner>, command: Option<Commands>) -> Result<i32> {
    match command {
        None => app.run(&RunOpts::default()),
        Some(Commands::Run(args)) => app.run(&run_opts(args)),
        Some(Commands::External(args)) => app.run(&external_opts(args)),
        Some(Commands::Project(args)) => app.project(&project_opts(args)),
        Some(Commands::Eval(args)) => app.eval(&eval_opts(args)),
        Some(Commands::Edit { command }) => app.edit(command.as_deref()),
        Some(Commands::Gc { dry_run }) => app.gc(dry_run),
        Some(Commands::History(args)) => match app.history(&history_opts(args))? {
            Outcome::Done(code) => Ok(code),
            Outcome::Rerun(words) => rerun(app, words),
        },
        Some(Commands::Internal(Internal::Mangen)) => mangen::write_man_page(),
        Some(Commands::Internal(Internal::GuiExec { payload_file })) => {
            gui_exec::run_payload(&payload_file)
        }
        Some(Commands::New(args)) => app.new_command(&NewOpts {
            name: args.name,
            desc: args.desc,
            lang: args.lang,
            src: args.src,
        }),
    }
}

/// Runs a line from the history as if it had been typed.
///
/// Through the same parser, so the recorded options and values mean what
/// they meant the first time. A recorded `history` is refused rather than
/// looped: the picker is already open.
fn rerun(app: &mut App<TuiPicker, RealRunner>, recorded: Vec<String>) -> Result<i32> {
    let parsed = parse_history_cli(recorded)?;
    dispatch(app, parsed.command)
}

/// Parses a recorded invocation through the same command-line parser as a
/// fresh invocation, rejecting recursive history before either UI dispatches.
fn parse_history_cli(recorded: Vec<String>) -> Result<Cli> {
    let line = std::iter::once("nixon".to_owned()).chain(recorded);
    let parsed = Cli::try_parse_from(line)
        .map_err(|err| NixonError::NothingSelected(err.to_string().trim_end().to_owned()))?;

    if matches!(parsed.command, Some(Commands::History(_))) {
        return Err(NixonError::NothingSelected(
            "Refusing to run history from history.".to_owned(),
        ));
    }
    Ok(parsed)
}

fn history_opts(args: HistoryArgs) -> HistoryOpts {
    HistoryOpts {
        query: args.query,
        list: args.list,
        select: args.select,
        limit: args.limit,
        clear: args.clear,
    }
}

fn run_opts(args: RunArgs) -> RunOpts {
    RunOpts {
        insert: args.insert,
        list: args.list,
        select: args.select,
        ..external_opts(args.args)
    }
}

/// `nixon foo bar` is `nixon run foo bar`: the first word is the command.
///
/// A `--` between the name and the rest is the shell's way of saying "no
/// more flags"; it has done its job by the time we get here, so it does not
/// travel on into the command's arguments.
fn external_opts(args: Vec<String>) -> RunOpts {
    let mut args = args.into_iter();
    let command = args.next();
    let mut rest: Vec<String> = args.collect();
    if rest.first().is_some_and(|arg| arg == "--") {
        rest.remove(0);
    }
    RunOpts {
        command,
        args: rest,
        ..RunOpts::default()
    }
}

fn project_opts(args: ProjectArgs) -> ProjectOpts {
    ProjectOpts {
        project: args.project,
        run: RunOpts {
            insert: args.insert,
            ..external_opts(args.args)
        },
        list: args.list,
        select: args.select,
        inspect: args.inspect,
    }
}

/// Splits `eval`'s positionals: with `--file` there is no expression among
/// them, so the first word is a placeholder like the rest.
fn eval_opts(args: EvalArgs) -> EvalOpts {
    try_eval_opts(args).unwrap_or_else(|(first, err)| {
        Cli::command()
            .error(
                clap::error::ErrorKind::InvalidValue,
                format!("invalid value '{first}' for '[PLACEHOLDERS]...': {err}"),
            )
            .exit()
    })
}

/// Eval option conversion that reports bad recorded values to the GUI.
fn try_eval_opts(
    args: EvalArgs,
) -> std::result::Result<EvalOpts, (String, nixon::placeholder::ParseError)> {
    let mut placeholders = args.placeholders;
    let source = match (args.file.is_some(), args.command) {
        (true, Some(first)) => {
            // Through clap, so a bad first word reads and exits exactly
            // like a bad second one.
            let parsed =
                nixon::placeholder::parse_one(&first).map_err(|error| (first.clone(), error))?;
            placeholders.insert(0, parsed);
            None
        }
        (true, None) => None,
        (false, command) => command,
    };
    Ok(EvalOpts {
        source,
        file: args.file,
        placeholders,
        language: args.language,
        // `--project` with no value means "ask"; with one it names the
        // project outright, which is what a recorded eval replays.
        select_project: args.project.as_ref().is_some_and(String::is_empty),
        project: args.project.filter(|path| !path.is_empty()),
    })
}

/// Plain messages on stderr, no prefix or timestamp.
fn init_tracing(level: Option<LogLevel>) {
    use tracing_subscriber::filter::LevelFilter;

    let filter = match level.unwrap_or(LogLevel::Warning) {
        LogLevel::Debug => LevelFilter::DEBUG,
        LogLevel::Info => LevelFilter::INFO,
        LogLevel::Warning => LevelFilter::WARN,
        LogLevel::Error => LevelFilter::ERROR,
    };

    let _ = tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_max_level(filter)
        .without_time()
        .with_target(false)
        .with_level(false)
        .try_init();
}
