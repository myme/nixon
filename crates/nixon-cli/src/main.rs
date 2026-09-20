//! The `nixon` binary. Argument parsing, diagnostics and process exit live
//! here; everything else is in the `nixon` library. ENGINEERING §4.3.

// A binary has no external API, so `pub` on its private modules' items is
// how they are shared between those modules, not an exported surface.
#![allow(unreachable_pub)]
// Tests assert on known-good values.
#![cfg_attr(test, allow(clippy::unwrap_used, clippy::expect_used, clippy::panic))]

mod cli;
mod complete;
mod mangen;

use std::process::ExitCode;

use clap::Parser as _;
use nixon::app::eval::EvalOpts;
use nixon::app::new::NewOpts;
use nixon::app::project::ProjectOpts;
use nixon::app::{App, Environment, RunOpts};
use nixon::config::{Config, LogLevel, load};
use nixon::error::{NixonError, Result};
use nixon::fs::Dirs;
use nixon::process::RealRunner;
use nixon_picker::TuiPicker;

use cli::{Cli, Commands, EvalArgs, Internal, ProjectArgs, RunArgs};

fn main() -> ExitCode {
    // Completion must answer before anything writes to stdout. SPEC §2.4.
    complete::maybe_complete();

    match run() {
        Ok(code) => ExitCode::from(u8::try_from(code).unwrap_or(1)),
        Err(err) => {
            report(&err);
            ExitCode::from(u8::try_from(err.exit_code()).unwrap_or(1))
        }
    }
}

/// Prints an error the way SPEC §10.8 wants: plain text, on stderr.
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
    let dirs =
        Dirs::from_env().map_err(|err| NixonError::Io(std::io::Error::other(err.to_string())))?;

    let cli_config = parsed.global.to_config();
    init_tracing(cli_config.loglevel);

    let config_path = parsed
        .global
        .config
        .clone()
        .unwrap_or_else(|| dirs.global_config());
    let file_config = load::load_global(&config_path)?;

    // SPEC §2.3: defaults, then the file, then the command line.
    let config = Config::defaults().merge(file_config).merge(cli_config);

    let env = Environment {
        cwd: std::env::current_dir()?,
        shell: std::env::var("SHELL").ok(),
        direnv_dir: std::env::var("DIRENV_DIR").ok(),
        editor: std::env::var("VISUAL")
            .or_else(|_| std::env::var("EDITOR"))
            .ok(),
    };

    let mut app = App::new(config, dirs, env, TuiPicker, RealRunner);

    match parsed.command {
        None => app.run(&RunOpts::default()),
        Some(Commands::Run(args)) => app.run(&run_opts(args)),
        Some(Commands::External(args)) => app.run(&external_opts(args)),
        Some(Commands::Project(args)) => app.project(&project_opts(args)),
        Some(Commands::Eval(args)) => app.eval(&eval_opts(args)),
        Some(Commands::Edit { command }) => app.edit(command.as_deref()),
        Some(Commands::Gc { dry_run }) => app.gc(dry_run),
        Some(Commands::Internal(Internal::Mangen)) => mangen::write_man_page(),
        Some(Commands::New(args)) => app.new_command(&NewOpts {
            name: args.name,
            desc: args.desc,
            lang: args.lang,
            src: args.src,
        }),
    }
}

fn run_opts(args: RunArgs) -> RunOpts {
    RunOpts {
        command: args.command,
        args: args.args,
        insert: args.insert,
        list: args.list,
        select: args.select,
    }
}

/// `nixon foo bar` is `nixon run foo bar`. SPEC §2.2.
fn external_opts(args: Vec<String>) -> RunOpts {
    let mut args = args.into_iter();
    RunOpts {
        command: args.next(),
        args: args.collect(),
        ..RunOpts::default()
    }
}

fn project_opts(args: ProjectArgs) -> ProjectOpts {
    ProjectOpts {
        project: args.project,
        run: RunOpts {
            command: args.command,
            args: args.args,
            insert: args.insert,
            list: false,
            select: false,
        },
        list: args.list,
        select: args.select,
        inspect: args.inspect,
    }
}

fn eval_opts(args: EvalArgs) -> EvalOpts {
    EvalOpts {
        source: args.command,
        file: args.file,
        placeholders: args.placeholders,
        language: args.language,
        select_project: args.project,
    }
}

/// Plain messages on stderr, no prefix or timestamp. SPEC §11.
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
