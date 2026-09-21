//! Evaluating commands.

pub mod cache;
pub mod wrap;

use std::path::{Path, PathBuf};

use crate::command::Command;
use crate::config::Config;
use crate::error::{NixonError, Result};
use crate::process::{Captured, ExitCode, Invocation, ProcessRunner};

/// The variable every command can read to find its project.
pub const PROJECT_PATH_VAR: &str = "nixon_project_path";

/// The variable every command can read to call nixon back.
///
/// An absolute path, so a command that runs nixon in another project works
/// the same under `cargo run`, `nix run` and an installed binary.
pub const BIN_VAR: &str = "nixon_bin";

/// Everything evaluation needs that comes from outside the pure layers.
pub struct Context<'a> {
    /// The effective configuration.
    pub config: &'a Config,
    /// Where scripts are cached.
    pub cache_dir: &'a Path,
    /// `$SHELL`, for [`crate::language::Language::None`].
    pub shell: Option<&'a str>,
    /// `$DIRENV_DIR`, for the direnv wrapper.
    pub direnv_dir: Option<&'a str>,
    /// The running executable, exported as [`BIN_VAR`].
    pub exe: Option<&'a Path>,
}

/// One command, ready to run.
pub struct Evaluation {
    /// Positional arguments after the script path.
    pub args: Vec<String>,
    /// Working directory.
    pub cwd: Option<PathBuf>,
    /// Variables added to the environment.
    pub env: Vec<(String, String)>,
    /// Lines piped to stdin, if any.
    pub stdin: Option<Vec<String>>,
}

/// Writes the script and builds the invocation that runs it.
///
/// `direnv` is tried before `nix`, and the first that applies wins.
pub fn prepare(
    context: &Context<'_>,
    command: &Command,
    evaluation: &Evaluation,
) -> Result<Invocation> {
    let interpreter =
        command
            .lang
            .interpreter(context.shell)
            .ok_or_else(|| NixonError::NoInterpreter {
                language: command.lang.to_string(),
            })?;

    let script = cache::write_script(context.cache_dir, command)?;

    let mut argv = interpreter;
    argv.push(script.to_string_lossy().into_owned());
    argv.extend(evaluation.args.iter().cloned());

    let argv = wrap::maybe_wrap(
        context.config,
        argv,
        evaluation.cwd.as_deref(),
        context.direnv_dir,
    );

    // Every command can call nixon back, wherever nixon was run from.
    let mut env = evaluation.env.clone();
    if let Some(exe) = context.exe {
        env.push((BIN_VAR.to_owned(), exe.to_string_lossy().into_owned()));
    }

    Ok(Invocation {
        argv,
        cwd: evaluation.cwd.clone(),
        env,
        stdin: evaluation.stdin.clone(),
    })
}

/// Runs a command.
///
/// The decision tree is now just `is_bg`: detached if the heading ended in
/// `&`, foreground otherwise. The terminal-spawning branch went with the GUI
/// backend, and `force_tty` with it.
///
/// While a foreground child runs, SIGINT is ignored in nixon so `^C` reaches
/// only the child. The child's exit code is returned, where v1 discarded it.
pub fn evaluate<R: ProcessRunner>(
    context: &Context<'_>,
    runner: &mut R,
    command: &Command,
    evaluation: &Evaluation,
) -> Result<ExitCode> {
    let invocation = prepare(context, command, evaluation)?;

    if command.is_bg {
        runner.spawn_detached(&invocation)?;
        return Ok(0);
    }

    let _guard = SigintGuard::install();
    Ok(runner.run(&invocation)?)
}

/// Runs a command and captures its stdout, for placeholder candidates.
pub fn evaluate_capture<R: ProcessRunner>(
    context: &Context<'_>,
    runner: &mut R,
    command: &Command,
    evaluation: &Evaluation,
) -> Result<Captured> {
    let invocation = prepare(context, command, evaluation)?;
    Ok(runner.run_capture(&invocation)?)
}

/// Ignores SIGINT for as long as it is alive, so `^C` reaches only the
/// child.
#[cfg(unix)]
struct SigintGuard {
    id: Option<signal_hook::SigId>,
}

#[cfg(unix)]
impl SigintGuard {
    /// Replaces the default action with one that only sets a flag nobody
    /// reads, which is async-signal-safe and leaves nixon running.
    fn install() -> Self {
        let flag = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        Self {
            id: signal_hook::flag::register(signal_hook::consts::SIGINT, flag).ok(),
        }
    }
}

#[cfg(unix)]
impl Drop for SigintGuard {
    fn drop(&mut self) {
        if let Some(id) = self.id.take() {
            signal_hook::low_level::unregister(id);
        }
    }
}

/// Nixon targets Linux and macOS; elsewhere there is nothing to install.
#[cfg(not(unix))]
struct SigintGuard;

#[cfg(not(unix))]
impl SigintGuard {
    const fn install() -> Self {
        Self
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{Context, Evaluation, evaluate, prepare};
    use crate::command::Command;
    use crate::config::Config;
    use crate::error::NixonError;
    use crate::language::Language;
    use crate::process::{FakeRunner, RunKind};

    fn command(source: &str, lang: Language) -> Command {
        Command {
            name: "hello".to_owned(),
            source: source.to_owned(),
            lang,
            ..Command::default()
        }
    }

    fn context<'a>(config: &'a Config, cache: &'a Path) -> Context<'a> {
        Context {
            config,
            cache_dir: cache,
            shell: Some("/bin/zsh"),
            direnv_dir: None,
            exe: None,
        }
    }

    fn evaluation(cwd: Option<&Path>) -> Evaluation {
        Evaluation {
            args: Vec::new(),
            cwd: cwd.map(Path::to_path_buf),
            env: Vec::new(),
            stdin: None,
        }
    }

    #[test]
    fn the_interpreter_runs_the_cached_script() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let invocation = prepare(
            &context(&config, temp.path()),
            &command("echo hi\n", Language::Bash),
            &evaluation(None),
        )
        .unwrap();

        assert_eq!(invocation.argv[0], "bash");
        assert!(invocation.argv[1].ends_with("-hello.sh"));
        assert_eq!(
            std::fs::read_to_string(&invocation.argv[1]).unwrap(),
            "echo hi\n"
        );
    }

    #[test]
    fn positional_args_follow_the_script_path() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let mut eval = evaluation(None);
        eval.args = vec!["README.md".to_owned(), "src".to_owned()];

        let invocation = prepare(
            &context(&config, temp.path()),
            &command("echo\n", Language::Bash),
            &eval,
        )
        .unwrap();
        assert_eq!(&invocation.argv[2..], ["README.md", "src"]);
    }

    #[test]
    fn no_language_runs_under_the_shell() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let invocation = prepare(
            &context(&config, temp.path()),
            &command("echo\n", Language::None),
            &evaluation(None),
        )
        .unwrap();
        assert_eq!(invocation.argv[0], "/bin/zsh");
    }

    #[test]
    fn a_language_without_an_interpreter_is_an_error() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let err = prepare(
            &context(&config, temp.path()),
            &command("x\n", Language::Unknown("ruby".to_owned())),
            &evaluation(None),
        )
        .unwrap_err();
        assert!(matches!(err, NixonError::NoInterpreter { .. }));
        assert_eq!(err.to_string(), "No interpreter for ruby");
    }

    #[test]
    fn the_nix_wrapper_applies_when_enabled() {
        let temp = TempDir::new().unwrap();
        let project = temp.child("project");
        project.create_dir_all().unwrap();
        project.child("shell.nix").write_str("{}\n").unwrap();

        let config = Config {
            use_nix: Some(true),
            ..Config::default()
        };
        let invocation = prepare(
            &context(&config, temp.path()),
            &command("echo\n", Language::Bash),
            &evaluation(Some(project.path())),
        )
        .unwrap();
        assert_eq!(invocation.argv[0], "nix-shell");
        assert_eq!(invocation.argv[1], "--command");
    }

    #[test]
    fn a_foreground_command_runs_and_returns_its_exit_code() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let mut runner = FakeRunner::new().with_code(3);

        let code = evaluate(
            &context(&config, temp.path()),
            &mut runner,
            &command("exit 3\n", Language::Bash),
            &evaluation(None),
        )
        .unwrap();

        assert_eq!(code, 3);
        assert_eq!(runner.calls[0].0, RunKind::Foreground);
    }

    #[test]
    fn a_background_command_is_detached_and_reports_success() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let mut runner = FakeRunner::new();
        let mut cmd = command("sleep 10\n", Language::Bash);
        cmd.is_bg = true;

        let code = evaluate(
            &context(&config, temp.path()),
            &mut runner,
            &cmd,
            &evaluation(None),
        )
        .unwrap();

        assert_eq!(code, 0);
        assert_eq!(runner.calls[0].0, RunKind::Detached);
    }

    #[test]
    fn the_environment_and_stdin_reach_the_runner() {
        let temp = TempDir::new().unwrap();
        let config = Config::default();
        let mut runner = FakeRunner::new();
        let project = PathBuf::from("/tmp/project");

        let eval = Evaluation {
            args: Vec::new(),
            cwd: Some(project.clone()),
            env: vec![(
                super::PROJECT_PATH_VAR.to_owned(),
                project.display().to_string(),
            )],
            stdin: Some(vec!["one".to_owned()]),
        };
        evaluate(
            &context(&config, temp.path()),
            &mut runner,
            &command("cat\n", Language::Bash),
            &eval,
        )
        .unwrap();

        let invocation = runner.last().unwrap();
        assert_eq!(invocation.cwd.as_deref(), Some(project.as_path()));
        assert_eq!(invocation.env[0].0, "nixon_project_path");
        assert_eq!(
            invocation.stdin.as_deref(),
            Some(["one".to_owned()].as_slice())
        );
    }
}
