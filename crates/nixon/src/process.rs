//! Running child processes. SPEC §7.3, §7.4, ENGINEERING §4.2.

use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// A child's exit code, with the shell's `128 + signal` convention.
pub type ExitCode = i32;

/// What to run, where, and with what. SPEC §7.3.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Invocation {
    /// Interpreter, script path and arguments.
    pub argv: Vec<String>,
    /// Working directory for the child.
    pub cwd: Option<PathBuf>,
    /// Variables added to the inherited environment.
    pub env: Vec<(String, String)>,
    /// Lines piped to the child's stdin; `None` inherits nixon's.
    pub stdin: Option<Vec<String>>,
}

/// A captured run. SPEC §7.3 `run_with_output`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Captured {
    /// The child's exit code.
    pub code: ExitCode,
    /// Everything the child wrote to stdout. stderr is inherited.
    pub stdout: Vec<u8>,
}

impl Captured {
    /// stdout as lines, which is how placeholder candidates arrive. SPEC §5.6.
    pub fn lines(&self) -> Vec<String> {
        String::from_utf8_lossy(&self.stdout)
            .lines()
            .map(ToOwned::to_owned)
            .collect()
    }
}

/// The seam every subprocess goes through, so tests need no real ones.
/// ENGINEERING §4.2.
pub trait ProcessRunner {
    /// Runs in the foreground, inheriting stdout and stderr. SPEC §7.3.
    fn run(&mut self, invocation: &Invocation) -> io::Result<ExitCode>;

    /// Runs capturing stdout, stderr inherited. SPEC §7.3.
    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured>;

    /// Runs detached and returns at once, for `&` commands. SPEC §7.3.
    fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()>;
}

/// Runs real processes.
#[derive(Clone, Copy, Debug, Default)]
pub struct RealRunner;

impl RealRunner {
    /// Builds the child, minus its stdio.
    fn command(invocation: &Invocation) -> io::Result<Command> {
        let Some((program, args)) = invocation.argv.split_first() else {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "empty command"));
        };
        let mut command = Command::new(program);
        command.args(args);
        if let Some(cwd) = &invocation.cwd {
            command.current_dir(cwd);
        }
        for (key, value) in &invocation.env {
            command.env(key, value);
        }
        Ok(command)
    }
}

impl ProcessRunner for RealRunner {
    fn run(&mut self, invocation: &Invocation) -> io::Result<ExitCode> {
        let mut command = Self::command(invocation)?;
        command.stdin(stdin_for(invocation));

        let mut child = command.spawn()?;
        write_stdin(&mut child, invocation.stdin.as_deref())?;
        Ok(exit_code(child.wait()?))
    }

    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
        let mut command = Self::command(invocation)?;
        command.stdin(stdin_for(invocation));
        command.stdout(Stdio::piped());

        let mut child = command.spawn()?;
        write_stdin(&mut child, invocation.stdin.as_deref())?;
        let output = child.wait_with_output()?;
        Ok(Captured {
            code: exit_code(output.status),
            stdout: output.stdout,
        })
    }

    /// Detaches with a new process group rather than `fork` + `setsid`.
    ///
    /// ENGINEERING §2.1: forking a process that owns a threadpool is
    /// UB-adjacent, and `process_group` is safe, so this crate needs no
    /// `unsafe` at all.
    fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()> {
        let mut command = Self::command(invocation)?;
        command
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null());

        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt as _;
            command.process_group(0);
        }

        command.spawn().map(|_| ())
    }
}

/// Piped when nixon supplies the lines, inherited otherwise. SPEC §7.3.
fn stdin_for(invocation: &Invocation) -> Stdio {
    if invocation.stdin.is_some() {
        Stdio::piped()
    } else {
        Stdio::inherit()
    }
}

/// Feeds the supplied lines to the child and closes its stdin.
fn write_stdin(child: &mut std::process::Child, lines: Option<&[String]>) -> io::Result<()> {
    let Some(lines) = lines else {
        return Ok(());
    };
    if let Some(mut pipe) = child.stdin.take() {
        for line in lines {
            writeln!(pipe, "{line}")?;
        }
    }
    Ok(())
}

/// The child's code, or `128 + signal` when it was killed. SPEC §7.3.
fn exit_code(status: std::process::ExitStatus) -> ExitCode {
    status.code().unwrap_or_else(|| {
        #[cfg(unix)]
        {
            use std::os::unix::process::ExitStatusExt as _;
            status.signal().map_or(1, |signal| 128 + signal)
        }
        #[cfg(not(unix))]
        {
            1
        }
    })
}

/// How a recorded invocation was run.
#[cfg(any(test, feature = "test-util"))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RunKind {
    /// [`ProcessRunner::run`].
    Foreground,
    /// [`ProcessRunner::run_capture`].
    Captured,
    /// [`ProcessRunner::spawn_detached`].
    Detached,
}

/// A runner that records what it was asked and replays scripted output.
/// ENGINEERING §4.2.
#[cfg(any(test, feature = "test-util"))]
#[derive(Debug, Default)]
pub struct FakeRunner {
    /// Every invocation, in order.
    pub calls: Vec<(RunKind, Invocation)>,
    outputs: std::collections::VecDeque<Vec<u8>>,
    codes: std::collections::VecDeque<ExitCode>,
}

#[cfg(any(test, feature = "test-util"))]
impl FakeRunner {
    /// A runner whose captured runs produce no output and succeed.
    pub fn new() -> Self {
        Self::default()
    }

    /// Queues stdout for the next captured run, as lines.
    #[must_use]
    pub fn with_output(mut self, lines: &[&str]) -> Self {
        let mut text = lines.join("\n");
        if !text.is_empty() {
            text.push('\n');
        }
        self.outputs.push_back(text.into_bytes());
        self
    }

    /// Queues raw stdout for the next captured run.
    #[must_use]
    pub fn with_raw_output(mut self, bytes: &[u8]) -> Self {
        self.outputs.push_back(bytes.to_vec());
        self
    }

    /// Queues the exit code for the next run.
    #[must_use]
    pub fn with_code(mut self, code: ExitCode) -> Self {
        self.codes.push_back(code);
        self
    }

    /// The argv of every invocation, in order.
    pub fn argvs(&self) -> Vec<&[String]> {
        self.calls
            .iter()
            .map(|(_, invocation)| invocation.argv.as_slice())
            .collect()
    }

    /// The most recent invocation.
    pub fn last(&self) -> Option<&Invocation> {
        self.calls.last().map(|(_, invocation)| invocation)
    }

    fn next_code(&mut self) -> ExitCode {
        self.codes.pop_front().unwrap_or(0)
    }
}

#[cfg(any(test, feature = "test-util"))]
impl ProcessRunner for FakeRunner {
    fn run(&mut self, invocation: &Invocation) -> io::Result<ExitCode> {
        self.calls.push((RunKind::Foreground, invocation.clone()));
        Ok(self.next_code())
    }

    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
        self.calls.push((RunKind::Captured, invocation.clone()));
        Ok(Captured {
            code: self.next_code(),
            stdout: self.outputs.pop_front().unwrap_or_default(),
        })
    }

    fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()> {
        self.calls.push((RunKind::Detached, invocation.clone()));
        Ok(())
    }
}

/// Where the interpreter and script live, for readability at call sites.
pub fn invocation(argv: Vec<String>, cwd: Option<&Path>) -> Invocation {
    Invocation {
        argv,
        cwd: cwd.map(Path::to_path_buf),
        ..Invocation::default()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{Captured, FakeRunner, Invocation, ProcessRunner, RealRunner, RunKind};

    fn argv(items: &[&str]) -> Vec<String> {
        items.iter().map(|s| (*s).to_owned()).collect()
    }

    fn sh(script: &str) -> Invocation {
        Invocation {
            argv: argv(&["sh", "-c", script]),
            ..Invocation::default()
        }
    }

    #[test]
    fn captured_output_splits_into_lines() {
        let captured = Captured {
            code: 0,
            stdout: b"one\ntwo\n".to_vec(),
        };
        assert_eq!(captured.lines(), ["one", "two"]);
    }

    #[test]
    fn captured_output_of_nothing_is_no_lines() {
        let captured = Captured {
            code: 0,
            stdout: Vec::new(),
        };
        assert!(captured.lines().is_empty());
    }

    #[test]
    fn an_empty_argv_is_an_error() {
        let mut runner = RealRunner;
        assert!(runner.run(&Invocation::default()).is_err());
    }

    #[test]
    fn a_real_child_runs_and_its_output_is_captured() {
        let mut runner = RealRunner;
        let captured = runner.run_capture(&sh("echo hello")).unwrap();
        assert_eq!(captured.code, 0);
        assert_eq!(captured.lines(), ["hello"]);
    }

    #[test]
    fn a_real_child_exit_code_is_returned() {
        let mut runner = RealRunner;
        assert_eq!(runner.run(&sh("exit 3")).unwrap(), 3);
    }

    #[test]
    fn a_real_child_killed_by_a_signal_reports_128_plus_it() {
        let mut runner = RealRunner;
        // SIGTERM is 15.
        assert_eq!(runner.run(&sh("kill -TERM $$")).unwrap(), 128 + 15);
    }

    #[test]
    fn a_real_child_sees_its_cwd_and_env() {
        let temp = assert_fs::TempDir::new().unwrap();
        let mut runner = RealRunner;
        let invocation = Invocation {
            argv: argv(&["sh", "-c", "pwd; echo \"$NIXON_TEST\""]),
            cwd: Some(temp.path().to_path_buf()),
            env: vec![("NIXON_TEST".to_owned(), "set".to_owned())],
            stdin: None,
        };
        let captured = runner.run_capture(&invocation).unwrap();
        let lines = captured.lines();
        assert!(lines[0].ends_with(temp.path().file_name().unwrap().to_string_lossy().as_ref()));
        assert_eq!(lines[1], "set");
    }

    #[test]
    fn stdin_lines_reach_a_real_child() {
        let mut runner = RealRunner;
        let invocation = Invocation {
            argv: argv(&["cat"]),
            stdin: Some(vec!["one".to_owned(), "two".to_owned()]),
            ..Invocation::default()
        };
        assert_eq!(
            runner.run_capture(&invocation).unwrap().lines(),
            ["one", "two"]
        );
    }

    #[test]
    fn the_fake_records_every_invocation_with_its_kind() {
        let mut runner = FakeRunner::new();
        runner.run(&sh("one")).unwrap();
        runner.run_capture(&sh("two")).unwrap();
        runner.spawn_detached(&sh("three")).unwrap();

        let kinds: Vec<_> = runner.calls.iter().map(|(kind, _)| *kind).collect();
        assert_eq!(
            kinds,
            [RunKind::Foreground, RunKind::Captured, RunKind::Detached]
        );
        assert_eq!(runner.calls[1].1.argv, argv(&["sh", "-c", "two"]));
    }

    #[test]
    fn the_fake_replays_queued_output_in_order() {
        let mut runner = FakeRunner::new()
            .with_output(&["first"])
            .with_output(&["second", "third"]);
        assert_eq!(runner.run_capture(&sh("x")).unwrap().lines(), ["first"]);
        assert_eq!(
            runner.run_capture(&sh("y")).unwrap().lines(),
            ["second", "third"]
        );
        assert!(runner.run_capture(&sh("z")).unwrap().lines().is_empty());
    }

    #[test]
    fn the_fake_replays_queued_exit_codes() {
        let mut runner = FakeRunner::new().with_code(3);
        assert_eq!(runner.run(&sh("x")).unwrap(), 3);
        assert_eq!(runner.run(&sh("y")).unwrap(), 0);
    }

    #[test]
    fn the_fake_records_cwd_env_and_stdin() {
        let mut runner = FakeRunner::new();
        let invocation = Invocation {
            argv: argv(&["bash", "script.sh"]),
            cwd: Some(PathBuf::from("/tmp/project")),
            env: vec![("nixon_project_path".to_owned(), "/tmp/project".to_owned())],
            stdin: Some(vec!["line".to_owned()]),
        };
        runner.run(&invocation).unwrap();
        assert_eq!(runner.last(), Some(&invocation));
    }
}
