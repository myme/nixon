//! Running child processes.

use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use serde::{Deserialize, Serialize};

/// A child's exit code, with the shell's `128 + signal` convention.
pub type ExitCode = i32;

/// What to run, where, and with what.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
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

/// A captured run.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Captured {
    /// The child's exit code.
    pub code: ExitCode,
    /// Everything the child wrote to stdout. stderr is inherited.
    pub stdout: Vec<u8>,
}

impl Captured {
    /// stdout as lines, which is how placeholder candidates arrive.
    pub fn lines(&self) -> Vec<String> {
        String::from_utf8_lossy(&self.stdout)
            .lines()
            .map(ToOwned::to_owned)
            .collect()
    }
}

/// A child that is still running, streaming its output.
///
/// Send, because cancelling a pick kills the child from the picker's side.
pub trait Running: Send {
    /// Waits for the child and returns its exit code.
    fn wait(&mut self) -> io::Result<ExitCode>;

    /// Checks for an exit without waiting. Runners without this capability
    /// treat a closed candidate channel as successful completion.
    fn try_wait(&mut self) -> io::Result<Option<ExitCode>> {
        Ok(Some(0))
    }

    /// Kills the child, for when a selection is cancelled.
    fn kill(&mut self) -> io::Result<()>;
}

/// The seam every subprocess goes through, so tests need no real ones.
pub trait ProcessRunner {
    /// Runs in the foreground, inheriting stdout and stderr.
    fn run(&mut self, invocation: &Invocation) -> io::Result<ExitCode>;

    /// Runs capturing stdout, stderr inherited.
    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured>;

    /// Runs detached and returns at once, for `&` commands.
    fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()>;

    /// Starts the command, handing each stdout line to `sink` as it arrives.
    ///
    /// Returns as soon as the child is spawned, so a picker can open and be
    /// interactive while the command is still producing candidates.
    fn run_streaming(
        &mut self,
        invocation: &Invocation,
        sink: Box<dyn FnMut(String) + Send>,
    ) -> io::Result<Box<dyn Running>>;
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
        let writer = write_stdin(&mut child, invocation.stdin.as_deref());
        let status = child.wait()?;
        join(writer);
        Ok(exit_code(status))
    }

    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
        let mut command = Self::command(invocation)?;
        command.stdin(stdin_for(invocation));
        command.stdout(Stdio::piped());

        let mut child = command.spawn()?;
        let writer = write_stdin(&mut child, invocation.stdin.as_deref());
        let output = child.wait_with_output()?;
        join(writer);
        Ok(Captured {
            code: exit_code(output.status),
            stdout: output.stdout,
        })
    }

    /// Detaches with a new process group rather than `fork` + `setsid`.
    ///
    /// Forking a process that owns a threadpool is UB-adjacent, and
    /// `process_group` is safe, so this crate needs no `unsafe` at all.
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

    fn run_streaming(
        &mut self,
        invocation: &Invocation,
        mut sink: Box<dyn FnMut(String) + Send>,
    ) -> io::Result<Box<dyn Running>> {
        let mut command = Self::command(invocation)?;
        command.stdin(stdin_for(invocation));
        command.stdout(Stdio::piped());

        // Its own group, so cancelling can take the whole pipeline down.
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt as _;
            command.process_group(0);
        }

        let mut child = command.spawn()?;
        // Not joined: the picker may cancel long before the child has read
        // everything, and the thread ends by itself when the pipe closes.
        drop(write_stdin(&mut child, invocation.stdin.as_deref()));

        let stdout = child.stdout.take();
        let reader = stdout.map(|stdout| {
            std::thread::spawn(move || {
                use std::io::BufRead as _;
                for line in io::BufReader::new(stdout).lines() {
                    match line {
                        Ok(line) => sink(line),
                        Err(_) => break,
                    }
                }
            })
        });

        Ok(Box::new(RunningChild { child, reader }))
    }
}

/// A real child with a thread draining its stdout.
struct RunningChild {
    child: std::process::Child,
    /// Left to finish on its own: it ends when the pipe closes, and waiting
    /// for that is exactly what `kill` must not do.
    reader: Option<std::thread::JoinHandle<()>>,
}

impl Running for RunningChild {
    fn wait(&mut self) -> io::Result<ExitCode> {
        let status = self.child.wait()?;
        // Join only here: the pipe is closed, so the reader is about to end.
        if let Some(reader) = self.reader.take() {
            let _ = reader.join();
        }
        Ok(exit_code(status))
    }

    fn try_wait(&mut self) -> io::Result<Option<ExitCode>> {
        self.child.try_wait().map(|status| status.map(exit_code))
    }

    /// Kills the whole process group, not just the child.
    ///
    /// A command is run through an interpreter, so the child is a shell and
    /// the work is its children. Killing only the shell leaves them holding
    /// the stdout pipe open, and a `sleep 30` in a command would keep nixon
    /// waiting after the user had already cancelled.
    fn kill(&mut self) -> io::Result<()> {
        #[cfg(unix)]
        {
            use nix::sys::signal::{Signal, killpg};
            use nix::unistd::Pid;

            let pid = Pid::from_raw(
                i32::try_from(self.child.id())
                    .map_err(|_| io::Error::other("child pid out of range"))?,
            );
            // The child leads its own group, so its pid is the group id.
            let _ = killpg(pid, Signal::SIGTERM);
        }

        match self.child.kill() {
            // Already gone is not a failure.
            Err(err) if err.kind() == io::ErrorKind::InvalidInput => Ok(()),
            other => other,
        }?;
        let _ = self.child.wait();
        // The reader is deliberately not joined: a grandchild may still hold
        // the pipe, and the thread ends by itself when it closes.
        drop(self.reader.take());
        Ok(())
    }
}

/// Piped when nixon supplies the lines, inherited otherwise.
fn stdin_for(invocation: &Invocation) -> Stdio {
    if invocation.stdin.is_some() {
        Stdio::piped()
    } else {
        Stdio::inherit()
    }
}

/// Feeds the supplied lines to the child on a thread, closing stdin after.
///
/// A pipe holds a page or two. Writing more than that from the thread that
/// afterwards reads the child's stdout deadlocks: the child blocks writing
/// to a full stdout pipe while nixon blocks writing to a full stdin pipe.
fn write_stdin(
    child: &mut std::process::Child,
    lines: Option<&[String]>,
) -> Option<std::thread::JoinHandle<()>> {
    let lines = lines?.to_vec();
    let mut pipe = child.stdin.take()?;

    Some(std::thread::spawn(move || {
        for line in lines {
            // A child that stops reading closes the pipe. That is its
            // choice, not a failure to report.
            if writeln!(pipe, "{line}").is_err() {
                break;
            }
        }
    }))
}

/// Waits for the stdin writer, once the child can no longer be read from.
fn join(writer: Option<std::thread::JoinHandle<()>>) {
    if let Some(writer) = writer {
        let _ = writer.join();
    }
}

/// The child's code, or `128 + signal` when it was killed.
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
    /// [`ProcessRunner::run_streaming`].
    Streamed,
}

/// A runner that records what it was asked and replays scripted output.
#[cfg(any(test, feature = "test-util"))]
#[derive(Debug, Default)]
pub struct FakeRunner {
    /// Every invocation, in order.
    pub calls: Vec<(RunKind, Invocation)>,
    outputs: std::collections::VecDeque<Vec<u8>>,
    codes: std::collections::VecDeque<ExitCode>,
    killed: std::sync::Arc<std::sync::atomic::AtomicBool>,
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

    /// Whether a streamed child was killed, as cancelling does.
    pub fn was_killed(&self) -> bool {
        self.killed.load(std::sync::atomic::Ordering::SeqCst)
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

    /// Feeds the sink from a thread, as a real child does.
    ///
    /// Producing everything before returning would hide every ordering and
    /// liveness bug in whatever consumes the stream, which is the part worth
    /// testing.
    fn run_streaming(
        &mut self,
        invocation: &Invocation,
        mut sink: Box<dyn FnMut(String) + Send>,
    ) -> io::Result<Box<dyn Running>> {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicBool, Ordering};

        self.calls.push((RunKind::Streamed, invocation.clone()));
        let output = self.outputs.pop_front().unwrap_or_default();
        let lines: Vec<String> = String::from_utf8_lossy(&output)
            .lines()
            .map(ToOwned::to_owned)
            .collect();

        let stop = Arc::new(AtomicBool::new(false));
        let stopped = Arc::clone(&stop);
        let feeder = std::thread::spawn(move || {
            for line in lines {
                if stopped.load(Ordering::SeqCst) {
                    break;
                }
                sink(line);
                std::thread::yield_now();
            }
        });

        Ok(Box::new(StreamingFake {
            code: self.next_code(),
            killed: Arc::clone(&self.killed),
            stop,
            feeder: Some(feeder),
        }))
    }
}

/// A fake child with a thread feeding the sink.
#[cfg(any(test, feature = "test-util"))]
struct StreamingFake {
    code: ExitCode,
    killed: std::sync::Arc<std::sync::atomic::AtomicBool>,
    stop: std::sync::Arc<std::sync::atomic::AtomicBool>,
    feeder: Option<std::thread::JoinHandle<()>>,
}

#[cfg(any(test, feature = "test-util"))]
impl Running for StreamingFake {
    fn wait(&mut self) -> io::Result<ExitCode> {
        if let Some(feeder) = self.feeder.take() {
            let _ = feeder.join();
        }
        Ok(self.code)
    }

    fn try_wait(&mut self) -> io::Result<Option<ExitCode>> {
        if self
            .feeder
            .as_ref()
            .is_some_and(std::thread::JoinHandle::is_finished)
        {
            self.wait().map(Some)
        } else {
            Ok(None)
        }
    }

    fn kill(&mut self) -> io::Result<()> {
        self.stop.store(true, std::sync::atomic::Ordering::SeqCst);
        self.killed.store(true, std::sync::atomic::Ordering::SeqCst);
        if let Some(feeder) = self.feeder.take() {
            let _ = feeder.join();
        }
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

    use super::{Captured, FakeRunner, Invocation, ProcessRunner, RealRunner, RunKind, Running};

    #[test]
    fn running_without_exit_polling_completes_when_stream_closes() {
        struct MinimalRunning;

        impl Running for MinimalRunning {
            fn wait(&mut self) -> std::io::Result<i32> {
                Ok(0)
            }

            fn kill(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        assert_eq!(MinimalRunning.try_wait().unwrap(), Some(0));
    }

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

    /// Lines for a stdin larger than a pipe buffer: past that, the child
    /// fills its own stdout pipe before nixon has finished writing.
    fn big_stdin() -> Vec<String> {
        (0..8000).map(|n| format!("{n:060}")).collect()
    }

    /// The regression is a hang, so this waits with a deadline rather than
    /// asserting. On a regression the worker and its `cat` stay blocked
    /// until the test binary exits; that is the failure, not a leak.
    fn within<T: Send + 'static>(what: &str, work: impl FnOnce() -> T + Send + 'static) -> T {
        let (sender, receiver) = std::sync::mpsc::channel();
        std::thread::spawn(move || sender.send(work()));
        receiver
            .recv_timeout(std::time::Duration::from_secs(20))
            .unwrap_or_else(|_| panic!("{what} deadlocked"))
    }

    #[test]
    fn a_captured_run_does_not_deadlock_on_a_large_stdin() {
        let lines = big_stdin();
        let expected = lines.clone();

        let captured = within("run_capture", move || {
            RealRunner
                .run_capture(&Invocation {
                    argv: argv(&["cat"]),
                    stdin: Some(lines),
                    ..Invocation::default()
                })
                .unwrap()
        });

        assert_eq!(captured.code, 0);
        assert_eq!(captured.lines(), expected);
    }

    #[test]
    fn a_streamed_run_does_not_deadlock_on_a_large_stdin() {
        use std::sync::{Arc, Mutex};

        let lines = big_stdin();
        let expected = lines.clone();
        let seen = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&seen);

        // The picker is supposed to open at once, so this must return long
        // before the child has read everything.
        let mut running = within("run_streaming", move || {
            RealRunner
                .run_streaming(
                    &Invocation {
                        argv: argv(&["cat"]),
                        stdin: Some(lines),
                        ..Invocation::default()
                    },
                    Box::new(move |line| sink.lock().unwrap().push(line)),
                )
                .unwrap()
        });

        assert_eq!(running.wait().unwrap(), 0);
        assert_eq!(*seen.lock().unwrap(), expected);
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
    fn a_real_child_streams_its_lines_as_they_arrive() {
        use std::sync::{Arc, Mutex};

        let mut runner = RealRunner;
        let seen = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&seen);

        let mut running = runner
            .run_streaming(
                &sh("printf 'one\ntwo\nthree\n'"),
                Box::new(move |line| sink.lock().unwrap().push(line)),
            )
            .unwrap();

        assert_eq!(running.wait().unwrap(), 0);
        assert_eq!(*seen.lock().unwrap(), ["one", "two", "three"]);
    }

    #[test]
    fn a_streamed_child_reports_its_exit_code() {
        let mut runner = RealRunner;
        let mut running = runner
            .run_streaming(&sh("echo out; exit 4"), Box::new(|_| {}))
            .unwrap();
        assert_eq!(running.wait().unwrap(), 4);
    }

    #[test]
    fn killing_a_streamed_child_stops_it() {
        let mut runner = RealRunner;
        let mut running = runner
            .run_streaming(&sh("sleep 30"), Box::new(|_| {}))
            .unwrap();

        // Returns rather than hanging for the sleep.
        let start = std::time::Instant::now();
        running.kill().unwrap();
        assert!(start.elapsed() < std::time::Duration::from_secs(5));
    }

    #[test]
    fn killing_a_finished_child_is_not_an_error() {
        let mut runner = RealRunner;
        let mut running = runner.run_streaming(&sh("true"), Box::new(|_| {})).unwrap();
        running.wait().unwrap();
        running.kill().unwrap();
    }

    #[test]
    fn the_fake_streams_its_queued_output_and_records_kills() {
        use std::sync::{Arc, Mutex};

        let mut runner = FakeRunner::new().with_output(&["one", "two"]);
        let seen = Arc::new(Mutex::new(Vec::new()));
        let sink = Arc::clone(&seen);

        let mut running = runner
            .run_streaming(
                &sh("x"),
                Box::new(move |line| sink.lock().unwrap().push(line)),
            )
            .unwrap();

        // The fake feeds from a thread, so the lines are there once it has
        // been waited for, not before.
        running.wait().unwrap();
        assert_eq!(*seen.lock().unwrap(), ["one", "two"]);
        assert_eq!(runner.calls[0].0, RunKind::Streamed);

        assert!(!runner.was_killed());
        running.kill().unwrap();
        assert!(runner.was_killed());
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
