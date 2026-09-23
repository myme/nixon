//! Process runner that hands foreground GUI commands to a terminal.

use std::ffi::{OsStr, OsString};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use nixon::process::{Captured, Invocation, ProcessRunner, Running};

use crate::gui_exec;

/// A GUI runner that preserves the existing process behavior except for
/// foreground runs, which it hands to a separate terminal.
pub struct GuiProcessRunner<R> {
    inner: R,
    configured_terminal: Option<Vec<String>>,
    terminal_env: Option<String>,
    path_env: Option<OsString>,
    exe: PathBuf,
}

impl<R: ProcessRunner> GuiProcessRunner<R> {
    /// Creates a runner with explicit environment values for deterministic use.
    /// `configured_terminal` is the effective launcher field for the selected
    /// project, so a local override can be passed when the GUI action is wired.
    pub const fn new(
        inner: R,
        configured_terminal: Option<Vec<String>>,
        terminal_env: Option<String>,
        path_env: Option<OsString>,
        exe: PathBuf,
    ) -> Self {
        Self {
            inner,
            configured_terminal,
            terminal_env,
            path_env,
            exe,
        }
    }

    /// Reads terminal settings from the current process environment.
    pub fn from_environment(
        inner: R,
        configured_terminal: Option<Vec<String>>,
        exe: PathBuf,
    ) -> Self {
        Self::new(
            inner,
            configured_terminal,
            std::env::var("TERMINAL").ok(),
            std::env::var_os("PATH"),
            exe,
        )
    }

    fn terminal_prefix(&self) -> io::Result<Vec<String>> {
        if let Some(prefix) = &self.configured_terminal {
            return validate_prefix(
                prefix.clone(),
                "launcher.terminal",
                self.path_env.as_deref(),
            );
        }
        if let Some(terminal) = &self.terminal_env {
            let mut prefix = shell_words::split(terminal).map_err(|err| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("Invalid $TERMINAL argument string: {err}"),
                )
            })?;
            if prefix.len() == 1 {
                let executable = Path::new(&prefix[0])
                    .file_name()
                    .and_then(OsStr::to_str)
                    .unwrap_or("");
                let execute_flag = match executable {
                    "alacritty"
                    | "kitty"
                    | "foot"
                    | "konsole"
                    | "xterm"
                    | "uxterm"
                    | "x-terminal-emulator" => "-e",
                    "gnome-terminal" => "--",
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            format!(
                                "$TERMINAL names {executable} without an execute flag; set it to a complete terminal argv such as 'kitty -e'"
                            ),
                        ));
                    }
                };
                prefix.push(execute_flag.to_owned());
            }
            return validate_prefix(prefix, "$TERMINAL", self.path_env.as_deref());
        }
        #[cfg(target_os = "linux")]
        {
            if available("x-terminal-emulator", self.path_env.as_deref()) {
                return Ok(vec!["x-terminal-emulator".to_owned(), "-e".to_owned()]);
            }
        }
        Err(io::Error::new(
            io::ErrorKind::NotFound,
            "No terminal launcher is available; set launcher.terminal to an installed terminal executable and its execute flag, or set $TERMINAL.",
        ))
    }
}

impl<R: ProcessRunner> ProcessRunner for GuiProcessRunner<R> {
    fn run(&mut self, invocation: &Invocation) -> io::Result<i32> {
        if invocation.argv.is_empty() {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "empty command"));
        }
        if !self.exe.is_absolute() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "GUI executable path must be absolute",
            ));
        }
        let exe = self.exe.to_str().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "GUI executable path is not UTF-8",
            )
        })?;
        let mut argv = self.terminal_prefix()?;
        let launcher = argv[0].clone();
        let payload = gui_exec::write_payload(invocation)?;
        let Some(payload_arg) = payload.to_str() else {
            let _ = fs::remove_file(&payload);
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "GUI payload path is not UTF-8",
            ));
        };
        argv.extend([
            exe.to_owned(),
            "internal".to_owned(),
            "gui-exec".to_owned(),
            payload_arg.to_owned(),
        ]);
        let terminal = Invocation {
            argv,
            ..Invocation::default()
        };
        if let Err(err) = self.inner.spawn_detached(&terminal) {
            if let Err(cleanup) = fs::remove_file(&payload) {
                return Err(io::Error::new(
                    err.kind(),
                    format!(
                        "Could not launch terminal {launcher}: {err}; could not remove GUI payload {}: {cleanup}",
                        payload.display()
                    ),
                ));
            }
            return Err(io::Error::new(
                err.kind(),
                format!("Could not launch terminal {launcher}: {err}"),
            ));
        }
        Ok(0)
    }

    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
        self.inner.run_capture(invocation)
    }

    fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()> {
        self.inner.spawn_detached(invocation)
    }

    fn run_streaming(
        &mut self,
        invocation: &Invocation,
        sink: Box<dyn FnMut(String) + Send>,
    ) -> io::Result<Box<dyn Running>> {
        self.inner.run_streaming(invocation, sink)
    }
}

fn validate_prefix(
    prefix: Vec<String>,
    source: &str,
    path_env: Option<&OsStr>,
) -> io::Result<Vec<String>> {
    if prefix.len() < 2 || prefix.iter().any(|part| part.trim().is_empty()) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "{source} must contain a terminal executable and its execute flag as separate arguments"
            ),
        ));
    }
    if !available(&prefix[0], path_env) {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "{source} executable '{}' is unavailable; install it or set launcher.terminal to an available terminal command",
                prefix[0]
            ),
        ));
    }
    Ok(prefix)
}

fn available(program: &str, path_env: Option<&OsStr>) -> bool {
    let path = Path::new(program);
    if path.is_absolute() || path.components().count() > 1 {
        return executable_file(path);
    }
    path_env.is_some_and(|value| {
        std::env::split_paths(value)
            .filter(|dir| !dir.as_os_str().is_empty())
            .any(|dir| executable_file(&dir.join(program)))
    })
}

fn executable_file(path: &Path) -> bool {
    let Ok(metadata) = fs::metadata(path) else {
        return false;
    };
    if !metadata.is_file() {
        return false;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt as _;

        metadata.permissions().mode() & 0o111 != 0
    }
    #[cfg(not(unix))]
    {
        true
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;
    use std::fs;
    use std::io;
    use std::path::{Path, PathBuf};
    use std::sync::{Arc, Mutex};

    use nixon::config::Config;
    use nixon::process::{Captured, Invocation, ProcessRunner, Running};
    use tempfile::TempDir;

    use super::GuiProcessRunner;
    use crate::gui_exec::read_payload;

    #[derive(Default)]
    struct RecordingRunner {
        calls: Vec<(&'static str, Invocation)>,
        fail_spawn: bool,
    }

    impl ProcessRunner for RecordingRunner {
        fn run(&mut self, _invocation: &Invocation) -> io::Result<i32> {
            Err(io::Error::other("unexpected inner foreground run"))
        }

        fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
            self.calls.push(("capture", invocation.clone()));
            Ok(Captured {
                code: 7,
                stdout: b"captured".to_vec(),
            })
        }

        fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()> {
            self.calls.push(("detached", invocation.clone()));
            if self.fail_spawn {
                Err(io::Error::new(io::ErrorKind::NotFound, "spawn failed"))
            } else {
                Ok(())
            }
        }

        fn run_streaming(
            &mut self,
            invocation: &Invocation,
            mut sink: Box<dyn FnMut(String) + Send>,
        ) -> io::Result<Box<dyn Running>> {
            self.calls.push(("streaming", invocation.clone()));
            sink("streamed".to_owned());
            Ok(Box::new(Completed))
        }
    }

    struct Completed;

    impl Running for Completed {
        fn wait(&mut self) -> io::Result<i32> {
            Ok(8)
        }

        fn kill(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    fn executable(dir: &Path, name: &str) -> PathBuf {
        let path = dir.join(name);
        fs::write(&path, b"").unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;

            fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).unwrap();
        }
        path
    }

    fn command() -> Invocation {
        Invocation {
            argv: vec![
                "program with spaces".to_owned(),
                "argument with 'quotes' and \"double quotes\"".to_owned(),
            ],
            cwd: Some("/tmp/a cwd with 'quotes'".into()),
            env: vec![("VALUE".to_owned(), "some 'quoted' value".to_owned())],
            stdin: Some(vec!["one two".to_owned(), "three 'four'".to_owned()]),
        }
    }

    #[test]
    fn foreground_handoff_preserves_all_fields_and_uses_exact_argv() {
        let temp = TempDir::new().unwrap();
        let configured = executable(temp.path(), "terminal with 'quotes'");
        let exe = temp.path().join("nixon with spaces");
        let mut runner = GuiProcessRunner::new(
            RecordingRunner::default(),
            Some(vec![
                configured.to_string_lossy().into_owned(),
                "--title=two words".to_owned(),
                "-e".to_owned(),
            ]),
            Some("ignored-terminal -e".to_owned()),
            Some(OsString::from("/missing")),
            exe.clone(),
        );
        let original = command();
        assert_eq!(runner.run(&original).unwrap(), 0);
        let (kind, launched) = &runner.inner.calls[0];
        assert_eq!(*kind, "detached");
        assert_eq!(launched.cwd, None);
        assert!(launched.env.is_empty());
        assert_eq!(launched.stdin, None);
        assert_eq!(launched.argv.len(), 7);
        assert_eq!(launched.argv[0], configured.to_string_lossy());
        assert_eq!(launched.argv[1], "--title=two words");
        assert_eq!(launched.argv[2], "-e");
        assert_eq!(launched.argv[3], exe.to_string_lossy());
        assert_eq!(launched.argv[4], "internal");
        assert_eq!(launched.argv[5], "gui-exec");
        let payload = PathBuf::from(&launched.argv[6]);
        assert_eq!(read_payload(&payload).unwrap(), original);
        assert!(!payload.exists());
    }

    #[test]
    fn terminal_env_is_parsed_as_argv_and_linux_fallback_is_last() {
        let temp = TempDir::new().unwrap();
        let custom = executable(temp.path(), "custom terminal");
        executable(temp.path(), "alacritty");
        #[cfg(target_os = "linux")]
        executable(temp.path(), "x-terminal-emulator");
        let path = Some(temp.path().as_os_str().to_owned());
        let runner = GuiProcessRunner::new(
            RecordingRunner::default(),
            None,
            Some(format!("'{}' --title 'two words' -e", custom.display())),
            path.clone(),
            temp.path().join("nixon"),
        );
        assert_eq!(
            runner.terminal_prefix().unwrap(),
            [
                custom.to_string_lossy().into_owned(),
                "--title".to_owned(),
                "two words".to_owned(),
                "-e".to_owned(),
            ]
        );
        let bare_known = GuiProcessRunner::new(
            RecordingRunner::default(),
            None,
            Some("alacritty".to_owned()),
            path.clone(),
            temp.path().join("nixon"),
        );
        assert_eq!(bare_known.terminal_prefix().unwrap(), ["alacritty", "-e"]);
        #[cfg(target_os = "linux")]
        {
            let fallback = GuiProcessRunner::new(
                RecordingRunner::default(),
                None,
                None,
                path,
                temp.path().join("nixon"),
            );
            assert_eq!(
                fallback.terminal_prefix().unwrap(),
                ["x-terminal-emulator", "-e"]
            );
        }
    }

    #[test]
    fn effective_local_launcher_terminal_can_override_global_terminal() {
        let temp = TempDir::new().unwrap();
        let global_terminal = executable(temp.path(), "global-terminal");
        let local_terminal = executable(temp.path(), "local-terminal");
        let mut global = Config::defaults();
        global.launcher.terminal = Some(vec![
            global_terminal.to_string_lossy().into_owned(),
            "-e".to_owned(),
        ]);
        let mut local = Config::default();
        local.launcher.terminal = Some(vec![
            local_terminal.to_string_lossy().into_owned(),
            "--execute".to_owned(),
        ]);
        let effective = global.merge(local);
        let runner = GuiProcessRunner::new(
            RecordingRunner::default(),
            effective.launcher.terminal,
            None,
            None,
            temp.path().join("nixon"),
        );
        assert_eq!(
            runner.terminal_prefix().unwrap(),
            [
                local_terminal.to_string_lossy().into_owned(),
                "--execute".to_owned()
            ]
        );
    }

    #[test]
    fn missing_or_invalid_launcher_reports_actionable_error() {
        let temp = TempDir::new().unwrap();
        let path = Some(temp.path().as_os_str().to_owned());
        let mut missing = GuiProcessRunner::new(
            RecordingRunner::default(),
            None,
            None,
            path.clone(),
            temp.path().join("nixon"),
        );
        let error = missing.run(&command()).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
        assert!(error.to_string().contains("set launcher.terminal"));
        assert!(missing.inner.calls.is_empty());

        let available = executable(temp.path(), "available-terminal");
        let invalid = GuiProcessRunner::new(
            RecordingRunner::default(),
            Some(vec!["missing-terminal".to_owned(), "-e".to_owned()]),
            Some(format!("{} -e", available.display())),
            path,
            temp.path().join("nixon"),
        );
        let error = invalid.terminal_prefix().unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
        assert!(error.to_string().contains("launcher.terminal executable"));

        let unknown_bare = GuiProcessRunner::new(
            RecordingRunner::default(),
            None,
            Some("available-terminal".to_owned()),
            Some(temp.path().as_os_str().to_owned()),
            temp.path().join("nixon"),
        );
        let error = unknown_bare.terminal_prefix().unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
        assert!(error.to_string().contains("execute flag"));

        let incomplete_config = GuiProcessRunner::new(
            RecordingRunner::default(),
            Some(vec![available.to_string_lossy().into_owned()]),
            None,
            None,
            temp.path().join("nixon"),
        );
        let error = incomplete_config.terminal_prefix().unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
        assert!(error.to_string().contains("launcher.terminal"));
    }

    #[test]
    fn failed_terminal_spawn_removes_payload() {
        let temp = TempDir::new().unwrap();
        let terminal = executable(temp.path(), "terminal");
        let mut runner = GuiProcessRunner::new(
            RecordingRunner {
                fail_spawn: true,
                ..RecordingRunner::default()
            },
            Some(vec![
                terminal.to_string_lossy().into_owned(),
                "-e".to_owned(),
            ]),
            None,
            None,
            temp.path().join("nixon"),
        );
        let error = runner.run(&command()).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
        assert!(error.to_string().contains("Could not launch terminal"));
        let payload = PathBuf::from(runner.inner.calls[0].1.argv.last().unwrap());
        assert!(!payload.exists());
    }

    #[test]
    fn capture_streaming_and_detached_calls_delegate_unchanged() {
        let mut runner = GuiProcessRunner::new(
            RecordingRunner::default(),
            None,
            None,
            None,
            PathBuf::from("unused"),
        );
        let original = command();
        let captured = runner.run_capture(&original).unwrap();
        assert_eq!(captured.code, 7);
        assert_eq!(captured.stdout, b"captured");
        let lines = Arc::new(Mutex::new(Vec::new()));
        let collected = Arc::clone(&lines);
        let mut streaming = runner
            .run_streaming(
                &original,
                Box::new(move |line| collected.lock().unwrap().push(line)),
            )
            .unwrap();
        assert_eq!(streaming.wait().unwrap(), 8);
        assert_eq!(*lines.lock().unwrap(), ["streamed"]);
        runner.spawn_detached(&original).unwrap();
        assert_eq!(
            runner.inner.calls,
            [
                ("capture", original.clone()),
                ("streaming", original.clone()),
                ("detached", original),
            ]
        );
    }
}
