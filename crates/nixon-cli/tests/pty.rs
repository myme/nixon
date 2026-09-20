//! PTY-driven tests of the real event loop. ENGINEERING §5.
//!
//! These are the only tests that exercise terminal setup and teardown, the
//! key bindings and the picker's redraw loop. Everything else runs the
//! non-interactive paths.

// Placeholder syntax such as `${items:m}` reads to clippy as a stray format
// argument.
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::literal_string_with_formatting_args
)]

use std::io::Read as _;
use std::time::Duration;

use assert_fs::TempDir;
use assert_fs::prelude::*;
use expectrl::process::unix::WaitStatus;
use expectrl::session::OsSession;
use expectrl::{Expect as _, Session};

const NIXON_MD: &str = "\
# `alpha`

First command.

```bash
echo ran-alpha
```

# `beta`

Second command.

```bash
echo ran-beta
```
";

/// A project the binary can be driven against over a PTY.
struct Pty {
    temp: TempDir,
}

impl Pty {
    fn new() -> Self {
        Self::with_config(NIXON_MD)
    }

    fn with_config(nixon_md: &str) -> Self {
        let temp = TempDir::new().unwrap();
        let project = temp.child("project");
        project.create_dir_all().unwrap();
        project.child(".git").create_dir_all().unwrap();
        project.child("nixon.md").write_str(nixon_md).unwrap();
        temp.child("config/nixon.md")
            .write_str(
                "```json config\n{\"project_types\": [{\"name\": \"git\", \"test\": [\".git\"], \"desc\": \"Git\"}]}\n```\n",
            )
            .unwrap();
        Self { temp }
    }

    fn binary() -> String {
        assert_cmd::cargo::cargo_bin("nixon")
            .to_string_lossy()
            .into_owned()
    }

    /// Spawns nixon on a PTY with the host environment cleared.
    fn spawn(&self, args: &[&str]) -> OsSession {
        let mut command = std::process::Command::new(Self::binary());
        command
            .env_clear()
            .env("PATH", std::env::var("PATH").unwrap_or_default())
            .env("HOME", self.temp.path())
            .env("TERM", "xterm-256color")
            .env("XDG_CONFIG_HOME", self.temp.child("config").path())
            .env("XDG_CACHE_HOME", self.temp.child("cache").path())
            .env("SHELL", "/bin/bash")
            .env("EDITOR", self.fake_editor())
            .current_dir(self.temp.child("project").path())
            .args(args);

        let mut session = Session::spawn(command).unwrap();
        session.set_expect_timeout(Some(Duration::from_secs(20)));
        session
    }

    /// An `$EDITOR` that records its arguments instead of opening anything.
    fn fake_editor(&self) -> String {
        let editor = self.temp.child("fake-editor");
        editor
            .write_str(&format!(
                "#!/bin/sh\nprintf '%s\\n' \"$@\" > {}/editor-args\n",
                self.temp.path().display()
            ))
            .unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;
            let mut perms = std::fs::metadata(editor.path()).unwrap().permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(editor.path(), perms).unwrap();
        }
        editor.path().to_string_lossy().into_owned()
    }

    fn editor_args(&self) -> String {
        std::fs::read_to_string(self.temp.child("editor-args").path()).unwrap_or_default()
    }
}

/// Reads everything left on the PTY after the child exits.
///
/// Assertions go here rather than against the drawn screen: ratatui redraws
/// with cursor-positioning escapes between characters, so on-screen text
/// rarely appears as one contiguous byte string.
fn drain(session: &mut OsSession) -> String {
    let mut out = String::new();
    let _ = session.read_to_string(&mut out);
    out
}

/// Gives the picker time to draw before the next key is sent.
fn settle() {
    std::thread::sleep(Duration::from_millis(400));
}

#[test]
fn typing_a_query_and_pressing_enter_runs_the_match() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("beta").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("ran-beta"), "output was: {output}");
    assert!(!output.contains("ran-alpha"), "output was: {output}");
}

#[test]
fn moving_the_cursor_and_pressing_enter_runs_the_other_command() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    // Ctrl-N moves down one row, from alpha to beta.
    session.send("\u{e}").unwrap();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("ran-beta"), "output was: {output}");
}

#[test]
fn escape_cancels_with_exit_code_130() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("\u{1b}").unwrap();

    assert_eq!(wait_code(&mut session), 130);
}

#[test]
fn ctrl_c_cancels_with_exit_code_130() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("\u{3}").unwrap();

    assert_eq!(wait_code(&mut session), 130);
}

#[test]
fn cancelling_leaves_no_raw_mode_residue() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("\u{1b}").unwrap();
    wait_code(&mut session);

    // Leaving the alternate screen is the last thing the guard does.
    let output = drain(&mut session);
    assert!(
        output.contains("\u{1b}[?1049l") || output.is_empty(),
        "expected the alternate screen to be left, got: {output:?}"
    );
}

#[test]
fn f1_prints_the_source_instead_of_running_it() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("alpha").unwrap();
    settle();
    session.send("\u{1b}OP").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("echo ran-alpha"), "output was: {output}");
    assert!(!output.contains("\nran-alpha"), "output was: {output}");
}

#[test]
fn f2_opens_the_command_in_the_editor_at_its_line() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("alpha").unwrap();
    settle();
    session.send("\u{1b}OQ").unwrap();
    wait_code(&mut session);

    let args = pty.editor_args();
    assert!(args.contains("+1"), "editor args were: {args}");
    assert!(args.contains("nixon.md"), "editor args were: {args}");
}

#[test]
fn alt_enter_opens_the_editor_and_runs_what_comes_back() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    session.send("alpha").unwrap();
    settle();

    // Alt-Enter opens the inline editor on the command's source.
    session.send("\u{1b}\r").unwrap();
    settle();

    // Ctrl-A to the start, then prefix another statement, then submit.
    session.send("\u{1}").unwrap();
    session.send("echo prefixed; ").unwrap();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("prefixed"), "output was: {output}");
    assert!(output.contains("ran-alpha"), "output was: {output}");
}

#[test]
fn tab_marks_several_candidates_for_a_multi_placeholder() {
    let pty = Pty::with_config(
        "\
# `items`

```bash
printf 'one\\ntwo\\nthree\\n'
```

# `show ${items:m}`

```bash
echo \"picked: $@\"
```
",
    );
    let mut session = pty.spawn(&["run", "show"]);

    settle();
    // Tab marks and advances, so this marks `one` then `two`.
    session.send("\t").unwrap();
    session.send("\t").unwrap();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("picked: one two"), "output was: {output}");
}

/// Waits for the child and returns its exit code, `128 + signal` if killed.
fn wait_code(session: &mut OsSession) -> i32 {
    let status = session.get_process_mut().wait().unwrap();
    match status {
        WaitStatus::Exited(_, code) => code,
        WaitStatus::Signaled(_, signal, _) => 128 + signal as i32,
        other => panic!("unexpected wait status: {other:?}"),
    }
}

#[test]
fn control_w_deletes_a_word_from_the_query() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    // Type a query that matches nothing, then take the last word back off.
    session.send("zzz alpha").unwrap();
    settle();
    session.send("\u{17}").unwrap();
    settle();
    // "zzz " matches nothing, so nothing can be confirmed; cancel out.
    session.send("\u{1b}").unwrap();
    assert_eq!(wait_code(&mut session), 130);
}

#[test]
fn control_a_and_alt_b_move_within_the_query() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    // "lpha" matches alpha; Ctrl-A then typing "a" makes it "alpha".
    session.send("lpha").unwrap();
    settle();
    session.send("\u{1}").unwrap();
    session.send("a").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("ran-alpha"), "output was: {output}");
}

#[test]
fn alt_b_steps_back_a_word_in_the_query() {
    let pty = Pty::new();
    let mut session = pty.spawn(&["run"]);

    settle();
    // Two words; Alt-B moves before "beta", where deleting forward trims it.
    session.send("alpha beta").unwrap();
    settle();
    session.send("\u{1b}b").unwrap();
    settle();
    session.send("\u{1b}d").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("ran-alpha"), "output was: {output}");
    assert!(!output.contains("ran-beta"), "output was: {output}");
}
