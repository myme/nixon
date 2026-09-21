//! PTY-driven tests of the real event loop.
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

    /// Spawns nixon on a PTY with its stdout redirected to a file.
    ///
    /// stdin and stderr stay on the pty, so the picker draws and reads as
    /// usual while stdout is captured — which is what a shell widget does
    /// with `$(nixon …)`, and the only way to tell the two streams apart
    /// when both are the same pty.
    fn spawn_capturing(&self, args: &[&str]) -> OsSession {
        let quoted: Vec<String> = args
            .iter()
            .map(|arg| format!("'{}'", arg.replace('\'', "'\\''")))
            .collect();
        let script = format!(
            "exec {} {} > {}/stdout",
            Self::binary(),
            quoted.join(" "),
            self.temp.path().display()
        );

        let mut command = std::process::Command::new("sh");
        command.arg("-c").arg(script);
        self.apply(&mut command);
        let mut session = Session::spawn(command).unwrap();
        session.get_process_mut().set_window_size(80, 24).unwrap();
        session.set_expect_timeout(Some(Duration::from_secs(20)));
        session
    }

    /// What the captured run wrote to stdout.
    fn captured_stdout(&self) -> String {
        std::fs::read_to_string(self.temp.child("stdout").path()).unwrap_or_default()
    }

    /// Spawns nixon on a PTY with the host environment cleared.
    fn spawn(&self, args: &[&str]) -> OsSession {
        let mut command = std::process::Command::new(Self::binary());
        self.apply(&mut command);
        command.args(args);

        let mut session = Session::spawn(command).unwrap();
        // A real terminal size, so the picker actually draws. A fresh pty is
        // 0x0, where ratatui renders nothing and the tests measure far less
        // than they look like they do.
        session.get_process_mut().set_window_size(80, 24).unwrap();
        session.set_expect_timeout(Some(Duration::from_secs(20)));
        session
    }

    /// The environment every spawn runs with.
    fn apply(&self, command: &mut std::process::Command) {
        command
            .env_clear()
            .env("PATH", std::env::var("PATH").unwrap_or_default())
            .env("HOME", self.temp.path())
            .env("TERM", "xterm-256color")
            .env("XDG_CONFIG_HOME", self.temp.child("config").path())
            .env("XDG_CACHE_HOME", self.temp.child("cache").path())
            .env("SHELL", "/bin/bash")
            .env("EDITOR", self.fake_editor())
            .current_dir(self.temp.child("project").path());

        // The environment is cleared, but bash needs terminfo to turn line
        // editing on at all: without it the widget's key binding arrives as
        // literal text.
        for name in ["TERMINFO", "TERMINFO_DIRS"] {
            if let Ok(value) = std::env::var(name) {
                command.env(name, value);
            }
        }
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

/// Reads the PTY until the child closes it, and returns its exit code.
///
/// The read is what makes the wait terminate. A pty buffer holds about a
/// kilobyte on macOS against 64 KiB on Linux, so a child drawing a full
/// screen fills it long before it exits; waiting for the child without
/// reading leaves it blocked in `write` and the wait never returns. On
/// Linux the whole session fits in the buffer and the bug is invisible.
///
/// Bounded by the session's expect timeout rather than by `read_to_end`, so
/// a child that really does hang fails the test instead of the run.
fn finish(session: &mut OsSession) -> (i32, String) {
    let found = match session.expect(expectrl::Eof) {
        Ok(found) => found,
        Err(err) => panic!("the child never closed the pty: {err}"),
    };
    let output = String::from_utf8_lossy(found.as_bytes()).into_owned();

    let status = session.get_process_mut().wait().unwrap();
    let code = match status {
        WaitStatus::Exited(_, code) => code,
        WaitStatus::Signaled(_, signal, _) => 128 + signal as i32,
        other => panic!("unexpected wait status: {other:?}"),
    };
    (code, output)
}

/// Everything the child wrote, once it has finished.
///
/// Assertions go here rather than against the drawn screen: ratatui redraws
/// with cursor-positioning escapes between characters, so on-screen text
/// rarely appears as one contiguous byte string.
fn drain(session: &mut OsSession) -> String {
    finish(session).1
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
    let (_, output) = finish(&mut session);

    // Leaving the alternate screen is the last thing the guard does.
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
    finish(session).0
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

/// The picker must be usable while the command that
/// produces its candidates is still running.
///
/// The command emits two lines at once and then sleeps. Two matter: with
/// `-1` in force a single candidate might still turn out to be the only one,
/// so nixon cannot draw until a second arrives and rules `-1` out. That is
/// also the moment a real command listing files rules it out — immediately.
#[test]
fn the_picker_is_interactive_before_a_slow_command_finishes() {
    let pty = Pty::with_config(
        "\
# `slow-files`

```bash
printf 'alpha\\nbeta\\n'
sleep 6
printf 'omega\\n'
```

# `show ${slow-files}`

```bash
echo \"picked: $1\"
```
",
    );
    let started = std::time::Instant::now();
    let mut session = pty.spawn(&["run", "show"]);

    // The first line is out almost at once; the command runs six more
    // seconds. Pick while it is still going.
    settle();
    session.send("alpha").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    let elapsed = started.elapsed();

    assert!(output.contains("picked: alpha"), "output was: {output}");
    assert!(
        elapsed < Duration::from_secs(6),
        "took {elapsed:?}; the picker waited for the command instead of \
         streaming"
    );
}

/// Cancelling stops the command rather than leaving it running.
///
/// Two lines again, so the picker is open and can take the `Esc`.
#[test]
fn cancelling_kills_a_still_running_candidate_command() {
    let pty = Pty::with_config(
        "\
# `slow-files`

```bash
printf 'alpha\\nbeta\\n'
sleep 30
```

# `show ${slow-files}`

```bash
echo \"picked: $1\"
```
",
    );
    let started = std::time::Instant::now();
    let mut session = pty.spawn(&["run", "show"]);

    settle();
    session.send("\u{1b}").unwrap();

    assert_eq!(wait_code(&mut session), 130);
    assert!(
        started.elapsed() < Duration::from_secs(25),
        "nixon waited for the killed command to finish"
    );
}

/// Shift-Tab marks and steps upwards, so a run can be taken from the bottom.
#[test]
fn shift_tab_marks_two_rows_from_the_bottom() {
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
    // Down to the last row, then mark upwards: three, then two.
    session.send("\u{1b}[B").unwrap();
    session.send("\u{1b}[B").unwrap();
    settle();
    // CSI Z is Shift-Tab as a legacy terminal sends it.
    session.send("\u{1b}[Z").unwrap();
    session.send("\u{1b}[Z").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("picked: two three"), "output was: {output}");
}

/// Marks are keyed by candidate, so they outlive the query that made them.
#[test]
fn marks_survive_a_query_change() {
    let pty = Pty::with_config(
        "\
# `items`

```bash
printf 'alpha-one\\nbeta-two\\nalpha-three\\n'
```

# `show ${items:m}`

```bash
echo \"picked: $@\"
```
",
    );
    let mut session = pty.spawn(&["run", "show"]);

    settle();
    // Mark a row under one search...
    session.send("alpha-one").unwrap();
    settle();
    session.send("\t").unwrap();
    settle();

    // ...clear it, search for something else, and mark there too.
    session.send("\u{15}").unwrap();
    session.send("beta").unwrap();
    settle();
    session.send("\t").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(
        output.contains("picked: alpha-one beta-two"),
        "output was: {output}"
    );
}

/// The heading the user reported the missing options row for.
const GWR_MD: &str = "\
# `_git-worktree`

```bash
printf '/tmp/wt-one\\n/tmp/wt-two\\n'
```

# `gwr --force ${_git-worktree}`

Remove a worktree.

- `--force`: off — also removes worktrees with local changes

```bash
echo \"gwr args: $*\"
```

# `flags --release`

- `--release`: off

```bash
echo \"flags args: $* release=$nixon_opt_release\"
```
";

/// The options row is drawn on the placeholder picker, and `Alt-1` there
/// changes what runs.
#[test]
fn alt_one_toggles_an_option_at_the_placeholder_picker() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "gwr"]);

    settle();
    // Alt-1 flips `--force`, then Enter takes the first worktree.
    session.send("\u{1b}1").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(
        output.contains("gwr args: --force /tmp/wt-one"),
        "output was: {output}"
    );
}

/// Without the toggle, the same picker runs on the declared default.
#[test]
fn a_placeholder_picker_leaves_an_option_at_its_default() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "gwr"]);

    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(
        output.contains("gwr args: /tmp/wt-one"),
        "output was: {output}"
    );
}

/// A command with options and no placeholder gets a prompt of its own.
#[test]
fn the_confirm_prompt_toggles_and_runs() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "flags"]);

    settle();
    // Space toggles the focused option, Enter runs.
    session.send(" ").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(
        output.contains("flags args: --release release=1"),
        "output was: {output}"
    );
}

/// Escaping the confirm prompt cancels rather than running on defaults.
#[test]
fn escaping_the_confirm_prompt_exits_130() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "flags"]);

    settle();
    session.send("\u{1b}").unwrap();

    assert_eq!(wait_code(&mut session), 130);
}

/// A command line that settles every option never opens the prompt.
#[test]
fn a_complete_command_line_runs_without_the_confirm_prompt() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "flags", "--release"]);

    let output = drain(&mut session);
    assert!(
        output.contains("flags args: --release release=1"),
        "output was: {output}"
    );
}

const OVERLAPPING_MD: &str = "\
# `_worktrees`

```bash
printf 'bugs\\nbugs2\\n'
```

# `open ${_worktrees}`

```bash
echo \"opened $1\"
```
";

/// An argument that is only a fuzzy match still opens the picker, where the
/// pre-filled query narrows to two rows and the user chooses.
#[test]
fn a_fuzzy_placeholder_argument_still_opens_the_picker() {
    let pty = Pty::with_config(OVERLAPPING_MD);
    let mut session = pty.spawn(&["run", "open", "bgs"]);

    settle();
    // Two rows match; move to the second and take it.
    session.send("\u{e}").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(output.contains("opened bugs2"), "output was: {output}");
}

/// An exact argument never draws: the command runs straight away.
#[test]
fn an_exact_placeholder_argument_runs_without_drawing() {
    let pty = Pty::with_config(OVERLAPPING_MD);
    let mut session = pty.spawn(&["run", "open", "bugs"]);

    let output = drain(&mut session);
    assert!(output.contains("opened bugs"), "output was: {output}");
    assert!(!output.contains("opened bugs2"), "output was: {output}");
}

/// `Alt-o` moves onto the options row, where `Space` toggles; walking off
/// the end goes back to the query, and `Enter` runs.
#[test]
fn alt_o_and_space_toggle_an_option() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "gwr"]);

    settle();
    session.send("\u{1b}o").unwrap();
    settle();
    session.send(" ").unwrap();
    settle();
    // Off the end of the one-option row, back to the query.
    session.send("\u{1b}[C").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(
        output.contains("gwr args: --force /tmp/wt-one"),
        "output was: {output}"
    );
}

/// From the options row, `Enter` runs rather than toggling.
#[test]
fn enter_confirms_from_the_options_row() {
    let pty = Pty::with_config(GWR_MD);
    let mut session = pty.spawn(&["run", "gwr"]);

    settle();
    session.send("\u{1b}o").unwrap();
    settle();
    session.send("\r").unwrap();

    let output = drain(&mut session);
    assert!(
        output.contains("gwr args: /tmp/wt-one"),
        "output was: {output}"
    );
}

/// Nothing nixon writes for the terminal may reach stdout.
///
/// `cd "$(nixon project -s)"` broke on this: crossterm's keyboard-detection
/// query went to stdout — its `/dev/tty` handle is opened read-only, so the
/// write to it always fails and the fallback always runs — and `cd` was
/// handed `^[[?u^[[c/Users/…`.
#[test]
fn stdout_carries_no_escape_sequences() {
    let pty = Pty::with_config("# `files`\n\n```bash\nprintf 'a.txt\\nb.txt\\n'\n```\n");
    let mut session = pty.spawn_capturing(&["run", "-s", "files"]);

    settle();
    // Two candidates and multi-select, so the picker really opens.
    session.send("\r").unwrap();
    assert_eq!(wait_code(&mut session), 0);

    let stdout = pty.captured_stdout();
    assert!(
        !stdout.contains('\u{1b}'),
        "an escape sequence reached stdout: {stdout:?}"
    );
    assert_eq!(stdout, "a.txt\n\n");
}

/// The same for the two other things a widget captures.
#[test]
fn insert_and_project_select_keep_stdout_clean() {
    let pty = Pty::new();
    let mut session = pty.spawn_capturing(&["run", "-i"]);

    settle();
    session.send("\r").unwrap();
    assert_eq!(wait_code(&mut session), 0);

    let stdout = pty.captured_stdout();
    assert!(
        !stdout.contains('\u{1b}'),
        "an escape sequence reached stdout: {stdout:?}"
    );
    assert_eq!(stdout, "echo ran-alpha\n");
}

/// The bash `Alt-p` widget changes directory and leaves no history entry.
///
/// Driven through a real interactive bash so the readline macro is what is
/// tested, not a reimplementation of it.
#[test]
#[cfg(unix)]
fn the_bash_widget_cds_into_a_project_without_a_history_entry() {
    let pty = Pty::new();
    let projects = pty.temp.child("code");
    for name in ["alpha-project", "beta-project"] {
        projects.child(name).child(".git").create_dir_all().unwrap();
    }
    pty.temp
        .child("config/nixon.md")
        .write_str(&format!(
            "```json config\n{{\"project_dirs\": [\"{}\"], \"project_types\": [{{\"name\": \"git\", \"test\": [\".git\"], \"desc\": \"Git\"}}]}}\n```\n",
            projects.path().display()
        ))
        .unwrap();

    // nixon has to be on PATH for the widget to find it.
    let bin_dir = pty.temp.child("bin");
    bin_dir.create_dir_all().unwrap();
    std::os::unix::fs::symlink(Pty::binary(), bin_dir.child("nixon").path()).unwrap();

    let rc = pty.temp.child("bashrc");
    rc.write_str(&format!(
        "PATH={}:$PATH\nHISTCONTROL=ignoreboth\nHISTFILE=\nPS1='ready> '\nsource {}\n",
        bin_dir.path().display(),
        std::fs::canonicalize("../../extra/nixon-widget.bash")
            .unwrap()
            .display()
    ))
    .unwrap();

    let mut command = std::process::Command::new("bash");
    command.args([
        "--noprofile",
        "--rcfile",
        &rc.path().to_string_lossy(),
        "-i",
    ]);
    pty.apply(&mut command);

    let mut session = Session::spawn(command).unwrap();
    session.get_process_mut().set_window_size(80, 24).unwrap();
    session.set_expect_timeout(Some(Duration::from_secs(20)));

    session.expect("ready> ").unwrap();
    // Alt-p, then narrow to the one project and take it.
    session.send("\u{1b}p").unwrap();
    settle();
    session.send("alpha").unwrap();
    settle();
    session.send("\r").unwrap();
    settle();

    session.send("echo PWD=$PWD\r").unwrap();
    let want = format!("PWD={}", projects.child("alpha-project").path().display());
    if session.expect(want.as_str()).is_err() {
        session.send("exit\r").unwrap();
        let (_, seen) = finish(&mut session);
        panic!("the widget did not change directory.\nwanted {want}\nsaw {seen:?}");
    }

    // The prior line comes back untouched: the macro saves and restores it.
    session.send("echo BACK=[$READLINE_LINE]\r").unwrap();
    session
        .expect("BACK=[]")
        .expect("the widget left something on the command line");

    session.send("exit\r").unwrap();
    let _ = finish(&mut session);
}
