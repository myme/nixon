//! Functional tests against the built binary. SPEC §10, ENGINEERING §5.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use assert_cmd::Command;
use assert_fs::TempDir;
use assert_fs::prelude::*;
use predicates::prelude::PredicateBooleanExt as _;
use predicates::str::contains;

const NIXON_MD: &str = "\
# `hello`

Say hello.

```bash
echo \"Hello World\"
```

# `boom`

```bash
exit 3
```

# `_hidden`

```bash
echo hidden
```
";

/// A project with a nixon.md, and XDG dirs of its own.
struct Fixture {
    temp: TempDir,
}

impl Fixture {
    fn new() -> Self {
        Self::with_config(NIXON_MD)
    }

    /// The project carries a marker and the global config declares the type,
    /// so `find_in_project` recognises it. Without that, subcommands that
    /// resolve a project fall back to the picker and fail with no TTY —
    /// which is what v1 does too (SPEC §10.3).
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

    /// The binary, with the host environment cleared. ENGINEERING §5.
    fn nixon(&self) -> Command {
        let mut cmd = Command::cargo_bin("nixon").unwrap();
        cmd.env_clear()
            .env("PATH", std::env::var("PATH").unwrap_or_default())
            .env("HOME", self.temp.path())
            .env("XDG_CONFIG_HOME", self.temp.child("config").path())
            .env("XDG_CACHE_HOME", self.temp.child("cache").path())
            .env("SHELL", "/bin/bash")
            .current_dir(self.temp.child("project").path());
        cmd
    }
}

#[test]
fn help_describes_the_v2_flags_only() {
    let fixture = Fixture::new();
    let output = fixture.nixon().arg("--help").assert().success();
    let help = String::from_utf8_lossy(&output.get_output().stdout).into_owned();

    // Deterministic because the config default collapses $HOME to `~`.
    insta::assert_snapshot!(help);
}

#[test]
fn help_shows_the_computed_default_config_path_with_home_collapsed() {
    Fixture::new()
        .nixon()
        .arg("--help")
        .assert()
        .success()
        .stdout(contains("[default: ~/config/nixon.md]"));
}

#[test]
fn the_removed_backend_flags_are_unexpected_arguments() {
    for flag in ["-b", "--backend", "-T", "--force-tty", "-t", "--terminal"] {
        Fixture::new()
            .nixon()
            .args([flag, "fzf"])
            .assert()
            .failure()
            .stderr(contains("unexpected argument"));
    }
}

#[test]
fn run_list_prints_every_command_including_hidden_ones() {
    Fixture::new()
        .nixon()
        .args(["run", "-l"])
        .assert()
        .success()
        .stdout("_hidden\nboom\nhello - Say hello.\n");
}

#[test]
fn run_list_with_a_query_filters() {
    Fixture::new()
        .nixon()
        .args(["run", "-l", "hello"])
        .assert()
        .success()
        .stdout("hello - Say hello.\n");
}

#[test]
fn run_list_with_no_matches_says_so_on_stderr_and_still_exits_zero() {
    Fixture::new()
        .nixon()
        .args(["run", "-l", "zzzz"])
        .assert()
        .success()
        .stdout("")
        .stderr(contains("No commands."));
}

#[test]
fn a_unique_query_runs_without_a_terminal() {
    Fixture::new()
        .nixon()
        .args(["run", "hello"])
        .assert()
        .success()
        .stdout("Hello World\n");
}

#[test]
fn bare_arguments_are_the_run_subcommand() {
    Fixture::new()
        .nixon()
        .arg("hello")
        .assert()
        .success()
        .stdout("Hello World\n");
}

#[test]
fn a_child_exit_code_becomes_the_process_exit_code() {
    Fixture::new()
        .nixon()
        .args(["run", "boom"])
        .assert()
        .code(3);
}

#[test]
fn insert_prints_the_source_including_its_trailing_newline() {
    Fixture::new()
        .nixon()
        .args(["run", "-i", "hello"])
        .assert()
        .success()
        .stdout("echo \"Hello World\"\n");
}

#[test]
fn eval_runs_an_inline_expression() {
    Fixture::new()
        .nixon()
        .args(["eval", "echo from-eval"])
        .assert()
        .success()
        .stdout("from-eval\n");
}

#[test]
fn eval_works_outside_any_recognised_project() {
    let fixture = Fixture::new();
    // No marker here, so this directory is not a project.
    let elsewhere = fixture.temp.child("elsewhere");
    elsewhere.create_dir_all().unwrap();

    let mut cmd = fixture.nixon();
    cmd.current_dir(elsewhere.path())
        .args(["eval", "echo anywhere"])
        .assert()
        .success()
        .stdout("anywhere\n");
}

#[test]
fn eval_reads_a_file_with_dash_f() {
    let fixture = Fixture::new();
    let script = fixture.temp.child("script.sh");
    script.write_str("echo from-file\n").unwrap();

    fixture
        .nixon()
        .args(["eval", "-f", script.path().to_str().unwrap()])
        .assert()
        .success()
        .stdout("from-file\n");
}

#[test]
fn gc_reports_what_it_removes() {
    let fixture = Fixture::new();
    fixture.nixon().args(["run", "hello"]).assert().success();

    fixture
        .nixon()
        .args(["gc", "--dry-run"])
        .assert()
        .success()
        .stdout(contains("would remove ").and(contains("-hello.sh")));

    fixture
        .nixon()
        .arg("gc")
        .assert()
        .success()
        .stdout(contains("removed ").and(contains("-hello.sh")));

    fixture.nixon().arg("gc").assert().success().stdout("");
}

#[test]
fn a_missing_global_config_is_tolerated() {
    Fixture::new()
        .nixon()
        .args(["-C", "/nonexistent/nixon.md", "run", "-l"])
        .assert()
        .success()
        .stdout(contains("hello"));
}

#[test]
fn a_config_parse_error_is_fatal_and_names_the_file() {
    let fixture = Fixture::with_config("# `broken`\n\nno source block\n");
    fixture
        .nixon()
        .args(["run", "-l"])
        .assert()
        .failure()
        .code(1)
        .stderr(contains("Expecting source block for broken"));
}

#[test]
fn project_list_prints_discovered_projects() {
    let fixture = Fixture::new();
    fixture
        .temp
        .child("config/nixon.md")
        .write_str(&format!(
            "```json config\n{{\"project_dirs\": [\"{}\"], \"project_types\": [{{\"name\": \"git\", \"test\": [\".git\"], \"desc\": \"Git\"}}]}}\n```\n",
            fixture.temp.path().display()
        ))
        .unwrap();

    fixture
        .nixon()
        .args(["project", "-l"])
        .assert()
        .success()
        .stdout(contains("project"));
}

#[test]
fn an_unknown_placeholder_command_is_a_clean_error() {
    let fixture = Fixture::with_config("# `uses ${nope}`\n\n```bash\necho \"$1\"\n```\n");
    fixture
        .nixon()
        .args(["run", "uses"])
        .assert()
        .failure()
        .code(1)
        .stderr(contains("Invalid argument: nope"));
}

#[test]
fn a_placeholder_with_a_unique_match_resolves_without_a_terminal() {
    let fixture = Fixture::with_config(
        "# `one-file`\n\n```bash\necho only.txt\n```\n\n# `show ${one-file}`\n\n```bash\necho \"got $1\"\n```\n",
    );
    fixture
        .nixon()
        .args(["run", "show"])
        .assert()
        .success()
        .stdout("got only.txt\n");
}

#[test]
fn a_list_placeholder_prints_every_candidate() {
    let fixture = Fixture::with_config(
        "# `files`\n\n```bash\nprintf 'a\\nb\\n'\n```\n\n# `show ${files | list}`\n\n```bash\necho \"$@\"\n```\n",
    );
    fixture
        .nixon()
        .args(["run", "show"])
        .assert()
        .success()
        .stdout("a b\n");
}

#[test]
fn select_runs_the_command_and_prints_its_output_lines() {
    let fixture = Fixture::with_config("# `files`\n\n```bash\nprintf 'only.txt\\n'\n```\n");
    fixture
        .nixon()
        .args(["run", "-s", "files"])
        .assert()
        .success()
        .stdout("only.txt\n\n");
}

#[test]
fn the_project_path_is_in_the_environment() {
    let fixture = Fixture::with_config("# `where`\n\n```bash\necho \"$nixon_project_path\"\n```\n");
    let project = fixture.temp.child("project");
    fixture
        .nixon()
        .args(["run", "where"])
        .assert()
        .success()
        .stdout(format!("{}\n", project.path().display()));
}

/// One command, so `-1` resolves "Insert after" without a terminal.
const ONE_COMMAND_MD: &str = "# `hello`\n\nSay hello.\n\n```bash\necho \"Hello World\"\n```\n";

#[test]
fn new_splices_a_command_in_when_confirmed() {
    let fixture = Fixture::with_config(ONE_COMMAND_MD);
    fixture
        .nixon()
        .args(["new", "-n", "spliced", "-s", "echo spliced"])
        .env("EDITOR", "true")
        .write_stdin("y\n")
        .assert()
        .success();

    let updated = std::fs::read_to_string(fixture.temp.child("project/nixon.md").path()).unwrap();
    assert!(updated.contains("`spliced`"), "nixon.md was: {updated}");
    assert!(updated.contains("echo spliced"), "nixon.md was: {updated}");
    // The command it was inserted after is still there.
    assert!(updated.contains("`hello`"), "nixon.md was: {updated}");
}

#[test]
fn new_leaves_the_file_alone_when_declined() {
    let fixture = Fixture::with_config(ONE_COMMAND_MD);
    let before = std::fs::read_to_string(fixture.temp.child("project/nixon.md").path()).unwrap();

    fixture
        .nixon()
        .args(["new", "-n", "spliced"])
        .env("EDITOR", "true")
        .write_stdin("n\n")
        .assert()
        .success();

    let after = std::fs::read_to_string(fixture.temp.child("project/nixon.md").path()).unwrap();
    assert_eq!(before, after);
}

#[test]
fn new_asks_before_writing() {
    Fixture::with_config(ONE_COMMAND_MD)
        .nixon()
        .args(["new", "-n", "spliced"])
        .env("EDITOR", "true")
        .write_stdin("n\n")
        .assert()
        .success()
        .stdout(contains("Update ").and(contains("? [y/N]")));
}
