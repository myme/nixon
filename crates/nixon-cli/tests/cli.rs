//! Functional tests against the built binary.

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
    /// which is what v1 does too.
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

    /// The binary, with the host environment cleared.
    fn nixon(&self) -> Command {
        let mut cmd = Command::cargo_bin("nixon").unwrap();
        self.apply(&mut cmd);
        cmd
    }

    /// The same environment, for a command that wraps the binary.
    fn apply(&self, cmd: &mut Command) {
        cmd.env_clear()
            .env("PATH", std::env::var("PATH").unwrap_or_default())
            .env("HOME", self.temp.path())
            .env("XDG_CONFIG_HOME", self.temp.child("config").path())
            .env("XDG_CACHE_HOME", self.temp.child("cache").path())
            .env("XDG_STATE_HOME", self.temp.child("state").path())
            .env("SHELL", "/bin/bash")
            .current_dir(self.temp.child("project").path());
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

/// The prompt is for a person, so it belongs on stderr with the picker;
/// stdout is data.
#[test]
fn new_asks_before_writing_on_stderr() {
    Fixture::with_config(ONE_COMMAND_MD)
        .nixon()
        .args(["new", "-n", "spliced"])
        .env("EDITOR", "true")
        .write_stdin("n\n")
        .assert()
        .success()
        .stdout(contains("? [y/N]").not())
        .stderr(contains("Update ").and(contains("? [y/N]")));
}

/// Writing the file must not change what it is: a copy over the original
/// keeps the mode, but so must a rename.
#[test]
#[cfg(unix)]
fn new_keeps_the_config_files_permissions() {
    use std::os::unix::fs::PermissionsExt as _;

    let fixture = Fixture::with_config(ONE_COMMAND_MD);
    let config = fixture.temp.child("project/nixon.md");
    std::fs::set_permissions(config.path(), std::fs::Permissions::from_mode(0o640)).unwrap();

    fixture
        .nixon()
        .args(["new", "-n", "spliced"])
        .env("EDITOR", "true")
        .write_stdin("y\n")
        .assert()
        .success();

    let mode = std::fs::metadata(config.path())
        .unwrap()
        .permissions()
        .mode();
    assert_eq!(mode & 0o777, 0o640, "the file's mode changed");
    assert!(
        std::fs::read_to_string(config.path())
            .unwrap()
            .contains("`spliced`"),
        "the update did not land"
    );
}

/// `docs/cli.md` opens with `nixon --help`; this keeps that block honest.
///
/// A direct comparison rather than trycmd: it is the only console block in
/// the documentation, and trycmd would pull in a dependency tree to check
/// one command whose environment-dependent default path still needs
/// normalising by hand.
#[test]
fn the_documented_help_block_matches_the_binary() {
    let doc = std::fs::read_to_string("../../docs/cli.md").unwrap();
    let block = doc
        .split("```console\n$ nixon --help\n")
        .nth(1)
        .and_then(|rest| rest.split("\n```").next())
        .expect("docs/cli.md should contain a `nixon --help` console block");

    let output = Fixture::new().nixon().arg("--help").assert().success();
    let actual = String::from_utf8_lossy(&output.get_output().stdout).into_owned();

    // The config default is a real path in the binary and `[..]` in the
    // documentation, since it depends on the environment.
    let normalise = |text: &str| -> Vec<String> {
        text.lines()
            .map(|line| {
                line.find("[default: ").map_or_else(
                    || line.to_owned(),
                    |at| format!("{}[default: [..]]", &line[..at]),
                )
            })
            .filter(|line| !line.trim().is_empty())
            .collect()
    };

    assert_eq!(
        normalise(block),
        normalise(&actual),
        "the --help block in docs/cli.md has drifted from the binary"
    );
}

/// `internal` is packaging machinery; `--help` must not offer it.
#[test]
fn the_internal_subcommand_is_hidden() {
    Fixture::new()
        .nixon()
        .arg("--help")
        .assert()
        .success()
        .stdout(contains("internal").not());
}

/// The nix build pipes this straight into `share/man/man1/nixon.1`.
#[test]
fn internal_mangen_writes_a_man_page() {
    let output = Fixture::new()
        .nixon()
        .args(["internal", "mangen"])
        .assert()
        .success();
    let page = String::from_utf8_lossy(&output.get_output().stdout).into_owned();

    assert!(
        page.lines().any(|line| line.starts_with(".TH nixon 1")),
        "man page should carry a .TH header, got:\n{page}"
    );
    assert!(
        page.contains(".SH \"SEE ALSO\""),
        "man page should point at the docs/ pages"
    );
}

/// With no controlling terminal, a pick that needs one must say so rather
/// than surface the raw `ENXIO` from `/dev/tty`.
#[test]
#[cfg(unix)]
fn a_selection_without_a_terminal_is_a_named_error() {
    let fixture = Fixture::new();
    let nixon = assert_cmd::cargo::cargo_bin("nixon");

    // setsid drops the controlling terminal; stderr may still be one.
    let mut cmd = Command::new("setsid");
    cmd.arg("--wait").arg(nixon);
    fixture.apply(&mut cmd);

    cmd.write_stdin("")
        .assert()
        .failure()
        .code(1)
        .stderr(contains("interactive selection needs a terminal"));
}

/// `--list` matches the way the picker does, so a local `exact_match` has to
/// reach it too; it used to read the global config only.
#[test]
fn list_matching_honours_the_projects_own_config() {
    let fixture = Fixture::with_config(
        "\
```json config
{\"exact_match\": true}
```

# `deploy-staging`

```bash
echo staging
```
",
    );

    // `dpst` is a fuzzy match for `deploy-staging`, and no exact one.
    fixture
        .nixon()
        .args(["run", "-l", "dpst"])
        .assert()
        .success()
        .stdout("")
        .stderr(contains("No commands."));

    // The same query without the local setting does match.
    Fixture::new()
        .nixon()
        .args(["run", "-l", "hlo"])
        .assert()
        .success()
        .stdout(contains("hello"));
}

/// fzf's extended syntax: `gaia$` is a suffix match, not a literal `$`.
///
/// The suffix narrows two projects to one, so `-1` selects it and no
/// terminal is needed.
#[test]
fn a_query_operator_narrows_the_project_selection() {
    let fixture = Fixture::new();
    let src = fixture.temp.child("src");
    for name in ["gaia", "gaia-old"] {
        src.child(name).child(".git").create_dir_all().unwrap();
    }

    fixture
        .temp
        .child("config/nixon.md")
        .write_str(&format!(
            "```json config\n{{\"project_dirs\": [\"{}\"], \"project_types\": [{{\"name\": \"git\", \"test\": [\".git\"], \"desc\": \"Git\"}}]}}\n```\n",
            src.path().display()
        ))
        .unwrap();

    fixture
        .nixon()
        .args(["project", "gaia$", "-s"])
        .assert()
        .success()
        .stdout(format!("{}\n", src.child("gaia").path().display()));
}

/// The picker colours inline code in a description; `--list` is data and
/// must be plain, with no markers and no inserted spaces.
#[test]
fn list_output_is_plain_text() {
    Fixture::with_config(
        "# `build`\n\nRun `cargo build` for the workspace.\n\n```bash\ncargo build\n```\n",
    )
    .nixon()
    .args(["run", "-l"])
    .assert()
    .success()
    .stdout("build - Run cargo build for the workspace.\n");
}

const OPTIONS_MD: &str = "\
# `show --force -v`

Shows what it was given.

- `--force`: off — the dangerous one
- `-v`: on

```bash
echo \"args: $*\"
echo \"force=$nixon_opt_force verbose=$nixon_opt_v\"
```
";

/// Option tokens on the command line settle the options, and land in argv
/// where the heading put them.
#[test]
fn option_tokens_on_the_command_line_reach_argv_and_the_environment() {
    Fixture::with_config(OPTIONS_MD)
        .nixon()
        .args(["show", "--force", "-v"])
        .assert()
        .success()
        .stdout("args: --force -v\nforce=1 verbose=1\n");
}

/// `--no-<name>` turns an option off, whatever its default says.
#[test]
fn no_prefixed_tokens_turn_options_off() {
    Fixture::with_config(OPTIONS_MD)
        .nixon()
        .args(["show", "--no-force", "--no-v"])
        .assert()
        .success()
        .stdout("args: \nforce= verbose=\n");
}

/// A `nixon.md` symlinked out of a dotfiles repository must stay a symlink,
/// and the file it points at must be the one that changes.
#[test]
#[cfg(unix)]
fn new_writes_through_a_symlinked_config() {
    let fixture = Fixture::with_config(ONE_COMMAND_MD);
    let real = fixture.temp.child("elsewhere/nixon.md");
    real.write_str(ONE_COMMAND_MD).unwrap();

    let link = fixture.temp.child("project/nixon.md");
    std::fs::remove_file(link.path()).unwrap();
    std::os::unix::fs::symlink(real.path(), link.path()).unwrap();

    fixture
        .nixon()
        .args(["new", "-n", "spliced"])
        .env("EDITOR", "true")
        .write_stdin("y\n")
        .assert()
        .success();

    assert!(
        std::fs::symlink_metadata(link.path())
            .unwrap()
            .file_type()
            .is_symlink(),
        "the symlink was replaced by a regular file"
    );
    assert!(
        std::fs::read_to_string(real.path())
            .unwrap()
            .contains("`spliced`"),
        "the file behind the link was not updated"
    );
}

/// The confirm prompt is a convenience: with no terminal to ask with, the
/// defaults are already valid and the command runs.
#[test]
#[cfg(unix)]
fn a_command_with_options_runs_on_its_defaults_without_a_terminal() {
    let fixture = Fixture::with_config(
        "# `show --force -v`\n\n- `-v`: on\n\n```bash\necho \"args: $*\"\n```\n",
    );
    let nixon = assert_cmd::cargo::cargo_bin("nixon");

    let mut cmd = Command::new("setsid");
    cmd.arg("--wait").arg(nixon).arg("show");
    fixture.apply(&mut cmd);

    cmd.write_stdin("").assert().success().stdout("args: -v\n");
}

/// The path is absolute and runnable, so a command can exec it.
#[test]
fn a_command_can_run_the_nixon_binary_it_was_started_by() {
    Fixture::with_config("# `again`\n\n```bash\n\"$nixon_bin\" --version\n```\n")
        .nixon()
        .arg("again")
        .assert()
        .success()
        .stdout(contains("nixon "));
}

const AMBIGUOUS_MD: &str = "\
# `link`

```bash
echo ran-link
```

# `link-all`

```bash
echo ran-link-all
```

# `_packages`

```bash
echo ran-packages
```
";

/// An exact name runs without a picker even though it also fuzzy-matches
/// `link-all`, and so without a terminal.
#[test]
#[cfg(unix)]
fn an_exact_command_name_runs_without_a_terminal() {
    let fixture = Fixture::with_config(AMBIGUOUS_MD);
    let nixon = assert_cmd::cargo::cargo_bin("nixon");

    let mut cmd = Command::new("setsid");
    cmd.arg("--wait").arg(nixon).arg("link");
    fixture.apply(&mut cmd);

    cmd.write_stdin("").assert().success().stdout("ran-link\n");
}

/// A hidden command is not offered by the picker, so naming it is the only
/// way to run it — and it must work.
#[test]
fn a_hidden_command_runs_when_named_in_full() {
    Fixture::with_config(AMBIGUOUS_MD)
        .nixon()
        .arg("_packages")
        .assert()
        .success()
        .stdout("ran-packages\n");
}

/// A project given as a path runs a command in it with no discovery.
#[test]
fn a_project_path_runs_a_command_in_that_directory() {
    let fixture = Fixture::with_config("# `where`\n\n```bash\necho \"$nixon_project_path\"\n```\n");
    let project = fixture.temp.child("project");

    fixture
        .nixon()
        .args(["project", &project.path().to_string_lossy(), "where"])
        .assert()
        .success()
        .stdout(format!(
            "{}\n",
            project.path().canonicalize().unwrap().display()
        ));
}

/// A path that is not there names itself in the error.
#[test]
fn a_project_path_that_does_not_exist_is_reported() {
    Fixture::new()
        .nixon()
        .args(["project", "/nowhere/at/all", "hello"])
        .assert()
        .failure()
        .code(1)
        .stderr(contains("no such project: /nowhere/at/all"));
}

/// The `.bare` container layout, built by real git rather than by hand, so
/// the fixtures elsewhere stay honest.
#[test]
fn project_list_includes_the_worktrees_of_a_bare_container() {
    let fixture = Fixture::new();
    let code = fixture.temp.child("code");
    let gaia = code.child("novem/gaia");
    gaia.create_dir_all().unwrap();

    let git = |args: &[&str], cwd: &std::path::Path| {
        let status = std::process::Command::new("git")
            .args(args)
            .current_dir(cwd)
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .env("GIT_AUTHOR_NAME", "t")
            .env("GIT_AUTHOR_EMAIL", "t@example.com")
            .env("GIT_COMMITTER_NAME", "t")
            .env("GIT_COMMITTER_EMAIL", "t@example.com")
            .output()
            .unwrap();
        assert!(status.status.success(), "git {args:?}: {status:?}");
    };

    git(
        &["init", "-q", "--bare", "-b", "main", ".bare"],
        gaia.path(),
    );
    // A worktree needs a commit to branch from.
    let seed = fixture.temp.child("seed");
    seed.create_dir_all().unwrap();
    git(&["init", "-q", "-b", "main", "."], seed.path());
    git(
        &["commit", "-q", "--allow-empty", "-m", "seed"],
        seed.path(),
    );
    git(
        &[
            "push",
            "-q",
            &gaia.child(".bare").path().to_string_lossy(),
            "main",
        ],
        seed.path(),
    );
    git(
        &["worktree", "add", "-q", "../bugs", "main"],
        gaia.child(".bare").path(),
    );

    fixture
        .temp
        .child("config/nixon.md")
        .write_str(&format!(
            "```json config\n{{\"project_dirs\": [\"{}/*\"], \"project_types\": [{{\"name\": \"any\", \"desc\": \"Any\"}}]}}\n```\n",
            code.path().display()
        ))
        .unwrap();

    fixture
        .nixon()
        .args(["project", "-l"])
        .assert()
        .success()
        .stdout(contains("gaia").and(contains("gaia/bugs")));
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

/// An exact value settles the placeholder, so no terminal is needed even
/// though `bugs` also fuzzy-matches `bugs2`.
#[test]
#[cfg(unix)]
fn an_exact_placeholder_value_runs_without_a_terminal() {
    let fixture = Fixture::with_config(OVERLAPPING_MD);
    let nixon = assert_cmd::cargo::cargo_bin("nixon");

    let mut cmd = Command::new("setsid");
    cmd.arg("--wait").arg(nixon).args(["open", "bugs"]);
    fixture.apply(&mut cmd);

    cmd.write_stdin("")
        .assert()
        .success()
        .stdout("opened bugs\n");
}

const CLASHING_MD: &str = "\
# `clash -i -l -s`

- `-i`: off
- `-l`: off
- `-s`: off

```bash
echo \"args: $*\"
```
";

/// Everything after the command name belongs to the command, so the two
/// spellings agree rather than one of them meaning `--insert`.
#[test]
fn a_flag_after_the_command_name_reaches_the_command() {
    let expected = "args: -i\n";
    for args in [
        ["run", "clash", "-i"].as_slice(),
        ["clash", "-i"].as_slice(),
    ] {
        Fixture::with_config(CLASHING_MD)
            .nixon()
            .args(args)
            .assert()
            .success()
            .stdout(expected);
    }
}

/// `-l` and `-s` after the name are the command's too.
#[test]
fn list_and_select_flags_after_the_name_are_the_commands() {
    Fixture::with_config(CLASHING_MD)
        .nixon()
        .args(["run", "clash", "-l", "-s"])
        .assert()
        .success()
        .stdout("args: -l -s\n");
}

/// nixon's own flags still work, before the command name.
#[test]
fn nixons_flags_still_work_before_the_command_name() {
    Fixture::new()
        .nixon()
        .args(["run", "-l", "hello"])
        .assert()
        .success()
        .stdout(contains("hello"));

    // One line, so the unique match settles it without a terminal.
    Fixture::with_config("# `one`\n\n```bash\nprintf 'only.txt\\n'\n```\n")
        .nixon()
        .args(["run", "-s", "one"])
        .assert()
        .success()
        .stdout("only.txt\n\n");
}

/// The flag boundary: nixon's flags go before the command name, and
/// everything after it belongs to the command. The same for both spellings
/// and for `project`.
#[test]
fn nixons_flags_stop_at_the_command_name() {
    let run = |args: &[&str], expected: &str| {
        Fixture::with_config(CLASHING_MD)
            .nixon()
            .args(args)
            .assert()
            .success()
            .stdout(expected.to_owned());
    };

    // After the command name: the command's.
    run(&["clash", "-i"], "args: -i\n");
    run(&["run", "clash", "-i"], "args: -i\n");
    run(&["project", ".", "clash", "-i"], "args: -i\n");

    // Before it: nixon's own. `-i` prints the source instead of running.
    Fixture::with_config(CLASHING_MD)
        .nixon()
        .args(["run", "-i", "clash"])
        .assert()
        .success()
        .stdout(contains("echo \"args: $*\""));

    Fixture::with_config(CLASHING_MD)
        .nixon()
        .args(["project", ".", "-i", "clash"])
        .assert()
        .success()
        .stdout(contains("echo \"args: $*\""));
}

/// `project`'s own flags still work directly after the project name, since
/// the command name has not arrived yet.
#[test]
fn a_project_flag_after_the_project_name_is_still_nixons() {
    Fixture::new()
        .nixon()
        .args(["project", ".", "-I"])
        .assert()
        .success()
        .stdout(contains("project"));
}

/// `--` says "no more flags"; it has done its job before nixon sees it, so
/// it does not travel on into the command's arguments.
#[test]
fn a_double_dash_does_not_reach_the_command() {
    for args in [
        ["clash", "--", "-i"].as_slice(),
        ["run", "clash", "--", "-i"].as_slice(),
        ["project", ".", "clash", "--", "-i"].as_slice(),
    ] {
        Fixture::with_config(CLASHING_MD)
            .nixon()
            .args(args)
            .assert()
            .success()
            .stdout("args: -i\n");
    }
}

/// The log records what ran, in the form that runs it again.
#[test]
fn running_a_command_is_recorded_in_the_log() {
    let fixture = Fixture::with_config("# `greet`\n\n```bash\necho hello\n```\n");
    fixture
        .nixon()
        .arg("greet")
        .assert()
        .success()
        .stdout("hello\n");

    let log = std::fs::read_to_string(fixture.temp.child("state/nixon/history").path()).unwrap();
    let fields: Vec<&str> = log.trim_end().split('\t').collect();
    assert_eq!(fields.len(), 3, "log line was {log:?}");
    assert!(fields[0].parse::<u64>().unwrap() > 1_700_000_000);
    assert_eq!(fields[2], "nixon run greet");
}

/// `history: false` writes nothing.
#[test]
fn the_log_can_be_turned_off() {
    let fixture = Fixture::with_config(
        "```json config\n{\"history\": false}\n```\n\n# `greet`\n\n```bash\necho hello\n```\n",
    );
    fixture.nixon().arg("greet").assert().success();

    assert!(!fixture.temp.child("state/nixon/history").path().exists());
}

/// Seeds the log with lines as nixon would have written them.
fn seed_history(fixture: &Fixture, lines: &[&str]) {
    use std::fmt::Write as _;

    let path = fixture.temp.child("state/nixon/history");
    let cwd = fixture.temp.child("project").path().display().to_string();
    let mut log = String::new();
    for (n, invocation) in lines.iter().enumerate() {
        let at = 1_700_000_000 + n as u64;
        let _ = writeln!(log, "{at}\t{cwd}\t{invocation}");
    }
    path.write_str(&log).unwrap();
}

/// `--list` prints the invocations, newest first.
#[test]
fn history_list_prints_the_invocations() {
    let fixture = Fixture::new();
    seed_history(&fixture, &["nixon run alpha", "nixon run beta"]);

    fixture
        .nixon()
        .args(["history", "-l"])
        .assert()
        .success()
        .stdout("nixon run beta\nnixon run alpha\n");
}

/// The query filters, as the other listings do.
#[test]
fn history_list_filters_on_its_query() {
    let fixture = Fixture::new();
    seed_history(&fixture, &["nixon run alpha", "nixon run beta"]);

    fixture
        .nixon()
        .args(["history", "-l", "alpha"])
        .assert()
        .success()
        .stdout("nixon run alpha\n");
}

/// `-n` keeps the newest.
#[test]
fn history_list_honours_the_limit() {
    let fixture = Fixture::new();
    seed_history(&fixture, &["nixon run alpha", "nixon run beta"]);

    fixture
        .nixon()
        .args(["history", "-l", "-n", "1"])
        .assert()
        .success()
        .stdout("nixon run beta\n");
}

/// `--clear` asks first, and `n` leaves the log alone.
#[test]
fn history_clear_asks_before_emptying() {
    let fixture = Fixture::new();
    seed_history(&fixture, &["nixon run alpha"]);
    let log = fixture.temp.child("state/nixon/history");

    fixture
        .nixon()
        .args(["history", "--clear"])
        .write_stdin("n\n")
        .assert()
        .success()
        .stderr(contains("Clear ").and(contains("? [y/N]")));
    assert!(!std::fs::read_to_string(log.path()).unwrap().is_empty());

    fixture
        .nixon()
        .args(["history", "--clear"])
        .write_stdin("y\n")
        .assert()
        .success();
    assert!(std::fs::read_to_string(log.path()).unwrap().is_empty());
}

/// With recording off there is nothing to show.
#[test]
fn history_needs_recording_to_be_on() {
    Fixture::with_config(
        "```json config\n{\"history\": false}\n```\n\n# `greet`\n\n```bash\necho hi\n```\n",
    )
    .nixon()
    .args(["history", "-l"])
    .assert()
    .failure()
    .code(1)
    .stderr(contains("history is disabled in the configuration"));
}

/// A query matching one line needs no terminal, as everywhere else.
#[test]
#[cfg(unix)]
fn history_select_with_a_unique_query_needs_no_terminal() {
    let fixture = Fixture::new();
    seed_history(&fixture, &["nixon run alpha", "nixon run beta"]);
    let nixon = assert_cmd::cargo::cargo_bin("nixon");

    let mut cmd = Command::new("setsid");
    cmd.arg("--wait")
        .arg(nixon)
        .args(["history", "-s", "alpha"]);
    fixture.apply(&mut cmd);

    cmd.write_stdin("")
        .assert()
        .success()
        .stdout("nixon run alpha\n");
}

/// Sourcing the fish widget with no log yet must be quiet and leave one
/// number behind.
///
/// It used to leave two — an `or`/`and` chain printed `0` twice when the
/// file was missing — which made the first prompt of every fish session
/// fail with `math: Missing operator` and swallowed the first command.
#[test]
fn the_fish_widget_handles_a_missing_log() {
    let fixture = Fixture::new();
    let widget = std::fs::canonicalize("../../extra/nixon-widget.fish").unwrap();

    let output = Command::new("fish")
        .arg("-c")
        .arg(format!(
            "source {}; echo seen=[$__nixon_history_seen]; __nixon_history__; echo ok",
            widget.display()
        ))
        .env("NIXON_HISTORY_FILE", fixture.temp.child("absent").path())
        .env("HOME", fixture.temp.path())
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stdout.contains("seen=[0]"), "stdout was {stdout:?}");
    assert!(stdout.contains("ok"), "the hook did not return: {stderr:?}");
    assert!(stderr.is_empty(), "stderr was {stderr:?}");
}
