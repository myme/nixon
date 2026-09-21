//! End-to-end tests of the subcommand layer with fake picker and runner.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use assert_fs::TempDir;
use assert_fs::prelude::*;
use nixon::app::new::NewOpts;
use nixon::app::project::ProjectOpts;
use nixon::app::{App, Environment, RunOpts};
use nixon::config::Config;
use nixon::error::NixonError;
use nixon::fs::Dirs;
use nixon::process::{FakeRunner, RunKind};
use nixon_picker::picker::ScriptedPicker;
use nixon_picker::{Candidate, Selection, SelectionType};

/// A project on disk with a `nixon.md`, plus the dirs nixon reads.
struct Fixture {
    temp: TempDir,
    config: Config,
}

impl Fixture {
    fn new(nixon_md: &str) -> Self {
        let temp = TempDir::new().unwrap();
        let project = temp.child("project");
        project.create_dir_all().unwrap();
        // A marker nothing above the temp dir can have.
        project.child(Self::MARKER).touch().unwrap();
        project.child("nixon.md").write_str(nixon_md).unwrap();

        let config = Config {
            project_types: vec![nixon::project::ProjectType {
                id: "marked".to_owned(),
                markers: vec![nixon::project::ProjectMarker::Path(PathBuf::from(
                    Self::MARKER,
                ))],
                description: "Marked".to_owned(),
            }],
            ..Config::default()
        };
        Self { temp, config }
    }

    const MARKER: &'static str = ".nixon-fixture-marker";

    fn project_path(&self) -> PathBuf {
        self.temp.child("project").to_path_buf()
    }

    fn dirs(&self) -> Dirs {
        Dirs {
            home: self.temp.path().to_path_buf(),
            config: self.temp.child("config").to_path_buf(),
            cache: self.temp.child("cache").to_path_buf(),
        }
    }

    fn env(&self) -> Environment {
        Environment {
            cwd: self.project_path(),
            shell: Some("/bin/bash".to_owned()),
            direnv_dir: None,
            editor: Some("true".to_owned()),
            exe: Some(PathBuf::from("/usr/bin/nixon")),
        }
    }

    fn app(&self, picker: ScriptedPicker, runner: FakeRunner) -> App<ScriptedPicker, FakeRunner> {
        App::new(self.config.clone(), self.dirs(), self.env(), picker, runner)
    }
}

fn selected(values: &[&str]) -> Selection<Candidate> {
    Selection::selected(
        SelectionType::Default,
        values.iter().map(|v| Candidate::identity(*v)).collect(),
    )
}

/// The picker answers by candidate value, so the tests read like the UI.
fn picks(values: &[&[&str]]) -> ScriptedPicker {
    ScriptedPicker::new(values.iter().map(|v| selected(v)).collect())
}

const VIM_FILE_MD: &str = "\
# `git-files`

List the project's files.

```bash
git ls-files
```

# `vim-file ${git-files}`

Open a file.

```bash
vim \"$1\"
```
";

#[test]
fn a_placeholder_command_runs_then_its_selection_becomes_an_argument() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["vim-file"], &["README.md"]]);
    let runner = FakeRunner::new().with_output(&["Cargo.toml", "README.md"]);
    let mut app = fixture.app(picker, runner);

    let code = app.run(&RunOpts::default()).unwrap();
    assert_eq!(code, 0);

    // git-files ran first, streamed into the picker, in the project.
    assert_eq!(app.runner.calls.len(), 2);
    let (kind, first) = &app.runner.calls[0];
    assert_eq!(*kind, RunKind::Streamed);
    assert_eq!(first.cwd.as_deref(), Some(fixture.project_path().as_path()));
    assert_eq!(first.argv[0], "bash");
    assert_eq!(
        std::fs::read_to_string(&first.argv[1]).unwrap(),
        "git ls-files\n"
    );

    // vim-file then ran in the foreground with the chosen file appended.
    let (kind, second) = &app.runner.calls[1];
    assert_eq!(*kind, RunKind::Foreground);
    assert_eq!(second.argv[0], "bash");
    assert_eq!(second.argv[2], "README.md");
    assert_eq!(
        std::fs::read_to_string(&second.argv[1]).unwrap(),
        "vim \"$1\"\n"
    );
    assert_eq!(
        second.cwd.as_deref(),
        Some(fixture.project_path().as_path())
    );
    assert_eq!(
        second.env[0],
        (
            "nixon_project_path".to_owned(),
            fixture.project_path().to_string_lossy().into_owned()
        )
    );
}

#[test]
fn the_script_lands_in_the_cache_directory() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();

    let script = Path::new(&app.runner.calls[0].1.argv[1]);
    assert!(script.starts_with(fixture.dirs().cache_dir()));
    assert!(
        script
            .file_name()
            .unwrap()
            .to_string_lossy()
            .ends_with("-git-files.sh")
    );
}

#[test]
fn a_child_exit_code_is_propagated() {
    let fixture = Fixture::new("# `boom`\n\n```bash\nexit 3\n```\n");
    let picker = picks(&[&["boom"]]);
    let runner = FakeRunner::new().with_code(3);
    let mut app = fixture.app(picker, runner);

    assert_eq!(app.run(&RunOpts::default()).unwrap(), 3);
}

#[test]
fn cancelling_during_expansion_is_a_cancel_not_a_panic() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = ScriptedPicker::new(vec![selected(&["vim-file"]), Selection::Canceled]);
    let runner = FakeRunner::new().with_output(&["README.md"]);
    let mut app = fixture.app(picker, runner);

    let err = app.run(&RunOpts::default()).unwrap_err();
    assert!(matches!(err, NixonError::Canceled));
    assert_eq!(err.exit_code(), 130);
}

#[test]
fn cancelling_command_selection_is_a_cancel() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = ScriptedPicker::new(vec![Selection::Canceled]);
    let mut app = fixture.app(picker, FakeRunner::new());

    assert!(matches!(
        app.run(&RunOpts::default()).unwrap_err(),
        NixonError::Canceled
    ));
}

#[test]
fn selecting_nothing_reports_no_command_selected() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = ScriptedPicker::new(vec![Selection::Empty]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let err = app.run(&RunOpts::default()).unwrap_err();
    assert_eq!(err.to_string(), "No command selected.");
}

#[test]
fn a_background_command_is_detached() {
    let fixture = Fixture::new("# `serve &`\n\n```bash\npython -m http.server\n```\n");
    let picker = picks(&[&["serve"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(app.runner.calls[0].0, RunKind::Detached);
}

#[test]
fn hidden_commands_are_not_offered_but_are_still_reachable() {
    let md = "\
# `_secret`

```bash
echo secret
```

# `uses ${_secret}`

```bash
echo \"$1\"
```
";
    let fixture = Fixture::new(md);
    let picker = picks(&[&["uses"], &["hush"]]);
    let runner = FakeRunner::new().with_output(&["hush"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();

    // The command picker only saw the visible command...
    let offered: Vec<&str> = app.picker.calls[0]
        .1
        .iter()
        .map(|c| c.value.as_str())
        .collect();
    assert_eq!(offered, ["uses"]);

    // ...but the hidden one still ran as a placeholder source.
    assert_eq!(app.runner.calls.len(), 2);
    assert_eq!(app.runner.calls[1].1.argv[2], "hush");
}

#[test]
fn the_command_positional_pre_fills_the_picker_query() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    let opts = RunOpts {
        command: Some("git".to_owned()),
        ..RunOpts::default()
    };
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&opts).unwrap();
    assert_eq!(app.picker.calls[0].0.initial_query.as_deref(), Some("git"));
}

#[test]
fn the_command_picker_header_names_the_project() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(
        app.picker.calls[0].0.header.as_deref(),
        Some(
            format!(
                "Select command [project] ({})",
                fixture.temp.path().display()
            )
            .as_str()
        )
    );
}

#[test]
fn the_placeholder_picker_header_is_the_outer_command() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["vim-file"], &["README.md"]]);
    let runner = FakeRunner::new().with_output(&["README.md"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(
        app.picker.calls[1].0.header.as_deref(),
        Some("vim-file ${git-files}")
    );
}

#[test]
fn a_local_nixon_md_adds_its_commands() {
    let fixture = Fixture::new("# `local-only`\n\n```bash\necho local\n```\n");
    let picker = picks(&[&["local-only"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();
    let offered: Vec<&str> = app.picker.calls[0]
        .1
        .iter()
        .map(|c| c.value.as_str())
        .collect();
    assert_eq!(offered, ["local-only"]);
}

#[test]
fn a_project_typed_command_is_filtered_out_elsewhere() {
    let fixture = Fixture::new(
        "# `only-git` {type=\"git\"}\n\n```bash\necho git\n```\n\n# `always`\n\n```bash\necho always\n```\n",
    );
    let picker = picks(&[&["always"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();
    let offered: Vec<&str> = app.picker.calls[0]
        .1
        .iter()
        .map(|c| c.value.as_str())
        .collect();
    assert_eq!(offered, ["always"]);
}

#[test]
fn gc_removes_cached_scripts() {
    let fixture = Fixture::new("# `x`\n\n```bash\necho x\n```\n");
    let mut app = fixture.app(ScriptedPicker::new(Vec::new()), FakeRunner::new());

    let cache = fixture.dirs().cache_dir();
    std::fs::create_dir_all(&cache).unwrap();
    std::fs::write(cache.join("abc-x.sh"), "echo x\n").unwrap();

    assert_eq!(app.gc(true).unwrap(), 0);
    assert!(cache.join("abc-x.sh").exists());

    assert_eq!(app.gc(false).unwrap(), 0);
    assert!(!cache.join("abc-x.sh").exists());
}

#[test]
fn the_nix_wrapper_reaches_the_runner_when_enabled() {
    let fixture = Fixture::new("# `x`\n\n```bash\necho x\n```\n");
    fixture
        .temp
        .child("project/shell.nix")
        .write_str("{}\n")
        .unwrap();

    let mut config = fixture.config.clone();
    config.use_nix = Some(true);

    let mut app = App::new(
        config,
        fixture.dirs(),
        fixture.env(),
        picks(&[&["x"]]),
        FakeRunner::new(),
    );

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(app.runner.calls[0].1.argv[0], "nix-shell");
}

/// A line-oriented placeholder feeds the picker as its command runs, so the
/// picker opens without waiting for it.
#[test]
fn a_lines_placeholder_streams_its_candidates() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["vim-file"], &["README.md"]]);
    // The fake feeds these from a thread, one at a time.
    let runner = FakeRunner::new().with_output(&["Cargo.toml", "README.md"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(app.runner.calls[0].0, RunKind::Streamed);

    // Every line reached the picker, in the order the command wrote them.
    let offered: Vec<String> = app.picker.calls[1]
        .1
        .iter()
        .map(|candidate| candidate.value.clone())
        .collect();
    assert_eq!(offered, ["Cargo.toml", "README.md"]);
}

/// Columns need every row before the widths are known, so they stay buffered.
#[test]
fn a_columns_placeholder_stays_buffered() {
    let md = "\
# `rows`

```bash
printf 'NAME  ID\\nalpha 1\\n'
```

# `show`

```bash ${rows | cols+h 1}
echo \"$1\"
```
";
    let fixture = Fixture::new(md);
    let picker = picks(&[&["show"], &["alpha"]]);
    let runner = FakeRunner::new().with_output(&["NAME  ID", "alpha 1"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(app.runner.calls[0].0, RunKind::Captured);
}

/// JSON needs the whole document before a single candidate exists.
#[test]
fn a_json_placeholder_stays_buffered() {
    let md = "\
# `items`

```bash
echo '[\"one\"]'
```

# `show`

```bash ${items | json}
echo \"$1\"
```
";
    let fixture = Fixture::new(md);
    let picker = picks(&[&["show"], &["one"]]);
    let runner = FakeRunner::new().with_raw_output(b"[\"one\"]");
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(app.runner.calls[0].0, RunKind::Captured);
}

/// A `| list` placeholder prints matches, so there is nothing to stream into.
#[test]
fn a_list_placeholder_stays_buffered() {
    let md = "\
# `files`

```bash
printf 'a\\nb\\n'
```

# `show`

```bash ${files | list}
echo \"$@\"
```
";
    let fixture = Fixture::new(md);
    let picker = picks(&[&["show"]]);
    let runner = FakeRunner::new().with_output(&["a", "b"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(app.runner.calls[0].0, RunKind::Captured);
}

/// Cancelling is cancelling wherever it happens; outside `run` it used to
/// come back as a message and exit 1.
#[test]
fn cancelling_project_edit_or_new_exits_130() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let canceled = || ScriptedPicker::new(vec![Selection::Canceled]);

    let errors = [
        fixture
            .app(canceled(), FakeRunner::new())
            .project(&ProjectOpts::default())
            .unwrap_err(),
        fixture
            .app(canceled(), FakeRunner::new())
            .edit(None)
            .unwrap_err(),
        fixture
            .app(canceled(), FakeRunner::new())
            .new_command(&NewOpts::default())
            .unwrap_err(),
    ];

    for err in errors {
        assert!(matches!(err, NixonError::Canceled), "got {err:?}");
        assert_eq!(err.exit_code(), 130);
    }
}

/// An empty candidate command is a failure with a reason, not a cancel.
#[test]
fn a_placeholder_command_that_produces_nothing_is_reported() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["vim-file"]]);
    // `git-files` runs and prints nothing.
    let runner = FakeRunner::new().with_output(&[]);

    let err = fixture
        .app(picker, runner)
        .run(&RunOpts::default())
        .unwrap_err();

    assert!(
        matches!(&err, NixonError::NoCandidates { name, .. } if name == "git-files"),
        "got {err:?}"
    );
    assert_eq!(err.exit_code(), 1);
}

/// A picker answering with a value no command owns is a bug, not a command.
#[test]
fn a_selection_that_names_no_command_is_an_error() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["not-a-command"]]);

    let err = fixture
        .app(picker, FakeRunner::new())
        .run(&RunOpts::default())
        .unwrap_err();

    assert!(
        matches!(&err, NixonError::UnknownCommand { name } if name == "not-a-command"),
        "got {err:?}"
    );
}

const OPTIONS_MD: &str = "\
# `_worktrees`

```bash
git worktree list
```

# `remove --force ${_worktrees}`

Removes a worktree.

- `--force`: off — also removes worktrees with local changes

```bash
git worktree remove \"$@\"
```

# `build --release`

- `--release`: off

```bash
cargo build \"$@\"
```
";

/// The toggles ride along with the placeholder picker.
#[test]
fn a_placeholder_picker_shows_the_commands_options() {
    let fixture = Fixture::new(OPTIONS_MD);
    let picker = picks(&[&["remove"], &["../wt"]]);
    let runner = FakeRunner::new().with_output(&["../wt"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();

    // Call 0 is the command picker, call 1 the placeholder's.
    let shown = &app.picker.calls[1].0.options;
    assert_eq!(shown.len(), 1);
    assert_eq!(shown[0].label, "--force");
    assert!(!shown[0].on);
    assert_eq!(
        shown[0].description.as_deref(),
        Some("also removes worktrees with local changes")
    );
}

/// A toggle flipped at the placeholder picker reaches argv.
#[test]
fn toggling_at_the_picker_changes_what_runs() {
    let fixture = Fixture::new(OPTIONS_MD);
    // Only the placeholder pick is offered the toggles; it flips the first,
    // as `Alt-1` would.
    let picker =
        ScriptedPicker::new(vec![selected(&["remove"]), selected(&["../wt"])]).toggling(&[0]);
    let runner = FakeRunner::new().with_output(&["../wt"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();

    let invocation = app.runner.last().unwrap();
    assert_eq!(&invocation.argv[2..], ["--force", "../wt"]);
    assert!(
        invocation
            .env
            .contains(&("nixon_opt_force".to_owned(), "1".to_owned()))
    );
}

/// With no placeholder there is nothing to pick, so the options get a prompt
/// of their own.
#[test]
fn a_command_with_only_options_is_confirmed() {
    let fixture = Fixture::new(OPTIONS_MD);
    let picker = ScriptedPicker::new(vec![selected(&["build"])]).toggling(&[0]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts {
        command: Some("build".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert_eq!(&app.runner.last().unwrap().argv[2..], ["--release"]);
}

/// Cancelling the confirm prompt is a cancel, not a run with defaults.
#[test]
fn cancelling_the_confirm_prompt_exits_130() {
    let fixture = Fixture::new(OPTIONS_MD);
    let mut picker = ScriptedPicker::new(vec![selected(&["build"])]);
    picker.cancel_confirm = true;
    let mut app = fixture.app(picker, FakeRunner::new());

    let err = app
        .run(&RunOpts {
            command: Some("build".to_owned()),
            ..RunOpts::default()
        })
        .unwrap_err();
    assert!(matches!(err, NixonError::Canceled));
    assert_eq!(err.exit_code(), 130);
}

/// A command line that settles every option has nothing left to ask.
#[test]
fn a_complete_command_line_skips_the_confirm_prompt() {
    let fixture = Fixture::new(OPTIONS_MD);
    let picker = ScriptedPicker::new(vec![selected(&["build"])]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts {
        command: Some("build".to_owned()),
        args: vec!["--release".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();

    // The name is exact, so not even the command picker ran; what matters
    // is that no confirm call was recorded.
    assert!(app.picker.calls.is_empty());
    assert_eq!(&app.runner.last().unwrap().argv[2..], ["--release"]);
}

/// A placeholder's own command runs with its own defaults, not with the
/// toggles the prompt is showing for the command that referenced it.
#[test]
fn an_inner_command_is_not_given_the_outer_commands_toggles() {
    let fixture = Fixture::new(OPTIONS_MD);
    let picker =
        ScriptedPicker::new(vec![selected(&["remove"]), selected(&["../wt"])]).toggling(&[0]);
    let runner = FakeRunner::new().with_output(&["../wt"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();

    // `_worktrees` has no options of its own, so it gets no option vars.
    let inner = &app.runner.calls[0].1;
    assert!(
        !inner
            .env
            .iter()
            .any(|(name, _)| name.starts_with("nixon_opt_")),
        "the inner command saw {:?}",
        inner.env
    );
}

/// Every command can call nixon back, wherever nixon itself came from.
#[test]
fn the_nixon_binary_is_in_the_environment() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();

    let env = &app.runner.last().unwrap().env;
    assert!(
        env.contains(&("nixon_bin".to_owned(), "/usr/bin/nixon".to_owned())),
        "env was {env:?}"
    );
}

/// A path is already an answer: no discovery, no picker.
#[test]
fn a_project_given_as_a_path_is_resolved_directly() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.project(&ProjectOpts {
        project: Some(fixture.project_path().to_string_lossy().into_owned()),
        select: true,
        ..ProjectOpts::default()
    })
    .unwrap();

    // Only the command pick, if any: the project was never picked.
    assert!(
        app.picker.calls.is_empty(),
        "the picker was asked: {:?}",
        app.picker.calls
    );
}

/// The types come from the directory named, so its own commands apply.
#[test]
fn a_project_given_as_a_path_carries_its_types() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let project = app
        .pick_one_project(Some(&fixture.project_path().to_string_lossy()))
        .unwrap();

    assert_eq!(
        project.path(),
        fixture.project_path().canonicalize().unwrap()
    );
    assert_eq!(
        project
            .types
            .iter()
            .map(|t| t.id.as_str())
            .collect::<Vec<_>>(),
        ["marked"]
    );
}

/// `~` expands against the home nixon was told about.
#[test]
fn a_project_path_expands_a_leading_tilde() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let project = app.pick_one_project(Some("~/project")).unwrap();
    assert_eq!(
        project.path(),
        fixture.project_path().canonicalize().unwrap()
    );
}

/// A path that is not there is an error naming it, not a fuzzy query.
#[test]
fn a_project_path_that_does_not_exist_is_an_error() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let err = app.pick_one_project(Some("~/nowhere")).unwrap_err();
    assert!(
        matches!(&err, NixonError::NoSuchProject { path } if path.ends_with("/nowhere")),
        "got {err:?}"
    );
}

/// A name with no separator is still a query for the picker.
#[test]
fn a_bare_name_is_still_a_query() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["/somewhere/else"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.pick_one_project(Some("project")).unwrap();
    assert_eq!(
        app.picker.calls[0].0.initial_query.as_deref(),
        Some("project")
    );
}

/// A directory under `project_dirs` is the same project whichever way it is
/// reached.
#[test]
fn a_path_and_discovery_agree_on_the_same_directory() {
    let mut fixture = Fixture::new(VIM_FILE_MD);
    fixture.config.project_dirs = vec![fixture.temp.path().to_path_buf()];

    let picker = picks(&[]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let discovered = app
        .projects()
        .into_iter()
        .find(|project| project.name == Path::new("project"))
        .expect("discovery should find the fixture project");
    let by_path = app
        .pick_one_project(Some(&fixture.project_path().to_string_lossy()))
        .unwrap();

    assert_eq!(by_path.path(), discovered.path().canonicalize().unwrap());
    assert_eq!(
        by_path
            .types
            .iter()
            .map(|t| t.id.as_str())
            .collect::<Vec<_>>(),
        discovered
            .types
            .iter()
            .map(|t| t.id.as_str())
            .collect::<Vec<_>>()
    );
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

/// An exact name wins over the rows it also fuzzy-matches.
#[test]
fn an_exact_command_name_needs_no_picker() {
    let fixture = Fixture::new(AMBIGUOUS_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());

    app.run(&RunOpts {
        command: Some("link".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert!(app.picker.calls.is_empty(), "the picker was asked");
    assert!(app.runner.last().unwrap().argv[1].ends_with("-link.sh"));
}

/// A hidden command is not in the picker, so naming it has to be enough.
#[test]
fn a_hidden_command_runs_when_named_in_full() {
    let fixture = Fixture::new(AMBIGUOUS_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());

    app.run(&RunOpts {
        command: Some("_packages".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert!(app.runner.last().unwrap().argv[1].ends_with("-_packages.sh"));
}

/// A partial name still goes to the picker.
#[test]
fn a_partial_command_name_still_opens_the_picker() {
    let fixture = Fixture::new(AMBIGUOUS_MD);
    let mut app = fixture.app(picks(&[&["link-all"]]), FakeRunner::new());

    app.run(&RunOpts {
        command: Some("lnk".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert_eq!(app.picker.calls.len(), 1);
    assert!(app.runner.last().unwrap().argv[1].ends_with("-link-all.sh"));
}

/// `edit` offers hidden commands, and an exact name goes straight there.
#[test]
fn edit_takes_an_exact_hidden_name() {
    let fixture = Fixture::new(AMBIGUOUS_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());

    app.edit(Some("_packages")).unwrap();

    assert!(app.picker.calls.is_empty());
    let argv = &app.runner.last().unwrap().argv;
    assert!(argv.last().unwrap().ends_with("nixon.md"), "argv: {argv:?}");
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

# `open-many ${_worktrees:m}`

```bash
echo \"opened $@\"
```

# `_rows`

```bash
printf 'NAME\\nbugs\\nbugs2\\n'
```

# `columns ${_rows | cols+h 1}`

```bash
echo \"picked $1\"
```
";

/// An argument equal to a candidate's value is an answer, even though it
/// also fuzzy-matches `bugs2`.
#[test]
fn an_exact_placeholder_value_needs_no_picker() {
    let fixture = Fixture::new(OVERLAPPING_MD);
    let picker = picks(&[]);
    let runner = FakeRunner::new().with_output(&["bugs", "bugs2"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("open".to_owned()),
        args: vec!["bugs".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();

    assert!(app.picker.calls.is_empty(), "the picker was asked");
    assert_eq!(&app.runner.last().unwrap().argv[2..], ["bugs"]);
}

/// The same for a buffered format, where the whole list exists first.
#[test]
fn an_exact_value_works_for_a_buffered_format() {
    let fixture = Fixture::new(OVERLAPPING_MD);
    let picker = picks(&[]);
    let runner = FakeRunner::new().with_output(&["NAME", "bugs", "bugs2"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("columns".to_owned()),
        args: vec!["bugs".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();

    assert!(app.picker.calls.is_empty(), "the picker was asked");
    assert_eq!(&app.runner.last().unwrap().argv[2..], ["bugs"]);
}

/// An argument that is only a fuzzy match still opens the picker.
#[test]
fn a_partial_placeholder_value_still_opens_the_picker() {
    let fixture = Fixture::new(OVERLAPPING_MD);
    let picker = picks(&[&["bugs2"]]);
    let runner = FakeRunner::new().with_output(&["bugs", "bugs2"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("open".to_owned()),
        args: vec!["bgs".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();

    assert_eq!(app.picker.calls.len(), 1);
    assert_eq!(&app.runner.last().unwrap().argv[2..], ["bugs2"]);
}

/// A multi placeholder given an exact value takes that one item, not the
/// list it would otherwise offer.
#[test]
fn an_exact_value_selects_one_item_of_a_multi_placeholder() {
    let fixture = Fixture::new(OVERLAPPING_MD);
    let picker = picks(&[]);
    let runner = FakeRunner::new().with_output(&["bugs", "bugs2"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("open-many".to_owned()),
        args: vec!["bugs".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();

    assert!(app.picker.calls.is_empty(), "the picker was asked");
    assert_eq!(&app.runner.last().unwrap().argv[2..], ["bugs"]);
}
