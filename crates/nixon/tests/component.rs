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
        matches!(&err, NixonError::NoCandidates { name } if name == "git-files"),
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
