//! End-to-end tests of the subcommand layer with fake picker and runner.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use assert_fs::TempDir;
use assert_fs::prelude::*;
use nixon::app::eval::EvalOpts;
use nixon::app::history::{HistoryOpts, HistoryReadMode};
use nixon::app::new::NewOpts;
use nixon::app::project::{ProjectDecision, ProjectOpts};
use nixon::app::run::RunDecision;
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

    /// A second project beside the first.
    ///
    /// Anything that must reach the project picker needs one: `-1` takes a
    /// lone candidate without asking.
    fn sibling_project(&mut self, name: &str) {
        let dir = self.temp.child(name);
        dir.create_dir_all().unwrap();
        dir.child(Self::MARKER).touch().unwrap();
        self.config.project_dirs = vec![self.temp.child("*").to_path_buf()];
    }

    fn dirs(&self) -> Dirs {
        Dirs {
            home: self.temp.path().to_path_buf(),
            config: self.temp.child("config").to_path_buf(),
            cache: self.temp.child("cache").to_path_buf(),
            state: self.temp.child("state").to_path_buf(),
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
    let runner = FakeRunner::new().with_output(&["README.md", "Cargo.toml"]);
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
    let mut app = fixture.app(
        ScriptedPicker::new(vec![Selection::Empty]),
        FakeRunner::new(),
    );
    let project = app.current_project();
    assert!(matches!(
        app.prepare_run_command(&project, &RunOpts::default())
            .unwrap(),
        RunDecision::Empty
    ));

    let picker = ScriptedPicker::new(vec![Selection::Empty]);
    let mut app = fixture.app(picker, FakeRunner::new());
    let err = app.run(&RunOpts::default()).unwrap_err();
    assert_eq!(err.to_string(), "No command selected.");
}

#[test]
fn invalid_command_selection_is_typed_and_keeps_cli_error() {
    let fixture = Fixture::new(VIM_FILE_MD);
    for (selection, count, message) in [
        (
            Selection::Selected {
                kind: SelectionType::Default,
                items: Vec::new(),
            },
            0,
            "No command selected.",
        ),
        (
            selected(&["git-files", "vim-file"]),
            2,
            "Multiple commands selected.",
        ),
    ] {
        let mut app = fixture.app(
            ScriptedPicker::new(vec![selection.clone()]),
            FakeRunner::new(),
        );
        let project = app.current_project();
        let decision = app
            .prepare_run_command(&project, &RunOpts::default())
            .unwrap();
        assert!(matches!(decision, RunDecision::InvalidSelection(n) if n == count));
        assert_eq!(decision.selection_error().unwrap().to_string(), message);

        let mut app = fixture.app(ScriptedPicker::new(vec![selection]), FakeRunner::new());
        assert_eq!(
            app.run(&RunOpts::default()).unwrap_err().to_string(),
            message
        );
    }
}

#[test]
fn run_decision_lists_before_picking_or_insert_and_includes_hidden_names() {
    let fixture = Fixture::new(
        "# `_hidden`\n\n```bash\necho hidden\n```\n\n# `alpha`\n\n```bash\necho alpha\n```\n",
    );
    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let project = app.current_project();

    let decision = app
        .prepare_run_command(
            &project,
            &RunOpts {
                list: true,
                insert: true,
                select: true,
                ..RunOpts::default()
            },
        )
        .unwrap();

    assert!(matches!(decision, RunDecision::List(lines) if lines == ["_hidden", "alpha"]));
    assert!(app.picker.calls.is_empty());
    assert!(app.runner.calls.is_empty());

    let decision = app
        .prepare_run_command(
            &project,
            &RunOpts {
                command: Some("missing".to_owned()),
                list: true,
                ..RunOpts::default()
            },
        )
        .unwrap();
    assert!(matches!(decision, RunDecision::List(lines) if lines.is_empty()));

    let decision = app
        .prepare_run_command(
            &project,
            &RunOpts {
                command: Some("_hidden".to_owned()),
                ..RunOpts::default()
            },
        )
        .unwrap();
    assert!(matches!(decision, RunDecision::Run(command, _) if command.name == "_hidden"));
    assert!(app.picker.calls.is_empty());
}

#[test]
fn run_decision_insert_precedes_select_and_picker_show() {
    let fixture = Fixture::new(
        "# `alpha`\n\n```bash\necho alpha\n```\n\n# `beta`\n\n```bash\necho beta\n```\n",
    );
    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Show,
        vec![Candidate::identity("alpha")],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());
    let project = app.current_project();

    let decision = app
        .prepare_run_command(
            &project,
            &RunOpts {
                insert: true,
                select: true,
                ..RunOpts::default()
            },
        )
        .unwrap();

    assert!(matches!(decision, RunDecision::InsertSource(command) if command.name == "alpha"));
    assert_eq!(app.picker.calls.len(), 1);
    assert!(app.runner.calls.is_empty());
}

#[test]
fn run_decision_select_precedes_picker_show_and_returns_values() {
    let fixture = Fixture::new(
        "# `alpha`\n\n```bash\necho alpha\n```\n\n# `beta`\n\n```bash\necho beta\n```\n",
    );
    let picker = ScriptedPicker::new(vec![
        Selection::selected(SelectionType::Show, vec![Candidate::identity("alpha")]),
        selected(&["first"]),
    ]);
    let mut app = fixture.app(picker, FakeRunner::new().with_output(&["first", "second"]));
    let project = app.current_project();

    let decision = app
        .prepare_run_command(
            &project,
            &RunOpts {
                select: true,
                ..RunOpts::default()
            },
        )
        .unwrap();

    assert!(
        matches!(decision, RunDecision::SelectedValues(command, values) if command.name == "alpha" && values == ["first"])
    );
    assert_eq!(app.picker.calls.len(), 2);
    assert!(fixture.history().is_empty());
}

#[test]
fn run_decision_preserves_picker_actions_arguments_and_cancel() {
    let fixture = Fixture::new(
        "# `alpha`\n\n```bash\necho alpha\n```\n\n# `beta`\n\n```bash\necho beta\n```\n",
    );
    for kind in [
        SelectionType::Default,
        SelectionType::Show,
        SelectionType::Edit,
        SelectionType::Visit,
    ] {
        let picker = ScriptedPicker::new(vec![Selection::selected(
            kind,
            vec![Candidate::identity("alpha")],
        )]);
        let mut app = fixture.app(picker, FakeRunner::new());
        let project = app.current_project();
        let decision = app
            .prepare_run_command(
                &project,
                &RunOpts {
                    args: vec!["two words".to_owned()],
                    ..RunOpts::default()
                },
            )
            .unwrap();
        match (kind, decision) {
            (SelectionType::Default, RunDecision::Run(command, args))
            | (SelectionType::Edit, RunDecision::Edit(command, args)) => {
                assert_eq!(command.name, "alpha");
                assert_eq!(args, ["two words"]);
            }
            (SelectionType::Show, RunDecision::ShowSource(command))
            | (SelectionType::Visit, RunDecision::Visit(command)) => {
                assert_eq!(command.name, "alpha");
            }
            (_, decision) => panic!("unexpected decision: {decision:?}"),
        }
        assert!(app.runner.calls.is_empty());
    }

    let mut app = fixture.app(
        ScriptedPicker::new(vec![Selection::Canceled]),
        FakeRunner::new(),
    );
    let project = app.current_project();
    assert!(matches!(
        app.prepare_run_command(&project, &RunOpts::default()),
        Err(NixonError::Canceled)
    ));
}

#[test]
fn choosing_commands_with_only_hidden_entries_returns_empty_without_a_picker() {
    let fixture = Fixture::new("# `_secret`\n\n```bash\necho secret\n```\n");
    let mut app = fixture.app(picks(&[]), FakeRunner::new());

    let selection = app.choose_command(&app.current_project(), None).unwrap();

    assert!(matches!(selection, Selection::Empty));
    assert!(app.picker.calls.is_empty());
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

# `plain`

```bash
echo plain
```
";
    let fixture = Fixture::new(md);
    let picker = picks(&[&["uses"], &["hush"]]);
    let runner = FakeRunner::new().with_output(&["hush", "shush"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();

    // The command picker only saw the visible command...
    let offered: Vec<&str> = app.picker.calls[0]
        .1
        .iter()
        .map(|c| c.value.as_str())
        .collect();
    assert_eq!(offered, ["plain", "uses"]);

    // ...but the hidden one still ran as a placeholder source.
    assert_eq!(app.runner.calls.len(), 2);
    assert_eq!(app.runner.calls[1].1.argv[2], "hush");
}

#[test]
fn the_command_positional_pre_fills_the_picker_query() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[&["git-files"]]);
    // A query both commands match, or `-1` would answer it without asking.
    let opts = RunOpts {
        command: Some("file".to_owned()),
        ..RunOpts::default()
    };
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&opts).unwrap();
    assert_eq!(app.picker.calls[0].0.initial_query.as_deref(), Some("file"));
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
    let runner = FakeRunner::new().with_output(&["README.md", "Cargo.toml"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts::default()).unwrap();
    assert_eq!(
        app.picker.calls[1].0.header.as_deref(),
        Some("vim-file ${git-files}")
    );
}

#[test]
fn a_local_nixon_md_adds_its_commands() {
    let fixture = Fixture::new(
        "# `local-only`\n\n```bash\necho local\n```\n\n# `local-too`\n\n```bash\necho too\n```\n",
    );
    let picker = picks(&[&["local-only"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();
    let offered: Vec<&str> = app.picker.calls[0]
        .1
        .iter()
        .map(|c| c.value.as_str())
        .collect();
    assert_eq!(offered, ["local-only", "local-too"]);
}

#[test]
fn a_project_typed_command_is_filtered_out_elsewhere() {
    let fixture = Fixture::new(
        "# `only-git` {type=\"git\"}\n\n```bash\necho git\n```\n\n# `always`\n\n```bash\necho always\n```\n\n# `also`\n\n```bash\necho also\n```\n",
    );
    let picker = picks(&[&["always"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    app.run(&RunOpts::default()).unwrap();
    let offered: Vec<&str> = app.picker.calls[0]
        .1
        .iter()
        .map(|c| c.value.as_str())
        .collect();
    assert_eq!(offered, ["also", "always"]);
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
    let mut fixture = Fixture::new(VIM_FILE_MD);
    fixture.sibling_project("other");
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
    let runner = FakeRunner::new().with_output(&["../wt", "../spare"]);
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
    let runner = FakeRunner::new().with_output(&["../wt", "../spare"]);
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
    let runner = FakeRunner::new().with_output(&["../wt", "../spare"]);
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

#[test]
fn explicit_project_pick_shows_one_candidate_and_can_cancel() {
    let mut fixture = Fixture::new(VIM_FILE_MD);
    fixture.config.project_dirs.push(fixture.project_path());
    let picker = ScriptedPicker::new(vec![Selection::Canceled]);
    let mut app = fixture.app(picker, FakeRunner::new());

    assert!(matches!(
        app.pick_project_explicit().unwrap(),
        Selection::Canceled
    ));
    assert_eq!(app.picker.calls.len(), 1);
    let (options, candidates) = &app.picker.calls[0];
    assert!(!options.select_one);
    assert_eq!(candidates.len(), 1);
    assert_eq!(
        candidates[0].value,
        fixture.project_path().to_string_lossy()
    );

    let mut cli_app = fixture.app(picks(&[]), FakeRunner::new());
    assert!(cli_app.pick_projects(None, false).is_ok());
    assert!(cli_app.picker.calls.is_empty());
}

#[test]
fn explicit_project_show_maps_the_candidate_to_its_detected_project() {
    let mut fixture = Fixture::new(VIM_FILE_MD);
    let path = fixture.project_path();
    fixture.config.project_dirs.push(path.clone());
    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Show,
        vec![Candidate::identity(path.to_string_lossy().into_owned())],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let Selection::Selected { kind, items } = app.pick_project_explicit().unwrap() else {
        panic!("expected selected project");
    };
    assert_eq!(kind, SelectionType::Show);
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].path(), path);
    assert_eq!(items[0].types[0].id, "marked");
}

#[test]
fn project_decision_list_precedes_select_inspect_and_picker() {
    let mut fixture = Fixture::new(VIM_FILE_MD);
    fixture.sibling_project("project-two");
    let mut app = fixture.app(
        ScriptedPicker::new(vec![Selection::Canceled]),
        FakeRunner::new(),
    );
    let decision = app
        .prepare_project(&ProjectOpts {
            project: Some("project".to_owned()),
            list: true,
            select: true,
            inspect: true,
            ..ProjectOpts::default()
        })
        .unwrap();
    let ProjectDecision::List(lines) = decision else {
        panic!("project --list should win");
    };
    assert_eq!(lines.len(), 2);
    assert!(app.picker.calls.is_empty());
    assert!(app.runner.calls.is_empty());
}

#[test]
fn project_decision_preserves_multi_select_and_show_without_running() {
    let mut fixture = Fixture::new(VIM_FILE_MD);
    fixture.sibling_project("project-two");
    let first = fixture.project_path();
    let second = fixture.temp.child("project-two").to_path_buf();
    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Default,
        vec![
            Candidate::identity(first.to_string_lossy().into_owned()),
            Candidate::identity(second.to_string_lossy().into_owned()),
        ],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());
    let decision = app
        .prepare_project(&ProjectOpts {
            select: true,
            inspect: true,
            ..ProjectOpts::default()
        })
        .unwrap();
    let ProjectDecision::SelectedPaths(projects) = decision else {
        panic!("--select should win over --inspect");
    };
    assert_eq!(
        projects
            .iter()
            .map(nixon::project::Project::path)
            .collect::<Vec<_>>(),
        [first.as_path(), second.as_path()]
    );
    assert!(app.picker.calls[0].0.multi);
    assert!(app.runner.calls.is_empty());

    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Show,
        vec![Candidate::identity(first.to_string_lossy().into_owned())],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());
    assert!(matches!(
        app.prepare_project(&ProjectOpts::default()),
        Ok(ProjectDecision::Inspect(projects)) if projects.len() == 1 && projects[0].path() == first
    ));
    assert!(app.runner.calls.is_empty());
}

#[test]
fn project_decision_prepares_local_command_and_retains_arguments() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let decision = app
        .prepare_project(&ProjectOpts {
            project: Some(fixture.project_path().to_string_lossy().into_owned()),
            run: RunOpts {
                command: Some("git-files".to_owned()),
                args: vec!["two words".to_owned()],
                ..RunOpts::default()
            },
            ..ProjectOpts::default()
        })
        .unwrap();
    let ProjectDecision::Command { project, decision } = decision else {
        panic!("expected project command");
    };
    assert_eq!(project.path(), fixture.project_path());
    assert!(matches!(
        *decision,
        RunDecision::Run(command, args)
            if command.name == "git-files"
                && command.source.contains("git ls-files")
                && args == ["two words"]
    ));
    assert!(app.picker.calls.is_empty());
    assert!(app.runner.calls.is_empty());
}

#[test]
fn project_decision_preserves_picker_cancellation() {
    let mut fixture = Fixture::new(VIM_FILE_MD);
    fixture.sibling_project("project-two");
    let mut app = fixture.app(
        ScriptedPicker::new(vec![Selection::Canceled]),
        FakeRunner::new(),
    );
    assert!(matches!(
        app.prepare_project(&ProjectOpts::default()),
        Err(NixonError::Canceled)
    ));
    assert!(app.runner.calls.is_empty());
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
    let mut fixture = Fixture::new(VIM_FILE_MD);
    // Both names match the query, or `-1` would answer it without asking.
    fixture.sibling_project("project-two");
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

/// A relative path is relative to where nixon was invoked, not to the
/// process working directory.
#[test]
fn a_relative_project_path_resolves_against_the_invocation_directory() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let here = app.pick_one_project(Some("./")).unwrap();
    assert_eq!(here.path(), fixture.project_path().canonicalize().unwrap());
}

/// `..` walks up from the invocation directory too.
#[test]
fn a_parent_project_path_resolves_against_the_invocation_directory() {
    let fixture = Fixture::new(VIM_FILE_MD);
    let picker = picks(&[]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let up = app.pick_one_project(Some("../")).unwrap();
    assert_eq!(up.path(), fixture.temp.path().canonicalize().unwrap());
}

const HISTORY_MD: &str = "\
# `_files`

```bash
printf 'a.txt\\nb c.txt\\n'
```

# `edit ${_files}`

```bash
vim \"$1\"
```

# `edit-many ${_files:m}`

```bash
vim -p \"$@\"
```

# `build --release`

- `--release`: on

```bash
cargo build \"$@\"
```
";

impl Fixture {
    /// The lines recorded in the history log.
    fn history(&self) -> Vec<String> {
        std::fs::read_to_string(self.temp.child("state/nixon/history").path())
            .unwrap_or_default()
            .lines()
            .map(|line| line.split('\t').nth(2).unwrap_or_default().to_owned())
            .collect()
    }
}

/// A run is recorded in the form that repeats it.
#[test]
fn running_a_command_records_a_replayable_line() {
    let fixture = Fixture::new(HISTORY_MD);
    let picker = picks(&[&["a.txt"]]);
    let runner = FakeRunner::new().with_output(&["a.txt", "b c.txt"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("edit".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert_eq!(fixture.history(), ["nixon run edit a.txt"]);
}

/// A value with a space in it is quoted, so the line can be run again.
#[test]
fn a_recorded_value_is_shell_quoted() {
    let fixture = Fixture::new(HISTORY_MD);
    let picker = picks(&[&["b c.txt"]]);
    let runner = FakeRunner::new().with_output(&["a.txt", "b c.txt"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("edit".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert_eq!(fixture.history(), ["nixon run edit 'b c.txt'"]);
}

/// Several values are consecutive arguments.
#[test]
fn a_multi_selection_records_every_value() {
    let fixture = Fixture::new(HISTORY_MD);
    let picker = picks(&[&["a.txt", "b c.txt"]]);
    let runner = FakeRunner::new().with_output(&["a.txt", "b c.txt"]);
    let mut app = fixture.app(picker, runner);

    app.run(&RunOpts {
        command: Some("edit-many".to_owned()),
        ..RunOpts::default()
    })
    .unwrap();

    assert_eq!(fixture.history(), ["nixon run edit-many a.txt 'b c.txt'"]);
}

/// Only options that differ from their default are recorded, and an option
/// turned off against an `on` default is recorded as `--no-`.
#[test]
fn only_overridden_options_are_recorded() {
    let fixture = Fixture::new(HISTORY_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    app.run(&RunOpts {
        command: Some("build".to_owned()),
        args: vec!["--no-release".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();
    assert_eq!(fixture.history(), ["nixon run build --no-release"]);

    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    app.run(&RunOpts {
        command: Some("build".to_owned()),
        args: vec!["--release".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();
    assert_eq!(fixture.history()[1], "nixon run build");
}

/// `eval` has no name to look up, so the source is what repeats it.
#[test]
fn eval_records_its_expression() {
    let fixture = Fixture::new(HISTORY_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());

    app.eval(&nixon::app::eval::EvalOpts {
        source: Some("echo hello".to_owned()),
        ..nixon::app::eval::EvalOpts::default()
    })
    .unwrap();

    // The language too, or a replay runs it as bash whatever it was.
    assert_eq!(fixture.history(), ["nixon eval -l bash 'echo hello'"]);
}

#[test]
fn prepare_eval_uses_current_project_and_explicit_path_without_a_picker() {
    let mut fixture = Fixture::new(HISTORY_MD);
    fixture.sibling_project("elsewhere");
    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let current = app.prepare_eval(&EvalOpts::default()).unwrap();
    assert_eq!(current.kind, SelectionType::Default);
    assert_eq!(current.project.path(), fixture.project_path());
    let explicit = app
        .prepare_eval(&EvalOpts {
            project: Some(
                fixture
                    .temp
                    .child("elsewhere")
                    .path()
                    .to_string_lossy()
                    .into_owned(),
            ),
            select_project: true,
            ..EvalOpts::default()
        })
        .unwrap();
    assert_eq!(explicit.kind, SelectionType::Default);
    assert_eq!(
        explicit.project.path(),
        fixture.temp.child("elsewhere").path()
    );
    assert!(app.picker.calls.is_empty());
    assert!(app.runner.calls.is_empty());
}

#[test]
fn prepare_eval_preserves_prompt_show_and_cancel() {
    let mut fixture = Fixture::new(HISTORY_MD);
    fixture.sibling_project("elsewhere");
    let chosen = fixture.temp.child("elsewhere").to_path_buf();
    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Show,
        vec![Candidate::identity(chosen.to_string_lossy().into_owned())],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());
    let choice = app
        .prepare_eval(&EvalOpts {
            select_project: true,
            ..EvalOpts::default()
        })
        .unwrap();
    assert_eq!(choice.kind, SelectionType::Show);
    assert_eq!(choice.project.path(), chosen);
    assert_eq!(app.picker.calls.len(), 1);
    assert!(app.runner.calls.is_empty());

    let mut app = fixture.app(
        ScriptedPicker::new(vec![Selection::Canceled]),
        FakeRunner::new(),
    );
    assert!(matches!(
        app.prepare_eval(&EvalOpts {
            select_project: true,
            ..EvalOpts::default()
        }),
        Err(NixonError::Canceled)
    ));
    assert!(app.runner.calls.is_empty());
}

#[test]
fn cli_eval_still_runs_after_project_show_and_records_it() {
    let mut fixture = Fixture::new(HISTORY_MD);
    fixture.sibling_project("elsewhere");
    let chosen = fixture.temp.child("elsewhere").to_path_buf();
    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Show,
        vec![Candidate::identity(chosen.to_string_lossy().into_owned())],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());
    app.eval(&EvalOpts {
        source: Some("echo chosen".to_owned()),
        select_project: true,
        ..EvalOpts::default()
    })
    .unwrap();
    assert_eq!(app.runner.calls.len(), 1);
    assert_eq!(app.runner.calls[0].1.cwd.as_deref(), Some(chosen.as_path()));
    assert!(fixture.history()[0].contains("echo chosen"));
}

/// Nothing that did not run is recorded.
#[test]
fn listing_and_selecting_record_nothing() {
    let fixture = Fixture::new(HISTORY_MD);
    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let project = app.current_project();

    app.list_commands(&project, None).unwrap();
    assert!(fixture.history().is_empty());

    let mut app = fixture.app(picks(&[&["build"]]), FakeRunner::new());
    app.run(&RunOpts {
        insert: true,
        ..RunOpts::default()
    })
    .unwrap();
    assert!(fixture.history().is_empty());
}

#[test]
fn matching_command_names_include_hidden_commands_and_local_matching() {
    let fixture = Fixture::new(
        "```yaml config\nexact_match: true\n```\n\n# `_hidden`\n\n```bash\necho hidden\n```\n\n# `deploy-staging`\n\n```bash\necho staging\n```\n",
    );
    let app = fixture.app(picks(&[]), FakeRunner::new());
    let project = app.current_project();

    assert_eq!(
        app.matching_command_names(&project, None).unwrap(),
        ["_hidden", "deploy-staging"]
    );
    assert!(
        app.matching_command_names(&project, Some("dpst"))
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        app.matching_command_names(&project, Some("_hidden"))
            .unwrap(),
        ["_hidden"]
    );
}

#[test]
fn matching_project_paths_keep_plain_display_order_and_query() {
    let mut fixture = Fixture::new("");
    fixture.sibling_project("work-two");
    fixture.sibling_project("work-one");
    let app = fixture.app(picks(&[]), FakeRunner::new());

    assert_eq!(
        app.matching_project_paths(None).unwrap(),
        ["~/project", "~/work-one", "~/work-two"]
    );
    assert_eq!(
        app.matching_project_paths(Some("work")).unwrap(),
        ["~/work-one", "~/work-two"]
    );
    assert!(
        app.matching_project_paths(Some("missing"))
            .unwrap()
            .is_empty()
    );
}

/// `history: false` writes nothing at all.
#[test]
fn the_history_can_be_turned_off() {
    let mut fixture = Fixture::new(HISTORY_MD);
    fixture.config.history = Some(false);

    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    app.run(&RunOpts {
        command: Some("build".to_owned()),
        args: vec!["--no-release".to_owned()],
        ..RunOpts::default()
    })
    .unwrap();

    assert!(fixture.history().is_empty());
}

/// Seeds the log with lines as nixon would have written them.
fn seed_history(fixture: &Fixture, lines: &[(u64, &str)]) {
    use std::fmt::Write as _;

    let cwd = fixture.project_path().display().to_string();
    let mut log = String::new();
    for (at, invocation) in lines {
        let _ = writeln!(log, "{at}\t{cwd}\t{invocation}");
    }
    fixture
        .temp
        .child("state/nixon/history")
        .write_str(&log)
        .unwrap();
}

#[test]
fn shared_history_candidates_report_or_ignore_read_errors() {
    let fixture = Fixture::new(HISTORY_MD);
    fixture
        .temp
        .child("state/nixon/history")
        .create_dir_all()
        .unwrap();
    let mut app = fixture.app(picks(&[]), FakeRunner::new());

    assert!(matches!(
        app.history_candidates(Some(10), HistoryReadMode::ReportErrors),
        Err(NixonError::Io(_))
    ));
    let (_, candidates) = app
        .history_candidates(Some(10), HistoryReadMode::IgnoreErrors)
        .unwrap();
    assert!(candidates.is_empty());
    assert!(matches!(
        app.history(&HistoryOpts::default()),
        Err(NixonError::NothingSelected(_))
    ));
    assert!(app.picker.calls[0].1.is_empty());
}

#[test]
fn shared_history_picker_preserves_query_header_and_cancellation() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build"), (2, "nixon run other")]);
    let picker = ScriptedPicker::new(vec![Selection::Canceled]);
    let mut app = fixture.app(picker, FakeRunner::new());
    let (config, candidates) = app
        .history_candidates(Some(10), HistoryReadMode::ReportErrors)
        .unwrap();

    assert!(matches!(
        app.pick_history_candidates(&config, candidates, Some("run"), Some("Recent runs")),
        Ok(Selection::Canceled)
    ));
    let (options, offered) = &app.picker.calls[0];
    assert_eq!(options.header.as_deref(), Some("Recent runs"));
    assert_eq!(options.initial_query.as_deref(), Some("run"));
    assert_eq!(offered[0].value, "nixon run other");
    assert_eq!(offered[1].value, "nixon run build");
}

/// Newest first, and a run of the same command shown once.
#[test]
fn the_history_picker_shows_newest_first_without_repeats() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(
        &fixture,
        &[
            (1, "nixon run build"),
            (2, "nixon run edit a.txt"),
            (3, "nixon run edit a.txt"),
        ],
    );

    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let _ = app.history(&HistoryOpts::default());

    let offered: Vec<String> = app.picker.calls[0]
        .1
        .iter()
        .map(|candidate| candidate.value.clone())
        .collect();
    assert_eq!(offered, ["nixon run edit a.txt", "nixon run build"]);
}

/// `-n` keeps the newest.
#[test]
fn the_history_limit_keeps_the_newest() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(
        &fixture,
        &[
            (1, "nixon run one"),
            (2, "nixon run two"),
            (3, "nixon run three"),
        ],
    );

    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let _ = app.history(&HistoryOpts {
        limit: Some(2),
        ..HistoryOpts::default()
    });

    assert_eq!(app.picker.calls[0].1.len(), 2);
}

/// Choosing a line hands it back for the caller to run.
#[test]
fn choosing_a_history_line_asks_for_it_to_be_rerun() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build --no-release")]);

    let picker = picks(&[&["nixon run build --no-release"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    match app.history(&HistoryOpts::default()).unwrap() {
        nixon::app::history::Outcome::Rerun(argv) => {
            assert_eq!(argv, ["run", "build", "--no-release"]);
        }
        nixon::app::history::Outcome::Done(_) => panic!("expected a rerun"),
    }
}

/// `Alt-Enter` hands the line back instead of running it.
#[test]
fn alt_enter_on_a_history_line_prints_it() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build"), (2, "nixon run other")]);

    let picker = ScriptedPicker::new(vec![Selection::selected(
        SelectionType::Edit,
        vec![Candidate::identity("nixon run build")],
    )]);
    let mut app = fixture.app(picker, FakeRunner::new());

    match app.history(&HistoryOpts::default()).unwrap() {
        nixon::app::history::Outcome::Done(code) => assert_eq!(code, 0),
        nixon::app::history::Outcome::Rerun(argv) => panic!("it ran {argv:?}"),
    }
}

/// `--select` prints rather than running.
#[test]
fn selecting_a_history_line_runs_nothing() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build")]);

    let picker = picks(&[&["nixon run build"]]);
    let mut app = fixture.app(picker, FakeRunner::new());

    match app
        .history(&HistoryOpts {
            select: true,
            ..HistoryOpts::default()
        })
        .unwrap()
    {
        nixon::app::history::Outcome::Done(code) => assert_eq!(code, 0),
        nixon::app::history::Outcome::Rerun(_) => panic!("it should not have run"),
    }
    assert!(app.runner.calls.is_empty());
}

/// With recording off there is nothing to show.
#[test]
fn the_history_command_needs_recording_to_be_on() {
    let mut fixture = Fixture::new(HISTORY_MD);
    fixture.config.history = Some(false);

    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let err = app.history(&HistoryOpts::default()).unwrap_err();

    assert!(matches!(err, NixonError::HistoryDisabled), "got {err:?}");
    assert_eq!(err.exit_code(), 1);
}

/// Cancelling the picker is a cancel.
#[test]
fn cancelling_the_history_picker_exits_130() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build")]);

    let picker = ScriptedPicker::new(vec![Selection::Canceled]);
    let mut app = fixture.app(picker, FakeRunner::new());

    let err = app.history(&HistoryOpts::default()).unwrap_err();
    assert!(matches!(err, NixonError::Canceled));
    assert_eq!(err.exit_code(), 130);
}

/// Looking at the history is not asking to run something.
#[test]
fn the_history_picker_draws_even_for_a_single_entry() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build")]);

    let picker = picks(&[&["nixon run build"]]);
    let mut app = fixture.app(picker, FakeRunner::new());
    let _ = app.history(&HistoryOpts::default());

    assert_eq!(app.picker.calls.len(), 1, "the picker was not asked");
}

/// Naming it is asking, so no picker is needed.
#[test]
fn a_history_query_matching_one_line_needs_no_picker() {
    let fixture = Fixture::new(HISTORY_MD);
    seed_history(&fixture, &[(1, "nixon run build"), (2, "nixon run other")]);

    let mut app = fixture.app(picks(&[]), FakeRunner::new());
    let outcome = app
        .history(&HistoryOpts {
            query: Some("build".to_owned()),
            select: true,
            ..HistoryOpts::default()
        })
        .unwrap();

    assert!(app.picker.calls.is_empty(), "the picker was asked");
    assert!(matches!(outcome, nixon::app::history::Outcome::Done(0)));
}
