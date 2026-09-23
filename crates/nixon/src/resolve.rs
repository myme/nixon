//! Resolving a command's placeholders.

use nixon_picker::{
    Candidate, CandidateStream, FilterPicker, Picker, PickerOption, PickerOptions, Selection,
    strip_ansi,
};
use serde::Deserialize;

use crate::command::{ArgSpec, Command};
use crate::error::{NixonError, Result};
use crate::eval::{Context, Evaluation, PROJECT_PATH_VAR, evaluate_capture, prepare};
use crate::format::{format_columns, pick_fields};
use crate::placeholder::{Placeholder, PlaceholderFormat, PlaceholderType};
use crate::process::ProcessRunner;
use crate::project::Project;

/// What a command's placeholders resolved to.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Resolved {
    /// Lines piped to the command's stdin.
    pub stdin: Option<Vec<String>>,
    /// Positional arguments, one per selected line.
    pub args: Vec<String>,
    /// Environment variables, each selection space-joined.
    pub env: Vec<(String, String)>,
    /// The arguments that would run this again: the options as they ended
    /// up, then every placeholder's chosen values, then the overflow.
    ///
    /// Values rather than queries: an exact value settles a placeholder
    /// without a prompt, so a replayed line asks nothing.
    pub replay: Vec<String>,
}

/// One JSON candidate: a bare string, or a title and a value.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum JsonCandidate {
    Plain(String),
    Titled { title: String, value: String },
}

impl From<JsonCandidate> for Candidate {
    fn from(candidate: JsonCandidate) -> Self {
        match candidate {
            JsonCandidate::Plain(text) => Self::identity(text),
            JsonCandidate::Titled { title, value } => Self::with_title(title, value),
        }
    }
}

/// Resolves placeholders by running the commands they reference.
pub struct Resolver<'a, P: Picker, R: ProcessRunner> {
    /// Everything evaluation needs from outside.
    pub context: &'a Context<'a>,
    /// The project commands run in.
    pub project: &'a Project,
    /// Every command available here, for looking placeholders up.
    pub commands: &'a [Command],
    /// The picker placeholder selections go through.
    pub picker: &'a mut P,
    /// The runner referenced commands are run with.
    pub runner: &'a mut R,
    /// Picker header: `show_command` of the *outer* command.
    pub header: String,
    /// The outer command's toggles, shown on every placeholder picker and
    /// carried from one to the next.
    pub options: Vec<PickerOption>,
}

impl<P: Picker, R: ProcessRunner> Resolver<'_, P, R> {
    /// Resolves `command` with its options at their defaults.
    ///
    /// This is the inner path: a command a placeholder references runs with
    /// its own defaults, never with the toggles the prompt is showing for
    /// the command that referenced it.
    pub fn resolve_env(&mut self, command: &Command, args: &[String]) -> Result<Resolved> {
        self.resolve_inner(command, args, &command.default_options(), false)
    }

    /// Resolves every argument of `command`, with the options in `on`.
    ///
    /// `args` are search queries for the matching placeholders, not values;
    /// args beyond the placeholders become pre-expanded positional arguments.
    /// An option that is on contributes its token where the heading put it,
    /// so `$1` and `"$@"` read like the heading.
    pub fn resolve_with(
        &mut self,
        command: &Command,
        args: &[String],
        on: &[bool],
    ) -> Result<Resolved> {
        self.resolve_inner(command, args, on, true)
    }

    /// `outer` says whether the toggles on screen belong to this command.
    fn resolve_inner(
        &mut self,
        command: &Command,
        args: &[String],
        on: &[bool],
        outer: bool,
    ) -> Result<Resolved> {
        let mut resolved = Resolved {
            env: vec![(
                PROJECT_PATH_VAR.to_owned(),
                self.project.path().to_string_lossy().into_owned(),
            )],
            ..Resolved::default()
        };

        let placeholders: Vec<Placeholder> = command.placeholders().cloned().collect();
        let queries = zip_args(&placeholders, args);

        // Every pick can change a toggle, so the picks come first and argv
        // is assembled from where the toggles ended up.
        let mut picked = Vec::with_capacity(queries.len());
        for (placeholder, query) in queries {
            let values = if placeholder.value.is_empty() {
                self.select_for(&placeholder, query.as_deref())?
            } else {
                placeholder.value.clone()
            };
            picked.push((placeholder, values));
        }

        // The picks above may have moved a toggle, so the final state wins.
        let on: Vec<bool> = if outer && !self.options.is_empty() {
            self.options.iter().map(|option| option.on).collect()
        } else {
            on.to_vec()
        };

        for (index, option) in command.options.iter().enumerate() {
            let set = on.get(index).copied().unwrap_or(option.default);
            resolved
                .env
                .push((option.env_var(), if set { "1" } else { "" }.to_owned()));
            // Only what the defaults do not already say.
            if set != option.default {
                resolved.replay.push(if set {
                    option.token.clone()
                } else {
                    format!("--no-{}", option.name)
                });
            }
        }

        for (_, values) in &picked {
            resolved.replay.extend(values.iter().cloned());
        }

        let mut picked = picked.into_iter();
        for spec in &command.args {
            match spec {
                ArgSpec::Option(index) => {
                    let option = &command.options[*index];
                    if on.get(*index).copied().unwrap_or(option.default) {
                        resolved.args.push(option.token.clone());
                    }
                }
                ArgSpec::Placeholder(_) => {
                    let Some((placeholder, values)) = picked.next() else {
                        continue;
                    };
                    place(&mut resolved, &placeholder, values);
                }
            }
        }

        // Arguments beyond the placeholders, already expanded.
        for (placeholder, values) in picked {
            place(&mut resolved, &placeholder, values);
        }

        Ok(resolved)
    }

    /// Runs the command a placeholder references and selects from its output.
    ///
    fn select_for(
        &mut self,
        placeholder: &Placeholder,
        query: Option<&str>,
    ) -> Result<Vec<String>> {
        let command = self
            .commands
            .iter()
            .find(|c| c.name == placeholder.name)
            .cloned()
            .ok_or_else(|| NixonError::UnknownCommand {
                name: placeholder.name.clone(),
            })?;

        // The referenced command resolves its own placeholders first, with no
        // arguments of its own.
        let resolved = self.resolve_env(&command, &[])?;
        let evaluation = Evaluation {
            args: resolved.args,
            // A placeholder command always runs in the project, whatever its
            // own pwd says.
            cwd: Some(self.project.path()),
            env: resolved.env,
            stdin: resolved.stdin,
        };

        let options = PickerOptions {
            header: Some(self.header.clone()),
            initial_query: query.map(ToOwned::to_owned),
            multi: placeholder.multiple,
            select_one: true,
            // A command-line argument that is a candidate's value is an
            // answer, not a search for one.
            select_exact: true,
            matching: crate::matcher_options(self.context.config),
            options: self.options.clone(),
            ..PickerOptions::default()
        };

        // `| filter` narrows the candidates before anything is shown, so the
        // command needs to have finished: a filtered placeholder buffers.
        let streams = placeholder.can_stream() && !placeholder.list && placeholder.filter.is_none();

        let selection = if streams {
            // Line-oriented formats are fed to the picker as the command
            // produces them, so it opens without waiting for the command.
            let mut stream = self.stream_candidates(&command, &evaluation, &placeholder.format)?;
            // Dropping the stream stops the command, on this path and on
            // the error path alike.
            let (selection, state) = self.picker.pick_stream_options(&options, &mut stream)?;
            self.set_options(&state);
            selection
        } else {
            // Columns need every row before the widths are known and JSON
            // needs the whole document, so those stay buffered.
            let captured = evaluate_capture(self.context, self.runner, &command, &evaluation)?;
            let mut candidates = candidates_for(&placeholder.format, &captured, &placeholder.name)?;

            if let Some(query) = &placeholder.filter {
                candidates = nixon_picker::filter(query, &candidates, options.matching);
            }

            // `| list` prints matches instead of asking, whatever picker is
            // configured.
            if placeholder.list {
                FilterPicker.pick(&options, candidates)?
            } else {
                let (selection, state) = self.picker.pick_options(&options, candidates)?;
                self.set_options(&state);
                selection
            }
        };

        match selection {
            Selection::Selected { items, .. } => Ok(items.into_iter().map(|c| c.value).collect()),
            // A command that produced nothing is not a user who changed
            // their mind: reporting it as a cancel exited 130 and said
            // "Selection canceled." for an empty `git ls-files`.
            Selection::Empty => Err(NixonError::NoCandidates {
                name: placeholder.name.clone(),
                filter: placeholder.filter.clone(),
            }),
            Selection::Canceled => Err(NixonError::Canceled),
        }
    }

    /// Carries the toggles from one placeholder picker to the next.
    fn set_options(&mut self, state: &[bool]) {
        for (option, on) in self.options.iter_mut().zip(state) {
            option.on = *on;
        }
    }

    /// Starts the command and streams its lines in as candidates.
    fn stream_candidates(
        &mut self,
        command: &Command,
        evaluation: &Evaluation,
        format: &PlaceholderFormat,
    ) -> Result<CandidateStream> {
        let invocation = prepare(self.context, command, evaluation)?;
        let (sender, receiver) = std::sync::mpsc::channel();

        let fields = match format {
            PlaceholderFormat::Fields(fields) => fields.clone(),
            _ => Vec::new(),
        };

        let running = self.runner.run_streaming(
            &invocation,
            Box::new(move |line| {
                let _ = sender.send(line_candidate(&line, &fields));
            }),
        )?;

        // Cancelling the pick kills the command, rather than leaving it to
        // finish into a channel nobody is reading.
        let running = std::sync::Arc::new(std::sync::Mutex::new(running));
        let cancel_running = std::sync::Arc::clone(&running);
        Ok(CandidateStream::new(
            receiver,
            Box::new(move || {
                if let Ok(mut running) = cancel_running.lock() {
                    let _ = running.kill();
                }
            }),
        )
        .with_completion(Box::new(move || {
            running
                .lock()
                .map_err(|_| std::io::Error::other("producer lock was poisoned"))?
                .try_wait()
        })))
    }
}

/// Builds selectable candidates from a command's output.
fn candidates_for(
    format: &PlaceholderFormat,
    captured: &crate::process::Captured,
    name: &str,
) -> Result<Vec<Candidate>> {
    Ok(match format {
        PlaceholderFormat::Lines => captured
            .lines()
            .into_iter()
            .map(Candidate::identity)
            .collect(),

        PlaceholderFormat::Fields(fields) => captured
            .lines()
            .into_iter()
            .map(|line| line_candidate(&line, fields))
            .collect(),

        PlaceholderFormat::Columns { has_header, cols } => {
            // Widths and columns are counted in what the user sees; the row
            // is still shown as the command printed it.
            let rows = captured.lines();
            let plain: Vec<String> = rows.iter().map(|row| strip_ansi(row)).collect();
            let titles = if *has_header { &rows[1..] } else { &rows[..] };
            titles
                .iter()
                .zip(format_columns(*has_header, cols, &plain))
                .map(|(title, (_, value))| Candidate::with_title(title, value))
                .collect()
        }

        PlaceholderFormat::Json => {
            let parsed: Vec<JsonCandidate> =
                serde_json::from_slice(&captured.stdout).map_err(|source| {
                    NixonError::InvalidJson {
                        name: name.to_owned(),
                        source,
                    }
                })?;
            parsed.into_iter().map(Into::into).collect()
        }
    })
}

/// One output line as a candidate, for the line-oriented formats.
///
/// Fields are cut from the visible text: an escape sequence with spaces
/// around it would otherwise count as a field of its own.
fn line_candidate(line: &str, fields: &[usize]) -> Candidate {
    if fields.is_empty() {
        return Candidate::identity(line);
    }
    let plain = strip_ansi(line);
    let words: Vec<String> = plain.split_whitespace().map(ToOwned::to_owned).collect();
    Candidate::with_title(line, pick_fields(fields, &words).join(" "))
}

/// Puts a placeholder's values in the right part of `resolved`.
fn place(resolved: &mut Resolved, placeholder: &Placeholder, values: Vec<String>) {
    match &placeholder.kind {
        PlaceholderType::Stdin => {
            resolved.stdin.get_or_insert_with(Vec::new).extend(values);
        }
        PlaceholderType::Arg => resolved.args.extend(values),
        PlaceholderType::EnvVar(name) => {
            resolved.env.push((name.clone(), values.join(" ")));
        }
    }
}

/// Pairs placeholders with the arguments given on the command line.
///
/// Each argument is the *search query* for its placeholder, not its value.
/// Arguments beyond the placeholders become pre-expanded `Arg` placeholders;
/// placeholders beyond the arguments get no query.
pub fn zip_args(
    placeholders: &[Placeholder],
    args: &[String],
) -> Vec<(Placeholder, Option<String>)> {
    let mut zipped: Vec<(Placeholder, Option<String>)> = placeholders
        .iter()
        .zip(args.iter())
        .map(|(placeholder, arg)| (placeholder.clone(), Some(arg.clone())))
        .collect();

    zipped.extend(
        placeholders
            .iter()
            .skip(args.len())
            .map(|placeholder| (placeholder.clone(), None)),
    );

    zipped.extend(args.iter().skip(placeholders.len()).map(|arg| {
        (
            Placeholder::with_value(PlaceholderType::Arg, "arg", vec![arg.clone()]),
            None,
        )
    }));

    zipped
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use nixon_picker::picker::ScriptedPicker;
    use nixon_picker::{
        Candidate, CandidateStream, Picker, PickerOptions, Selection, SelectionType,
    };

    use super::{Resolved, Resolver, zip_args};
    use crate::command::Command;
    use crate::config::Config;
    use crate::error::NixonError;
    use crate::eval::Context;
    use crate::language::Language;
    use crate::placeholder::{Placeholder, PlaceholderFormat, PlaceholderType, parse_one};
    use crate::process::FakeRunner;
    use crate::project::{Project, ProjectType};

    fn project() -> Project {
        Project::from_path(Path::new("/tmp/proj"), Vec::<ProjectType>::new())
    }

    fn command(name: &str, source: &str, placeholders: Vec<Placeholder>) -> Command {
        Command {
            name: name.to_owned(),
            source: source.to_owned(),
            lang: Language::Bash,
            args: crate::command::arg_specs(placeholders),
            ..Command::default()
        }
    }

    fn selected(values: &[&str]) -> Selection<Candidate> {
        Selection::selected(
            SelectionType::Default,
            values.iter().map(|v| Candidate::identity(*v)).collect(),
        )
    }

    struct Harness {
        cache: assert_fs::TempDir,
        config: Config,
        commands: Vec<Command>,
    }

    impl Harness {
        fn new(commands: Vec<Command>) -> Self {
            Self {
                cache: assert_fs::TempDir::new().unwrap(),
                config: Config::default(),
                commands,
            }
        }

        fn resolve(
            &self,
            outer: &Command,
            args: &[String],
            picker: &mut impl Picker,
            runner: &mut FakeRunner,
        ) -> crate::error::Result<Resolved> {
            let context = Context {
                config: &self.config,
                cache_dir: self.cache.path(),
                shell: Some("/bin/bash"),
                direnv_dir: None,
                exe: None,
            };
            let project = project();
            let mut resolver = Resolver {
                context: &context,
                project: &project,
                commands: &self.commands,
                picker,
                runner,
                header: outer.show(),
                options: Vec::new(),
            };
            resolver.resolve_env(outer, args)
        }

        fn resolve_with(
            &self,
            outer: &Command,
            args: &[String],
            on: &[bool],
            picker: &mut impl Picker,
            runner: &mut FakeRunner,
        ) -> crate::error::Result<Resolved> {
            let context = Context {
                config: &self.config,
                cache_dir: self.cache.path(),
                shell: Some("/bin/bash"),
                direnv_dir: None,
                exe: None,
            };
            let project = project();
            let mut resolver = Resolver {
                context: &context,
                project: &project,
                commands: &self.commands,
                picker,
                runner,
                header: outer.show(),
                options: Vec::new(),
            };
            resolver.resolve_with(outer, args, on)
        }
    }

    fn arg(name: &str) -> Placeholder {
        Placeholder::new(PlaceholderType::Arg, name)
    }

    /// A picker that cannot take the terminal, as happens with no TTY.
    struct FailingPicker;

    impl Picker for FailingPicker {
        fn pick(
            &mut self,
            _: &PickerOptions,
            _: Vec<Candidate>,
        ) -> std::io::Result<nixon_picker::Selection<Candidate>> {
            Err(std::io::Error::other("no terminal"))
        }

        fn pick_stream(
            &mut self,
            _: &PickerOptions,
            _: &mut CandidateStream,
        ) -> std::io::Result<nixon_picker::Selection<Candidate>> {
            Err(std::io::Error::other("no terminal"))
        }
    }

    fn with_options(name: &str, heading: &str) -> Command {
        let (_, args, options) = crate::command::parse_command_name(heading).unwrap();
        Command {
            name: name.to_owned(),
            source: "true\n".to_owned(),
            lang: Language::Bash,
            args,
            options,
            ..Command::default()
        }
    }

    #[test]
    fn an_option_that_is_on_takes_its_place_in_argv() {
        let harness = Harness::new(vec![command("files", "ls\n", Vec::new())]);
        let outer = with_options("edit", "edit --force ${files} -v");

        let mut picker = ScriptedPicker::new(vec![selected(&["a.txt"])]);
        let mut runner = FakeRunner::new().with_output(&["a.txt"]);

        let resolved = harness
            .resolve_with(&outer, &[], &[true, false], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(resolved.args, ["--force", "a.txt"]);

        let mut picker = ScriptedPicker::new(vec![selected(&["a.txt"])]);
        let mut runner = FakeRunner::new().with_output(&["a.txt"]);
        let resolved = harness
            .resolve_with(&outer, &[], &[false, true], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(resolved.args, ["a.txt", "-v"]);
    }

    #[test]
    fn every_option_is_exported_whether_on_or_off() {
        let harness = Harness::new(Vec::new());
        let outer = with_options("build", "build --release --no-cache");

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new();
        let resolved = harness
            .resolve_with(&outer, &[], &[true, false], &mut picker, &mut runner)
            .unwrap();

        assert!(
            resolved
                .env
                .contains(&("nixon_opt_release".to_owned(), "1".to_owned()))
        );
        assert!(
            resolved
                .env
                .contains(&("nixon_opt_no_cache".to_owned(), String::new()))
        );
    }

    #[test]
    fn options_fall_back_to_their_declared_defaults() {
        let harness = Harness::new(Vec::new());
        let mut outer = with_options("build", "build --release");
        outer.options[0].default = true;

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new();
        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(resolved.args, ["--release"]);
    }

    #[test]
    fn a_filter_modifier_narrows_the_candidates_before_selection() {
        let harness = Harness::new(vec![command("files", "ls\n", Vec::new())]);
        let mut placeholder = arg("files");
        placeholder.filter = Some("rs$".to_owned());
        let outer = command("edit", "vim\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![selected(&["src/main.rs"])]);
        let mut runner = FakeRunner::new().with_output(&["src/main.rs", "src/lib.rs", "README.md"]);

        harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        let offered: Vec<String> = picker.calls[0]
            .1
            .iter()
            .map(|candidate| candidate.value.clone())
            .collect();
        assert_eq!(offered, ["src/main.rs", "src/lib.rs"]);
    }

    #[test]
    fn a_filter_that_matches_nothing_says_which_filter() {
        let harness = Harness::new(vec![command("files", "ls\n", Vec::new())]);
        let mut placeholder = arg("files");
        placeholder.filter = Some("rs$".to_owned());
        let outer = command("edit", "vim\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new().with_output(&["README.md"]);

        let err = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap_err();
        assert_eq!(err.to_string(), "no candidates from `files` matching `rs$`");
    }

    #[test]
    fn candidates_from_a_command_that_printed_nothing_name_only_the_command() {
        let harness = Harness::new(vec![command("files", "ls\n", Vec::new())]);
        let outer = command("edit", "vim\n", vec![arg("files")]);

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new();

        let err = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap_err();
        assert_eq!(err.to_string(), "no candidates from `files`");
    }

    #[test]
    fn a_failed_pick_still_kills_the_streaming_command() {
        let harness = Harness::new(vec![command("files", "ls\n", Vec::new())]);
        let outer = command("edit", "vim\n", vec![arg("files")]);

        let mut picker = FailingPicker;
        let mut runner = FakeRunner::new().with_output(&["one", "two"]);

        assert!(
            harness
                .resolve(&outer, &[], &mut picker, &mut runner)
                .is_err()
        );
        assert!(
            runner.was_killed(),
            "the candidate command was left running after the pick failed"
        );
    }

    #[test]
    fn a_command_without_placeholders_resolves_to_the_project_path_only() {
        let harness = Harness::new(Vec::new());
        let outer = command("hello", "echo hi\n", Vec::new());
        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new();

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(resolved.args, Vec::<String>::new());
        assert_eq!(resolved.stdin, None);
        assert_eq!(
            resolved.env,
            [("nixon_project_path".to_owned(), "/tmp/proj".to_owned())]
        );
        assert!(runner.calls.is_empty());
    }

    #[test]
    fn an_arg_placeholder_runs_its_command_and_appends_the_selection() {
        let git_files = command("git-files", "git ls-files\n", Vec::new());
        let harness = Harness::new(vec![git_files]);
        let outer = command("vim-file", "vim \"$@\"\n", vec![arg("git-files")]);

        let mut picker = ScriptedPicker::new(vec![selected(&["README.md"])]);
        let mut runner = FakeRunner::new().with_output(&["Cargo.toml", "README.md"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        // The referenced command ran, in the project.
        assert_eq!(runner.calls.len(), 1);
        let invocation = runner.last().unwrap();
        assert_eq!(invocation.argv[0], "bash");
        assert_eq!(invocation.cwd, Some(PathBuf::from("/tmp/proj")));
        assert_eq!(
            std::fs::read_to_string(&invocation.argv[1]).unwrap(),
            "git ls-files\n"
        );

        // The picker saw both candidates and the outer command as its header.
        assert_eq!(picker.calls[0].1.len(), 2);
        assert_eq!(
            picker.calls[0].0.header.as_deref(),
            Some("vim-file ${git-files}")
        );

        assert_eq!(resolved.args, ["README.md"]);
    }

    #[test]
    fn several_selected_lines_become_several_positional_args() {
        let files = command("files", "ls\n", Vec::new());
        let harness = Harness::new(vec![files]);
        let mut placeholder = arg("files");
        placeholder.multiple = true;
        let outer = command("edit", "vim \"$@\"\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![selected(&["one", "two"])]);
        let mut runner = FakeRunner::new().with_output(&["one", "two", "three"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(resolved.args, ["one", "two"]);
        assert!(picker.calls[0].0.multi);
    }

    #[test]
    fn a_stdin_placeholder_becomes_stdin_lines() {
        let files = command("files", "ls\n", Vec::new());
        let harness = Harness::new(vec![files]);
        let outer = command(
            "count",
            "wc -l\n",
            vec![Placeholder::new(PlaceholderType::Stdin, "files")],
        );

        let mut picker = ScriptedPicker::new(vec![selected(&["one", "two"])]);
        let mut runner = FakeRunner::new().with_output(&["one", "two"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(
            resolved.stdin,
            Some(vec!["one".to_owned(), "two".to_owned()])
        );
        assert!(resolved.args.is_empty());
    }

    #[test]
    fn two_stdin_placeholders_concatenate_in_order() {
        let harness = Harness::new(vec![
            command("first", "a\n", Vec::new()),
            command("second", "b\n", Vec::new()),
        ]);
        let outer = command(
            "both",
            "cat\n",
            vec![
                Placeholder::new(PlaceholderType::Stdin, "first"),
                Placeholder::new(PlaceholderType::Stdin, "second"),
            ],
        );

        let mut picker = ScriptedPicker::new(vec![selected(&["one"]), selected(&["two"])]);
        let mut runner = FakeRunner::new()
            .with_output(&["one"])
            .with_output(&["two"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(
            resolved.stdin,
            Some(vec!["one".to_owned(), "two".to_owned()])
        );
    }

    #[test]
    fn an_env_var_placeholder_is_space_joined() {
        let files = command("files", "ls\n", Vec::new());
        let harness = Harness::new(vec![files]);
        let outer = command(
            "show",
            "echo $FILES\n",
            vec![Placeholder::new(
                PlaceholderType::EnvVar("FILES".to_owned()),
                "files",
            )],
        );

        let mut picker = ScriptedPicker::new(vec![selected(&["one", "two"])]);
        let mut runner = FakeRunner::new().with_output(&["one", "two"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(
            resolved.env,
            [
                ("nixon_project_path".to_owned(), "/tmp/proj".to_owned()),
                ("FILES".to_owned(), "one two".to_owned()),
            ]
        );
    }

    #[test]
    fn a_cli_arg_becomes_the_placeholders_search_query() {
        let files = command("files", "ls\n", Vec::new());
        let harness = Harness::new(vec![files]);
        let outer = command("edit", "vim\n", vec![arg("files")]);

        let mut picker = ScriptedPicker::new(vec![selected(&["README.md"])]);
        // Two rows match the query, or `-1` would answer it without asking.
        let mut runner = FakeRunner::new().with_output(&["README.md", "READING.md"]);

        harness
            .resolve(&outer, &["READ".to_owned()], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(picker.calls[0].0.initial_query.as_deref(), Some("READ"));
    }

    #[test]
    fn args_beyond_the_placeholders_are_passed_through_unselected() {
        let harness = Harness::new(Vec::new());
        let outer = command("echo", "echo \"$@\"\n", Vec::new());

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new();

        let resolved = harness
            .resolve(
                &outer,
                &["one".to_owned(), "two".to_owned()],
                &mut picker,
                &mut runner,
            )
            .unwrap();
        assert_eq!(resolved.args, ["one", "two"]);
        assert!(picker.calls.is_empty());
    }

    #[test]
    fn a_missing_referenced_command_is_a_typed_error() {
        let harness = Harness::new(Vec::new());
        let outer = command("edit", "vim\n", vec![arg("nope")]);

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new();

        let err = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap_err();
        assert!(matches!(err, NixonError::UnknownCommand { .. }));
        assert_eq!(err.to_string(), "Invalid argument: nope");
    }

    #[test]
    fn cancelling_during_expansion_propagates() {
        let files = command("files", "ls\n", Vec::new());
        let harness = Harness::new(vec![files]);
        let outer = command("edit", "vim\n", vec![arg("files")]);

        let mut picker = ScriptedPicker::new(vec![Selection::Canceled]);
        let mut runner = FakeRunner::new().with_output(&["one", "two"]);

        let err = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap_err();
        assert!(matches!(err, NixonError::Canceled));
        assert_eq!(err.exit_code(), 130);
    }

    #[test]
    fn a_nested_placeholder_resolves_before_its_parent_runs() {
        let branches = command("branches", "git branch\n", Vec::new());
        let log = command("log", "git log $1\n", vec![arg("branches")]);
        let harness = Harness::new(vec![branches, log]);
        let outer = command("show", "echo\n", vec![arg("log")]);

        let mut picker = ScriptedPicker::new(vec![selected(&["main"]), selected(&["abc123"])]);
        let mut runner = FakeRunner::new()
            .with_output(&["main", "riir"])
            .with_output(&["abc123", "def456"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        // branches ran first, then log with the chosen branch as its argument.
        assert_eq!(runner.calls.len(), 2);
        assert_eq!(runner.calls[1].1.argv.last().unwrap(), "main");
        assert_eq!(resolved.args, ["abc123"]);
    }

    #[test]
    fn a_fields_placeholder_selects_the_named_fields() {
        let listing = command("listing", "ls -l\n", Vec::new());
        let harness = Harness::new(vec![listing]);
        let mut placeholder = arg("listing");
        placeholder.format = PlaceholderFormat::Fields(vec![2]);
        let outer = command("pick", "echo\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![Selection::selected(
            SelectionType::Default,
            vec![Candidate::with_title("alpha beta gamma", "beta")],
        )]);
        let mut runner = FakeRunner::new().with_output(&["alpha beta gamma", "one two three"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        assert_eq!(picker.calls[0].1[0].display, "alpha beta gamma");
        assert_eq!(picker.calls[0].1[0].value, "beta");
        assert_eq!(resolved.args, ["beta"]);
    }

    #[test]
    fn a_json_placeholder_accepts_strings_and_title_value_objects() {
        let items = command("items", "echo\n", Vec::new());
        let harness = Harness::new(vec![items]);
        let mut placeholder = arg("items");
        placeholder.format = PlaceholderFormat::Json;
        let outer = command("pick", "echo\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![selected(&["v1"])]);
        let mut runner =
            FakeRunner::new().with_raw_output(br#"["plain", {"title": "Titled", "value": "v1"}]"#);

        harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        let candidates = &picker.calls[0].1;
        assert_eq!(candidates[0].display, "plain");
        assert_eq!(candidates[0].value, "plain");
        assert_eq!(candidates[1].display, "Titled");
        assert_eq!(candidates[1].value, "v1");
    }

    /// Fields are counted in what the user sees. An escape sequence with
    /// spaces around it is not a field of its own.
    #[test]
    fn fields_are_counted_in_the_visible_text() {
        let harness = Harness::new(vec![command("colors", "ls\n", Vec::new())]);
        let mut placeholder = arg("colors");
        placeholder.format = PlaceholderFormat::Fields(vec![1]);
        let outer = command("show", "echo\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![selected(&["alpha"])]);
        // The colour is set before the word, with a space between.
        let mut runner =
            FakeRunner::new().with_output(&["\u{1b}[31m alpha beta", "\u{1b}[32m gamma delta"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        assert_eq!(picker.calls[0].1[0].value, "alpha");
        assert_eq!(resolved.args, ["alpha"]);
    }

    /// Colour is for the eye: a field picked out of a coloured line must
    /// reach argv as the text, escapes and all removed.
    #[test]
    fn a_streamed_field_value_has_no_ansi_in_it() {
        let harness = Harness::new(vec![command("colors", "ls\n", Vec::new())]);
        let mut placeholder = arg("colors");
        placeholder.format = PlaceholderFormat::Fields(vec![1]);
        let outer = command("show", "echo\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![selected(&["alpha"])]);
        let mut runner = FakeRunner::new().with_output(&[
            "\u{1b}[31malpha\u{1b}[0m beta",
            "\u{1b}[32mgamma\u{1b}[0m delta",
        ]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        let offered = &picker.calls[0].1;
        assert!(offered[0].display.contains('\u{1b}'), "colour was lost");
        assert_eq!(offered[0].value, "alpha");
        assert_eq!(resolved.args, ["alpha"]);
    }

    /// The buffered formats build the same way, through `with_title`.
    #[test]
    fn a_buffered_column_or_json_value_has_no_ansi_in_it() {
        let harness = Harness::new(vec![command("items", "echo\n", Vec::new())]);
        let mut columns = arg("items");
        columns.format = PlaceholderFormat::Columns {
            has_header: false,
            cols: vec![1],
        };
        let outer = command("pick", "echo\n", vec![columns]);

        let mut picker = ScriptedPicker::new(vec![selected(&["alpha"])]);
        let mut runner = FakeRunner::new().with_output(&[
            "\u{1b}[31malpha\u{1b}[0m beta",
            "\u{1b}[32mgamma\u{1b}[0m delta",
        ]);
        harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert_eq!(picker.calls[0].1[0].value, "alpha");

        let mut json = arg("items");
        json.format = PlaceholderFormat::Json;
        let outer = command("pick", "echo\n", vec![json]);

        let mut picker = ScriptedPicker::new(vec![selected(&["v1"])]);
        // Two, or `-1` answers it without asking.
        let mut runner = FakeRunner::new().with_raw_output(
            b"[{\"title\": \"\\u001b[31mTitled\\u001b[0m\", \"value\": \"\\u001b[31mv1\\u001b[0m\"}, \"plain\"]",
        );
        harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();
        assert!(picker.calls[0].1[0].display.contains('\u{1b}'));
        assert_eq!(picker.calls[0].1[0].value, "v1");
    }

    #[test]
    fn bad_json_is_a_typed_error_not_a_panic() {
        let items = command("items", "echo\n", Vec::new());
        let harness = Harness::new(vec![items]);
        let mut placeholder = arg("items");
        placeholder.format = PlaceholderFormat::Json;
        let outer = command("pick", "echo\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(Vec::new());
        let mut runner = FakeRunner::new().with_raw_output(b"not json at all");

        let err = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap_err();
        assert!(matches!(err, NixonError::InvalidJson { .. }));
    }

    #[test]
    fn a_list_placeholder_never_reaches_the_configured_picker() {
        let files = command("files", "ls\n", Vec::new());
        let harness = Harness::new(vec![files]);
        let placeholder = parse_one("${files | list}").unwrap();
        let outer = command("show", "echo\n", vec![placeholder]);

        let mut picker = ScriptedPicker::new(vec![selected(&["never used"])]);
        let mut runner = FakeRunner::new().with_output(&["one", "two"]);

        let resolved = harness
            .resolve(&outer, &[], &mut picker, &mut runner)
            .unwrap();

        assert!(picker.calls.is_empty());
        assert_eq!(resolved.args, ["one", "two"]);
    }

    #[test]
    fn zip_args_pairs_queries_then_overflows() {
        let placeholders = vec![arg("a"), arg("b")];
        let args = vec!["qa".to_owned(), "qb".to_owned(), "extra".to_owned()];

        let zipped = zip_args(&placeholders, &args);
        assert_eq!(zipped.len(), 3);
        assert_eq!(zipped[0].1.as_deref(), Some("qa"));
        assert_eq!(zipped[1].1.as_deref(), Some("qb"));
        assert_eq!(zipped[2].0.value, ["extra"]);
        assert_eq!(zipped[2].1, None);
    }

    #[test]
    fn zip_args_leaves_unmatched_placeholders_without_a_query() {
        let placeholders = vec![arg("a"), arg("b")];
        let zipped = zip_args(&placeholders, &["only".to_owned()]);
        assert_eq!(zipped.len(), 2);
        assert_eq!(zipped[0].1.as_deref(), Some("only"));
        assert_eq!(zipped[1].1, None);
    }
}
