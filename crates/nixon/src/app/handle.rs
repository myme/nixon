//! What happens once a command has been selected.

use nixon_picker::{Picker, PickerOption, PickerOptions, Selection, SelectionType};

use super::run::{NO_COMMANDS, RunDecision};
use super::{App, RunOpts, context};
use crate::command::{Command, Description};
use crate::config::Config;
use crate::error::{NixonError, Result};
use crate::eval::{Evaluation, evaluate};
use crate::output;
use crate::process::{ExitCode, ProcessRunner};
use crate::project::Project;
use crate::resolve::Resolver;

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Acts on a command selection.
    ///
    /// `--insert` and `--select` are checked before the selection type, so
    /// `nixon -i` followed by Alt-Enter still just prints the source.
    pub fn handle_cmd(
        &mut self,
        project: &Project,
        selection: Selection<Command>,
        opts: &RunOpts,
    ) -> Result<ExitCode> {
        let decision = self.prepare_selected_command(project, selection, opts)?;
        self.finish_run_decision(project, decision)
    }

    pub(super) fn prepare_selected_command(
        &mut self,
        project: &Project,
        selection: Selection<Command>,
        opts: &RunOpts,
    ) -> Result<RunDecision> {
        let (kind, command) = match selection {
            Selection::Empty => {
                return Err(NixonError::NothingSelected(
                    "No command selected.".to_owned(),
                ));
            }
            Selection::Canceled => return Err(NixonError::Canceled),
            Selection::Selected { items, .. } if items.len() > 1 => {
                return Err(NixonError::NothingSelected(
                    "Multiple commands selected.".to_owned(),
                ));
            }
            Selection::Selected { kind, items } => {
                let Some(command) = items.into_iter().next() else {
                    return Err(NixonError::NothingSelected(
                        "No command selected.".to_owned(),
                    ));
                };
                (kind, command)
            }
        };

        if opts.insert {
            return Ok(RunDecision::InsertSource(command));
        }

        if opts.select {
            let values = self.select_from(project, &command)?;
            return Ok(RunDecision::SelectedValues(command, values));
        }

        Ok(match kind {
            SelectionType::Default => RunDecision::Run(command, opts.args.clone()),
            SelectionType::Edit => RunDecision::Edit(command, opts.args.clone()),
            SelectionType::Show => RunDecision::ShowSource(command),
            SelectionType::Visit => RunDecision::Visit(command),
        })
    }

    pub(super) fn finish_run_decision(
        &mut self,
        project: &Project,
        decision: RunDecision,
    ) -> Result<ExitCode> {
        match decision {
            RunDecision::List(lines) => {
                if lines.is_empty() {
                    tracing::error!("{NO_COMMANDS}");
                } else {
                    output::lines(&lines)?;
                }
                Ok(0)
            }
            RunDecision::InsertSource(command) | RunDecision::ShowSource(command) => {
                output::raw(&command.source)?;
                Ok(0)
            }
            RunDecision::SelectedValues(_, values) => {
                output::lines(&values)?;
                Ok(0)
            }
            RunDecision::Run(command, args) => self.run_cmd(project, &command, &args),
            RunDecision::Edit(command, args) => self.edit_then_run(project, command, &args),
            RunDecision::Visit(command) => {
                self.visit_cmd(&command)?;
                Ok(0)
            }
        }
    }

    /// Resolves a command's placeholders and runs it.
    pub fn run_cmd(
        &mut self,
        project: &Project,
        command: &Command,
        args: &[String],
    ) -> Result<ExitCode> {
        let config = self.config_for(project)?;
        let commands = self.commands_for(project)?;
        let cache = self.dirs.cache_dir();
        let header = command.show();

        // A word matching an option's token settles it; the rest stay
        // placeholder queries.
        let overrides = command.split_args(args);
        let mut on = overrides.apply(&command.default_options());

        // With no placeholder to ask about, the toggles get a prompt of
        // their own — unless the command line already settled them all, or
        // there is no terminal, in which case the defaults stand.
        if command.placeholders().next().is_none()
            && !command.options.is_empty()
            && !overrides.is_complete()
        {
            let prompt = PickerOptions {
                header: Some(command.show()),
                options: picker_options(command, &on),
                ..PickerOptions::default()
            };
            match self.picker.confirm(&prompt)? {
                Some(state) => on = state,
                None => return Err(NixonError::Canceled),
            }
        }

        let resolved = {
            let context = context(&self.env, &config, &cache);
            let mut resolver = Resolver {
                context: &context,
                project,
                commands: &commands,
                picker: &mut self.picker,
                runner: &mut self.runner,
                header,
                options: picker_options(command, &on),
            };
            resolver.resolve_with(command, &overrides.queries, &on)?
        };

        let replay = resolved.replay.clone();

        let context = context(&self.env, &config, &cache);
        let code = evaluate(
            &context,
            &mut self.runner,
            command,
            &Evaluation {
                args: resolved.args,
                // `eval` sets pwd; everything else runs in the project.
                cwd: Some(command.pwd.clone().unwrap_or_else(|| project.path())),
                env: resolved.env,
                stdin: resolved.stdin,
            },
        )?;

        // It ran, so it is worth recording — whatever it exited with.
        self.record(&config, project, command, replay);
        Ok(code)
    }

    /// Logs the run, in the form that repeats it.
    ///
    /// `nixon project <path> <name>` when the command ran somewhere other
    /// than the project the current directory is in, so the line means the
    /// same thing from anywhere.
    fn record(&self, config: &Config, project: &Project, command: &Command, replay: Vec<String>) {
        // The project's config, so a repository can turn recording off for
        // itself.
        if !config.records_history() {
            return;
        }

        // An unnamed command is an `eval`; there is nothing to look up, so
        // the source itself is what repeats it — in the language it ran in,
        // and in the project it ran in.
        let mut invocation = if command.name.is_empty() {
            let mut eval = vec!["eval".to_owned()];
            if project.path() != self.current_project().path() {
                eval.push(format!("--project={}", project.path().display()));
            }
            eval.push("-l".to_owned());
            eval.push(command.lang.to_string());
            eval.push(command.source.trim_end().to_owned());
            // The specs, not the values they resolved to: `eval` takes
            // placeholders after its source, and a resolved value has no
            // spelling there. A replay asks again.
            eval.extend(command.args.iter().filter_map(|spec| match spec {
                crate::command::ArgSpec::Placeholder(placeholder) => Some(placeholder.to_string()),
                crate::command::ArgSpec::Option(_) => None,
            }));
            return self.write_record(eval);
        } else if project.path() == self.current_project().path() {
            vec!["run".to_owned(), command.name.clone()]
        } else {
            vec![
                "project".to_owned(),
                project.path().to_string_lossy().into_owned(),
                command.name.clone(),
            ]
        };
        invocation.extend(replay);

        self.write_record(invocation);
    }

    /// Appends one invocation to the log.
    fn write_record(&self, invocation: Vec<String>) {
        let entry = crate::history::Entry::new(&self.env.cwd, invocation);
        crate::history::record(&self.dirs.history_file(), &entry);
    }

    /// Runs a command and offers its output for selection.
    ///
    /// `--select` treats the chosen command as a candidate producer, not as
    /// something to run for its effect; multi-select is forced on.
    pub fn select_from(&mut self, project: &Project, command: &Command) -> Result<Vec<String>> {
        let config = self.config_for(project)?;
        let commands = self.commands_for(project)?;
        let cache = self.dirs.cache_dir();

        let mut placeholder = crate::placeholder::Placeholder::new(
            crate::placeholder::PlaceholderType::Arg,
            command.name.clone(),
        );
        placeholder.multiple = true;

        let producer = Command {
            args: crate::command::arg_specs(vec![placeholder]),
            ..Command::default()
        };

        let context = context(&self.env, &config, &cache);
        let mut resolver = Resolver {
            context: &context,
            project,
            commands: &commands,
            picker: &mut self.picker,
            runner: &mut self.runner,
            header: command.show(),
            options: Vec::new(),
        };
        Ok(resolver.resolve_env(&producer, &[])?.args)
    }

    /// Opens the source in an inline editor, then runs what comes back.
    fn edit_then_run(
        &mut self,
        project: &Project,
        command: Command,
        args: &[String],
    ) -> Result<ExitCode> {
        let edited = match nixon_picker::editor::edit_text(command.source.trim())? {
            nixon_picker::editor::Edited::Canceled => return Err(NixonError::Canceled),
            nixon_picker::editor::Edited::Submitted(text) => text,
        };
        self.run_edited_cmd(project, command, &edited, args)
    }

    /// Runs submitted editor text with the command's original metadata.
    pub fn run_edited_cmd(
        &mut self,
        project: &Project,
        mut command: Command,
        edited: &str,
        args: &[String],
    ) -> Result<ExitCode> {
        if edited.trim().is_empty() {
            return Err(NixonError::NothingSelected("Empty command.".to_owned()));
        }

        command.source = format!("{}\n", edited.trim_end());
        self.run_cmd(project, &command, args)
    }

    /// Opens a command where it is defined.
    pub fn visit_cmd(&mut self, command: &Command) -> Result<()> {
        let location = command.location.as_ref().ok_or_else(|| {
            NixonError::NothingSelected("Unable to find command location.".to_owned())
        })?;

        let invocation = crate::process::Invocation {
            argv: vec![
                self.env.editor().to_owned(),
                format!("+{}", location.start_line),
                location.file_path.to_string_lossy().into_owned(),
            ],
            ..crate::process::Invocation::default()
        };
        self.runner.run(&invocation)?;
        Ok(())
    }
}

/// The toggles a command shows, at the state it is about to run with.
fn picker_options(command: &Command, on: &[bool]) -> Vec<PickerOption> {
    command
        .options
        .iter()
        .enumerate()
        .map(|(index, option)| PickerOption {
            label: option.token.clone(),
            description: option.description.as_ref().map(Description::plain),
            on: on.get(index).copied().unwrap_or(option.default),
        })
        .collect()
}
