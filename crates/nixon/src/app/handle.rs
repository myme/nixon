//! What happens once a command has been selected. SPEC §10.7.

use nixon_picker::{Picker, Selection, SelectionType};

use super::{App, RunOpts, context};
use crate::command::Command;
use crate::error::{NixonError, Result};
use crate::eval::{Evaluation, evaluate};
use crate::output;
use crate::process::{ExitCode, ProcessRunner};
use crate::project::Project;
use crate::resolve::Resolver;

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Acts on a command selection. SPEC §10.7.
    ///
    /// `--insert` and `--select` are checked before the selection type, so
    /// `nixon -i` followed by Alt-Enter still just prints the source.
    pub fn handle_cmd(
        &mut self,
        project: &Project,
        selection: Selection<Command>,
        opts: &RunOpts,
    ) -> Result<ExitCode> {
        let (kind, command) = match selection {
            Selection::Empty => {
                return Err(NixonError::NothingSelected(
                    "No command selected.".to_owned(),
                ));
            }
            Selection::Canceled => return Err(NixonError::Canceled),
            Selection::Selected { mut items, .. } if items.len() > 1 => {
                items.clear();
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
            output::raw(&command.source)?;
            return Ok(0);
        }

        if opts.select {
            let values = self.select_from(project, &command)?;
            output::lines(&values)?;
            // v1 printed a trailing blank line after the values. SPEC §10.7.
            output::line("")?;
            return Ok(0);
        }

        match kind {
            SelectionType::Default => self.run_cmd(project, &command, &opts.args),
            SelectionType::Edit => self.edit_then_run(project, command, &opts.args),
            SelectionType::Show => {
                output::raw(&command.source)?;
                Ok(0)
            }
            SelectionType::Visit => {
                self.visit_cmd(&command)?;
                Ok(0)
            }
        }
    }

    /// Resolves a command's placeholders and runs it. SPEC §5.6, §7.3.
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

        let resolved = {
            let context = context(&self.env, &config, &cache);
            let mut resolver = Resolver {
                context: &context,
                project,
                commands: &commands,
                picker: &mut self.picker,
                runner: &mut self.runner,
                header,
            };
            resolver.resolve_env(command, args)?
        };

        let context = context(&self.env, &config, &cache);
        evaluate(
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
        )
    }

    /// Runs a command and offers its output for selection. SPEC §10.7.
    ///
    /// `--select` treats the chosen command as a candidate producer, not as
    /// something to run for its effect; multi-select is forced on.
    fn select_from(&mut self, project: &Project, command: &Command) -> Result<Vec<String>> {
        let config = self.config_for(project)?;
        let commands = self.commands_for(project)?;
        let cache = self.dirs.cache_dir();

        let mut placeholder = crate::placeholder::Placeholder::new(
            crate::placeholder::PlaceholderType::Arg,
            command.name.clone(),
        );
        placeholder.multiple = true;

        let producer = Command {
            placeholders: vec![placeholder],
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
        };
        Ok(resolver.resolve_env(&producer, &[])?.args)
    }

    /// Opens the source in an inline editor, then runs what comes back.
    /// SPEC §10.7, ENGINEERING §7.1 decision 4.
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
        if edited.trim().is_empty() {
            return Err(NixonError::NothingSelected("Empty command.".to_owned()));
        }

        let mut edited_command = command;
        edited_command.source = format!("{}\n", edited.trim_end());
        self.run_cmd(project, &edited_command, args)
    }

    /// Opens a command where it is defined. SPEC §10.5.
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
