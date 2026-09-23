//! `nixon run`.

use nixon_picker::{Candidate, FilterPicker, Picker, PickerOptions, Selection, SelectionType};

use super::{App, RunOpts};
use crate::command::Command;
use crate::error::{NixonError, Result};
use crate::output;
use crate::process::{ExitCode, ProcessRunner};
use crate::project::Project;
use crate::select;

/// Message shown when `run --list` has no matches.
pub const NO_COMMANDS: &str = "No commands.";

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Selects a command in the current project and acts on it.
    pub fn run(&mut self, opts: &RunOpts) -> Result<ExitCode> {
        let project = self.current_project();
        if opts.list {
            return self.list_commands(&project, opts.command.as_deref());
        }
        self.find_and_handle_cmd(&project, opts)
    }

    /// Prints matching command names.
    ///
    /// Hidden commands are included here, unlike in the picker, and no
    /// matches prints `No commands.` on stderr and still exits 0 — the shell
    /// widgets rely on that.
    pub fn list_commands(&mut self, project: &Project, query: Option<&str>) -> Result<ExitCode> {
        let matched = self.matching_command_names(project, query)?;

        if matched.is_empty() {
            tracing::error!("{NO_COMMANDS}");
            return Ok(0);
        }
        output::lines(&matched)?;
        Ok(0)
    }

    /// Returns the same plain command lines that `run --list` prints.
    pub fn matching_command_names(
        &self,
        project: &Project,
        query: Option<&str>,
    ) -> Result<Vec<String>> {
        let commands = self.commands_for(project)?;
        let candidates = select::command_candidates(&commands);

        // The project's config, as the picker uses: a local `exact_match`
        // must mean the same thing for `--list` as for selecting.
        let config = self.config_for(project)?;
        let options = PickerOptions {
            initial_query: query.map(ToOwned::to_owned),
            matching: crate::matcher_options(&config),
            ..PickerOptions::default()
        };
        let selection = FilterPicker.pick(&options, candidates)?;

        Ok(selection
            .items()
            .iter()
            // The visible text: display may carry ANSI for the picker.
            .map(Candidate::plain)
            .collect())
    }

    /// Picks a command, then hands it to [`App::handle_cmd`].
    ///
    /// Hidden `_commands` are excluded from the picker but remain available
    /// to placeholders.
    pub fn find_and_handle_cmd(&mut self, project: &Project, opts: &RunOpts) -> Result<ExitCode> {
        let selection = self.choose_command(project, opts.command.as_deref())?;
        self.handle_cmd(project, selection, opts)
    }

    /// Chooses a command with the same exact-name and hidden-command rules as `run`.
    pub fn choose_command(
        &mut self,
        project: &Project,
        query: Option<&str>,
    ) -> Result<Selection<Command>> {
        let commands = self.commands_for(project)?;

        // A name given in full is an answer, hidden or not: `nixon run
        // _packages` means that command, and it is not in the picker to be
        // chosen from.
        if let Some(command) = exact(&commands, query) {
            return Ok(Selection::selected(SelectionType::Default, vec![command]));
        }

        let visible: Vec<Command> = commands
            .iter()
            .filter(|command| !command.is_hidden)
            .cloned()
            .collect();

        if visible.is_empty() {
            return Ok(Selection::Empty);
        }

        self.pick_command(project, &visible, "Select command", query)
    }

    /// Runs a command selection through the picker.
    pub fn pick_command(
        &mut self,
        project: &Project,
        commands: &[Command],
        prompt: &str,
        query: Option<&str>,
    ) -> Result<Selection<Command>> {
        // A name that matches one of these exactly needs no picker, even
        // when it is also a fuzzy match for others.
        if let Some(command) = exact(commands, query) {
            return Ok(Selection::selected(SelectionType::Default, vec![command]));
        }

        let config = self.config_for(project)?;
        let options = select::command_options(&config, project, prompt, query);
        let candidates = select::command_candidates(commands);
        let selection = self.picker.pick(&options, candidates)?;

        // A value that maps to no command means the picker and the command
        // list have gone out of step. Defaulting produced an empty command
        // that then ran, writing an empty script and executing it.
        match selection {
            Selection::Empty => Ok(Selection::Empty),
            Selection::Canceled => Ok(Selection::Canceled),
            Selection::Selected { kind, items } => {
                let picked: Result<Vec<Command>> = items
                    .into_iter()
                    .map(|candidate| {
                        commands
                            .iter()
                            .find(|command| command.name == candidate.value)
                            .cloned()
                            .ok_or(NixonError::UnknownCommand {
                                name: candidate.value,
                            })
                    })
                    .collect();
                Ok(Selection::Selected {
                    kind,
                    items: picked?,
                })
            }
        }
    }
}

/// The command whose name is exactly `query`.
fn exact(commands: &[Command], query: Option<&str>) -> Option<Command> {
    let query = query?;
    commands
        .iter()
        .find(|command| command.name == query)
        .cloned()
}
