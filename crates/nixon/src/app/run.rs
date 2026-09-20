//! `nixon run`. SPEC §10.1.

use nixon_picker::{FilterPicker, Picker, PickerOptions, Selection};

use super::{App, RunOpts};
use crate::command::Command;
use crate::error::Result;
use crate::output;
use crate::process::{ExitCode, ProcessRunner};
use crate::project::Project;
use crate::select;

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Selects a command in the current project and acts on it. SPEC §10.1.
    pub fn run(&mut self, opts: &RunOpts) -> Result<ExitCode> {
        let project = self.current_project();
        if opts.list {
            return self.list_commands(&project, opts.command.as_deref());
        }
        self.find_and_handle_cmd(&project, opts)
    }

    /// Prints matching command names. SPEC §10.1.
    ///
    /// Hidden commands are included here, unlike in the picker, and no
    /// matches prints `No commands.` on stderr and still exits 0 — the shell
    /// widgets rely on that.
    pub fn list_commands(&mut self, project: &Project, query: Option<&str>) -> Result<ExitCode> {
        let commands = self.commands_for(project)?;
        let candidates = select::command_candidates(&commands);

        let options = PickerOptions {
            initial_query: query.map(ToOwned::to_owned),
            matching: crate::matcher_options(&self.config),
            ..PickerOptions::default()
        };
        let selection = FilterPicker.pick(&options, candidates)?;

        let matched: Vec<String> = selection
            .items()
            .iter()
            .map(|candidate| candidate.display.clone())
            .collect();

        if matched.is_empty() {
            tracing::error!("No commands.");
            return Ok(0);
        }
        output::lines(&matched)?;
        Ok(0)
    }

    /// Picks a command, then hands it to [`App::handle_cmd`]. SPEC §10.1.
    ///
    /// Hidden `_commands` are excluded from the picker but remain available
    /// to placeholders. SPEC §5.5.
    pub fn find_and_handle_cmd(&mut self, project: &Project, opts: &RunOpts) -> Result<ExitCode> {
        let commands = self.commands_for(project)?;
        let visible: Vec<Command> = commands
            .iter()
            .filter(|command| !command.is_hidden)
            .cloned()
            .collect();

        let selection =
            self.pick_command(project, &visible, "Select command", opts.command.as_deref())?;
        self.handle_cmd(project, selection, opts)
    }

    /// Runs a command selection through the picker. SPEC §8.4.
    pub fn pick_command(
        &mut self,
        project: &Project,
        commands: &[Command],
        prompt: &str,
        query: Option<&str>,
    ) -> Result<Selection<Command>> {
        let config = self.config_for(project)?;
        let options = select::command_options(&config, project, prompt, query);
        let candidates = select::command_candidates(commands);
        let selection = self.picker.pick(&options, candidates)?;

        Ok(selection.map(|candidate| {
            commands
                .iter()
                .find(|command| command.name == candidate.value)
                .cloned()
                .unwrap_or_default()
        }))
    }
}
