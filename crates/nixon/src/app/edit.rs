//! `nixon edit`. SPEC §10.5.

use nixon_picker::{Picker, Selection};

use super::App;
use crate::error::{NixonError, Result};
use crate::process::{ExitCode, ProcessRunner};

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Opens a command where it is defined. SPEC §10.5.
    ///
    /// Hidden commands are offered here, unlike in the run picker.
    pub fn edit(&mut self, query: Option<&str>) -> Result<ExitCode> {
        let project = self.current_project();
        let commands = self.commands_for(&project)?;
        let selection = self.pick_command(&project, &commands, "Edit command", query)?;

        let command = match selection {
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
            Selection::Selected { items, .. } => items
                .into_iter()
                .next()
                .ok_or_else(|| NixonError::NothingSelected("No command selected.".to_owned()))?,
        };

        self.visit_cmd(&command)?;
        Ok(0)
    }
}
