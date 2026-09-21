//! `nixon eval`.

use std::path::PathBuf;

use nixon_picker::{Picker, Selection, SelectionType};

use super::{App, RunOpts};
use crate::command::Command;
use crate::error::Result;
use crate::language::Language;
use crate::placeholder::Placeholder;
use crate::process::{ExitCode, ProcessRunner};

/// What to evaluate and how.
#[derive(Clone, Debug, Default)]
pub struct EvalOpts {
    /// The source to run, given inline.
    pub source: Option<String>,
    /// A file to read the source from, relative to nixon's cwd.
    pub file: Option<PathBuf>,
    /// Placeholders given on the command line.
    pub placeholders: Vec<Placeholder>,
    /// Override the language.
    pub language: Option<Language>,
    /// Select a project instead of using the current directory.
    pub select_project: bool,
    /// Run in the project at this path, rather than asking.
    pub project: Option<String>,
}

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Evaluates a one-off command.
    pub fn eval(&mut self, opts: &EvalOpts) -> Result<ExitCode> {
        // Without --project, fall back to the current
        // directory as `run` does, rather than v1's interactive picker.
        let project = match (&opts.project, opts.select_project) {
            (Some(path), _) => self.project_for_query(Some(path))?,
            (None, true) => self.pick_one_project(None)?,
            (None, false) => self.current_project(),
        };

        let (source, detected) = match &opts.file {
            Some(path) => (
                std::fs::read_to_string(path)?,
                Language::from_file_path(path),
            ),
            None => (opts.source.clone().unwrap_or_default(), Language::Bash),
        };

        let command = Command {
            name: String::new(),
            source,
            lang: opts.language.clone().unwrap_or(detected),
            pwd: Some(project.path()),
            args: crate::command::arg_specs(opts.placeholders.clone()),
            ..Command::default()
        };

        let selection = Selection::Selected {
            kind: SelectionType::Default,
            items: vec![command],
        };
        self.handle_cmd(&project, selection, &RunOpts::default())
    }
}
