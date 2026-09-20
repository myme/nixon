//! `nixon project`.

use std::path::{Path, PathBuf};

use nixon_picker::{Candidate, FilterPicker, Picker, PickerOptions, Selection, SelectionType};

use super::{App, RunOpts};
use crate::error::{NixonError, Result};
use crate::output;
use crate::process::{ExitCode, ProcessRunner};
use crate::project::Project;
use crate::project::detect::{find_in_project, find_project_types, inspect};
use crate::select;

/// What `project` was asked to do.
#[derive(Clone, Debug, Default)]
pub struct ProjectOpts {
    /// The project name, used as the picker's query.
    pub project: Option<String>,
    /// What to do in the project once chosen.
    pub run: RunOpts,
    /// List projects instead of choosing one.
    pub list: bool,
    /// Print the chosen project's path.
    pub select: bool,
    /// Print what is known about the chosen project.
    pub inspect: bool,
}

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Selects a project and then a command in it.
    pub fn project(&mut self, opts: &ProjectOpts) -> Result<ExitCode> {
        if opts.list {
            return self.list_projects(opts.project.as_deref());
        }

        let multiple = opts.select || opts.inspect;
        let (kind, projects) = self.pick_projects(opts.project.as_deref(), multiple)?;

        if opts.select {
            let paths: Vec<String> = projects
                .iter()
                .map(|project| project.path().to_string_lossy().into_owned())
                .collect();
            output::lines(&paths)?;
            return Ok(0);
        }

        if opts.inspect || kind == SelectionType::Show {
            output::raw(&inspect(&projects))?;
            return Ok(0);
        }

        match projects.len() {
            1 => self.find_and_handle_cmd(&projects[0], &opts.run),
            _ => Err(NixonError::NothingSelected(
                "Multiple projects selected.".to_owned(),
            )),
        }
    }

    /// Prints matching project paths with `~` for `$HOME`.
    pub fn list_projects(&mut self, query: Option<&str>) -> Result<ExitCode> {
        let projects = self.projects();
        let candidates = select::project_candidates(&projects, &self.dirs.home);

        let options = PickerOptions {
            initial_query: query.map(ToOwned::to_owned),
            matching: crate::matcher_options(&self.config),
            ..PickerOptions::default()
        };
        let selection = FilterPicker.pick(&options, candidates)?;

        let matched: Vec<String> = selection
            .items()
            .iter()
            // The visible text: display may carry ANSI for the picker.
            .map(Candidate::plain)
            .collect();

        if matched.is_empty() {
            tracing::error!("No projects.");
            return Ok(0);
        }
        output::lines(&matched)?;
        Ok(0)
    }

    /// Picks projects, honouring the `.` shortcut.
    ///
    /// `.` means the project containing the current directory, falling back
    /// to an unfiltered picker when there is none.
    fn pick_projects(
        &mut self,
        query: Option<&str>,
        multiple: bool,
    ) -> Result<(SelectionType, Vec<Project>)> {
        if query == Some(".")
            && let Some(project) = find_in_project(&self.config.project_types, &self.env.cwd)
        {
            return Ok((SelectionType::Default, vec![project]));
        }
        if let Some(path) = query.filter(|query| *query != ".").and_then(as_path) {
            return Ok((SelectionType::Default, vec![self.project_at(&path)?]));
        }
        let query = if query == Some(".") { None } else { query };

        let projects = self.projects();
        let options = select::project_options(&self.config, query, multiple);
        let candidates = select::project_candidates(&projects, &self.dirs.home);

        match self.picker.pick(&options, candidates)? {
            Selection::Empty => Err(NixonError::NothingSelected(
                "No project selected.".to_owned(),
            )),
            Selection::Canceled => Err(NixonError::Canceled),
            Selection::Selected { kind, items } => {
                let chosen = items
                    .into_iter()
                    .map(|candidate| {
                        let path = PathBuf::from(&candidate.value);
                        projects
                            .iter()
                            .find(|project| project.path() == path)
                            .cloned()
                            .unwrap_or_else(|| Project::from_path(&path, Vec::new()))
                    })
                    .collect();
                Ok((kind, chosen))
            }
        }
    }

    /// The project a path names, with its types detected on the spot.
    ///
    /// No discovery and no picker: a path is already an answer. It need not
    /// be under `project_dirs` at all, which is the point — a command can
    /// hand nixon a directory it worked out for itself.
    fn project_at(&self, path: &Path) -> Result<Project> {
        let path = expand_home(path, &self.dirs.home);
        if !path.is_dir() {
            return Err(NixonError::NoSuchProject {
                path: path.to_string_lossy().into_owned(),
            });
        }
        let path = path.canonicalize().unwrap_or(path);
        let types = find_project_types(&path, &self.config.project_types);
        Ok(Project::from_path(&path, types))
    }

    /// Picks exactly one project, which is what `eval --project` wants.
    pub fn pick_one_project(&mut self, query: Option<&str>) -> Result<Project> {
        let (_, projects) = self.pick_projects(query, false)?;
        projects
            .into_iter()
            .next()
            .ok_or_else(|| NixonError::NothingSelected("No project selected.".to_owned()))
    }

    /// The project for a query, or the current one.
    pub fn project_for_query(&mut self, query: Option<&str>) -> Result<Project> {
        if query == Some(".")
            && let Some(project) = find_in_project(&self.config.project_types, &self.env.cwd)
        {
            return Ok(project);
        }
        self.pick_one_project(query)
    }
}

/// Whether a query names a directory rather than describing one.
///
/// A path is anything with a separator in it, or the three prefixes a shell
/// user writes for "here" and "home".
fn as_path(query: &str) -> Option<PathBuf> {
    let looks_like_path = query.contains('/')
        || query == "~"
        || query == ".."
        || query.starts_with("~/")
        || query.starts_with("./")
        || query.starts_with("../");
    looks_like_path.then(|| PathBuf::from(query))
}

/// Expands a leading `~` against the user's home.
fn expand_home(path: &Path, home: &Path) -> PathBuf {
    let text = path.to_string_lossy();
    match text.strip_prefix('~') {
        Some("") => home.to_path_buf(),
        Some(rest) => rest
            .strip_prefix('/')
            .map_or_else(|| path.to_path_buf(), |rest| home.join(rest)),
        None => path.to_path_buf(),
    }
}
