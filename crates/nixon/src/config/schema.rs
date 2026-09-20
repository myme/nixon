//! The JSON/YAML schema of a config block. SPEC §3.4.

use std::path::PathBuf;

use serde::Deserialize;

use crate::project::{ProjectMarker, ProjectType};

/// A config block's fields. Unknown keys are ignored. SPEC §3.4.
///
/// SPEC also lists `terminal`; ENGINEERING §7.2 removes it, so it now falls
/// under the unknown-keys rule like any other stray key.
#[derive(Debug, Default, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct ConfigBlock {
    /// Directories of executables offered as commands.
    pub bin_dirs: Vec<PathBuf>,
    /// Exact rather than fuzzy matching in the picker.
    pub exact_match: Option<bool>,
    /// Case-insensitive matching in the picker.
    pub ignore_case: Option<bool>,
    /// Where to look for projects; entries may use `~` and `$VAR`.
    pub project_dirs: Vec<PathBuf>,
    /// Project type definitions.
    pub project_types: Vec<ProjectTypeSpec>,
    /// Wrap commands in `direnv exec`.
    pub use_direnv: Option<bool>,
    /// Wrap commands in `nix-shell`.
    pub use_nix: Option<bool>,
    /// Whether discovery also finds git worktrees. ENGINEERING §7.6.
    pub git_worktrees: Option<bool>,
}

/// One entry of `project_types`. `name` and `desc` are required. SPEC §3.4.
#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct ProjectTypeSpec {
    /// The type's id.
    pub name: String,
    /// Marker paths, relative to the project root.
    #[serde(default)]
    pub test: Vec<PathBuf>,
    /// Human-readable description.
    pub desc: String,
}

impl From<ProjectTypeSpec> for ProjectType {
    fn from(spec: ProjectTypeSpec) -> Self {
        Self {
            id: spec.name,
            markers: spec.test.into_iter().map(ProjectMarker::Path).collect(),
            description: spec.desc,
        }
    }
}
