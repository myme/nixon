//! Project types and the markers that identify them.
//!
//! Detection and discovery live in the submodules; this module is the pure
//! data the config block parses into.

pub mod detect;
pub mod discover;
pub mod worktree;

use std::path::PathBuf;

/// What makes a directory a project of some type.
///
/// v1 had a `Func` variant too; it was only constructible from Haskell
/// code embedding nixon as a library, never from configuration, so v2 omits it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ProjectMarker {
    /// Exists, as either a file or a directory.
    Path(PathBuf),
    /// Exists and is a file.
    File(PathBuf),
    /// Exists and is a directory.
    Dir(PathBuf),
    /// Any of these match.
    Or(Vec<Self>),
}

/// A kind of project, and how to recognise one.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ProjectType {
    /// The name commands reference with `type="…"`.
    pub id: String,
    /// All markers must match; no markers means it always matches.
    pub markers: Vec<ProjectMarker>,
    /// Shown when inspecting a project.
    pub description: String,
}

/// A directory recognised as a project.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Project {
    /// The project directory's own name.
    pub name: PathBuf,
    /// The directory containing it.
    pub dir: PathBuf,
    /// Every type that matched.
    pub types: Vec<ProjectType>,
}
