//! Finding projects under the configured source directories. SPEC §9.6.

use std::path::{Path, PathBuf};

use super::detect::{find_project, sort_projects};
use super::{Project, ProjectType};

/// What `~` and `$VAR` expand to, passed in so discovery stays testable.
/// SPEC §9.6.
pub struct Expansion<'a> {
    /// The value `~` expands to.
    pub home: &'a Path,
    /// Lookup for `$VAR`; a miss expands to the empty string, as wordexp did.
    pub var: &'a dyn Fn(&str) -> Option<String>,
}

impl Expansion<'_> {
    /// Expands one `project_dirs` entry. A bad glob contributes nothing.
    ///
    /// An undefined `$VAR` expands to the empty string, as wordexp did
    /// without `WRDE_UNDEF`.
    pub fn expand(&self, path: &Path) -> Vec<PathBuf> {
        let raw = path.to_string_lossy();
        let expanded = shellexpand::full_with_context_no_errors(
            raw.as_ref(),
            || Some(self.home.to_string_lossy().into_owned()),
            |name| (self.var)(name),
        );
        glob::glob(expanded.as_ref()).map_or_else(
            |_| Vec::new(),
            |paths| paths.filter_map(Result::ok).collect(),
        )
    }
}

/// Every project at or below the source directories, to `max_depth`. SPEC §9.6.
///
/// A candidate that is itself a project is yielded **and** still has its
/// children scanned. SPEC §9.6 says the children are skipped in that case,
/// but v1 concatenated both branches unconditionally; this reproduces v1.
pub fn find_projects(
    max_depth: i64,
    ptypes: &[ProjectType],
    source_dirs: &[PathBuf],
    expansion: &Expansion<'_>,
) -> Vec<Project> {
    if max_depth < 0 {
        return Vec::new();
    }

    let mut found = Vec::new();
    for source in source_dirs {
        for candidate in expansion.expand(source) {
            if !candidate.is_dir() {
                continue;
            }
            if let Some(project) = find_project(ptypes, &candidate) {
                found.push(project);
            }
            found.extend(find_projects(
                max_depth - 1,
                ptypes,
                &children(&candidate),
                expansion,
            ));
        }
    }
    found
}

/// Discovery as the subcommands see it: depth 1, sorted by path, no dedupe.
/// SPEC §9.6.
pub fn get_sorted_projects(
    ptypes: &[ProjectType],
    source_dirs: &[PathBuf],
    expansion: &Expansion<'_>,
) -> Vec<Project> {
    let mut projects = find_projects(1, ptypes, source_dirs, expansion);
    sort_projects(&mut projects);
    projects
}

/// Immediate children of a directory, dotfiles included. SPEC §9.6.
fn children(dir: &Path) -> Vec<PathBuf> {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return Vec::new();
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok().map(|e| e.path())).collect();
    paths.sort();
    paths
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{Expansion, find_projects, get_sorted_projects};
    use crate::project::{ProjectMarker, ProjectType};

    fn marker_type(marker: &str) -> ProjectType {
        ProjectType {
            id: "marked".to_owned(),
            markers: vec![ProjectMarker::Path(PathBuf::from(marker))],
            description: "Marked project".to_owned(),
        }
    }

    fn no_vars(_: &str) -> Option<String> {
        None
    }

    fn expansion(home: &Path) -> Expansion<'_> {
        Expansion {
            home,
            var: &no_vars,
        }
    }

    /// Nothing above the temp dir can carry this marker.
    fn unique_marker(temp: &TempDir) -> String {
        format!(
            ".marker-{}",
            temp.path().file_name().unwrap().to_string_lossy()
        )
    }

    fn make_project(parent: &assert_fs::fixture::ChildPath, name: &str, marker: &str) -> PathBuf {
        let dir = parent.child(name);
        dir.create_dir_all().unwrap();
        dir.child(marker).touch().unwrap();
        dir.to_path_buf()
    }

    fn paths(projects: &[crate::project::Project]) -> Vec<PathBuf> {
        projects.iter().map(crate::project::Project::path).collect()
    }

    #[test]
    fn scans_children_of_a_source_dir() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let one = make_project(&src, "one", &marker);
        let two = make_project(&src, "two", &marker);
        src.child("not-a-project").create_dir_all().unwrap();

        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[src.to_path_buf()],
            &expansion(temp.path()),
        );
        assert_eq!(paths(&found), vec![one, two]);
    }

    #[test]
    fn a_source_dir_that_is_a_project_yields_itself_and_still_scans_children() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        src.child(&marker).touch().unwrap();
        let inner = make_project(&src, "inner", &marker);

        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[src.to_path_buf()],
            &expansion(temp.path()),
        );
        assert_eq!(paths(&found), vec![src.to_path_buf(), inner]);
    }

    #[test]
    fn grandchildren_are_never_scanned_at_depth_one() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let child = src.child("child");
        child.create_dir_all().unwrap();
        make_project(&child, "grandchild", &marker);

        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[src.to_path_buf()],
            &expansion(temp.path()),
        );
        assert!(found.is_empty());
    }

    #[test]
    fn a_negative_depth_finds_nothing() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        make_project(&temp.child("."), "proj", &marker);
        assert!(
            find_projects(
                -1,
                &[marker_type(&marker)],
                &[temp.to_path_buf()],
                &expansion(temp.path()),
            )
            .is_empty()
        );
    }

    #[test]
    fn hidden_directories_are_included() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let hidden = make_project(&src, ".hidden-project", &marker);

        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[src.to_path_buf()],
            &expansion(temp.path()),
        );
        assert_eq!(paths(&found), vec![hidden]);
    }

    #[test]
    fn results_are_sorted_by_path_without_dedupe() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let beta = make_project(&src, "beta", &marker);
        let alpha = make_project(&src, "alpha", &marker);

        // The same directory twice: v1 does not dedupe.
        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[src.to_path_buf(), src.to_path_buf()],
            &expansion(temp.path()),
        );
        assert_eq!(
            paths(&found),
            vec![alpha.clone(), alpha, beta.clone(), beta]
        );
    }

    #[test]
    fn a_tilde_expands_to_the_given_home() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let one = make_project(&src, "one", &marker);

        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[PathBuf::from("~/src")],
            &expansion(temp.path()),
        );
        assert_eq!(paths(&found), vec![one]);
    }

    #[test]
    fn a_variable_expands_from_the_given_lookup() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let one = make_project(&src, "one", &marker);

        let root = temp.path().to_string_lossy().into_owned();
        let lookup = move |name: &str| (name == "ROOT").then(|| root.clone());
        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[PathBuf::from("$ROOT/src")],
            &Expansion {
                home: temp.path(),
                var: &lookup,
            },
        );
        assert_eq!(paths(&found), vec![one]);
    }

    #[test]
    fn a_wildcard_expands_to_every_match() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let alpha = make_project(&src.child("a"), "alpha", &marker);
        let beta = make_project(&src.child("b"), "beta", &marker);

        let pattern = src.path().join("*");
        let found =
            get_sorted_projects(&[marker_type(&marker)], &[pattern], &expansion(temp.path()));
        assert_eq!(paths(&found), vec![alpha, beta]);
    }

    #[test]
    fn a_missing_source_dir_contributes_nothing() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[temp.path().join("nope")],
            &expansion(temp.path()),
        );
        assert!(found.is_empty());
    }
}
