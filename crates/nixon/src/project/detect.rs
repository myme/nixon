//! Recognising projects on disk. SPEC §9.2-§9.5.

use std::path::{Path, PathBuf};

use super::{Project, ProjectMarker, ProjectType};

/// Every type whose markers all match. SPEC §9.2.
///
/// A type with no markers always matches; a non-directory has no types.
pub fn find_project_types(path: &Path, ptypes: &[ProjectType]) -> Vec<ProjectType> {
    if !path.is_dir() {
        return Vec::new();
    }
    ptypes
        .iter()
        .filter(|ptype| ptype.markers.iter().all(|marker| test_marker(path, marker)))
        .cloned()
        .collect()
}

/// Whether one marker holds for a directory. SPEC §9.2.
pub fn test_marker(path: &Path, marker: &ProjectMarker) -> bool {
    match marker {
        ProjectMarker::Path(p) => path.join(p).exists(),
        ProjectMarker::File(p) => path.join(p).is_file(),
        ProjectMarker::Dir(p) => path.join(p).is_dir(),
        ProjectMarker::Or(markers) => markers.iter().any(|m| test_marker(path, m)),
    }
}

/// The project rooted exactly at `dir`, if it is one. SPEC §9.3.
///
/// A directory matched only by marker-less catch-all types is not a project,
/// but the catch-all types are still recorded on one that is.
pub fn find_project(ptypes: &[ProjectType], dir: &Path) -> Option<Project> {
    if !dir.is_dir() {
        return None;
    }
    let types = find_project_types(dir, ptypes);
    if types.iter().all(|ptype| ptype.markers.is_empty()) {
        return None;
    }
    Some(Project::from_path(dir, types))
}

/// The nearest project at or above `path`. SPEC §9.4.
///
/// The returned project is rooted at the directory whose markers matched.
/// v1 returned that directory's *parent* with an empty name whenever the
/// search started below the root; SPEC §9.4 records that as a bug to fix.
pub fn find_in_project(ptypes: &[ProjectType], path: &Path) -> Option<Project> {
    let mut dir = path;
    loop {
        if let Some(project) = find_project(ptypes, dir) {
            return Some(project);
        }
        let parent = dir.parent()?;
        parent.parent()?;
        dir = parent;
    }
}

/// The nearest project, or `path` itself as a project. SPEC §9.5.
///
/// Commands with no type restriction still work outside any project.
pub fn find_in_project_or_default(ptypes: &[ProjectType], path: &Path) -> Project {
    find_in_project(ptypes, path)
        .unwrap_or_else(|| Project::from_path(path, find_project_types(path, ptypes)))
}

/// Sorts projects by full path. SPEC §9.6.
pub fn sort_projects(projects: &mut [Project]) {
    projects.sort_by_key(Project::path);
}

/// Renders projects for `project --inspect`. SPEC §9.7.
pub fn inspect(projects: &[Project]) -> String {
    projects
        .iter()
        .map(|project| {
            let types: Vec<&str> = project.types.iter().map(|t| t.id.as_str()).collect();
            format!(
                "Name: {}\nPath: {}\nTypes: {}\n",
                project.name.display(),
                project.path().display(),
                types.join(", ")
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// The path a project lives at, as SPEC §9.1 defines it.
impl Project {
    /// Builds a project rooted at `path`.
    pub fn from_path(path: &Path, types: Vec<ProjectType>) -> Self {
        Self {
            name: path.file_name().map(PathBuf::from).unwrap_or_default(),
            dir: path.parent().map(Path::to_path_buf).unwrap_or_default(),
            types,
        }
    }

    /// `dir / name`. SPEC §9.1.
    pub fn path(&self) -> PathBuf {
        self.dir.join(&self.name)
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{
        find_in_project, find_in_project_or_default, find_project, find_project_types, inspect,
        sort_projects, test_marker,
    };
    use crate::project::{Project, ProjectMarker, ProjectType};

    fn ptype(id: &str, markers: &[&str]) -> ProjectType {
        ProjectType {
            id: id.to_owned(),
            markers: markers
                .iter()
                .map(|m| ProjectMarker::Path(PathBuf::from(m)))
                .collect(),
            description: format!("{id} project"),
        }
    }

    fn catch_all() -> ProjectType {
        ProjectType {
            id: "any".to_owned(),
            markers: Vec::new(),
            description: "Generic project".to_owned(),
        }
    }

    /// A marker name nothing above the temp dir can accidentally have: the
    /// search walks to the filesystem root, and this host has a stray
    /// /tmp/.git.
    fn unique_marker(temp: &TempDir) -> String {
        format!(
            ".marker-{}",
            temp.path().file_name().unwrap().to_string_lossy()
        )
    }

    fn ids(project: &Project) -> Vec<&str> {
        project.types.iter().map(|t| t.id.as_str()).collect()
    }

    #[test]
    fn a_type_with_no_markers_matches_any_directory() {
        let temp = TempDir::new().unwrap();
        let types = find_project_types(temp.path(), &[catch_all()]);
        assert_eq!(types.len(), 1);
    }

    #[test]
    fn a_catch_all_alone_does_not_make_a_directory_a_project() {
        let temp = TempDir::new().unwrap();
        assert_eq!(find_project(&[catch_all()], temp.path()), None);
    }

    #[test]
    fn a_marker_bearing_type_makes_a_directory_a_project_and_keeps_the_catch_all() {
        let temp = TempDir::new().unwrap();
        temp.child(".git").create_dir_all().unwrap();
        let project = find_project(&[catch_all(), ptype("git", &[".git"])], temp.path()).unwrap();
        assert_eq!(ids(&project), ["any", "git"]);
    }

    #[test]
    fn all_markers_must_match() {
        let temp = TempDir::new().unwrap();
        temp.child(".git").create_dir_all().unwrap();
        assert!(
            find_project_types(temp.path(), &[ptype("both", &[".git", "Cargo.toml"])]).is_empty()
        );
        temp.child("Cargo.toml").touch().unwrap();
        assert_eq!(
            find_project_types(temp.path(), &[ptype("both", &[".git", "Cargo.toml"])]).len(),
            1
        );
    }

    #[test]
    fn a_non_directory_has_no_types() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("a-file");
        file.touch().unwrap();
        assert!(find_project_types(file.path(), &[catch_all()]).is_empty());
        assert_eq!(find_project(&[catch_all()], file.path()), None);
    }

    #[test]
    fn markers_test_files_directories_and_alternatives() {
        let temp = TempDir::new().unwrap();
        temp.child("a-file").touch().unwrap();
        temp.child("a-dir").create_dir_all().unwrap();

        assert!(test_marker(
            temp.path(),
            &ProjectMarker::Path("a-file".into())
        ));
        assert!(test_marker(
            temp.path(),
            &ProjectMarker::Path("a-dir".into())
        ));
        assert!(test_marker(
            temp.path(),
            &ProjectMarker::File("a-file".into())
        ));
        assert!(!test_marker(
            temp.path(),
            &ProjectMarker::File("a-dir".into())
        ));
        assert!(test_marker(
            temp.path(),
            &ProjectMarker::Dir("a-dir".into())
        ));
        assert!(!test_marker(
            temp.path(),
            &ProjectMarker::Dir("a-file".into())
        ));
        assert!(test_marker(
            temp.path(),
            &ProjectMarker::Or(vec![
                ProjectMarker::Path("nope".into()),
                ProjectMarker::Path("a-file".into()),
            ])
        ));
        assert!(!test_marker(
            temp.path(),
            &ProjectMarker::Or(vec![ProjectMarker::Path("nope".into())])
        ));
    }

    #[test]
    fn types_come_back_in_config_order() {
        let temp = TempDir::new().unwrap();
        temp.child(".git").create_dir_all().unwrap();
        temp.child("Cargo.toml").touch().unwrap();
        let project = find_project(
            &[ptype("cargo", &["Cargo.toml"]), ptype("git", &[".git"])],
            temp.path(),
        )
        .unwrap();
        assert_eq!(ids(&project), ["cargo", "git"]);
    }

    #[test]
    fn discovery_from_a_subdirectory_returns_the_project_root() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let proj = temp.child("proj");
        proj.create_dir_all().unwrap();
        proj.child(&marker).create_dir_all().unwrap();
        let deeper = proj.child("sub/deeper");
        deeper.create_dir_all().unwrap();

        let project = find_in_project(&[ptype("git", &[&marker])], deeper.path()).unwrap();
        assert_eq!(project.name, PathBuf::from("proj"));
        assert_eq!(project.dir, temp.path());
        assert_eq!(project.path(), proj.to_path_buf());
    }

    #[test]
    fn discovery_from_the_project_root_returns_the_same_project() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let proj = temp.child("proj");
        proj.create_dir_all().unwrap();
        proj.child(&marker).create_dir_all().unwrap();

        let from_root = find_in_project(&[ptype("git", &[&marker])], proj.path()).unwrap();
        let from_below = {
            let sub = proj.child("sub");
            sub.create_dir_all().unwrap();
            find_in_project(&[ptype("git", &[&marker])], sub.path()).unwrap()
        };
        assert_eq!(from_root, from_below);
    }

    #[test]
    fn the_nearest_enclosing_project_wins() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let outer = temp.child("outer");
        outer.create_dir_all().unwrap();
        outer.child(&marker).create_dir_all().unwrap();
        let inner = outer.child("inner");
        inner.create_dir_all().unwrap();
        inner.child(&marker).create_dir_all().unwrap();

        let project = find_in_project(&[ptype("git", &[&marker])], inner.path()).unwrap();
        assert_eq!(project.path(), inner.to_path_buf());
    }

    #[test]
    fn outside_any_project_the_directory_itself_is_used() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let plain = temp.child("plain");
        plain.create_dir_all().unwrap();

        assert_eq!(
            find_in_project(&[ptype("git", &[&marker])], plain.path()),
            None
        );

        let project =
            find_in_project_or_default(&[catch_all(), ptype("git", &[&marker])], plain.path());
        assert_eq!(project.name, PathBuf::from("plain"));
        assert_eq!(project.path(), plain.to_path_buf());
        assert_eq!(ids(&project), ["any"]);
    }

    #[test]
    fn projects_sort_by_full_path() {
        let mut projects = [
            Project::from_path(Path::new("/src/b"), vec![]),
            Project::from_path(Path::new("/src/a"), vec![]),
            Project::from_path(Path::new("/opt/c"), vec![]),
        ];
        sort_projects(&mut projects);
        let paths: Vec<_> = projects.iter().map(Project::path).collect();
        assert_eq!(
            paths,
            ["/opt/c", "/src/a", "/src/b"].map(PathBuf::from).to_vec()
        );
    }

    #[test]
    fn inspect_prints_name_path_and_types() {
        let project = Project::from_path(
            Path::new("/home/me/code/nixon"),
            vec![ptype("git", &[".git"]), ptype("cargo", &["Cargo.toml"])],
        );
        assert_eq!(
            inspect(&[project]),
            "Name: nixon\nPath: /home/me/code/nixon\nTypes: git, cargo\n"
        );
    }

    #[test]
    fn inspect_separates_projects_with_a_blank_line() {
        let projects = [
            Project::from_path(Path::new("/a/one"), vec![]),
            Project::from_path(Path::new("/a/two"), vec![]),
        ];
        assert_eq!(
            inspect(&projects),
            "Name: one\nPath: /a/one\nTypes: \n\nName: two\nPath: /a/two\nTypes: \n"
        );
    }
}
