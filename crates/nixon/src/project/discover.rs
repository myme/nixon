//! Finding projects under the configured source directories. SPEC §9.6.

use std::path::{Path, PathBuf};

use super::detect::{find_project, find_project_types, sort_projects};
use super::{Project, ProjectType, worktree};

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
    let roots: Vec<PathBuf> = source_dirs
        .iter()
        .flat_map(|source| expansion.expand(source))
        .collect();
    scan(max_depth, ptypes, &roots)
}

/// The recursion, over paths that are already real. SPEC §9.6.
///
/// Only `project_dirs` entries are patterns. Putting a `read_dir` result back
/// through the glob dropped any directory whose name contained a metacharacter
/// — `c++[wip]` read as a character class, `brack[et` as an unterminated
/// pattern — and re-expanded a `$VAR` in a literal name.
fn scan(max_depth: i64, ptypes: &[ProjectType], roots: &[PathBuf]) -> Vec<Project> {
    if max_depth < 0 {
        return Vec::new();
    }

    let mut found = Vec::new();
    for candidate in roots {
        if !candidate.is_dir() {
            continue;
        }
        if let Some(project) = find_project(ptypes, candidate) {
            found.push(project);
        }
        found.extend(scan(max_depth - 1, ptypes, &children(candidate)));
    }
    found
}

/// Discovery as the subcommands see it: depth 1, sorted by path. SPEC §9.6.
///
/// With `worktrees`, every git directory found also contributes its
/// worktrees, which live inside bare repositories or outside `project_dirs`
/// and would otherwise be missed. ENGINEERING §7.6.
pub fn get_sorted_projects(
    ptypes: &[ProjectType],
    source_dirs: &[PathBuf],
    expansion: &Expansion<'_>,
    worktrees: bool,
) -> Vec<Project> {
    let mut projects = find_projects(1, ptypes, source_dirs, expansion);
    if worktrees {
        projects.extend(find_worktrees(ptypes, source_dirs, expansion, &projects));
    }
    sort_projects(&mut projects);
    projects
}

/// The worktrees of every git directory at or below the source directories.
/// ENGINEERING §7.6.
///
/// Scanned independently of `found`, because a bare repository is not itself
/// a project the marker search would return, yet its worktrees are.
fn find_worktrees(
    ptypes: &[ProjectType],
    source_dirs: &[PathBuf],
    expansion: &Expansion<'_>,
    found: &[Project],
) -> Vec<Project> {
    let mut seen: std::collections::BTreeSet<PathBuf> = found.iter().map(Project::path).collect();

    let mut worktrees = Vec::new();
    for dir in git_dirs(source_dirs, expansion) {
        for root in worktree::worktrees_of(&dir) {
            // A worktree that is also under project_dirs is already there.
            if !seen.insert(root.clone()) {
                continue;
            }
            // Types come from the worktree's own directory: it has a `.git`
            // file, so `git` matches, and Cargo.toml and friends match as
            // usual.
            worktrees.push(Project::from_path(&root, find_project_types(&root, ptypes)));
        }
    }
    worktrees
}

/// Git directories at, or immediately below, the source directories.
///
/// The same depth-1 shape as project discovery, so a `~/src` holding bare
/// repositories is scanned without walking the whole tree.
fn git_dirs(source_dirs: &[PathBuf], expansion: &Expansion<'_>) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    for source in source_dirs {
        for candidate in expansion.expand(source) {
            if !candidate.is_dir() {
                continue;
            }
            if worktree::is_git_dir(&candidate) {
                dirs.push(candidate.clone());
            }
            dirs.extend(
                children(&candidate)
                    .into_iter()
                    .filter(|child| child.is_dir() && worktree::is_git_dir(child)),
            );
        }
    }
    dirs
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

    #[test]
    fn child_directories_are_not_re_expanded_as_glob_patterns() {
        let temp = TempDir::new().unwrap();
        let marker = unique_marker(&temp);
        let src = temp.child("src");
        src.create_dir_all().unwrap();

        for name in ["plain", "c++[wip]", "has$VAR", "brack[et"] {
            make_project(&src, name, &marker);
        }

        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[src.to_path_buf()],
            &Expansion {
                home: temp.path(),
                // A real lookup, as the subcommands pass: `has$VAR` must
                // survive that too.
                var: &|_| Some("expanded".to_owned()),
            },
            false,
        );
        let names: Vec<String> = found
            .iter()
            .map(|p| p.name.to_string_lossy().into_owned())
            .collect();

        assert_eq!(
            names,
            ["brack[et", "c++[wip]", "has$VAR", "plain"],
            "child project dirs were dropped"
        );
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

    /// Builds a working tree's `.git` layout, without running git.
    fn working_tree(at: &assert_fs::fixture::ChildPath) {
        at.create_dir_all().unwrap();
        at.child(".git/HEAD")
            .write_str("ref: refs/heads/main\n")
            .unwrap();
        at.child(".git/objects").create_dir_all().unwrap();
        at.child(".git/refs").create_dir_all().unwrap();
        at.child(".git/config")
            .write_str("[core]\n\tbare = false\n")
            .unwrap();
    }

    fn bare_repo(at: &assert_fs::fixture::ChildPath) {
        at.create_dir_all().unwrap();
        at.child("HEAD")
            .write_str("ref: refs/heads/main\n")
            .unwrap();
        at.child("objects").create_dir_all().unwrap();
        at.child("refs").create_dir_all().unwrap();
        at.child("config")
            .write_str("[core]\n\tbare = true\n")
            .unwrap();
    }

    fn register_worktree(
        git_dir: &assert_fs::fixture::ChildPath,
        name: &str,
        root: &assert_fs::fixture::ChildPath,
    ) {
        root.create_dir_all().unwrap();
        let admin = git_dir.child(format!("worktrees/{name}"));
        admin.create_dir_all().unwrap();
        admin
            .child("gitdir")
            .write_str(&format!("{}\n", root.child(".git").path().display()))
            .unwrap();
        root.child(".git")
            .write_str(&format!("gitdir: {}\n", admin.path().display()))
            .unwrap();
    }

    fn git_type() -> ProjectType {
        ProjectType {
            id: "git".to_owned(),
            markers: vec![ProjectMarker::Path(PathBuf::from(".git"))],
            description: "Git".to_owned(),
        }
    }

    #[test]
    fn worktrees_are_found_even_outside_the_source_dirs() {
        let temp = TempDir::new().unwrap();
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let repo = src.child("repo");
        working_tree(&repo);

        // Deliberately outside src/, where marker discovery never looks.
        let tree = temp.child("elsewhere/feature");
        register_worktree(&repo.child(".git"), "feature", &tree);

        let found = get_sorted_projects(
            &[git_type()],
            &[src.to_path_buf()],
            &expansion(temp.path()),
            true,
        );
        // Sorted by path, so elsewhere/ comes before src/.
        assert_eq!(paths(&found), vec![tree.to_path_buf(), repo.to_path_buf()]);
    }

    #[test]
    fn a_bare_repository_contributes_its_worktrees() {
        let temp = TempDir::new().unwrap();
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let repo = src.child("repo.git");
        bare_repo(&repo);
        let tree = temp.child("trees/main");
        register_worktree(&repo, "main", &tree);

        let found = get_sorted_projects(
            &[git_type()],
            &[src.to_path_buf()],
            &expansion(temp.path()),
            true,
        );
        // The bare repo is itself a git project, and its worktree comes too.
        assert_eq!(paths(&found), vec![repo.to_path_buf(), tree.to_path_buf()]);
    }

    #[test]
    fn a_worktree_carries_the_types_of_its_own_directory() {
        let temp = TempDir::new().unwrap();
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let repo = src.child("repo");
        working_tree(&repo);
        let tree = temp.child("trees/feature");
        register_worktree(&repo.child(".git"), "feature", &tree);
        tree.child("Cargo.toml").write_str("[package]\n").unwrap();

        let cargo = ProjectType {
            id: "cargo".to_owned(),
            markers: vec![ProjectMarker::Path(PathBuf::from("Cargo.toml"))],
            description: "Cargo".to_owned(),
        };
        let found = get_sorted_projects(
            &[git_type(), cargo],
            &[src.to_path_buf()],
            &expansion(temp.path()),
            true,
        );
        let worktree = found
            .iter()
            .find(|p| p.path() == tree.to_path_buf())
            .unwrap();
        let ids: Vec<&str> = worktree.types.iter().map(|t| t.id.as_str()).collect();
        assert_eq!(ids, ["git", "cargo"]);
    }

    #[test]
    fn a_stale_worktree_is_not_offered() {
        let temp = TempDir::new().unwrap();
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let repo = src.child("repo");
        working_tree(&repo);
        let gone = temp.child("trees/gone");
        register_worktree(&repo.child(".git"), "gone", &gone);
        std::fs::remove_dir_all(gone.path()).unwrap();

        let found = get_sorted_projects(
            &[git_type()],
            &[src.to_path_buf()],
            &expansion(temp.path()),
            true,
        );
        assert_eq!(paths(&found), vec![repo.to_path_buf()]);
    }

    #[test]
    fn a_worktree_already_under_the_source_dirs_is_not_duplicated() {
        let temp = TempDir::new().unwrap();
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let repo = src.child("repo");
        working_tree(&repo);
        // The worktree sits beside the repo, so discovery finds it too.
        let tree = src.child("feature");
        register_worktree(&repo.child(".git"), "feature", &tree);

        let found = get_sorted_projects(
            &[git_type()],
            &[src.to_path_buf()],
            &expansion(temp.path()),
            true,
        );
        assert_eq!(paths(&found), vec![tree.to_path_buf(), repo.to_path_buf()]);
    }

    #[test]
    fn worktree_discovery_can_be_turned_off() {
        let temp = TempDir::new().unwrap();
        let src = temp.child("src");
        src.create_dir_all().unwrap();
        let repo = src.child("repo");
        working_tree(&repo);
        let tree = temp.child("elsewhere/feature");
        register_worktree(&repo.child(".git"), "feature", &tree);

        let found = get_sorted_projects(
            &[git_type()],
            &[src.to_path_buf()],
            &expansion(temp.path()),
            false,
        );
        assert_eq!(paths(&found), vec![repo.to_path_buf()]);
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
            true,
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
            true,
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
            true,
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
            true,
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
            true,
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
            true,
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
            true,
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
        let found = get_sorted_projects(
            &[marker_type(&marker)],
            &[pattern],
            &expansion(temp.path()),
            true,
        );
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
            true,
        );
        assert!(found.is_empty());
    }
}
