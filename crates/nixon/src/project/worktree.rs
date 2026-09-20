//! Git worktree discovery. ENGINEERING §7.6.
//!
//! Worktrees live inside bare repositories or outside `project_dirs`, so
//! marker-based discovery misses them. They are enumerated by reading git's
//! own layout rather than by invoking `git`.

use std::path::{Path, PathBuf};

/// Whether `dir` is a git directory: a working tree or a bare repository.
/// ENGINEERING §7.6.
pub fn is_git_dir(dir: &Path) -> bool {
    dir.join(".git").exists() || is_bare_repo(dir)
}

/// Whether `dir` is a bare repository. ENGINEERING §7.6.
///
/// The layout alone is not enough — a `.git` directory has it too — so the
/// config must also say `bare = true`.
pub fn is_bare_repo(dir: &Path) -> bool {
    if !(dir.join("HEAD").is_file() && dir.join("objects").is_dir() && dir.join("refs").is_dir()) {
        return false;
    }
    std::fs::read_to_string(dir.join("config")).is_ok_and(|config| declares_bare(&config))
}

/// Whether a git config sets `bare = true`.
fn declares_bare(config: &str) -> bool {
    config.lines().any(|line| {
        let line = line.trim();
        let Some((key, value)) = line.split_once('=') else {
            return false;
        };
        key.trim() == "bare" && value.trim().eq_ignore_ascii_case("true")
    })
}

/// The git directory holding a repository's metadata. ENGINEERING §7.6.
///
/// `<dir>/.git` for a working tree, `<dir>` itself for a bare repository. A
/// `.git` *file* points elsewhere with a `gitdir:` line, which is how a
/// worktree refers back to the repository it belongs to.
pub fn git_dir(dir: &Path) -> Option<PathBuf> {
    let dot_git = dir.join(".git");
    if dot_git.is_dir() {
        return Some(dot_git);
    }
    if dot_git.is_file() {
        return read_gitdir_pointer(&dot_git);
    }
    is_bare_repo(dir).then(|| dir.to_path_buf())
}

/// Follows a `gitdir: <path>` pointer, resolving it against `file`'s parent.
fn read_gitdir_pointer(file: &Path) -> Option<PathBuf> {
    let contents = std::fs::read_to_string(file).ok()?;
    let target = contents
        .lines()
        .find_map(|line| line.trim().strip_prefix("gitdir:"))?
        .trim();

    let path = Path::new(target);
    let resolved = if path.is_absolute() {
        path.to_path_buf()
    } else {
        file.parent()?.join(path)
    };
    Some(resolved)
}

/// Every worktree registered with the repository at `dir`. ENGINEERING §7.6.
///
/// Reads `<gitdir>/worktrees/<name>/gitdir`, each of which holds the path of
/// that worktree's own `.git` file; its parent is the worktree root. Entries
/// whose root has gone are stale and skipped. The main working tree is not
/// included: ordinary discovery already finds it.
pub fn worktrees_of(dir: &Path) -> Vec<PathBuf> {
    let Some(git_dir) = git_dir(dir) else {
        return Vec::new();
    };
    let Ok(entries) = std::fs::read_dir(git_dir.join("worktrees")) else {
        return Vec::new();
    };

    let mut found: Vec<PathBuf> = entries
        .filter_map(Result::ok)
        .filter_map(|entry| worktree_root(&entry.path()))
        .filter(|root| root.is_dir())
        .collect();
    found.sort();
    found.dedup();
    found
}

/// The working directory an administrative `worktrees/<name>` entry points at.
fn worktree_root(entry: &Path) -> Option<PathBuf> {
    let pointer = std::fs::read_to_string(entry.join("gitdir")).ok()?;
    let dot_git = Path::new(pointer.trim());
    // The file named is the worktree's `.git`; the worktree is its parent.
    dot_git.parent().map(Path::to_path_buf)
}

#[cfg(test)]
mod tests {
    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{declares_bare, git_dir, is_bare_repo, is_git_dir, worktrees_of};

    /// Builds the layout git would, without running git.
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

    /// Registers `root` as a worktree of the repository whose gitdir is given.
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
        // The worktree's own .git is a file pointing back at the admin dir.
        root.child(".git")
            .write_str(&format!("gitdir: {}\n", admin.path().display()))
            .unwrap();
    }

    #[test]
    fn a_working_tree_is_a_git_dir() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        working_tree(&repo);
        assert!(is_git_dir(repo.path()));
        assert!(!is_bare_repo(repo.path()));
    }

    #[test]
    fn a_bare_repository_is_a_git_dir() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo.git");
        bare_repo(&repo);
        assert!(is_git_dir(repo.path()));
        assert!(is_bare_repo(repo.path()));
    }

    #[test]
    fn a_plain_directory_is_not_a_git_dir() {
        let temp = TempDir::new().unwrap();
        let plain = temp.child("plain");
        plain.create_dir_all().unwrap();
        assert!(!is_git_dir(plain.path()));
    }

    #[test]
    fn the_bare_layout_alone_is_not_enough() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        bare_repo(&repo);
        // A .git directory has the same layout but is not bare.
        repo.child("config")
            .write_str("[core]\n\tbare = false\n")
            .unwrap();
        assert!(!is_bare_repo(repo.path()));
    }

    #[test]
    fn bare_is_read_from_the_config_whatever_its_spacing() {
        assert!(declares_bare("[core]\n\tbare = true\n"));
        assert!(declares_bare("bare=true"));
        assert!(declares_bare("[core]\nbare = True\n"));
        assert!(!declares_bare("[core]\n\tbare = false\n"));
        assert!(!declares_bare("[core]\n\trepositoryformatversion = 0\n"));
    }

    #[test]
    fn the_git_dir_of_a_working_tree_is_its_dot_git() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        working_tree(&repo);
        assert_eq!(git_dir(repo.path()), Some(repo.child(".git").to_path_buf()));
    }

    #[test]
    fn the_git_dir_of_a_bare_repository_is_itself() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo.git");
        bare_repo(&repo);
        assert_eq!(git_dir(repo.path()), Some(repo.to_path_buf()));
    }

    #[test]
    fn a_dot_git_file_is_followed_to_where_it_points() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        working_tree(&repo);
        let tree = temp.child("tree");
        register_worktree(&repo.child(".git"), "tree", &tree);

        assert_eq!(
            git_dir(tree.path()),
            Some(repo.child(".git/worktrees/tree").to_path_buf())
        );
    }

    #[test]
    fn a_repository_lists_its_worktrees() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        working_tree(&repo);
        let one = temp.child("trees/one");
        let two = temp.child("trees/two");
        register_worktree(&repo.child(".git"), "one", &one);
        register_worktree(&repo.child(".git"), "two", &two);

        assert_eq!(
            worktrees_of(repo.path()),
            vec![one.to_path_buf(), two.to_path_buf()]
        );
    }

    #[test]
    fn a_bare_repository_lists_its_worktrees() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo.git");
        bare_repo(&repo);
        let tree = temp.child("trees/main");
        register_worktree(&repo, "main", &tree);

        assert_eq!(worktrees_of(repo.path()), vec![tree.to_path_buf()]);
    }

    #[test]
    fn a_stale_worktree_is_skipped() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        working_tree(&repo);
        let live = temp.child("trees/live");
        let gone = temp.child("trees/gone");
        register_worktree(&repo.child(".git"), "live", &live);
        register_worktree(&repo.child(".git"), "gone", &gone);
        std::fs::remove_dir_all(gone.path()).unwrap();

        assert_eq!(worktrees_of(repo.path()), vec![live.to_path_buf()]);
    }

    #[test]
    fn a_repository_without_worktrees_lists_none() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("repo");
        working_tree(&repo);
        assert!(worktrees_of(repo.path()).is_empty());
    }

    #[test]
    fn a_non_repository_lists_none() {
        let temp = TempDir::new().unwrap();
        let plain = temp.child("plain");
        plain.create_dir_all().unwrap();
        assert!(worktrees_of(plain.path()).is_empty());
    }
}
