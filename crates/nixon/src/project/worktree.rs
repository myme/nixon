//! Git worktree discovery.
//!
//! Worktrees live inside bare repositories or outside `project_dirs`, so
//! marker-based discovery misses them. They are enumerated by reading git's
//! own layout rather than by invoking `git`.

use std::path::{Path, PathBuf};

/// Whether `dir` is a git directory.
///
/// A working tree, a bare repository, or a container: a directory that keeps
/// its bare repository in a dot-subdirectory and its worktrees beside it.
pub fn is_git_dir(dir: &Path) -> bool {
    dir.join(".git").exists() || is_bare_repo(dir) || nested_bare_repo(dir).is_some()
}

/// A bare repository kept in a direct dot-subdirectory of `dir`.
///
/// The `.bare` layout: `~/code/gaia/.bare` is the repository and
/// `~/code/gaia/bugs` is a worktree of it, so the container has no `.git` of
/// its own and marker-based discovery walks straight past it. Recognised by
/// the repository layout rather than by the name, which is only a
/// convention; the first in sorted order wins if there is more than one.
pub fn nested_bare_repo(dir: &Path) -> Option<PathBuf> {
    let mut found: Vec<PathBuf> = std::fs::read_dir(dir)
        .ok()?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| is_hidden(path) && is_bare_repo(path))
        .collect();
    found.sort();
    found.into_iter().next()
}

/// Whether a path's own name starts with a dot.
fn is_hidden(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.starts_with('.'))
}

/// Whether `dir` is a bare repository.
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

/// The git directory holding a repository's metadata.
///
/// `<dir>/.git` for a working tree, `<dir>` itself for a bare repository,
/// and the nested repository for a container. A `.git` *file* points
/// elsewhere with a `gitdir:` line, which is how a worktree refers back to
/// the repository it belongs to and how a container may point at its own
/// `.bare`.
pub fn git_dir(dir: &Path) -> Option<PathBuf> {
    let dot_git = dir.join(".git");
    if dot_git.is_dir() {
        return Some(dot_git);
    }
    if dot_git.is_file() {
        return read_gitdir_pointer(&dot_git);
    }
    if is_bare_repo(dir) {
        return Some(dir.to_path_buf());
    }
    nested_bare_repo(dir)
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

/// Every checkout of the repository at `dir`.
///
/// Reads `<common>/worktrees/<name>/gitdir`, each of which holds the path of
/// that worktree's own `.git` file; its parent is the worktree root. Entries
/// whose root has gone are stale and skipped.
///
/// The main working tree is included too. Ordinary discovery finds it when
/// it is under `project_dirs`, but entering through a linked worktree is
/// exactly the case where it is not, and a checkout should not be reachable
/// in one direction only. Duplicates are dropped here and again upstream.
pub fn worktrees_of(dir: &Path) -> Vec<PathBuf> {
    let Some(git_dir) = git_dir(dir) else {
        return Vec::new();
    };
    let common = common_dir(&git_dir);
    let Ok(entries) = std::fs::read_dir(common.join("worktrees")) else {
        return Vec::new();
    };

    let mut found: Vec<PathBuf> = entries
        .filter_map(Result::ok)
        .filter_map(|entry| worktree_root(&entry.path()))
        .chain(main_worktree(&common))
        .filter(|root| root.is_dir())
        .collect();
    found.sort();
    found.dedup();
    found
}

/// The working tree the common git directory belongs to, if it has one.
///
/// `<root>/.git` is a directory in a normal clone; a bare repository has no
/// working tree, and its own directory is not one.
fn main_worktree(common: &Path) -> Option<PathBuf> {
    if is_bare_repo(common) {
        return None;
    }
    let root = common.parent()?;
    root.join(".git").is_dir().then(|| root.to_path_buf())
}

/// The repository's shared git directory.
///
/// A linked worktree's `.git` resolves to `<common>/worktrees/<name>`, which
/// belongs to that worktree alone; the registry of them all lives under the
/// common directory, which `commondir` names relative to the private one.
fn common_dir(git_dir: &Path) -> PathBuf {
    let Ok(contents) = std::fs::read_to_string(git_dir.join("commondir")) else {
        return git_dir.to_path_buf();
    };
    let target = Path::new(contents.trim());
    if target.is_absolute() {
        target.to_path_buf()
    } else {
        lexically_clean(&git_dir.join(target))
    }
}

/// Resolves `.` and `..` textually, without touching the filesystem.
///
/// `commondir` is relative, so joining it leaves `..` components behind, and
/// `Path::parent` would then strip one of those rather than climb — the
/// parent of `a/b/..` is `a/b`, not `a`. Canonicalising instead would also
/// resolve symlinks, and these paths are compared against ones that have
/// not been.
fn lexically_clean(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for part in path.components() {
        match part {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if !out.pop() {
                    out.push("..");
                }
            }
            other => out.push(other),
        }
    }
    out
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

    use super::{
        Path, PathBuf, declares_bare, git_dir, is_bare_repo, is_git_dir, nested_bare_repo,
        worktrees_of,
    };

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
        // As git writes it: where the shared git directory is from here.
        admin.child("commondir").write_str("../..\n").unwrap();
    }

    /// Runs git, failing loudly: a silent skip would hide the defect.
    fn git(args: &[&str]) {
        let status = std::process::Command::new("git")
            .args(args)
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .env("GIT_AUTHOR_NAME", "t")
            .env("GIT_AUTHOR_EMAIL", "t@example.invalid")
            .env("GIT_COMMITTER_NAME", "t")
            .env("GIT_COMMITTER_EMAIL", "t@example.invalid")
            .status()
            .unwrap();
        assert!(status.success(), "git {args:?} failed");
    }

    /// A linked worktree knows about its siblings.
    ///
    /// Its `.git` points at `<main>/.git/worktrees/<name>`, which is private
    /// to it; the shared `worktrees` directory is one level up, where
    /// `commondir` says. Appending `worktrees` to the private directory
    /// searched somewhere that does not exist.
    #[test]
    fn a_linked_worktree_finds_the_others() {
        let temp = TempDir::new().unwrap();
        let main = temp.child("main");
        main.create_dir_all().unwrap();
        let at = main.path().to_string_lossy().into_owned();
        git(&["init", "-q", "-b", "main", &at]);
        git(&["-C", &at, "commit", "-q", "--allow-empty", "-m", "root"]);

        let first = temp.child("roots/first");
        let outside = temp.child("outside");
        git(&[
            "-C",
            &at,
            "worktree",
            "add",
            "-q",
            &first.path().to_string_lossy(),
        ]);
        git(&[
            "-C",
            &at,
            "worktree",
            "add",
            "-q",
            &outside.path().to_string_lossy(),
        ]);

        let canonical = |path: &Path| std::fs::canonicalize(path).unwrap();
        let mut found: Vec<PathBuf> = worktrees_of(first.path())
            .iter()
            .map(|r| canonical(r))
            .collect();
        found.sort();
        let mut want = vec![
            canonical(main.path()),
            canonical(first.path()),
            canonical(outside.path()),
        ];
        want.sort();
        assert_eq!(found, want, "entering through a linked worktree");
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

    /// The `.bare` container layout, as `git init --bare .bare` plus
    /// `git -C .bare worktree add ../bugs` leaves it on disk.
    fn container(at: &assert_fs::fixture::ChildPath, worktrees: &[&str]) {
        at.create_dir_all().unwrap();
        let repo = at.child(".bare");
        bare_repo(&repo);
        for name in worktrees {
            register_worktree(&repo, name, &at.child(name));
        }
    }

    #[test]
    fn a_container_holding_a_bare_repo_is_a_git_dir() {
        let temp = TempDir::new().unwrap();
        let gaia = temp.child("gaia");
        container(&gaia, &["bugs"]);

        // No .git of its own, and not bare itself.
        assert!(!gaia.child(".git").path().exists());
        assert!(!is_bare_repo(gaia.path()));
        assert!(is_git_dir(gaia.path()));
        assert_eq!(
            git_dir(gaia.path()),
            Some(gaia.child(".bare").to_path_buf())
        );
    }

    #[test]
    fn a_containers_worktrees_are_found() {
        let temp = TempDir::new().unwrap();
        let gaia = temp.child("gaia");
        container(&gaia, &["bugs", "claims"]);

        assert_eq!(
            worktrees_of(gaia.path()),
            [
                gaia.child("bugs").to_path_buf(),
                gaia.child("claims").to_path_buf()
            ]
        );
    }

    /// Some containers keep a `.git` file at the root pointing at `.bare`.
    #[test]
    fn a_container_with_a_git_file_at_its_root_works_the_same_way() {
        let temp = TempDir::new().unwrap();
        let atlas = temp.child("atlas");
        container(&atlas, &["bugs"]);
        atlas
            .child(".git")
            .write_str(
                "gitdir: ./.bare
",
            )
            .unwrap();

        assert!(is_git_dir(atlas.path()));
        assert_eq!(
            worktrees_of(atlas.path()),
            [atlas.child("bugs").to_path_buf()]
        );
    }

    /// A dot-subdirectory that is not a repository proves nothing.
    #[test]
    fn a_hidden_directory_that_is_not_bare_does_not_make_a_container() {
        let temp = TempDir::new().unwrap();
        let plain = temp.child("plain");
        plain.child(".cache/objects").create_dir_all().unwrap();
        assert!(!is_git_dir(plain.path()));
        assert_eq!(nested_bare_repo(plain.path()), None);
    }

    /// The name is a convention; the layout is what is checked.
    #[test]
    fn the_nested_repository_is_found_under_any_dot_name() {
        let temp = TempDir::new().unwrap();
        let repo = temp.child("project");
        repo.create_dir_all().unwrap();
        bare_repo(&repo.child(".git-store"));

        assert_eq!(
            nested_bare_repo(repo.path()),
            Some(repo.child(".git-store").to_path_buf())
        );
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

        // The main working tree is a checkout like the others.
        assert_eq!(
            worktrees_of(repo.path()),
            vec![repo.to_path_buf(), one.to_path_buf(), two.to_path_buf()]
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

        assert_eq!(
            worktrees_of(repo.path()),
            vec![repo.to_path_buf(), live.to_path_buf()]
        );
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
