//! Path lookups and the directories nixon reads. SPEC §3.1, §7.2, §9.8.

use std::path::{Path, PathBuf};

/// The directories nixon reads, resolved once so nothing below reads the
/// environment. SPEC §3.1, §7.2.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dirs {
    /// The user's home, used to shorten paths for display.
    pub home: PathBuf,
    /// `$XDG_CONFIG_HOME`.
    pub config: PathBuf,
    /// `$XDG_CACHE_HOME`.
    pub cache: PathBuf,
}

impl Dirs {
    /// Resolves the XDG directories, on macOS too, as v1 did.
    pub fn from_env() -> Result<Self, etcetera::HomeDirError> {
        use etcetera::base_strategy::{BaseStrategy as _, Xdg};

        let xdg = Xdg::new()?;
        Ok(Self {
            home: xdg.home_dir().to_path_buf(),
            config: xdg.config_dir(),
            cache: xdg.cache_dir(),
        })
    }

    /// The global config file, `$XDG_CONFIG_HOME/nixon.md`. SPEC §3.1.
    pub fn global_config(&self) -> PathBuf {
        self.config.join("nixon.md")
    }

    /// The script cache, `$XDG_CACHE_HOME/nixon`. SPEC §7.2.
    pub fn cache_dir(&self) -> PathBuf {
        self.cache.join("nixon")
    }
}

/// Finds `name` in `start` or the nearest ancestor holding it. SPEC §3.1.
///
/// The filesystem root is never itself tested, matching v1's loop condition.
pub fn find_dominating_file(start: &Path, name: &str) -> Option<PathBuf> {
    let mut dir = start;
    loop {
        let candidate = dir.join(name);
        if dir.is_dir() && candidate.exists() {
            return Some(candidate);
        }
        let parent = dir.parent()?;
        parent.parent()?;
        dir = parent;
    }
}

/// Replaces a leading `$HOME/` with `~/`. SPEC §9.8.
///
/// A path equal to `$HOME` is left alone: v1 stripped `$HOME/`, with the
/// separator, so the home directory itself never matched.
pub fn implode_home(path: &Path, home: &Path) -> PathBuf {
    path.strip_prefix(home)
        .ok()
        .filter(|rest| !rest.as_os_str().is_empty())
        .map_or_else(|| path.to_path_buf(), |rest| Path::new("~").join(rest))
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;
    use rstest::rstest;

    use super::{Dirs, find_dominating_file, implode_home};

    #[rstest]
    #[case("/home/me/code/nixon", "~/code/nixon")]
    #[case("/home/me/x", "~/x")]
    #[case("/home/me", "/home/me")]
    #[case("/home/meow/x", "/home/meow/x")]
    #[case("/etc/passwd", "/etc/passwd")]
    fn implode_home_replaces_only_an_exact_home_prefix(#[case] path: &str, #[case] expected: &str) {
        assert_eq!(
            implode_home(Path::new(path), Path::new("/home/me")),
            PathBuf::from(expected)
        );
    }

    #[test]
    fn finds_a_file_in_the_starting_directory() {
        let temp = TempDir::new().unwrap();
        temp.child("nixon.md").touch().unwrap();
        assert_eq!(
            find_dominating_file(temp.path(), "nixon.md"),
            Some(temp.child("nixon.md").to_path_buf())
        );
    }

    #[test]
    fn walks_up_to_find_the_file() {
        let temp = TempDir::new().unwrap();
        temp.child("nixon.md").touch().unwrap();
        let deep = temp.child("a/b/c");
        deep.create_dir_all().unwrap();
        assert_eq!(
            find_dominating_file(deep.path(), "nixon.md"),
            Some(temp.child("nixon.md").to_path_buf())
        );
    }

    #[test]
    fn finds_the_nearest_of_several() {
        let temp = TempDir::new().unwrap();
        temp.child("nixon.md").touch().unwrap();
        let nearer = temp.child("a/b");
        nearer.create_dir_all().unwrap();
        nearer.child("nixon.md").touch().unwrap();
        let deep = temp.child("a/b/c");
        deep.create_dir_all().unwrap();
        assert_eq!(
            find_dominating_file(deep.path(), "nixon.md"),
            Some(nearer.child("nixon.md").to_path_buf())
        );
    }

    #[test]
    fn a_directory_counts_as_a_match() {
        let temp = TempDir::new().unwrap();
        temp.child(".git").create_dir_all().unwrap();
        let deep = temp.child("src");
        deep.create_dir_all().unwrap();
        assert_eq!(
            find_dominating_file(deep.path(), ".git"),
            Some(temp.child(".git").to_path_buf())
        );
    }

    #[test]
    fn returns_none_when_nothing_matches() {
        let temp = TempDir::new().unwrap();
        let deep = temp.child("a/b");
        deep.create_dir_all().unwrap();
        assert_eq!(find_dominating_file(deep.path(), "nixon.md"), None);
    }

    #[test]
    fn dirs_place_the_config_and_cache_under_their_xdg_roots() {
        let dirs = Dirs {
            home: PathBuf::from("/home/me"),
            config: PathBuf::from("/home/me/.config"),
            cache: PathBuf::from("/home/me/.cache"),
        };
        assert_eq!(
            dirs.global_config(),
            PathBuf::from("/home/me/.config/nixon.md")
        );
        assert_eq!(dirs.cache_dir(), PathBuf::from("/home/me/.cache/nixon"));
    }
}
