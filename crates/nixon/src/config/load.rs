//! Locating and reading config files.

use std::path::{Path, PathBuf};

use super::{Config, ConfigError};
use crate::fs::find_dominating_file;
use crate::markdown;

/// The local config filenames, in the order v1 searched for them.
const LOCAL_NAMES: [&str; 2] = ["nixon.md", ".nixon.md"];

/// Reads one config file.
pub fn read(path: &Path) -> Result<Config, ConfigError> {
    let text = std::fs::read_to_string(path).map_err(|err| match err.kind() {
        std::io::ErrorKind::NotFound => ConfigError::NoSuchFile,
        _ => ConfigError::ParseError(err.to_string()),
    })?;
    if text.trim_start().is_empty() {
        return Err(ConfigError::EmptyFile);
    }
    markdown::parse_config_file(&path.to_string_lossy(), &text).map_err(ConfigError::Markdown)
}

/// Reads the global config, tolerating its absence.
///
/// v1 made a missing or empty global config a fatal error; v2 treats both as
/// an empty config. Parse errors stay fatal.
pub fn load_global(path: &Path) -> Result<Config, ConfigError> {
    match read(path) {
        Ok(config) => Ok(config),
        Err(ConfigError::NoSuchFile | ConfigError::EmptyFile) => Ok(Config::default()),
        Err(err) => Err(err),
    }
}

/// The local config file for a project, if any.
///
/// v1 searched the whole ancestor chain for `nixon.md` before trying
/// `.nixon.md` at all, so a `nixon.md` further up wins over a `.nixon.md`
/// closer to the project. It reads like a per-directory choice and is not:
/// v1's `firstOf (find_dominating_file path) [...]` searches per filename.
pub fn find_local_file(start: &Path) -> Option<PathBuf> {
    LOCAL_NAMES
        .iter()
        .find_map(|name| find_dominating_file(start, name))
}

/// Loads the local config for a project.
///
/// A missing or empty local config is simply absent; a parse error is fatal.
pub fn find_local(start: &Path) -> Result<Option<Config>, ConfigError> {
    let Some(path) = find_local_file(start) else {
        return Ok(None);
    };
    match read(&path) {
        Ok(config) => Ok(Some(config)),
        Err(ConfigError::NoSuchFile | ConfigError::EmptyFile) => Ok(None),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod tests {
    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{find_local, find_local_file, load_global, read};
    use crate::config::{Config, ConfigError};

    const COMMAND: &str = "# `hello`\n\n```bash\necho hello\n```\n";

    #[test]
    fn reads_a_config_file() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("nixon.md");
        file.write_str(COMMAND).unwrap();
        let config = read(file.path()).unwrap();
        assert_eq!(config.commands.len(), 1);
        assert_eq!(config.commands[0].name, "hello");
    }

    #[test]
    fn a_missing_file_is_no_such_file() {
        let temp = TempDir::new().unwrap();
        assert_eq!(
            read(&temp.path().join("nope.md")),
            Err(ConfigError::NoSuchFile)
        );
    }

    #[test]
    fn a_whitespace_only_file_is_empty() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("nixon.md");
        file.write_str("  \n\t\n").unwrap();
        assert_eq!(read(file.path()), Err(ConfigError::EmptyFile));
    }

    #[test]
    fn a_missing_global_config_is_an_empty_config() {
        let temp = TempDir::new().unwrap();
        assert_eq!(
            load_global(&temp.path().join("nope.md")),
            Ok(Config::default())
        );
    }

    #[test]
    fn an_empty_global_config_is_an_empty_config() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("nixon.md");
        file.write_str("\n\n").unwrap();
        assert_eq!(load_global(file.path()), Ok(Config::default()));
    }

    #[test]
    fn a_global_config_parse_error_is_fatal() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("nixon.md");
        file.write_str("# Config {.config}\n\n```bash\n```\n")
            .unwrap();
        assert!(load_global(file.path()).is_err());
    }

    #[test]
    fn finds_nixon_md_in_the_project_root() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("nixon.md");
        file.write_str(COMMAND).unwrap();
        assert_eq!(find_local_file(temp.path()), Some(file.to_path_buf()));
    }

    #[test]
    fn finds_a_dot_nixon_md_when_there_is_no_plain_one() {
        let temp = TempDir::new().unwrap();
        let file = temp.child(".nixon.md");
        file.write_str(COMMAND).unwrap();
        assert_eq!(find_local_file(temp.path()), Some(file.to_path_buf()));
    }

    #[test]
    fn prefers_nixon_md_over_a_dot_nixon_md_in_the_same_directory() {
        let temp = TempDir::new().unwrap();
        let plain = temp.child("nixon.md");
        plain.write_str(COMMAND).unwrap();
        temp.child(".nixon.md").write_str(COMMAND).unwrap();
        assert_eq!(find_local_file(temp.path()), Some(plain.to_path_buf()));
    }

    #[test]
    fn a_farther_nixon_md_beats_a_nearer_dot_nixon_md() {
        let temp = TempDir::new().unwrap();
        let outer = temp.child("nixon.md");
        outer.write_str(COMMAND).unwrap();
        let inner = temp.child("project");
        inner.create_dir_all().unwrap();
        inner.child(".nixon.md").write_str(COMMAND).unwrap();

        assert_eq!(find_local_file(inner.path()), Some(outer.to_path_buf()));
    }

    #[test]
    fn walks_up_from_the_project_root() {
        let temp = TempDir::new().unwrap();
        let file = temp.child("nixon.md");
        file.write_str(COMMAND).unwrap();
        let deep = temp.child("a/b");
        deep.create_dir_all().unwrap();
        assert_eq!(find_local_file(deep.path()), Some(file.to_path_buf()));
    }

    #[test]
    fn a_local_parse_error_is_fatal() {
        let temp = TempDir::new().unwrap();
        temp.child("nixon.md")
            .write_str("# `broken`\n\nno source block follows\n")
            .unwrap();
        assert!(find_local(temp.path()).is_err());
    }

    #[test]
    fn an_empty_local_config_is_simply_absent() {
        let temp = TempDir::new().unwrap();
        temp.child("nixon.md").write_str("\n").unwrap();
        assert_eq!(find_local(temp.path()), Ok(None));
    }
}
