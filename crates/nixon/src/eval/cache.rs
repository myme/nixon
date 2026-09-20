//! The script cache. SPEC §7.2.

use std::io;
use std::path::{Path, PathBuf};

use sha1::{Digest as _, Sha1};

use crate::command::Command;

/// Where a command's script is written: `<sha1-of-source>-<name><ext>`.
/// SPEC §7.2.
///
/// The name is only a label, so anything that would make it a path is
/// flattened: a `bin_dirs` command is named after its file, and a separator
/// in there would point the write at a directory that does not exist.
pub fn script_path(cache_dir: &Path, command: &Command) -> PathBuf {
    let digest = Sha1::digest(command.source.as_bytes());
    let name = flatten(&command.name);
    let ext = command.lang.extension();
    cache_dir.join(format!("{digest:x}-{name}{ext}"))
}

/// Replaces anything that would turn a name into a path.
fn flatten(name: &str) -> String {
    name.chars()
        .map(|c| match c {
            '/' | '\\' | '\0' => '_',
            other => other,
        })
        .collect()
}

/// Writes a command's source to the cache and returns the path. SPEC §7.2.
///
/// The file is deliberately not made executable: the interpreter is always
/// explicit, so a shebang in the source is ignored.
pub fn write_script(cache_dir: &Path, command: &Command) -> io::Result<PathBuf> {
    std::fs::create_dir_all(cache_dir)?;
    let path = script_path(cache_dir, command);
    std::fs::write(&path, &command.source)?;
    Ok(path)
}

/// Empties the cache, reporting each file. SPEC §7.2.
///
/// Reports `would remove <path>` when `dry_run`, `removed <path>` otherwise.
/// Paths are sorted, where v1 used directory order; nothing depends on the
/// order and a stable one is testable.
pub fn garbage_collect(cache_dir: &Path, dry_run: bool) -> io::Result<Vec<String>> {
    let entries = match std::fs::read_dir(cache_dir) {
        Ok(entries) => entries,
        Err(err) if err.kind() == io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(err) => return Err(err),
    };

    let mut paths: Vec<PathBuf> = entries
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| path.is_file())
        .collect();
    paths.sort();

    let mut reported = Vec::new();
    for path in paths {
        if !dry_run {
            std::fs::remove_file(&path)?;
        }
        let verb = if dry_run { "would remove" } else { "removed" };
        reported.push(format!("{verb} {}", path.display()));
    }
    Ok(reported)
}

#[cfg(test)]
mod tests {
    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{garbage_collect, script_path, write_script};
    use crate::command::Command;
    use crate::language::Language;

    fn command(name: &str, source: &str, lang: Language) -> Command {
        Command {
            name: name.to_owned(),
            source: source.to_owned(),
            lang,
            ..Command::default()
        }
    }

    /// A `bin_dirs` command is named after a path; the cache is one flat
    /// directory, so writing it used to fail with ENOENT.
    #[test]
    fn a_name_with_a_separator_stays_one_file() {
        let temp = TempDir::new().unwrap();
        let cmd = command("bin/deploy", "echo go\n", Language::Bash);

        let path = script_path(temp.path(), &cmd);
        assert_eq!(path.parent(), Some(temp.path()));
        assert!(
            path.file_name()
                .unwrap()
                .to_string_lossy()
                .ends_with("-bin_deploy.sh"),
            "got {}",
            path.display()
        );
        assert!(write_script(temp.path(), &cmd).is_ok());
    }

    #[test]
    fn the_script_path_is_the_source_digest_the_name_and_the_extension() {
        let temp = TempDir::new().unwrap();
        let cmd = command("hello", "echo Hello World\n", Language::Bash);
        let path = script_path(temp.path(), &cmd);
        let name = path.file_name().unwrap().to_string_lossy();

        // sha1 of "echo Hello World\n"
        assert!(name.ends_with("-hello.sh"));
        assert_eq!(name.len(), 40 + "-hello.sh".len());
        assert!(name[..40].chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn the_same_source_hashes_the_same_and_a_different_one_does_not() {
        let temp = TempDir::new().unwrap();
        let one = command("x", "echo one\n", Language::Bash);
        let same = command("x", "echo one\n", Language::Bash);
        let other = command("x", "echo two\n", Language::Bash);

        assert_eq!(
            script_path(temp.path(), &one),
            script_path(temp.path(), &same)
        );
        assert_ne!(
            script_path(temp.path(), &one),
            script_path(temp.path(), &other)
        );
    }

    #[test]
    fn the_extension_follows_the_language() {
        let temp = TempDir::new().unwrap();
        for (lang, ext) in [
            (Language::Bash, ".sh"),
            (Language::Python, ".py"),
            (Language::Json, ".json"),
            (Language::None, ".sh"),
        ] {
            let path = script_path(temp.path(), &command("x", "s", lang));
            assert!(
                path.to_string_lossy().ends_with(ext),
                "{path:?} should end with {ext}"
            );
        }
    }

    #[test]
    fn an_eval_command_has_an_empty_name() {
        let temp = TempDir::new().unwrap();
        let path = script_path(temp.path(), &command("", "echo\n", Language::Bash));
        assert!(
            path.file_name()
                .unwrap()
                .to_string_lossy()
                .ends_with("-.sh")
        );
    }

    #[test]
    fn writing_creates_the_cache_directory_and_the_script() {
        let temp = TempDir::new().unwrap();
        let cache = temp.child("cache/nixon");
        let cmd = command("hello", "echo Hello World\n", Language::Bash);

        let path = write_script(cache.path(), &cmd).unwrap();
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            "echo Hello World\n"
        );
    }

    #[test]
    fn writing_twice_is_idempotent() {
        let temp = TempDir::new().unwrap();
        let cmd = command("hello", "echo hi\n", Language::Bash);
        let first = write_script(temp.path(), &cmd).unwrap();
        let second = write_script(temp.path(), &cmd).unwrap();
        assert_eq!(first, second);
    }

    #[test]
    fn a_dry_run_reports_without_removing() {
        let temp = TempDir::new().unwrap();
        temp.child("a.sh").write_str("x").unwrap();
        temp.child("b.sh").write_str("y").unwrap();

        let reported = garbage_collect(temp.path(), true).unwrap();
        assert_eq!(reported.len(), 2);
        assert!(reported[0].starts_with("would remove "));
        assert!(reported[0].ends_with("a.sh"));
        assert!(temp.child("a.sh").path().exists());
    }

    #[test]
    fn a_real_run_removes_and_reports() {
        let temp = TempDir::new().unwrap();
        temp.child("a.sh").write_str("x").unwrap();

        let reported = garbage_collect(temp.path(), false).unwrap();
        assert_eq!(reported.len(), 1);
        assert!(reported[0].starts_with("removed "));
        assert!(!temp.child("a.sh").path().exists());
    }

    #[test]
    fn a_missing_cache_directory_reports_nothing() {
        let temp = TempDir::new().unwrap();
        let reported = garbage_collect(&temp.path().join("nope"), false).unwrap();
        assert!(reported.is_empty());
    }

    #[test]
    fn an_empty_cache_reports_nothing() {
        let temp = TempDir::new().unwrap();
        assert!(garbage_collect(temp.path(), false).unwrap().is_empty());
    }
}
