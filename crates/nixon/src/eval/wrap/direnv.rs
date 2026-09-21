//! Wrapping a command in `direnv exec`.

use std::path::Path;

use crate::fs::find_dominating_file;

/// Wraps `argv` for direnv, or reports that direnv does not apply.
///
/// Returns `Some(argv)` unchanged when direnv is already active for `cwd`,
/// which is what stops the nix wrapper being tried as well.
pub fn wrap(argv: &[String], cwd: &Path, direnv_dir: Option<&str>) -> Option<Vec<String>> {
    let envrc = find_dominating_file(cwd, ".envrc")?;
    if is_active(envrc.parent()?, direnv_dir) {
        return Some(argv.to_vec());
    }

    let mut wrapped = vec![
        "direnv".to_owned(),
        "exec".to_owned(),
        cwd.to_string_lossy().into_owned(),
    ];
    wrapped.extend_from_slice(argv);
    Some(wrapped)
}

/// Whether the loaded direnv environment is the one `nearest` would load.
///
/// It has to be that one and not merely an ancestor: in a monorepo a
/// package with its own `.envrc` under an already-loaded root needs its own
/// environment, and treating any ancestor as active never wrapped it.
///
/// direnv prefixes `$DIRENV_DIR` with `-`, which v1 dropped by discarding
/// everything before the first `/`.
fn is_active(nearest: &Path, direnv_dir: Option<&str>) -> bool {
    let Some(dir) = direnv_dir else {
        return false;
    };
    let Some(slash) = dir.find('/') else {
        return false;
    };
    nearest == Path::new(&dir[slash..])
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::wrap;

    fn argv() -> Vec<String> {
        vec!["bash".to_owned(), "script.sh".to_owned()]
    }

    #[test]
    fn wraps_when_an_envrc_is_present() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();

        let wrapped = wrap(&argv(), temp.path(), None).unwrap();
        assert_eq!(
            wrapped,
            [
                "direnv",
                "exec",
                &temp.path().to_string_lossy(),
                "bash",
                "script.sh"
            ]
        );
    }

    #[test]
    fn wraps_when_an_envrc_is_further_up() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        let deep = temp.child("a/b");
        deep.create_dir_all().unwrap();

        let wrapped = wrap(&argv(), deep.path(), None).unwrap();
        assert_eq!(wrapped[2], deep.path().to_string_lossy());
    }

    #[test]
    fn does_not_apply_without_an_envrc() {
        let temp = TempDir::new().unwrap();
        assert_eq!(wrap(&argv(), temp.path(), None), None);
    }

    #[test]
    fn leaves_the_command_alone_when_direnv_is_already_active_here() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        let active = format!("-{}", temp.path().display());

        assert_eq!(wrap(&argv(), temp.path(), Some(&active)), Some(argv()));
    }

    #[test]
    fn leaves_the_command_alone_when_direnv_is_active_in_a_parent() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        let deep = temp.child("a/b");
        deep.create_dir_all().unwrap();
        let active = format!("-{}", temp.path().display());

        // The parent's .envrc is the nearest one, so it is the loaded one.
        assert_eq!(wrap(&argv(), deep.path(), Some(&active)), Some(argv()));
    }

    /// A monorepo: the root is loaded, but the package has an `.envrc` of
    /// its own, so its environment is not the one in effect.
    #[test]
    fn a_nearer_envrc_is_wrapped_even_under_an_active_parent() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        let package = temp.child("packages/api");
        package.create_dir_all().unwrap();
        package.child(".envrc").write_str("use nix\n").unwrap();

        let active = format!("-{}", temp.path().display());
        let wrapped = wrap(&argv(), package.path(), Some(&active)).unwrap();

        assert_eq!(wrapped[0], "direnv");
        assert_eq!(wrapped[2], package.path().to_string_lossy());
    }

    #[test]
    fn direnv_does_not_apply_without_an_envrc_whatever_is_loaded() {
        let temp = TempDir::new().unwrap();
        let active = format!("-{}", temp.path().display());
        assert_eq!(wrap(&argv(), temp.path(), Some(&active)), None);
    }

    #[test]
    fn an_unrelated_active_direnv_does_not_count() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();

        let wrapped = wrap(&argv(), temp.path(), Some("-/somewhere/else")).unwrap();
        assert_eq!(wrapped[0], "direnv");
    }

    #[test]
    fn a_direnv_dir_without_a_slash_is_ignored() {
        assert_eq!(
            wrap(&argv(), Path::new("/tmp/nope-nixon"), Some("junk")),
            None
        );
    }
}
