//! Wrapping a command in `nix-shell`.

use std::path::Path;

use crate::fs::find_dominating_file;

/// Nix files, in the order v1 looked for them.
const NIX_FILES: [&str; 2] = ["shell.nix", "default.nix"];

/// Wraps `argv` in `nix-shell`, if there is a nix file at or above `cwd`.
///
/// As with the local config search, the whole ancestor chain is tried for
/// `shell.nix` before `default.nix` is considered at all, so a `shell.nix`
/// further up beats a closer `default.nix`. `flake.nix` is not supported.
///
/// Each argument is shell-quoted, fixing v1's naive `"…"` wrap that broke
/// on arguments containing spaces.
pub fn wrap(argv: &[String], cwd: &Path) -> Option<Vec<String>> {
    let nix_file = NIX_FILES
        .iter()
        .find_map(|name| find_dominating_file(cwd, name))?;

    Some(vec![
        "nix-shell".to_owned(),
        "--command".to_owned(),
        shell_words::join(argv),
        nix_file.to_string_lossy().into_owned(),
    ])
}

#[cfg(test)]
mod tests {
    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::wrap;

    fn argv() -> Vec<String> {
        vec!["bash".to_owned(), "script.sh".to_owned()]
    }

    #[test]
    fn wraps_with_a_shell_nix() {
        let temp = TempDir::new().unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();

        let wrapped = wrap(&argv(), temp.path()).unwrap();
        assert_eq!(wrapped[0], "nix-shell");
        assert_eq!(wrapped[1], "--command");
        assert_eq!(wrapped[2], "bash script.sh");
        assert!(wrapped[3].ends_with("shell.nix"));
    }

    #[test]
    fn falls_back_to_default_nix() {
        let temp = TempDir::new().unwrap();
        temp.child("default.nix").write_str("{}\n").unwrap();

        let wrapped = wrap(&argv(), temp.path()).unwrap();
        assert!(wrapped[3].ends_with("default.nix"));
    }

    #[test]
    fn a_shell_nix_further_up_beats_a_closer_default_nix() {
        let temp = TempDir::new().unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();
        let inner = temp.child("a/b");
        inner.create_dir_all().unwrap();
        inner.child("default.nix").write_str("{}\n").unwrap();

        let wrapped = wrap(&argv(), inner.path()).unwrap();
        assert!(wrapped[3].ends_with("shell.nix"));
    }

    #[test]
    fn does_not_apply_without_a_nix_file() {
        let temp = TempDir::new().unwrap();
        assert_eq!(wrap(&argv(), temp.path()), None);
    }

    #[test]
    fn a_flake_does_not_count() {
        let temp = TempDir::new().unwrap();
        temp.child("flake.nix").write_str("{}\n").unwrap();
        assert_eq!(wrap(&argv(), temp.path()), None);
    }

    #[test]
    fn arguments_with_spaces_are_quoted_individually() {
        let temp = TempDir::new().unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();

        let argv = vec![
            "bash".to_owned(),
            "script.sh".to_owned(),
            "a file.txt".to_owned(),
            "it's".to_owned(),
        ];
        let wrapped = wrap(&argv, temp.path()).unwrap();
        assert_eq!(wrapped[2], "bash script.sh 'a file.txt' 'it'\\''s'");
    }
}
