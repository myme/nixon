//! Wrapping a command in an environment.

pub mod direnv;
pub mod nix;

use std::path::Path;

use crate::config::Config;

/// Applies direnv, then nix; the first that applies wins.
///
/// A command with no working directory is never wrapped.
pub fn maybe_wrap(
    config: &Config,
    argv: Vec<String>,
    cwd: Option<&Path>,
    direnv_dir: Option<&str>,
) -> Vec<String> {
    let Some(cwd) = cwd else {
        return argv;
    };

    if config.use_direnv == Some(true)
        && let Some(wrapped) = direnv::wrap(&argv, cwd, direnv_dir)
    {
        return wrapped;
    }
    if config.use_nix == Some(true)
        && let Some(wrapped) = nix::wrap(&argv, cwd)
    {
        return wrapped;
    }
    argv
}

#[cfg(test)]
mod tests {
    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::maybe_wrap;
    use crate::config::Config;

    fn argv() -> Vec<String> {
        vec!["bash".to_owned(), "script.sh".to_owned()]
    }

    fn both_enabled() -> Config {
        Config {
            use_direnv: Some(true),
            use_nix: Some(true),
            ..Config::default()
        }
    }

    #[test]
    fn nothing_is_wrapped_without_a_working_directory() {
        assert_eq!(maybe_wrap(&both_enabled(), argv(), None, None), argv());
    }

    #[test]
    fn nothing_is_wrapped_when_neither_is_enabled() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();

        assert_eq!(
            maybe_wrap(&Config::default(), argv(), Some(temp.path()), None),
            argv()
        );
    }

    #[test]
    fn direnv_is_tried_before_nix() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();

        let wrapped = maybe_wrap(&both_enabled(), argv(), Some(temp.path()), None);
        assert_eq!(wrapped[0], "direnv");
    }

    #[test]
    fn nix_applies_when_direnv_does_not() {
        let temp = TempDir::new().unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();

        let wrapped = maybe_wrap(&both_enabled(), argv(), Some(temp.path()), None);
        assert_eq!(wrapped[0], "nix-shell");
    }

    #[test]
    fn an_active_direnv_stops_nix_being_applied() {
        let temp = TempDir::new().unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();
        let active = format!("-{}", temp.path().display());

        let wrapped = maybe_wrap(&both_enabled(), argv(), Some(temp.path()), Some(&active));
        assert_eq!(wrapped, argv());
    }

    #[test]
    fn only_nix_enabled_skips_direnv_entirely() {
        let temp = TempDir::new().unwrap();
        temp.child(".envrc").write_str("use nix\n").unwrap();
        temp.child("shell.nix").write_str("{}\n").unwrap();

        let config = Config {
            use_nix: Some(true),
            ..Config::default()
        };
        let wrapped = maybe_wrap(&config, argv(), Some(temp.path()), None);
        assert_eq!(wrapped[0], "nix-shell");
    }
}
