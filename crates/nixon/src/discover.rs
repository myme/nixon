//! The commands available in a project.

use std::path::{Path, PathBuf};

use crate::command::{Command, CommandLocation};
use crate::config::Config;
use crate::project::Project;

/// Every command offered in a project, sorted by name.
///
/// Hidden `_commands` are included: placeholders reference them by name. The
/// run picker filters them out itself.
pub fn find_project_commands(config: &Config, project: &Project) -> Vec<Command> {
    let ptypes: Vec<&str> = project.types.iter().map(|t| t.id.as_str()).collect();

    let mut commands: Vec<Command> = config
        .commands
        .iter()
        .filter(|cmd| {
            cmd.project_types.is_empty()
                || cmd
                    .project_types
                    .iter()
                    .any(|wanted| ptypes.contains(&wanted.as_str()))
        })
        .cloned()
        .collect();

    commands.extend(find_bin_commands(config, project));
    // Stable, so equal names keep the local-before-global order of §3.3.
    commands.sort_by(|a, b| a.name.cmp(&b.name));
    commands
}

/// Executables in the project's `bin_dirs`, as commands.
///
/// Every entry that is executable, which is what v1 plainly meant. v1
/// itself used turtle's `lsif`, whose predicate
/// only decides whether to *descend* into a directory: it yielded every
/// entry, executable or not, directories included, and recursed into any
/// traversable subdirectory. Non-executable files became commands that could
/// not run. Treated as a bug and not reproduced.
fn find_bin_commands(config: &Config, project: &Project) -> Vec<Command> {
    let mut found = Vec::new();
    for dir in &config.bin_dirs {
        let bin_path = project.path().join(dir);
        let Ok(entries) = std::fs::read_dir(&bin_path) else {
            continue;
        };
        let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok().map(|e| e.path())).collect();
        paths.sort();

        for path in paths {
            if !is_executable_file(&path) {
                continue;
            }
            let Some(name) = path.file_name().map(|n| n.to_string_lossy().into_owned()) else {
                continue;
            };
            found.push(Command {
                name,
                source: format!("{} \"$@\"", shell_words::quote(&path.to_string_lossy())),
                location: Some(CommandLocation {
                    file_path: path,
                    start_line: 0,
                    end_line: 0,
                    level: 0,
                }),
                ..Command::default()
            });
        }
    }
    found
}

/// Whether a path is a regular file with any execute bit set.
#[cfg(unix)]
fn is_executable_file(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt as _;

    std::fs::metadata(path)
        .is_ok_and(|meta| meta.is_file() && meta.permissions().mode() & 0o111 != 0)
}

/// Non-unix hosts have no execute bit; nixon targets Linux and macOS.
#[cfg(not(unix))]
fn is_executable_file(path: &Path) -> bool {
    path.is_file()
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::find_project_commands;
    use crate::command::{Command, CommandLocation};
    use crate::config::Config;
    use crate::markdown;
    use crate::project::{Project, ProjectType};

    fn command(name: &str, types: &[&str]) -> Command {
        Command {
            name: name.to_owned(),
            project_types: types.iter().map(|t| (*t).to_owned()).collect(),
            ..Command::default()
        }
    }

    fn ptype(id: &str) -> ProjectType {
        ProjectType {
            id: id.to_owned(),
            markers: Vec::new(),
            description: format!("{id} project"),
        }
    }

    fn project(path: &Path, types: Vec<ProjectType>) -> Project {
        Project::from_path(path, types)
    }

    fn names(commands: &[Command]) -> Vec<&str> {
        commands.iter().map(|c| c.name.as_str()).collect()
    }

    #[test]
    fn fetches_empty_commands() {
        let temp = TempDir::new().unwrap();
        let found = find_project_commands(&Config::default(), &project(temp.path(), vec![]));
        assert!(found.is_empty());
    }

    #[test]
    fn fetches_markdown_commands() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            commands: vec![command("foo", &[])],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert_eq!(names(&found), ["foo"]);
    }

    #[test]
    fn filters_away_missing_project_types() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            commands: vec![command("foo", &["bar"])],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert!(found.is_empty());
    }

    #[test]
    fn matches_project_type() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            commands: vec![command("foo", &["bar"])],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![ptype("bar")]));
        assert_eq!(names(&found), ["foo"]);
    }

    #[test]
    fn filters_away_a_typed_command_from_markdown() {
        let temp = TempDir::new().unwrap();
        let parsed = markdown::parse_config_file(
            "some-file.md",
            "# `hello` {type=\"git\"}\n```bash\necho Hello World\n```\n",
        )
        .unwrap();
        let found = find_project_commands(&parsed, &project(temp.path(), vec![]));
        assert!(found.is_empty());
    }

    #[test]
    fn a_command_matching_any_of_its_types_is_kept() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            commands: vec![command("foo", &["nope", "bar"])],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![ptype("bar")]));
        assert_eq!(names(&found), ["foo"]);
    }

    #[test]
    fn hidden_commands_are_included() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            commands: vec![Command {
                is_hidden: true,
                ..command("_hidden", &[])
            }],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert_eq!(names(&found), ["_hidden"]);
    }

    #[test]
    fn commands_are_sorted_by_name() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            commands: vec![
                command("zebra", &[]),
                command("alpha", &[]),
                command("mid", &[]),
            ],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert_eq!(names(&found), ["alpha", "mid", "zebra"]);
    }

    #[test]
    fn equal_names_keep_their_original_order() {
        let temp = TempDir::new().unwrap();
        let local = Command {
            source: "local".to_owned(),
            ..command("same", &[])
        };
        let global = Command {
            source: "global".to_owned(),
            ..command("same", &[])
        };
        let config = Config {
            commands: vec![local, global],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        let sources: Vec<&str> = found.iter().map(|c| c.source.as_str()).collect();
        assert_eq!(sources, ["local", "global"]);
    }

    #[cfg(unix)]
    /// A file name is not a shell word: a space splits the command and
    /// `$(…)` in it used to run.
    #[test]
    #[cfg(unix)]
    fn a_bin_path_with_shell_metacharacters_is_quoted() {
        let temp = TempDir::new().unwrap();
        let bin = temp.child("bin");
        bin.create_dir_all().unwrap();
        let script = bin.child("a b$(id)");
        script.write_str("#!/bin/sh\n").unwrap();
        make_executable(script.path());

        let config = Config {
            bin_dirs: vec![PathBuf::from("bin")],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), Vec::new()));

        assert_eq!(found[0].name, "a b$(id)");
        assert_eq!(
            found[0].source,
            format!(
                "{} \"$@\"",
                shell_words::quote(&script.path().to_string_lossy())
            )
        );
    }

    fn make_executable(path: &Path) {
        use std::os::unix::fs::PermissionsExt as _;
        let mut perms = std::fs::metadata(path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(path, perms).unwrap();
    }

    #[test]
    #[cfg(unix)]
    fn bin_dir_executables_become_commands() {
        let temp = TempDir::new().unwrap();
        let bin = temp.child("bin");
        bin.create_dir_all().unwrap();
        let script = bin.child("deploy");
        script.write_str("#!/bin/sh\n").unwrap();
        make_executable(script.path());

        let config = Config {
            bin_dirs: vec![PathBuf::from("bin")],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert_eq!(names(&found), ["deploy"]);
        assert_eq!(
            found[0].source,
            format!("{} \"$@\"", script.path().display())
        );
        assert_eq!(
            found[0].location,
            Some(CommandLocation {
                file_path: script.to_path_buf(),
                start_line: 0,
                end_line: 0,
                level: 0,
            })
        );
    }

    #[test]
    #[cfg(unix)]
    fn non_executable_files_in_a_bin_dir_are_skipped() {
        let temp = TempDir::new().unwrap();
        let bin = temp.child("bin");
        bin.create_dir_all().unwrap();
        bin.child("README").write_str("not a script\n").unwrap();
        let script = bin.child("run");
        script.write_str("#!/bin/sh\n").unwrap();
        make_executable(script.path());

        let config = Config {
            bin_dirs: vec![PathBuf::from("bin")],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert_eq!(names(&found), ["run"]);
    }

    #[test]
    #[cfg(unix)]
    fn directories_in_a_bin_dir_are_not_commands() {
        let temp = TempDir::new().unwrap();
        let bin = temp.child("bin");
        bin.create_dir_all().unwrap();
        let nested = bin.child("helpers");
        nested.create_dir_all().unwrap();
        let inner = nested.child("inner");
        inner.write_str("#!/bin/sh\n").unwrap();
        make_executable(inner.path());

        let config = Config {
            bin_dirs: vec![PathBuf::from("bin")],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert!(found.is_empty());
    }

    #[test]
    fn a_missing_bin_dir_yields_nothing() {
        let temp = TempDir::new().unwrap();
        let config = Config {
            bin_dirs: vec![PathBuf::from("nope")],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert!(found.is_empty());
    }

    #[test]
    #[cfg(unix)]
    fn bin_commands_sort_together_with_markdown_commands() {
        let temp = TempDir::new().unwrap();
        let bin = temp.child("bin");
        bin.create_dir_all().unwrap();
        let script = bin.child("middle");
        script.write_str("#!/bin/sh\n").unwrap();
        make_executable(script.path());

        let config = Config {
            bin_dirs: vec![PathBuf::from("bin")],
            commands: vec![command("zebra", &[]), command("alpha", &[])],
            ..Config::default()
        };
        let found = find_project_commands(&config, &project(temp.path(), vec![]));
        assert_eq!(names(&found), ["alpha", "middle", "zebra"]);
    }
}
