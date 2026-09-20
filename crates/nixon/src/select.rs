//! Bridges nixon's types to the picker. SPEC §8.4, ENGINEERING §4.2.
//!
//! The only module that names the picker; nothing else in this crate does.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use nixon_picker::{Candidate, PickerOptions, SelectionType};

use crate::command::Command;
use crate::config::Config;
use crate::fs::implode_home;
use crate::matcher_options;
use crate::project::Project;
use std::path::Path;

/// Builds picker candidates from plain lines, as SPEC §5.6's `Lines` does.
pub fn line_candidates(lines: &[String]) -> Vec<Candidate> {
    lines.iter().map(Candidate::identity).collect()
}

/// Candidates for command selection. SPEC §8.4.
///
/// The text is `show_command_with_description`, and the value is the command
/// name, which is what maps back to the command.
pub fn command_candidates(commands: &[Command]) -> Vec<Candidate> {
    commands
        .iter()
        .map(|command| Candidate::with_title(command.to_string(), command.name.clone()))
        .collect()
}

/// Options for command selection. SPEC §8.4.
///
/// Header is `"<prompt> [<project name>] (<project dir>)"`, candidates keep
/// discovery order, and the three expect keys confirm with their own type.
pub fn command_options(
    config: &Config,
    project: &Project,
    prompt: &str,
    query: Option<&str>,
) -> PickerOptions {
    let header = format!(
        "{prompt} [{}] ({})",
        project.name.display(),
        project.dir.display()
    );

    let mut options = PickerOptions {
        header: Some(header),
        initial_query: query.map(ToOwned::to_owned),
        matching: matcher_options(config),
        select_one: true,
        ..PickerOptions::default()
    }
    // Matching filters; discovery order (by name) is the ranking. SPEC §8.4.
    .no_sort();

    options.expect = expect_keys();
    options
}

/// Candidates for project selection: `~`-collapsed paths, sorted. SPEC §8.4.
pub fn project_candidates(projects: &[Project], home: &Path) -> Vec<Candidate> {
    let mut candidates: Vec<Candidate> = projects
        .iter()
        .map(|project| {
            let path = project.path();
            Candidate::with_title(
                implode_home(&path, home).to_string_lossy().into_owned(),
                path.to_string_lossy().into_owned(),
            )
        })
        .collect();
    candidates.sort_by(|a, b| a.display.cmp(&b.display));
    candidates.dedup_by(|a, b| a.value == b.value);
    candidates
}

/// Options for project selection. SPEC §8.4.
pub fn project_options(config: &Config, query: Option<&str>, multi: bool) -> PickerOptions {
    PickerOptions {
        header: Some("Select project".to_owned()),
        initial_query: query.map(ToOwned::to_owned),
        matching: matcher_options(config),
        multi,
        expect: vec![(key(KeyCode::F(1)), SelectionType::Show)],
        select_one: true,
    }
}

/// `Alt-Enter` edits, `F1` shows, `F2` visits. SPEC §8.4, ENGINEERING §7.2.
fn expect_keys() -> Vec<(KeyEvent, SelectionType)> {
    vec![
        (
            KeyEvent::new(KeyCode::Enter, KeyModifiers::ALT),
            SelectionType::Edit,
        ),
        (key(KeyCode::F(1)), SelectionType::Show),
        (key(KeyCode::F(2)), SelectionType::Visit),
    ]
}

const fn key(code: KeyCode) -> KeyEvent {
    KeyEvent::new(code, KeyModifiers::NONE)
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use nixon_picker::SelectionType;

    use super::{
        command_candidates, command_options, line_candidates, project_candidates, project_options,
    };
    use crate::command::Command;
    use crate::config::Config;
    use crate::project::{Project, ProjectType};

    fn command(name: &str, desc: Option<&str>) -> Command {
        Command {
            name: name.to_owned(),
            desc: desc.map(ToOwned::to_owned),
            ..Command::default()
        }
    }

    fn project(path: &str) -> Project {
        Project::from_path(Path::new(path), Vec::<ProjectType>::new())
    }

    #[test]
    fn lines_become_candidates_that_are_their_own_value() {
        let lines = vec!["src/main.rs".to_owned(), "README.md".to_owned()];
        let candidates = line_candidates(&lines);
        assert_eq!(candidates[0].value, "src/main.rs");
        assert_eq!(candidates[1].display, "README.md");
    }

    #[test]
    fn a_command_shows_its_description_and_returns_its_name() {
        let candidates = command_candidates(&[
            command("build", Some("Build the workspace")),
            command("run", None),
        ]);
        assert_eq!(candidates[0].display, "build - Build the workspace");
        assert_eq!(candidates[0].value, "build");
        assert_eq!(candidates[1].display, "run");
    }

    #[test]
    fn command_selection_names_the_project_in_its_header() {
        let options = command_options(
            &Config::default(),
            &project("/home/me/code/nixon"),
            "Select command",
            None,
        );
        assert_eq!(
            options.header.as_deref(),
            Some("Select command [nixon] (/home/me/code)")
        );
    }

    #[test]
    fn command_selection_keeps_discovery_order() {
        let options = command_options(&Config::default(), &project("/a/b"), "Select command", None);
        assert!(!options.matching.sort);
    }

    #[test]
    fn command_selection_binds_the_three_expect_keys() {
        let options = command_options(&Config::default(), &project("/a/b"), "x", None);
        let kinds: Vec<SelectionType> = options.expect.iter().map(|(_, k)| *k).collect();
        assert_eq!(
            kinds,
            [
                SelectionType::Edit,
                SelectionType::Show,
                SelectionType::Visit
            ]
        );
    }

    #[test]
    fn a_query_is_carried_into_the_picker() {
        let options = command_options(&Config::default(), &project("/a/b"), "x", Some("build"));
        assert_eq!(options.initial_query.as_deref(), Some("build"));
        assert!(options.select_one);
    }

    #[test]
    fn projects_show_a_collapsed_home_but_return_the_full_path() {
        let candidates =
            project_candidates(&[project("/home/me/code/nixon")], Path::new("/home/me"));
        assert_eq!(candidates[0].display, "~/code/nixon");
        assert_eq!(candidates[0].value, "/home/me/code/nixon");
    }

    #[test]
    fn projects_are_sorted_and_deduplicated() {
        let candidates = project_candidates(
            &[
                project("/src/zebra"),
                project("/src/alpha"),
                project("/src/alpha"),
            ],
            Path::new("/home/me"),
        );
        let values: Vec<&str> = candidates.iter().map(|c| c.value.as_str()).collect();
        assert_eq!(values, ["/src/alpha", "/src/zebra"]);
    }

    #[test]
    fn project_selection_binds_f1_to_show() {
        let options = project_options(&Config::default(), None, false);
        assert_eq!(options.header.as_deref(), Some("Select project"));
        assert_eq!(options.expect.len(), 1);
        assert_eq!(options.expect[0].1, SelectionType::Show);
    }

    #[test]
    fn project_selection_allows_multi_when_asked() {
        assert!(project_options(&Config::default(), None, true).multi);
        assert!(!project_options(&Config::default(), None, false).multi);
    }

    #[test]
    fn a_path_outside_home_is_left_alone() {
        let candidates = project_candidates(&[project("/opt/thing")], Path::new("/home/me"));
        assert_eq!(candidates[0].display, "/opt/thing");
        assert_eq!(
            candidates[0].value,
            PathBuf::from("/opt/thing").to_string_lossy()
        );
    }
}
