//! Bridges nixon's types to the picker.
//!
//! The only module that names the picker; nothing else in this crate does.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use nixon_picker::{Candidate, PickerOptions, SelectionType};

use crate::command::{Command, DescSpan, Description};
use crate::config::Config;
use crate::fs::implode_home;
use crate::matcher_options;
use crate::project::Project;
use std::path::Path;

/// Builds picker candidates from plain lines, as a `lines` placeholder does.
pub fn line_candidates(lines: &[String]) -> Vec<Candidate> {
    lines.iter().map(Candidate::identity).collect()
}

/// Candidates for command selection.
///
/// The text is `show_command_with_description`, and the value is the command
/// name, which is what maps back to the command. The description is dimmed
/// so the name reads first; the picker renders the ANSI, and matching runs
/// on the visible text either way.
pub fn command_candidates(commands: &[Command]) -> Vec<Candidate> {
    commands
        .iter()
        .map(|command| {
            let display = command.desc.as_ref().map_or_else(
                || command.name.clone(),
                |desc| format!("{}{DIM} - {}{RESET}", command.name, styled(desc)),
            );
            Candidate::with_title(display, command.name.clone())
        })
        .collect()
}

/// A description with its inline code picked out of the dimmed prose.
fn styled(desc: &Description) -> String {
    desc.spans
        .iter()
        .map(|span| match span {
            DescSpan::Text(text) => text.clone(),
            // `2m` only adds dim: without a reset first, the code colour
            // runs on into the prose after it.
            DescSpan::Code(code) => format!("{CODE}{code}{RESET}{DIM}"),
        })
        .collect()
}

/// Dims the secondary half of a candidate.
const DIM: &str = "\u{1b}[2m";
/// Inline code within a description; still dim, but a colour of its own.
const CODE: &str = "\u{1b}[2;36m";
/// Ends the dimmed run.
const RESET: &str = "\u{1b}[0m";

/// Options for command selection.
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
    // Matching filters; discovery order (by name) is the ranking.
    .no_sort();

    options.expect = expect_keys();
    options
}

/// Candidates for the history picker.
///
/// The value is the invocation, which is what gets re-run or printed; the
/// when and where are dimmed context in front of it.
pub fn history_candidates(
    entries: &[crate::history::Entry],
    home: &Path,
    now: u64,
) -> Vec<Candidate> {
    entries
        .iter()
        .map(|entry| {
            // The whole command line, program name and all: this is what a
            // user presses Enter on after it is printed or inserted.
            let invocation = shell_words::join(
                std::iter::once("nixon").chain(entry.invocation.iter().map(String::as_str)),
            );
            let display = format!(
                "{DIM}{:>4}  {}{RESET}  {invocation}",
                crate::history::ago(entry.at, now),
                implode_home(Path::new(&entry.cwd), home).display(),
            );
            Candidate::with_title(display, invocation)
        })
        .collect()
}

/// Options for the history picker.
pub fn history_options(config: &Config, query: Option<&str>) -> PickerOptions {
    let mut options = PickerOptions {
        header: Some("Run again".to_owned()),
        initial_query: query.map(ToOwned::to_owned),
        matching: matcher_options(config),
        // Only when asked for something: a query naming one line needs no
        // picker, but opening `history` and having it run the only entry
        // is not what anyone meant by looking.
        select_one: query.is_some(),
        ..PickerOptions::default()
    }
    // Newest first is the order; ranking would undo it.
    .no_sort();
    options.expect = vec![(key(KeyCode::F(1)), SelectionType::Show)];
    options
}

/// Candidates for project selection: `~`-collapsed paths, sorted.
pub fn project_candidates(projects: &[Project], home: &Path) -> Vec<Candidate> {
    let mut candidates: Vec<Candidate> = projects
        .iter()
        .map(|project| {
            let path = project.path();
            let shown = implode_home(&path, home);
            let text = shown.to_string_lossy();
            // The same text v1 listed, with the directory it sits in dimmed
            // so the project's own name reads first.
            let display = text.rfind('/').map_or_else(
                || text.clone().into_owned(),
                |at| format!("{DIM}{}{RESET}{}", &text[..=at], &text[at + 1..]),
            );
            Candidate::with_title(display, path.to_string_lossy().into_owned())
        })
        .collect();
    candidates.sort_by_key(Candidate::plain);
    candidates.dedup_by(|a, b| a.value == b.value);
    candidates
}

/// Options for project selection.
pub fn project_options(config: &Config, query: Option<&str>, multi: bool) -> PickerOptions {
    PickerOptions {
        header: Some("Select project".to_owned()),
        initial_query: query.map(ToOwned::to_owned),
        matching: matcher_options(config),
        multi,
        expect: vec![(key(KeyCode::F(1)), SelectionType::Show)],
        select_one: true,
        select_exact: false,
        options: Vec::new(),
    }
}

/// `Alt-Enter` edits, `F1` shows, `F2` visits.
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
    use std::path::Path;

    use nixon_picker::SelectionType;

    use super::{
        command_candidates, command_options, line_candidates, project_candidates, project_options,
    };
    use crate::command::{Command, DescSpan, Description};
    use crate::config::Config;
    use crate::project::{Project, ProjectType};

    fn command(name: &str, desc: Option<&str>) -> Command {
        Command {
            name: name.to_owned(),
            desc: desc.map(Description::text),
            ..Command::default()
        }
    }

    #[test]
    fn inline_code_in_a_description_gets_its_own_colour() {
        let described = Command {
            name: "build".to_owned(),
            desc: Some(Description::new([
                DescSpan::Text("Run ".to_owned()),
                DescSpan::Code("cargo build".to_owned()),
                DescSpan::Text(".".to_owned()),
            ])),
            ..Command::default()
        };
        let candidates = command_candidates(std::slice::from_ref(&described));

        // The visible text is what matching and stdout see: no markers, and
        // no spaces around the code span.
        assert_eq!(candidates[0].plain(), "build - Run cargo build.");
        assert!(
            candidates[0].display.contains("\u{1b}[2;36mcargo build"),
            "the code span was not styled: {:?}",
            candidates[0].display
        );
        // The prose after the code span goes back to plain dim, rather than
        // inheriting the code colour to the end of the line.
        assert!(
            candidates[0]
                .display
                .contains("cargo build\u{1b}[0m\u{1b}[2m."),
            "the code colour bled into the prose: {:?}",
            candidates[0].display
        );
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
        assert_eq!(candidates[0].plain(), "build - Build the workspace");
        assert_eq!(candidates[0].value, "build");
        assert_eq!(candidates[1].display, "run");
    }

    #[test]
    fn a_description_is_dimmed_but_still_matchable() {
        let candidates = command_candidates(&[command("build", Some("Build the workspace"))]);
        assert!(candidates[0].display.starts_with("build\u{1b}[2m"));
        // Matching and highlighting run on the visible text.
        assert_eq!(candidates[0].plain(), "build - Build the workspace");
    }

    #[test]
    fn a_command_without_a_description_carries_no_escapes() {
        let candidates = command_candidates(&[command("run", None)]);
        assert!(!candidates[0].display.contains('\u{1b}'));
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
        assert_eq!(candidates[0].plain(), "~/code/nixon");
        assert_eq!(candidates[0].value, "/home/me/code/nixon");
    }

    #[test]
    fn the_directory_a_project_sits_in_is_dimmed() {
        let candidates =
            project_candidates(&[project("/home/me/code/nixon")], Path::new("/home/me"));
        assert!(candidates[0].display.starts_with("\u{1b}[2m~/code/"));
        assert!(candidates[0].display.ends_with("nixon"));
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
        assert_eq!(candidates[0].plain(), "/opt/thing");
        assert_eq!(candidates[0].value, "/opt/thing");
    }
}
