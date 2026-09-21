//! The effective configuration and how its sources combine.

pub mod load;
pub mod schema;
pub mod yaml;

use std::path::PathBuf;

use crate::command::Command;
use crate::project::ProjectType;

/// Verbosity, ordered so a message is emitted iff its level is at least the
/// configured one.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum LogLevel {
    /// Everything.
    Debug,
    /// Progress worth narrating.
    Info,
    /// The default.
    Warning,
    /// Failures only.
    Error,
}

/// Why a config could not be loaded.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ConfigError {
    /// The config file does not exist. Tolerated for the global config in v2
    /// in v2; v1 treated it as fatal.
    #[error("no such file")]
    NoSuchFile,
    /// The file is empty or only whitespace.
    #[error("empty file")]
    EmptyFile,
    /// The markdown, JSON or YAML could not be parsed.
    #[error("{0}")]
    ParseError(String),
    /// A markdown config file failed to parse, with its position.
    #[error("{0}")]
    Markdown(#[from] crate::markdown::MarkdownError),
}

/// The effective configuration.
///
/// `backend`, `force_tty` and `terminal` are gone with the backend concept.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Config {
    /// Directories of executables offered as commands.
    pub bin_dirs: Vec<PathBuf>,
    /// Exact rather than fuzzy matching in the picker.
    pub exact_match: Option<bool>,
    /// Case-insensitive matching in the picker.
    pub ignore_case: Option<bool>,
    /// Where to look for projects.
    pub project_dirs: Vec<PathBuf>,
    /// Project type definitions.
    pub project_types: Vec<ProjectType>,
    /// Commands parsed from markdown.
    pub commands: Vec<Command>,
    /// Wrap commands in `direnv exec`.
    pub use_direnv: Option<bool>,
    /// Wrap commands in `nix-shell`.
    pub use_nix: Option<bool>,
    /// Verbosity.
    pub loglevel: Option<LogLevel>,
    /// Whether discovery also finds git worktrees.
    pub git_worktrees: Option<bool>,
    /// Whether executed commands are recorded.
    pub history: Option<bool>,
}

impl Config {
    /// The built-in defaults every other source merges on top of.
    pub fn defaults() -> Self {
        Self {
            loglevel: Some(LogLevel::Warning),
            // Worktree discovery is on unless turned off.
            git_worktrees: Some(true),
            history: Some(true),
            ..Self::default()
        }
    }

    /// Whether to look for git worktrees.
    pub fn finds_worktrees(&self) -> bool {
        self.git_worktrees.unwrap_or(true)
    }

    /// Whether to record what was run.
    pub fn records_history(&self) -> bool {
        self.history.unwrap_or(true)
    }

    /// Merges `rhs` over `self`.
    ///
    /// Options take the right-hand value when it is set; path and type lists
    /// concatenate left-then-right; commands concatenate **right first**, so
    /// local commands shadow global ones in the first-match lookups of §5.5.
    #[must_use]
    pub fn merge(mut self, rhs: Self) -> Self {
        self.bin_dirs.extend(rhs.bin_dirs);
        self.project_dirs.extend(rhs.project_dirs);
        self.project_types.extend(rhs.project_types);

        let mut commands = rhs.commands;
        commands.append(&mut self.commands);
        self.commands = commands;

        self.exact_match = rhs.exact_match.or(self.exact_match);
        self.ignore_case = rhs.ignore_case.or(self.ignore_case);
        self.use_direnv = rhs.use_direnv.or(self.use_direnv);
        self.use_nix = rhs.use_nix.or(self.use_nix);
        self.loglevel = rhs.loglevel.or(self.loglevel);
        self.git_worktrees = rhs.git_worktrees.or(self.git_worktrees);
        self.history = rhs.history.or(self.history);
        self
    }
}

impl From<schema::ConfigBlock> for Config {
    fn from(block: schema::ConfigBlock) -> Self {
        Self {
            bin_dirs: block.bin_dirs,
            exact_match: block.exact_match,
            ignore_case: block.ignore_case,
            project_dirs: block.project_dirs,
            project_types: block.project_types.into_iter().map(Into::into).collect(),
            commands: Vec::new(),
            use_direnv: block.use_direnv,
            use_nix: block.use_nix,
            loglevel: None,
            git_worktrees: block.git_worktrees,
            history: block.history,
        }
    }
}

/// Parses a config block: `json` or no language is JSON, `yaml` is
/// YAML, anything else is an error.
pub fn parse_block(lang: &str, source: &str) -> Result<Config, ConfigError> {
    let block: schema::ConfigBlock = match lang {
        "json" | "" => {
            serde_json::from_str(source).map_err(|err| ConfigError::ParseError(err.to_string()))?
        }
        "yaml" => yaml::from_str(source).map_err(ConfigError::ParseError)?,
        other => {
            return Err(ConfigError::ParseError(format!(
                "Invalid config language: {other}"
            )));
        }
    };
    Ok(block.into())
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use proptest::prelude::*;

    use super::{Config, ConfigError, LogLevel, parse_block};
    use crate::command::Command;
    use crate::project::{ProjectMarker, ProjectType};

    fn command(name: &str) -> Command {
        Command {
            name: name.to_owned(),
            ..Command::default()
        }
    }

    fn named(config: &Config) -> Vec<&str> {
        config.commands.iter().map(|c| c.name.as_str()).collect()
    }

    #[test]
    fn an_empty_json_object_is_valid_and_all_default() {
        assert_eq!(parse_block("json", "{}"), Ok(Config::default()));
    }

    #[test]
    fn a_missing_language_is_json() {
        assert_eq!(
            parse_block("", r#"{"use_nix": true}"#).map(|c| c.use_nix),
            Ok(Some(true))
        );
    }

    #[test]
    fn yaml_is_parsed_as_yaml() {
        let parsed = parse_block("yaml", "use_direnv: true\nbin_dirs: [bin]\n").unwrap();
        assert_eq!(parsed.use_direnv, Some(true));
        assert_eq!(parsed.bin_dirs, vec![PathBuf::from("bin")]);
    }

    #[test]
    fn any_other_language_is_rejected() {
        assert_eq!(
            parse_block("toml", "x = 1"),
            Err(ConfigError::ParseError(
                "Invalid config language: toml".to_owned()
            ))
        );
    }

    #[test]
    fn unknown_keys_are_ignored() {
        let parsed = parse_block("json", r#"{"terminal": "kitty", "nonsense": 1}"#);
        assert_eq!(parsed, Ok(Config::default()));
    }

    #[test]
    fn project_types_become_path_markers() {
        let parsed = parse_block(
            "json",
            r#"{"project_types": [{"name": "git", "test": [".git"], "desc": "Git"}]}"#,
        )
        .unwrap();
        assert_eq!(
            parsed.project_types,
            vec![ProjectType {
                id: "git".to_owned(),
                markers: vec![ProjectMarker::Path(PathBuf::from(".git"))],
                description: "Git".to_owned(),
            }]
        );
    }

    #[test]
    fn a_project_type_without_test_has_no_markers() {
        let parsed = parse_block(
            "json",
            r#"{"project_types": [{"name": "any", "desc": "Generic"}]}"#,
        )
        .unwrap();
        assert!(parsed.project_types[0].markers.is_empty());
    }

    #[test]
    fn a_project_type_without_desc_is_a_parse_error() {
        assert!(parse_block("json", r#"{"project_types": [{"name": "git"}]}"#).is_err());
    }

    #[test]
    fn a_project_type_without_name_is_a_parse_error() {
        assert!(parse_block("json", r#"{"project_types": [{"desc": "Git"}]}"#).is_err());
    }

    #[test]
    fn options_take_the_right_hand_value_when_set() {
        let lhs = Config {
            use_nix: Some(true),
            exact_match: Some(true),
            ..Config::default()
        };
        let rhs = Config {
            use_nix: Some(false),
            ..Config::default()
        };
        let merged = lhs.merge(rhs);
        assert_eq!(merged.use_nix, Some(false));
        assert_eq!(merged.exact_match, Some(true));
    }

    #[test]
    fn path_lists_concatenate_left_then_right_without_dedupe() {
        let lhs = Config {
            project_dirs: vec![PathBuf::from("a"), PathBuf::from("b")],
            ..Config::default()
        };
        let rhs = Config {
            project_dirs: vec![PathBuf::from("b"), PathBuf::from("c")],
            ..Config::default()
        };
        assert_eq!(
            lhs.merge(rhs).project_dirs,
            ["a", "b", "b", "c"].map(PathBuf::from).to_vec()
        );
    }

    #[test]
    fn local_commands_come_before_global_ones() {
        let global = Config {
            commands: vec![command("global-one"), command("global-two")],
            ..Config::default()
        };
        let local = Config {
            commands: vec![command("local-one")],
            ..Config::default()
        };
        assert_eq!(
            named(&global.merge(local)),
            ["local-one", "global-one", "global-two"]
        );
    }

    #[test]
    fn defaults_carry_the_warning_log_level() {
        assert_eq!(Config::defaults().loglevel, Some(LogLevel::Warning));
    }

    #[test]
    fn log_levels_are_ordered() {
        assert!(LogLevel::Debug < LogLevel::Info);
        assert!(LogLevel::Info < LogLevel::Warning);
        assert!(LogLevel::Warning < LogLevel::Error);
    }

    fn config_strategy() -> impl Strategy<Value = Config> {
        (
            prop::collection::vec("[a-z]{1,3}", 0..3),
            prop::option::of(any::<bool>()),
            prop::option::of(any::<bool>()),
            prop::collection::vec("[a-z]{1,3}", 0..3),
        )
            .prop_map(|(dirs, exact, nix, cmds)| Config {
                project_dirs: dirs.into_iter().map(PathBuf::from).collect(),
                exact_match: exact,
                use_nix: nix,
                commands: cmds.iter().map(|n| command(n)).collect(),
                ..Config::default()
            })
    }

    proptest! {
        #[test]
        fn merge_is_associative(
            a in config_strategy(),
            b in config_strategy(),
            c in config_strategy(),
        ) {
            let left = a.clone().merge(b.clone()).merge(c.clone());
            let right = a.merge(b.merge(c));
            prop_assert_eq!(left, right);
        }

        #[test]
        fn merging_defaults_on_the_left_keeps_the_right(config in config_strategy()) {
            let merged = Config::default().merge(config.clone());
            prop_assert_eq!(merged, config);
        }
    }
}
