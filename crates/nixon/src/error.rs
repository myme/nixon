//! Typed errors and their exit codes.

use crate::config::ConfigError;
use crate::markdown::MarkdownError;
use crate::placeholder::ParseError;

/// The result of anything that can fail in nixon.
pub type Result<T> = std::result::Result<T, NixonError>;

/// Exit code for a cancelled selection.
pub const CANCELED: i32 = 130;

/// Everything that can go wrong.
#[derive(Debug, thiserror::Error)]
pub enum NixonError {
    /// A config file could not be read or parsed.
    #[error(transparent)]
    Config(#[from] ConfigError),

    /// A markdown file could not be parsed.
    #[error(transparent)]
    Markdown(#[from] MarkdownError),

    /// A placeholder on the command line could not be parsed.
    #[error(transparent)]
    Placeholder(#[from] ParseError),

    /// A language with no interpreter.
    #[error("No interpreter for {language}")]
    NoInterpreter {
        /// The language as written in the info string.
        language: String,
    },

    /// A placeholder referenced a command that does not exist.
    ///
    /// v1 called `error` here; v2 reports it.
    #[error("Invalid argument: {name}")]
    UnknownCommand {
        /// The name the placeholder referenced.
        name: String,
    },

    /// A `| json` placeholder's command produced output that is not a JSON
    /// array of candidates. v1 panicked.
    #[error("Invalid JSON candidates from {name}: {source}")]
    InvalidJson {
        /// The command that produced the output.
        name: String,
        /// What the JSON parser said.
        source: serde_json::Error,
    },

    /// A placeholder's command produced nothing to choose from.
    #[error("no candidates from `{name}`{}", filtered(.filter.as_deref()))]
    NoCandidates {
        /// The command the placeholder referenced.
        name: String,
        /// The `| filter` query, when one narrowed the list away.
        filter: Option<String>,
    },

    /// A project was named by a path that is not a directory.
    #[error("no such project: {path}")]
    NoSuchProject {
        /// The path as it was given, with `~` expanded.
        path: String,
    },

    /// Nothing was selected.
    #[error("{0}")]
    NothingSelected(String),

    /// A selection was cancelled. Exits 130.
    #[error("Selection canceled.")]
    Canceled,

    /// An interactive selection was needed without a terminal.
    ///
    #[error("interactive selection needs a terminal")]
    NoTerminal,

    /// Anything the filesystem or a child process reported.
    #[error(transparent)]
    Io(std::io::Error),
}

/// The picker reports "no terminal" as `NotConnected`, which is otherwise
/// unreachable here: nixon opens files and pipes, never sockets.
impl From<std::io::Error> for NixonError {
    fn from(err: std::io::Error) -> Self {
        if err.kind() == std::io::ErrorKind::NotConnected {
            Self::NoTerminal
        } else {
            Self::Io(err)
        }
    }
}

/// The "matching …" tail, when a filter is what emptied the list.
fn filtered(filter: Option<&str>) -> String {
    filter.map_or_else(String::new, |query| format!(" matching `{query}`"))
}

impl NixonError {
    /// The process exit code for this error.
    pub const fn exit_code(&self) -> i32 {
        match self {
            Self::Canceled => CANCELED,
            _ => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{CANCELED, NixonError};

    #[test]
    fn cancelling_exits_130_and_everything_else_exits_1() {
        assert_eq!(NixonError::Canceled.exit_code(), CANCELED);
        assert_eq!(
            NixonError::UnknownCommand {
                name: "nope".to_owned()
            }
            .exit_code(),
            1
        );
    }

    #[test]
    fn a_picker_without_a_terminal_becomes_a_named_error() {
        let err = NixonError::from(nixon_picker::terminal::no_terminal());
        assert!(matches!(err, NixonError::NoTerminal));
        assert_eq!(err.to_string(), "interactive selection needs a terminal");
        assert_eq!(err.exit_code(), 1);
    }

    #[test]
    fn other_io_errors_are_left_alone() {
        let err = NixonError::from(std::io::Error::from(std::io::ErrorKind::NotFound));
        assert!(matches!(err, NixonError::Io(_)));
    }

    #[test]
    fn errors_render_without_haskell_show_quoting() {
        assert_eq!(
            NixonError::NothingSelected("No command selected.".to_owned()).to_string(),
            "No command selected."
        );
        assert_eq!(
            NixonError::UnknownCommand {
                name: "git-files".to_owned()
            }
            .to_string(),
            "Invalid argument: git-files"
        );
        assert_eq!(
            NixonError::NoInterpreter {
                language: "ruby".to_owned()
            }
            .to_string(),
            "No interpreter for ruby"
        );
    }
}
