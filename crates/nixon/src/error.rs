//! Typed errors and their exit codes. ENGINEERING §6, §7.2.

use crate::config::ConfigError;
use crate::markdown::MarkdownError;
use crate::placeholder::ParseError;

/// The result of anything that can fail in nixon.
pub type Result<T> = std::result::Result<T, NixonError>;

/// Exit code for a cancelled selection. ENGINEERING §7.2.
pub const CANCELED: i32 = 130;

/// Everything that can go wrong. SPEC §10.8.
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

    /// A language with no interpreter. SPEC §7.1.
    #[error("No interpreter for {language}")]
    NoInterpreter {
        /// The language as written in the info string.
        language: String,
    },

    /// A placeholder referenced a command that does not exist.
    ///
    /// SPEC §5.6 has v1 calling `error` here; v2 reports it.
    #[error("Invalid argument: {name}")]
    UnknownCommand {
        /// The name the placeholder referenced.
        name: String,
    },

    /// A `| json` placeholder's command produced output that is not a JSON
    /// array of candidates. SPEC §5.6; v1 panicked.
    #[error("Invalid JSON candidates from {name}: {source}")]
    InvalidJson {
        /// The command that produced the output.
        name: String,
        /// What the JSON parser said.
        source: serde_json::Error,
    },

    /// Nothing was selected. SPEC §10.7.
    #[error("{0}")]
    NothingSelected(String),

    /// A selection was cancelled. Exits 130. ENGINEERING §7.2.
    #[error("Selection canceled.")]
    Canceled,

    /// An interactive selection was needed without a terminal.
    /// ENGINEERING §7.2.
    #[error("interactive selection needs a terminal")]
    NoTerminal,

    /// Anything the filesystem or a child process reported.
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

impl NixonError {
    /// The process exit code for this error. SPEC §10.8, ENGINEERING §7.2.
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
