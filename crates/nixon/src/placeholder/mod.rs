//! Placeholders: a command referencing another command as a source of
//! selectable values.

pub mod grammar;

use std::fmt;

pub use grammar::{ParseError, parse_one, scan_all};

/// How a resolved placeholder reaches the command.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PlaceholderType {
    /// `${name}` — each selected line becomes a positional argument.
    Arg,
    /// `={name}` or `ALIAS={name}` — selection joined by spaces into this var.
    EnvVar(String),
    /// `<{name}` — selection is piped to the command's stdin.
    Stdin,
}

/// How the referenced command's output is turned into candidates.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum PlaceholderFormat {
    /// Positional columns; `cols+h` drops the header row. Indices are 1-based.
    Columns {
        /// Whether the first row is a header.
        has_header: bool,
        /// Which columns to keep, 1-based.
        cols: Vec<usize>,
    },
    /// Whitespace-separated fields, 1-based.
    Fields(Vec<usize>),
    /// Each output line is one candidate.
    #[default]
    Lines,
    /// Output parsed as a JSON array of candidates.
    Json,
}

/// A reference to another command, and how to select from its output.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Placeholder {
    /// How the value reaches the command.
    pub kind: PlaceholderType,
    /// Name of the command being referenced.
    pub name: String,
    /// How to read that command's output.
    pub format: PlaceholderFormat,
    /// Query applied to the candidates before selection.
    pub filter: Option<String>,
    /// Only list the candidates instead of selecting interactively.
    pub list: bool,
    /// Allow selecting more than one candidate.
    pub multiple: bool,
    /// Pre-expanded value; when set, no command is run and nothing is selected.
    pub value: Vec<String>,
}

impl Placeholder {
    /// Builds a placeholder with the grammar's defaults.
    pub fn new(kind: PlaceholderType, name: impl Into<String>) -> Self {
        Self {
            kind,
            name: name.into(),
            format: PlaceholderFormat::Lines,
            filter: None,
            list: false,
            multiple: false,
            value: Vec::new(),
        }
    }

    /// Whether candidates can be built from output a line at a time.
    ///
    /// Columns need every row before the widths are known, and JSON needs the
    /// whole document, so those wait for the command to finish.
    pub const fn can_stream(&self) -> bool {
        matches!(
            self.format,
            PlaceholderFormat::Lines | PlaceholderFormat::Fields(_)
        )
    }

    /// Builds an already-resolved placeholder, as overflow CLI args are.
    ///
    pub fn with_value(kind: PlaceholderType, name: impl Into<String>, value: Vec<String>) -> Self {
        Self {
            value,
            ..Self::new(kind, name)
        }
    }
}

/// Renders a placeholder back into the source form the grammar accepts.
impl fmt::Display for Placeholder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            PlaceholderType::Arg => f.write_str("${")?,
            PlaceholderType::Stdin => f.write_str("<{")?,
            PlaceholderType::EnvVar(alias) => write!(f, "{alias}={{")?,
        }
        f.write_str(&self.name)?;
        match &self.format {
            PlaceholderFormat::Lines => {}
            PlaceholderFormat::Fields(ns) => write!(f, " | fields {}", join(ns))?,
            PlaceholderFormat::Json => f.write_str(" | json")?,
            PlaceholderFormat::Columns { has_header, cols } => {
                let header = if *has_header { "+h" } else { "" };
                write!(f, " | cols{header} {}", join(cols))?;
            }
        }
        if let Some(filter) = &self.filter {
            write!(f, " | filter \"{filter}\"")?;
        }
        if self.list {
            f.write_str(" | list")?;
        }
        if self.multiple {
            f.write_str(" | multi")?;
        }
        f.write_str("}")
    }
}

fn join(ns: &[usize]) -> String {
    ns.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",")
}
