//! The command/config state machine and location tracking. SPEC §4.4, §4.6.

use super::MarkdownError;
use super::command::parse_command;
use super::extract::Node;
use crate::command::{Command, CommandLocation};
use crate::config::{Config, parse_block};

/// What one markdown file contributes: at most one config block, and commands.
#[derive(Debug, Default)]
pub struct ParsedFile {
    /// The config block, if the file has one.
    pub config: Option<Config>,
    /// Commands in document order.
    pub commands: Vec<Command>,
}

/// One enclosing heading and the project types it contributes. SPEC §4.4.
struct Frame {
    level: usize,
    types: Vec<String>,
}

/// The command whose location has not been closed yet. SPEC §4.6.
struct Pending {
    line: usize,
    level: usize,
}

/// Walks the flattened nodes. SPEC §4.4.
pub fn walk(file: &str, nodes: &[Node]) -> Result<ParsedFile, MarkdownError> {
    let mut parsed = ParsedFile::default();
    let mut stack: Vec<Frame> = Vec::new();
    let mut pending: Option<Pending> = None;
    let mut rest = nodes;

    while let Some(node) = rest.first() {
        rest = &rest[1..];
        match node {
            Node::End { line } => {
                close_location(file, &mut parsed.commands, pending.as_ref(), *line);
            }

            Node::Head { line, level, attrs } => {
                close_location(file, &mut parsed.commands, pending.as_ref(), *line);

                // Any heading scopes its `type=` to everything nested beneath
                // it, until a sibling or shallower heading. SPEC §4.4 BUG: v1
                // only did this for command headings.
                stack.retain(|frame| frame.level < *level);
                let inherited: Vec<String> =
                    stack.iter().flat_map(|frame| frame.types.clone()).collect();
                let own = attrs.kwarg_values("type");
                stack.push(Frame {
                    level: *level,
                    types: own.clone(),
                });

                if attrs.has_arg("config") {
                    let (config, remaining) = parse_config(file, *line, rest)?;
                    set_config(file, *line, &mut parsed, config)?;
                    rest = remaining;
                } else if attrs.has_arg("command") {
                    let mut types = own;
                    types.extend(inherited);
                    let (mut command, remaining) =
                        parse_command(file, *line, &attrs.name, types, rest)?;
                    command.is_bg = attrs.has_arg("bg");
                    parsed.commands.push(command);
                    pending = Some(Pending {
                        line: *line,
                        level: *level,
                    });
                    rest = remaining;
                }
            }

            Node::Source { attrs, .. } if attrs.iter().any(|attr| attr == "config") => {
                let nodes = std::slice::from_ref(node);
                let (config, _) = parse_config(file, 0, nodes)?;
                set_config(file, 0, &mut parsed, config)?;
            }

            _ => {}
        }
    }

    Ok(parsed)
}

/// Closes the most recent command's location, first write wins. SPEC §4.6.
fn close_location(file: &str, commands: &mut [Command], pending: Option<&Pending>, next: usize) {
    let Some(pending) = pending else { return };
    let Some(command) = commands.last_mut() else {
        return;
    };
    if command.location.is_some() {
        return;
    }
    command.location = Some(CommandLocation {
        file_path: file.into(),
        start_line: pending.line,
        end_line: next.saturating_sub(1),
        level: pending.level,
    });
}

/// A config heading must be followed immediately by a code block. SPEC §3.4.
fn parse_config<'n>(
    file: &str,
    line: usize,
    nodes: &'n [Node],
) -> Result<(Config, &'n [Node]), MarkdownError> {
    let Some(Node::Source { lang, text, .. }) = nodes.first() else {
        return Err(MarkdownError::new(
            file,
            Some(line),
            "Expecting config source after header".to_owned(),
        ));
    };
    let config = parse_block(&lang.to_string(), text)
        .map_err(|err| MarkdownError::new(file, Some(line), err.to_string()))?;
    Ok((config, &nodes[1..]))
}

/// Exactly one config block per file. SPEC §3.4.
fn set_config(
    file: &str,
    line: usize,
    parsed: &mut ParsedFile,
    config: Config,
) -> Result<(), MarkdownError> {
    if parsed.config.is_some() {
        return Err(MarkdownError::new(
            file,
            Some(line),
            "Found multiple configuration blocks".to_owned(),
        ));
    }
    parsed.config = Some(config);
    Ok(())
}
