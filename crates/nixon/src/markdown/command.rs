//! Turning a command heading and the nodes after it into a command. SPEC §4.5.

use super::MarkdownError;
use super::extract::Node;
use crate::command::{Command, parse_command_name};
use crate::placeholder::scan_all;

/// Parses one command, returning it and the nodes it did not consume.
///
/// Paragraphs and container ends between the heading and the code block are
/// skipped; the first paragraph becomes the description. SPEC §4.5 plus the
/// §7.3 fix that non-paragraph blocks no longer fail the whole file.
pub fn parse_command<'n>(
    file: &str,
    line: usize,
    heading: &str,
    project_types: Vec<String>,
    nodes: &'n [Node],
) -> Result<(Command, &'n [Node]), MarkdownError> {
    let mut desc: Option<String> = None;
    let mut rest = nodes;

    loop {
        match rest.first() {
            Some(Node::Paragraph(text)) => {
                if desc.is_none() {
                    desc = Some(text.trim().to_owned());
                }
                rest = &rest[1..];
            }
            Some(Node::End { .. }) => rest = &rest[1..],
            _ => break,
        }
    }

    let Some(Node::Source { lang, attrs, text }) = rest.first() else {
        return Err(MarkdownError::new(
            file,
            Some(line),
            format!("Expecting source block for {heading}"),
        ));
    };

    let (name, header_placeholders) = parse_command_name(heading)
        .map_err(|err| MarkdownError::new(file, Some(line), err.to_string()))?;
    let source_placeholders = scan_all(&attrs.join(" "))
        .map_err(|err| MarkdownError::new(file, Some(line), err.to_string()))?;

    if !header_placeholders.is_empty() && !source_placeholders.is_empty() {
        return Err(MarkdownError::new(
            file,
            Some(line),
            format!(
                "{file}:{line} {name} uses placeholders in both command header and source code block"
            ),
        ));
    }

    let mut placeholders = header_placeholders;
    placeholders.extend(source_placeholders);

    let command = Command {
        is_hidden: name.starts_with('_'),
        name,
        desc,
        lang: lang.clone(),
        project_types,
        source: text.clone(),
        placeholders,
        ..Command::default()
    };
    Ok((command, &rest[1..]))
}
