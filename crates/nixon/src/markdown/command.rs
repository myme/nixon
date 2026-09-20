//! Turning a command heading and the nodes after it into a command.

use super::MarkdownError;
use super::extract::Node;
use crate::command::{Command, DescSpan, Description, option_name, parse_args, parse_command_name};

/// Parses one command, returning it and the nodes it did not consume.
///
/// Paragraphs and container ends between the heading and the code block are
/// skipped; the first paragraph becomes the description. Unlike v1, a
/// non-paragraph block between the two no longer fails the whole file.
pub fn parse_command<'n>(
    file: &str,
    line: usize,
    heading: &str,
    project_types: Vec<String>,
    nodes: &'n [Node],
) -> Result<(Command, &'n [Node]), MarkdownError> {
    let fail = |message: String| MarkdownError::new(file, Some(line), message);

    let (name, header_args, mut options) =
        parse_command_name(heading).map_err(|err| fail(err.to_string()))?;

    let mut desc: Option<Description> = None;
    let mut declarations: Vec<Declaration> = Vec::new();
    let mut rest = nodes;

    loop {
        match rest.first() {
            Some(Node::Paragraph(paragraph)) => {
                if desc.is_none() {
                    desc = Some(paragraph.clone());
                }
                rest = &rest[1..];
            }
            Some(Node::ListItem(item)) => {
                if let Some(declaration) = parse_declaration(item) {
                    declarations.push(declaration.map_err(&fail)?);
                }
                rest = &rest[1..];
            }
            Some(Node::End { .. }) => rest = &rest[1..],
            _ => break,
        }
    }

    let Some(Node::Source {
        lang, attrs, text, ..
    }) = rest.first()
    else {
        return Err(fail(format!("Expecting source block for {heading}")));
    };

    let (source_args, source_options) =
        parse_args(&attrs.join(" ")).map_err(|err| fail(err.to_string()))?;

    if !header_args.is_empty() && !source_args.is_empty() {
        return Err(fail(format!(
            "{file}:{line} {name} uses placeholders in both command header and source code block"
        )));
    }

    let args = if header_args.is_empty() {
        options = source_options;
        source_args
    } else {
        header_args
    };

    for declaration in declarations {
        let Some(option) = options
            .iter_mut()
            .find(|option| option.name == declaration.name)
        else {
            return Err(fail(format!("Undeclared option: {}", declaration.name)));
        };
        option.default = declaration.default;
        option.description = declaration.description;
    }

    let command = Command {
        is_hidden: name.starts_with('_'),
        name,
        desc,
        lang: lang.clone(),
        project_types,
        source: text.clone(),
        args,
        options,
        ..Command::default()
    };
    Ok((command, &rest[1..]))
}

/// What a declaration list item says about one option.
struct Declaration {
    name: String,
    default: bool,
    description: Option<Description>,
}

/// Reads an option declaration from a list item, if it is one.
///
/// The shape is `` `--token`: on — what it does ``. A list item that does not
/// start with inline code followed by `:` is ordinary prose and is ignored.
fn parse_declaration(item: &Description) -> Option<Result<Declaration, String>> {
    let (first, rest) = item.spans.split_first()?;
    let DescSpan::Code(token) = first else {
        return None;
    };
    let name = option_name(token)?;

    let (head, tail) = rest.split_first()?;
    let DescSpan::Text(text) = head else {
        return None;
    };
    let text = text.strip_prefix(':')?;

    let (value, description) = split_description(text, tail);
    let default = match value.trim() {
        "on" => true,
        "off" => false,
        other => {
            return Some(Err(format!(
                "Option {token} is set to {other}; only on and off are supported"
            )));
        }
    };
    Some(Ok(Declaration {
        name,
        default,
        description,
    }))
}

/// Splits a declaration's value from the description after its dash.
fn split_description(text: &str, tail: &[DescSpan]) -> (String, Option<Description>) {
    for separator in [" — ", " - "] {
        if let Some((value, description)) = text.split_once(separator) {
            let mut spans = vec![DescSpan::Text(description.to_owned())];
            spans.extend_from_slice(tail);
            return (value.to_owned(), Some(Description::new(spans)));
        }
    }

    // No dash: the value is everything, and any trailing spans are part of it.
    let mut value = text.to_owned();
    for span in tail {
        value.push_str(span.text());
    }
    (value, None)
}
