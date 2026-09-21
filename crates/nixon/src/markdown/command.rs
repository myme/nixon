//! Turning a command heading and the nodes after it into a command.

use super::MarkdownError;
use super::extract::Node;
use crate::command::{
    ArgSpec, Command, CommandOption, DescSpan, Description, option_name, parse_args,
    parse_command_name,
};

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

    let (name, header_args, header_options) =
        parse_command_name(heading).map_err(|err| fail(err.to_string()))?;

    let mut desc: Option<Description> = None;
    let mut items: Vec<Description> = Vec::new();
    let mut rest = nodes;

    loop {
        match rest.first() {
            Some(Node::Paragraph(paragraph)) => {
                if desc.is_none() {
                    desc = Some(paragraph.clone());
                }
                rest = &rest[1..];
            }
            // Kept until the options are known: whether an item declares
            // one depends on what was declared.
            Some(Node::ListItem(item)) => {
                items.push(item.clone());
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

    // Placeholders belong in one place or the other; options may be in
    // either, since a heading names what the user toggles and an info
    // string names what the source reads.
    if header_args.iter().any(is_placeholder) && source_args.iter().any(is_placeholder) {
        return Err(fail(format!(
            "{file}:{line} {name} uses placeholders in both command header and source code block"
        )));
    }

    let (args, options) = combine(header_args, header_options, source_args, source_options)
        .map_err(|name| fail(format!("Duplicate option: {name}")))?;

    let mut options = options;
    for item in &items {
        let Some(declaration) = parse_declaration(item, &options) else {
            continue;
        };
        let declaration = declaration.map_err(&fail)?;
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

/// Whether an argument is a placeholder rather than an option.
const fn is_placeholder(arg: &ArgSpec) -> bool {
    matches!(arg, ArgSpec::Placeholder(_))
}

/// Joins the heading's arguments with the info string's, heading first.
///
/// Option indices from the info string shift past the heading's, and a name
/// declared in both places is an error as it is within one.
fn combine(
    header_args: Vec<ArgSpec>,
    header_options: Vec<CommandOption>,
    source_args: Vec<ArgSpec>,
    source_options: Vec<CommandOption>,
) -> Result<(Vec<ArgSpec>, Vec<CommandOption>), String> {
    if let Some(clash) = source_options
        .iter()
        .find(|option| header_options.iter().any(|other| other.name == option.name))
    {
        return Err(clash.name.clone());
    }

    let shift = header_options.len();
    let mut args = header_args;
    args.extend(source_args.into_iter().map(|arg| match arg {
        ArgSpec::Option(index) => ArgSpec::Option(index + shift),
        placeholder @ ArgSpec::Placeholder(_) => placeholder,
    }));

    let mut options = header_options;
    options.extend(source_options);
    Ok((args, options))
}

/// What a declaration list item says about one option.
struct Declaration {
    name: String,
    default: bool,
    description: Option<Description>,
}

/// Reads an option declaration from a list item, if it is one.
///
/// The shape is `` `--token`: on — what it does ``. An item is a declaration
/// only when its token names an option the command declared, or its value is
/// literally `on` or `off` — so `` - `-v`: increases verbosity `` is prose,
/// while `` - `-v`: on `` with no `-v` anywhere is a mistake worth reporting.
fn parse_declaration(
    item: &Description,
    options: &[CommandOption],
) -> Option<Result<Declaration, String>> {
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
    let known = options.iter().any(|option| option.name == name);
    let default = match value.trim() {
        "on" => true,
        "off" => false,
        // Prose unless the command has an option by that name, in which
        // case the author meant a declaration and got the value wrong.
        other if known => {
            return Some(Err(format!(
                "Option {token} is set to {other}; only on and off are supported"
            )));
        }
        _ => return None,
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
