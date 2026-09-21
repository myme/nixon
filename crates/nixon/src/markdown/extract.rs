//! Flattens the comrak AST into the node list the walker consumes.

use comrak::nodes::{AstNode, NodeValue};

use super::header::{HeaderArgs, parse_header_args};
use crate::command::{DescSpan, Description};
use crate::language::Language;

/// One flattened markdown node.
#[derive(Clone, Debug)]
pub enum Node {
    /// A heading, with its attributes and the line it starts on.
    Head {
        /// 1-based line of the heading.
        line: usize,
        /// Heading level.
        level: usize,
        /// Parsed attribute block, plus the implicit `command`/`bg` flags.
        attrs: HeaderArgs,
    },
    /// A fenced code block.
    Source {
        /// 1-based line the fence starts on.
        line: usize,
        /// Language from the info string's first word.
        lang: Language,
        /// Remaining words of the info string.
        attrs: Vec<String>,
        /// The block's contents, including its trailing newline.
        text: String,
    },
    /// A paragraph, with its inline code kept apart.
    Paragraph(Description),
    /// One item of a list, flattened to its text.
    ListItem(Description),
    /// Closes a container; carries the line after the container's last.
    End {
        /// One past the container's last line.
        line: usize,
    },
}

/// Flattens a document.
pub fn extract<'a>(node: &'a AstNode<'a>) -> Vec<Node> {
    let data = node.data.borrow();
    let pos = data.sourcepos;

    match &data.value {
        NodeValue::Heading(heading) => {
            let children: Vec<_> = node.children().collect();
            let mut attrs = parse_header_args(&get_text(&children));

            // A heading is a command if it contains inline code; the first such
            // child ending in `&` marks it as a background command.
            let first_code = children
                .iter()
                .find_map(|child| match &child.data.borrow().value {
                    NodeValue::Code(code) => Some(code.literal.clone()),
                    _ => None,
                });
            if let Some(code) = &first_code {
                if code.trim_end().ends_with('&') && !attrs.has_arg("bg") {
                    attrs.args.insert(0, "bg".to_owned());
                }
                if !attrs.has_arg("command") {
                    let at = usize::from(attrs.has_arg("bg"));
                    attrs.args.insert(at, "command".to_owned());
                }
            }

            vec![Node::Head {
                line: pos.start.line,
                level: heading.level as usize,
                attrs,
            }]
        }
        NodeValue::CodeBlock(block) => {
            let mut words = block.info.split_whitespace();
            let (lang, attrs) = words.next().map_or_else(
                || (Language::None, Vec::new()),
                |first| {
                    (
                        Language::from(first),
                        words.map(ToOwned::to_owned).collect(),
                    )
                },
            );
            vec![Node::Source {
                line: pos.start.line,
                lang,
                attrs,
                text: block.literal.clone(),
            }]
        }
        NodeValue::Paragraph => {
            let children: Vec<_> = node.children().collect();
            vec![Node::Paragraph(description(&children))]
        }
        // A list item is one line as far as nixon is concerned: it is where
        // a command declares an option.
        NodeValue::Item(_) | NodeValue::TaskItem(_) => {
            let children: Vec<_> = node.children().collect();
            vec![Node::ListItem(description(&children))]
        }
        _ => {
            let mut nodes: Vec<Node> = node.children().flat_map(extract).collect();
            nodes.push(Node::End {
                line: pos.end.line + 1,
            });
            nodes
        }
    }
}

/// The text of a node list, concatenated as written.
///
/// v1 joined the pieces with a space, which put two spaces either side of
/// every inline code span. That was reproduced on purpose and is visible in
/// `--list`, so it is gone.
fn get_text<'a>(nodes: &[&'a AstNode<'a>]) -> String {
    description(nodes).plain()
}

/// A node list as description spans, inline code kept apart.
fn description<'a>(nodes: &[&'a AstNode<'a>]) -> Description {
    let mut spans = Vec::new();
    collect(nodes, &mut spans);
    Description::new(spans)
}

fn collect<'a>(nodes: &[&'a AstNode<'a>], out: &mut Vec<DescSpan>) {
    for node in nodes {
        if let NodeValue::Code(code) = &node.data.borrow().value {
            out.push(DescSpan::Code(code.literal.clone()));
            continue;
        }
        if let Some(text) = own_text(node) {
            out.push(DescSpan::Text(text));
        }
        let children: Vec<_> = node.children().collect();
        collect(&children, out);
    }
}

/// The text a single node contributes; containers contribute nothing.
fn own_text<'a>(node: &'a AstNode<'a>) -> Option<String> {
    match &node.data.borrow().value {
        NodeValue::Text(text) => Some(text.to_string()),
        NodeValue::HtmlInline(html) => Some(html.clone()),
        NodeValue::HtmlBlock(block) => Some(block.literal.clone()),
        NodeValue::CodeBlock(block) => Some(block.literal.clone()),
        // A wrapped line is one space, not a join of two words.
        NodeValue::SoftBreak | NodeValue::LineBreak => Some(" ".to_owned()),
        _ => None,
    }
}
