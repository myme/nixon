//! Flattens the comrak AST into the node list the walker consumes. SPEC §4.2.

use comrak::nodes::{AstNode, NodeValue};

use super::header::{HeaderArgs, parse_header_args};
use crate::language::Language;

/// One flattened markdown node. SPEC §4.2.
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
    /// A paragraph's text.
    Paragraph(String),
    /// Closes a container; carries the line after the container's last.
    End {
        /// One past the container's last line.
        line: usize,
    },
}

/// Flattens a document. SPEC §4.2.
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
            vec![Node::Paragraph(get_text(&children))]
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

/// Concatenates the text of a node list, one space between every node. SPEC §4.2.
fn get_text<'a>(nodes: &[&'a AstNode<'a>]) -> String {
    let Some((node, rest)) = nodes.split_first() else {
        return String::new();
    };
    let children: Vec<_> = node.children().collect();
    let parts = [own_text(node), get_text(&children), get_text(rest)];
    parts.join(" ").trim().to_owned()
}

/// The text a single node contributes; containers contribute nothing.
fn own_text<'a>(node: &'a AstNode<'a>) -> String {
    match &node.data.borrow().value {
        NodeValue::Text(text) => text.to_string(),
        NodeValue::HtmlInline(html) => html.clone(),
        NodeValue::HtmlBlock(block) => block.literal.clone(),
        NodeValue::Code(code) => code.literal.clone(),
        NodeValue::CodeBlock(block) => block.literal.clone(),
        _ => String::new(),
    }
}
