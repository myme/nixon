//! The Pandoc-style heading attribute block, `{.arg key="value"}`.

use winnow::ascii::space0;
use winnow::combinator::{alt, delimited, preceded, repeat};
use winnow::token::take_while;
use winnow::{ModalResult, Parser};

/// A heading split into its name, its flags and its key/value pairs.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct HeaderArgs {
    /// Everything before the attribute block, trimmed.
    pub name: String,
    /// `.foo` flags, without the dot.
    pub args: Vec<String>,
    /// `key=value` pairs, in order.
    pub kwargs: Vec<(String, String)>,
}

impl HeaderArgs {
    /// Whether a `.flag` is present.
    pub fn has_arg(&self, key: &str) -> bool {
        self.args.iter().any(|arg| arg == key)
    }

    /// Every value given for a key, in order. `type` may be repeated.
    pub fn kwarg_values(&self, key: &str) -> Vec<String> {
        self.kwargs
            .iter()
            .filter(|(k, _)| k == key)
            .map(|(_, v)| v.clone())
            .collect()
    }
}

/// Parses a heading's text. A heading that does not parse is all name.
pub fn parse_header_args(input: &str) -> HeaderArgs {
    // Anything after the attribute block is ignored rather than fatal, as in v1.
    let mut rest = input;
    header(&mut rest).unwrap_or_else(|_| HeaderArgs {
        name: input.to_owned(),
        args: Vec::new(),
        kwargs: Vec::new(),
    })
}

fn header(input: &mut &str) -> ModalResult<HeaderArgs> {
    let name = take_while(0.., |c| c != '{').parse_next(input)?;
    let items: Vec<Item> =
        delimited('{', repeat(0.., preceded(space0, item)), (space0, '}')).parse_next(input)?;

    let mut parsed = HeaderArgs {
        name: name.trim().to_owned(),
        ..HeaderArgs::default()
    };
    for item in items {
        match item {
            Item::Arg(arg) => parsed.args.push(arg),
            Item::Kwarg(key, value) => parsed.kwargs.push((key, value)),
        }
    }
    Ok(parsed)
}

#[derive(Clone)]
enum Item {
    Arg(String),
    Kwarg(String, String),
}

fn item(input: &mut &str) -> ModalResult<Item> {
    alt((arg, kwarg)).parse_next(input)
}

fn arg(input: &mut &str) -> ModalResult<Item> {
    preceded('.', identifier)
        .map(|id: &str| Item::Arg(id.to_owned()))
        .parse_next(input)
}

/// Values are unquoted `[A-Za-z0-9_-]+`, or quoted anything but `"`.
fn kwarg(input: &mut &str) -> ModalResult<Item> {
    let key = identifier.parse_next(input)?;
    '='.parse_next(input)?;
    let value = alt((
        delimited('"', take_while(0.., |c| c != '"'), '"'),
        take_while(1.., |c: char| c.is_alphanumeric() || c == '_' || c == '-'),
    ))
    .parse_next(input)?;
    Ok(Item::Kwarg(key.to_owned(), value.to_owned()))
}

fn identifier<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
    take_while(1.., |c: char| c.is_alphabetic() || c == '_' || c == '-').parse_next(input)
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::parse_header_args;

    fn owned(items: &[&str]) -> Vec<String> {
        items.iter().map(|s| (*s).to_owned()).collect()
    }

    fn pairs(items: &[(&str, &str)]) -> Vec<(String, String)> {
        items
            .iter()
            .map(|(k, v)| ((*k).to_owned(), (*v).to_owned()))
            .collect()
    }

    #[test]
    fn extracts_name() {
        let parsed = parse_header_args("some header");
        assert_eq!(parsed.name, "some header");
        assert!(parsed.args.is_empty());
        assert!(parsed.kwargs.is_empty());
    }

    #[test]
    fn extracts_name_arg_and_kwarg() {
        let parsed = parse_header_args("some header {.some-arg some-kw=\"value\"}");
        assert_eq!(parsed.name, "some header");
        assert_eq!(parsed.args, owned(&["some-arg"]));
        assert_eq!(parsed.kwargs, pairs(&[("some-kw", "value")]));
    }

    #[rstest]
    #[case("{.bg}", "bg")]
    #[case("{.config}", "config")]
    #[case("{.command}", "command")]
    #[case("{.json}", "json")]
    fn extracts_a_flag(#[case] input: &str, #[case] expected: &str) {
        let parsed = parse_header_args(input);
        assert_eq!(parsed.name, "");
        assert_eq!(parsed.args, owned(&[expected]));
        assert!(parsed.kwargs.is_empty());
    }

    #[rstest]
    #[case("{type=git}")]
    #[case("{type=\"git\"}")]
    fn extracts_type(#[case] input: &str) {
        let parsed = parse_header_args(input);
        assert_eq!(parsed.name, "");
        assert!(parsed.args.is_empty());
        assert_eq!(parsed.kwargs, pairs(&[("type", "git")]));
    }

    #[test]
    fn mixes_args_and_kwargs() {
        let parsed = parse_header_args("{.command type=\"git\"}");
        assert_eq!(parsed.name, "");
        assert_eq!(parsed.args, owned(&["command"]));
        assert_eq!(parsed.kwargs, pairs(&[("type", "git")]));
    }

    #[rstest]
    #[case("{type=\"my-type\"}", "my-type")]
    #[case("{type=\"c++\"}", "c++")]
    #[case("{type=my-type}", "my-type")]
    #[case("{type=\"\"}", "")]
    fn kwarg_values_are_not_limited_to_letters(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(
            parse_header_args(input).kwargs,
            pairs(&[("type", expected)])
        );
    }
}
