//! The placeholder grammar.
//!
//! ```text
//! placeholder := start '{' name modifiers? '}'
//! start       := '<' | '$' | alias '='
//! name        := 1+ chars not in " :|}"
//! modifiers   := pipe-modifiers | colon-modifiers
//! ```

use std::ops::Range;

use winnow::ascii::{digit1, space0};
use winnow::combinator::{alt, cut_err, delimited, opt, preceded, repeat, separated};
use winnow::error::{ContextError, ParseError as WinnowParseError};
use winnow::token::take_while;
use winnow::{ModalResult, Parser};

use super::{Placeholder, PlaceholderFormat, PlaceholderType};

/// A placeholder that could not be parsed.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ParseError {
    /// Two format modifiers on one placeholder, e.g. `| cols 1 | fields 2`.
    /// v1 only rejected some orderings of this.
    #[error("Placeholder format already set")]
    FormatAlreadySet,
    /// The grammar did not match.
    #[error("{0}")]
    Syntax(String),
    /// A heading declared the same option twice.
    #[error("Duplicate option: {0}")]
    DuplicateOption(String),
}

/// One modifier, before it is folded into a placeholder.
#[derive(Clone)]
enum Modifier {
    Format(PlaceholderFormat),
    Filter(String),
    List,
    Multiple,
}

/// Parses exactly one placeholder, which must span the whole input.
pub fn parse_one(input: &str) -> Result<Placeholder, ParseError> {
    placeholder
        .parse(input)
        .map_err(|err: WinnowParseError<_, ContextError>| ParseError::Syntax(err.to_string()))?
}

/// Collects every placeholder in a string, skipping anything else.
///
/// A `${`, `<{` or `alias={` that is never closed is an error rather than
/// ordinary text, matching v1's committed parse.
pub fn scan_all(input: &str) -> Result<Vec<Placeholder>, ParseError> {
    Ok(scan_spans(input)?
        .into_iter()
        .map(|(_, placeholder)| placeholder)
        .collect())
}

/// Every placeholder in `input`, with the byte range it spans.
///
/// The ranges are what lets a heading interleave placeholders with option
/// tokens in the order they were written.
pub fn scan_spans(input: &str) -> Result<Vec<(Range<usize>, Placeholder)>, ParseError> {
    let mut found = Vec::new();
    let mut rest = input;

    while !rest.is_empty() {
        let at = input.len() - rest.len();
        let mut probe = rest;
        if placeholder_start(&mut probe).is_ok() {
            let mut cursor = rest;
            let parsed = placeholder(&mut cursor).map_err(|err| {
                err.into_inner().map_or_else(
                    |_| ParseError::Syntax("incomplete placeholder".to_owned()),
                    |ctx| ParseError::Syntax(ctx.to_string()),
                )
            })?;
            let end = input.len() - cursor.len();
            found.push((at..end, parsed?));
            rest = cursor;
        } else {
            let mut chars = rest.chars();
            chars.next();
            rest = chars.as_str();
        }
    }

    Ok(found)
}

/// Recognises the opening of a placeholder without consuming its body.
fn placeholder_start(input: &mut &str) -> ModalResult<PlaceholderType> {
    let kind = alt((
        '<'.value(PlaceholderType::Stdin),
        '$'.value(PlaceholderType::Arg),
        take_while(0.., |c: char| c.is_alphanumeric() || c == '_')
            .map(|alias: &str| PlaceholderType::EnvVar(alias.to_owned())),
    ))
    .parse_next(input)?;
    if matches!(kind, PlaceholderType::EnvVar(_)) {
        '='.parse_next(input)?;
    }
    '{'.parse_next(input)?;
    Ok(kind)
}

/// `start '{' name modifiers? '}'`. Committed once the opening matched.
fn placeholder(input: &mut &str) -> ModalResult<Result<Placeholder, ParseError>> {
    let kind = placeholder_start(input)?;
    let name = cut_err(take_while(1.., |c: char| !" :|}".contains(c))).parse_next(input)?;
    let modifiers = cut_err(modifiers).parse_next(input)?;
    cut_err('}').parse_next(input)?;

    Ok(build(kind, name, modifiers))
}

/// Folds modifiers onto a placeholder, rejecting a second format.
fn build(
    kind: PlaceholderType,
    name: &str,
    modifiers: Vec<Modifier>,
) -> Result<Placeholder, ParseError> {
    // An empty alias takes the command's name; either way '-' becomes '_'.
    let kind = match kind {
        PlaceholderType::EnvVar(alias) if alias.is_empty() => {
            PlaceholderType::EnvVar(name.replace('-', "_"))
        }
        PlaceholderType::EnvVar(alias) => PlaceholderType::EnvVar(alias.replace('-', "_")),
        same => same,
    };

    let mut placeholder = Placeholder::new(kind, name);
    for modifier in modifiers {
        match modifier {
            Modifier::Format(format) => {
                if placeholder.format != PlaceholderFormat::Lines {
                    return Err(ParseError::FormatAlreadySet);
                }
                placeholder.format = format;
            }
            Modifier::Filter(filter) => placeholder.filter = Some(filter),
            Modifier::List => placeholder.list = true,
            Modifier::Multiple => placeholder.multiple = true,
        }
    }
    Ok(placeholder)
}

fn modifiers(input: &mut &str) -> ModalResult<Vec<Modifier>> {
    alt((
        pipe_modifiers,
        colon_modifiers,
        winnow::combinator::empty.map(|()| Vec::new()),
    ))
    .parse_next(input)
}

/// `( spaces '|' spaces pipe-mod spaces )+`
fn pipe_modifiers(input: &mut &str) -> ModalResult<Vec<Modifier>> {
    repeat(1.., delimited((space0, '|', space0), pipe_modifier, space0)).parse_next(input)
}

fn pipe_modifier(input: &mut &str) -> ModalResult<Modifier> {
    alt((
        columns_modifier,
        preceded(("fields", space0), fields)
            .map(|f| Modifier::Format(PlaceholderFormat::Fields(f))),
        "json".value(Modifier::Format(PlaceholderFormat::Json)),
        filter_modifier,
        "list".value(Modifier::List),
        "multi".value(Modifier::Multiple),
    ))
    .parse_next(input)
}

/// `cols` keeps every row; `cols+h` treats the first row as a header.
fn columns_modifier(input: &mut &str) -> ModalResult<Modifier> {
    let has_header = preceded("cols", opt("+h")).parse_next(input)?.is_some();
    let cols = preceded(space0, fields).parse_next(input)?;
    Ok(Modifier::Format(PlaceholderFormat::Columns {
        has_header,
        cols,
    }))
}

/// `filter "…"`. v1 accepted alphanumerics only; this takes anything but `"`.
fn filter_modifier(input: &mut &str) -> ModalResult<Modifier> {
    preceded(
        ("filter", space0),
        delimited('"', take_while(0.., |c: char| c != '"'), '"'),
    )
    .map(|f: &str| Modifier::Filter(f.to_owned()))
    .parse_next(input)
}

/// `':' ( fields 'm'? | 'm' fields? )` — fields and multi in either order.
fn colon_modifiers(input: &mut &str) -> ModalResult<Vec<Modifier>> {
    preceded(
        ':',
        alt((
            (fields, opt('m')).map(|(f, m)| {
                let mut out = vec![Modifier::Format(PlaceholderFormat::Fields(f))];
                if m.is_some() {
                    out.push(Modifier::Multiple);
                }
                out
            }),
            ('m', opt(fields)).map(|(_, f)| {
                let mut out = vec![Modifier::Multiple];
                if let Some(f) = f {
                    out.push(Modifier::Format(PlaceholderFormat::Fields(f)));
                }
                out
            }),
        )),
    )
    .parse_next(input)
}

/// `digits (',' digits)*`, 1-based.
fn fields(input: &mut &str) -> ModalResult<Vec<usize>> {
    separated(1.., digit1.parse_to::<usize>(), ',').parse_next(input)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rstest::rstest;

    use super::{ParseError, parse_one};
    use crate::placeholder::{Placeholder, PlaceholderFormat, PlaceholderType};

    #[rstest]
    #[case("${placeholder | cols 1}", PlaceholderFormat::Columns { has_header: false, cols: vec![1] })]
    #[case("${placeholder | cols+h 1}", PlaceholderFormat::Columns { has_header: true, cols: vec![1] })]
    #[case("${placeholder | cols 1,3}", PlaceholderFormat::Columns { has_header: false, cols: vec![1, 3] })]
    #[case("${placeholder | json}", PlaceholderFormat::Json)]
    #[case("${placeholder | fields 2}", PlaceholderFormat::Fields(vec![2]))]
    #[case("${placeholder}", PlaceholderFormat::Lines)]
    fn pipe_modifiers_set_the_format(#[case] input: &str, #[case] expected: PlaceholderFormat) {
        assert_eq!(parse_one(input).map(|p| p.format), Ok(expected));
    }

    #[rstest]
    #[case("${arg | cols 1 | fields 2}")]
    #[case("${arg | json | fields 1}")]
    #[case("${arg | fields 1 | json}")]
    #[case("${arg | fields 1 | cols 2}")]
    #[case("${arg | cols 1 | cols+h 2}")]
    fn a_second_format_modifier_is_rejected(#[case] input: &str) {
        assert_eq!(parse_one(input), Err(ParseError::FormatAlreadySet));
    }

    #[rstest]
    #[case("${arg | filter \"a-b c!\"}", "a-b c!")]
    #[case("${arg | filter \"\"}", "")]
    #[case("${arg | filter \"c++\"}", "c++")]
    fn a_filter_accepts_anything_but_a_quote(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(
            parse_one(input).map(|p| p.filter),
            Ok(Some(expected.to_owned()))
        );
    }

    /// The pipe needs no surrounding spaces.
    #[rstest]
    #[case("${a|multi}")]
    #[case("${a | multi}")]
    #[case("${a   |   multi}")]
    fn a_pipe_modifier_needs_no_spaces(#[case] input: &str) {
        assert_eq!(parse_one(input).map(|p| p.multiple), Ok(true));
    }

    #[test]
    fn modifiers_combine() {
        let parsed = parse_one("<{files | cols+h 1,2 | filter \"src\" | list | multi}").unwrap();
        assert_eq!(parsed.kind, PlaceholderType::Stdin);
        assert_eq!(parsed.name, "files");
        assert_eq!(
            parsed.format,
            PlaceholderFormat::Columns {
                has_header: true,
                cols: vec![1, 2]
            }
        );
        assert_eq!(parsed.filter.as_deref(), Some("src"));
        assert!(parsed.list);
        assert!(parsed.multiple);
    }

    #[rstest]
    #[case("${arg")]
    #[case("${}")]
    #[case("$arg}")]
    fn a_malformed_placeholder_is_an_error(#[case] input: &str) {
        assert!(parse_one(input).is_err());
    }

    fn name_strategy() -> impl Strategy<Value = String> {
        "[a-zA-Z][a-zA-Z0-9_-]{0,12}"
    }

    fn format_strategy() -> impl Strategy<Value = PlaceholderFormat> {
        prop_oneof![
            Just(PlaceholderFormat::Lines),
            Just(PlaceholderFormat::Json),
            prop::collection::vec(1usize..20, 1..4).prop_map(PlaceholderFormat::Fields),
            (any::<bool>(), prop::collection::vec(1usize..20, 1..4))
                .prop_map(|(has_header, cols)| PlaceholderFormat::Columns { has_header, cols }),
        ]
    }

    fn kind_strategy() -> impl Strategy<Value = PlaceholderType> {
        prop_oneof![
            Just(PlaceholderType::Arg),
            Just(PlaceholderType::Stdin),
            "[a-zA-Z][a-zA-Z0-9_]{0,8}".prop_map(PlaceholderType::EnvVar),
        ]
    }

    proptest! {
        #[test]
        fn display_round_trips_through_the_parser(
            kind in kind_strategy(),
            name in name_strategy(),
            format in format_strategy(),
            filter in prop::option::of("[a-zA-Z0-9 _+-]{0,10}"),
            list in any::<bool>(),
            multiple in any::<bool>(),
        ) {
            let placeholder = Placeholder {
                kind,
                name,
                format,
                filter,
                list,
                multiple,
                value: Vec::new(),
            };
            prop_assert_eq!(parse_one(&placeholder.to_string()), Ok(placeholder));
        }
    }
}
