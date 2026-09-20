//! The command model and its display forms.

use std::fmt;
use std::path::PathBuf;

use crate::language::Language;
use crate::placeholder::{ParseError, Placeholder, scan_all};

/// Where a command is defined, so `edit` and `new` can find it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CommandLocation {
    /// The config file the command was parsed from.
    pub file_path: PathBuf,
    /// 1-based line of the command's heading.
    pub start_line: usize,
    /// Last line the command covers, including trailing blanks.
    pub end_line: usize,
    /// Heading level of the command's heading.
    pub level: usize,
}

/// A piece of a description, as it was written.
///
/// Kept apart so the picker can style inline code; everything on stdout uses
/// [`Description::plain`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DescSpan {
    /// Ordinary prose.
    Text(String),
    /// An inline code span, without its backticks.
    Code(String),
}

impl DescSpan {
    /// The characters this span contributes.
    pub fn text(&self) -> &str {
        match self {
            Self::Text(text) | Self::Code(text) => text,
        }
    }
}

/// A command's description.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Description {
    /// The pieces, in order, with adjacent prose already merged.
    pub spans: Vec<DescSpan>,
}

impl Description {
    /// Builds a description, merging adjacent prose and trimming the ends.
    pub fn new(spans: impl IntoIterator<Item = DescSpan>) -> Self {
        let mut merged: Vec<DescSpan> = Vec::new();
        for span in spans {
            match (merged.last_mut(), &span) {
                (Some(DescSpan::Text(last)), DescSpan::Text(text)) => last.push_str(text),
                _ => merged.push(span),
            }
        }

        if let Some(DescSpan::Text(first)) = merged.first_mut() {
            *first = first.trim_start().to_owned();
        }
        if let Some(DescSpan::Text(last)) = merged.last_mut() {
            *last = last.trim_end().to_owned();
        }
        merged.retain(|span| !matches!(span, DescSpan::Text(text) if text.is_empty()));
        Self { spans: merged }
    }

    /// A description of plain prose.
    pub fn text(text: impl Into<String>) -> Self {
        Self::new([DescSpan::Text(text.into())])
    }

    /// The description as characters, which is what stdout carries.
    pub fn plain(&self) -> String {
        self.spans.iter().map(DescSpan::text).collect()
    }

    /// Whether there is nothing to show.
    pub const fn is_empty(&self) -> bool {
        self.spans.is_empty()
    }
}

impl fmt::Display for Description {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.plain())
    }
}

/// A runnable command.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Command {
    /// The name selected by, and referenced by placeholders.
    pub name: String,
    /// First paragraph after the heading, if any.
    pub desc: Option<Description>,
    /// Language of the source block.
    pub lang: Language,
    /// Project types this applies to; empty means every project.
    pub project_types: Vec<String>,
    /// The code block's contents, including its trailing newline.
    pub source: String,
    /// Working directory; only `eval` sets this.
    pub pwd: Option<PathBuf>,
    /// Placeholders from the heading or the info string.
    pub placeholders: Vec<Placeholder>,
    /// Marked `&`: runs detached.
    pub is_bg: bool,
    /// Name starts with `_`: hidden from the run picker.
    pub is_hidden: bool,
    /// Where it was defined.
    pub location: Option<CommandLocation>,
}

impl Command {
    /// `name ${placeholder}…`, used as the picker header.
    pub fn show(&self) -> String {
        use std::fmt::Write as _;

        let mut out = self.name.clone();
        for placeholder in &self.placeholders {
            let _ = write!(out, " ${{{}}}", placeholder.name);
        }
        out
    }
}

/// `name` or `name - desc`, the candidate text in selection.
impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.desc {
            Some(desc) => write!(f, "{} - {desc}", self.name),
            None => f.write_str(&self.name),
        }
    }
}

/// Splits a command heading into its name and its placeholders.
///
/// The name is the first whitespace-delimited word; everything after it is
/// scanned for placeholders and otherwise ignored, so a trailing `&` and
/// ordinary shell text are skipped.
pub fn parse_command_name(input: &str) -> Result<(String, Vec<Placeholder>), ParseError> {
    let trimmed = input.trim_start();
    let Some(end) = trimmed.find(char::is_whitespace) else {
        return Ok((trimmed.to_owned(), scan_all("")?));
    };
    let (name, rest) = trimmed.split_at(end);
    Ok((name.to_owned(), scan_all(rest)?))
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::parse_command_name;
    use crate::placeholder::{Placeholder, PlaceholderFormat, PlaceholderType};

    fn arg(name: &str) -> Placeholder {
        Placeholder::new(PlaceholderType::Arg, name)
    }

    fn stdin(name: &str) -> Placeholder {
        Placeholder::new(PlaceholderType::Stdin, name)
    }

    fn env(alias: &str, name: &str) -> Placeholder {
        Placeholder::new(PlaceholderType::EnvVar(alias.to_owned()), name)
    }

    fn fields(mut p: Placeholder, ns: &[usize]) -> Placeholder {
        p.format = PlaceholderFormat::Fields(ns.to_vec());
        p
    }

    fn multi(mut p: Placeholder) -> Placeholder {
        p.multiple = true;
        p
    }

    #[test]
    fn parses_empty_name() {
        assert_eq!(parse_command_name(""), Ok((String::new(), vec![])));
    }

    #[rstest]
    #[case("echo 'foo bar baz'", "echo")]
    #[case("   echo 'foo bar baz'", "echo")]
    fn parses_text_part(#[case] input: &str, #[case] name: &str) {
        assert_eq!(parse_command_name(input), Ok((name.to_owned(), vec![])));
    }

    #[rstest]
    #[case("cat ${arg}", arg("arg"))]
    #[case("cat <{arg}", stdin("arg"))]
    #[case("cat ={arg}", env("arg", "arg"))]
    #[case("cat FOO={bar}", env("FOO", "bar"))]
    #[case("cat <{arg:1}", fields(stdin("arg"), &[1]))]
    #[case("cat <{arg:1,3,5}", fields(stdin("arg"), &[1, 3, 5]))]
    #[case("cat ${arg:m}", multi(arg("arg")))]
    #[case("cat ${arg | fields 1,3}", fields(arg("arg"), &[1, 3]))]
    #[case("cat ${arg | multi}", multi(arg("arg")))]
    #[case("cat <{arg:m}", multi(stdin("arg")))]
    #[case("cat <{arg:m1,3,5}", multi(fields(stdin("arg"), &[1, 3, 5])))]
    #[case("cat <{arg:1,3,5m}", multi(fields(stdin("arg"), &[1, 3, 5])))]
    #[case("cat <{arg | fields 1,3,5}", fields(stdin("arg"), &[1, 3, 5]))]
    #[case("cat <{arg | fields 1,3,5 | multi}", multi(fields(stdin("arg"), &[1, 3, 5])))]
    #[case("cat \"${arg}\"", arg("arg"))]
    #[case("cat \"={some-arg}\"", env("some_arg", "some-arg"))]
    #[case("cat some_arg={some-arg}", env("some_arg", "some-arg"))]
    fn parses_placeholder_part(#[case] input: &str, #[case] expected: Placeholder) {
        assert_eq!(
            parse_command_name(input),
            Ok(("cat".to_owned(), vec![expected]))
        );
    }

    #[test]
    fn parses_list_modifier() {
        let mut expected = arg("arg");
        expected.list = true;
        assert_eq!(
            parse_command_name("cat ${arg | list}"),
            Ok(("cat".to_owned(), vec![expected]))
        );
    }

    #[test]
    fn parses_filter_modifier() {
        let mut expected = arg("arg");
        expected.filter = Some("filter".to_owned());
        assert_eq!(
            parse_command_name("cat ${arg | filter \"filter\"}"),
            Ok(("cat".to_owned(), vec![expected]))
        );
    }

    #[rstest]
    #[case("echo $SOME_VAR", "echo")]
    #[case("echo <SOME_VAR", "echo")]
    fn a_lone_dollar_or_angle_is_not_a_placeholder(#[case] input: &str, #[case] name: &str) {
        assert_eq!(parse_command_name(input), Ok((name.to_owned(), vec![])));
    }

    #[test]
    fn fails_on_unterminated_arg() {
        assert!(parse_command_name("cat \"${arg\"").is_err());
    }

    #[test]
    fn a_trailing_background_marker_is_not_a_placeholder() {
        assert_eq!(
            parse_command_name("hello ${arg} ${another-arg} &"),
            Ok(("hello".to_owned(), vec![arg("arg"), arg("another-arg")]))
        );
    }
}
