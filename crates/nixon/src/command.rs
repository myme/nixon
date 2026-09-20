//! The command model and its display forms. SPEC §5.1.

use std::fmt;
use std::path::PathBuf;

use crate::language::Language;
use crate::placeholder::{ParseError, Placeholder, scan_all};

/// Where a command is defined, so `edit` and `new` can find it. SPEC §4.6.
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

/// A runnable command. SPEC §5.1.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Command {
    /// The name selected by, and referenced by placeholders.
    pub name: String,
    /// First paragraph after the heading, if any.
    pub desc: Option<String>,
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
    /// `name ${placeholder}…`, used as the picker header. SPEC §5.1.
    pub fn show(&self) -> String {
        use std::fmt::Write as _;

        let mut out = self.name.clone();
        for placeholder in &self.placeholders {
            let _ = write!(out, " ${{{}}}", placeholder.name);
        }
        out
    }
}

/// `name` or `name - desc`, the candidate text in selection. SPEC §5.1.
impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.desc {
            Some(desc) => write!(f, "{} - {desc}", self.name),
            None => f.write_str(&self.name),
        }
    }
}

/// Splits a command heading into its name and its placeholders. SPEC §4.5.1.
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
