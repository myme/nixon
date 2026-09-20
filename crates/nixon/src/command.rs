//! The command model and its display forms.

use std::fmt;
use std::path::PathBuf;

use crate::language::Language;
use crate::placeholder::{ParseError, Placeholder, scan_spans};

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

/// A flag declared in a command heading and toggled at the prompt.
///
/// Only on/off for now. The `=value` form parses so the syntax has room for
/// valued options later; the value is not read.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CommandOption {
    /// The token with its dashes stripped and any `=…` dropped.
    pub name: String,
    /// The token as written, which is what goes into argv when it is on.
    pub token: String,
    /// Whether it starts on. An option with no declaration starts off.
    pub default: bool,
    /// From the declaration list item, if there was one.
    pub description: Option<Description>,
}

impl CommandOption {
    /// The environment variable the option is exported as.
    pub fn env_var(&self) -> String {
        format!("nixon_opt_{}", self.name.replace('-', "_"))
    }
}

/// One argument a heading declares, in the order it was written.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgSpec {
    /// A value chosen at the prompt.
    Placeholder(Placeholder),
    /// A flag toggled at the prompt; the index is into [`Command::options`].
    Option(usize),
}

/// What the command line said about a command's options.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Overrides {
    /// Per option, `Some` when the command line named it.
    pub set: Vec<Option<bool>>,
    /// The arguments that were not option tokens.
    pub queries: Vec<String>,
}

impl Overrides {
    /// The option state these overrides imply, over `defaults`.
    pub fn apply(&self, defaults: &[bool]) -> Vec<bool> {
        defaults
            .iter()
            .enumerate()
            .map(|(index, default)| self.set.get(index).copied().flatten().unwrap_or(*default))
            .collect()
    }

    /// Whether the command line settled every option.
    pub fn is_complete(&self) -> bool {
        !self.set.is_empty() && self.set.iter().all(Option::is_some)
    }
}

/// Wraps placeholders as arguments, for callers that have no options.
pub fn arg_specs(placeholders: Vec<Placeholder>) -> Vec<ArgSpec> {
    placeholders.into_iter().map(ArgSpec::Placeholder).collect()
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
    /// Arguments from the heading or the info string, in order.
    pub args: Vec<ArgSpec>,
    /// Options declared in the heading, in order.
    pub options: Vec<CommandOption>,
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
        for placeholder in self.placeholders() {
            let _ = write!(out, " ${{{}}}", placeholder.name);
        }
        out
    }

    /// The placeholders among the arguments, in order.
    pub fn placeholders(&self) -> impl Iterator<Item = &Placeholder> {
        self.args.iter().filter_map(|arg| match arg {
            ArgSpec::Placeholder(placeholder) => Some(placeholder),
            ArgSpec::Option(_) => None,
        })
    }

    /// Every option at its declared default.
    pub fn default_options(&self) -> Vec<bool> {
        self.options.iter().map(|option| option.default).collect()
    }

    /// Splits command-line arguments into option overrides and queries.
    ///
    /// A word equal to an option's token turns it on and `--no-<name>` turns
    /// it off; everything else stays a placeholder query, so an unknown
    /// `--x` searches rather than failing.
    pub fn split_args(&self, args: &[String]) -> Overrides {
        let mut overrides = Overrides {
            set: vec![None; self.options.len()],
            queries: Vec::new(),
        };

        for arg in args {
            if let Some(index) = self.options.iter().position(|o| &o.token == arg) {
                overrides.set[index] = Some(true);
            } else if let Some(name) = arg.strip_prefix("--no-")
                && let Some(index) = self.options.iter().position(|o| o.name == name)
            {
                overrides.set[index] = Some(false);
            } else {
                overrides.queries.push(arg.clone());
            }
        }

        overrides
    }

    /// The option with this name, if the command declares one.
    pub fn option(&self, name: &str) -> Option<&CommandOption> {
        self.options.iter().find(|option| option.name == name)
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

/// Splits a command heading into its name and its arguments.
///
/// The name is the first whitespace-delimited word; everything after it is
/// scanned for placeholders and option tokens, and otherwise ignored, so a
/// trailing `&` and ordinary shell text are skipped.
pub fn parse_command_name(
    input: &str,
) -> Result<(String, Vec<ArgSpec>, Vec<CommandOption>), ParseError> {
    let trimmed = input.trim_start();
    let Some(end) = trimmed.find(char::is_whitespace) else {
        return Ok((trimmed.to_owned(), Vec::new(), Vec::new()));
    };
    let (name, rest) = trimmed.split_at(end);
    let (args, options) = parse_args(rest)?;
    Ok((name.to_owned(), args, options))
}

/// Scans a heading tail, or an info string, into arguments and options.
///
/// A token is an option when it is `-x` or `--name`, optionally `=value`;
/// anything else is either a placeholder or text to ignore.
pub fn parse_args(input: &str) -> Result<(Vec<ArgSpec>, Vec<CommandOption>), ParseError> {
    let spans = scan_spans(input)?;
    let mut args = Vec::new();
    let mut options: Vec<CommandOption> = Vec::new();

    let mut at = 0;
    for (range, placeholder) in spans {
        scan_options(&input[at..range.start], &mut args, &mut options)?;
        args.push(ArgSpec::Placeholder(placeholder));
        at = range.end;
    }
    scan_options(&input[at..], &mut args, &mut options)?;

    Ok((args, options))
}

/// Collects the option tokens in a stretch of heading text.
fn scan_options(
    text: &str,
    args: &mut Vec<ArgSpec>,
    options: &mut Vec<CommandOption>,
) -> Result<(), ParseError> {
    for token in text.split_whitespace() {
        let Some(name) = option_name(token) else {
            continue;
        };
        if options.iter().any(|option| option.name == name) {
            return Err(ParseError::DuplicateOption(name));
        }
        args.push(ArgSpec::Option(options.len()));
        options.push(CommandOption {
            name,
            token: token.to_owned(),
            ..CommandOption::default()
        });
    }
    Ok(())
}

/// The name an option token declares: `-f`, or `--name` with an optional
/// `=value` that is dropped.
pub(crate) fn option_name(token: &str) -> Option<String> {
    if let Some(long) = token.strip_prefix("--") {
        let name = long.split_once('=').map_or(long, |(name, _)| name);
        let mut chars = name.chars();
        let first = chars.next()?;
        if !first.is_ascii_alphanumeric() {
            return None;
        }
        if !chars.all(|c| c.is_ascii_alphanumeric() || c == '-') {
            return None;
        }
        return Some(name.to_owned());
    }

    let short = token.strip_prefix('-')?;
    let mut chars = short.chars();
    let first = chars.next()?;
    if !first.is_ascii_alphanumeric() || chars.next().is_some() {
        return None;
    }
    Some(short.to_owned())
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::{ArgSpec, Command, parse_command_name};
    use crate::placeholder::{ParseError, Placeholder, PlaceholderFormat, PlaceholderType};

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

    /// Name and placeholders, for the cases that declare no options.
    fn parsed(input: &str) -> Result<(String, Vec<Placeholder>), ParseError> {
        let (name, args, options) = parse_command_name(input)?;
        assert!(options.is_empty(), "expected no options, got {options:?}");
        Ok((
            name,
            args.into_iter()
                .map(|arg| match arg {
                    ArgSpec::Placeholder(placeholder) => placeholder,
                    ArgSpec::Option(_) => unreachable!(),
                })
                .collect(),
        ))
    }

    #[test]
    fn parses_empty_name() {
        assert_eq!(parsed(""), Ok((String::new(), vec![])));
    }

    #[rstest]
    #[case("echo 'foo bar baz'", "echo")]
    #[case("   echo 'foo bar baz'", "echo")]
    fn parses_text_part(#[case] input: &str, #[case] name: &str) {
        assert_eq!(parsed(input), Ok((name.to_owned(), vec![])));
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
        assert_eq!(parsed(input), Ok(("cat".to_owned(), vec![expected])));
    }

    #[test]
    fn parses_list_modifier() {
        let mut expected = arg("arg");
        expected.list = true;
        assert_eq!(
            parsed("cat ${arg | list}"),
            Ok(("cat".to_owned(), vec![expected]))
        );
    }

    #[test]
    fn parses_filter_modifier() {
        let mut expected = arg("arg");
        expected.filter = Some("filter".to_owned());
        assert_eq!(
            parsed("cat ${arg | filter \"filter\"}"),
            Ok(("cat".to_owned(), vec![expected]))
        );
    }

    #[rstest]
    #[case("echo $SOME_VAR", "echo")]
    #[case("echo <SOME_VAR", "echo")]
    fn a_lone_dollar_or_angle_is_not_a_placeholder(#[case] input: &str, #[case] name: &str) {
        assert_eq!(parsed(input), Ok((name.to_owned(), vec![])));
    }

    #[test]
    fn fails_on_unterminated_arg() {
        assert!(parsed("cat \"${arg\"").is_err());
    }

    /// A `-x` or `--name` token in the heading declares a toggleable option.
    #[rstest]
    #[case("remove --force", &["force"], &["--force"])]
    #[case("remove -f", &["f"], &["-f"])]
    #[case("clone --depth=1", &["depth"], &["--depth=1"])]
    #[case("build --no-cache --release", &["no-cache", "release"], &["--no-cache", "--release"])]
    fn option_tokens_declare_options(
        #[case] input: &str,
        #[case] names: &[&str],
        #[case] tokens: &[&str],
    ) {
        let (_, _, options) = parse_command_name(input).unwrap();
        assert_eq!(
            options.iter().map(|o| o.name.as_str()).collect::<Vec<_>>(),
            names
        );
        assert_eq!(
            options.iter().map(|o| o.token.as_str()).collect::<Vec<_>>(),
            tokens
        );
        assert!(options.iter().all(|o| !o.default));
    }

    #[rstest]
    #[case("cmd -")]
    #[case("cmd --")]
    #[case("cmd -ab")]
    #[case("cmd --=1")]
    #[case("cmd --a.b")]
    #[case("cmd &")]
    #[case("cmd -$")]
    fn other_tokens_are_not_options(#[case] input: &str) {
        let (_, _, options) = parse_command_name(input).unwrap();
        assert!(options.is_empty(), "{input} declared {options:?}");
    }

    #[test]
    fn options_keep_their_place_among_the_placeholders() {
        let (name, args, options) = parse_command_name("remove --force ${worktree} -v").unwrap();

        assert_eq!(name, "remove");
        assert_eq!(options.len(), 2);
        assert!(matches!(args[0], ArgSpec::Option(0)));
        assert!(matches!(&args[1], ArgSpec::Placeholder(p) if p.name == "worktree"));
        assert!(matches!(args[2], ArgSpec::Option(1)));
    }

    #[test]
    fn a_repeated_option_is_an_error() {
        assert_eq!(
            parse_command_name("cmd --force --force"),
            Err(ParseError::DuplicateOption("force".to_owned()))
        );
        // The `=value` form names the same option.
        assert!(parse_command_name("cmd --depth --depth=1").is_err());
    }

    #[test]
    fn an_options_env_var_replaces_dashes() {
        let (_, _, options) = parse_command_name("cmd --no-cache").unwrap();
        assert_eq!(options[0].env_var(), "nixon_opt_no_cache");
    }

    fn options_of(heading: &str) -> Command {
        let (name, args, options) = parse_command_name(heading).unwrap();
        Command {
            name,
            args,
            options,
            ..Command::default()
        }
    }

    fn words(items: &[&str]) -> Vec<String> {
        items.iter().map(|s| (*s).to_owned()).collect()
    }

    #[test]
    fn a_matching_token_sets_an_option_and_the_rest_stay_queries() {
        let command = options_of("remove --force ${worktree}");
        let split = command.split_args(&words(&["--force", "main"]));

        assert_eq!(split.set, [Some(true)]);
        assert_eq!(split.queries, ["main"]);
        assert_eq!(split.apply(&command.default_options()), [true]);
    }

    #[test]
    fn a_no_prefixed_token_turns_an_option_off() {
        let mut command = options_of("remove --force");
        command.options[0].default = true;

        let split = command.split_args(&words(&["--no-force"]));
        assert_eq!(split.set, [Some(false)]);
        assert_eq!(split.apply(&command.default_options()), [false]);
    }

    #[test]
    fn an_unknown_dashed_word_stays_a_query() {
        let command = options_of("remove --force");
        let split = command.split_args(&words(&["--quiet", "-x"]));

        assert_eq!(split.set, [None]);
        assert_eq!(split.queries, ["--quiet", "-x"]);
    }

    #[test]
    fn a_command_line_that_names_every_option_is_complete() {
        let command = options_of("build --release --no-cache");
        assert!(!command.split_args(&words(&["--release"])).is_complete());
        assert!(
            command
                .split_args(&words(&["--release", "--no-no-cache"]))
                .is_complete()
        );
        // A command with no options is never "complete"; there is nothing
        // for the prompt to ask about either way.
        assert!(!options_of("plain").split_args(&[]).is_complete());
    }

    #[test]
    fn a_trailing_background_marker_is_not_a_placeholder() {
        assert_eq!(
            parsed("hello ${arg} ${another-arg} &"),
            Ok(("hello".to_owned(), vec![arg("arg"), arg("another-arg")]))
        );
    }
}
