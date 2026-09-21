//! Parsing `nixon.md`: config blocks and commands.

pub mod command;
pub mod extract;
pub mod header;
pub mod walk;

use comrak::{Arena, Options, parse_document};

pub use header::{HeaderArgs, parse_header_args};
pub use walk::ParsedFile;

use crate::config::Config;

/// A markdown file that could not be parsed.
///
/// `message` is v1's wording so the behaviour stays checkable; `file` and
/// `line` are what miette renders a snippet from.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
#[error("{message}")]
pub struct MarkdownError {
    /// The file being parsed.
    pub file: String,
    /// 1-based line, when the failure is attributable to one.
    pub line: Option<usize>,
    /// What went wrong, in v1's words.
    pub message: String,
}

impl MarkdownError {
    /// Builds an error carrying v1's wording.
    pub fn new(file: &str, line: Option<usize>, message: String) -> Self {
        Self {
            file: file.to_owned(),
            line,
            message,
        }
    }
}

/// Parses a markdown config file into its config and commands.
pub fn parse(file: &str, text: &str) -> Result<ParsedFile, MarkdownError> {
    let arena = Arena::new();
    let root = parse_document(&arena, text, &Options::default());
    walk::walk(file, &extract::extract(root))
}

/// Parses a markdown config file into one effective [`Config`].
pub fn parse_config_file(file: &str, text: &str) -> Result<Config, MarkdownError> {
    let parsed = parse(file, text)?;
    let mut config = parsed.config.unwrap_or_default();
    config.commands = parsed.commands;
    Ok(config)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use rstest::rstest;

    use super::{parse, parse_config_file};
    use crate::command::{ArgSpec, Command, CommandLocation, DescSpan, Description};
    use crate::language::Language;
    use crate::placeholder::{Placeholder, PlaceholderFormat, PlaceholderType};

    const FILE: &str = "some-file.md";

    fn md(lines: &[&str]) -> String {
        let mut out = lines.join("\n");
        out.push('\n');
        out
    }

    fn commands(lines: &[&str]) -> Vec<Command> {
        parse(FILE, &md(lines)).unwrap().commands
    }

    fn names(lines: &[&str]) -> Vec<String> {
        commands(lines).into_iter().map(|c| c.name).collect()
    }

    fn error(lines: &[&str]) -> String {
        parse(FILE, &md(lines)).unwrap_err().message
    }

    fn arg(name: &str) -> Placeholder {
        Placeholder::new(PlaceholderType::Arg, name)
    }

    #[test]
    fn allows_empty_json_object() {
        let config = parse_config_file(FILE, &md(&["# Config {.config}", "```", "{}", "```"]));
        assert_eq!(config, Ok(crate::config::Config::default()));
    }

    #[rstest]
    #[case(&["# Config {.config}", "```", "{", "  \"bin_dirs\": [\"bin\"],", "  \"exact_match\": true,", "  \"ignore_case\": true,", "  \"project_dirs\": [\"foo\", \"bar\"],", "  \"use_direnv\": true,", "  \"use_nix\": true", "}", "```"])]
    #[case(&["``` json  config", "{", "  \"bin_dirs\": [\"bin\"],", "  \"exact_match\": true,", "  \"ignore_case\": true,", "  \"project_dirs\": [\"foo\", \"bar\"],", "  \"use_direnv\": true,", "  \"use_nix\": true", "}", "```"])]
    #[case(&["``` yaml config", "bin_dirs:", "  - bin", "exact_match: true", "ignore_case: true", "project_dirs:", "  - foo", "  - bar", "use_direnv: true", "use_nix: true", "```"])]
    fn parses_a_config_block(#[case] lines: &[&str]) {
        let config = parse_config_file(FILE, &md(lines)).unwrap();
        assert_eq!(config.bin_dirs, vec![PathBuf::from("bin")]);
        assert_eq!(config.exact_match, Some(true));
        assert_eq!(config.ignore_case, Some(true));
        assert_eq!(
            config.project_dirs,
            ["foo", "bar"].map(PathBuf::from).to_vec()
        );
        assert_eq!(config.use_direnv, Some(true));
        assert_eq!(config.use_nix, Some(true));
        assert!(config.commands.is_empty());
    }

    #[test]
    fn errors_without_a_config_block() {
        assert_eq!(
            error(&["# Config {.config}"]),
            "Expecting config source after header"
        );
    }

    #[test]
    fn errors_on_an_unexpected_bash_config_block() {
        assert_eq!(
            error(&["# Config {.config}", "```bash", "```"]),
            "Invalid config language: bash"
        );
    }

    #[rstest]
    #[case(&["# Config {.config}", "```json", "```"])]
    #[case(&["# Config {.config}", "```json", "{,}", "```"])]
    fn errors_on_malformed_json(#[case] lines: &[&str]) {
        assert!(parse(FILE, &md(lines)).is_err());
    }

    #[rstest]
    #[case(&["# Config {.config}", "```json", "{}", "```", "# Config {.config}", "```json", "{}", "```"])]
    #[case(&["```json config", "{}", "```", "```json config", "{}", "```"])]
    fn errors_on_multiple_config_blocks(#[case] lines: &[&str]) {
        assert!(error(lines).contains("Found multiple configuration blocks"));
    }

    #[test]
    fn errors_without_a_source_block() {
        assert_eq!(
            error(&["# command {.command}"]),
            "Expecting source block for command"
        );
    }

    #[rstest]
    #[case(&["# hello {.command}", "```bash", "echo Hello World", "```"], "hello", false)]
    #[case(&["# `hello`", "```bash", "echo Hello World", "```"], "hello", false)]
    #[case(&["# `_hidden`", "```bash", "echo Hello World", "```"], "_hidden", true)]
    fn detects_a_command_and_whether_it_is_hidden(
        #[case] lines: &[&str],
        #[case] name: &str,
        #[case] hidden: bool,
    ) {
        let parsed = commands(lines);
        assert_eq!(parsed[0].name, name);
        assert_eq!(parsed[0].is_hidden, hidden);
    }

    #[test]
    fn can_bump_header_level_gaps() {
        assert_eq!(
            names(&[
                "## `hello`",
                "```bash",
                "echo Hello World",
                "```",
                "#### `world`",
                "```bash",
                "echo World",
                "```"
            ]),
            ["hello", "world"]
        );
    }

    #[test]
    fn command_name_is_the_first_word() {
        assert_eq!(
            names(&[
                "# `hello ${foo} ${bar}`",
                "```bash",
                "echo Hello World",
                "```"
            ]),
            ["hello"]
        );
    }

    #[test]
    fn extracts_source_block() {
        let parsed = commands(&[
            "# hello {.command .bg}",
            "",
            "Command description.",
            "",
            "```bash",
            "echo Hello World",
            "```",
        ]);
        assert_eq!(parsed[0].name, "hello");
        assert_eq!(parsed[0].lang, Language::Bash);
        assert_eq!(
            parsed[0].desc.as_ref().map(Description::plain).as_deref(),
            Some("Command description.")
        );
        assert_eq!(parsed[0].source, "echo Hello World\n");
        assert_eq!(parsed[0].placeholders().count(), 0);
        assert!(parsed[0].is_bg);
    }

    /// v1 joined inline nodes with a space, so a description with code in it
    /// read `Run   cargo build  .` in `--list`.
    #[test]
    fn inline_code_does_not_add_spaces_around_itself() {
        let parsed = commands(&[
            "# `build`",
            "",
            "Run `cargo build` for the workspace.",
            "",
            "```bash",
            "cargo build",
            "```",
        ]);

        let desc = parsed[0].desc.as_ref().unwrap();
        assert_eq!(desc.plain(), "Run cargo build for the workspace.");
        assert_eq!(
            desc.spans,
            [
                DescSpan::Text("Run ".to_owned()),
                DescSpan::Code("cargo build".to_owned()),
                DescSpan::Text(" for the workspace.".to_owned()),
            ]
        );
    }

    #[test]
    fn a_heading_with_inline_code_and_attributes_reads_cleanly() {
        let parsed = parse(
            FILE,
            &md(&["# `hello` {type=\"git\"}", "", "```bash", "echo hi", "```"]),
        )
        .unwrap();
        assert_eq!(parsed.commands[0].name, "hello");
        assert_eq!(parsed.commands[0].project_types, ["git"]);
    }

    #[test]
    fn a_list_item_declares_an_options_default_and_description() {
        let parsed = commands(&[
            "# `remove --force ${worktree}`",
            "",
            "Removes a worktree.",
            "",
            "- `--force`: on — also removes worktrees with local changes",
            "",
            "```bash",
            "git worktree remove \"$@\"",
            "```",
        ]);

        let options = &parsed[0].options;
        assert_eq!(options.len(), 1);
        assert_eq!(options[0].name, "force");
        assert!(options[0].default);
        assert_eq!(
            options[0].description.as_ref().map(Description::plain),
            Some("also removes worktrees with local changes".to_owned())
        );
        assert_eq!(
            parsed[0].desc.as_ref().map(Description::plain).as_deref(),
            Some("Removes a worktree.")
        );
    }

    /// A heading with a `{` that is not an attribute block becomes all name
    /// (v1 parity). The options in it are still options, and an error in
    /// them still fails the file rather than being swallowed.
    #[test]
    fn an_option_error_wins_over_the_attribute_fallback() {
        let err = parse(
            FILE,
            &md(&[
                "# `remove --force --force` {not an attribute block",
                "",
                "```bash",
                "true",
                "```",
            ]),
        )
        .unwrap_err();
        assert_eq!(err.message, "Duplicate option: force");
        assert_eq!(err.line, Some(1));
    }

    /// Everything after an attribute block is ignored, so an option there
    /// would be dropped without a word. It is an error instead.
    #[test]
    fn an_option_after_the_attribute_block_is_an_error() {
        let err = parse(
            FILE,
            &md(&[
                "# `deploy` {type=\"git\"} --force",
                "",
                "```bash",
                "true",
                "```",
            ]),
        )
        .unwrap_err();
        assert_eq!(
            err.message,
            "--force: options must come before the attribute block"
        );
        assert_eq!(err.line, Some(1));
    }

    #[test]
    fn other_trailing_text_after_the_attribute_block_is_still_ignored() {
        let parsed = commands(&[
            "# `deploy` {type=\"git\"} and some words",
            "",
            "```bash",
            "true",
            "```",
        ]);
        assert_eq!(parsed[0].name, "deploy");
        assert_eq!(parsed[0].project_types, ["git"]);
        assert!(parsed[0].options.is_empty());
    }

    #[test]
    fn an_option_before_the_attribute_block_is_read() {
        let parsed = commands(&[
            "# `deploy --force` {type=\"git\"}",
            "",
            "```bash",
            "true",
            "```",
        ]);
        assert_eq!(parsed[0].options.len(), 1);
        assert_eq!(parsed[0].project_types, ["git"]);
    }

    #[test]
    fn an_option_survives_a_heading_that_falls_back_to_all_name() {
        let parsed = commands(&[
            "# `remove --force` {not an attribute block",
            "",
            "```bash",
            "true",
            "```",
        ]);
        assert_eq!(parsed[0].name, "remove");
        assert_eq!(parsed[0].options.len(), 1);
    }

    /// Options and placeholders may be declared in different places: the
    /// heading names what the user toggles, the info string what the source
    /// reads.
    #[test]
    fn an_option_in_the_heading_and_a_placeholder_in_the_info_string() {
        let parsed = commands(&[
            "# `build --release`",
            "",
            "- `--release`: on",
            "",
            "```bash ${_files}",
            "cargo build \"$@\"",
            "```",
        ]);

        assert_eq!(parsed[0].options.len(), 1);
        assert!(parsed[0].options[0].default);
        assert_eq!(parsed[0].placeholders().count(), 1);
        // Heading first, then the info string.
        assert!(matches!(parsed[0].args[0], ArgSpec::Option(0)));
        assert!(matches!(&parsed[0].args[1], ArgSpec::Placeholder(p) if p.name == "_files"));
    }

    #[test]
    fn options_in_both_places_are_kept_in_order() {
        let parsed = commands(&[
            "# `build --release`",
            "",
            "```bash -v ${_files}",
            "true",
            "```",
        ]);
        assert_eq!(
            parsed[0]
                .options
                .iter()
                .map(|o| o.name.as_str())
                .collect::<Vec<_>>(),
            ["release", "v"]
        );
        assert!(matches!(parsed[0].args[0], ArgSpec::Option(0)));
        assert!(matches!(parsed[0].args[1], ArgSpec::Option(1)));
    }

    #[test]
    fn the_same_option_in_both_places_is_an_error() {
        let err = parse(
            FILE,
            &md(&[
                "# `build --release`",
                "",
                "```bash --release",
                "true",
                "```",
            ]),
        )
        .unwrap_err();
        assert_eq!(err.message, "Duplicate option: release");
    }

    /// A list item that reads as prose is prose, even when it starts with
    /// inline code and a colon.
    #[test]
    fn a_prose_list_item_naming_an_unknown_flag_is_not_a_declaration() {
        let parsed = commands(&[
            "# `show`",
            "",
            "- `-v`: increases verbosity",
            "",
            "```bash",
            "true",
            "```",
        ]);
        assert!(parsed[0].options.is_empty());
    }

    /// The same shape with an on/off value is a declaration, and naming an
    /// option that does not exist is a mistake.
    #[test]
    fn an_on_off_list_item_for_an_unknown_option_is_still_an_error() {
        let err = parse(
            FILE,
            &md(&["# `show`", "", "- `-v`: on", "", "```bash", "true", "```"]),
        )
        .unwrap_err();
        assert_eq!(err.message, "Undeclared option: v");
    }

    /// A declared option with prose where its value should be is a mistake
    /// too: the author meant a declaration.
    #[test]
    fn a_declared_option_with_a_prose_value_is_an_error() {
        let err = parse(
            FILE,
            &md(&[
                "# `show -v`",
                "",
                "- `-v`: increases verbosity",
                "",
                "```bash",
                "true",
                "```",
            ]),
        )
        .unwrap_err();
        assert_eq!(
            err.message,
            "Option -v is set to increases verbosity; only on and off are supported"
        );
    }

    #[test]
    fn an_undeclared_option_in_a_list_item_is_an_error() {
        let err = parse(
            FILE,
            &md(&[
                "# `remove --force`",
                "",
                "- `--quiet`: off",
                "",
                "```bash",
                "true",
                "```",
            ]),
        )
        .unwrap_err();
        assert_eq!(err.message, "Undeclared option: quiet");
    }

    #[test]
    fn a_value_that_is_not_on_or_off_is_not_supported_yet() {
        let err = parse(
            FILE,
            &md(&[
                "# `clone --depth`",
                "",
                "- `--depth`: 1",
                "",
                "```bash",
                "true",
                "```",
            ]),
        )
        .unwrap_err();
        assert_eq!(
            err.message,
            "Option --depth is set to 1; only on and off are supported"
        );
    }

    #[test]
    fn an_ordinary_list_item_is_not_a_declaration() {
        let parsed = commands(&[
            "# `remove --force`",
            "",
            "Removes a worktree.",
            "",
            "- one",
            "- `git` is not a declaration",
            "",
            "```bash",
            "true",
            "```",
        ]);
        assert_eq!(parsed[0].options.len(), 1);
        assert!(!parsed[0].options[0].default);
    }

    #[test]
    fn an_option_declared_in_the_info_string_works_the_same_way() {
        let parsed = commands(&[
            "# `remove`",
            "",
            "- `--force`: on",
            "",
            "```bash --force ${worktree}",
            "true",
            "```",
        ]);
        assert_eq!(parsed[0].options.len(), 1);
        assert!(parsed[0].options[0].default);
        assert_eq!(parsed[0].placeholders().count(), 1);
    }

    #[test]
    fn detects_command_by_code_block() {
        let parsed = commands(&["# `hello`", "```bash", "echo Hello World", "```"]);
        assert_eq!(parsed[0].lang, Language::Bash);
        assert_eq!(parsed[0].source, "echo Hello World\n");
        assert!(!parsed[0].is_bg);
    }

    #[rstest]
    #[case("${placeholder | cols 1}", PlaceholderFormat::Columns { has_header: false, cols: vec![1] })]
    #[case("${placeholder | cols+h 1}", PlaceholderFormat::Columns { has_header: true, cols: vec![1] })]
    #[case("${placeholder | json}", PlaceholderFormat::Json)]
    fn detects_output_format(#[case] info: &str, #[case] expected: PlaceholderFormat) {
        let fence = format!("```bash {info}");
        let parsed = commands(&["# `hello`", &fence, "echo Hello World", "```"]);
        assert_eq!(parsed[0].name, "hello");
        assert_eq!(parsed[0].placeholders().next().unwrap().format, expected);
    }

    #[test]
    fn errors_on_combined_columns_and_fields() {
        assert!(
            error(&[
                "# `hello`",
                "```bash ${placeholder | cols 1 | fields 2}",
                "echo Hello World",
                "```"
            ])
            .contains("Placeholder format already set")
        );
    }

    #[test]
    fn detects_project_type() {
        let parsed = commands(&[
            "# `hello` {type=\"git\"}",
            "```bash",
            "echo Hello World",
            "```",
        ]);
        assert_eq!(parsed[0].project_types, ["git"]);
    }

    #[test]
    fn detects_background_commands_by_ampersand() {
        let parsed = commands(&["# `hello &`", "```bash", "echo Hello World", "```"]);
        assert_eq!(parsed[0].name, "hello");
        assert!(parsed[0].is_bg);
    }

    #[test]
    fn supports_alternate_header_format() {
        assert_eq!(
            names(&["`hello`", "=======", "```bash", "echo Hello World", "```"]),
            ["hello"]
        );
    }

    #[test]
    fn extracts_environment_placeholders() {
        let parsed = commands(&[
            "# `hello ${arg} ${another-arg} &`",
            "```bash",
            "echo Hello \"$arg\" \"$another_arg\"",
            "```",
        ]);
        assert_eq!(parsed[0].name, "hello");
        assert!(parsed[0].is_bg);
        assert_eq!(
            parsed[0].placeholders().cloned().collect::<Vec<_>>(),
            vec![arg("arg"), arg("another-arg")]
        );
    }

    #[test]
    fn extracts_code_block_placeholders() {
        let parsed = commands(&[
            "# `one`",
            "``` bash ${arg-one}",
            "echo Hello \"$1\"",
            "```",
            "",
            "# `two`",
            "``` bash ${arg-two:m}",
            "echo Hello \"$1\"",
            "```",
            "",
            "# `three`",
            "``` bash ${arg-three:1,2}",
            "echo Hello \"$1\"",
            "```",
            "# `four`",
            "``` bash ${arg-four | fields 1,2 | multi}",
            "echo Hello \"$1\"",
            "```",
        ]);
        let formats: Vec<_> = parsed
            .iter()
            .map(|c| {
                (
                    c.lang.clone(),
                    c.placeholders().next().unwrap().name.clone(),
                    c.placeholders().next().unwrap().format.clone(),
                    c.placeholders().next().unwrap().multiple,
                )
            })
            .collect();
        assert_eq!(
            formats,
            vec![
                (
                    Language::Bash,
                    "arg-one".to_owned(),
                    PlaceholderFormat::Lines,
                    false
                ),
                (
                    Language::Bash,
                    "arg-two".to_owned(),
                    PlaceholderFormat::Lines,
                    true
                ),
                (
                    Language::Bash,
                    "arg-three".to_owned(),
                    PlaceholderFormat::Fields(vec![1, 2]),
                    false
                ),
                (
                    Language::Bash,
                    "arg-four".to_owned(),
                    PlaceholderFormat::Fields(vec![1, 2]),
                    true
                ),
            ]
        );
    }

    #[test]
    fn complains_on_both_header_and_code_block_placeholders() {
        assert_eq!(
            error(&[
                "# `hello` ${foo}",
                "```bash ${bar}",
                "echo Hello \"$bar\"",
                "```"
            ]),
            "some-file.md:1 hello uses placeholders in both command header and source code block"
        );
    }

    fn location(file: &str, start: usize, end: usize, level: usize) -> CommandLocation {
        CommandLocation {
            file_path: PathBuf::from(file),
            start_line: start,
            end_line: end,
            level,
        }
    }

    #[test]
    fn finds_location_single() {
        let parsed = commands(&["# `foo`", "```bash", "echo Hello World", "```"]);
        assert_eq!(parsed[0].location, Some(location(FILE, 1, 4, 1)));
    }

    #[test]
    fn finds_location_multiple() {
        let parsed = commands(&[
            "# `foo`",
            "```bash",
            "echo Hello World",
            "```",
            "",
            "## `bar`",
            "```bash ${foo}",
            "echo Hello \"$foo\"",
            "```",
            "",
            "# `baz`",
            "```bash ${bar}",
            "echo Hello \"$bar\"",
            "```",
        ]);
        let locations: Vec<_> = parsed.iter().map(|c| c.location.clone()).collect();
        assert_eq!(
            locations,
            vec![
                Some(location(FILE, 1, 5, 1)),
                Some(location(FILE, 6, 10, 2)),
                Some(location(FILE, 11, 14, 1)),
            ]
        );
    }

    #[test]
    fn finds_location_with_config() {
        let parsed = commands(&[
            "# `foo`",
            "```bash",
            "echo Hello World",
            "```",
            "",
            "## `bar`",
            "```bash ${foo}",
            "echo Hello \"$foo\"",
            "```",
            "",
            "# Config",
            "",
            "```yaml config",
            "{}",
            "```",
            "",
            "# `baz`",
            "```bash ${bar}",
            "echo Hello \"$bar\"",
            "```",
        ]);
        let locations: Vec<_> = parsed.iter().map(|c| c.location.clone()).collect();
        assert_eq!(
            locations,
            vec![
                Some(location(FILE, 1, 5, 1)),
                Some(location(FILE, 6, 10, 2)),
                Some(location(FILE, 17, 20, 1)),
            ]
        );
    }

    #[test]
    fn a_section_heading_type_applies_to_commands_nested_under_it() {
        let parsed = commands(&[
            "## Git stuff {type=\"git\"}",
            "",
            "### `git-files`",
            "```bash",
            "git ls-files",
            "```",
        ]);
        assert_eq!(parsed[0].name, "git-files");
        assert_eq!(parsed[0].project_types, ["git"]);
    }

    #[test]
    fn a_section_heading_type_stops_at_a_sibling_heading() {
        let parsed = commands(&[
            "## Git stuff {type=\"git\"}",
            "",
            "### `git-files`",
            "```bash",
            "git ls-files",
            "```",
            "",
            "## Other stuff",
            "",
            "### `anything`",
            "```bash",
            "echo anything",
            "```",
        ]);
        assert_eq!(parsed[0].project_types, ["git"]);
        assert!(parsed[1].project_types.is_empty());
    }

    #[test]
    fn inherited_types_are_ordered_innermost_first() {
        let parsed = commands(&[
            "# Outer {type=\"outer\"}",
            "",
            "## Middle {type=\"middle\"}",
            "",
            "### `cmd` {type=\"own\"}",
            "```bash",
            "echo hi",
            "```",
        ]);
        assert_eq!(parsed[0].project_types, ["own", "middle", "outer"]);
    }

    #[test]
    fn a_command_heading_type_is_inherited_by_nested_commands() {
        let parsed = commands(&[
            "### `sub-parent` {type=\"git\"}",
            "```bash",
            "echo parent",
            "```",
            "#### `sub-child`",
            "```bash",
            "echo child",
            "```",
        ]);
        assert_eq!(parsed[0].project_types, ["git"]);
        assert_eq!(parsed[1].project_types, ["git"]);
    }

    #[test]
    fn non_paragraph_blocks_before_the_code_block_are_skipped() {
        let parsed = commands(&[
            "# `hello`",
            "",
            "Command description.",
            "",
            "- a list item",
            "- another",
            "",
            "> a block quote",
            "",
            "```bash",
            "echo Hello World",
            "```",
        ]);
        assert_eq!(parsed[0].name, "hello");
        assert_eq!(
            parsed[0].desc.as_ref().map(Description::plain).as_deref(),
            Some("Command description.")
        );
        assert_eq!(parsed[0].source, "echo Hello World\n");
    }

    #[test]
    fn a_config_block_may_sit_between_commands() {
        let parsed = parse(
            FILE,
            &md(&[
                "# `foo`",
                "```bash",
                "echo foo",
                "```",
                "",
                "```json config",
                "{\"use_nix\": true}",
                "```",
                "",
                "# `bar`",
                "```bash",
                "echo bar",
                "```",
            ]),
        )
        .unwrap();
        assert_eq!(parsed.commands.len(), 2);
        assert_eq!(parsed.config.and_then(|c| c.use_nix), Some(true));
    }
}
