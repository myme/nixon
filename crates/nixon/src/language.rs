//! Script languages: how a code block's info string names them, what they are
//! saved as, and what runs them. SPEC §7.1.

use std::fmt;
use std::path::Path;

/// A command's language, as named by a markdown code block's info string.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum Language {
    /// `sh`, `bash`.
    Bash,
    /// `haskell`.
    Haskell,
    /// `js`, `javascript`.
    JavaScript,
    /// `json`.
    Json,
    /// `plain`.
    Plain,
    /// `python`.
    Python,
    /// `yaml`.
    Yaml,
    /// An info string nixon does not know; it has no interpreter.
    Unknown(String),
    /// No info string at all; runs under `$SHELL`.
    #[default]
    None,
}

impl Language {
    /// The extension a script of this language is cached under. SPEC §7.2.
    pub const fn extension(&self) -> &'static str {
        match self {
            Self::Bash | Self::None => ".sh",
            Self::Haskell => ".hs",
            Self::JavaScript => ".js",
            Self::Json => ".json",
            Self::Plain | Self::Unknown(_) => ".txt",
            Self::Python => ".py",
            Self::Yaml => ".yaml",
        }
    }

    /// The argv that runs a script of this language, given the user's `$SHELL`.
    ///
    /// `None` for [`Language::Unknown`], which the evaluator reports as
    /// `No interpreter for <lang>`.
    pub fn interpreter(&self, shell: Option<&str>) -> Option<Vec<String>> {
        let argv: Vec<&str> = match self {
            Self::Bash => vec!["bash"],
            Self::None => vec![shell.unwrap_or("bash")],
            Self::Haskell => vec!["runghc"],
            Self::JavaScript => vec!["node"],
            Self::Json => vec!["jq", "-r", "."],
            Self::Plain => vec!["cat"],
            Self::Python => vec!["python3"],
            Self::Yaml => vec!["yq", "-r", "."],
            Self::Unknown(_) => return Option::None,
        };
        Some(argv.into_iter().map(ToOwned::to_owned).collect())
    }

    /// The language of a file, by extension. Used by `eval --file`.
    pub fn from_file_path(path: impl AsRef<Path>) -> Self {
        match path.as_ref().extension().and_then(|ext| ext.to_str()) {
            Some("sh") => Self::Bash,
            Some("hs") => Self::Haskell,
            Some("js") => Self::JavaScript,
            Some("json") => Self::Json,
            Some("txt") => Self::Plain,
            Some("py") => Self::Python,
            Some(other) => Self::Unknown(other.to_owned()),
            Option::None => Self::Unknown(String::new()),
        }
    }
}

/// Matching is case-sensitive: `Bash` is [`Language::Unknown`], not Bash.
impl From<&str> for Language {
    fn from(name: &str) -> Self {
        match name {
            "sh" | "bash" => Self::Bash,
            "haskell" => Self::Haskell,
            "js" | "javascript" => Self::JavaScript,
            "json" => Self::Json,
            "plain" => Self::Plain,
            "python" => Self::Python,
            "yaml" => Self::Yaml,
            "" => Self::None,
            other => Self::Unknown(other.to_owned()),
        }
    }
}

impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Bash => "bash",
            Self::Haskell => "haskell",
            Self::JavaScript => "javascript",
            Self::Json => "json",
            Self::Plain => "plain",
            Self::Python => "python",
            Self::Yaml => "yaml",
            Self::Unknown(lang) => lang,
            Self::None => "",
        };
        f.write_str(name)
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::Language;

    #[rstest]
    #[case("sh", Language::Bash)]
    #[case("bash", Language::Bash)]
    #[case("haskell", Language::Haskell)]
    #[case("js", Language::JavaScript)]
    #[case("javascript", Language::JavaScript)]
    #[case("json", Language::Json)]
    #[case("plain", Language::Plain)]
    #[case("python", Language::Python)]
    #[case("yaml", Language::Yaml)]
    #[case("", Language::None)]
    #[case("ruby", Language::Unknown("ruby".to_owned()))]
    fn info_string_names_a_language(#[case] name: &str, #[case] expected: Language) {
        assert_eq!(Language::from(name), expected);
    }

    #[rstest]
    #[case("Bash")]
    #[case("BASH")]
    #[case("Python")]
    fn language_matching_is_case_sensitive(#[case] name: &str) {
        assert_eq!(Language::from(name), Language::Unknown(name.to_owned()));
    }

    #[rstest]
    #[case(Language::Bash, "bash")]
    #[case(Language::Haskell, "haskell")]
    #[case(Language::JavaScript, "javascript")]
    #[case(Language::Json, "json")]
    #[case(Language::Plain, "plain")]
    #[case(Language::Python, "python")]
    #[case(Language::Yaml, "yaml")]
    #[case(Language::None, "")]
    #[case(Language::Unknown("ruby".to_owned()), "ruby")]
    fn display_is_the_lowercase_name(#[case] lang: Language, #[case] expected: &str) {
        assert_eq!(lang.to_string(), expected);
    }

    #[rstest]
    #[case("script.sh", Language::Bash)]
    #[case("Main.hs", Language::Haskell)]
    #[case("app.js", Language::JavaScript)]
    #[case("data.json", Language::Json)]
    #[case("notes.txt", Language::Plain)]
    #[case("script.py", Language::Python)]
    fn file_extension_names_a_language(#[case] path: &str, #[case] expected: Language) {
        assert_eq!(Language::from_file_path(path), expected);
    }

    #[rstest]
    #[case("config.yaml", "yaml")]
    #[case("script.rb", "rb")]
    fn an_unmapped_extension_is_unknown_under_that_extension(
        #[case] path: &str,
        #[case] ext: &str,
    ) {
        assert_eq!(
            Language::from_file_path(path),
            Language::Unknown(ext.to_owned())
        );
    }

    #[rstest]
    #[case("Makefile")]
    #[case("/usr/bin/env")]
    fn a_path_without_an_extension_is_unknown_under_the_empty_name(#[case] path: &str) {
        assert_eq!(
            Language::from_file_path(path),
            Language::Unknown(String::new())
        );
    }

    #[rstest]
    #[case(Language::Bash, ".sh")]
    #[case(Language::None, ".sh")]
    #[case(Language::Haskell, ".hs")]
    #[case(Language::JavaScript, ".js")]
    #[case(Language::Json, ".json")]
    #[case(Language::Plain, ".txt")]
    #[case(Language::Python, ".py")]
    #[case(Language::Yaml, ".yaml")]
    #[case(Language::Unknown("ruby".to_owned()), ".txt")]
    fn cache_extension_follows_the_language(#[case] lang: Language, #[case] expected: &str) {
        assert_eq!(lang.extension(), expected);
    }

    #[rstest]
    #[case(Language::Bash, vec!["bash"])]
    #[case(Language::Haskell, vec!["runghc"])]
    #[case(Language::JavaScript, vec!["node"])]
    #[case(Language::Json, vec!["jq", "-r", "."])]
    #[case(Language::Plain, vec!["cat"])]
    #[case(Language::Python, vec!["python3"])]
    #[case(Language::Yaml, vec!["yq", "-r", "."])]
    fn interpreter_argv_is_fixed_per_language(#[case] lang: Language, #[case] expected: Vec<&str>) {
        assert_eq!(
            lang.interpreter(Some("/bin/zsh")),
            Some(to_owned(&expected))
        );
    }

    #[test]
    fn no_language_runs_under_the_shell() {
        assert_eq!(
            Language::None.interpreter(Some("/bin/zsh")),
            Some(vec!["/bin/zsh".to_owned()])
        );
    }

    #[test]
    fn no_language_falls_back_to_bash_without_a_shell() {
        assert_eq!(
            Language::None.interpreter(None),
            Some(vec!["bash".to_owned()])
        );
    }

    #[test]
    fn an_unknown_language_has_no_interpreter() {
        assert_eq!(
            Language::Unknown("ruby".to_owned()).interpreter(Some("/bin/zsh")),
            None
        );
    }

    fn to_owned(argv: &[&str]) -> Vec<String> {
        argv.iter().map(|arg| (*arg).to_owned()).collect()
    }
}
