//! `nixon new`. SPEC §10.4.

use nixon_picker::{Picker, Selection};

use super::App;
use crate::error::{NixonError, Result};
use crate::language::Language;
use crate::process::{ExitCode, Invocation, ProcessRunner};

/// The command to splice in. SPEC §10.4.
#[derive(Clone, Debug)]
pub struct NewOpts {
    /// Name of the new command.
    pub name: String,
    /// Its description.
    pub desc: String,
    /// Its language.
    pub lang: Language,
    /// Its source.
    pub src: String,
}

impl Default for NewOpts {
    fn default() -> Self {
        Self {
            name: "<name>".to_owned(),
            desc: "Description…".to_owned(),
            lang: Language::Bash,
            src: String::new(),
        }
    }
}

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Splices a new command into a markdown file after a chosen one.
    /// SPEC §10.4.
    ///
    /// The temp file goes in the system temp directory, not `/tmp`, and a
    /// bin command is refused rather than spliced into an executable at line
    /// zero, both ENGINEERING §7.3 fixes.
    pub fn new_command(&mut self, opts: &NewOpts) -> Result<ExitCode> {
        let project = self.current_project();
        let commands = self.commands_for(&project)?;
        let selection = self.pick_command(&project, &commands, "Insert after", None)?;

        let after = match selection {
            Selection::Empty => {
                return Err(NixonError::NothingSelected(
                    "No command selected.".to_owned(),
                ));
            }
            Selection::Canceled => {
                return Err(NixonError::NothingSelected(
                    "Command selection canceled.".to_owned(),
                ));
            }
            Selection::Selected { items, .. } if items.len() > 1 => {
                return Err(NixonError::NothingSelected(
                    "Multiple commands selected.".to_owned(),
                ));
            }
            Selection::Selected { items, .. } => items.into_iter().next().unwrap_or_default(),
        };

        let location = after.location.as_ref().ok_or_else(|| {
            NixonError::NothingSelected("Unable to find command location.".to_owned())
        })?;
        if location.start_line == 0 {
            return Err(NixonError::NothingSelected(
                "cannot insert after a bin command".to_owned(),
            ));
        }

        let original = std::fs::read_to_string(&location.file_path)?;
        let spliced = splice(&original, location.end_line, location.level, opts);

        let temp = tempfile::Builder::new()
            .prefix("nixon")
            .suffix(".md")
            .tempfile()?;
        std::fs::write(temp.path(), &spliced)?;

        self.runner.run(&Invocation {
            argv: vec![
                self.env.editor().to_owned(),
                format!("+{}", location.end_line + 1),
                temp.path().to_string_lossy().into_owned(),
            ],
            ..Invocation::default()
        })?;

        self.runner.run(&Invocation {
            argv: vec![
                "diff".to_owned(),
                "-u".to_owned(),
                "--color=always".to_owned(),
                location.file_path.to_string_lossy().into_owned(),
                temp.path().to_string_lossy().into_owned(),
            ],
            ..Invocation::default()
        })?;

        Ok(0)
    }
}

/// Inserts the template after `end_line`. SPEC §10.4.
fn splice(original: &str, end_line: usize, level: usize, opts: &NewOpts) -> String {
    let lines: Vec<&str> = original.lines().collect();
    let (before, after) = lines.split_at(end_line.min(lines.len()));

    let template = format!(
        "{} `{}`\n\n{}\n\n```{}\n{}\n```\n",
        "#".repeat(level.max(1)),
        opts.name,
        opts.desc,
        opts.lang,
        opts.src
    );

    let mut out = before.join("\n");
    if !out.is_empty() {
        out.push('\n');
    }
    out.push_str(&template);
    if !after.is_empty() {
        out.push_str(&after.join("\n"));
        out.push('\n');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::{NewOpts, splice};
    use crate::language::Language;

    #[test]
    fn the_template_goes_after_the_chosen_command() {
        let original = "# `foo`\n\n```bash\necho foo\n```\n\n# `baz`\n";
        let opts = NewOpts {
            name: "bar".to_owned(),
            desc: "Does bar".to_owned(),
            lang: Language::Bash,
            src: "echo bar".to_owned(),
        };
        let spliced = splice(original, 5, 1, &opts);

        assert!(spliced.starts_with("# `foo`\n"));
        assert!(spliced.contains("# `bar`\n\nDoes bar\n\n```bash\necho bar\n```\n"));
        assert!(spliced.trim_end().ends_with("# `baz`"));
    }

    #[test]
    fn the_heading_level_matches_the_command_it_follows() {
        let spliced = splice("## `foo`\n", 1, 2, &NewOpts::default());
        assert!(spliced.contains("## `<name>`"));
    }

    #[test]
    fn defaults_match_the_spec() {
        let opts = NewOpts::default();
        assert_eq!(opts.name, "<name>");
        assert_eq!(opts.desc, "Description…");
        assert_eq!(opts.lang.to_string(), "bash");
        assert_eq!(opts.src, "");
    }
}
