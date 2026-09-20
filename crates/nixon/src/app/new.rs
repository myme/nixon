//! `nixon new`. SPEC §10.4.

use std::path::Path;

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
            Selection::Canceled => return Err(NixonError::Canceled),
            Selection::Selected { items, .. } if items.len() > 1 => {
                return Err(NixonError::NothingSelected(
                    "Multiple commands selected.".to_owned(),
                ));
            }
            Selection::Selected { items, .. } => items
                .into_iter()
                .next()
                .ok_or_else(|| NixonError::NothingSelected("No command selected.".to_owned()))?,
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

        // The diff is for the user to read; a non-zero exit just means the
        // files differ. SPEC §10.4.
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

        let path = location.file_path.clone();
        if confirm(&format!("Update {}? [y/N] ", path.display()))? {
            replace(&path, temp.path())?;
            tracing::info!("Updating {}…", path.display());
        } else {
            tracing::info!("Update canceled.");
        }

        Ok(0)
    }
}

/// Asks on stderr and reads the answer from stdin. SPEC §10.4.
///
/// The prompt is for a person, so it goes where the picker goes; stdout
/// carries data. Only a bare `y` or `Y` accepts; anything else, including
/// end of input, leaves the file alone.
fn confirm(prompt: &str) -> Result<bool> {
    use std::io::{BufRead as _, Write as _};

    let mut err = std::io::stderr().lock();
    err.write_all(prompt.as_bytes())?;
    err.flush()?;
    drop(err);

    let mut answer = String::new();
    std::io::stdin().lock().read_line(&mut answer)?;
    Ok(matches!(answer.trim(), "y" | "Y"))
}

/// Puts `source`'s contents in `path`, atomically and in place.
///
/// A copy truncates first, so an interrupted write leaves half a config
/// file. A sibling plus a rename never does, and starting the sibling from
/// the original's mode keeps the file's permissions.
fn replace(path: &Path, source: &Path) -> Result<()> {
    use std::io::Write as _;

    let dir = path.parent().unwrap_or_else(|| Path::new("."));
    let mut temp = tempfile::Builder::new().prefix(".nixon").tempfile_in(dir)?;
    temp.write_all(&std::fs::read(source)?)?;
    temp.flush()?;

    #[cfg(unix)]
    if let Ok(meta) = std::fs::metadata(path) {
        use std::os::unix::fs::PermissionsExt as _;
        let mode = meta.permissions().mode();
        let _ = temp
            .as_file()
            .set_permissions(std::fs::Permissions::from_mode(mode));
    }

    temp.persist(path).map_err(|err| err.error)?;
    Ok(())
}

/// Inserts the template after `end_line`. SPEC §10.4.
///
/// Lines keep their terminators, so a file with CRLF endings or without a
/// final newline comes back as it went in, everywhere but the new section.
fn splice(original: &str, end_line: usize, level: usize, opts: &NewOpts) -> String {
    let lines: Vec<&str> = original.split_inclusive('\n').collect();
    let (before, after) = lines.split_at(end_line.min(lines.len()));

    // The new section follows whatever the file already uses.
    let nl = if original.contains("\r\n") {
        "\r\n"
    } else {
        "\n"
    };
    let template = [
        format!("{} `{}`", "#".repeat(level.max(1)), opts.name),
        String::new(),
        opts.desc.clone(),
        String::new(),
        format!("```{}", opts.lang),
        opts.src.clone(),
        "```".to_owned(),
        String::new(),
    ]
    .join(nl);

    let mut out: String = before.concat();
    // The section has to start on a line of its own.
    if !out.is_empty() && !out.ends_with('\n') {
        out.push_str(nl);
    }
    out.push_str(&template);
    out.push_str(&after.concat());
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
    fn crlf_endings_and_a_missing_final_newline_survive() {
        let original = "# `foo`\r\n\r\n```bash\r\necho foo\r\n```\r\n\r\n# `baz`";
        let spliced = splice(original, 5, 1, &NewOpts::default());

        assert!(
            !spliced.contains("\n\n") || spliced.contains("\r\n"),
            "line endings were rewritten: {spliced:?}"
        );
        assert_eq!(
            spliced.matches('\n').count(),
            spliced.matches("\r\n").count()
        );
        assert!(spliced.ends_with("# `baz`"), "a final newline was added");
    }

    #[test]
    fn the_original_bytes_outside_the_new_section_are_untouched() {
        let original = "# `foo`\n\n```bash\necho foo\n```\n\n# `baz`";
        let spliced = splice(original, 5, 1, &NewOpts::default());

        assert!(spliced.starts_with("# `foo`\n\n```bash\necho foo\n```\n"));
        assert!(spliced.ends_with("\n\n# `baz`"));
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
