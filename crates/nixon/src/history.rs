//! The log of what has been run.
//!
//! One line per executed command, in a form that can be run again. The shell
//! widgets read it to put those lines in the shell's own history.

use std::io::Write as _;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

/// One recorded run: when, where, and the command line that repeats it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry {
    /// Seconds since the epoch.
    pub at: u64,
    /// The directory nixon was invoked in.
    pub cwd: String,
    /// The arguments after `nixon`, already shell-quoted.
    pub invocation: Vec<String>,
}

impl Entry {
    /// Builds an entry timestamped now.
    pub fn new(cwd: &Path, invocation: Vec<String>) -> Self {
        Self {
            at: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_or(0, |since| since.as_secs()),
            cwd: cwd.to_string_lossy().into_owned(),
            invocation,
        }
    }

    /// The line as it is stored: three tab-separated fields.
    ///
    /// Tabs separate them because a path may hold anything else. The
    /// invocation is shell-quoted, so it holds no tab of its own.
    pub fn line(&self) -> String {
        format!(
            "{}\t{}\t{}\n",
            self.at,
            self.cwd,
            shell_words::join(
                std::iter::once("nixon").chain(self.invocation.iter().map(String::as_str))
            )
        )
    }
}

/// Appends an entry to the log, or gives up quietly.
///
/// A history that cannot be written is not a reason to fail a command that
/// has already run, so every failure here is a debug message and nothing
/// more.
pub fn record(path: &Path, entry: &Entry) {
    if let Err(err) = append(path, &entry.line()) {
        tracing::debug!("could not record history in {}: {err}", path.display());
    }
}

/// One `write` of one whole line, appended.
///
/// Two nixons running at once both append; a single write of a line shorter
/// than a pipe buffer is what stops their lines interleaving.
fn append(path: &Path, line: &str) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)?;
    file.write_all(line.as_bytes())
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{Entry, record};

    fn entry(invocation: &[&str]) -> Entry {
        Entry {
            at: 1_700_000_000,
            cwd: "/home/me/code".to_owned(),
            invocation: invocation.iter().map(|s| (*s).to_owned()).collect(),
        }
    }

    #[test]
    fn a_line_is_time_cwd_and_the_command() {
        assert_eq!(
            entry(&["run", "hello"]).line(),
            "1700000000\t/home/me/code\tnixon run hello\n"
        );
    }

    #[test]
    fn arguments_are_quoted_so_the_line_can_be_run_again() {
        assert_eq!(
            entry(&["run", "edit", "a file.txt"]).line(),
            "1700000000\t/home/me/code\tnixon run edit 'a file.txt'\n"
        );
    }

    #[test]
    fn recording_creates_the_directory_and_appends() {
        let temp = TempDir::new().unwrap();
        let path = temp.child("state/nixon/history");

        record(path.path(), &entry(&["run", "one"]));
        record(path.path(), &entry(&["run", "two"]));

        let written = std::fs::read_to_string(path.path()).unwrap();
        assert_eq!(written.lines().count(), 2);
        assert!(written.ends_with("nixon run two\n"));
    }

    #[test]
    fn a_path_that_cannot_be_written_is_not_an_error() {
        let temp = TempDir::new().unwrap();
        let blocker = temp.child("blocker");
        blocker.write_str("not a directory\n").unwrap();

        // Recording under a regular file fails; the command still ran.
        record(&blocker.path().join("nixon/history"), &entry(&["run", "x"]));
    }

    #[test]
    fn an_entry_is_timestamped_now() {
        let entry = Entry::new(Path::new("/tmp"), vec!["run".to_owned()]);
        assert!(entry.at > 1_700_000_000, "got {}", entry.at);
    }
}
