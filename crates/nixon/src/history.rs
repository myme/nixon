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
    /// The arguments after `nixon`, unquoted.
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

/// Parses one stored line back into an entry.
///
/// A line that is not three fields is from a future format or a damaged
/// write; skipping it is better than refusing to show any history at all.
fn parse(line: &str) -> Option<Entry> {
    let mut fields = line.splitn(3, '\t');
    let at = fields.next()?.parse().ok()?;
    let cwd = fields.next()?.to_owned();
    let mut invocation = shell_words::split(fields.next()?).ok()?;
    // Stored with the program name, held without it: the field is a command
    // line, the struct is the arguments.
    if invocation.first().is_some_and(|word| word == "nixon") {
        invocation.remove(0);
    }
    (!invocation.is_empty()).then_some(Entry {
        at,
        cwd,
        invocation,
    })
}

/// Every entry in the log, oldest first.
///
/// A log that cannot be read is an empty one: there is nothing to show, and
/// nothing has gone wrong that the user can act on.
pub fn read(path: &Path) -> Vec<Entry> {
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .filter_map(parse)
        .collect()
}

/// The entries to show: newest first, runs of the same command collapsed,
/// and at most `limit` of them.
///
/// Consecutive duplicates only. Running something, then something else,
/// then the first again is three things the user did.
pub fn recent(entries: Vec<Entry>, limit: Option<usize>) -> Vec<Entry> {
    let mut out: Vec<Entry> = Vec::new();
    for entry in entries.into_iter().rev() {
        if out.last().is_some_and(|last: &Entry| {
            last.invocation == entry.invocation && last.cwd == entry.cwd
        }) {
            continue;
        }
        out.push(entry);
        if limit.is_some_and(|limit| out.len() >= limit) {
            break;
        }
    }
    out
}

/// Empties the log.
pub fn clear(path: &Path) -> std::io::Result<()> {
    match std::fs::write(path, "") {
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
        other => other,
    }
}

/// How long ago, in the roughest unit that still says something.
pub fn ago(at: u64, now: u64) -> String {
    let seconds = now.saturating_sub(at);
    let (amount, unit) = match seconds {
        0..=59 => (seconds, "s"),
        60..=3599 => (seconds / 60, "m"),
        3600..=86399 => (seconds / 3600, "h"),
        _ => (seconds / 86400, "d"),
    };
    format!("{amount}{unit}")
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
        create_dir(parent)?;
    }

    let mut options = std::fs::OpenOptions::new();
    options.create(true).append(true);
    // Every value the user has ever picked is in here, and every `eval`
    // source: it is theirs to read, nobody else's.
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt as _;
        options.mode(0o600);
    }

    let mut file = options.open(path)?;
    file.write_all(line.as_bytes())
}

/// Creates the log's directory, private to its owner.
fn create_dir(path: &Path) -> std::io::Result<()> {
    let mut builder = std::fs::DirBuilder::new();
    builder.recursive(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::DirBuilderExt as _;
        builder.mode(0o700);
    }
    builder.create(path)
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

    /// The log is the user's own: it names every value they ever picked.
    #[test]
    #[cfg(unix)]
    fn the_log_and_its_directory_are_private() {
        use std::os::unix::fs::PermissionsExt as _;

        let temp = TempDir::new().unwrap();
        let path = temp.child("state/nixon/history");
        record(path.path(), &entry(&["run", "one"]));

        let mode = |p: &std::path::Path| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode(path.path()), 0o600);
        assert_eq!(mode(path.path().parent().unwrap()), 0o700);
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

#[cfg(test)]
mod reading {
    use assert_fs::TempDir;
    use assert_fs::prelude::*;

    use super::{Entry, ago, read, recent};

    fn entry(at: u64, invocation: &[&str]) -> Entry {
        Entry {
            at,
            cwd: "/home/me".to_owned(),
            invocation: invocation.iter().map(|s| (*s).to_owned()).collect(),
        }
    }

    #[test]
    fn a_written_line_reads_back() {
        let temp = TempDir::new().unwrap();
        let path = temp.child("history");
        let original = entry(1_700_000_000, &["run", "edit", "a file.txt"]);
        path.write_str(&original.line()).unwrap();

        assert_eq!(read(path.path()), [original]);
    }

    #[test]
    fn a_damaged_line_is_skipped_rather_than_fatal() {
        let temp = TempDir::new().unwrap();
        let path = temp.child("history");
        path.write_str("not a log line\n1700000000\t/home/me\tnixon run ok\n\n")
            .unwrap();

        let entries = read(path.path());
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].invocation, ["run", "ok"]);
    }

    #[test]
    fn a_missing_log_reads_as_empty() {
        let temp = TempDir::new().unwrap();
        assert!(read(temp.child("nothing").path()).is_empty());
    }

    #[test]
    fn the_newest_comes_first() {
        let entries = vec![
            entry(1, &["run", "one"]),
            entry(2, &["run", "two"]),
            entry(3, &["run", "three"]),
        ];
        let shown: Vec<u64> = recent(entries, None).iter().map(|e| e.at).collect();
        assert_eq!(shown, [3, 2, 1]);
    }

    #[test]
    fn a_run_of_the_same_command_collapses_to_one() {
        let entries = vec![
            entry(1, &["run", "one"]),
            entry(2, &["run", "two"]),
            entry(3, &["run", "two"]),
            entry(4, &["run", "two"]),
        ];
        let shown: Vec<Vec<String>> = recent(entries, None)
            .into_iter()
            .map(|e| e.invocation)
            .collect();
        assert_eq!(shown.len(), 2);
        assert_eq!(shown[0], ["run", "two"]);
        assert_eq!(shown[1], ["run", "one"]);
    }

    /// Only consecutive ones: doing something else in between makes it a
    /// separate thing the user did.
    #[test]
    fn the_same_command_returned_to_is_kept() {
        let entries = vec![
            entry(1, &["run", "one"]),
            entry(2, &["run", "two"]),
            entry(3, &["run", "one"]),
        ];
        assert_eq!(recent(entries, None).len(), 3);
    }

    #[test]
    fn a_limit_keeps_the_newest() {
        let entries = vec![
            entry(1, &["run", "one"]),
            entry(2, &["run", "two"]),
            entry(3, &["run", "three"]),
        ];
        let shown: Vec<u64> = recent(entries, Some(2)).iter().map(|e| e.at).collect();
        assert_eq!(shown, [3, 2]);
    }

    #[test]
    fn relative_times_use_the_roughest_useful_unit() {
        let now = 10_000_000;
        assert_eq!(ago(now, now), "0s");
        assert_eq!(ago(now - 45, now), "45s");
        assert_eq!(ago(now - 90, now), "1m");
        assert_eq!(ago(now - 7200, now), "2h");
        assert_eq!(ago(now - 86400 * 3, now), "3d");
        // A clock that went backwards is not negative time.
        assert_eq!(ago(now + 100, now), "0s");
    }
}
