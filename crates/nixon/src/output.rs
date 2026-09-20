//! The only module that writes to stdout. SPEC §10.8.
//!
//! Everything else goes to stderr through `tracing`, so the shell widgets
//! can read stdout as data. The `print_stdout` lint catches the `println!`
//! family elsewhere; writing through a locked handle, as here, is the
//! convention this module exists to keep in one place.

use std::io::{self, Write as _};

/// Writes one line of data to stdout. SPEC §10.8.
pub fn line(text: &str) -> io::Result<()> {
    let mut stdout = io::stdout().lock();
    writeln!(stdout, "{text}")
}

/// Writes each value on its own line. SPEC §10.1, §10.2.
pub fn lines<S: AsRef<str>>(values: &[S]) -> io::Result<()> {
    for value in values {
        line(value.as_ref())?;
    }
    Ok(())
}

/// Writes text exactly as given, with no trailing newline added.
///
/// `--insert` prints a command's source, which already ends in a newline.
/// SPEC §10.7.
pub fn raw(text: &str) -> io::Result<()> {
    let mut stdout = io::stdout().lock();
    write!(stdout, "{text}")
}
