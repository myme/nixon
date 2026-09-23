//! One-use invocation payloads for a GUI to terminal process handoff.

use std::fs;
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};

use nixon::error::{NixonError, Result};
use nixon::process::{Invocation, ProcessRunner as _, RealRunner};

/// Writes an invocation to a private temporary file and returns its path.
///
/// The caller owns the path after this returns. A failed handoff must remove
/// it; `read_payload` removes it as soon as the receiving process reads it.
#[cfg_attr(
    not(test),
    expect(dead_code, reason = "the GUI terminal handoff will call this writer")
)]
pub fn write_payload(invocation: &Invocation) -> io::Result<PathBuf> {
    let mut file = tempfile::Builder::new()
        .prefix("nixon-gui-exec-")
        .tempfile()?;
    serde_json::to_writer(&mut file, invocation).map_err(io::Error::other)?;
    file.flush()?;
    file.into_temp_path().keep().map_err(|err| err.error)
}

/// Reads and removes a one-use invocation payload, including invalid JSON.
pub fn read_payload(path: &Path) -> io::Result<Invocation> {
    let bytes = fs::read(path)?;
    fs::remove_file(path)?;
    serde_json::from_slice(&bytes).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
}

/// Runs the prepared invocation with inherited stdout and stderr.
pub fn run_payload(path: &Path) -> Result<i32> {
    let invocation = read_payload(path).map_err(|err| {
        NixonError::Io(io::Error::new(
            err.kind(),
            format!(
                "Could not read GUI execution payload {}: {err}",
                path.display()
            ),
        ))
    })?;
    RealRunner.run(&invocation).map_err(Into::into)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use nixon::process::Invocation;

    use super::{read_payload, write_payload};

    #[test]
    fn payload_round_trips_and_is_removed_after_reading() {
        let invocation = Invocation {
            argv: vec![
                "program with spaces".to_owned(),
                "a 'quoted' argument".to_owned(),
            ],
            cwd: Some("/tmp/a directory with 'quotes'".into()),
            env: vec![(
                "ODD_VALUE".to_owned(),
                "two words and \"quotes\"".to_owned(),
            )],
            stdin: Some(vec!["first line".to_owned(), "second 'line'".to_owned()]),
        };
        let path = write_payload(&invocation).unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;

            let mode = fs::metadata(&path).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o600);
        }
        assert_eq!(read_payload(&path).unwrap(), invocation);
        assert!(!path.exists());
    }

    #[test]
    fn malformed_payload_is_rejected_and_removed() {
        let path = write_payload(&Invocation::default()).unwrap();
        fs::write(&path, b"{not valid JSON}").unwrap();
        let error = read_payload(&path).unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);
        assert!(!path.exists());
    }
}
