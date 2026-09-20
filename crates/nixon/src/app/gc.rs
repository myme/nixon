//! `nixon gc`.

use nixon_picker::Picker;

use super::App;
use crate::error::Result;
use crate::eval::cache;
use crate::output;
use crate::process::{ExitCode, ProcessRunner};

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Empties the script cache, reporting each file on stdout.
    pub fn gc(&mut self, dry_run: bool) -> Result<ExitCode> {
        let reported = cache::garbage_collect(&self.dirs.cache_dir(), dry_run)?;
        output::lines(&reported)?;
        Ok(0)
    }
}
