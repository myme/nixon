//! `nixon history`.

use nixon_picker::{Candidate, FilterPicker, Picker, PickerOptions, Selection, SelectionType};

use super::App;
use crate::config::Config;
use crate::error::{NixonError, Result};
use crate::history;
use crate::output;
use crate::process::{ExitCode, ProcessRunner};
use crate::select;

/// How far back the picker looks when nothing says otherwise.
pub const DEFAULT_LIMIT: usize = 1000;

/// Whether an unreadable history log is treated as empty.
#[derive(Clone, Copy, Debug)]
pub enum HistoryReadMode {
    /// Treat an unreadable log as empty, as terminal History does.
    IgnoreErrors,
    /// Return read errors for the GUI to display.
    ReportErrors,
}

/// What `history` was asked to do.
#[derive(Clone, Debug, Default)]
pub struct HistoryOpts {
    /// Query for the picker, or the filter for `--list`.
    pub query: Option<String>,
    /// Print matching invocations instead of choosing one.
    pub list: bool,
    /// Pick one and print it rather than running it.
    pub select: bool,
    /// Keep only the last N entries.
    pub limit: Option<usize>,
    /// Empty the log.
    pub clear: bool,
}

/// What the caller should do next.
///
/// Re-running goes back through the command line, so the picked invocation
/// is handed back rather than interpreted here: the argument parser lives
/// in the binary, and this crate does not know it.
#[derive(Debug)]
pub enum Outcome {
    /// Nothing further; exit with this code.
    Done(ExitCode),
    /// Run these arguments as if they had been typed.
    Rerun(Vec<String>),
}

impl<P: Picker, R: ProcessRunner> App<P, R> {
    /// Loads the effective history and builds its picker candidates.
    pub fn history_candidates(
        &self,
        limit: Option<usize>,
        read_mode: HistoryReadMode,
    ) -> Result<(Config, Vec<Candidate>)> {
        let config = self.config_for(&self.current_project())?;
        if !config.records_history() {
            return Err(NixonError::HistoryDisabled);
        }
        let path = self.dirs.history_file();
        let entries = match read_mode {
            HistoryReadMode::IgnoreErrors => history::read(&path, limit),
            HistoryReadMode::ReportErrors => history::read_checked(&path, limit)?,
        };
        let entries = history::recent(entries, limit);
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(0, |since| since.as_secs());
        let candidates = select::history_candidates(&entries, &self.dirs.home, now);
        Ok((config, candidates))
    }

    /// Opens the shared history picker with a caller-specific header.
    pub fn pick_history_candidates(
        &mut self,
        config: &Config,
        candidates: Vec<Candidate>,
        query: Option<&str>,
        header: Option<&str>,
    ) -> Result<Selection<Candidate>> {
        let mut options = select::history_options(config, query);
        if let Some(header) = header {
            options.header = Some(header.to_owned());
        }
        Ok(self.picker.pick(&options, candidates)?)
    }

    /// Shows what has been run, and runs it again.
    pub fn history(&mut self, opts: &HistoryOpts) -> Result<Outcome> {
        if opts.clear {
            let config = self.config_for(&self.current_project())?;
            if !config.records_history() {
                return Err(NixonError::HistoryDisabled);
            }
            let path = self.dirs.history_file();
            if output::confirm(&format!("Clear {}? [y/N] ", path.display()))? {
                history::clear(&path)?;
                tracing::info!("Cleared {}…", path.display());
            } else {
                tracing::info!("Clear canceled.");
            }
            return Ok(Outcome::Done(0));
        }

        // The picker shows a tail; a listing without a limit shows the lot.
        let limit = opts
            .limit
            .or(if opts.list { None } else { Some(DEFAULT_LIMIT) });
        let (config, candidates) = self.history_candidates(limit, HistoryReadMode::IgnoreErrors)?;

        if opts.list {
            return Self::list(&config, candidates, opts.query.as_deref()).map(Outcome::Done);
        }

        match self.pick_history_candidates(&config, candidates, opts.query.as_deref(), None)? {
            Selection::Empty => Err(NixonError::NothingSelected(
                "No command selected.".to_owned(),
            )),
            Selection::Canceled => Err(NixonError::Canceled),
            Selection::Selected { kind, items } => {
                let Some(candidate) = items.into_iter().next() else {
                    return Err(NixonError::NothingSelected(
                        "No command selected.".to_owned(),
                    ));
                };
                match kind {
                    // `--select` is the widgets' way of pressing F1: pick
                    // one, print it, run nothing.
                    _ if opts.select => {
                        output::line(&candidate.value)?;
                        Ok(Outcome::Done(0))
                    }
                    // `Alt-Enter` hands the line back to be edited, which
                    // for a logged command line means printing it rather
                    // than opening the source `Edit` means elsewhere.
                    SelectionType::Show | SelectionType::Edit => {
                        output::line(&candidate.value)?;
                        Ok(Outcome::Done(0))
                    }
                    _ => {
                        // The value is a whole command line; what runs it
                        // is the arguments after the program name.
                        let mut words = shell_words::split(&candidate.value)
                            .map_err(|err| NixonError::NothingSelected(err.to_string()))?;
                        if words.first().is_some_and(|word| word == "nixon") {
                            words.remove(0);
                        }
                        Ok(Outcome::Rerun(words))
                    }
                }
            }
        }
    }

    /// Prints the matching invocations, as the other listings do.
    fn list(config: &Config, candidates: Vec<Candidate>, query: Option<&str>) -> Result<ExitCode> {
        let options = PickerOptions {
            initial_query: query.map(ToOwned::to_owned),
            matching: crate::matcher_options(config),
            ..PickerOptions::default()
        };
        let selection = FilterPicker.pick(&options, candidates)?;

        let matched: Vec<String> = selection
            .items()
            .iter()
            .map(|candidate| candidate.value.clone())
            .collect();

        if matched.is_empty() {
            tracing::error!("No history.");
            return Ok(0);
        }
        output::lines(&matched)?;
        Ok(0)
    }
}
