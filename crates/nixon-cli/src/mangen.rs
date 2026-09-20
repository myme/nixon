//! `nixon internal mangen`: the `nixon(1)` man page, from the clap command
//! tree.
//!
//! Written at package time rather than committed. The other man pages are
//! rendered from `docs/` by pandoc, which needs no code.

use std::io::Write as _;

use clap::CommandFactory as _;
use clap::builder::Resettable;
use nixon::error::Result;

use crate::cli::Cli;

/// The `docs/` pages, referenced from the end of the page.
const SEE_ALSO: &str = ".SH \"SEE ALSO\"\n\
    \\fBnixon.md\\fR(5), \\fBnixon\\-picker\\fR(7), \\fBnixon\\-shell\\fR(7)\n";

/// Renders `nixon.1` to stdout.
pub fn write_man_page() -> Result<i32> {
    // The `--help` epilogue points at the other pages; in the man page that
    // is the SEE ALSO section below, not a section of its own.
    let command = Cli::command().after_help(Resettable::Reset);

    let mut page = Vec::new();
    clap_mangen::Man::new(command).render(&mut page)?;
    page.extend_from_slice(SEE_ALSO.as_bytes());

    let mut out = std::io::stdout().lock();
    out.write_all(&page)?;
    out.flush()?;
    Ok(0)
}
