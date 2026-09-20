//! Owning the terminal. The only module that touches crossterm directly.
//! ENGINEERING §4.1, §2.1.

use std::io::{self, Stderr};

use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;

/// A terminal in raw mode on the alternate screen, restored on drop.
///
/// Drawing goes to **stderr**: stdout carries data for `--select`, `--list`
/// and `--insert`, which the shell widgets read. ENGINEERING §2.1.
pub struct TerminalGuard {
    terminal: Terminal<CrosstermBackend<Stderr>>,
}

impl TerminalGuard {
    /// Takes the terminal, installing a panic hook that gives it back.
    pub fn new() -> io::Result<Self> {
        install_panic_hook();
        enable_raw_mode()?;
        let mut stderr = io::stderr();
        execute!(stderr, EnterAlternateScreen)?;
        Ok(Self {
            terminal: Terminal::new(CrosstermBackend::new(stderr))?,
        })
    }

    /// The terminal to draw on.
    pub const fn terminal(&mut self) -> &mut Terminal<CrosstermBackend<Stderr>> {
        &mut self.terminal
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        restore();
    }
}

/// Puts the terminal back. Safe to call more than once.
pub fn restore() {
    let _ = disable_raw_mode();
    let _ = execute!(io::stderr(), LeaveAlternateScreen);
}

/// Restores the terminal before a panic prints, so the message is readable.
fn install_panic_hook() {
    use std::sync::Once;

    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            restore();
            previous(info);
        }));
    });
}
