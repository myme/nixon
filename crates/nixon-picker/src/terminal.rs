//! Owning the terminal. The only module that touches crossterm directly.
//! ENGINEERING §4.1, §2.1.

use std::io::{self, Stderr};

use crossterm::event::{
    KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
    supports_keyboard_enhancement,
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;

/// A terminal in raw mode on the alternate screen, restored on drop.
///
/// Drawing goes to **stderr**: stdout carries data for `--select`, `--list`
/// and `--insert`, which the shell widgets read. ENGINEERING §2.1.
pub struct TerminalGuard {
    terminal: Terminal<CrosstermBackend<Stderr>>,
    /// Puts the terminal back, however construction or use ends.
    _restore: Restore,
}

/// The error a picker reports when there is no terminal to draw on.
///
/// Raised where it is first noticed: with no controlling terminal, raw mode
/// fails with a bare `ENXIO` that says nothing to a user.
pub fn no_terminal() -> io::Error {
    io::Error::new(io::ErrorKind::NotConnected, "no terminal")
}

/// Owns the restore, from the moment raw mode is on.
struct Restore;

impl Drop for Restore {
    fn drop(&mut self) {
        restore();
    }
}

impl TerminalGuard {
    /// Takes the terminal, installing a panic hook that gives it back.
    ///
    /// Where the terminal supports the kitty keyboard protocol, it is asked
    /// for disambiguated escape codes so `Ctrl-H`, `Alt-Enter` and
    /// `Alt-Backspace` arrive as distinct events rather than as whatever
    /// legacy byte they collide with. Terminals without it fall back to the
    /// legacy encoding, which the keymap also accepts. ENGINEERING §7.2.
    pub fn new() -> io::Result<Self> {
        install_panic_hook();
        enable_raw_mode().map_err(|_| no_terminal())?;
        // From here on every path, including an early return, restores.
        let _restore = Restore;

        let mut stderr = io::stderr();
        execute!(stderr, EnterAlternateScreen)?;
        if supports_keyboard_enhancement().unwrap_or(false) {
            let _ = execute!(
                stderr,
                PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES)
            );
        }
        Ok(Self {
            terminal: Terminal::new(CrosstermBackend::new(stderr))?,
            _restore,
        })
    }

    /// The terminal to draw on.
    pub const fn terminal(&mut self) -> &mut Terminal<CrosstermBackend<Stderr>> {
        &mut self.terminal
    }
}

/// Puts the terminal back. Safe to call more than once.
pub fn restore() {
    // Popping the flags when none were pushed is harmless.
    let _ = execute!(io::stderr(), PopKeyboardEnhancementFlags);
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
