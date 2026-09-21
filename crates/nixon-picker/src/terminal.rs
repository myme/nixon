//! Owning the terminal. The only module that touches crossterm directly.

use std::io::{self, Stderr};

use crossterm::event::{
    KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;

/// A terminal in raw mode on the alternate screen, restored on drop.
///
/// Drawing goes to **stderr**: stdout carries data for `--select`, `--list`
/// and `--insert`, which the shell widgets read.
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
    /// The kitty keyboard protocol is asked for unconditionally, so that
    /// `Ctrl-H`, `Alt-Enter` and `Alt-Backspace` arrive as distinct events
    /// rather than as whatever legacy byte they collide with. A terminal
    /// without it ignores the sequence and sends the legacy encoding, which
    /// the keymap also accepts.
    ///
    /// Asking is better than detecting. crossterm's detection writes its
    /// query to **stdout** — `File::open("/dev/tty")` is read-only, so the
    /// write to it always fails and the fallback always runs — which puts
    /// escape bytes in front of nixon's own output. `cd "$(nixon project
    /// -s)"` got `cd: no such file or directory: ^[[?u^[[c/Users/…`. It then
    /// waits two seconds for a reply that a captured stdout can never carry,
    /// on every single pick.
    pub fn new() -> io::Result<Self> {
        install_panic_hook();
        enable_raw_mode().map_err(|_| no_terminal())?;
        // From here on every path, including an early return, restores.
        let restore = Restore;

        let mut stderr = io::stderr();
        execute!(stderr, EnterAlternateScreen)?;
        let _ = execute!(
            stderr,
            PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES)
        );
        Ok(Self {
            terminal: Terminal::new(CrosstermBackend::new(stderr))?,
            _restore: restore,
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
