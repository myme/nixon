//! The confirm prompt: a row of toggles, `Enter` and `Esc`.
//!
//! What a command with options but no placeholders asks. It is a
//! convenience, not a gate: without a terminal the defaults stand.

use std::io;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;

use crate::options::{PickerOption, PickerOptions};
use crate::terminal::{TerminalGuard, no_terminal};
use crate::ui::keymap::{OptionKey, option_action};

/// What the prompt is doing.
pub struct Confirm {
    /// The toggles, as they currently stand.
    pub options: Vec<PickerOption>,
    /// Which toggle the row is on.
    pub focus: usize,
    /// `Some(true)` once confirmed, `Some(false)` once cancelled.
    pub done: Option<bool>,
}

impl Confirm {
    /// Starts on the first toggle.
    pub const fn new(options: Vec<PickerOption>) -> Self {
        Self {
            options,
            focus: 0,
            done: None,
        }
    }

    /// Each toggle's state.
    pub fn state(&self) -> Vec<bool> {
        self.options.iter().map(|option| option.on).collect()
    }

    /// Applies one key press.
    ///
    /// Unlike the picker's options row there is no query to go back to, so
    /// `Enter` confirms and only `Space` toggles.
    pub fn handle(&mut self, key: KeyEvent) {
        if let Some(OptionKey::Toggle(index)) = option_action(key) {
            self.toggle(index);
            return;
        }

        let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
        let last = self.options.len().saturating_sub(1);
        match (key.code, ctrl) {
            (KeyCode::Enter, _) => self.done = Some(true),
            (KeyCode::Esc, _) | (KeyCode::Char('c' | 'g'), true) => self.done = Some(false),
            (KeyCode::Char(' '), false) => self.toggle(self.focus),
            (KeyCode::Left | KeyCode::BackTab, _) => self.focus = self.focus.saturating_sub(1),
            (KeyCode::Right | KeyCode::Tab, _) => self.focus = (self.focus + 1).min(last),
            _ => {}
        }
    }

    fn toggle(&mut self, index: usize) {
        if let Some(option) = self.options.get_mut(index) {
            option.on = !option.on;
        }
    }
}

/// Draws the prompt and reads keys until it is answered.
pub fn run(options: &PickerOptions) -> io::Result<Option<Vec<bool>>> {
    let mut guard = match TerminalGuard::new() {
        Ok(guard) => guard,
        // No terminal to ask with: the defaults are already valid.
        Err(err) if err.kind() == no_terminal().kind() => {
            return Ok(Some(options.options.iter().map(|o| o.on).collect()));
        }
        Err(err) => return Err(err),
    };

    let mut confirm = Confirm::new(options.options.clone());
    let header = options.header.clone().unwrap_or_default();

    while confirm.done.is_none() {
        guard
            .terminal()
            .draw(|frame| render(&confirm, &header, frame))?;
        if let Event::Key(key) = event::read()?
            && key.kind == KeyEventKind::Press
        {
            confirm.handle(key);
        }
    }

    Ok(match confirm.done {
        Some(true) => Some(confirm.state()),
        _ => None,
    })
}

/// Header, toggles, and the one line of help.
fn render(confirm: &Confirm, header: &str, frame: &mut Frame<'_>) {
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Min(0),
        ])
        .split(frame.area());

    frame.render_widget(
        Paragraph::new(Line::from(Span::styled(
            header,
            Style::default().add_modifier(Modifier::BOLD),
        ))),
        rows[0],
    );
    render_options(confirm, frame, rows[1]);
    frame.render_widget(
        Paragraph::new(Line::from(Span::styled(
            "space toggles · enter runs · esc cancels",
            Style::default().add_modifier(Modifier::DIM),
        ))),
        rows[2],
    );
}

fn render_options(confirm: &Confirm, frame: &mut Frame<'_>, area: Rect) {
    let mut spans = Vec::new();
    for (index, option) in confirm.options.iter().enumerate() {
        if index > 0 {
            spans.push(Span::raw("  "));
        }
        let mark = if option.on { "x" } else { " " };
        let style = if index == confirm.focus {
            Style::default().add_modifier(Modifier::BOLD | Modifier::REVERSED)
        } else {
            Style::default()
        };
        spans.push(Span::styled(format!("[{mark}] {}", option.label), style));
    }

    if let Some(description) = confirm
        .options
        .get(confirm.focus)
        .and_then(|option| option.description.as_deref())
    {
        spans.push(Span::styled(
            format!("  {description}"),
            Style::default().add_modifier(Modifier::DIM),
        ));
    }

    frame.render_widget(Paragraph::new(Line::from(spans)), area);
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    use super::Confirm;
    use crate::options::PickerOption;

    fn confirm() -> Confirm {
        Confirm::new(vec![
            PickerOption::new("--force", false),
            PickerOption::new("-v", true),
        ])
    }

    fn press(confirm: &mut Confirm, code: KeyCode) {
        confirm.handle(KeyEvent::new(code, KeyModifiers::NONE));
    }

    #[test]
    fn space_toggles_and_enter_confirms() {
        let mut confirm = confirm();
        press(&mut confirm, KeyCode::Char(' '));
        assert_eq!(confirm.state(), [true, true]);

        press(&mut confirm, KeyCode::Enter);
        assert_eq!(confirm.done, Some(true));
    }

    #[test]
    fn focus_moves_and_stops_at_the_ends() {
        let mut confirm = confirm();
        press(&mut confirm, KeyCode::Left);
        assert_eq!(confirm.focus, 0);

        press(&mut confirm, KeyCode::Tab);
        press(&mut confirm, KeyCode::Right);
        assert_eq!(confirm.focus, 1);

        press(&mut confirm, KeyCode::Char(' '));
        assert_eq!(confirm.state(), [false, false]);
    }

    #[test]
    fn alt_digits_toggle_without_moving_focus() {
        let mut confirm = confirm();
        confirm.handle(KeyEvent::new(KeyCode::Char('2'), KeyModifiers::ALT));
        assert_eq!(confirm.state(), [false, false]);
        assert_eq!(confirm.focus, 0);
    }

    #[test]
    fn esc_cancels() {
        let mut prompt = confirm();
        press(&mut prompt, KeyCode::Esc);
        assert_eq!(prompt.done, Some(false));
    }

    #[test]
    fn ctrl_c_cancels() {
        let mut prompt = confirm();
        prompt.handle(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::CONTROL));
        assert_eq!(prompt.done, Some(false));
    }
}
