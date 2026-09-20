//! Drawing the picker. A pure function of [`App`]. ENGINEERING §4.1.

use ansi_to_tui::IntoText as _;
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::Paragraph;

use super::App;

/// Marks the row under the cursor.
const CURSOR: &str = "> ";
/// Indents rows that are not under the cursor.
const NO_CURSOR: &str = "  ";
/// Marks a row the user has marked for multi-select.
const MARK: &str = "*";

/// Draws the picker. Pure in `app`, so it snapshot-tests under `TestBackend`.
pub fn render(app: &App, frame: &mut Frame<'_>) {
    let area = frame.area();
    let has_header = app.header().is_some();
    let constraints = if has_header {
        vec![
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Min(0),
        ]
    } else {
        vec![Constraint::Length(1), Constraint::Min(0)]
    };
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints(constraints)
        .split(area);

    let (query_row, list_row) = if has_header {
        frame.render_widget(
            Paragraph::new(Line::from(Span::styled(
                app.header().unwrap_or_default(),
                Style::default().add_modifier(Modifier::BOLD),
            ))),
            rows[0],
        );
        (rows[1], rows[2])
    } else {
        (rows[0], rows[1])
    };

    frame.render_widget(
        Paragraph::new(Line::from(format!("> {}", app.query))),
        query_row,
    );
    render_list(app, frame, list_row);
}

/// Draws the candidate rows, ANSI in their display text preserved.
fn render_list(app: &App, frame: &mut Frame<'_>, area: Rect) {
    let lines: Vec<Line<'_>> = app
        .visible()
        .enumerate()
        .map(|(row, (index, candidate))| {
            let is_cursor = app.offset + row == app.cursor;
            let mut spans = vec![Span::raw(if is_cursor { CURSOR } else { NO_CURSOR })];
            if app.multi() {
                spans.push(Span::raw(if app.marked.contains(&index) {
                    MARK
                } else {
                    " "
                }));
                spans.push(Span::raw(" "));
            }
            spans.extend(display_spans(&candidate.display));

            let style = if is_cursor {
                Style::default().add_modifier(Modifier::REVERSED)
            } else {
                Style::default()
            };
            Line::from(spans).style(style)
        })
        .collect();

    frame.render_widget(Paragraph::new(Text::from(lines)), area);
}

/// Parses ANSI escapes so coloured output renders. SPEC §8.4 `--ansi`.
fn display_spans(display: &str) -> Vec<Span<'static>> {
    display.into_text().map_or_else(
        |_| vec![Span::raw(display.to_owned())],
        |text| {
            text.lines
                .into_iter()
                .next()
                .map_or_else(Vec::new, |line| line.spans)
        },
    )
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;

    use super::render;
    use crate::candidate::Candidate;
    use crate::options::PickerOptions;
    use crate::ui::App;

    fn draw(app: &mut App) -> String {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        app.set_height(17);
        terminal.draw(|frame| render(app, frame)).unwrap();
        format!("{}", terminal.backend())
    }

    fn candidates(items: &[&str]) -> Vec<Candidate> {
        items.iter().map(|s| Candidate::identity(*s)).collect()
    }

    fn commands() -> Vec<Candidate> {
        candidates(&[
            "build - Build the workspace",
            "check - Everything CI runs",
            "coverage - Build the HTML coverage report",
            "docs - Build and open the documentation",
            "fmt - Format Rust, TOML and Nix sources",
            "lint - Clippy with warnings denied",
            "run - Run nixon from the workspace",
            "tdd - Reloading test session",
            "test - Run the test suite",
        ])
    }

    fn press(app: &mut App, code: KeyCode) {
        app.handle(KeyEvent::new(code, KeyModifiers::NONE));
    }

    #[test]
    fn renders_an_empty_picker() {
        let mut app = App::new(Vec::new(), PickerOptions::default());
        insta::assert_snapshot!(draw(&mut app));
    }

    #[test]
    fn renders_the_full_list() {
        let mut app = App::new(commands(), PickerOptions::default());
        insta::assert_snapshot!(draw(&mut app));
    }

    #[test]
    fn renders_a_filtered_list() {
        let mut app = App::new(commands(), PickerOptions::default());
        for c in "co".chars() {
            press(&mut app, KeyCode::Char(c));
        }
        insta::assert_snapshot!(draw(&mut app));
    }

    #[test]
    fn renders_a_header() {
        let mut app = App::new(
            commands(),
            PickerOptions::default().header("Select command [nixon] (/home/me/code)"),
        );
        insta::assert_snapshot!(draw(&mut app));
    }

    #[test]
    fn renders_marked_rows() {
        let mut app = App::new(commands(), PickerOptions::default().multi(true));
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Down);
        press(&mut app, KeyCode::Tab);
        insta::assert_snapshot!(draw(&mut app));
    }

    #[test]
    fn renders_a_scrolled_list() {
        let mut app = App::new(commands(), PickerOptions::default());
        app.set_height(4);
        for _ in 0..6 {
            press(&mut app, KeyCode::Down);
        }
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|frame| render(&app, frame)).unwrap();
        insta::assert_snapshot!(format!("{}", terminal.backend()));
    }

    #[test]
    fn renders_no_matches() {
        let mut app = App::new(commands(), PickerOptions::default());
        for c in "zzz".chars() {
            press(&mut app, KeyCode::Char(c));
        }
        insta::assert_snapshot!(draw(&mut app));
    }

    #[test]
    fn ansi_in_a_candidate_does_not_leak_into_the_frame() {
        let coloured = vec![Candidate::with_title(
            "\u{1b}[32mgreen\u{1b}[0m branch",
            "green branch",
        )];
        let mut app = App::new(coloured, PickerOptions::default());
        insta::assert_snapshot!(draw(&mut app));
    }
}
