//! Drawing the picker. A pure function of [`App`]. ENGINEERING §4.1, §7.2.

use ansi_to_tui::IntoText as _;
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::Paragraph;

use super::App;
use crate::matcher::Match;

/// The prompt before the query.
const PROMPT: &str = "> ";
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
        Paragraph::new(Line::from(format!("{PROMPT}{}", app.query.text()))),
        query_row,
    );
    render_list(app, frame, list_row);

    // The terminal cursor sits in the query line, where typing happens.
    let (_, col) = app.query.cursor();
    let x = query_row
        .x
        .saturating_add(u16::try_from(PROMPT.len() + col).unwrap_or(u16::MAX));
    frame.set_cursor_position((x, query_row.y));
}

/// Draws the candidate rows, ANSI preserved and matches highlighted.
fn render_list(app: &App, frame: &mut Frame<'_>, area: Rect) {
    let lines: Vec<Line<'_>> = app
        .visible()
        .enumerate()
        .map(|(row, (matched, candidate))| {
            let is_cursor = app.offset + row == app.cursor;
            let mut spans = vec![Span::raw(if is_cursor { CURSOR } else { NO_CURSOR })];
            if app.multi() {
                spans.push(Span::raw(if app.marked.contains(&matched.index) {
                    MARK
                } else {
                    " "
                }));
                spans.push(Span::raw(" "));
            }
            spans.extend(display_spans(&candidate.display, matched));

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

/// The style matched characters are drawn in, as fzf does. ENGINEERING §7.2.
fn highlight() -> Style {
    Style::default()
        .fg(Color::Cyan)
        .add_modifier(Modifier::BOLD)
}

/// Renders a candidate, keeping its ANSI colours and marking what matched.
///
/// The ANSI text is exploded to one styled character per position so the
/// highlight can be layered on without discarding the original style, then
/// regrouped into as few spans as possible.
fn display_spans(display: &str, matched: &Match) -> Vec<Span<'static>> {
    let styled = styled_chars(display);
    let mut out: Vec<Span<'static>> = Vec::new();

    for (position, (c, base)) in styled.into_iter().enumerate() {
        let style = if matched
            .indices
            .binary_search(&u32::try_from(position).unwrap_or(u32::MAX))
            .is_ok()
        {
            base.patch(highlight())
        } else {
            base
        };

        match out.last_mut() {
            Some(last) if last.style == style => last.content.to_mut().push(c),
            _ => out.push(Span::styled(c.to_string(), style)),
        }
    }
    out
}

/// One `(character, style)` per position, with ANSI escapes applied.
fn styled_chars(display: &str) -> Vec<(char, Style)> {
    let Ok(text) = display.into_text() else {
        return display.chars().map(|c| (c, Style::default())).collect();
    };
    let Some(line) = text.lines.into_iter().next() else {
        return Vec::new();
    };
    line.spans
        .into_iter()
        .flat_map(|span| {
            let style = span.style;
            span.content.chars().map(|c| (c, style)).collect::<Vec<_>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use ratatui::style::Color;

    use super::render;
    use crate::candidate::Candidate;
    use crate::options::PickerOptions;
    use crate::ui::App;

    fn draw_with_cursor(app: &mut App) -> (String, Option<(u16, u16)>) {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        app.set_height(17);
        terminal.draw(|frame| render(app, frame)).unwrap();
        let cursor = terminal.get_cursor_position().ok().map(|p| (p.x, p.y));
        (format!("{}", terminal.backend()), cursor)
    }

    fn draw(app: &mut App) -> String {
        draw_with_cursor(app).0
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

    fn type_query(app: &mut App, text: &str) {
        for c in text.chars() {
            press(app, KeyCode::Char(c));
        }
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
        type_query(&mut app, "co");
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
        type_query(&mut app, "zzz");
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

    /// `(character, foreground)` for one row of the rendered buffer.
    ///
    /// `TestBackend`'s `Display` shows only text, so styles have to be read off
    /// the cells to prove anything about highlighting.
    fn row_styles(app: &mut App, row: u16) -> Vec<(char, Option<Color>)> {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        app.set_height(17);
        terminal.draw(|frame| render(app, frame)).unwrap();

        let buffer = terminal.backend().buffer().clone();
        (0..80)
            .map(|x| {
                let cell = &buffer[(x, row)];
                let c = cell.symbol().chars().next().unwrap_or(' ');
                (c, cell.style().fg)
            })
            .collect()
    }

    #[test]
    fn matched_characters_are_highlighted() {
        let mut app = App::new(commands(), PickerOptions::default());
        type_query(&mut app, "bld");
        insta::assert_snapshot!(draw(&mut app));

        // Row 1 is the only match, `build - Build the workspace`.
        let styles = row_styles(&mut app, 1);
        let highlighted: String = styles
            .iter()
            .filter(|(_, fg)| *fg == Some(Color::Cyan))
            .map(|(c, _)| *c)
            .collect();
        assert_eq!(highlighted, "bld");
    }

    #[test]
    fn only_the_matched_characters_are_highlighted() {
        let mut app = App::new(commands(), PickerOptions::default());
        type_query(&mut app, "bld");
        let styles = row_styles(&mut app, 1);

        let rendered: String = styles.iter().map(|(c, _)| *c).collect();
        assert!(
            rendered
                .trim_end()
                .starts_with("> build - Build the workspace")
        );

        // Exactly three characters carry the highlight, one per query char.
        let highlighted = styles
            .iter()
            .filter(|(_, fg)| *fg == Some(Color::Cyan))
            .count();
        assert_eq!(highlighted, 3);
    }

    #[test]
    fn highlighting_keeps_the_candidates_own_colours() {
        let coloured = vec![Candidate::with_title(
            "\u{1b}[32mgreen\u{1b}[0m branch",
            "green branch",
        )];
        let mut app = App::new(coloured, PickerOptions::default());
        type_query(&mut app, "gb");
        insta::assert_snapshot!(draw(&mut app));

        let styles = row_styles(&mut app, 1);
        let by_colour = |want: Color| -> String {
            styles
                .iter()
                .filter(|(_, fg)| *fg == Some(want))
                .map(|(c, _)| *c)
                .collect()
        };
        // The matched characters are highlighted...
        assert_eq!(by_colour(Color::Cyan), "gb");
        // ...and the rest of the ANSI-green word keeps its own colour.
        assert_eq!(by_colour(Color::Green), "reen");
    }

    #[test]
    fn the_cursor_sits_after_the_prompt_and_the_query() {
        let mut app = App::new(commands(), PickerOptions::default());
        let (_, cursor) = draw_with_cursor(&mut app);
        assert_eq!(cursor, Some((2, 0)));

        type_query(&mut app, "cov");
        let (_, cursor) = draw_with_cursor(&mut app);
        assert_eq!(cursor, Some((5, 0)));
    }

    #[test]
    fn the_cursor_follows_line_editing() {
        let mut app = App::new(commands(), PickerOptions::default());
        type_query(&mut app, "cov");
        app.handle(KeyEvent::new(KeyCode::Char('a'), KeyModifiers::CONTROL));
        let (_, cursor) = draw_with_cursor(&mut app);
        assert_eq!(cursor, Some((2, 0)));
    }

    #[test]
    fn the_cursor_allows_for_the_header_row() {
        let mut app = App::new(
            commands(),
            PickerOptions::default().header("Select command"),
        );
        let (_, cursor) = draw_with_cursor(&mut app);
        assert_eq!(cursor, Some((2, 1)));
    }
}
