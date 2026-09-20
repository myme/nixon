//! Drawing the picker. A pure function of [`App`]. ENGINEERING §4.1, §7.2.

use ansi_to_tui::IntoText as _;
use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span, Text};
use ratatui::widgets::Paragraph;

use super::App;

/// The prompt before the query.
const PROMPT: &str = "> ";
/// fzf's pointer glyph, in the margin of the current row.
const POINTER: &str = "\u{258c}";
/// fzf's marker glyph, for rows marked in multi-select.
const MARKER: &str = "\u{258c}";
/// Keeps unmarked rows aligned with marked ones.
const BLANK: &str = " ";

/// The current row's background: fzf's `bg+`, a gentle dark grey rather than
/// reverse video, so it reads on a dark terminal without filling the row.
const CURRENT_BG: Color = Color::Indexed(237);
/// The pointer's colour, fzf's `pointer`.
const POINTER_FG: Color = Color::Indexed(168);
/// The marker's colour, fzf's `marker`, distinct from the pointer.
const MARKER_FG: Color = Color::Indexed(114);

/// Draws the picker. Pure in `app`, so it snapshot-tests under `TestBackend`.
pub fn render(app: &mut App, frame: &mut Frame<'_>) {
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

    render_query(app, frame, query_row);
    render_list(app, frame, list_row);

    // The terminal cursor sits in the query line, where typing happens.
    let (_, col) = app.query.cursor();
    let x = query_row
        .x
        .saturating_add(u16::try_from(PROMPT.len() + col).unwrap_or(u16::MAX));
    frame.set_cursor_position((x, query_row.y));
}

/// Draws the query line and the match counts. ENGINEERING §7.2.
///
/// The counts read `matched/total`, with the number of marked rows in
/// parentheses when the picker is in multi mode, as fzf shows them.
fn render_query(app: &App, frame: &mut Frame<'_>, area: Rect) {
    // Straight from the matcher's snapshot, so it is correct while the
    // candidates are still streaming in. The marked count is the total, not
    // the visible total: marks outlive the query that made them.
    let counts = if app.multi() && !app.marked.is_empty() {
        format!(
            "{}/{} ({})",
            app.matched_count(),
            app.total_count(),
            app.marked.len()
        )
    } else {
        format!("{}/{}", app.matched_count(), app.total_count())
    };

    frame.render_widget(
        Paragraph::new(Line::from(format!("{PROMPT}{}", app.query.text()))),
        area,
    );

    let width = u16::try_from(counts.chars().count()).unwrap_or(u16::MAX);
    if area.width > width {
        let at = Rect {
            x: area.x + area.width - width,
            width,
            ..area
        };
        frame.render_widget(
            Paragraph::new(Line::from(Span::styled(
                counts,
                Style::default().add_modifier(Modifier::DIM),
            ))),
            at,
        );
    }
}

/// Draws the candidate rows, ANSI preserved and matches highlighted.
fn render_list(app: &mut App, frame: &mut Frame<'_>, area: Rect) {
    let multi = app.multi();
    let rows = app.rows();
    let lines: Vec<Line<'_>> = rows
        .iter()
        .map(|row| {
            // Margin: the pointer for the current row, then the marker
            // column when marking is possible. Blank margins keep the text
            // aligned so it never shifts as the cursor moves.
            let mut spans = vec![if row.is_cursor {
                Span::styled(POINTER, Style::default().fg(POINTER_FG))
            } else {
                Span::raw(BLANK)
            }];
            if multi {
                spans.push(if row.marked {
                    Span::styled(MARKER, Style::default().fg(MARKER_FG))
                } else {
                    Span::raw(BLANK)
                });
            }
            spans.push(Span::raw(" "));
            spans.extend(display_spans(&row.candidate.display, &row.indices));

            Line::from(spans)
        })
        .collect();

    frame.render_widget(Paragraph::new(Text::from(lines)), area);

    // The current row's background goes on last, as one pass over its cells.
    // A per-line style loses wherever a span carries its own background — an
    // ANSI-coloured segment, a highlighted match, the dimmed description —
    // and stops where the text does instead of covering the row.
    let at = app.cursor.saturating_sub(app.offset);
    if let Ok(offset) = u16::try_from(at)
        && offset < area.height
        && rows.iter().any(|row| row.is_cursor)
    {
        let line = Rect {
            y: area.y + offset,
            height: 1,
            ..area
        };
        frame
            .buffer_mut()
            .set_style(line, Style::default().bg(CURRENT_BG));
    }
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
fn display_spans(display: &str, indices: &[u32]) -> Vec<Span<'static>> {
    let styled = styled_chars(display);
    let mut out: Vec<Span<'static>> = Vec::new();

    for (position, (c, base)) in styled.into_iter().enumerate() {
        let style = if indices
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
///
/// Backgrounds are dropped: a candidate colours its text, and keeping a
/// `bg: Reset` from the parser would punch holes in the current row's
/// highlight.
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
            // Drop any background: a candidate colours its text, and a
            // `bg: Reset` from the parser would punch holes in the current
            // row's highlight.
            let style = Style {
                bg: None,
                ..span.style
            };
            span.content.chars().map(|c| (c, style)).collect::<Vec<_>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use ratatui::Terminal;
    use ratatui::backend::TestBackend;
    use ratatui::style::{Color, Modifier};

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
        terminal.draw(|frame| render(&mut app, frame)).unwrap();
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
            rendered.contains("build - Build the workspace"),
            "rendered was: {rendered}"
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

    /// `(character, modifiers)` for one rendered row.
    fn row_modifiers(app: &mut App, row: u16) -> Vec<(char, Modifier)> {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        app.set_height(17);
        terminal.draw(|frame| render(app, frame)).unwrap();

        let buffer = terminal.backend().buffer().clone();
        (0..80)
            .map(|x| {
                let cell = &buffer[(x, row)];
                (
                    cell.symbol().chars().next().unwrap_or(' '),
                    cell.style().add_modifier,
                )
            })
            .collect()
    }

    /// `(character, background)` for one rendered row.
    fn row_backgrounds(app: &mut App, row: u16) -> Vec<(char, Color)> {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        app.set_height(17);
        terminal.draw(|frame| render(app, frame)).unwrap();

        let buffer = terminal.backend().buffer().clone();
        (0..80)
            .map(|x| {
                let cell = &buffer[(x, row)];
                (
                    cell.symbol().chars().next().unwrap_or(' '),
                    cell.style().bg.unwrap_or(Color::Reset),
                )
            })
            .collect()
    }

    #[test]
    fn the_current_row_gets_a_subtle_background_not_reverse_video() {
        let mut app = App::new(commands(), PickerOptions::default());

        let current = row_backgrounds(&mut app, 1);
        assert!(
            current.iter().all(|(_, bg)| *bg == Color::Indexed(237)),
            "the whole current row should carry fzf's bg+"
        );

        let modifiers = row_modifiers(&mut app, 1);
        assert!(
            !modifiers
                .iter()
                .any(|(_, m)| m.contains(Modifier::REVERSED)),
            "the current row must not use reverse video"
        );
    }

    /// The background must survive every kind of styled span on the row.
    #[test]
    fn the_current_row_background_covers_ansi_highlight_and_dimmed_spans() {
        // A green segment, a dimmed suffix, and a query that highlights.
        let candidates = vec![Candidate::with_title(
            "[32mgreen[0m build[2m - Build the workspace[0m",
            "build",
        )];
        let mut app = App::new(candidates, PickerOptions::default());
        type_query(&mut app, "bld");

        let row = row_backgrounds(&mut app, 1);
        let gaps: Vec<char> = row
            .iter()
            .filter(|(_, bg)| *bg != Color::Indexed(237))
            .map(|(c, _)| *c)
            .collect();
        assert!(
            gaps.is_empty(),
            "these cells lost the row background: {gaps:?}"
        );

        // The styling underneath is still there.
        let styles = row_styles(&mut app, 1);
        assert!(styles.iter().any(|(_, fg)| *fg == Some(Color::Cyan)));
        assert!(styles.iter().any(|(_, fg)| *fg == Some(Color::Green)));
    }

    #[test]
    fn an_unselected_row_has_no_background() {
        let mut app = App::new(commands(), PickerOptions::default());
        let row = row_backgrounds(&mut app, 2);
        assert!(row.iter().all(|(_, bg)| *bg == Color::Reset));
    }

    #[test]
    fn the_current_row_carries_the_pointer_glyph_in_its_margin() {
        let mut app = App::new(commands(), PickerOptions::default());

        let current: Vec<(char, Option<Color>)> = row_styles(&mut app, 1);
        assert_eq!(current[0].0, '\u{258c}');
        assert_eq!(current[0].1, Some(Color::Indexed(168)));

        // Other rows keep a blank margin so the text never shifts.
        let other = row_styles(&mut app, 2);
        assert_eq!(other[0].0, ' ');
    }

    #[test]
    fn a_marked_row_carries_the_marker_glyph_in_a_second_colour() {
        let mut app = App::new(commands(), PickerOptions::default().multi(true));
        press(&mut app, KeyCode::Tab);

        // Row 1 is now marked, and the cursor has moved on to row 2.
        let marked = row_styles(&mut app, 1);
        assert_eq!(marked[1].0, '\u{258c}');
        assert_eq!(marked[1].1, Some(Color::Indexed(114)));

        let unmarked = row_styles(&mut app, 3);
        assert_eq!(unmarked[1].0, ' ');
    }

    #[test]
    fn a_description_renders_dimmed_while_the_name_does_not() {
        // The nixon side builds these; here the ANSI stands in for it.
        let candidates = vec![Candidate::with_title(
            "build\u{1b}[2m - Build the workspace\u{1b}[0m",
            "build",
        )];
        let mut app = App::new(candidates, PickerOptions::default());
        let row = row_modifiers(&mut app, 1);

        let dimmed: String = row
            .iter()
            .filter(|(_, m)| m.contains(Modifier::DIM))
            .map(|(c, _)| *c)
            .collect();
        assert_eq!(dimmed.trim_end(), " - Build the workspace");

        let name: String = row[2..7].iter().map(|(c, _)| *c).collect();
        assert_eq!(name, "build");
        assert!(!row[2].1.contains(Modifier::DIM));
    }

    #[test]
    fn the_counts_show_matches_over_total() {
        let mut app = App::new(commands(), PickerOptions::default());
        assert!(draw(&mut app).contains("9/9"));

        type_query(&mut app, "co");
        let drawn = draw(&mut app);
        assert!(drawn.contains("2/9"), "drawn was: {drawn}");
    }

    #[test]
    fn the_counts_include_the_marked_total_in_multi_mode() {
        let mut app = App::new(commands(), PickerOptions::default().multi(true));
        press(&mut app, KeyCode::Tab);
        press(&mut app, KeyCode::Tab);
        let drawn = draw(&mut app);
        assert!(drawn.contains("9/9 (2)"), "drawn was: {drawn}");
    }

    #[test]
    fn the_marked_count_is_absent_outside_multi_mode() {
        let mut app = App::new(commands(), PickerOptions::default());
        press(&mut app, KeyCode::Tab);
        let drawn = draw(&mut app);
        assert!(drawn.contains("9/9"), "drawn was: {drawn}");
        assert!(!drawn.contains('('), "drawn was: {drawn}");
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
