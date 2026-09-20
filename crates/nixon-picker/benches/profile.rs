//! A crude profile of the picker's hot paths on a large candidate set.
//!
//! Run with `cargo run --release --bench profile`. Not a test; it exists to
//! show where the time goes before and after changes.

// A profile reports its numbers on stdout; that is the whole point of it.
#![allow(clippy::print_stdout)]

use std::time::Instant;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use nixon_picker::matcher::{MatchOptions, matches};
use nixon_picker::ui::{App, render};
use nixon_picker::{Candidate, PickerOptions};
use ratatui::Terminal;
use ratatui::backend::TestBackend;

fn candidates(n: usize) -> Vec<Candidate> {
    (0..n)
        .map(|i| {
            Candidate::identity(format!(
                "crates/nixon/src/module{}/submodule{}/file_{i}.rs",
                i % 97,
                i % 13
            ))
        })
        .collect()
}

fn time<T>(label: &str, f: impl FnOnce() -> T) -> T {
    let start = Instant::now();
    let out = f();
    println!("{label:<44} {:>10.2?}", start.elapsed());
    out
}

fn main() {
    const N: usize = 200_000;

    let items = time("build 200k candidates", || candidates(N));

    time("matches() empty query", || {
        matches("", &items, MatchOptions::default()).len()
    });
    time("matches() 'file'", || {
        matches("file", &items, MatchOptions::default()).len()
    });
    time("matches() 'nixmod12'", || {
        matches("nixmod12", &items, MatchOptions::default()).len()
    });

    let mut app = time("App::new (200k)", || {
        App::new(items.clone(), PickerOptions::default())
    });
    app.set_height(40);

    let backend = TestBackend::new(120, 42);
    let mut terminal = Terminal::new(backend).expect("terminal");

    time("first render", || {
        terminal
            .draw(|frame| render(&mut app, frame))
            .expect("draw");
    });

    let mut worst = std::time::Duration::ZERO;
    time("8 keystrokes, each re-matching + render", || {
        for c in "file_123".chars() {
            let start = Instant::now();
            app.handle(KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE));
            terminal
                .draw(|frame| render(&mut app, frame))
                .expect("draw");
            worst = worst.max(start.elapsed());
        }
    });
    println!("{:<44} {worst:>10.2?}", "worst single keystroke");

    time("scroll 40 rows", || {
        for _ in 0..40 {
            app.handle(KeyEvent::new(KeyCode::Down, KeyModifiers::NONE));
            terminal
                .draw(|frame| render(&mut app, frame))
                .expect("draw");
        }
    });

    println!("matched after typing: {}", app.matched_count());
}
