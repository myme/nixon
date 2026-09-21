//! Generic fuzzy picker: terminal UI and matching.
//!
//! Knows nothing about commands, projects or markdown; nixon bridges its own
//! types to [`Candidate`] in `nixon::select`.

// Tests assert on known-good values.
#![cfg_attr(test, allow(clippy::unwrap_used, clippy::expect_used, clippy::panic))]

pub mod candidate;
pub mod confirm;
pub mod editor;
pub mod filter;
pub mod matcher;
pub mod options;
pub mod picker;
pub mod selection;
pub mod stream;
pub mod terminal;
pub mod textbuf;
pub mod ui;

pub use candidate::Candidate;
pub use filter::filter;
pub use options::{PickerOption, PickerOptions};
pub use picker::{FilterPicker, Picker, TuiPicker, exact_selection, unique_selection};
pub use selection::{Selection, SelectionType};
pub use stream::CandidateStream;
