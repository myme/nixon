//! Generic fuzzy picker: terminal UI and matching.
//!
//! Knows nothing about commands, projects or markdown; nixon bridges its own
//! types to [`Candidate`] in `nixon::select`. ENGINEERING §4.1.

// Tests assert on known-good values.
#![cfg_attr(test, allow(clippy::unwrap_used, clippy::expect_used, clippy::panic))]

pub mod candidate;
pub mod filter;
pub mod matcher;
pub mod options;
pub mod selection;

pub use candidate::Candidate;
pub use options::PickerOptions;
pub use selection::{Selection, SelectionType};
