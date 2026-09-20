//! The one place YAML is parsed, so the crate behind it can be swapped in a
//! single file. ENGINEERING §7.1 decision 3.

use serde::de::DeserializeOwned;

/// Parses YAML, reporting the underlying parser's message. SPEC §3.4.
pub fn from_str<T: DeserializeOwned>(input: &str) -> Result<T, String> {
    serde_saphyr::from_str(input).map_err(|err| err.to_string())
}
