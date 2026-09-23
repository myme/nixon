//! Pure URL and search-query resolution for the browser quick action.

/// Returns the target URL, or `None` when the input is blank.
#[must_use]
pub fn target_url(input: &str, search_url: &str) -> Option<String> {
    if input.trim().is_empty() {
        return None;
    }
    if input.starts_with("http://") || input.starts_with("https://") {
        return Some(input.to_owned());
    }
    if input.contains('.') && !input.chars().any(char::is_whitespace) {
        return Some(format!("https://{input}"));
    }
    Some(search_url.replace("{query}", &encode_query(input)))
}

fn encode_query(input: &str) -> String {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";

    let mut encoded = String::with_capacity(input.len());
    for byte in input.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                encoded.push(char::from(byte));
            }
            _ => {
                encoded.push('%');
                encoded.push(char::from(HEX[usize::from(byte >> 4)]));
                encoded.push(char::from(HEX[usize::from(byte & 0x0f)]));
            }
        }
    }
    encoded
}

#[cfg(test)]
mod tests {
    use super::target_url;

    const SEARCH: &str = "https://search.example/find?q={query}&source=nixon";

    #[test]
    fn full_http_urls_are_unchanged() {
        for url in ["http://example.com/a?x=1", "https://example.com/a#b"] {
            assert_eq!(target_url(url, SEARCH).as_deref(), Some(url));
        }
    }

    #[test]
    fn dotted_text_without_whitespace_gets_https() {
        assert_eq!(
            target_url("example.com/path", SEARCH).as_deref(),
            Some("https://example.com/path")
        );
        assert_eq!(
            target_url("example.com search", SEARCH).as_deref(),
            Some("https://search.example/find?q=example.com%20search&source=nixon")
        );
    }

    #[test]
    fn queries_use_the_configured_template_and_utf8_percent_encoding() {
        assert_eq!(
            target_url("café + tea", SEARCH).as_deref(),
            Some("https://search.example/find?q=caf%C3%A9%20%2B%20tea&source=nixon")
        );
    }

    #[test]
    fn blank_input_has_no_target() {
        assert_eq!(target_url("", SEARCH), None);
        assert_eq!(target_url("  ", SEARCH), None);
    }
}
