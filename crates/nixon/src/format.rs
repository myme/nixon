//! Column and field extraction from a command's output. SPEC §6.

/// Splits column-formatted output into rows of cells. SPEC §6.
///
/// Column widths come from the first row, so cells keep embedded spaces and a
/// row wider than the first shifts, exactly as `column`-style output does.
pub fn parse_columns(has_header: bool, rows: &[String]) -> Vec<Vec<String>> {
    let Some(first) = rows.first() else {
        return Vec::new();
    };
    let widths = parse_widths(first);
    let data = if has_header { &rows[1..] } else { rows };
    data.iter().map(|row| split_row(&widths, row)).collect()
}

/// Builds `(title, value)` candidates from column output. SPEC §6.
///
/// The title is the row as the command printed it; the value is the selected
/// columns joined by single spaces. A header row is dropped from both, so the
/// two stay aligned.
pub fn format_columns(has_header: bool, cols: &[usize], rows: &[String]) -> Vec<(String, String)> {
    let titles = if has_header && !rows.is_empty() {
        &rows[1..]
    } else {
        rows
    };
    let values = pick_columns(cols, &parse_columns(has_header, rows));
    titles
        .iter()
        .cloned()
        .zip(values.into_iter().map(|cells| cells.join(" ")))
        .collect()
}

/// Keeps the 1-based `cols` of every row, in the rows' own order. SPEC §6.
pub fn pick_columns(cols: &[usize], rows: &[Vec<String>]) -> Vec<Vec<String>> {
    rows.iter().map(|row| pick_fields(cols, row)).collect()
}

/// Keeps the 1-based `ns` of `items`, in `items`' order, ignoring out-of-range.
pub fn pick_fields(ns: &[usize], items: &[String]) -> Vec<String> {
    items
        .iter()
        .enumerate()
        .filter(|(index, _)| ns.contains(&(index + 1)))
        .map(|(_, item)| item.clone())
        .collect()
}

/// Widths of each column in a row: its word plus the spaces that follow it.
fn parse_widths(row: &str) -> Vec<usize> {
    let mut widths = Vec::new();
    let mut rest: Vec<char> = row.chars().collect();
    while !rest.is_empty() {
        let word = rest.iter().take_while(|c| !c.is_whitespace()).count();
        let space = rest[word..]
            .iter()
            .take_while(|c| c.is_whitespace())
            .count();
        widths.push(word + space);
        rest = rest.split_off(word + space);
    }
    widths
}

/// Splits one row at the cumulative widths; the last column runs to the end.
fn split_row(widths: &[usize], row: &str) -> Vec<String> {
    let mut cells = Vec::new();
    let mut rest: Vec<char> = row.chars().collect();
    for (index, width) in widths.iter().enumerate() {
        if index + 1 == widths.len() {
            break;
        }
        let at = (*width).min(rest.len());
        let tail = rest.split_off(at);
        cells.push(rest.iter().collect::<String>().trim().to_owned());
        rest = tail;
    }
    if !widths.is_empty() {
        cells.push(rest.iter().collect());
    }
    cells
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::{format_columns, parse_columns, pick_fields};

    fn lines(rows: &[&str]) -> Vec<String> {
        rows.iter().map(|row| (*row).to_owned()).collect()
    }

    const NMCLI: [&str; 4] = [
        "NAME               UUID                                  TYPE      DEVICE",
        "My Wifi            845b3837-c78e-44f1-a752-06ecd496599c  wifi      wlp9s0",
        "br-7defdaf327de    1b9a3d7c-d856-498f-ac12-4d79647f116f  bridge    br-7defdaf327de",
        "lo                 ae505c7d-8596-41b2-9329-c3d31f4c60ef  loopback  lo",
    ];

    #[test]
    fn parses_columns_empty_input() {
        assert!(parse_columns(false, &[]).is_empty());
        assert!(parse_columns(true, &[]).is_empty());
    }

    #[test]
    fn parses_columns_titles_only() {
        let input = lines(&[NMCLI[0]]);
        assert_eq!(
            parse_columns(false, &input),
            vec![lines(&["NAME", "UUID", "TYPE", "DEVICE"])]
        );
        assert!(parse_columns(true, &input).is_empty());
    }

    #[test]
    fn parses_columns() {
        assert_eq!(
            parse_columns(true, &lines(&NMCLI)),
            vec![
                lines(&[
                    "My Wifi",
                    "845b3837-c78e-44f1-a752-06ecd496599c",
                    "wifi",
                    "wlp9s0"
                ]),
                lines(&[
                    "br-7defdaf327de",
                    "1b9a3d7c-d856-498f-ac12-4d79647f116f",
                    "bridge",
                    "br-7defdaf327de"
                ]),
                lines(&[
                    "lo",
                    "ae505c7d-8596-41b2-9329-c3d31f4c60ef",
                    "loopback",
                    "lo"
                ]),
            ]
        );
    }

    #[test]
    fn parses_columns_no_headers() {
        let input = lines(&[NMCLI[2], NMCLI[1], NMCLI[3]]);
        assert_eq!(
            parse_columns(false, &input),
            vec![
                lines(&[
                    "br-7defdaf327de",
                    "1b9a3d7c-d856-498f-ac12-4d79647f116f",
                    "bridge",
                    "br-7defdaf327de"
                ]),
                lines(&[
                    "My Wifi",
                    "845b3837-c78e-44f1-a752-06ecd496599c",
                    "wifi",
                    "wlp9s0"
                ]),
                lines(&[
                    "lo",
                    "ae505c7d-8596-41b2-9329-c3d31f4c60ef",
                    "loopback",
                    "lo"
                ]),
            ]
        );
    }

    #[test]
    fn keeps_all_rows_and_aligns_titles_to_values_no_header() {
        let input = lines(&[
            "/home/myme/code/myme/nixon         99dad83 [main]",
            "/home/myme/code/myme/nixon-another 99dad83 [nixon-another]",
            "/home/myme/code/myme/nixon-test    99dad83 [nixon-test]",
        ]);
        assert_eq!(
            format_columns(false, &[1], &input),
            vec![
                (
                    "/home/myme/code/myme/nixon         99dad83 [main]".to_owned(),
                    "/home/myme/code/myme/nixon".to_owned()
                ),
                (
                    "/home/myme/code/myme/nixon-another 99dad83 [nixon-another]".to_owned(),
                    "/home/myme/code/myme/nixon-another".to_owned()
                ),
                (
                    "/home/myme/code/myme/nixon-test    99dad83 [nixon-test]".to_owned(),
                    "/home/myme/code/myme/nixon-test".to_owned()
                ),
            ]
        );
    }

    #[test]
    fn drops_the_header_row_from_both_titles_and_values() {
        let input = lines(&[NMCLI[0], NMCLI[1], NMCLI[3]]);
        assert_eq!(
            format_columns(true, &[1], &input),
            vec![
                (NMCLI[1].to_owned(), "My Wifi".to_owned()),
                (NMCLI[3].to_owned(), "lo".to_owned()),
            ]
        );
    }

    #[test]
    fn pick_fields_keeps_original_order_and_ignores_out_of_range() {
        let items = lines(&["one", "two", "three"]);
        assert_eq!(pick_fields(&[3, 1], &items), lines(&["one", "three"]));
        assert_eq!(pick_fields(&[9], &items), Vec::<String>::new());
        assert_eq!(pick_fields(&[], &items), Vec::<String>::new());
    }

    proptest! {
        #[test]
        fn format_columns_never_drops_a_row_without_a_header(
            rows in prop::collection::vec("[a-z]{1,6}( +[a-z]{1,6}){0,3}", 0..8),
            cols in prop::collection::vec(1usize..5, 0..4),
        ) {
            prop_assert_eq!(format_columns(false, &cols, &rows).len(), rows.len());
        }

        #[test]
        fn parse_columns_returns_one_row_per_data_line(
            rows in prop::collection::vec("[a-z]{1,6}( +[a-z]{1,6}){0,3}", 1..8),
            has_header in any::<bool>(),
        ) {
            let expected = if has_header { rows.len() - 1 } else { rows.len() };
            prop_assert_eq!(parse_columns(has_header, &rows).len(), expected);
        }
    }
}
