//! The `nixon` binary. Argument parsing, diagnostics and process exit live
//! here; everything else is in the `nixon` library (ENGINEERING §4.3).

use std::process::ExitCode;

fn main() -> ExitCode {
    nixon::run()
}

#[cfg(test)]
mod tests {
    use std::process::ExitCode;

    #[test]
    fn the_binary_exits_with_the_librarys_status() {
        assert_eq!(
            format!("{:?}", nixon::run()),
            format!("{:?}", ExitCode::SUCCESS)
        );
    }
}
