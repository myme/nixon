# Nixon commands

Dev commands for nixon itself. Everything assumes `nix develop` (or direnv).

### `run`

Run nixon from the workspace.

```bash
cargo run --bin nixon -- "$@"
```

### `build`

Build the workspace.

```bash
cargo build --all-targets
```

### `test`

Run the test suite.

```bash
cargo nextest run
```

### `tdd`

Reloading test session. Replaces the v1 ghcid loop.

```bash
bacon test
```

### `lint`

Clippy with the workspace lint levels, warnings denied.

```bash
cargo clippy --all-targets --all-features -- -D warnings
```

### `fmt`

Format Rust, TOML and Nix sources.

```bash
cargo fmt
taplo fmt
nixfmt flake.nix nix/*.nix
```

### `check`

Everything CI runs, from the same flake.

```bash
nix flake check -L
```

### `coverage`

Build the HTML coverage report and print its path.

```bash
cargo llvm-cov nextest --html
echo "Report: target/llvm-cov/html/index.html"
```

### `docs`

Build and open the workspace documentation.

```bash
cargo doc --no-deps --open
```
