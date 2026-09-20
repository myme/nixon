# Nixon v2 — working rules

Rust rewrite of nixon. The Haskell sources are the reference, not the target.

## The two documents

- `SPEC.md` — **what** v2 must do. A checklist of v1 behaviours, each MUST /
  SHOULD / QUIRK.
- `ENGINEERING.md` — **how** we build it: toolchain, crates, layout, tests.
- `ENGINEERING.md §7 Decisions wins wherever the two conflict.` The backend
  concept and the `-b`/`-t`/`-T` flags are gone; SPEC text describing them is
  historical.
- Cite the checklist in doc comments: `/// SPEC §5.3`. That traceability is
  how we know what is still unported.

## Prose

- Comments only where the code cannot say it itself. No commentary that
  restates the line below it.
- Function docstrings: one tight line, what it does, not how.
- No narrative paragraphs — not in code, not in commit messages, not in PR
  bodies. State the thing and stop.

## Commits

- Commit often: every logical or structural cut, every feature added. Many
  small commits beat one big one.
- Subject: imperative, says what changed. Body: hard-wrapped at ~72 columns,
  says why. No changelog-style bullet dumps.
- PR and issue bodies are not hard-wrapped: one line per paragraph and per
  bullet.

## Tooling

Everything runs through nix — `nix develop` for the shell, `nix flake check`
for the gates. Nothing is `cargo install`ed; nothing runs in CI that
`nix flake check` does not (ENGINEERING §1.1, §3).

- `cargo fmt` and `taplo fmt` clean.
- `cargo clippy --all-targets -- -D warnings`; lint levels live in
  `[workspace.lints]`, not on the command line.
- `cargo nextest run` for tests.
- Tests at every layer: unit and property next to the code, `insta` snapshots
  for picker frames and errors, component tests driving `App` with fakes,
  functional tests against the real binary (ENGINEERING §5). Port the Haskell
  suite verbatim first — it is the de-facto grammar spec.

## Hard rules

- **Stdout is data.** `--list`, `--select`, `--insert`, `inspect`, `gc` and
  `Show` write there and nothing else does; `output.rs` is the only module
  that prints. Everything for humans goes to stderr, including the TUI.
- No `unwrap`, `expect` or `panic!` on user input. Typed errors, miette at the
  binary edge. Cancelling a selection is exit 130, not an error.
