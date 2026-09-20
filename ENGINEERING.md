# Nixon v2 — Engineering Guide

Companion to [SPEC.md](./SPEC.md). SPEC.md says *what* v2 must do; this
document says *how we build it*: toolchain, crates, static analysis, code
structure and test strategy. Versions were checked against crates.io on
2026-09-20.

Two v2 decisions that shape everything below:

1. **Stand-alone.** No `fzf`, no `rofi`. Fuzzy matching and the picker UI are
   built in, on `ratatui` + `nucleo`. The GUI (rofi) backend is dropped.
2. **Test-first.** Every layer has a seam for testing without a terminal or
   real subprocesses, and CI runs unit, snapshot, property and functional
   tests.

---

## 1. Toolchain

| Item | Choice | Notes |
|---|---|---|
| Edition | `2024` | |
| Channel | stable, pinned in `rust-toolchain.toml` | `components = ["rustfmt", "clippy", "rust-analyzer", "llvm-tools-preview"]` (llvm-tools for coverage). The flake reads this same file (§1.1) so nix and rustup can't disagree |
| MSRV | `rust-version` in `Cargo.toml` = the pinned toolchain; verify with `cargo-msrv` | |
| Nix | flake with [crane](https://crane.dev) + [rust-overlay](https://github.com/oxalica/rust-overlay) | **All tooling comes from the dev shell**; the package and every CI check are flake outputs (§1.1) |
| Dev loop | [`bacon`](https://dystroy.org/bacon/) | Replaces `ghcid` (`bacon test`, `bacon clippy`) |
| Task runner | `nixon.md` itself | Dogfood: `cargo run`, `bacon`, `cargo nextest run`, etc. defined as nixon commands. A `justfile` is acceptable for CI parity but nixon.md is the source of truth |

### 1.1 Nix

Nix is the source of truth for the toolchain and for CI. Nothing is
`cargo install`ed; nothing in CI runs that `nix flake check` doesn't.

**Inputs**: `nixpkgs` (stable channel, as today), `crane`, `rust-overlay`,
`flake-utils`.

**Toolchain**: `rust-overlay`'s
`rust-bin.fromRustupToolchainFile ./rust-toolchain.toml` builds the exact
channel + components listed there. crane is instantiated with that
toolchain (`crane.lib.overrideToolchain`), so `nix build`, `nix develop`
and a bare `cargo` on a rustup machine all use the same compiler.

**Outputs** (per system via `flake-utils.eachDefaultSystem`):

| Output | Content |
|---|---|
| `packages.default` / `packages.nixon` | `craneLib.buildPackage` of the workspace (`nixon-cli` binary). `cargoArtifacts` (dependency-only build) is built once and shared by every check below, so a code change never rebuilds deps. `postInstall` keeps installing the shell widgets to `share/nixon/` and the completion loaders to `share/{bash-completion,zsh/site-functions,fish/vendor_completions.d}` |
| `checks.build` | the package |
| `checks.clippy` | `craneLib.cargoClippy` with `--all-targets -- -D warnings` |
| `checks.fmt` | `craneLib.cargoFmt` |
| `checks.toml-fmt` | `craneLib.taploFmt` |
| `checks.doc` | `craneLib.cargoDoc` with `RUSTDOCFLAGS=-D warnings` |
| `checks.test` | `craneLib.cargoNextest` (PTY tests included — the sandbox has `/dev/pts`) |
| `checks.deny` | `craneLib.cargoDeny` (advisories, licenses, sources). No separate `cargoAudit` check — same RustSec DB |
| `checks.shear` | `runCommand` wrapping `cargo-shear` (crane has no helper) |
| `checks.typos` | `runCommand` wrapping `typos` |
| `checks.coverage` | `craneLib.cargoLlvmCov` producing an HTML report as a build output (not a gate) |
| `devShells.default` | `craneLib.devShell` with: the toolchain, `bacon`, `cargo-nextest`, `cargo-llvm-cov`, `cargo-deny`, `cargo-shear`, `cargo-msrv`, `cargo-mutants`, `cargo-hack`, `typos`, `taplo`, `insta` (`cargo-insta`), `nixfmt`, `statix`, `deadnix`, plus runtime tools the tests shim or need (`bash`, `python3`, `jq`, `yq`, `direnv`) |
| `overlays.default` | exposes `nixon` (as today) |
| `formatter` | `nixfmt` |

**Rules**:

- `Cargo.lock` is committed; crane vendors from it. Builds run without
  network — no `build.rs` that fetches, no git dependencies without a
  `rev`.
- `nix flake check` **is** CI. `.github/workflows/ci.yml` becomes: checkout →
  `cachix/install-nix-action` → `cachix-action` (`myme`) → `nix flake check
  -L` → `nix build -L`, on `ubuntu-latest` and `macos-latest`. The
  cargo-only matrix job from v1 is dropped; there is one toolchain, not a
  matrix.
- Weekly job: `nix develop -c cargo mutants` (mutation testing is too slow
  and non-deterministic for a `check`).
- `.envrc` stays `use flake`. `nix develop` must give a shell where every
  command in `nixon.md` works.
- Nix files are linted too: `statix check`, `deadnix`, `nixfmt --check` are
  part of `checks`.
- Bumping the Rust toolchain = editing `rust-toolchain.toml` and
  `flake.lock` in one PR; `rust-version` in `Cargo.toml` follows.

---

## 2. Crates

### 2.1 Core runtime

| Concern | Crate | Version (2026-09) | Why |
|---|---|---|---|
| CLI parsing | [`clap`](https://crates.io/crates/clap) (derive) | 4.6.7 | Standard. Tri-state `--x/--no-x` flags (`exact`, `ignore-case`, `direnv`, `nix`): two `SetTrue` args with `overrides_with`, mapped to `Option<bool>` in a `From` impl |
| Shell completion | [`clap_complete`](https://crates.io/crates/clap_complete) with `unstable-dynamic` | 4.6.11 | `CompleteEnv` re-invokes the binary at Tab-time (`COMPLETE=bash nixon`), which is exactly what the Haskell `nixonCompleter` did. Dynamic values (command names, project names) via `ArgValueCompleter`. Must run before anything writes to stdout. The generated shell snippet should be `eval`'d at shell start, not written to a file (interface is unstable) |
| TUI | [`ratatui`](https://crates.io/crates/ratatui) | 0.30.2 | With the default `crossterm` backend (`ratatui-crossterm`). **Render to stderr** (`CrosstermBackend::new(io::stderr())`) — stdout must stay clean for `--select`/`--list`/`--insert` output consumed by the shell widgets; this is what fzf does with `/dev/tty` |
| Fuzzy matching | [`nucleo`](https://crates.io/crates/nucleo) / [`nucleo-matcher`](https://crates.io/crates/nucleo-matcher) | 0.5.0 / 0.3.x | Helix's matcher; fzf-compatible scoring, ~6× faster than skim, correct Unicode. `nucleo` (high-level) gives a background matcher + `Injector` so candidates from a slow placeholder command (`rg --files`) stream in while the user types. `nucleo-matcher` (sync) is used for the non-interactive `--filter` path and in tests. Reference implementation of ratatui+nucleo: [`television`](https://crates.io/crates/television) 0.15.9 |
| Text input widget | [`tui-textarea`](https://crates.io/crates/tui-textarea) | 0.7.0 | For the query line and for **edit-before-run** (replaces haskeline; gains multi-line editing). Last release Oct 2024 — stable but slow-moving; `tui-input` is the single-line fallback |
| ANSI in candidates | `ansi-to-tui` | — | fzf `--ansi` parity: `git log --color` output rendered with colours in the picker. Plus `strip-ansi-escapes` for the *values* returned |
| Markdown | [`comrak`](https://crates.io/crates/comrak) | 0.55.0 | Rust port of `cmark-gfm`, i.e. the same C parser the Haskell `cmark` binding wrapped — closest behavioural match. Full AST with `Sourcepos` (**1-based** line/column, start and end) on every node, which SPEC §4.6 needs. `pulldown-cmark` (iterator + byte offsets) is the lighter alternative if we ever want to drop the AST |
| Placeholder grammar | [`winnow`](https://crates.io/crates/winnow) | 1.0.4 | Parser combinators for SPEC §5.3 (`${}`, `<{}`, `={}`, `\| cols+h 1,2`). Replaces parsec. Good error positions |
| JSON | `serde` + `serde_json` | — | Config blocks and `\| json` candidates |
| YAML | [`serde-saphyr`](https://crates.io/crates/serde-saphyr) | 1.3.0 | `serde_yaml` is deprecated (Mar 2024). Forks: `serde_yaml_ng` (0.10, May 2024) and `serde_norway` (0.9.42, Dec 2024) are drop-ins but barely moving; `serde-saphyr` is pure Rust, panic-free, actively released (Sept 2026). Our YAML surface is tiny (a flat config object), so keep it behind `config::yaml::parse` to make swapping trivial |
| XDG dirs | [`etcetera`](https://crates.io/crates/etcetera) | 0.11.0 | `$XDG_CONFIG_HOME/nixon.md`, `$XDG_CACHE_HOME/nixon`. Unopinionated, supports the XDG strategy on macOS too (the Haskell used XDG on both) |
| Path expansion | `shellexpand` + `glob` | — | Replaces `wordexp` for `project_dirs` (`~/src`, `$HOME/x`, `~/src/*`) |
| Processes | `std::process::Command` + `nix` | — | Background spawn (SPEC §7.3): **do not `fork()`** — nucleo owns a threadpool and forking a multithreaded process is UB-adjacent. Use `Command` + `CommandExt::process_group(0)` (or `pre_exec` → `setsid()`), detach stdio, don't wait |
| Signals | `signal-hook` | — | Ignore SIGINT in nixon while a foreground child runs (SPEC §7.3) |
| Shell quoting | `shell-words` | — | Fix the nix-shell `--command` quoting quirk (SPEC §7.3) |
| Hashing | `sha1` + `hex` (RustCrypto) | — | Cache file names `<sha1>-<name><ext>` |
| Errors | `thiserror` (library) + [`miette`](https://crates.io/crates/miette) (binary) | 7.6.0 | Typed errors per module; miette renders `nixon.md:12:3` parse diagnostics with source snippets — a big upgrade over `ParseError "…"` |
| Logging | `tracing` + `tracing-subscriber` | — | Plain-text to stderr, level from `-L`. Spans around "resolve placeholder", "evaluate" make `-L debug` actually useful |
| Editor launch | (none) | — | `$VISUAL` → `$EDITOR` → `nano`, `+<line> <file>` as today |

### 2.2 Test-only

| Crate | Purpose |
|---|---|
| `rstest` | Fixtures + parametrised cases (the 24-row `parseCommandName` table becomes one `#[rstest]`) |
| `proptest` | Property tests (QuickCheck replacement): placeholder grammar round-trip, `parse_columns` invariants, config merge laws, log-level filtering |
| `insta` | Snapshots: picker frames rendered through `ratatui::backend::TestBackend`, error-message rendering, `--help` output |
| `pretty_assertions` | Readable diffs |
| `tempfile` | Project/config fixtures on disk |
| `assert_cmd` + `assert_fs` + `predicates` | Functional tests against the built binary in `tests/` |
| `trycmd` | Table-driven CLI cases in `tests/cmd/*.toml` **and** runs the code blocks in `README.md` as tests — keeps docs honest |
| [`expectrl`](https://crates.io/crates/expectrl) 0.9.0 | PTY-driven end-to-end: start `nixon`, type a query, press Enter, assert stdout. The only way to test the real event loop + terminal setup/teardown |
| `mockall` (optional) | If hand-written fakes for `ProcessRunner`/`Picker` get tedious. Prefer hand-written fakes first |

---

## 3. Static analysis & quality gates

All of these run in CI; the first four also run locally via `bacon`/`nixon.md`.

| Tool | Gate | Config |
|---|---|---|
| `cargo fmt --check` | formatting | `rustfmt.toml`: defaults + `imports_granularity = "Module"`, `group_imports = "StdExternalCrate"` (nightly-only options are fine to *have*; stable rustfmt ignores them) |
| `cargo clippy --all-targets --all-features -- -D warnings` | lints | Lint levels live in `[workspace.lints]` (below), not on the CLI, so IDEs agree with CI |
| `cargo doc --no-deps` with `RUSTDOCFLAGS="-D warnings"` | broken doc links, missing docs on pub items | `#![warn(missing_docs)]` on the lib |
| `cargo nextest run` | tests | Faster, per-test process isolation (important: tests that `chdir` or set env vars can't poison each other), JUnit output, retries for flaky PTY tests |
| `cargo llvm-cov nextest` | coverage report | Upload to Codecov/Coveralls or just publish the HTML artifact. Don't gate on a number initially; gate on "no decrease" once stable |
| `cargo deny check` | advisories, licenses, duplicate versions, sources | `deny.toml`. Replaces `cargo-audit` (same RustSec DB); we do not run both |
| [`cargo shear`](https://crates.io/crates/cargo-shear) 1.13 | unused / misplaced deps | Successor to `cargo-machete`/`cargo-udeps`; works on stable |
| `cargo msrv verify` | MSRV honesty | |
| `typos` | spelling in code, docs, nixon.md | Would have caught "Terminal emultor" |
| `taplo fmt --check` | `Cargo.toml` / `*.toml` formatting | |
| [`cargo mutants`](https://crates.io/crates/cargo-mutants) 27.1 | mutation testing | Nightly/weekly job, not per-PR. Scope to `markdown/`, `placeholder/`, `format/`, `config/` — the pure modules where a mutant slipping through means the parser spec is under-tested |
| `cargo hack --feature-powerset` | only if we grow optional features | Skip initially |
| Miri | only for `unsafe` | Expect a single `unsafe` (`pre_exec`/`setsid`); workspace `unsafe_code = "deny"`, `#![allow(unsafe_code)]` in `process.rs` only |

Not needed: `cargo-semver-checks` (we don't publish a library API), `cargo-vet` (overkill for a personal tool; `cargo-deny` sources/licence policy is enough).

### 3.1 Lint configuration (`Cargo.toml`)

```toml
[workspace.lints.rust]
unsafe_code = "deny"            # process.rs opts out with #![allow(unsafe_code)]; forbid can't be overridden
missing_docs = "warn"
unreachable_pub = "warn"
rust_2018_idioms = { level = "warn", priority = -1 }

[workspace.lints.clippy]
all = { level = "warn", priority = -1 }
pedantic = { level = "warn", priority = -1 }
nursery = { level = "warn", priority = -1 }
unwrap_used = "warn"            # allow in tests via #![cfg_attr(test, allow(clippy::unwrap_used))]
expect_used = "warn"
panic = "warn"
print_stdout = "warn"           # stdout is *data* (SPEC §10.8) — only output.rs may print
print_stderr = "warn"           # everything else goes through tracing
dbg_macro = "warn"
todo = "warn"
# pedantic lints we opt out of
module_name_repetitions = "allow"
must_use_candidate = "allow"
missing_errors_doc = "allow"
```

`print_stdout`/`print_stderr` + `panic`/`unwrap_used` are the important
ones: the Haskell version had four `error`/`undefined` crash paths (SPEC §5.6,
§8.4) and mixed stdout/stderr; the lints make both impossible to reintroduce
silently.

### 3.2 CI

CI runs `nix flake check` (§1.1); every tool in the table above is a flake
`check` and gets its version from nixpkgs, not from `cargo install` or a
GitHub action. This keeps `nix develop`, local `bacon`, and CI identical.

Workflow (`.github/workflows/ci.yml`), on push/PR to `main`:

1. `check` (`ubuntu-latest`, `macos-latest`): install nix, cachix (`myme`),
   `nix flake check -L`, `nix build -L`, upload the coverage HTML from
   `checks.coverage` as an artifact.
2. `mutants` (weekly schedule): `nix develop -c cargo mutants` on the pure
   modules; results posted as an artifact.

Use `DeterminateSystems/magic-nix-cache-action` or cachix for the store;
`Swatinem/rust-cache` is unnecessary since crane's `cargoArtifacts` derivation
is the cache.

---

## 4. Repository & crate layout

A Cargo **workspace** with three crates. The split isolates the reusable,
config-agnostic picker from the nixon domain logic and keeps `main.rs` thin so
integration tests can drive the library directly.

```
nixon/
├── Cargo.toml                 # [workspace], [workspace.lints], [workspace.dependencies]
├── rust-toolchain.toml
├── rustfmt.toml  deny.toml  typos.toml  taplo.toml  .config/nextest.toml
├── flake.nix  flake.lock       # crane + rust-overlay; package, checks, devShell (§1.1)
├── nix/                        # package.nix, checks.nix, shell.nix — keep flake.nix short
├── SPEC.md  ENGINEERING.md  README.md  nixon.md
├── crates/
│   ├── nixon-picker/          # generic fuzzy picker (ratatui + nucleo); no nixon concepts
│   ├── nixon/                 # library: everything in SPEC.md
│   └── nixon-cli/             # binary: clap, miette reporting, tracing setup, completion
└── extra/                     # shell widgets, completion loaders
```

### 4.1 `nixon-picker` (library)

Owns the terminal. Knows nothing about commands, projects or markdown.

```
src/
├── lib.rs           # pub use {Picker, PickerOptions, Candidate, Selection, SelectionType, Key}
├── candidate.rs     # Candidate { display: String (may contain ANSI), value: String }
├── options.rs       # PickerOptions { prompt/header, initial_query, multi, exact, ignore_case,
│                    #   sort: bool, expect: Vec<(Key, SelectionType)>, select_one: bool (fzf -1) }
├── selection.rs     # Selection<T> = Empty | Canceled | Selected { kind: SelectionType, items: Vec<T> }
├── matcher.rs       # thin wrapper over nucleo: Injector-based streaming, snapshot(), sync filter()
├── filter.rs        # non-interactive `filter(query, candidates) -> Vec<Candidate>` (fzf --filter parity)
├── ui/
│   ├── mod.rs       # App state machine: query, cursor, marked set, scroll; pure `update(Event) -> Effect`
│   ├── render.rs    # `fn render(&App, &mut Frame)` — pure, snapshot-testable via TestBackend
│   ├── keymap.rs    # Key → Action; expect keys resolve to SelectionType
│   └── input.rs     # tui-textarea wrapper for the query line and the edit-before-run editor
├── terminal.rs      # raw mode + alternate/inline viewport on stderr; RAII guard restores on panic
└── editor.rs        # `edit_text(initial) -> Option<String>` (multi-line, in the same terminal)
```

Design rules:

- `ui::App` is a **pure state machine**: `fn handle(&mut self, ev: Event)`; no
  I/O. `render` is a pure function of `&App`. Both are unit- and
  snapshot-tested with `TestBackend` without a PTY.
- `terminal.rs` is the only module touching crossterm; guarded by a `Drop`
  impl that always restores the terminal (also installed as a panic hook).
- The `Picker` trait:

  ```rust
  pub trait Picker {
      fn pick(&mut self, opts: &PickerOptions, candidates: CandidateStream) -> Result<Selection<Candidate>>;
  }
  ```

  Implementations: `TuiPicker` (real), `FilterPicker` (non-interactive,
  `--filter`/`--list` paths), and `ScriptedPicker` (test double: answers from
  a queue of pre-programmed selections). The last one lives in the crate
  under `#[cfg(feature = "test-util")]` so `nixon` tests can use it.

### 4.2 `nixon` (library) — SPEC.md module map

```
src/
├── lib.rs
├── config/
│   ├── mod.rs        # Config, merge() (§3.3), load_global(), find_local()
│   ├── schema.rs     # serde structs for the JSON/YAML block (§3.4)
│   └── yaml.rs       # the one place serde-saphyr is imported
├── markdown/
│   ├── mod.rs        # parse(path, text) -> Result<ParsedFile { config, commands }, ParseError>
│   ├── extract.rs    # comrak AST → Vec<Node> (§4.2)
│   ├── header.rs     # `{.arg key="val"}` attribute grammar (§4.3)
│   ├── walk.rs       # the command/config state machine (§4.4), location tracking (§4.6)
│   └── command.rs    # parse_command / parse_command_name (§4.5)
├── placeholder/
│   ├── mod.rs        # Placeholder, PlaceholderType, PlaceholderFormat (§5.2)
│   └── grammar.rs    # winnow parsers (§5.3) — `parse_one`, `scan_all`
├── command.rs        # Command, CommandLocation, display forms (§5.1)
├── language.rs       # Language, interpreter(), extension() (§7.1)
├── project/
│   ├── mod.rs        # Project, ProjectType, Marker
│   ├── detect.rs     # find_project_types, find_in_project[_or_default] (§9.2–9.5)
│   └── discover.rs   # find_projects (depth, expansion) (§9.6)
├── discover.rs       # find_project_commands (type filtering, bin dirs, sort) (§5.5)
├── resolve.rs        # zip_args, resolve_env, resolve_cmd (§5.6)
├── format.rs         # parse_columns, format_columns, pick_fields (§6)
├── eval/
│   ├── mod.rs        # evaluate(): mode decision tree (§7.3)
│   ├── cache.rs      # script cache + gc (§7.2)
│   └── wrap/{direnv.rs, nix.rs}
├── process.rs        # `trait ProcessRunner { run, run_capture, spawn_detached }` + RealRunner;
│                     # the only module with allow(unsafe_code)
├── select.rs         # bridges nixon types ↔ nixon-picker: command_candidates(), project_candidates()
├── app/
│   ├── mod.rs        # `struct App<P: Picker, R: ProcessRunner>` { config, picker, runner } — no backend field
│   ├── run.rs  project.rs  eval.rs  new.rs  edit.rs  gc.rs   # one per subcommand (§10)
│   └── handle.rs     # handle_cmd (§10.7)
├── output.rs         # the only module allowed to print to stdout
├── error.rs          # NixonError (thiserror); miette::Diagnostic impls with spans
└── fs.rs             # find_dominating_file, implode_home, xdg paths
```

Layering (dependencies point downward only; enforce by review, and
optionally with `cargo-modules` graphs):

```
app  ─►  discover / resolve / eval  ─►  config / markdown / project / placeholder / format / language
 │                 │
 └─► select ─► nixon-picker          └─► process / fs
```

Rules:

- `config`, `markdown`, `placeholder`, `format`, `language`, `command` are
  **pure** (no I/O, no env). They are where mutation testing and property
  tests concentrate.
- Anything that runs a subprocess goes through `ProcessRunner`; anything
  interactive goes through `Picker`. `App<P, R>` is generic over both so the
  whole subcommand layer runs in tests with fakes.
- No `std::env::set_current_dir`. Pass `cwd` explicitly everywhere (fixes
  SPEC §5.6 global-chdir quirk and makes tests parallel-safe).
- `output.rs` is the one place that writes to stdout (`print_stdout` lint
  elsewhere).

### 4.3 `nixon-cli` (binary)

```
src/
├── main.rs        # CompleteEnv::complete() first; then tracing init; then run(); miette report on Err
├── cli.rs         # clap derive: Cli, GlobalOpts, Subcommand enum (§2); From<GlobalOpts> for Config
├── complete.rs    # ArgValueCompleter impls for command / project names (§2.4)
└── exit.rs        # exit-code policy (propagate child status — SPEC §7.3 quirk fixed)
tests/
├── cli.rs         # assert_cmd: --help snapshots, error exits, --list/--select/--insert/gc/eval
├── cmd/*.toml     # trycmd cases; also `.case("../../README.md")`
├── pty.rs         # expectrl: real TUI sessions (type query, Enter, alt-Enter, F1, Esc)
└── fixtures/      # nixon.md files + fake project trees built with assert_fs
```

---

## 5. Test strategy

Four layers; every SPEC checklist item should map to at least one.

| Layer | Where | Tooling | What |
|---|---|---|---|
| Unit | `#[cfg(test)] mod tests` next to code | `rstest`, `proptest`, `pretty_assertions` | Port the Haskell suite **verbatim first** (SPEC §14 — it's the de-facto grammar spec), then add the untested areas: `zip_args`, stdin concat/env join, direnv/nix wrapping, `find_projects` depth, config merge, subdirectory project detection (the SPEC §9.4 bug), section-level `type` inheritance (SPEC §4.4 bug) |
| Snapshot | same, `snapshots/` dirs | `insta` + `TestBackend` | Picker frames at 80×20 for: empty, filtered, multi-marked, scrolled, header/prompt variants; miette error renderings; `--help` |
| Component | `crates/nixon/tests/` | `ScriptedPicker`, `FakeRunner`, `tempfile` | Drive `App` end-to-end in-process: "run `vim-file` → runner receives `git ls-files` → picker returns row 2 → runner receives `bash <cache>/…sh README.md` with cwd=project". This is where most SPEC §5.6/§7.3/§10 items live |
| Functional | `crates/nixon-cli/tests/` | `assert_cmd`, `assert_fs`, `trycmd`, `expectrl` | Real binary. Non-interactive paths (`-T run -l`, `project -l`, `eval`, `gc`, `--select` with a pre-seeded unique query) with `assert_cmd`; interactive paths through a PTY with `expectrl`. Fake interpreters on `$PATH` (a `bash` shim that records argv) to avoid depending on the host |
| Property | unit layer | `proptest` | `format!(placeholder) → parse → eq`; `parse_columns` never drops rows; `Config` merge is associative; log filtering |
| Mutation | scheduled CI | `cargo-mutants` | Pure modules only |

Conventions:

- Test names describe behaviour: `hidden_commands_are_excluded_from_selection_but_not_from_list`.
- Fixtures: a `fixtures::project()` builder (`assert_fs`) that lays out
  `.git/`, `nixon.md`, `bin/` etc. — no shared mutable temp dirs.
- Never rely on `$HOME`, `$EDITOR`, `$SHELL` of the host: tests set them
  explicitly via `Command::env_clear()` + `.env(...)`.
- Anything that walks **up** the filesystem (project detection, dominating
  files) must not use real marker names like `.git` in fixtures: an ancestor
  of the temp dir may match (this host has a stray `/tmp/.git`). Derive the
  marker name from the temp directory itself.
- PTY tests are tagged `#[ignore = "pty"]`-style via nextest filter
  (`.config/nextest.toml` profile) so they can be excluded on constrained
  runners but run in CI.

---

## 6. Conventions

- **Errors**: every module has an `enum XError` (`thiserror`); the lib
  exposes `NixonError` wrapping them; the binary maps to exit codes and
  miette reports. No `unwrap`/`expect` outside tests; no `panic!` for user
  input.
- **Selection cancel** (Esc/^C anywhere, including inside placeholder
  expansion) is a normal `Selection::Canceled` that unwinds to exit code
  `130`, not an error — replaces the Haskell `error "Argument expansion
  aborted"`.
- **Child exit status** is propagated (SPEC §7.3 quirk fixed).
- **Stdout is data.** Human messages → stderr via `tracing`. The TUI renders
  on stderr.
- **Docs**: every `pub` item documented; SPEC section numbers cited in doc
  comments (`/// SPEC §5.3`) so the checklist stays traceable.
- **Commits/PRs**: Conventional-ish prefixes as in the current history
  (`feat:`, `fix:`, `deps:`); PR bodies are one line per paragraph.
- **Dependencies**: additions need a one-line justification in the PR and
  must pass `cargo deny` (licence allow-list: MIT, Apache-2.0, BSD-*, MPL-2.0
  for nucleo, Unicode-3.0).

---

## 7. Decisions

Guiding principle: keep the day-to-day experience of v1 (same `nixon.md`
files, same CLI shapes, same keys, same outputs on stdout) and fix only what
is a bug or an accident. Anything that changes a documented behaviour is
listed here so it is deliberate.

### 7.1 The open questions

| # | Question | Decision | Rationale |
|---|---|---|---|
| 1 | Picker viewport | **Full-screen alternate screen**, like v1 (v1 called fzf without `--height`). Add a `picker.height` config key later if wanted; not in scope now. | Same look as today. Inline viewports interact badly with the shell widgets' readline redraw. |
| 2 | Filter-mode ordering | Use nucleo's score ordering for `--list`, `\| list` and the query pre-filter. **Best effort parity, not tested for exact order.** Command selection keeps v1's `--no-sort`: candidates that match are shown in discovery order (by name), i.e. matching is a filter, ranking is stable. | nucleo's algorithm is fzf-derived; exact tie-breaking differs and nobody depends on it. |
| 3 | YAML crate | **`serde-saphyr`**, imported only in `config/yaml.rs`. | Actively maintained, pure Rust, panic-free. Swappable in one file. |
| 4 | Edit-before-run | After the picker closes, show an inline `> ` editor (tui-textarea) in the same terminal, pre-filled with the stripped source, **multi-line capable**. `Enter` submits, `Alt-Enter` inserts a newline, `Esc`/`Ctrl-C` cancels, empty submit → `Empty command.` | Mirrors the v1 haskeline prompt; fixes v1's single-line-only editing of multi-line sources. |

### 7.2 Execution model (consequences of "stand-alone, no GUI")

The **backend concept is removed entirely**: no `Backend` type, no
`BackendType`, no `is_gui_backend`, no auto-detection, no `backend` config
key. There is one picker (`nixon-picker`) and one execution path. Everything
that only existed to serve the GUI/TTY split goes with it:

| Removed | v1 role | v2 |
|---|---|---|
| `-b/--backend` | choose fzf/rofi | Gone. Not even a hidden compat flag: passing it is a normal clap "unexpected argument" error. |
| `backend` in `Config` / `Env` | carry the choice | Gone. |
| Auto choice "fzf if stdin is a TTY else rofi" | pick a UI | Gone. The picker needs a TTY; a required interactive selection without one → `interactive selection needs a terminal`, exit 1. Non-interactive paths (`--list`, `\| list`, unique `-1` matches) work without a TTY. |
| `-t/--terminal`, `terminal` config, `$TERMINAL`, `x-terminal-emulator` fallback, the "Press Return to exit" suffix | spawn a terminal for non-GUI commands under rofi | Gone. Commands run in the current terminal. |
| `-T/--force-tty`, `force_tty` | "never fork or spawn" — collapse the GUI decision tree | Gone. The remaining decision is purely `is_bg` (`&`): foreground otherwise, detached if `&`. Wanting a `&` command in the foreground = don't mark it `&`. |
| `Backend.projectSelector / commandSelector / selector` trait | per-backend UIs | Replaced by the single `Picker` trait in `nixon-picker`; `select.rs` builds candidates, nothing else knows about the UI. |

**Shell widgets**: `extra/nixon-widget.{bash,zsh}` are rewritten for v2 —
`nixon run -s`, `nixon run -i`, `nixon project -s` (no `-b fzf -T`). They
ship with v2; the v1 widgets are not compatible and that is documented in the
changelog.

The `Nixon.hs` global-options row in SPEC §2.1 therefore shrinks to:
`-C`, `-e/--no-exact`, `-i/--no-ignore-case`, `-p`, `-d/--no-direnv`,
`-n/--no-nix`, `-L`.

| Kept | Decision |
|---|---|
| Background (`&`) commands | Detached via `Command` + new process group/session, stdio → `/dev/null`; no `fork()`. |
| SIGINT while a foreground child runs | Ignored in nixon, delivered to the child (as v1). |
| Child exit status | **Propagated** as nixon's exit code (v1 always exited 0). |
| Cancel (Esc/^C) anywhere, including during placeholder expansion | Exit **130**, message `Selection canceled.` on stderr. No panic. Deliberately collides with a child that itself exits 130 (SIGINT) — both mean "interrupted" to a shell script, and that is the conventional code. |
| Keys in the picker | `Enter` run · `Alt-Enter` edit-before-run · `F1` show source · `F2` open in `$EDITOR` · `Tab` toggle mark (multi) · `Esc`/`Ctrl-C` cancel · `Ctrl-N/P`, `Up/Down` move. Same as v1's fzf bindings plus fzf's defaults people already use. |
| `-1` auto-select | Kept: a query (from the CLI positional) that matches exactly one candidate selects it without opening the picker. |
| ANSI in candidates | Kept (fzf `--ansi`): rendered in the picker, stripped from returned values. |

### 7.3 Bugs fixed (behaviour changes from v1)

| SPEC | v1 behaviour | v2 |
|---|---|---|
| §9.4 | Project found from a subdirectory has empty name and parent path | Correct project root. Test from `proj/sub/`. |
| §4.4 | `type="…"` on a non-command section heading ignored | Applies to all commands nested under the heading. |
| §7.3 | Child exit code discarded | Propagated. |
| §5.6 | Missing placeholder command / bad JSON / cancel → panic | Clean errors; cancel → 130. |
| §5.6 | Process-wide `chdir` before selection | Explicit `cwd` everywhere. |
| §7.3 | nix-shell `--command` naive quoting | Each argv element shell-quoted (`shell-words`). |
| §3.2 | `terminal:` in config silently dropped; `backend`/`force_tty` not readable from config | Moot — all three removed (see 7.2). Unknown keys remain ignored. |
| §2.3 | Missing global config → hard error `NoSuchFile` | **Tolerated**: no file = empty config (logged at debug). Empty file likewise. Parse errors stay fatal, now rendered by miette with `file:line:col` and a snippet. |
| §2.3 / §10 | Error text printed via Haskell `show` (quoted strings, constructor names) | Plain messages. |
| §4.3 | Heading kwarg values limited to letters (`type="c++"` breaks the whole heading) | Unquoted: `[A-Za-z0-9_-]+`; quoted: anything but `"`. |
| §4.5 | Any non-paragraph block between heading and code block fails the file | Non-paragraph blocks are skipped; first paragraph is the description. |
| §5.3 | Second format modifier only sometimes rejected; `filter "…"` alnum-only | Any second format → error; filter value: anything but `"`. |
| §8.4 | Unknown picker failure → `Prelude.undefined` | Moot (no external picker). |
| §10.4 | `new` writes temp file to `/tmp`; can splice into a `bin/` executable | System temp dir; `new` refuses non-markdown commands with `cannot insert after a bin command`. |
| §2.1 | "Terminal emultor" | Moot (flag removed). |
| §8.1 | Backend picked by whether stdin is a TTY | Moot — no backends. |
| §5.5 | `bin_dirs` offered every entry (non-executables, subdirectories, recursive contents) as commands | Only regular files with an execute bit, non-recursive. |

### 7.4 Quirks kept on purpose (do not "fix")

- Config merge rules exactly as §3.3, including local commands first and
  list concatenation without dedupe.
- Local config = `nixon.md` anywhere up the ancestor chain first, then
  `.nixon.md` anywhere up the chain (§3.1) — a farther `nixon.md` beats a
  nearer `.nixon.md`.
- Project discovery depth 1 with wordexp-style expansion; hidden dirs
  included; discovery runs on every invocation (§9.6). (Perf: keep it
  cheap; add a cache only if measured.)
- `--list` with no matches prints `No commands.`/`No projects.` on stderr and
  exits **0** (§10.1) — the shell widgets rely on it.
- `--select` semantics (§10.7): the selected command is *run* and its output
  is offered for multi-selection; selected values printed to stdout.
- `--insert` prints the raw source including its trailing newline.
- Hidden `_commands` are excluded from the run picker but present for
  placeholders, `--list`, `edit`, `new` (§5.5).
- Script cache layout `$XDG_CACHE_HOME/nixon/<sha1>-<name><ext>` and the
  `gc` output format (§7.2). Scripts stay non-executable; shebangs ignored.
- Language table and interpreters (§7.1) unchanged. Per-language interpreter
  overrides are **deferred**, not planned for the parity release.
- `new` template layout and `y/N` confirmation flow (§10.4).
- Placeholder args from the CLI are *search queries*, not values (§5.6).

### 7.5 Explicitly deferred (post-parity)

Everything in SPEC §15 (`todos.org`), plus: picker height/style config,
per-language interpreters, `flake.nix` support for the nix wrapper, a
`loglevel` config file key.

## Sources

- [ratatui crate](https://crates.io/crates/ratatui) · [Backends](https://ratatui.rs/concepts/backends/) · [Testing with insta snapshots](https://ratatui.rs/recipes/testing/snapshots/)
- [nucleo (helix-editor)](https://github.com/helix-editor/nucleo) · [nucleo docs](https://docs.rs/nucleo) · [nucleo-matcher](https://crates.io/crates/nucleo-matcher) · [television](https://crates.io/crates/television)
- [comrak](https://github.com/kivikakk/comrak) · [Sourcepos](https://docs.rs/comrak/latest/comrak/nodes/struct.Sourcepos.html) · [LineColumn (1-based)](https://docs.rs/comrak/latest/comrak/nodes/struct.LineColumn.html)
- [serde_yaml deprecation thread](https://users.rust-lang.org/t/serde-yaml-deprecation-alternatives/108868) · [serde-saphyr](https://crates.io/crates/serde-saphyr) · [serde_yaml_ng](https://crates.io/crates/serde_yaml_ng) · [serde_norway](https://crates.io/crates/serde_norway)
- [clap](https://crates.io/crates/clap) · [clap_complete CompleteEnv](https://docs.rs/clap_complete/latest/clap_complete/env/struct.CompleteEnv.html) · [Dynamic completion discussion](https://github.com/clap-rs/clap/discussions/5677)
- [winnow](https://crates.io/crates/winnow) · [etcetera](https://crates.io/crates/etcetera) · [miette](https://crates.io/crates/miette) · [tui-textarea](https://crates.io/crates/tui-textarea)
- [trycmd](https://docs.rs/trycmd) · [snapbox](https://github.com/assert-rs/snapbox) · [CLI book: testing](https://rust-cli.github.io/book/tutorial/testing.html) · [expectrl](https://crates.io/crates/expectrl) · [ratatui-testlib](https://crates.io/crates/ratatui-testlib) (0.1.0, too young — noted, not adopted)
- [cargo-shear](https://crates.io/crates/cargo-shear) · [cargo-mutants](https://crates.io/crates/cargo-mutants) · [RustSec](https://rustsec.org/) · [cargo-audit/cargo-deny setup](https://adhdecode.com/articles/rust/rust-security-audit-cargo-deny/) · [Rust static analysis tools overview](https://analysis-tools.dev/tag/rust)
