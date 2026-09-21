# Nixon — working rules

Project environment and command launcher, in Rust. Three crates:
`nixon-picker` (generic fuzzy picker), `nixon` (everything the tool does),
`nixon-cli` (the binary).

## Architecture

Dependencies point one way: `nixon-cli` → `nixon` → `nixon-picker`.

- `nixon-picker` knows nothing about commands, projects or markdown, and
  `select.rs` is the only module that builds candidates and picker options
  from nixon's types. Other modules may name the picker's types.
- `Picker` is the only seam to the terminal, `ProcessRunner` the only seam to
  subprocesses. `App<P: Picker, R: ProcessRunner>` is generic over both, which
  is what lets the whole subcommand layer run in tests with fakes.
- `output.rs` is the only module that writes to stdout.
- Pass `cwd` explicitly. Never `set_current_dir`: it is process-global and
  makes tests order-dependent.
- `config/yaml.rs` is the only place serde-saphyr is imported, so the YAML
  crate can be swapped in one file.
- Pure modules — `config`, `markdown`, `placeholder`, `format`, `language`,
  `command` — do no I/O and read no environment. Anything they need is a
  parameter (`$SHELL`, `$HOME`, `$DIRENV_DIR`, XDG paths).

## Lints and tooling

Everything runs through nix: `nix develop` for the shell, `nix flake check`
for the gates. Nothing is `cargo install`ed; nothing runs in CI that
`nix flake check` does not.

- Lint levels live in `[workspace.lints]`, not on the command line, so the
  IDE and CI agree. They are `warn` there and the clippy check runs
  `-D warnings`, so a warning is a failed build.
- `unsafe_code`, with no exceptions anywhere. There is no `unsafe` block in
  the workspace. Detaching uses `Command::process_group(0)`, which is safe;
  do not reach for `fork`.
- `print_stdout`/`print_stderr` are lints so stdout stays data and human
  output goes through `tracing`. `output.rs` writes through a locked handle,
  which the lint does not catch — that is the convention, keep it there.
- `unwrap_used`/`expect_used`/`panic` apply outside tests; each crate allows
  them under `cfg(test)`.
- Prefer `#[expect(lint, reason = "…")]` over `#[allow]`: it fails once the
  suppression stops being needed.

## Tests

Four layers: unit and property next to the code, `insta` snapshots for picker
frames and rendered errors, component tests driving `App` with `ScriptedPicker`
and `FakeRunner`, functional tests against the built binary, plus PTY tests for
the real event loop.

Fixture rules, each learned from a failure:

- crane's source filter keeps only what cargo needs to **build**. Test
  fixtures must be added to `keep` in `nix/package.nix` — `.snap` and
  `README.md` are there — or the nix checks see a different tree than
  `cargo test` does, and pass locally while failing under nix.
- Fixtures for anything that walks **up** the filesystem must not use real
  marker names like `.git`: an ancestor of the temp dir may match. Derive the
  marker from the temp directory's own name.
- PTY tests assert on post-exit stdout, exit codes and side effects (a fake
  `$EDITOR` that records argv), never on screen content: ratatui interleaves
  cursor escapes between characters, so matching drawn text is a coin flip.
- `-1` fires on the **empty** query too, so a fixture with one candidate runs
  it before any key arrives. Interactive fixtures need at least two — in
  component tests as well, because `ScriptedPicker` honours the contract.
- `cargo doc` runs `--no-deps`; documenting dependencies raced on the shared
  `target/doc` and failed intermittently.
- `cargo-shear` runs inside crane's vendored registry: a sandboxed build has
  no network for `cargo metadata` to reach crates.io.
- The 200k-candidate timing test is a **regression guard, not a target**. It
  runs unoptimised beside the other checks; the threshold is wide on purpose.

## Picker facts

- Matching runs on the candidate's **visible** text, ANSI stripped. Matching
  the raw text let a query match an escape sequence and put the highlight
  indices out of step with the rendered characters.
- Marks are keyed by a candidate `id` assigned on injection, not by row
  position, so they survive a query change. nucleo exposes no stable id for a
  matched row, which is why `Candidate` carries one.
- Matching runs on nucleo's background worker. The UI thread must never make
  a pass over the candidates: that cost ~180ms per keystroke at 200k. Under
  `exact_match` every keystroke is a full rescore, because the query rewrite
  is not append-safe; the 200k guard covers both paths.
- Only the visible window is built per frame, and match indices are computed
  for those rows only.
- The kitty keyboard protocol is an improvement, not a requirement: the legacy
  encoding carries the whole keymap, and both spellings of a key reach us as
  the same `KeyCode`. It is asked for unconditionally and never detected —
  crossterm's detection writes its query to **stdout** (its `/dev/tty` handle
  is opened read-only, so the write to it always fails and the fallback always
  runs), which corrupts every `$(nixon …)` and then stalls two seconds waiting
  for a reply a captured stdout can never carry.
- Nothing meant for the terminal may reach stdout. A PTY test asserts it by
  redirecting stdout to a file while stdin and stderr stay on the pty, which
  is the only way to tell the streams apart when both are the same terminal.
- The options row sits between the header and the query: it belongs to the
  command, not to the list. `Alt-1`…`Alt-9` toggle from anywhere; `Alt-o`
  focuses the row, where `Space` toggles and the arrows move, walking off
  either end returns to the query, and every other key — `Enter`, `Esc`,
  `Ctrl-C`, the list keys — still means what it means elsewhere. Typing is
  the one thing the row swallows.
- Cancelling signals the child's **process group**. The child is an
  interpreter; the work is its children, and killing only the interpreter
  leaves them holding the stdout pipe.

## Behaviour that constrains changes

- No backend concept. There is one picker; `-b`, `-t` and `-T` do not exist.
- Exit codes: the child's status is propagated; a cancelled selection is 130.
- `-1` auto-select is decided **once**, on the query the picker opened with.
  The terminal is taken lazily so it can still apply without one.
- `exact_selection` and `unique_selection` are the `Picker` **contract**: the
  trait defaults, `TuiPicker` and the test doubles all honour them, or the
  component layer answers differently from production.
- `history` sets `-1` only when a query was given. Opening the log and having
  it run the only entry is not what looking meant.
- `Lines` and `Fields` placeholders stream as the command produces them;
  `Columns` and `JSON` are buffered, because widths and the whole document
  are needed before a candidate exists.
- Config merge: `Option` fields take the right-hand value when set; path and
  type lists concatenate; **commands concatenate right-first**, so local
  commands shadow global ones in first-match lookups.
- Local config: the whole ancestor chain is searched for `nixon.md` before
  `.nixon.md` is tried anywhere, so a farther `nixon.md` beats a nearer
  `.nixon.md`.
- `--list` with no matches prints to stderr and exits **0**. The shell widgets
  depend on it.
- Hidden `_commands` are excluded from the run picker but available to
  placeholders, `--list`, `edit` and `new`. A name given in full runs its
  command without the picker, hidden or not, and beats a fuzzy match. So does
  an argument equal to a candidate's value.
- `run` takes one positional holding the command name and everything after
  it, so `nixon run c -i` and `nixon c -i` agree and nixon's own flags go
  before the name. `project` keeps its own positionals, so a flag after the
  project name is still nixon's.
- A `project` argument with a separator in it, or starting with `~`, `./` or
  `../`, is a directory: resolved on the spot, no discovery, no picker. `.`
  still means the project containing the cwd.
- Every command gets `nixon_project_path` and `nixon_bin` — `current_exe()`,
  so a command can call the same nixon back.
- A heading's `-f`/`--name` tokens are options: toggled at the prompt, placed
  in argv where the heading put them, exported as `nixon_opt_<name>`. Malformed
  option syntax fails the file with a position, never silently. The prompt is
  a convenience — with no terminal, or a command line that settled every
  option, the defaults run.
- Script cache: `$XDG_CACHE_HOME/nixon/<sha1-of-source>-<name><ext>`. Scripts
  are never made executable; the interpreter is always explicit, so a shebang
  is ignored.
- A missing or empty global config is not an error. A parse error is.
- Every executed command is appended to `$XDG_STATE_HOME/nixon/history` as
  `<epoch>\t<cwd>\t<replayable command line>`, one `write` per line so
  concurrent nixons do not interleave. Recording never fails a command, and
  the project's own config decides whether it happens at all — as it does for
  `nixon history`, which re-runs a logged line by handing it back to the
  argument parser rather than interpreting it.

- The shell widgets are fzf's mechanics, not reimplementations: `Alt-p` is
  its `Alt-C` macro, because a `bind -x` function that changes directory
  leaves the prompt showing the old one. fzf's macro has one sequence,
  `\C-\e(`, that bash 5.3 does not bind — readline abandons the rest of a
  macro at an unbound sequence, so the `cd` is typed and never run. It is
  dropped here.

## Prose

- Comments only where the code cannot say it itself. No commentary that
  restates the line below it.
- Function docstrings: one tight line, what it does, not how.
- No narrative paragraphs — not in code, not in commit messages, not in PR
  bodies. State the thing and stop.

## Commits

- Commit often: every logical or structural cut, every feature added. Many
  small commits beat one big one.
- Stage by explicit path: `git add <path>…`, never `git add -A`, `git add .`
  or `commit -a`. Anything else in the worktree is the user's.
- Subject: imperative, says what changed. Body: hard-wrapped at ~72 columns,
  says why. No changelog-style bullet dumps.
- PR and issue bodies are not hard-wrapped: one line per paragraph and per
  bullet.

## History

The planning documents — `SPEC.md` (a checklist of v1 behaviour),
`ENGINEERING.md` (how v2 was built) and `PARITY.md` (the two reconciled) —
were removed in `a1c1978`; read them with `git show a1c1978^:SPEC.md`.

The v1 Haskell implementation was removed in `2a84de2`; read it with
`git show 2a84de2^:src/Nixon/<Module>.hs`.
