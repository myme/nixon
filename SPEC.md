# Nixon GUI launcher — behavioral specification and implementation checklist

This is the contract and implementation checklist for the graphical launcher
in the Rust Nixon workspace. It follows the v2 rewrite spec's checklist style.
Checked items have code and test evidence; unchecked items name remaining
behavior or verification work.

The original reference points were the Nixon Rust tree at `9448c1a`, the v2
spec (`70d2b02:SPEC.md`, especially §8), and the local katapult PoC
(`../katapult/src/main.rs` at `a0c85af`). Implementation notes refer to the
current Rust workspace.

Key source seams: `crates/nixon-picker/src/picker.rs` (picker contract),
`crates/nixon/src/select.rs` (candidate mapping),
`crates/nixon/src/app/mod.rs` (application wiring),
`crates/nixon/src/process.rs` (process runner),
`crates/nixon-gui/src/window.rs` (window and input), and
`crates/nixon-cli/src/gui.rs` (GUI workflows).

Legend:

- **MUST** — required for the first usable GUI launcher.
- **SHOULD** — expected behavior, but may follow the first usable cut.
- **DECISION** — a deliberate product or architecture choice.
- **OUT OF SCOPE** — excluded from this integration; changing it needs a new
  decision.

Unchecked requirements may be partially implemented; their notes identify the
remaining gap. The packaged GUI passed its Xvfb interaction smoke, headless
Weston Wayland startup check, and nested Weston Wayland keyboard check; the
full Linux `nix flake check` passed on 2026-09-23. A real Wayland desktop
session remains unverified.

---

## 1. Purpose and entry points

The launcher combines two interaction models:

1. The old rofi path: choose a project, command, history entry, or placeholder
   value from a searchable GUI list, then let Nixon carry out the action.
2. Katapult's path: press a mnemonic key to enter a small menu and complete an
   action with a few more keys, without searching a long list.

Both use the same discovered projects, commands, config, placeholder
resolution, evaluation, and history as the terminal CLI.

- [x] **MUST:** Provide `nixon --mode gui` (also `nixon -m gui`) for a
  desktop shortcut or window manager binding. Starting it opens the root
  launcher window without needing a controlling terminal.
- [ ] **MUST:** The root offers at least **Commands**, **Projects**,
  **History**, and configured quick actions. Commands use the current project;
  Projects first choose a project and then one of its commands. History uses
  Nixon's existing history store and replay semantics. The root and all three
  pickers work. GUI replay accepts saved global flags with the CLI's existing
  ignore-on-replay behavior, bare names, and no-name/fuzzy command picks;
  `run --list` and `project --list` show copyable results in-window. Other
  non-recordable CLI actions remain unsupported.
- [x] **MUST:** The same GUI picker handles command selection, project
  selection, history, command options, and placeholder candidates. A command
  chosen through a mnemonic may still lead to a GUI placeholder pick.
- [x] **MUST:** Launching the GUI does not change `nixon`'s default terminal
  picker, shell widgets, or scripting output. There is no implicit switch to
  GUI based on stdin or display environment variables.
- [x] **DECISION:** One `nixon` binary provides both modes. `--mode tui` is the
  default; `--mode gui` (or `-m gui`) explicitly opens the graphical menu.
  This replaces the earlier separate-binary plan and does not restore
  `nixon -b rofi`.
- [x] **IMPLEMENTED:** The GUI opens the merged-config menu. Commands and Projects
  discover and run commands through the GUI picker and existing App flow on a
  worker. Browser and media actions run on workers. Project Inspect and command
  Show open a read-only detail panel; Visit hands off to the configured editor
  through the terminal launcher. Edit opens an in-window source editor and
  runs submitted text through the existing command flow. History opens a
  searchable picker of recent entries; Enter parses and replays the selected
  Nixon invocation through the GUI worker, while F1 and Alt-Enter show its
  recorded line in the detail panel. Unsupported or malformed replay entries
  stay visible as errors. GUI subcommands and bare command names are explicitly
  rejected. History replay currently supports a subset of saved CLI actions.
- [ ] **OUT OF SCOPE:** Calling the external `rofi` program or restoring its
  old exit-code/argv protocol. The behavior to recover is GUI selection.

## 2. What the references actually do

### 2.1 Nixon v1 rofi

The v1 spec §8.5 records a `rofi -dmenu` picker for projects, commands, and
generic placeholder values. It supported a query, fuzzy or normal matching,
case handling, multi-select, and custom results for Run, Edit, Show, and Visit.
It used an automatically chosen GUI backend when stdin was not a TTY. The v2
rewrite intentionally removed that backend and its terminal-spawning behavior
(`70d2b02:ENGINEERING.md` §7.2).

- [x] **MUST:** Recover the *selection capabilities* above with the built-in
  GUI. Preserve duplicate candidates by stable identity; v1 rofi's
  title-keyed map silently deduplicated them.
- [x] **MUST:** Preserve v2's CLI and execution fixes: clean cancellation,
  propagated child status for direct CLI runs, local config precedence, and no
  source mutation to append a shell-specific “press Return” trailer.

### 2.2 Katapult PoC

The PoC uses `eframe`/`egui` in one file. Its root shows **S** for Spotify and
**W** for Browser. Spotify offers **Space** play/pause, **P** previous, **N**
next through the Spotify MPRIS D-Bus name. Browser offers **O** for an input
that opens a URL/domain or searches with Google. Backspace/Ctrl-H goes up;
Q/Escape/Ctrl-C closes. It creates an always-on-top X11 dialog window.

`CLAUDE.md` mentions Discord, but the actual `src/main.rs` has no Discord
view, key, or action. The spec follows the executable code.

- [x] **MUST:** Preserve the few-key menu interaction and visible key labels.
- [x] **MUST:** Preserve Spotify play/pause, previous, and next on a Linux
  desktop with the Spotify MPRIS service available. A missing service is a
  visible error and leaves the launcher usable.
- [x] **MUST:** Preserve browser open/search: an `http://` or `https://` input
  opens as given; a no-space input containing `.` opens with `https://`; other
  input is URL-encoded into a configured search URL. Empty input does nothing.
- [x] **SHOULD:** Let the browser search URL be configured; default to the
  PoC's Google search URL for initial parity.
- [ ] **OUT OF SCOPE:** A Discord-specific integration. A Nixon command can
  launch Discord like any other application.

## 3. Workspace boundaries

The terminal workflow follows `nixon-cli -> nixon -> nixon-picker`.
`nixon-picker` is domain-neutral. `nixon::select` alone maps projects and
commands to picker candidates; `App<P: Picker, R: ProcessRunner>` runs the
workflows.

- [x] **MUST:** Add `nixon-gui` as a GUI library crate that owns window state,
  rendering, keyboard routing, and a `GuiPicker` implementation of the
  existing `nixon_picker::Picker` contract. It may depend on `nixon` to
  orchestrate its workflows without duplicating them.
- [x] **MUST:** Use the existing `nixon-cli` binary for config/env loading,
  GUI startup, and error/exit reporting. It depends on `nixon-gui` and `nixon`;
  neither `nixon` nor `nixon-picker` depends on GUI types.
- [ ] **MUST:** Keep command/project discovery, config merging, resolution,
  evaluation, history, and process execution in `nixon`. GUI code sends typed
  actions into that domain layer; it does not parse markdown or build shell
  command strings itself. GUI startup now takes its effective launcher config
  from `App::config_for`. GUI project picks use `App` selection methods; ordinary
  command picks and History replay use `App::prepare_run_command` and present
  its `RunDecision`. History loading, candidate construction, and picker
  orchestration share `nixon::app::history`; CLI and GUI replay share
  output-neutral `RunDecision` and `ProjectDecision` choices. CLI eval and GUI
  replay share `App::prepare_eval` for project selection. History argument
  parsing and eval replay presentation still live in `nixon-cli/src/gui/history.rs`.
- [x] **MUST:** Reuse `Candidate`, `PickerOptions`, and `Selection` semantics.
  Add a domain-neutral UI field to those types only when necessary for both
  pickers. Keep GUI styling out of the domain crate.
- [x] **MUST:** Keep one native window and one GUI event loop across nested
  selections. Nixon workflows run on a worker; its `GuiPicker` sends typed
  pick/confirm requests to the GUI event loop and waits for typed replies.
  The GUI thread must remain responsive during project discovery, command
  execution, and placeholder production. No nested `eframe::run_native` call.
- [x] **MUST:** Route every subprocess through `ProcessRunner` or a small
  extension of that seam, so component tests can use a fake runner. Keep the
  workspace's no-`unsafe` rule.
- [x] **SHOULD:** Implement streaming placeholder candidates through the
  bridge, with cancellation stopping the producer's process group, as the TUI
  does. The GUI shows candidates as they arrive and reports producer failure.

Dependency direction:

```text
nixon-cli -> nixon-gui -> nixon -> nixon-picker
          |           \-------> nixon-picker
          +-----------> nixon
          +-----------> nixon-picker
```

## 4. Root menu and quick actions

- [x] **MUST:** A fresh window focuses the root menu. Every visible action
  shows its mnemonic, name, and short description. Pressing its key enters a
  submenu, starts a pick, or executes the action immediately, as configured.
  Focus, action routing, and default/configured row rendering are covered by
  GUI component tests. Missing or blank descriptions get GUI hints; supplied
  nonblank descriptions render unchanged.
- [x] **MUST:** A mnemonic belongs to one menu level. Duplicate keys at the
  same level are a config error with the menu path; keys in different menus
  may be reused. Matching is case-insensitive for letters, and the displayed
  key remains the configured spelling.
- [x] **MUST:** The default root uses **C** Commands, **P** Projects, **H**
  History, **W** Browser, and the PoC's **S** Spotify. The built-in key map
  is documented in the UI and user guide; a replacement menu may reassign
  keys, and config conflicts are detected at load time. The root and nested
  keys are listed in `docs/cli.md`.
- [x] **MUST:** A configured quick action may run a named Nixon command in
  the current project or a named/discovered project. It uses the same local
  config, command options, placeholders, `direnv`/Nix wrapping, and history
  recording as choosing that command from the GUI list.
- [x] **MUST:** A menu item may contain child items. The UI keeps a navigation
  stack, so Backspace/Ctrl-H returns one level and the root has no parent.
- [x] **MUST:** While a text field has focus, printable keys enter text and
  never trigger mnemonics. The text field owns Backspace. Escape leaves the
  field or current picker; a second Escape at the root closes the window.
- [x] **MUST:** A successful one-shot quick action closes the window. A failed
  action displays its error in the window and keeps the current menu open.
  User cancellation makes no error toast.
- [ ] **SHOULD:** Let the user return to the root after an action that only
  changes external state, such as media control, through a setting for
  “stay open”; default to closing for PoC parity. Successful media actions
  currently close; no stay-open setting exists.
- [ ] **DECISION:** General app launching is expressed as Nixon commands,
  which already own cwd, environment, wrapping, and history. Scanning XDG
  `.desktop` entries is a later feature, not part of the PoC or v1 rofi.

## 5. GUI picker behavior

- [x] **MUST:** Show a prompt/header, editable query, match count, and a
  scrollable candidate list. Display descriptions and project paths without
  exposing raw ANSI control sequences. Match against the visible text.
- [x] **MUST:** Use Nixon's matcher and matching options, including fuzzy vs
  exact, smart/forced case handling, initial query, and stable discovery order
  where `no_sort` is set. Do not write a separate GUI matcher.
- [x] **MUST:** Enter selects the highlighted row. Up/Down and Ctrl-N/Ctrl-P
  move; Page Up/Down move by a viewport. Mouse selection is supported without
  changing the keyboard path. No match means Enter cannot choose a value.
- [x] **MUST:** The query is focused on opening a picker. Typing changes the
  result list immediately. A unique initial match honors `select_one` without
  drawing a picker; an exact candidate value honors `select_exact`.
- [x] **MUST:** For a multi-select pick, Tab marks/unmarks the highlighted
  row; marked items retain identity while the query changes. Enter returns
  marked items in candidate order, or the current row if none are marked.
- [x] **MUST:** The GUI's `Picker` implementation returns `Empty`, `Canceled`,
  or `Selected` with the same meaning as the TUI. Escape/Ctrl-C cancels the
  active pick; it does not kill the whole launcher process.
- [x] **MUST:** Command picks support Default/Run, Edit, Show, and Visit.
  Preserve Enter, Alt-Enter, F1, and F2 as keys and expose the actions visibly
  so GUI users need not know the bindings. Picker buttons follow the active
  action bindings: projects show Select/Inspect, history Replay/Show, and
  placeholders Select, without offering unsupported actions.
- [x] **MUST:** Command options declared in headings can be toggled before
  execution and during placeholder picks. Return the final toggle state from
  `pick_options`/`confirm`; do not silently accept defaults as the trait's
  default implementation does.
- [x] **MUST:** Project picks include the full path and support inspect.
  Placeholder picks support the declared single/multi behavior and display
  any producer error without losing the launcher window.
- [ ] **SHOULD:** History search, candidate color, and duplicate titles behave
  like the TUI. Duplicate titles remain separately selectable by stable ID.
  Search and identity are tested; GUI rows strip ANSI color rather than
  reproducing TUI candidate color.

## 6. Actions after selection

- [x] **MUST:** Run a selected command via Nixon's existing `App` flow. The
  GUI must not duplicate placeholder resolution, option argument placement,
  project environment variables, or history serialization.
- [x] **MUST:** Show displays command source in a GUI panel with a way back;
  it does not print invisible data to the launcher's stdout.
- [x] **MUST:** Edit opens a multiline GUI editor initialized from the source.
  Submitting runs the edited source with the original command's language,
  placeholders, options, and project; cancelling changes nothing. This
  replaces the terminal-only `nixon_picker::editor` for the GUI path.
- [x] **MUST:** Visit opens the command definition in the configured editor
  at its recorded line. Errors are shown in the GUI. The GUI must not assume
  `$EDITOR` names a graphical program; a terminal editor needs the configured
  terminal launcher.
- [x] **MUST:** An action that only returns data (`--select`, `--insert`,
  project path/inspect, history show) has an explicit GUI presentation or copy
  action. GUI behavior must not silently send useful output to stdout.
- [x] **MUST:** Launch every foreground Nixon command from the GUI in an
  external terminal with the command's prepared invocation and project cwd.
  The handoff preserves argv, environment, and any resolved stdin values.
  Use argument vectors and a configured terminal launcher; never concatenate
  unescaped source or append a language-specific pause command. Detached `&`
  commands keep their existing detached semantics.
- [x] **MUST:** If no terminal launcher is available, show an actionable
  error before running a foreground command. Do not drop its output into an
  invisible pipe. An explicitly terminal-free action may run detached.
- [x] **DECISION:** Do not resurrect v1's automatic foreground-vs-detached
  choice based on whether the picker is a GUI. `nixon --mode gui` has an
  explicit terminal execution policy; the command's `&` marker still wins.
- [x] **DECISION:** A successful terminal handoff counts as launch success;
  the GUI cannot claim the later child exit status after it closes. The
  terminal owns that status. Direct `nixon` CLI runs continue propagating the
  child status.
- [ ] **SHOULD:** Show a short “running” state and child exit result where the
  GUI remains open. No work that can block on I/O runs on the UI thread. The
  status bar shows pending work; terminal handoff owns the later child status.

## 7. Configuration and defaults

The new settings live in an existing `nixon.md` config block. At window
startup, the menu and browser search URL use built-in defaults, global config,
and the current project's local file. Launcher settings merge field by field:
local `terminal` and `search_url` replace inherited values when set, and
local `items` replaces the whole inherited menu when set. Absent fields
inherit. The menu stays fixed for that window; selecting another project
applies its local config to that project's commands, without inheriting the
startup project's launcher settings.

Proposed shape (names are part of this draft's contract):

````markdown
```yaml config
launcher:
  terminal: ["x-terminal-emulator", "-e"]
  search_url: "https://www.google.com/search?q={query}"
  items:
    - key: C
      label: Commands
      action: commands
    - key: P
      label: Projects
      action: projects
    - key: H
      label: History
      action: history
    - key: W
      label: Browser
      items:
        - key: O
          label: Open URL or search
          action: browser_input
    - key: S
      label: Spotify
      items:
        - key: Space
          label: Play/Pause
          action: { mpris: PlayPause, player: spotify }
        - key: P
          label: Previous
          action: { mpris: Previous, player: spotify }
        - key: N
          label: Next
          action: { mpris: Next, player: spotify }
    - key: E
      label: Editor
      action: { command: edit }
```
````

- [x] **MUST:** The built-in root and the PoC actions work when there is no
  global config file. A supplied `launcher.items` replaces the inherited menu
  tree, making it possible to omit Spotify or rearrange keys deliberately.
- [x] **MUST:** Test the right-biased, field-by-field launcher merge. An
  omitted local `items` keeps the global menu; an explicit empty `items` is
  rejected as an unusable root menu.
- [x] **MUST:** Parse menu items as typed variants. Each item has exactly one
  of `action` or `items`; an empty label, empty key, duplicate sibling key,
  unknown action, or invalid action arguments is a config error with context.
- [x] **MUST:** A `command` action may specify a project path or use the
  current project. Its name resolves with local commands at action time, so
  a local override takes precedence. A missing command is a visible error.
- [x] **MUST:** Keep existing unknown top-level config keys tolerated.
  Validate every key *inside* `launcher` strictly, where typos could turn a
  keyboard action into a different action.
- [x] **MUST:** Document terminal launcher argv and its search order:
  `launcher.terminal`, then `$TERMINAL` parsed as argv, then an available
  platform fallback. Report a missing launcher clearly.

## 8. Platform, window, and lifecycle

- [x] **MUST:** Support Linux X11 and Wayland sessions. Window creation must
  not require `XDG_SESSION_TYPE` to be set; let the window toolkit choose its
  available backend. A missing display produces a clean diagnostic. Linux
  display absence is diagnosed before window creation. The development and
  packaged binaries passed the Xvfb interaction smoke. A packaged Wayland
  check under pinned headless Weston verifies toplevel creation, configure,
  and rendered-buffer commit. A second check runs Weston nested in Xvfb and
  verifies the packaged Wayland client opens Commands, returns with Escape,
  and closes with Escape. `DISPLAY` and `XDG_SESSION_TYPE` are unset for the
  client. A real Wayland desktop session remains unverified.
- [x] **MUST:** Request a compact, keyboard-first, frontmost launcher window.
  Do not assume every window manager will honor positioning or always-on-top
  hints. Focus the first actionable control when the window opens.
- [x] **MUST:** Closing the window cancels an active pick and stops any
  streaming producer. A detached command already handed to the process runner
  remains detached. No orphaned GUI worker waits on an unanswered pick.
- [x] **MUST:** GUI errors appear in the window and on stderr when launched
  from a terminal. stdout remains reserved for explicit machine-readable
  output, as in the CLI. The Xvfb smoke script triggers disabled History and
  checks the visible error, stderr, and empty stdout in one launched process;
  it passed against the development and packaged Linux binaries on 2026-09-23.
- [x] **DECISION:** Linux is the first packaged GUI target. The user confirmed
  that the GUI launches on macOS; GUI colors follow light and dark system
  themes and adapt to changes while open (headless theme tests). MPRIS control
  is Linux-only and reports an error on macOS. A packaged macOS GUI and native
  media-control behavior remain unverified.
- [ ] **OUT OF SCOPE:** A resident daemon, global hotkey registration, tray
  icon, and display-server-specific window focus tricks. The desktop binds a
  shortcut to start `nixon --mode gui`.

## 9. Verification checklist

Tests should prove the behavior at the seam where it can fail. A screenshot
snapshot alone cannot prove that a command received the right cwd or that a
placeholder producer was cancelled.

- [x] **MUST:** Unit-test menu parsing and navigation: defaults, duplicate
  sibling keys, nested keys, text-field focus, Backspace/Escape, and invalid
  action shape.
- [x] **MUST:** Test GUI picker mapping with duplicate titles, ANSI in titles,
  initial query, exact/unique selection, multi marks across query changes,
  selection types, command option toggles, and cancellation. `nixon-gui` window
  tests exercise visible rows and input; bridge tests cover exact/unique
  selection before a window request.
- [x] **MUST:** Component-test a project -> command -> placeholder -> run
  flow with `ScriptedPicker`/`FakeRunner` or the GUI bridge's equivalents.
  Verify the final invocation, cwd, environment, option arguments, and
  history record.
- [x] **MUST:** Test Spotify success/failure behind a media-control seam
  without reaching real D-Bus.
- [x] **MUST:** Test browser URL/domain/search classification and a failed
  URL opener without reaching a real browser.
- [x] **MUST:** Test GUI terminal-launch argv with paths and arguments
  containing spaces or quotes, plus resolved stdin and environment values.
  Verify no command source is rewritten.
- [x] **MUST:** Component-test the Commands default action with a fake
  process runner: current-project config, terminal handoff, option and nested
  placeholder continuation, cancellation, detached `&`, and visible launch errors.
- [x] **MUST:** Smoke-test the packaged GUI under an available virtual
  display, including open, keyboard navigation, cancel, and a second nested
  pick. Run the established `nix flake check` gates for the CLI as well. A
  Linux Nix check exercises the wrapped binary and passed under Xvfb on
  2026-09-23, including the visible History error, stderr, and empty stdout.
  The full Linux `nix flake check` passed on 2026-09-23, including separate
  packaged Wayland startup and keyboard gates.
- [ ] **SHOULD:** Exercise X11 and Wayland in packaging/CI where runners
  support them; manual verification is recorded for any session type CI
  cannot provide. Development and packaged X11 passed under Xvfb. Packaged
  Wayland startup passed under headless Weston, and keyboard interaction
  passed with Weston nested in Xvfb. A manual desktop Wayland session record
  remains outstanding.

## 10. Implementation cuts

1. Add the typed launcher config and menu model, plus tests; no GUI code.
2. Add the `nixon-gui` event loop and picker bridge. Prove a GUI project and
   command pick can return a `Selection` to `App`.
3. Add GUI-safe Run/Edit/Show/Visit and terminal execution policy; prove a
   placeholder workflow end to end.
4. Add mnemonic menus and the PoC's browser/MPRIS actions.
5. Package `nixon --mode gui` for Linux, document keys/config, and run the
   functional smoke tests. The packaged Linux smoke gate passes, and the
   built-in key map is listed in the user guide.

No cut is complete if its user-facing path can only be used from a terminal.
