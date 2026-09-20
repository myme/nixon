# Nixon v2 — Behavioral Specification & Reimplementation Checklist

This document is a complete description of what the Haskell `nixon` (v0.1.0.0)
does today, derived from reading every module, test, config sample and packaging
file in the repository. It is written as a checklist for a Rust reimplementation
("Nixon v2"). Each `[ ]` item is a behavior that the reimplementation must
either reproduce or explicitly decide to change.

Source references use the form `Module.hs:line` and refer to the Haskell
codebase as of commit `3d14823`.

Legend for checklist items:

- **MUST** — core behavior; tests or documented usage depend on it.
- **SHOULD** — observable behavior with no test but users likely rely on it.
- **QUIRK** — accidental/undocumented behavior; v2 may want to fix it. Each
  quirk is called out so the decision is deliberate.

---

## 1. Purpose and overall shape

Nixon is a "project environment and command launcher". It:

1. Reads **markdown files** (`nixon.md` / `.nixon.md`) that contain
   configuration blocks and *commands* (a heading in inline code + a fenced
   code block).
2. Discovers **projects** on disk from configured `project_dirs`, classifying
   them by `project_types` (marker files).
3. Presents fuzzy **selection UIs** (fzf in the terminal, rofi as a GUI) for
   picking projects and commands.
4. Resolves **placeholders** — a command can reference another command as a
   source of selectable candidates, which are then passed as positional args,
   stdin or env vars.
5. **Evaluates** the command source by writing it to a cache file and running
   it with a language-specific interpreter, optionally wrapped in
   `direnv exec` or `nix-shell`, in a terminal or forked to the background.

Execution is a single-shot CLI process: parse args → load config → discover
projects → dispatch subcommand → exit.

---

## 2. CLI

Source: `Nixon/Config/Options.hs`, verified against the built binary's `--help`.

### 2.1 Global options (`Options.hs:137-187`)

> **v2** drops `-b/--backend`, `-T/--force-tty` and `-t/--terminal`
> (ENGINEERING.md §7.2). The rest is kept as-is.

All global options come **before** the subcommand.

| Flag | Type | Meaning |
|---|---|---|
| `-C, --config CONFIG` | path | Path to config file. Default: `$XDG_CONFIG_HOME/nixon.md` (shown in help with `~` collapsed via `implode_home`). |
| ~~`-b, --backend BACKEND`~~ | `fzf` \| `rofi` | v1 only. Selection backend. Any other value is a parse error. |
| `-e, --exact` / `--no-exact` | tri-state | Exact matching in the selector. |
| `-i, --ignore-case` / `--no-ignore-case` | tri-state | Case-insensitive matching. |
| ~~`-T, --force-tty` / `--no-force-tty`~~ | tri-state | v1 only. Never fork/spawn; run in the current TTY. |
| `-p, --path PATH` | path, repeatable | Extra project directory (appended to `project_dirs`). |
| `-d, --direnv` / `--no-direnv` | tri-state | Wrap in `direnv exec`. |
| `-n, --nix` / `--no-nix` | tri-state | Wrap in `nix-shell`. |
| ~~`-t, --terminal TERMINAL`~~ | text | v1 only. Terminal emulator for non-GUI commands. |
| `-L, --loglevel LOGLEVEL` | `debug`\|`info`\|`warning`\|`warn`\|`error` | Log level. |
| `-h, --help` | | Help. |

- [ ] MUST: Tri-state flags parse to `Some(true)` / `Some(false)` / `None`
      (`maybeSwitch`, `Options.hs:122-135`). The `--no-*` form has no short
      flag and no help text.
- [ ] MUST: `--path` is repeatable; each occurrence appends to `project_dirs`.
- [ ] MUST: `bin_dirs`, `project_types`, `commands` are **not** CLI-settable.
- [ ] SHOULD: Help header text is `Command & environment launcher`.
- [ ] QUIRK: `--terminal` help says "Terminal emultor" (typo). Fix in v2.
- [ ] MUST: The default config path shown in help is computed at runtime
      from XDG and `$HOME`.

### 2.2 Subcommands (`Options.hs:142-155`)

```
nixon [global-opts] (edit | eval | gc | new | project | run | <run-args>)
```

- [ ] MUST: If no subcommand keyword is given, the remaining arguments are
      parsed as the `run` subcommand (`nixon foo bar` ≡ `nixon run foo bar`).
      This is the last alternative in the parser, so `nixon edit` is the
      `edit` subcommand, never a command named "edit".

#### `run [command] [args...] [-i|--insert] [-l|--list] [-s|--select]`

- `command` (optional positional, shell-completed with command names).
- `args...` (any number of positionals) — arguments to the command.
- `-i, --insert` — "Select a command and output its source".
- `-l, --list` — "List commands".
- `-s, --select` — "Output command selection on stdout".

#### `project [project] [command] [args...] [-i|--insert] [-I|--inspect] [-l|--list] [-s|--select]`

- `project` (optional positional, completed with project names).
- `command` (optional positional, completed with command names).
- `args...`.
- `-i, --insert` — "Select a project command and output its source".
- `-I, --inspect` — "Select a project and list some info about it".
- `-l, --list` — "List projects".
- `-s, --select` — "Select a project and output on stdout".

#### `eval ((-f|--file FILE) | command) [placeholder...] [-l|--language LANGUAGE] [-p|--project]`

- Exactly one of `-f FILE` or a positional `command` expression.
- `placeholder...` — zero or more positionals, each parsed with the
  placeholder grammar (§5.3), e.g. `'${git-files:m}'`. A parse failure is a
  CLI error.
- `-l, --language` — language name (§7.1); any string accepted (unknown →
  `Unknown`).
- `-p, --project` — interactively select project instead of using cwd.

#### `new [-n|--name NAME] [-d|--desc DESC] [-l|--lang LANG] [-s|--src SRC]`

Defaults: name `<name>`, desc `Description…`, lang `bash`, src `""`.

#### `edit [command]`

- `command` optional positional, completed with command names.

#### `gc [-d|--dry-run]`

### 2.3 Argument/config merge order (`Options.hs:259-269`, `Nixon.hs:276-281`)

- [ ] MUST: Effective config = `userConfig <> fileConfig <> cliConfig` where
      `<>` is the `Config` semigroup (§3.3). `userConfig` is `defaultConfig`
      for the binary (it's a library hook for embedding).
      Note: `parseArgs` returns `fileConfig <> cliConfig`; `nixonWithConfig`
      then computes `userConfig <> that`.
- [ ] MUST: The config file is read *after* CLI parsing (so `--config` is
      honored). Errors from reading the config file:
      - file missing → `NoSuchFile` → fatal error, exit 1
        (`die` prints `NoSuchFile` via `show`).
      - file empty/whitespace → `EmptyFile` → fatal, exit 1.
      - parse error → `ParseError <msg>` → fatal, exit 1.
- [ ] QUIRK (verified): A missing global config file is a hard error, not
      a silent default: `nixon -C /nonexistent.md run -l` prints exactly
      `NoSuchFile` to stderr and exits 1. Parse errors print
      `ParseError "<msg>"` (with the Haskell `show` quoting). v2 should print
      friendly messages and probably tolerate a missing global file.

### 2.4 Shell completion (`Options.hs:271-280`, `Nixon.hs:300-341`)

optparse-applicative provides:

- [ ] SHOULD: `nixon --bash-completion-script <path>` and
      `--zsh-completion-script <path>` (also `--fish-completion-script`) emit
      completion scripts. The Nix package installs these to
      `share/bash-completion/completions/nixon.bash` and
      `share/zsh/site-functions/_nixon_completion`.
- [ ] SHOULD: Completion protocol: `nixon --bash-completion-index N
      --bash-completion-word w0 --bash-completion-word w1 ...` prints
      candidates, one per line. (`--bash-completion-enriched` is used by
      zsh.)
- [ ] MUST: Dynamic completers (`nixonCompleter`):
  - `edit <command>` and `run <command>`: completes with command names of the
    project resolved from cwd (or from `project <name>` if the args start
    with `project <p>` and a project of that name exists among discovered
    projects — falls back to cwd project). Applies local `nixon.md`
    (`withLocalConfig`).
  - `project <project>`: completes with discovered project names
    (basename only).
  - `project <p> <command>`: same as `run` completer with the named project.
  - `eval`: no completions.
- [ ] SHOULD: The completer re-parses the completion words as a normal CLI
      invocation to pick up `-C`, `-p` etc. (so `nixon -C foo.md run <TAB>`
      completes against `foo.md`).

---

## 3. Configuration

### 3.1 Config file discovery

- [ ] MUST: Global config: `$XDG_CONFIG_HOME/nixon.md` (default
      `~/.config/nixon.md`), overridable by `-C`. (`Markdown.hs:67-68`)
- [ ] MUST: Local (project) config: `firstOf (find_dominating_file path)
      ["nixon.md", ".nixon.md"]` — the **entire** ancestor chain is searched
      for `nixon.md` first, and only if none exists anywhere is the chain
      searched again for `.nixon.md`. So a `nixon.md` three directories up
      beats a `.nixon.md` next to the project. (`Config.hs:20-27`,
      `Utils.hs:62-70`.) `find_dominating_file` requires the starting path
      to be a directory; stops at filesystem root. Kept in v2 (ENGINEERING
      §7.4), tested.
- [ ] MUST: Local config is merged on top of the global config
      (`env.config <> local`) for the duration of command discovery/handling
      (`Find.hs:120-124`, applied in `findCmd`, `findAndHandleCmd`, and the
      completer).
- [ ] MUST: Local config errors: `ParseError` is re-thrown (fatal);
      `NoSuchFile`/`EmptyFile` are treated as "no local config".
- [ ] QUIRK: Local config is found relative to the *project root*, walking
      upward — so a `nixon.md` in a parent dir of the project also applies
      (but only the first one found, not all ancestors).

### 3.2 Config fields (`Config/Types.hs:23-37`)

| Field | Type | From file | From CLI | Default |
|---|---|---|---|---|
| `backend` | `Option<Fzf\|Rofi>` | no | `-b` | auto (§8.1) |
| `bin_dirs` | `Vec<Path>` | yes | no | `[]` |
| `exact_match` | `Option<bool>` | yes | `-e` | `None` |
| `ignore_case` | `Option<bool>` | yes | `-i` | `None` |
| `force_tty` | `Option<bool>` | no | `-T` | `None` |
| `project_dirs` | `Vec<Path>` | yes | `-p` | `[]` |
| `project_types` | `Vec<ProjectType>` | yes | no | `[]` |
| `commands` | `Vec<Command>` | yes (parsed) | no | `[]` |
| `use_direnv` | `Option<bool>` | yes | `-d` | `None` |
| `use_nix` | `Option<bool>` | yes | `-n` | `None` |
| `terminal` | `Option<String>` | yes | `-t` | `None` |
| `loglevel` | `Option<LogLevel>` | no | `-L` | `Some(Warning)` |

- [ ] QUIRK: `force_tty` and `backend` and `loglevel` are **not** readable
      from the config file (`JSON.hs` has no such fields). v2 should probably
      allow them.
- [ ] QUIRK: `terminal` is parsed from JSON/YAML (`JSON.hs:50`) but
      `buildConfig` (`Markdown.hs:70-82`) **drops it** — it never reaches
      the effective config. Only `-t` works. v2 should fix this.
- [ ] QUIRK: Markdown-derived config sets `loglevel = None` explicitly; the
      default `Some(Warning)` comes from `defaultConfig` on the left of the
      merge.

### 3.3 Config merge semantics (`Config/Types.hs:39-54`)

`lhs <> rhs`:

- [ ] MUST: `Option` fields: `rhs` wins if `Some`, else `lhs`
      (`backend, exact_match, ignore_case, force_tty, use_direnv, use_nix,
      terminal, loglevel`).
- [ ] MUST: `bin_dirs`, `project_dirs`, `project_types`: `lhs ++ rhs`
      (concatenation, no dedupe).
- [ ] MUST: `commands`: `rhs ++ lhs` — **local/rhs commands come first**
      (so they shadow global ones in later "first match by name" lookups,
      §5.5).

### 3.4 Config block syntax inside markdown (`Markdown.hs`)

A config is a fenced code block whose contents are JSON or YAML. Two ways to
mark it:

1. A heading with the `.config` attribute: `# Config {.config}` followed
   (after optional paragraphs are *skipped*? — no, see below) by a code
   block.
2. A code block whose info string contains the word `config` after the
   language: ```` ```yaml config ```` or ```` ```json config ```` or
   ```` ``` json  config ````.

- [ ] MUST: Heading form: the node immediately following the heading (in the
      flattened node list, §4.2) **must** be a `Source` node; otherwise error
      `Expecting config source after header`. (Paragraphs between heading and
      code block are NOT tolerated in the heading form — `parseConfig`
      pattern-matches the very next node. Note: in practice a paragraph
      between them makes it fail.)
- [ ] MUST: Language of the config block: `json` → JSON; no language → JSON;
      `yaml` → YAML; anything else → error `Invalid config language: <lang>`.
- [ ] MUST: JSON parse errors and YAML parse errors are reported as
      `ParseError` with the underlying parser message.
- [ ] MUST: Exactly one config block per file; a second one →
      `Found multiple configuration blocks` (both heading form and info-string
      form count).
- [ ] MUST: An empty JSON object `{}` is valid and yields all-default config.
- [ ] MUST: A config block is allowed anywhere, including between commands;
      it does not disturb command location tracking (test "finds location
      (with config)").

JSON/YAML schema (`Config/JSON.hs`):

```yaml
bin_dirs: [string]        # optional, default []
exact_match: bool         # optional
ignore_case: bool         # optional
project_dirs: [string]    # optional, default []; each may contain ~ or $VAR (wordexp)
project_types:            # optional, default []
  - name: string          # required
    test: [string]        # optional, default []; each is a path relative to project root
    desc: string          # required
use_direnv: bool          # optional
use_nix: bool             # optional
terminal: string          # optional (currently dropped, see quirk)
```

- [ ] MUST: Unknown keys are ignored.
- [ ] MUST: `project_types[].desc` is **required** (missing → parse error).
      `name` required. `test` optional.
- [ ] MUST: `test` entries become `ProjectPath` markers (exists as file OR
      dir). (`ProjectFile`/`ProjectDir`/`ProjectOr`/`ProjectFunc` exist in
      the type but are only constructible from Haskell code, not config.)

---

## 4. Markdown parsing (`Nixon/Config/Markdown.hs`)

### 4.1 Overview

Markdown is parsed with **cmark** (CommonMark). The tree is flattened into a
list of nodes (`Head`, `Source`, `Paragraph`, `End`) and then a small state
machine walks the list to extract commands and the config.

- [ ] MUST: Use a CommonMark-compliant parser (Rust: `pulldown-cmark` or
      `comrak`). Source positions (line numbers) are required for command
      locations.
- [ ] MUST: Both ATX (`# foo`) and setext (`foo\n===`) headings work
      (test "supports alternate header format").

### 4.2 Node flattening (`extract`, `Markdown.hs:89-114`)

For each cmark node:

- `HEADING level` → `Head(pos, level, name, attrs)` where the heading text is
  `getText children` and then parsed by `parseHeaderArgs` (§4.3).
  - `isCommand` = the heading contains at least one inline `CODE` child.
  - `isBg` = the **first** inline code child's text, stripped, ends with `&`.
  - Implicit args added: `bg` if `isBg` (and not already present),
    `command` if `isCommand` (and not already present), then explicit args.
- `CODE_BLOCK info text` → `Source(lang, attrs, text)` where `info` is split
  on whitespace: first word → `parseLang` (§7.1), rest → attrs (raw words).
  If info is empty → `Lang::None`, attrs `[]`.
- `PARAGRAPH` → `Paragraph(getText children)`.
- Anything else (document root, lists, block quotes, …) → recurse into
  children, then append an `End(pos')` node where `pos'.startLine =
  pos.endLine + 1`. For the document root this yields a trailing `End` with
  start line = last line + 1, used to close the last command's range.

`getText` (`Markdown.hs:142-152`): concatenates `TEXT`, `HTML_BLOCK`,
`HTML_INLINE`, `CODE`, `CODE_BLOCK` node texts recursively, joining with
single spaces and stripping. Emphasis/links contribute only their text.

- [ ] MUST: Heading text for name purposes is the concatenation of all text
      + inline code, e.g. `# \`hello\` {type="git"}` → text
      `hello {type="git"}`.
- [ ] QUIRK: `getText` joins `[own text, children, siblings]` with single
      spaces and only strips the ends, so internal runs of two spaces occur:
      `` # `hello` {type="git"} `` → `hello  {type="git"}`. Invisible
      downstream (name is trimmed, first word taken). v2 reproduces it.

### 4.3 Heading attribute syntax (`parseHeaderArgs`, `Markdown.hs:120-140`)

Grammar (Pandoc-style attribute block):

```
header   := name '{' (arg | kwarg) (spaces (arg | kwarg))* '}'   -- or just name
name     := <any chars except '{'>  (stripped)
arg      := '.' identifier                  -- e.g. .command .bg .config
kwarg    := identifier '=' ( '"' letters '"' | letters )   -- e.g. type="git" or type=git
identifier := (letter | '_' | '-')+
letters  := letter+
```

- [ ] MUST: If parsing fails entirely, the whole text is the name with no
      args (`Left _ -> (input, [], [])`).
- [ ] MUST: Tests (`Markdown.hs` tests `parse_header_tests`):
  - `"some header"` → `("some header", [], [])`
  - `"some header {.some-arg some-kw=\"value\"}"` → `("some header", ["some-arg"], [("some-kw","value")])`
  - `"{.bg}"` → `("", ["bg"], [])`; likewise `.config`, `.command`, `.json`
  - `"{type=git}"` and `"{type=\"git\"}"` → `("", [], [("type","git")])`
  - `"{.command type=\"git\"}"` → `("", ["command"], [("type","git")])`
- [ ] QUIRK: kwarg values are `letters` only — `type="my-type"` or
      `type="c++"` fail to parse (whole heading falls back to no-attrs). v2
      should accept a broader value charset (at least `[A-Za-z0-9_-]` and
      anything inside quotes).
- [ ] QUIRK: A trailing `{...}` in the heading is parsed *after* the name
      even when the heading is a command (the name still goes through
      `parseCommandName`, §4.5).

### 4.4 Command / config state machine (`parse`, `Markdown.hs:172-223`)

**v2 model (implemented):** walk the flattened nodes with a **stack of
`(level, types)` frames**, one per open heading. On any `Head(level, …)`,
pop every frame with `frame.level >= level`, then push
`(level, kwargs["type"])`. A command's project types are the concatenation
of the `type` kwargs of every frame on the stack, innermost first. This
gives: sibling headings don't inherit from each other; nested headings
inherit from all ancestors; and — the v1 bug fixed — `type` on a
*non-command* section heading applies to the commands beneath it.

Then per node:

- `Head` with `config` in args → `parseConfig(rest)`: the very next node must
  be a `Source`, else `Expecting config source after header`. A second
  config anywhere → `Found multiple configuration blocks`.
- `Head` with `command` in args → `parseCommand` (§4.5) with the stack's
  types and `isBg = "bg" in args`; record the heading's position for the
  location (§4.6).
- `Source` with `config` in attrs → config block, same duplicate rule.
- `End`/next `Head` → closes the previous command's location.
- Everything else is skipped.

**v1 behaviour, for reference:** v1 kept a single `(headerLevel,
projectTypes, lastPos)` state. Rule "level < headerLevel → reset types and
reprocess" existed only to clear inherited types; command headings updated
the state (`types = own ++ parent`, parent = `[]` if same level), but
**non-command headings did not touch it**, which is why section-level
`type="…"` was silently ignored (verified against the binary). The stack
subsumes the reset rule and every ported test passes unchanged.

- [ ] MUST: Commands are returned in document order.
- [ ] MUST (verified against the binary): `type="…"` on a **command**
      heading applies to that command and to command headings nested under
      it until a heading of the same or higher level.
- [ ] MUST (v2 fix of a verified v1 BUG): `type="…"` on a non-command
      section heading (`## Git stuff {type="git"}` → `### \`git-files\``)
      applies to the commands beneath it. Tested.
- [ ] MUST: Header level gaps are fine (`##` then `####` are both top-level
      commands; test "can bump header level gaps").

### 4.5 Command parsing (`parseCommand`, `Markdown.hs:265-292`)

Given the nodes after a command heading:

- Zero or more `Paragraph` nodes are consumed; the **first** one becomes
  the description (`parseCommand` recurses and applies `description` on the
  way back out, so the outermost/first paragraph is applied last and wins).
  - [ ] MUST (verified): description = **first** paragraph after the
        heading, stripped; later paragraphs before the code block are
        ignored.
  - [ ] QUIRK (verified): only paragraphs are tolerated between the heading
        and the code block. A list, blockquote, table, etc. there makes the
        whole file fail with `Expecting source block for <name>`. v2: skip
        any non-code-block content, take the first paragraph as description.
- The next node must be `Source(lang, attrs, src)`; otherwise error
  `Expecting source block for <name>`.
- `parseCommandName(name)` (§4.5.1) → `(name', headerPlaceholders)`.
- `attrs` (code block info words after the language) are joined with spaces
  and parsed with `parseCommandArgs` → `sourcePlaceholders`.
- [ ] MUST: If both header and source placeholders are non-empty → error
      `<file>:<line> <name> uses placeholders in both command header and source code block`.
- [ ] MUST: `placeholders = headerPlaceholders ++ sourcePlaceholders`.
- [ ] MUST: `cmdIsHidden = name starts with "_"`.
- [ ] MUST: `cmdSource` is the raw code block content **including the
      trailing newline** (test expects `"echo Hello World\n"`).
- [ ] MUST: `cmdLang` from the code block info string; `None` if absent.
- [ ] MUST: `cmdProjectTypes` = computed `pt` from §4.4.

#### 4.5.1 `parseCommandName` (`Markdown.hs:294-302`)

```
spaces, then either EOF → ("", []) or:
name  := 1+ non-whitespace chars
args  := parseCommandArgs on the remainder
```

`parseCommandArgs` scans the rest of the string char-by-char, collecting every
occurrence that parses as a placeholder (§5.3) and skipping everything else.

- [ ] MUST: name is the first whitespace-delimited word: `` `hello ${foo}` ``
      → `hello`; `` `echo 'foo bar baz'` `` → `echo`.
- [ ] MUST: Leading spaces are ignored.
- [ ] MUST: Trailing `&` (background marker) is simply ignored by the name
      parser (it's not a placeholder) — `hello ${arg} ${another-arg} &` →
      name `hello`, 2 placeholders, `isBg = true` (from heading detection).
- [ ] MUST: `$` / `<` not followed by `{` are ignored (`echo $SOME_VAR` →
      no placeholders).
- [ ] MUST: Unterminated placeholder (`cat "${arg"`) → parse error (fails
      the whole file parse with the parsec error message).
- [ ] MUST: Placeholders can be embedded in quotes: `cat "${arg}"` → one
      `Arg` placeholder.

### 4.6 Command locations (`addLocation`, `Markdown.hs:225-237`)

Each markdown command gets a `CommandLocation`:

```
file_path: the config file path (as given to parseMarkdown)
start_line: line of the command's heading (1-based, from cmark)
end_line:   (start line of the next Head/End node) - 1
level:      heading level of the command heading
```

- [ ] MUST: Only set once per command (first `addLocation` wins).
- Note: the Haskell defaults (`start_line = -1`, `end_line = -1`) apply only
  when a node has no position, which cannot happen for the document root;
  v2 uses `usize` with saturating subtraction and never observes them.
- [ ] MUST: Test expectations:
  - Single command at lines 1–4 (`# foo`, fence, body, fence) →
    `Loc("some-file.md", 1, 4, 1)` (document `End` is line 5 → end 4).
  - Three commands `# foo`(1) / `## bar`(6) / `# baz`(11), blank lines
    between → `(1,5,1)`, `(6,10,2)`, `(11,14,1)`.
  - With a `# Config` + yaml block between `bar` and `baz` → `bar` ends at
    line 10 (the `# Config` heading is line 11 → end 10) and `baz` is
    `(17,20,1)`.
- [ ] MUST: `end_line` includes trailing blank lines up to the next heading.
- [ ] MUST: Bin-dir commands (§5.5) get `Loc(path, 0, 0, 0)`.

### 4.7 Errors surface as

- [ ] MUST: All parse errors → `ConfigError::ParseError(String)`; the global
      config → fatal exit 1; local config → fatal exit 1 too (re-thrown).

---

## 5. Commands and placeholders

### 5.1 Command model (`Nixon/Command.hs`)

```
Command {
  name: String,
  desc: Option<String>,
  lang: Language,
  project_types: Vec<String>,   // empty = applies to every project
  source: String,
  pwd: Option<Path>,            // only set by `eval`
  placeholders: Vec<Placeholder>,
  is_bg: bool,
  is_hidden: bool,              // name starts with '_'
  location: Option<CommandLocation>,
}
```

Display forms:

- [ ] MUST: `show_command` = `name` followed by ` ${<placeholder name>}` for
      each placeholder, space-separated. Used as the selector header/title
      when resolving placeholders (§5.6).
- [ ] MUST: `show_command_with_description` = `name` or `name - desc`.
      Used as the candidate text in command selection and `--list` output.
- [ ] QUIRK: `is_bg_command` is dead code (`const False`).

### 5.2 Placeholder model (`Nixon/Command/Placeholder.hs`)

```
PlaceholderType = Arg | EnvVar(name) | Stdin
PlaceholderFormat = Lines | Fields(Vec<usize>) | Columns{has_header: bool, cols: Vec<usize>} | JSON
Placeholder {
  type_: PlaceholderType,
  name: String,            // referenced command name
  format: PlaceholderFormat,   // default Lines
  filter: Option<String>,  // default None
  list: bool,              // default false
  multiple: bool,          // default false
  value: Vec<String>,      // pre-expanded value; default empty
}
```

### 5.3 Placeholder grammar (`parseCommandPlaceholder`, `Markdown.hs:316-407`)

```
placeholder := start '{' name modifiers? '}'
start       := '<'                 -- Stdin
             | '$'                 -- Arg
             | alias '='           -- EnvVar(alias); alias := [A-Za-z0-9_]*  (may be empty)
name        := 1+ chars not in " :|}"
modifiers   := pipe-modifiers | colon-modifiers

pipe-modifiers  := ( spaces '|' spaces pipe-mod spaces )+
pipe-mod        := 'cols' ('+h')? spaces fields      -- Columns{has_header, fields}
                 | 'fields' spaces fields             -- Fields
                 | 'json'                             -- JSON
                 | 'filter' spaces '"' alnum* '"'     -- filter
                 | 'list'                             -- list = true
                 | 'multi'                            -- multiple = true
fields          := digits (',' digits)*

colon-modifiers := ':' ( fields 'm'? | 'm' fields? )   -- Fields + multiple, either order
```

- [ ] MUST: EnvVar naming: if alias is empty (`={git-files}`) the env var
      name is the command name with `-` → `_` (`git_files`). If an alias is
      given (`FILES={git-files}`) it's used verbatim (the alias grammar is
      `[A-Za-z0-9_]*`, so it can never contain `-`).
- [ ] MUST: Setting a format twice (`| cols 1 | fields 2`) → error
      `Placeholder format already set`. (Only checked in `parseFields`, so
      `| json | fields 1` also errors, but `| fields 1 | json` silently
      overrides to JSON. QUIRK — v2 should reject any second format.)
- [ ] MUST: Field/column numbers are **1-based**.
- [ ] MUST: Test vectors (from `parse_command_name_tests` and
      `command_tests`):

  | Input | type | name | format | filter | list | multi |
  |---|---|---|---|---|---|---|
  | `${arg}` | Arg | arg | Lines | – | f | f |
  | `<{arg}` | Stdin | arg | Lines | – | f | f |
  | `={arg}` | EnvVar("arg") | arg | Lines | – | f | f |
  | `FOO={bar}` | EnvVar("FOO") | bar | Lines | – | f | f |
  | `={some-arg}` | EnvVar("some_arg") | some-arg | Lines | – | f | f |
  | `some_arg={some-arg}` | EnvVar("some_arg") | some-arg | Lines | – | f | f |
  | `<{arg:1}` | Stdin | arg | Fields[1] | – | f | f |
  | `<{arg:1,3,5}` | Stdin | arg | Fields[1,3,5] | – | f | f |
  | `${arg:m}` | Arg | arg | Lines | – | f | t |
  | `<{arg:m1,3,5}` | Stdin | arg | Fields[1,3,5] | – | f | t |
  | `<{arg:1,3,5m}` | Stdin | arg | Fields[1,3,5] | – | f | t |
  | `${arg \| fields 1,3}` | Arg | arg | Fields[1,3] | – | f | f |
  | `${arg \| multi}` | Arg | arg | Lines | – | f | t |
  | `<{arg \| fields 1,3,5 \| multi}` | Stdin | arg | Fields[1,3,5] | – | f | t |
  | `${arg \| list}` | Arg | arg | Lines | – | t | f |
  | `${arg \| filter "filter"}` | Arg | arg | Lines | "filter" | f | f |
  | `${placeholder \| cols 1}` | Arg | placeholder | Columns{f,[1]} | – | f | f |
  | `${placeholder \| cols+h 1}` | Arg | placeholder | Columns{t,[1]} | – | f | f |
  | `${placeholder \| json}` | Arg | placeholder | JSON | – | f | f |

- [ ] QUIRK: `filter "…"` only accepts alphanumerics inside the quotes. v2
      should accept any chars except `"`.
- [ ] QUIRK: Pipe modifiers require `|` to be preceded by the name directly
      or by spaces; `${a|multi}` works (zero spaces allowed).

### 5.4 Placeholder sources: header vs code block

- [ ] MUST: Placeholders may be written in the heading (`` ### `vim ${git-files}` ``)
      **or** in the code-block info string after the language
      (```` ```bash ${git-files} ````), never both (error §4.5).
- [ ] MUST: Multiple placeholders are allowed in one place, e.g.
      ```` ```bash <{git-files | multi} <{rg-files:m} ````.

### 5.5 Command discovery for a project (`Find.hs:49-77`)

`findProjectCommands(project)`:

1. Take `config.commands` (already merged global+local, local first).
2. Keep a command if `cmd.project_types` is empty **or** intersects the
   project's detected type ids.
3. Add **bin commands**: for each `dir` in `config.bin_dirs`, list
   `project_path/dir` and for every *regular file with an execute bit*
   (non-recursive) create `Command { name: basename, source: "<full path>
   \"$@\"", location: Loc(path,0,0,0), lang: None, … }`. Non-existent dirs
   yield nothing.
   - [ ] BUG (v1): turtle's `lsif` uses its predicate only to decide whether
         to *descend* into subdirectories and emits every entry regardless,
         so v1 actually offered every file in `bin/` (executable or not),
         every subdirectory as a command, and everything under traversable
         subdirectories. `bin/README` became a command that cannot run. v2
         implements the intent above (ENGINEERING §7.3).
4. **Sort by name** (stable, so for equal names local-before-global order
   from the merge is preserved).

- [ ] MUST: Tests: empty config → `[]`; config commands with no types →
      returned; command with type `bar` and project without types →
      filtered out; command with type `bar` and project with type `bar` →
      kept.
- [ ] MUST: Hidden commands (`_name`) are included by `findProjectCommands`
      (so placeholders can reference them) but **excluded** from the
      interactive selection in `findAndHandleCmd` (`Find.hs:95`). They are
      *not* excluded from `findCmd` (used by `edit` and `new`) nor from
      `--list` output.
- [ ] MUST: Placeholder resolution finds the referenced command by exact
      name among `findProjectCommands(project)` (`Run.hs:93-95`) — first
      match; a missing name is a **panic/`error`**:
      `Invalid argument: <name>`. v2: proper error.

### 5.6 Placeholder resolution (`Nixon/Command/Run.hs`)

`runCmd(selector, project, cmd, args)`:

1. `cd project_path` (process-wide chdir!) before invoking the selector.
   [QUIRK: global chdir; v2 should pass cwd explicitly.]
2. `resolveEnv` → `(stdin: Option<lines>, args: Vec<String>, env: Vec<(k,v)>)`.
3. `pwd = cmd.pwd.or(project_path)`.
4. `evaluate(cmd, args, pwd, env, stdin)` (§7.3).

`resolveEnv(project, selector, cmd, args)`:

- `env` always starts with `("nixon_project_path", project_path)`.
- `zipArgs(placeholders, args)` pairs placeholders with CLI args:
  - [ ] MUST: For each `(placeholder, arg)` pair, the arg becomes the
        **search query** for that placeholder's selector (`Select.search a`)
        — it does *not* bypass selection, it pre-fills the query
        (fzf `--query`, rofi `-filter`). With fzf's `-1` (select-1) flag, a
        unique match is auto-selected.
  - [ ] MUST: Extra args beyond the placeholders become overflow
        placeholders of type `Arg` with `value = [arg]` (pre-expanded, no
        selection) → appended as positional args.
  - [ ] MUST: Placeholders beyond the args get default selector opts.
- For each `(placeholder, opts)` in order (fold, left to right):
  - If `placeholder.value` is non-empty → use it directly.
  - Else:
    - Find the referenced command (`assertCommand`).
    - If `placeholder.list` is true, the selector is replaced by
      **fzf in `--filter` mode** with the search query (or `""`) — i.e.
      non-interactive: print all candidates matching the query. This ignores
      the configured backend.
    - Selector opts: `format = placeholder.format`, `multiple =
      Some(placeholder.multiple)`, `search = query`.
    - `resolveCmd(project, selector, referencedCmd, opts)` → `Vec<String>`
      (recursively resolves the referenced command's own placeholders
      first, with **no** args).
  - Then by type:
    - `Stdin` → concatenated to any previous stdin lines (multiple stdin
      placeholders concatenate, in order).
    - `Arg` → each resolved line appended as a separate positional argument.
    - `EnvVar(name)` → `env.push((name, resolved.join(" ")))` (space-joined
      into one variable).

`resolveCmd(project, selector, cmd, opts)`:

1. Recursively `resolveEnv` for `cmd` (its own placeholders), args `[]`.
2. Run the command with output captured (§7.2 `getEvaluator` with
   `run_with_output`) in `project_path`, with env, stdin.
3. Build candidates from output according to `opts.format`:
   - `Lines` → each output line is `Identity(line)` (title = value).
   - `Fields(ns)` → for each line: title = full line, value =
     `pickFields(ns, line.split_whitespace()).join(" ")`.
   - `Columns{has_header, cols}` → `formatColumns` (§6): title = full
     original row (header row dropped if `has_header`), value = selected
     columns joined by spaces.
   - `JSON` → parse whole stdout as JSON array of candidates, where each
     element is either a string (`Identity`) or `{"title": …, "value": …}`.
     Parse failure → `error` (panic). v2: proper error.
4. Invoke the selector with the candidates.
5. `Selection(_, values)` → return values; `Empty`/`Canceled` → **panic**
   `Argument expansion aborted`. v2: propagate as a clean "canceled" exit.

- [ ] MUST: Env vars set for nested placeholder commands include
      `nixon_project_path` too.
- [ ] MUST: The selector title (fzf `--header` / rofi `-p`) during
      placeholder resolution is `show_command(cmd)` of the *outer* command,
      e.g. `vim-file ${git-files}`.
- [ ] SHOULD: The cwd for the referenced command execution is the project
      path (`cmd.pwd` is ignored for placeholder commands — always project
      path).

---

## 6. Column / field formatting (`Nixon/Format.hs`)

`parseColumns(has_header, rows) -> Vec<Vec<String>>`:

- Column widths are computed from the **first row** (header if
  `has_header`, else first data row): each column = a run of non-space chars
  + the following run of spaces; widths = lengths of those spans.
- Each row is split at those cumulative widths; each cell is stripped; the
  last column takes the rest of the line.
- If `has_header`, the header row is not included in the output rows.
- [ ] MUST: Tests:
  - `[]` → `[]` (both modes).
  - Header only, `has_header=true` → `[]`; `has_header=false` →
    `[["NAME","UUID","TYPE","DEVICE"]]`.
  - The `nmcli connection` sample parses `"My Wifi"` as one cell (spaces
    inside a cell are preserved because splitting is positional).
- [ ] QUIRK: Widths come from the first row; if a later row is wider in a
      column the cells shift. Accept as-is (matches `column`-style output).

`formatColumns(has_header, cols, rows) -> Vec<(title, value)>`:

- titles = rows minus header (if `has_header`), values = selected columns
  (1-based, in order of appearance not order of `cols`) joined by `" "`.
- [ ] MUST: Regression test: no-header mode keeps every row aligned
      (commit `e1b47a2`).

`pickFields(ns, words)` / `pickColumns(ns, cols)`:

- [ ] MUST: Keep items whose 1-based index ∈ `ns`, preserving original
      order (not the order in `ns`). Out-of-range indices ignored.

---

## 7. Languages, evaluation and process handling

### 7.1 Languages (`Nixon/Language.hs`)

| Language | info-string names | file ext (`fromFilePath`) | cache ext | interpreter argv |
|---|---|---|---|---|
| Bash | `sh`, `bash` | `.sh` | `.sh` | `bash` |
| Haskell | `haskell` | `.hs` | `.hs` | `runghc` |
| JavaScript | `js`, `javascript` | `.js` | `.js` | `node` |
| JSON | `json` | `.json` | `.json` | `jq -r .` |
| Plain | `plain` | `.txt` | `.txt` | `cat` |
| Python | `python` | `.py` | `.py` | `python3` |
| YAML | `yaml` | – | `.yaml` | `yq -r .` |
| None | `` (empty) | – | `.sh` | `$SHELL` if set, else `bash` |
| Unknown(s) | anything else | any other ext `x` → `Unknown("x")`; no ext → `Unknown("")` | `.txt` | **none** → fatal `No interpreter for <s>` |

- [ ] MUST: `Display` of a language is the lowercase name; `Unknown(s)` →
      `s`; `None` → `""`. Used in `new` template (```` ```bash ````).
- [ ] MUST: Language matching is case-sensitive (`Bash` → Unknown).
- [ ] SHOULD: `--language` on `eval`/`new` accepts the same names.

### 7.2 Script cache (`Nixon/Evaluator.hs:51-81`)

- [ ] MUST: Cache dir: `$XDG_CACHE_HOME/nixon` (default `~/.cache/nixon`),
      created with `mkdir -p`.
- [ ] MUST: Every evaluation writes the command source to
      `<cache>/<sha1-hex-of-source>-<name><ext>` and runs the interpreter on
      that path. Positional args follow the path. (`name` is empty for
      `eval`, giving `<sha1>-.sh` — verified via `gc --dry-run`.)
- [ ] MUST: `gc` removes every file in the cache dir and prints
      `removed <path>` per file; `--dry-run` prints `would remove <path>`.
      Output goes to stdout.
- [ ] QUIRK: The script file is never made executable; interpreter is always
      explicit. A shebang line in the source is ignored.
- [ ] QUIRK: `runghc`/`node`/etc. can't be configured. v2 could allow a
      per-language interpreter override in config.

### 7.3 Evaluation modes (`Evaluator.hs:83-140`)

Given `cmd, args, cwd, env, stdin`:

1. Write script (§7.2), pick interpreter, build `argv = interpreter ++
   [script] ++ args`.
2. `maybeWrapCmd(cwd, argv)` — try **direnv first, then nix** (first that
   applies wins):
   - **direnv** (`Wrappers/Direnv.hs`), only if `use_direnv == Some(true)`:
     - If `$DIRENV_DIR` is set and (after dropping its leading non-`/`
       chars, i.e. the `-` prefix direnv adds) equals `cwd` or any parent of
       `cwd` → direnv is already active → run unwrapped.
     - Else if a `.envrc` exists in `cwd` or any ancestor → argv becomes
       `direnv exec <cwd> <argv…>`.
     - Else → not applicable.
   - **nix** (`Wrappers/Nix.hs:61-71`), only if `use_nix == Some(true)`:
     - Find `shell.nix` walking up from `cwd`; if not found, `default.nix`
       walking up. (Note: `shell.nix` anywhere up the tree beats a closer
       `default.nix`.)
     - argv becomes `nix-shell --command "<argv joined by spaces, quoted>"
       <nix file>`. The quoting is a naive `"…"` wrap with `"` and `\`
       escaped — args containing spaces are **not** individually quoted
       (QUIRK; v2 should shell-quote each arg).
     - `flake.nix` is **not** supported.
   - [ ] MUST: `cwd == None` → never wrapped.
3. Log `Running command <source>` (info), `Args`, `Env` (debug),
   `Evaluating <path>` (info).
4. Decide **how** to run:
   - `use_tty = backend is not GUI (i.e. fzf) && stdin is a TTY`
   - `force_tty = config.force_tty.unwrap_or(false)`
   - If `use_tty || force_tty`:
     - Install a SIGINT handler that **ignores** SIGINT in nixon for the
       duration (so ^C reaches only the child), restore afterwards.
     - Run in the foreground (`run`): inherit stdout/stderr; stdin is either
       the provided lines (piped) or inherited.
     - Wait for exit. **Exit code of the child is ignored** (nixon exits 0;
       verified: `nixon eval 'exit 3'` exits 0). QUIRK — v2 should propagate
       the child's exit status.
   - Else if `cmd.is_bg`:
     - `spawn`: fork, `setsid()`, run the command in the child; parent
       returns immediately. No terminal.
   - Else (GUI backend, non-bg):
     - Append to the source:
       ```
       \necho -e "\n[Press Return to exit]\n"\nread\n
       ```
       (so the terminal stays open).
     - Terminal = `config.terminal` → `$TERMINAL` → `x-terminal-emulator`.
     - `spawn` `<terminal> -e <argv…>` (forked + setsid).
     - QUIRK: the "Press Return" suffix is bash syntax appended regardless
       of language.
5. Process environment = current environment + `env` pairs appended
   (`Process.hs:64-73`). cwd set on the child.

- [ ] MUST: `run` passes provided stdin lines to the child via a pipe;
      when there is no stdin placeholder the child inherits nixon's stdin
      (the `cat` command in `extra/config.md` documents this).
- [ ] MUST: `run_with_output` (used for placeholder candidates) captures
      stdout as lines (or raw bytes for JSON). stderr is inherited.
- [ ] SHOULD: Rust: use `std::process::Command`, `nix::unistd::{fork,
      setsid}` or `daemonize`-style double-fork for `spawn`.

### 7.4 Process helpers (`Nixon/Process.hs`)

- `build_args`/`flag`/`arg`/`arg_fmt` — optional-arg list builders used by
  the backends; trivial to port.
- `HasProc` trait abstracts `proc'(cmd, args, stdin_lines) -> (ExitCode,
  stdout)` so tests can mock fzf. [ ] SHOULD: keep an equivalent seam in
  Rust for testing the fzf/rofi wrappers.

---

## 8. Selection backends

> **v2**: the backend concept, `-b/--backend`, `-t/--terminal`,
> `-T/--force-tty` and the rofi backend are removed; one built-in picker
> replaces both. See ENGINEERING.md §7.2. The fzf behaviours below remain
> the reference for the built-in picker's semantics (candidate indexing,
> expect keys, `-1`, `--ansi`, `--no-sort`, filter mode).

### 8.1 Backend choice (`Types.hs:37-40`)

- [ ] MUST: `-b` wins; otherwise **fzf if stdin is a TTY, else rofi**.
- [ ] MUST: `is_gui_backend`: fzf → false, rofi → true.

### 8.2 Common selection model (`Nixon/Select.hs`)

```
SelectionType = Default | Edit | Show | Visit
Selection<T> = Empty | Canceled | Selection(SelectionType, Vec<T>)
Candidate = Identity(text) | WithTitle(title, value)
SelectorOpts { title: Option<String>, search: Option<String>,
               format: PlaceholderFormat (Lines), multiple: Option<bool> }
```

- [ ] MUST: `catMaybeSelection`: `Selection(_, [])` → `Empty`; drops
      `None`s from a lookup.
- [ ] MUST: JSON candidates: string → `Identity`; object with `title` and
      `value` → `WithTitle`; anything else → parse error.

### 8.3 Backend interface (`Nixon/Backend.hs`)

```
project_selector(opts, query: Option<String>, projects) -> Selection<Project>
command_selector(project, prompt: String, query: Option<String>, commands) -> Selection<Command>
selector(opts, candidates) -> Selection<String>      // generic, used for placeholders
```

Both backends map config → options identically (`Fzf.hs:65-81`,
`Rofi.hs:45-61`): `exact_match` → exact, `ignore_case` → ignore-case,
`opts.search` → query, `opts.title` → header/prompt, `opts.multiple ==
Some(true)` → multi.

### 8.4 fzf backend (`Nixon/Backend/Fzf.hs`)

**Option → argv mapping** (`fzfBuildArgs`, tested):

| Option | argv |
|---|---|
| border | `--border` |
| exact=true | `--exact` (false → nothing) |
| expect keys | `--expect k1,k2,…` |
| ignore_case=true | `-i` |
| header | `--header <text>` |
| height n | `--height n%` |
| query | `--query <text>` |
| filter | `--filter <text>` |
| preview | `--preview <cmd>` |
| with_nth | `--with-nth <spec>` where spec ∈ `N`, `..N`, `N..`, `A..B`, `..` |
| no_sort | `--no-sort` |
| multi | `--multi` |

- [ ] MUST: Options form a monoid: right-biased for `Option` fields, OR
      for bools, and `expect_keys` **concatenate** (`right ++ left`, so
      `opts <> expect(a) <> expect(b)` yields `--expect b,a` — order is
      irrelevant to fzf).
- [ ] MUST: Property tests: associativity, left/right identity for each
      option constructor.

**Invocation** (`fzfRaw`, `Fzf.hs:208-230`):

- [ ] MUST: In filter mode: argv = `["--filter", text]` only (no other
      options). Output lines are returned as `Selection(Default, lines)`
      verbatim.
- [ ] MUST: Interactive mode: argv = `["-1", "--ansi"] ++ build_args`.
      `-1` auto-selects when only one candidate matches the query.
- [ ] MUST: Exit code mapping: `0` → success; `1` → `Empty` (no match);
      `130` → `Canceled` (^C/Esc); other → **crash** with
      `nixon: Prelude.undefined` (verified: running interactive fzf without a
      TTY gives fzf exit 2 → crash). v2: treat any other code as a clean
      error (`fzf failed with exit code N`).
- [ ] MUST: Output parsing with `--expect`: the first output line is the key
      pressed (`""` for Enter). `[]` or `[""]` → `Empty`; `["", sel…]` →
      `Selection(Default, sel)`; `[key, sel…]` → `Selection(type-for-key,
      sel)`; unknown key → `Empty`.
- [ ] MUST: Tests: `(ExitSuccess, "")` → Empty; `(130, "")` → Canceled;
      `"\n1"` with expect → Default; `"alt-enter\n1"` → Edit; filter mode
      ignores the expect-key line.

**Candidate indexing** (`fzf`, `Fzf.hs:232-247`):

- [ ] MUST: Non-filter mode: candidates are numbered `1..n` and fed to fzf
      as `"<n> <title>"` with `--with-nth 2..` (so the index is hidden in
      the UI). Output lines are mapped back by taking the text up to the
      first space (`takeToSpace`) and looking up the index → **value**
      (not title). ANSI escapes are stripped from the value.
  - Rationale: allows duplicate titles and ANSI-colored titles.
  - Any index lookup failure → `Empty`.
- [ ] MUST: Filter mode feeds raw **titles** and returns matched titles
      verbatim (no value mapping).
- [ ] MUST: Test: candidates `["one two three","four five six","seven
      eight nine"]` with fzf output `"1\n3"` → `Selection(Default, ["one two
      three","seven eight nine"])`.

**Project selection** (`fzfProjects`):

- [ ] MUST: Candidates = project paths with `$HOME` replaced by `~`
      (`implode_home`), **sorted**, deduplicated by path (Map).
- [ ] MUST: Options: header `Select project`, `--border`, `--query` if a
      query was given, `--expect f1` → `Show`.
- [ ] SHOULD: Selected value(s) are mapped back to `Project`s.

**Command selection** (`fzfProjectCommand`):

- [ ] MUST: Candidate text = `show_command_with_description`; mapped back
      by exact text lookup (first match).
- [ ] MUST: Header = `"<prompt> [<project name>] (<project dir>)"` e.g.
      `Select command [nixon] (/home/me/code)`.
- [ ] MUST: `--no-sort` (keeps the by-name order from discovery), `--query`
      if given, expect keys: `alt-enter` → Edit, `f1` → Show, `f2` → Visit.
- [ ] MUST: Documented bindings (README): Return = run; Alt-Return = edit
      before run; F1 = print source; F2 = open in `$EDITOR`.

### 8.5 rofi backend (`Nixon/Backend/Rofi.hs`)

**Option → argv**:

| Option | argv |
|---|---|
| (always) | `-dmenu` |
| exact | `-matching normal` if true, `-matching fuzzy` if false (only when set) |
| ignore_case=true | `-i` |
| markup | `-markup-rows` |
| multi | `-multi-select` |
| `$XDG_SESSION_TYPE == wayland` | `-normal-window` (rofi issue #446) |
| msg | `-mesg <text>` |
| prompt | `-p <text>` |
| query | `-filter <text>` |

- [ ] MUST: Exit code mapping: `0` → Default; `1` → Canceled; `10` → Edit;
      `11` → Show; `12` → Visit (rofi's `-kb-custom-1..3`); other → error.
      Empty output → `Empty`.
- [ ] MUST: Candidates fed as titles (Map keys — so **sorted and
      deduplicated by title**; QUIRK: order is lexical, not discovery order);
      output lines looked up → values.
- [ ] MUST: Project candidate formatting: `"<name padded to 30> <i><dir
      with ~></i>"` with `-markup-rows` and prompt `Select project`.
- [ ] MUST: Command selection: prompt = the given prompt (e.g. `Select
      command`), no project info in the prompt, no `-mesg`.
- [ ] MUST: `$XDG_SESSION_TYPE` unset → `getEnv` throws → crash. v2: treat
      as not wayland.
- [ ] SHOULD: The custom key bindings (`-kb-custom-1` etc.) are **not**
      passed; the user must configure rofi themselves. v2 could pass
      `-kb-custom-1 alt+Return -kb-custom-2 F1 -kb-custom-3 F2` for parity
      with fzf.

---

## 9. Projects (`Nixon/Project.hs`)

### 9.1 Model

```
Project { name: Path (basename), dir: Path (parent), types: Vec<ProjectType> }
project_path = dir / name
ProjectType { id: String, markers: Vec<ProjectMarker>, description: String }
ProjectMarker = Path(p) | File(p) | Dir(p) | Or(Vec<Marker>) | Func(fn)
```

v2 omits `Func`: it was only constructible by Haskell code embedding nixon
as a library, never from config. v2 names the placeholder discriminant
`kind` rather than `type_`.

### 9.2 Type detection (`find_project_types`, `test_marker`)

- [ ] MUST: For a directory, a project type matches if **all** its markers
      pass; a type with **no markers always matches** (the "Generic project"
      catch-all in the sample config).
- [ ] MUST: `Path(p)` → `dir/p` exists (file or dir); `File` → is file;
      `Dir` → is dir; `Or` → any; `Func` → callback.
- [ ] MUST: A non-directory path has no types.
- [ ] MUST: Types are returned in **config order**, filtered.

### 9.3 `find_project(ptypes, dir)` → `Option<Project>`

- [ ] MUST: `None` if `dir` is not a directory.
- [ ] MUST: `None` if every matching type has an empty marker list (i.e.
      only the catch-all matched) — a directory is a *project* only if some
      marker-bearing type matches.
- [ ] MUST: Otherwise `Project { name: basename, dir: parent, types }` —
      note `types` **includes** the marker-less catch-all types.

### 9.4 `find_in_project(ptypes, path)` → `Option<Project>`

- [ ] MUST: Try `find_project` on `path`, then each parent, stopping when
      `parent(path) == root(path)`. QUIRK: the loop condition means the
      root directory itself is never tested (fine).
- [ ] BUG (verified against the binary, turtle ≥ 1.6): `parent` returns a
      path **with a trailing slash** (`/a/b/c` → `/a/b/`), so a project found
      by walking up from a *subdirectory* gets `name = ""` and
      `dir = <grandparent>`, i.e. `project_path` is the project's **parent**
      directory. Running `nixon` from `proj/sub/` therefore executes
      commands with cwd and `$nixon_project_path` set to the directory
      *containing* `proj`, and `project . -I` prints `Name:` empty. Only
      running from the project root itself works correctly. v2 MUST NOT
      replicate this: the project path must be the directory where the
      markers matched, and a test must cover discovery from a subdirectory.

### 9.5 `find_in_project_or_default(ptypes, path)` → `Project`

- [ ] MUST: `find_in_project` or, if not inside a project, a project for
      `path` itself with `types = find_project_types(path)` (so the
      catch-all type applies; commands with no type restriction still work
      from any directory).

### 9.6 `find_projects(max_depth, ptypes, source_dirs)` (`Project.hs:165-181`)

- [ ] MUST: Each source dir is expanded with **wordexp** (`~`, `$VAR`;
      command substitution disabled). Expansion failure → that entry
      contributes nothing. Wildcards (`~/src/*`) therefore work.
- [ ] MUST: For each expanded candidate that is a directory: yield it if it
      is a project (`find_project`) **and, regardless, recurse into its
      children** with `max_depth - 1`. (`project <|> subprojects` on
      turtle's `Shell` is stream concatenation, not choice; the Haskell doc
      comment saying "for each source directory *not* a project" is wrong.)
      `max_depth < 0` → `[]`.
- [ ] MUST: Called with `max_depth = 1` → source dirs themselves (depth 1)
      and their immediate children (depth 0) are considered; grandchildren
      are not.
- [ ] MUST: A source dir that is itself a project yields itself **and** any
      child projects (e.g. `~/src` being a git repo with vendored repos
      inside lists both). Tested in v2.
- [ ] MUST: `$VAR` expansion of an undefined variable yields the empty
      string (wordexp without `WRDE_UNDEF`); v2 uses shellexpand's no-error
      context for the same result.
- [ ] MUST: Results sorted by full path (`sort_projects`) in
      `getSortedProjects`. No dedupe.
- [ ] SHOULD: Hidden directories are not excluded (`ls` lists dotfiles).
- [ ] MUST: Projects are discovered **on every invocation** before
      dispatch (even for `gc`) — `nixonWithConfig` calls
      `getSortedProjects` first. Discovery is I/O bound; v2 may make it
      lazy per subcommand but must keep results identical.

### 9.7 `inspectProjects` (`Project.hs:208-214`)

- [ ] MUST: For each project print to stdout:
  ```
  Name: <name>
  Path: <full path>
  Types: <id1>, <id2>
  ```
  with a blank line between projects.

### 9.8 `implode_home`

- [ ] MUST: Replace a leading `$HOME/` with `~/` (exact prefix match; a
      path equal to `$HOME` itself is **not** shortened because the prefix
      is `$HOME/`).

---

## 10. Subcommand behaviors (`Nixon.hs`)

Common: after dispatch, errors of type `NixonError::{Empty, Nixon}(msg)`
are caught at top level and printed via `log_error` (to stderr) followed by
exit 1. Other exceptions propagate (crash with Haskell's default handler).

`die(x)` = `log_error(show x)` + `exit 1`. Note: `die` on a `String`
prints it **with quotes** (`show`), e.g. `"No command selected."`. QUIRK —
v2 should print without quotes.

### 10.1 `run` (`runAction`, `Nixon.hs:247-253`)

1. `project = find_in_project_or_default(ptypes, cwd)`.
2. If `--list`: `listProjectCommands(project, query=command)`:
   - [ ] MUST: candidates = `show_command_with_description` for every
         command from `findProjectCommands` (hidden included), run through
         `fzf --filter <query or "">` (non-interactive, **always fzf** even
         with `-b rofi`), print matching lines to stdout, one per line.
         No matches → `log_error "No commands."` on stderr and **exit 0**
         (verified). QUIRK.
3. Else `findAndHandleCmd`:
   - apply local config; select among non-hidden commands with prompt
     `Select command` and query = the `command` positional (with fzf `-1`
     this makes `nixon foo` run `foo` directly if it's the unique match;
     with multiple fuzzy matches the picker opens pre-filtered).
   - `handleCmd(project, selection, opts)` (§10.7).

### 10.2 `project` (`projectAction`, `Nixon.hs:219-244`)

- If `--list`: `listProjects(projects, query)`: paths with `~`, through
  `fzf --filter`, printed to stdout one per line; none → `log_error "No
  projects."`.
- Else:
  - `multiple = --select || --inspect` (multi-select allowed for those).
  - `findProject(projects, opts, query)`:
    - [ ] MUST: If `query == Some(".")`: use the project containing cwd
          (`find_in_project`) if any; otherwise fall back to interactive
          selection with no query.
    - Else interactive project selection with the query as pre-filled
      search (`-1` auto-selects unique).
  - `Empty` → error `No project selected.`; `Canceled` → `Project selection
    canceled.` (both exit 1).
  - If `--select`: print each selected project's full path (no `~`) to
    stdout, one per line.
  - Else if `--inspect` or the selection type is `Show` (F1 in fzf) →
    `inspectProjects` (§9.7).
  - Else exactly one project → `findAndHandleCmd` with
    `RunOpts { command: projCommand, args, insert: --insert, list: --list,
    select: --select }` — i.e. after picking a project, pick and run a
    command in it, exactly like `run` (the `--list` there is always false
    at this point).
  - Multiple projects selected (only possible with multi) → error
    `Multiple projects selected.`
- [ ] QUIRK: `nixon project <name>` where `<name>` matches multiple
      projects fuzzily → picker opens with the query; unique → direct.

### 10.3 `eval` (`evalAction`, `Nixon.hs:138-170`)

1. Project: if `--project` → interactive selection (no query); else
   `findProject(…, Some("."))` → cwd's project or interactive fallback.
   Errors as in §10.2.
2. Source: inline `command` text (language default `Bash`) or `--file`
   contents (language from extension, §7.1), overridable by `--language`.
3. Build `Command { name: "", source, placeholders (from CLI), pwd:
   Some(project_path), lang }` and `handleCmd` with `Selection(Default,
   [cmd])` and default run opts → runs it (§10.7 Default path).
- [ ] MUST: Placeholders given on the CLI resolve against the project's
      commands (e.g. `nixon eval 'vim "$1"' '${git-files}'`).
- [ ] MUST: `--file` is read relative to nixon's cwd, not the project.

### 10.4 `new` (`newAction`, `Nixon.hs:179-216`)

1. `findCmd("Insert after", None)` — interactive command selection (all
   commands **including hidden**, prompt `Insert after`) in the cwd project
   with local config applied.
2. Errors: `Empty` → `"No command selected."` (die, quoted); `Canceled` →
   `"Command selection canceled."`; multiple → `"Multiple commands
   selected."`; command without location → `"Unable to find command
   location."`.
3. Create a temp file `/tmp/nixon.md*` (mktempfile). Read the command's
   file; split lines at `loc.end_line` (so the new command goes after the
   selected command's full range, including trailing blanks); insert a
   template:
   ```
   <'#' × loc.level> `<name>`
   <blank>
   <description>
   <blank>
   ```<language>
   <source>
   ```
   ```
   (each element on its own line, `unlines`).
4. Open `$VISUAL`/`$EDITOR`/`nano` on the temp file at line
   `loc.end_line + 1`.
5. Run `diff -u --color=always <original> <tmp>` (output to terminal;
   non-zero exit ignored).
6. Prompt on stdout `Update <path>? [y/N] `, read a line from stdin; `y`/`Y`
   (stripped) → overwrite the original with the temp file contents and log
   `Updating <path>…` (info); otherwise log `Update canceled.` (info).
- [ ] QUIRK: The temp file is created under `/tmp` regardless of
      `$TMPDIR`.
- [ ] QUIRK: Selecting a **bin** command (location `(path,0,0,0)`) would
      splice the template into the executable file at line 0. v2: refuse or
      only offer markdown commands.
- [ ] SHOULD: `new` with fzf: `alt-enter`/`f1`/`f2` selection types are
      ignored (any `Selection(_, [cmd])` proceeds).

### 10.5 `edit` (`editAction`, `Nixon.hs:127-135`)

1. `findCmd("Edit command", query=command)` (hidden included).
2. Errors as in `new`.
3. `visitCmd`: open `$VISUAL`/`$EDITOR`/`nano` at
   `+<loc.start_line> <loc.file_path>`; error `Unable to find command
   location.` if none.
- [ ] MUST: For bin commands this opens the executable at line 0 → editor
      receives `+0`.

### 10.6 `gc` — see §7.2.

### 10.7 `handleCmd` (`Nixon.hs:77-96`) — shared by `run`, `project`, `eval`

Given `Selection<Command>`:

- `Empty` / `Selection(_, [])` → error `No command selected.`
- `Canceled` → error `Command selection canceled.`
- More than one → error `Multiple commands selected.`
- Exactly one `cmd` with selection type `t`:
  - If `--insert`: print `cmd.source` to stdout (with trailing newline
    added by `putStrLn` — source already ends with `\n`, so output ends
    with a blank line). Used by shell widgets to paste the source.
  - Else if `--select`: resolve the command's placeholders with
    `multiple = Some(true)` forced (`resolveCmd` — i.e. **run the command
    and let the user select from its output**, NOT run it as a command) and
    print the selected values, one per line, followed by an extra newline
    (`printf (s % "\n") (T.unlines resolved)`). Used by the shell widgets
    to insert a selection into the command line.
    - [ ] QUIRK: `--select` treats *the selected command* as a candidate
          producer: `nixon run -s git-files` shows the files from `git
          ls-files` for multi-selection and prints the chosen ones.
  - Else by `t`:
    - `Default` → `runCmd` (§5.6/§7.3) with the positional `args`.
    - `Edit` (fzf alt-enter / rofi exit 10) → `editCmd`: open a readline
      prompt `> ` pre-filled with the stripped source (haskeline
      `getInputLineWithInitial`; single-line editing of the whole source —
      multi-line sources are shown with embedded newlines); empty result →
      error `Empty command.`; otherwise run the edited source in place of
      `cmd.source` (same lang, same placeholders, same args).
    - `Show` (f1 / exit 11) → print `cmd.source` to stdout.
    - `Visit` (f2 / exit 12) → `visitCmd` (§10.5).
- [ ] MUST: Selection-type actions ignore `--insert`/`--select` (those are
      checked first regardless of type). E.g. `nixon -i` + alt-enter still
      just prints the source.

### 10.8 Exit codes / output summary

- [ ] MUST: Success paths exit 0, including when the executed child fails
      (QUIRK, §7.3).
- [ ] MUST: `NixonError`s and `die` → stderr message, exit 1.
- [ ] MUST: All logging goes to **stderr**; only data (`--list`,
      `--select`, `--insert`, `inspect`, `gc`, `Show`) goes to stdout.

---

## 11. Logging (`Nixon/Logging.hs`)

- [ ] MUST: Levels `Debug < Info < Warning < Error`. A message is emitted
      iff `msg_level >= configured_level`. Default level `Warning`.
- [ ] MUST: Output: plain message text to stderr, no prefix/timestamp.
- [ ] MUST: Property test: for each `log_<lvl>` and every configured level
      the message appears iff `lvl >= configured`.
- [ ] SHOULD: Messages currently emitted:
  - info: `Running <edit|eval|gc|new|project|run> command`,
    `Running command <source>`, `Evaluating <path>`, `Updating <path>…`,
    `Update canceled.`
  - debug: `Args: […]`, `Env: […]`
  - error: `No projects.`, `No commands.`, fatal messages.

---

## 12. Shell integration (`extra/`)

Installed by the Nix package to `share/nixon/` and `share/zsh/site-functions/_nixon_widget`.

**bash** (`nixon-widget.bash`):

- `Alt-i` → `nixon -b fzf -T run -s` → each output line `%q`-quoted and
  space-joined, inserted at the cursor.
- `Alt-I` → `nixon -b fzf -T run -i` → source inserted at the cursor.
- `Alt-p` → `nixon -b fzf -T project -s` → path inserted at the cursor.

**zsh** (`nixon-widget.zsh`): `Alt-i` → `nixon -b fzf -T run -s` appended
to `LBUFFER`, `zle reset-prompt`.

- [ ] ~~MUST: v2 must keep the exact CLI shapes these widgets use:
      `-b fzf`, `-T`, `run -s`, `run -i`, `project -s`.~~ Superseded by
      ENGINEERING §7.2: `-b`/`-T` are removed and the widgets are
      rewritten; `run -s`, `run -i`, `project -s` remain.
- [ ] SHOULD: Ship equivalent widget files and completion scripts (clap
      has `clap_complete` for static completion; dynamic command/project
      completion needs a custom completer hook — see §2.4).

---

## 13. Packaging & build

- [ ] SHOULD: Nix flake with `defaultPackage` (static exe), `devShell`,
      overlay exposing `nixon`. Runtime deps: `fzf` (all), `rofi` (Linux
      only).
- [ ] SHOULD: `postInstall` copies widgets and generates completions
      (§12).
- [ ] SHOULD: GitHub Actions: build + lint + test on push/PR to `main`;
      nix build with cachix (`myme`).
- [ ] SHOULD: The repo's own `nixon.md` defines dev commands (`run`,
      `tdd`, `hoogle`); v2 should provide a Rust equivalent (`cargo run`,
      `cargo watch -x test`, …).
- [ ] SHOULD: Supported platforms: Linux and macOS (commits `9281b1b`,
      `1a3f293` — case-insensitive FS tolerance).

---

## 14. Test inventory (what the Haskell suite covers → port these)

| Suite | Cases | Port target |
|---|---|---|
| `Backend.Fzf` | option monoid laws, argv building for every option, exit-code mapping, expect-key parsing, index→value mapping, filter passthrough, projects/commands selection via mocked process | fzf module unit tests with a mocked process runner |
| `Command.Find` | empty config, markdown commands returned, project-type filtering (both directions), markdown `{type=…}` filtered out for untyped project | discovery tests with temp dirs |
| `Config.Markdown` | config blocks (JSON/YAML/heading/info-string), all error messages, single-config rule, command name/tick/hidden/level-gap/first-word/description/bg/setext/`type`, placeholder formats (`cols`, `cols+h`, `json`, conflict error), header-vs-block placeholder conflict, locations (single/multiple/with config), `parseHeaderArgs` (9 cases), `parseCommandName` (24 cases) | markdown parser tests — port **verbatim**; they are the de-facto grammar spec |
| `Format.Columns` | parseColumns (4), formatColumns (2) | port verbatim |
| `Logging` | level filtering property | port |
| `Process` | mock runner sanity | port |
| `Utils` | `escape`, `quote`, `takeToSpace` properties | port |

Untested today (add tests in v2): placeholder resolution (`zipArgs`,
stdin concat, env join), evaluation wrapping (direnv/nix), project discovery
(`find_projects` depth semantics, wordexp), `new` splice, rofi argv, CLI
parsing, config merge.

---

## 15. Known TODOs from `todos.org` (candidate v2 features, not required for parity)

- Org-mode config support.
- `new` sub-command (DONE — but limited).
- Dump config or selection to file.
- Bookmarks / recent commands.
- nix-shell-like build inputs per code block.
- Parallel composition of commands.
- Default selections for multi-select; placeholder default values.
- Pass-through backend configuration (fzf/rofi styling).
- Add to / delete from shell history.

---

## 16. Suggested Rust module map (for orientation only)

| Haskell | Rust |
|---|---|
| `Nixon.Config.Options` | `cli.rs` (clap derive; custom tri-state flags; dynamic completion) |
| `Nixon.Config.{Types,JSON}` | `config.rs` (serde for JSON+YAML; `Config` + `merge`) |
| `Nixon.Config.Markdown` | `markdown.rs` (pulldown-cmark/comrak with line offsets) + `placeholder.rs` (nom/winnow grammar) |
| `Nixon.Command`, `.Placeholder` | `command.rs` |
| `Nixon.Command.Find` | `discover.rs` |
| `Nixon.Command.Run` | `resolve.rs` |
| `Nixon.Evaluator`, `.Wrappers.*` | `eval.rs`, `wrappers/{direnv,nix}.rs` |
| `Nixon.Format` | `format.rs` |
| `Nixon.Language` | `language.rs` |
| `Nixon.Backend{,.Fzf,.Rofi}`, `Nixon.Select` | `select/{mod,fzf,rofi}.rs` |
| `Nixon.Project` | `project.rs` (wordexp via `shellexpand`/`glob`) |
| `Nixon.Process` | `process.rs` (trait for mocking) |
| `Nixon.Logging` | `log`/`tracing` to stderr, plain format |
| `Nixon` (dispatch) | `main.rs` / `app.rs` |
