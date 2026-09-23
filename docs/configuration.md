# Configuration

Nixon is configured by markdown files. The same file holds settings and
commands: a fenced code block tagged `config` is the settings, headings with
inline code are the [commands](commands.md).

## Where the files are

| File | Purpose |
|---|---|
| `$XDG_CONFIG_HOME/nixon.md` | Global settings and commands. |
| `<project>/nixon.md` | Local to a project. |
| `<project>/.nixon.md` | Local, hidden alternative. |
| `-C <path>` | Replaces the global file. |

A missing or empty global file is fine; nixon treats it as empty.

The local file is found by walking up from the current directory. The **whole**
chain is searched for `nixon.md` before `.nixon.md` is tried anywhere, so a
`nixon.md` two directories up wins over a `.nixon.md` in the project root. The
filesystem root itself is never tested.

A malformed config file is always an error. A missing one is only an error when
you named it with `-C`.

## How the sources combine

Built-in defaults, then the global file, then the local file, then the command
line — each merged over the last:

- A setting that is on or off takes the right-hand value when it is set at all.
- `project_dirs`, `bin_dirs` and `project_types` concatenate.
- Commands concatenate **right-first**, so a local command of the same name
  shadows a global one.
- `launcher.terminal` and `launcher.search_url` replace inherited values when
  supplied. `launcher.items` replaces the whole inherited menu tree.

## The config block

One code block per file, tagged `config` after the language. Both YAML and JSON
are accepted; unknown keys are ignored.

````markdown
``` yaml config
exact_match: true
ignore_case: true
use_direnv: true
use_nix: true
project_dirs:
  - ~/src
  - ~/work/*
project_types:
  - name: cabal
    test: ["cabal.project"]
    desc: Cabal new-style project
  - name: git
    test: [".git"]
    desc: Git repository
  - name: project
    desc: Generic project
```
````

A second config block in the same file is an error, so a stray one cannot
silently win.

## Settings

- **`bin_dirs`** (paths) — directories, relative to a project, whose
  executables are offered as commands.
- **`exact_match`** (bool, off) — substring matching in the picker instead of
  fuzzy matching.
- **`ignore_case`** (bool, off) — case-insensitive matching.
- **`project_dirs`** (paths) — where to look for projects. `~`, `$VAR` and
  globs are expanded.
- **`project_types`** (list) — project type definitions; see below.
- **`use_direnv`** (bool, off) — run commands through `direnv exec`.
- **`use_nix`** (bool, off) — run commands through `nix-shell`.
- **`git_worktrees`** (bool, **on**) — also discover git worktrees.
- **`history`** (bool, **on**) — record every command that runs. See
  [the log](cli.md#the-log) and
  [shell history](shell-integration.md#shell-history).

An undefined `$VAR` in `project_dirs` expands to the empty string, and a glob
that matches nothing contributes nothing.

## GUI menu and hotkeys

The GUI builds its root menu from built-in defaults, the global config, and
the current project's local `nixon.md` when the window opens. Set
`launcher.items` in either file to replace the entire inherited menu. Each
item has one `key`, a `label`, and either an `action` or nested `items`.
Omitted launcher fields inherit their prior values. The menu stays fixed
while the window is open; picking another project uses that project's local
settings for its commands and terminal, without changing the menu.

```yaml
launcher:
  items:
    - key: C
      label: Commands
      action: commands
    - key: T
      label: Tools
      items:
        - key: E
          label: Edit project
          action: { command: edit }
```

Keys are ASCII letters or digits, or `Space`. Sibling keys must be unique.
Actions include `commands`, `projects`, `history`, `browser_input`, a named
`command` (optionally with `project`), and `mpris`. An empty menu or invalid
launcher field is an error reported with its config file context before the
window opens.

## GUI browser search

The graphical menu's Browser action uses the effective `launcher.search_url`
at startup. The value must contain `{query}`; the typed search text replaces
that placeholder after UTF-8 URL encoding. The default is
`https://www.google.com/search?q={query}`.

```yaml
launcher:
  search_url: "https://search.example/find?q={query}"
```

A full `http://` or `https://` URL is opened unchanged. A dotted address with
no whitespace gets `https://` prepended. Other input uses the search URL.
On Linux, the opener is `xdg-open`; the Nix package and development shell
include it.

## GUI terminal launcher

The GUI Commands and Projects actions and configured command actions use the
selected project's effective `launcher.terminal` as an argument vector for
foreground runs. Put the terminal's execute flag in its own element, for
example:

```yaml
launcher:
  terminal: ["kitty", "-e"]
```

F2 Visit uses this terminal to open the recorded command file and line in
`$VISUAL` or `$EDITOR` (falling back to `nano`). The editor setting names an
executable; its file and line are passed as separate arguments.

If the effective launcher config has no `terminal`, the runner parses
`$TERMINAL` as arguments, preserving quotes. A bare `alacritty`, `kitty`,
`foot`, `konsole`, `xterm`, `uxterm`, or `x-terminal-emulator` gets `-e`; a bare
`gnome-terminal` gets `--`. If `$TERMINAL` contains more than the executable,
include its execute flag yourself. An unknown bare terminal is rejected with
that instruction. If neither setting is present on Linux, an available
`x-terminal-emulator -e` is used. An unavailable configured terminal or an
incomplete argument vector produces an error before the command is handed off.
The runner passes the prepared command through a private one-use payload,
without joining arguments into a shell command.

## Project types

A project type gives a name commands can be scoped to, and the markers that
recognise one.

```yaml
project_types:
  - name: npm
    test: ["package.json"]
    desc: NPM project
  - name: project
    desc: Generic project
```

- `name` and `desc` are required; `test` defaults to empty.
- Every entry in `test` must exist for the type to match. A type with no `test`
  matches every directory, which is how a catch-all type is written.
- Paths are relative to the project root and may be files or directories.

Types are used in two places: `type="npm"` on a heading scopes commands to
matching projects, and `nixon project --inspect` lists what a project matched.

## Project discovery

`nixon project` lists every directory under `project_dirs` that matches a type,
one level deep. A directory that is itself a project is listed **and** its
children are still scanned.

With `git_worktrees` on, the worktrees of every git repository found are listed
too, read from git's own layout rather than by running `git`. This picks up
worktrees of bare repositories, and worktrees that live outside `project_dirs`
entirely.

It also picks up the *container* layout, where a directory holds its bare
repository in a dot-subdirectory and its worktrees beside it:

```text
~/code/gaia/.bare/     the repository
~/code/gaia/bugs/      a worktree
~/code/gaia/claims/    another
```

The worktree scan recognises such a container by the repository layout rather
than by the `.bare` name, and lists each worktree. A container that does carry
a `.git` file pointing at its repository works the same way.

A `test: [".git"]` marker matches a container too, and a bare repository, so
the container is listed alongside its worktrees without any extra
configuration. Only `.git` is read that widely; every other marker is a path
that has to exist.

## Environments

A command can be wrapped in a project environment. `direnv` is tried first, and
the first wrapper that applies wins.

- `use_direnv` wraps in `direnv exec <cwd>` when there is an `.envrc` at or
  above the working directory. Nothing is wrapped — and nix is not tried —
  when the environment already loaded is the one that directory would load,
  that is when `$DIRENV_DIR` is the directory of its nearest `.envrc`. A
  nearer `.envrc` than the loaded one is still wrapped, so a package inside a
  monorepo gets its own environment.
- `use_nix` wraps in `nix-shell --command`. As with the local config, the whole
  ancestor chain is searched for `shell.nix` before `default.nix` is considered.
  `flake.nix` is not supported.

A command with no working directory is never wrapped.
