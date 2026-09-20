# Command line

```console
$ nixon --help
Command & environment launcher

Usage: nixon [OPTIONS] [COMMAND]

Commands:
  edit     Edit a command in `$EDITOR`
  eval     Evaluate an expression
  gc       Garbage collect cached scripts
  new      Insert a new command into a config file
  project  Select a project and run a command in it
  run      Select and run a command
  help     Print this message or the help of the given subcommand(s)

Options:
  -C, --config <CONFIG>      Path to config file [default: [..]]
  -e, --exact                Exact match in the selector
  -i, --ignore-case          Case-insensitive match in the selector
  -p, --path <PATH>          Project directory, repeatable
  -d, --direnv               Run commands through `direnv exec`
  -n, --nix                  Run commands through `nix-shell`
  -L, --loglevel <LOGLEVEL>  Log level
  -h, --help                 Print help
  -V, --version              Print version

See nixon.md(5), nixon-picker(7), nixon-shell(7).

```

Global options come before the subcommand. Each on/off flag has a hidden
`--no-…` counterpart, and the last one given wins, so a config file setting can
be overridden either way.

`-L` takes `debug`, `info`, `warning` (or `warn`) and `error`. The default is
`warning`.

A bare argument that is not a subcommand is `run`'s, so `nixon hello` and
`nixon run hello` are the same. A word that *is* a subcommand is always the
subcommand: a command called `edit` has to be run as `nixon run edit`. The
reserved words are `edit`, `eval`, `gc`, `new`, `project`, `run`, `help` and
`internal` — the last of these is packaging machinery and is hidden from
`--help`, but it shadows a command of that name like any other.

## `run`

```
nixon run [-i] [-l] [-s] [COMMAND] [ARGS]...
```

Selects a command in the current project and runs it. `COMMAND` is a search
query for the picker, not an exact name — if it matches exactly one command,
that command runs without the picker appearing. `ARGS` are queries for the
command's [placeholders](placeholders.md), in order.

| Flag | Effect |
|---|---|
| `-l`, `--list` | Print matching command names, hidden ones included, and exit. |
| `-i`, `--insert` | Print the selected command's source instead of running it. |
| `-s`, `--select` | Resolve the command's placeholders and print what was selected. |

`--list` with no matches says so on stderr and still exits **0**; the shell
widgets rely on that.

## `project`

```
nixon project [-i] [-I] [-l] [-s] [PROJECT] [COMMAND] [ARGS]...
```

Selects a project, then a command in it. `PROJECT` is a query, with one special
case: `.` means the project containing the current directory, falling back to
an unfiltered picker when there is none.

| Flag | Effect |
|---|---|
| `-l`, `--list` | Print matching project paths, with `~` for `$HOME`, and exit. |
| `-s`, `--select` | Print the selected project's path. Several may be selected. |
| `-I`, `--inspect` | Print what the project matched: its path and types. |
| `-i`, `--insert` | Print the selected command's source instead of running it. |

## `eval`

```
nixon eval [-f FILE] [-l LANG] [-p] [EXPRESSION] [PLACEHOLDERS]...
```

Runs an expression as if it were a command in the project, so it gets the same
working directory, `$nixon_project_path`, and `direnv`/`nix-shell` wrapping.

| Flag | Effect |
|---|---|
| `-f`, `--file` | Read the expression from a file. Its extension picks the language. |
| `-l`, `--language` | Set the language explicitly. |
| `-p`, `--project` | Select a project first, instead of using the current directory. |

Placeholders are given as arguments in placeholder syntax:

```shell
nixon eval 'vim "$1"' '${git-files:m}'
```

## `edit`

```
nixon edit [COMMAND]
```

Opens the selected command in `$EDITOR`, at the line where it is defined.
Hidden `_commands` are offered here.

## `new`

```
nixon new [-n NAME] [-d DESC] [-l LANG] [-s SRC]
```

Inserts a new command into a markdown file. You pick the command to insert
after; nixon splices the new section into a copy, opens it in `$EDITOR`, shows
a diff, and asks before overwriting the original. Answering anything but `y`
leaves the file alone.

Defaults are `<name>`, `Description…` and `bash`, which is a template to edit
rather than a command to keep.

A command that came from `bin_dirs` has no markdown location, so it cannot be
inserted after.

## `gc`

```
nixon gc [-d]
```

Removes cached scripts, printing each path on stdout. `-d`/`--dry-run` prints
without removing.

## Exit codes

| Code | Meaning |
|---|---|
| The command's own | A command ran. |
| `130` | A selection was cancelled. |
| `1` | Anything else that failed. |
| `0` | `--list` with no matches, and a detached command. |

While a foreground command runs, nixon ignores `SIGINT`, so `^C` reaches only
the command.

## Output

stdout carries data and nothing else: selections, listings, sources, paths.
Everything meant for a person — the picker, log messages, errors — goes to
stderr, so `nixon run -s | …` is always safe to pipe.
