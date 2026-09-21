# Command line

```console
$ nixon --help
Command & environment launcher

Usage: nixon [OPTIONS] [COMMAND]

Commands:
  edit     Edit a command in `$EDITOR`
  eval     Evaluate an expression
  gc       Garbage collect cached scripts
  history  Show what has been run, and run it again
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

**Where nixon's flags stop.** A subcommand's own flags go *before* the command
name; everything after the command name belongs to the command. `nixon run -i
hello` prints `hello`'s source, `nixon run hello -i` passes `-i` to `hello`.
The same for `project`, where the project name comes first and nixon's flags
may follow it, because the command name has not arrived yet:

| Command line | `-i` belongs to |
|---|---|
| `nixon run -i hello` | nixon |
| `nixon run hello -i` | the command |
| `nixon hello -i` | the command |
| `nixon project . -i hello` | nixon |
| `nixon project . hello -i` | the command |

A `--` after the command name is the shell's way of saying "no more flags". It
has done its job by the time nixon sees it, so it is dropped rather than passed
on: `nixon run hello -- -i` gives `hello` the single argument `-i`.

`-L` takes `debug`, `info`, `warning` (or `warn`) and `error`. The default is
`warning`.

A bare argument that is not a subcommand is `run`'s, so `nixon hello` and
`nixon run hello` are the same. A word that *is* a subcommand is always the
subcommand: a command called `edit` has to be run as `nixon run edit`. The
reserved words are `edit`, `eval`, `gc`, `history`, `new`, `project`, `run`,
`help` and `internal` — the last of these is packaging machinery and is hidden
from `--help`, but it shadows a command of that name like any other.

## `run`

```
nixon run [-i] [-l] [-s] [COMMAND] [ARGS]...
```

nixon's own flags go before `COMMAND`; see the flag boundary above.

Selects a command in the current project and runs it. `COMMAND` is a search
query for the picker; a name matching a command exactly runs it without the
picker, hidden `_names` included, and so does a query matching exactly one
command. `ARGS` are queries for the command's
[placeholders](placeholders.md), in order — and an argument equal to a
candidate's value takes that candidate outright, so a fully specified command
line never stops to ask.

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

nixon's own flags go before `COMMAND`, which may be after `PROJECT`; see the
flag boundary above.

Selects a project, then a command in it. `PROJECT` is a query, with two
special cases:

- `.` means the project **containing** the current directory, falling back to
  an unfiltered picker when there is none.
- Anything containing `/`, or starting with `~`, `./` or `../`, is a
  directory: nixon resolves it directly, with no discovery and no picker, and
  reports a path that is not there rather than searching for it.

Those two differ outside a project. `.` walks up to find one, so from a
subdirectory it gives the project root; `./` names the current directory
itself, whether or not anything recognises it as a project. Relative paths are
resolved against the directory nixon was invoked in.

A `COMMAND` naming a command exactly runs it without the picker, hidden
`_names` included.

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

## `history`

```
nixon history [-l] [-n N] [--clear] [QUERY]
```

Picks from [the log](#the-log), newest first, and runs the chosen line again —
through the same argument parser, so its options and values mean what they
meant the first time, and the re-run is itself recorded. A run of the same
command collapses to one row.

A `QUERY` that matches exactly one line takes it without drawing anything.
Without one the picker always opens, even for a single entry: looking at the
history is not asking to run something.

| Flag | Effect |
|---|---|
| `-l`, `--list` | Print matching invocations and exit. |
| `-s`, `--select` | Pick one and print it, running nothing. |
| `-n`, `--limit` | Keep only the last N entries. |
| `--clear` | Empty the log, after asking. |

What `--list` and `--select` print is the whole command line, `nixon` and all,
so it can be pasted or run as it stands.

A line recorded as `run <name>` runs against the project the *current*
directory is in, wherever it was first run — it is the command that is
remembered, not the place. A command run in another project is recorded as
`project <path> <name>` and goes back to that one.

`F1` and `Alt-Enter` print the invocation instead of running it, and `Esc`
cancels. With `history: false` in the configuration there is nothing to show,
and the command exits 1 saying so.

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

## The log

Every command that actually runs is appended to
`$XDG_STATE_HOME/nixon/history` — `~/.local/state/nixon/history` by default.
Listing, `--select`, `--insert` and printing a source record nothing.

Each line is three tab-separated fields: the time in seconds since the epoch,
the directory nixon was invoked in, and the command line that runs it again.

```text
1700000000	/home/me/code/gaia	nixon run deploy --force staging
```

The third field is shell-quoted and complete: the options as they ended up —
only where they differ from their declared defaults — then every placeholder's
chosen value. Values rather than queries, so a replayed line
[settles each placeholder outright](placeholders.md#arguments-on-the-command-line)
and asks nothing. A command run in another project is recorded as
`nixon project <path> <name>`, so the line means the same thing from anywhere.

One run is one line. The directory and the command line are escaped for it —
`\\` for a backslash, `\t` for a tab, `\n` for a newline — because shell
quoting makes a value safe for a shell, not for a line-oriented file: it
leaves a newline inside a quoted word where it was. Anything reading the log
by hand has to put those back; the shell hooks do.

An `eval` is recorded as its source, in the language it ran in, followed by
the placeholders as they were written:

```text
1700000000	/home/me/code	nixon eval -l python 'print(files)' '${git-files}'
```

Its placeholders are the one thing that does not replay settled. `eval` reads
the words after its source as placeholders, and a chosen value has no spelling
there, so a replayed `eval` asks again. A project chosen with `--project` is
recorded as `--project=<path>`.

Set `history: false` to record nothing. The shell widgets can put these lines
into your shell's own history; see
[shell integration](shell-integration.md#shell-history).

## Output

stdout carries data and nothing else: selections, listings, sources, paths.
Everything meant for a person — the picker, log messages, errors — goes to
stderr, so `nixon run -s | …` is always safe to pipe.
