# Shell integration

Nixon installs shell widgets and generates its own completions. Both are under
the package's `share` directory.

## Widgets

The widgets put nixon behind a key, so a selection lands on the command line
you are already typing rather than in a subshell.

| Key | Action |
|---|---|
| `Alt-i` | Run a command and insert what you pick from its output |
| `Alt-I` | Insert a command's source at the cursor |
| `Alt-p` | Pick a project and change directory into it |
| `Alt-P` | Insert a project's path at the cursor |

`Alt-p` is fzf's `Alt-C` for projects: nothing is typed, the prompt is redrawn
in the new directory, and cancelling leaves everything as it was.

Source the one for your shell:

```shell
# ~/.bashrc
source /path/to/share/nixon/nixon-widget.bash

# ~/.zshrc
source /path/to/share/nixon/nixon-widget.zsh

# ~/.config/fish/config.fish
source /path/to/share/nixon/nixon-widget.fish
```

Each is a few lines of shell around `nixon run -s`, `nixon run -i` and
`nixon project -s`, so they are worth reading and rebinding to taste.

### Shell history

The widgets also install a prompt hook that copies what nixon ran into the
shell's own history, so `Ctrl-R` finds it like any other command. It reads
[the log](cli.md#the-log), remembers how far it had got, and does nothing
until the file grows — one `stat` per prompt.

Point `NIXON_HISTORY_FILE` somewhere else to override the path.

- **bash** and **zsh** add the line with `history -s` / `print -s`.
- **fish** uses `history append`, which arrived in fish 3.2. An older fish
  keeps the log but adds nothing to its own history.

Whatever was logged before the shell started counts as read, so opening a
terminal does not replay everything that ever ran.

A `| multi` selection is recorded as consecutive values. Replaying it takes
the first as the placeholder's exact value and the rest as ordinary
arguments, which is not the same thing — that one does not round-trip.

### `Alt-p` and shell history

Changing directory has to go through the shell itself, and each one allows a
different amount of discretion about it:

- **fish** runs `cd` inside the function. Nothing is typed and nothing is
  recorded.
- **zsh** types ` builtin cd -- <path>` with a leading space, so
  `setopt HIST_IGNORE_SPACE` keeps it out of history. Without that option it
  is recorded, as it is with fzf.
- **bash** records it. The `cd` is produced by readline's `shell-expand-line`,
  which expands the line as words and drops leading whitespace, so
  `HISTCONTROL=ignorespace` has nothing to act on. fzf's `Alt-C` behaves the
  same way. Add `HISTIGNORE='builtin cd -- *'` to suppress it.

A recorded `cd` is not purely a nuisance — it is the jump, repeatable from
anywhere, which is why fzf keeps it.

The v1 widgets do not work with v2: they passed `-b fzf -T`, which no longer
parse.

## Completion

Completion is produced by the binary itself, so command and project names come
from your actual configuration rather than from a generated script that goes
stale.

```shell
# bash
eval "$(COMPLETE=bash nixon)"

# zsh
eval "$(COMPLETE=zsh nixon)"

# fish
COMPLETE=fish nixon | source
```

The package installs these one-line loaders where each shell looks for them —
`share/bash-completion/completions`, `share/zsh/site-functions`,
`share/fish/vendor_completions.d` — so on Nix they are picked up without any
further configuration.

## Scripting

stdout is data. `nixon run -s` prints the selected values one per line,
`nixon run -l` prints command names, `nixon project -l` prints project paths.
The picker draws on stderr, so it still works when stdout is a pipe.

A query that matches exactly one candidate never opens the picker, so
`nixon some-command` in a script with no terminal is safe.

`nixon run -l` with no matches prints nothing on stdout, says so on stderr and
exits 0, so a widget can act on empty output without treating it as failure.
