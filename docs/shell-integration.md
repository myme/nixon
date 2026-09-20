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
| `Alt-p` | Insert a project's path |

Source the one for your shell:

```shell
# ~/.bashrc
source /path/to/share/nixon/nixon-widget.bash

# ~/.zshrc
source /path/to/share/nixon/nixon-widget.zsh

# ~/.config/fish/config.fish
source /path/to/share/nixon/nixon-widget.fish
```

Each is three lines of shell around `nixon run -s`, `nixon run -i` and
`nixon project -s`, so they are worth reading and rebinding to taste.

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
