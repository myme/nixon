# Nixon

Project environment and command launcher.

`nixon` reads `nixon.md` files, finds the commands in them, lets you pick one
with a built-in fuzzy picker, and runs it — optionally inside `direnv` or
`nix-shell`. It has no external dependencies: the picker is built in, so there
is no `fzf` or `rofi` to install.

## Configuration

`nixon` bases its configuration around `nixon.md` files. The configuration files
are generic markdown files, with some syntactic markers to indicate which part
of the file is supposed to be treated as either commands or configuration by
`nixon`.

General configuration may be done by placing a `nixon.md` in
`$XDG_CONFIG_DIRS/nixon`. Project specific configuration may be done by placing
a `nixon.md` (or `.nixon.md`) in the root of the project.

Following is an example configuration. There is also an inspirational
configuration under [./extra/config.md](./extra/config.md).

Example configuration:

~~~~~~markdown

# Nixon

## Config

The following source code block defines a `nixon` configuration using `YAML`:

``` yaml config
exact_match: true
ignore_case: true
use_direnv: true
use_nix: true
project_dirs:
  - ~/src
project_types:
  - name: cabal
    test: ["cabal.project"]
    desc: Cabal new-style project,
  - name: npm
    test: ["package.json"]
    desc: NPM project,
  - name: nix
    test: ["default.nix", "shell.nix"]
    desc: Nix project,
  - name: git
    test: [".git"]
    desc: Git repository,
  - name: project
    desc: Generic project
```

`JSON` is also supported:

``` json config
{
  "exact_match": true,
  "ignore_case": true,
  "use_direnv":true,
  "use_nix":true,
  "project_dirs": [
    "~/src"
  ],
  "project_types": [
    { "name": "cabal", "test": ["cabal.project"], "desc": "Cabal new-style project"},
    { "name": "npm", "test": ["package.json"], "desc": "NPM project"},
    { "name": "nix", "test": ["default.nix", "shell.nix"], "desc": "Nix project"},
    { "name": "git", "test": [".git"], "desc": "Git repository"},
    { "name": "project", "desc": "Generic project"}
  ]
}
```

Please note that only one configuration source code block is allowed per file,
to avoid misconfiguration.

## Commands

Commands are defined as markdown sections with titles in inline code tags.

### `hello-sh`

This is a basic shell command with a description.

```
echo "Hello, World!"
```

### `hello-python`

This is a Python command (note the `python` language annotation):

```python
print("Hello, World!")
```

### `terminal &`

Spawn a terminal as a background task.

```bash
x-terminal-emulator
```

## Git stuff {type="git"}

Markdown headers can indicate what kind of projects commands are associated
with. Commands under this "Git stuff" heading are only available within projects
detected as `git` projects. That is determined by the `name: git` test in the
`project_types`, testing for a `.git` directory (or file) in the project root.

### `git-files`

```bash
git ls-files
```

### `vim-file`

This `vim-file` command references the `git-files` command as an argument
placeholder. In this case `nixon` will first execute the `git-files` command to
list all the tracked files within the project. It will then present the user
with an interactive, fuzzy-finding prompt. Once the user makes their selection
the selected file will be passed as `$1` (first argument) to the `vim-file`
command.

```bash ${git-files}
vim "$1"
```

### `vim-files`

It's possible to specify a multi-selection modifier to let the user select
multiple files to pass to `vim`. In the `fzf` interface marking files for
selection is done using `<tab>`.

```bash ${git-files | multi}
vim -p "$@"
```

### `vim-files-m`

There's a shorthand `:m` if typing ` | multi` is too long.

```bash ${git-files:m}
vim -p "$@"
```

### `vim-stdin`

The `stdin` placeholder may be used to select candidates that will be passed to
the command's `stdin`. Here we're using the `xargs` command to relay that as
positional arguments to `vim`.

```bash <{git-files | multi}
xargs vim -p
```

### `vim-env`

The `environment variable` placeholder places the selection of a placeholder
into an environmental variable. The `environment variable` is named after the
placeholder action with `-` *(dashes)* replaced by `_` *(underscore)*,
`git_files` in this case.

```bash ={git-files | multi}
vim -p $git_files
```

### `vim-env-alias`

It is possible to give the environment variable an explicit name by placing and
alias before the `=`, in this case `FILES`.

```bash FILES={git-files | multi}
vim -p $FILES
```

~~~~~~

## Usage

Pick a command in the current project and run it:

``` shell
nixon
```

Run a command by name. A name that matches exactly one command runs without
the picker appearing:

``` shell
nixon hello-sh
```

Pick a project first, then a command in it:

``` shell
nixon project
```

List commands, or projects, without running anything:

``` shell
nixon run -l
nixon project -l
```

Evaluate a one-off expression in the current project:

``` shell
nixon eval 'echo "$nixon_project_path"'
```

Help text:

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

```

### Picker keys

The picker follows `fzf`'s bindings, and the query line follows readline's.

Choosing:

| Key | Action |
|---|---|
| `Enter` | Run the selected command |
| `Alt-Enter` | Edit the command's source before running it |
| `F1` | Print the command's source |
| `F2` | Open the command where it is defined, in `$EDITOR` |
| `Tab` | Mark the row and move down, when several may be selected |
| `Shift-Tab` | Mark the row and move up |

Marks stick: search for one thing and mark a few rows, then search for
something else and mark more. `Enter` returns all of them, including the ones
the current search no longer shows. The count on the right reads
`matched/total (marked)`.
| `Esc`, `Ctrl-C` | Cancel; nixon exits 130 |

Moving:

| Key | Action |
|---|---|
| `Down`, `Ctrl-N`, `Ctrl-J` | Next candidate |
| `Up`, `Ctrl-P`, `Ctrl-K` | Previous candidate |
| `PgDn`, `Ctrl-V` / `PgUp`, `Alt-V` | A full page |
| `Alt-J` / `Alt-K` | Half a page |

Editing the query — and the same keys work in the edit-before-run editor,
where `Up`/`Down` move between lines and `Alt-Enter` inserts a newline:

| Key | Action |
|---|---|
| `Ctrl-A`, `Home` / `Ctrl-E`, `End` | Start / end of line |
| `Ctrl-B`, `Left` / `Ctrl-F`, `Right` | Back / forward one character |
| `Alt-B` / `Alt-F` | Back / forward one word |
| `Backspace`, `Ctrl-H` / `Delete`, `Ctrl-D` | Delete before / under the cursor |
| `Ctrl-W`, `Alt-Backspace` / `Alt-D` | Delete the word before / after the cursor |
| `Ctrl-U` | Delete to the start of the line |
| `Ctrl-Y` | Paste back what was last deleted |

The row you are on is highlighted, matched characters are picked out, and the
counts on the right read `matched/total`, with `(marked)` added when you are
selecting several.

### Shell widgets

The package installs widgets to `$out/share/nixon`. Source the one for your
shell to get:

| Key | Action |
|---|---|
| `Alt-i` | Run a command and insert what you pick from its output |
| `Alt-I` | Insert a command's source at the cursor |
| `Alt-p` | Insert a project's path |

``` shell
# ~/.bashrc
source /path/to/share/nixon/nixon-widget.bash

# ~/.zshrc
source /path/to/share/nixon/nixon-widget.zsh

# ~/.config/fish/config.fish
source /path/to/share/nixon/nixon-widget.fish
```

### Completion

Completion is generated by the binary itself, so command and project names are
completed from your actual configuration. Add the line for your shell:

``` shell
# bash
eval "$(COMPLETE=bash nixon)"

# zsh
eval "$(COMPLETE=zsh nixon)"

# fish
COMPLETE=fish nixon | source
```

## Changes from v1

v2 is a rewrite in Rust. Configuration files carry over unchanged; the
command line has a few deliberate differences.

- **The picker is built in.** `fzf` and `rofi` are no longer needed or used,
  and the `rofi` GUI mode is gone with them.
- **Removed flags.** `-b/--backend`, `-t/--terminal` and `-T/--force-tty` went
  with the backend concept. Passing one is now an ordinary argument error.
  Commands run in the terminal you started them from; a command marked `&`
  still detaches.
- **Exit codes are propagated.** v1 always exited 0; v2 exits with the
  command's status. Cancelling a selection exits 130.
- **The shell widgets are not compatible with v1's** — they used `-b fzf -T`,
  which no longer parse. Use the ones shipped with v2.
- **A missing config file is no longer fatal.** v1 exited 1 when
  `$XDG_CONFIG_HOME/nixon.md` did not exist; v2 treats it as empty.
  Malformed configuration is still an error.
- **A project found from a subdirectory** now resolves to the project root.
  v1 reported its parent directory with an empty name.
- **`type="…"` on a section heading** applies to the commands beneath it. v1
  documented this but only honoured it on command headings.
- **Git worktrees are discovered**, including those of bare repositories and
  those living outside `project_dirs`. Set `git_worktrees: false` in the
  config block to turn it off.

## Some history, inspirations & similar projects

`nixon` draws inspiration from several places:

First and foremost the combination of text and documentation with runnable code
undeniably draws inspiration from [literate
programming](https://en.wikipedia.org/wiki/Literate_programming). Although first
coined by Donald Knuth I never paid the idea of literate programming much
attention. That was until I [switched my main editor to
emacs](https://myme.no/posts/2017-12-24-move-to-spacemacs.html) and started
getting down and dirty with [Org Mode](https://orgmode.org/). Org mode's ability
to seamlessly evaluate code blocks written in all kinds of languages from a
single document was fascinating.

Secondly, when I first [got into
NixOS](https://myme.no/posts/2019-07-01-nixos-into-the-deep-end.html) I had
plenty of issues getting into the environments I wanted for my various projects
and ecosystems. I wanted a quick way to not have to think about `nix-shell` and
various other papercuts that I experienced.

`nixon` started off as a way for me to write and execute, in somewhat literate fashion,
Org Model-inspired style, small commands in a file I could dump into my various
projects. As the name indicates, it would also be `nix`-aware and run commands
in a `nix-shell`, if configured to do so. It also gained support for `direnv`.

The first version of `nixon` was written in `Haskell`, where one of the most
popular libraries, [Pandoc](https://pandoc.org/), is a document parser that
understands Org Mode syntax quite well. I eventually felt like `Markdown` was
more appropriate simply due to its popularity. v2 is a Rust rewrite, and parses
Markdown with [comrak](https://github.com/kivikakk/comrak).

After starting `nixon` I've become aware of various projects that has made
similar approaches to markdown-based code evaluation. Here's a short-list of
some of the projects I've both drawn inspiration from, and coincidentally landed
on similar approaches to:

 - [mask](https://github.com/jacobdeichert/mask) - A CLI task runner defined by a simple markdown file
 - [tesh](https://github.com/OceanSprint/tesh) - TEstable SHell sessions in Markdown
