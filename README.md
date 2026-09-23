# Nixon

Project environment and command launcher.

`nixon` reads `nixon.md` files, finds the commands in them, lets you pick one
with a built-in fuzzy picker, and runs it — optionally inside `direnv` or
`nix-shell`. The terminal picker needs no `fzf` or `rofi` install.

## Install

With flakes:

``` shell
nix run github:myme/nixon
nix profile install github:myme/nixon
```

Or build from a checkout:

``` shell
nix build
./result/bin/nixon --help
```

To preview the graphical menu from a checkout, enter `nix develop` and run
`cargo run -- -m gui`. Re-enter the dev shell after updating it so the GUI
runtime library paths take effect.

## Quick start

Write a `nixon.md` in a project:

~~~~~~markdown
# My project

## Config

``` yaml config
use_direnv: true
project_dirs:
  - ~/src
```

### `hello`

Says hello.

```bash
echo "Hello, World!"
```

### `git-files`

```bash
git ls-files
```

### `vim-file`

Picks a tracked file and opens it.

```bash ${git-files}
vim "$1"
```
~~~~~~

Then pick a command and run it:

``` shell
nixon
```

Run one by name — a name matching exactly one command skips the picker:

``` shell
nixon hello
```

Pick a project first, then a command in it:

``` shell
nixon project
```

## Documentation

- [Configuration](docs/configuration.md) — where config files live, how they
  merge, and every setting.
- [Commands](docs/commands.md) — how a markdown heading becomes a runnable
  command.
- [Placeholders](docs/placeholders.md) — commands that feed other commands.
- [Command line](docs/cli.md) — every subcommand and flag.
- [The picker](docs/picker.md) — keys, matching and auto-selection.
- [Shell integration](docs/shell-integration.md) — widgets, completion and
  scripting.

There is also an inspirational configuration under
[extra/config.md](./extra/config.md).

Installed as man pages: `nixon(1)`, `nixon.md(5)`, `nixon-picker(7)` and
`nixon-shell(7)`.

## Changes from v1

v2 is a rewrite in Rust. Configuration files carry over unchanged; the
command line has a few deliberate differences.

- **The picker is built in.** `fzf` and `rofi` are no longer needed or used.
  `nixon --mode gui` opens a graphical menu. Commands opens a graphical picker;
  Enter resolves the selected command and hands foreground runs to an external
  terminal. Browser opens a URL or searches from a focused input. Spotify
  (default **S**) controls playback through the player's
  MPRIS session bus service on Linux; a missing service is shown in the
  window. Configured command actions run a named command in the current or a
  selected project with the same prompts and terminal handoff. Projects picks
  a project and then a command to run there. F1 shows command source or project
  details in a read-only panel with Copy and Back. History remains a preview.
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
- **Any `-x`/`--x` token in a command heading is now an option** the user can
  toggle at the prompt. A v1 heading that carried a command line for
  decoration — `` ### `show git log --oneline -n 20` `` — declares those
  flags as options; move the command line into the code block.

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
