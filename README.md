# Nixon

Project environment and command launcher.

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

```bash ${git-files:m}
vim -p "$@"
```

### `vim-stdin`

The `stdin` placeholder may be used to select candidates that will be passed to
the command's `stdin`. Here we're using the `xargs` command to relay that as
positional arguments to `vim`.

```bash <{git-files:m}
xargs vim -p
```

### `vim-env`

The `environment variable` placeholder places the selection of a placeholder
into an environmental variable. The `environment variable` is named after the
placeholder action with `-` *(dashes)* replaced by `_` *(underscore)*,
`git_files` in this case.

```bash ={git-files:m}
vim -p $git_files
```

### `vim-env-alias`

It is possible to give the environment variable an explicit name by placing and
alias before the `=`, in this case `FILES`.

```bash FILES={git-files:m}
vim -p $FILES
```

~~~~~~

## Usage

Run a command from the current directory:

``` shell
nixon
```

Query for a project and command:

``` shell
nixon project
```

Help text:

```
❯ nixon --help
Command & environment launcher

Usage: nixon [-C|--config CONFIG] [-b|--backend BACKEND]
             [(-e|--exact) | --no-exact] [(-i|--ignore-case) | --no-ignore-case]
             [(-T|--force-tty) | --no-force-tty] [-p|--path PATH]
             [(-d|--direnv) | --no-direnv] [(-n|--nix) | --no-nix]
             [-t|--terminal TERMINAL] [-L|--loglevel LOGLEVEL]
             [eval | gc | project | run | [command] [args...] [-l|--list]
               [-s|--select]]

Available options:
  -h,--help                Show this help text
  -C,--config CONFIG       Path to configuration file (default:
                           ~/.config/nixon.md)
  -b,--backend BACKEND     Backend to use: fzf, rofi
  -e,--exact               Enable exact match
  -i,--ignore-case         Case-insensitive match
  -T,--force-tty           Never fork or spawn off separate processes
  -p,--path PATH           Project directory
  -d,--direnv              Evaluate .envrc files using `direnv exec`
  -n,--nix                 Invoke nix-shell if *.nix files are found
  -t,--terminal TERMINAL   Terminal emultor for non-GUI commands
  -L,--loglevel LOGLEVEL   Loglevel: debug, info, warning, error
  command                  Command to run
  args...                  Arguments to command
  -l,--list                List commands
  -s,--select              Select a command and output on stdout

Available commands:
  eval                     Evaluate expression
  gc                       Garbage collect cached items
  project                  Project actions
  run                      Run command
```

### FZF selection bindings

There are some additional bindings available when selection commands through the
`fzf` interface:

 <dl>
  <dt>Return</dt>
  <dd>Primary selection</dd>
  <dt>Alt-Return</dt>
  <dd>Edit selection before execution</dd>
  <dt>F1</dt>
  <dd>Print out the source of the selected command</dd>
  <dt>F2</dt>
  <dd>Jump to the selected command in `$EDITOR`</dd>
</dl>

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

Although this project is written in `Haskell` and one of the most popular
`Haskell` libraries, [Pandoc](https://pandoc.org/), is a document parser that
understand Org Mode syntax quite well, I eventually felt like `Markdown` was
more appropriate simply due to its popularity.

After starting `nixon` I've become aware of various projects that has made
similar approaches to markdown-based code evaluation. Here's a short-list of
some of the projects I've both drawn inspiration from, and coincidentally landed
on similar approaches to:

 - [mask](https://github.com/jacobdeichert/mask) - A CLI task runner defined by a simple markdown file
 - [tesh](https://github.com/OceanSprint/tesh) - TEstable SHell sessions in Markdown
