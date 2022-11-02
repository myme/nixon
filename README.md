# Nixon

Project environment and command launcher.

## Configuration

Example configuration:

~~~~~~markdown

# Nixon

## Config {.config}

``` json
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

## Commands

### `hello-sh`

This is a basic shell command with a description.

```
echo "Hello, World!"
```

### `hello-python`

This is a Python command.

```python
print("Hello, World!")
```

### `terminal &`

Spawn a terminal as a background task.

```bash
x-terminal-emulator
```

## Git stuff {type="git"}

Commands for projects using `git`. Detected by the `project_types` test for a `.git` directory.

### `git-files`

```bash
git ls-files
```

### `vim-file ${git-files}`

Use placeholder argument to select a single file to edit in `vim`.

```bash
vim "$1"
```

### `vim-files ${git-files:m}`

Use multi-line placeholder to select a list of files to edit in `vim` tabs.

```bash
vim -p "$@"
```

### `vim-stdin <{git-files:m}`

Use stdin placeholder to select a list of files to edit in `vim` tabs handled by `xargs`.

```bash
xargs vim -p
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
Launch project environment

Usage: nixon [-C|--config CONFIG] [-b|--backend BACKEND]
             [(-e|--exact) | --no-exact] [(-i|--ignore-case) | --no-ignore-case]
             [(-T|--force-tty) | --no-force-tty] [-p|--path PATH]
             [(-d|--direnv) | --no-direnv] [(-n|--nix) | --no-nix]
             [-t|--terminal TERMINAL] [-l|--loglevel LOGLEVEL]
             [project | run | [COMMAND] [-l|--list] [-s|--select]]

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
  -l,--loglevel LOGLEVEL   Loglevel: debug, info, warning, error
  COMMAND                  Command to run
  -l,--list                List commands
  -s,--select              Select a command and output on stdout

Available commands:
  project                  Project actions
  run                      Run command
```

