# Nixon

## Config

The following source code block defines a `nixon` configuration using `YAML`:

``` yaml config
# Enable "exact" matching algorithm.
exact_match: true

# Ignore case by default.
ignore_case: true

# Evaluate direnv environments.
use_direnv: true

# Evaluate nix shell environments.
use_nix: true

# Location of project directories.
project_dirs:
  - ~/src

# Project types, with marker file detection.
project_types:
  - name: cabal
    test: ["cabal.project"]
    desc: Cabal new-style project,
  - name: npm
    test: ["package.json"]
    desc: NPM project,
  - name: nix
    test: ["flake.nix"]
    desc: Nix flake project,
  - name: git
    test: [".git"]
    desc: Git repository,
  - name: rust
    test: ["Cargo.toml"]
    desc: Rust project,
  - name: project
    desc: Generic project
```

## Generic commands

### `terminal &`

```
x-terminal-emulator
```

### `emacs &`

```bash
emacs
```

### `json-greetings` {.json}

```json
[
 {"title": "Norwegian", "value": "Hei, Verden!"},
 {"title": "English", "value": "Hello, World!"},
 {"title": "French", "value": "Bonjour, Monde!"}
]
```

### `hello-world`

Select from one or more greetings in a JSON format.

```bash <{json-greetings | multi}
cat
```

### `networks`

Use `nmcli` to list available networks.

```bash
nmcli connection
```

### `network-connect`

Use the `networks` placeholder to select a network to connect to.

```bash ${networks | cols 1}
nmcli connection up "$1"
```

### `pd`

The current project directory is available through the `$nixon_project_path`
environment variable.

```bash
echo "Project dir: $nixon_project_path"
```

## nix stuff

### `nix-build`

```bash
nix-build
```

### `nix-shell`

```bash
nix-shell
```

## npm stuff {type="npm"}

### `npm-scripts` {.json}

List all `npm` scripts in a `package.json`.

```bash
jq '.scripts | to_entries | map({ title: (.key + " → " + .value), value: .key })' package.json
```

### `npm-run`

Run a `npm` script from `package.json`.

```bash ${npm-scripts}
npm run "$1"
```

### `npm-install`

```bash
npm install
```

## yarn stuff {type="yarn"}

### `yarn-run`

```bash ${npm-scripts}
yarn run "$1"
```

### `yarn-install`

```bash
yarn install
```

## Cabal stuff {type="cabal"}

### `cabal-build`

```bash
cabal build
```

### `cabal-run`

```bash
cabal run
```

### `cabal-test`

```bash
cabal test
```

## Git stuff

### `git-log`

```bash
git log --oneline --color
```

### `git-files`

```bash
git ls-files
```

### `git-show`

Invoke `git show` on commits selected from `git log`. It uses multiple selection
and a field selector of `1` to pick the commit `SHA1` from the log.

```bash ${git-log | multi | fields 1}
git show "$@"
```

## Files

### `cat`

Basic check for `stdin` is readable when there are no `stdin` placeholder args.

```bash
cat
```

### `rg-files`

```bash
rg --files
```

### `vim-stdin`

Open files in `vim` passing files to open in through `stdin` and `xargs`.

```bash <{rg-files | multi}
xargs vim -p
```

### `vim-stdin-concat`

Open files in `vim` passing files to open in through `stdin` and `xargs`
concatenating every `stdin` input.

```bash <{git-files | multi} <{rg-files:m}
xargs vim -p
```

### `vim-arg`

Open files in `vim` passing files as a single positional argument.

```bash ${rg-files}
vim -p "$1"
```

### `vim-args`

Open files in `vim` passing files as positional arguments.

```bash ${rg-files:m}
vim -p "$@"
```

### `vim-env`

Open files in `vim` passing files in an environment variable.

```bash ={rg-files:m}
vim -p $rg_files
```

### `vim-env-alias`

Open files in `vim` passing files in an environment variable.

```bash FILES={rg-files | multi}
vim -p $FILES
```
