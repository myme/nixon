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

```bash <{json-greetings:m}
cat
```

### `networks`

```bash
nmcli -t connection | cut -d':' -f1
```

### `network-connect`

```bash ${networks}
nmcli connection up "$1"
```

### `pd`

```bash
cd "$nixon_project_path/../.."
echo -n "Current dir: "
pwd
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

```bash ${git-log:m1}
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

```bash <{rg-files:m}
xargs vim -p
```

### `vim-stdin-concat`

Open files in `vim` passing files to open in through `stdin` and `xargs`
concatenating every `stdin` input.

```bash <{git-files:m} <{rg-files:m}
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

```bash FILES={rg-files:m}
vim -p $FILES
```
