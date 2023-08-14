# Nixon commands

## Config

The following source code block defines a `nixon` configuration using `JSON`:

``` json config
{
  "exact_match": true,
  "ignore_case": true,
  "use_direnv":true,
  "use_nix":true,
  "project_dirs": [
    "~/code/*",
    "~/code/**/*"
  ],
  "project_types": [
    { "name": "cabal", "test": ["cabal.project"], "desc": "Cabal new-style project"},
    { "name": "npm", "test": ["package.json"], "desc": "NPM project"},
    { "name": "yarn", "test": ["yarn.lock"], "desc": "Yarn project"},
    { "name": "nix", "test": ["default.nix", "shell.nix"], "desc": "Nix project"},
    { "name": "direnv", "test": [".envrc"], "desc": "Direnv project"},
    { "name": "git", "test": [".git"], "desc": "Git repository"},
    { "name": "hg", "test": [".hg"], "desc": "Mercurial project"},
    { "name": "project", "desc": "Generic project"}
  ]
}
```

`YAML` is also supported. Although in the following code block `yaml` is used as
opposed to `yaml config`. That is to keep this file a valid `nixon` file as only
one configuration block is allowed per file:

``` yaml
exact_match: true
ignore_case: true
use_direnv: true
use_nix: true
project_dirs:
  - ~/code/*
  - ~/code/**/*
project_types:
  - name: cabal
    test: ["cabal.project"]
    desc: Cabal new-style project
  - name: npm
    test: ["package.json"]
    desc: NPM project
  - name: yarn
    test: ["yarn.lock"]
    desc: Yarn project
  - name: nix
    test: ["default.nix", "shell.nix"]
    desc: Nix project
  - name: direnv
    test: [".envrc"]
    desc: Direnv project
  - name: git
    test: [".git"]
    desc: Git repository
  - name: hg
    test: [".hg"]
    desc: Mercurial project
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

### `hello-world <{json-greetings:m}`

```bash
cat
```

### `networks`

```bash
nmcli -t connection | cut -d':' -f1
```

### `network-connect ${networks}`

```bash
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

### `npm-run ${npm-scripts}`

Run a `npm` script from `package.json`.

```bash
npm run "$1"
```

### `npm-install`

```bash
npm install
```

## yarn stuff {type="yarn"}

### `yarn-run ${npm-scripts}`

```bash
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

## Git stuff {type="git"}

### `git-log`

```bash
git log --oneline --color
```

### `git-files`

```bash
git ls-files
```

### `git-rev ${git-log}`

```bash
echo "$1"
```

### `git-show ${git-rev}`

```bash
git show $(echo "$1" | cut -f1 -d' ')
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

### `vim-stdin <{rg-files:m}`

Open files in `vim` passing files to open in through `stdin` and `xargs`.

```bash
xargs vim -p
```

### `vim-stdin-concat <{git-files:m} <{rg-files:m}`

Open files in `vim` passing files to open in through `stdin` and `xargs`
concatenating every `stdin` input.

```bash
xargs vim -p
```

### `vim-arg ${rg-files}`

Open files in `vim` passing files as a single positional argument.

```bash
vim -p "$1"
```

### `vim-args ${rg-files:m}`

Open files in `vim` passing files as positional arguments.

```bash
vim -p "$@"
```

### `vim-env ={rg-files:m}`

Open files in `vim` passing files in an environment variable.

```bash
vim -p $rg_files
```

### `vim-env-alias FILES={rg-files:m}`

Open files in `vim` passing files in an environment variable.

```bash
vim -p $FILES
```
