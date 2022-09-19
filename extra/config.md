# Nixon commands

## Config {.config}

```json
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
    { "name": "yarn", "test": ["yarn.lock"], "desc": "Yarn project"},
    { "name": "nix", "test": ["default.nix", "shell.nix"], "desc": "Nix project"},
    { "name": "direnv", "test": [".envrc"], "desc": "Direnv project"},
    { "name": "git", "test": [".git"], "desc": "Git repository"},
    { "name": "hg", "test": [".hg"], "desc": "Mercurial project"},
    { "name": "project", "desc": "Generic project"}
  ]
}
```

## Generic commands

### `terminal &`

```bash
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

### `hello-world ${json-greetings}`

```bash
echo ${json_greetings}
```

### `networks`

```bash
nmcli -t connection | cut -d':' -f1
```

### `network-connect`

```bash
nmcli connection up "$(nixon networks)"
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

```bash
npm run "$npm_scripts"
```

### `npm-install`

```bash
npm install
```

## yarn stuff {type="yarn"}

### `yarn-run ${npm-scripts}`

```bash
yarn run "$npm_scripts"
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
echo $git_log
```

### `git-show ${git-log}`

```bash
git show $(echo $git_log | cut -f1 -d' ')
```

## Files

### `rg-files`

```bash
rg --files
```

### `vim-file ${rg-files}`

```bash
vim "$rg_files"
```
