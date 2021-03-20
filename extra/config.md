# Nixon commands

## Config {.config}

```json
{
  "exact_match": true,
  "ignore_case": true,
  "use_direnv":true,
  "use_nix":true,
  "source_dirs": [
  ],
  "projects": [
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

### `terminal` {.gui}

```bash
x-terminal-emulator
```

### `emacs` {.gui}

```bash
emacs
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

### `npm-run`

```bash
npm run "$(nixon npm-scripts)"
```

### `npm-install`

```bash
npm install
```

## yarn stuff {type="yarn"}

### `yarn-run`

```bash
yarn run "$(nixon npm-scripts)"
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

## Files

### `rg-files`

```bash
rg --files
```

### `vim-file`

```bash
vim "$(nixon rg-files)"
```
