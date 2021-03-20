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

### terminal {.command .gui}

```bash
x-terminal-emulator
```

### emacs {.command .gui}

```bash
emacs
```

### networks {.command}

```bash
nmcli -t connection | cut -d':' -f1
```

### network-connect {.command}

```bash
nmcli connection up "$(nixon networks)"
```

## `nix` stuff

### nix-build {.command}

```bash
nix-build
```

### nix-shell {.command}

```bash
nix-shell
```

## `npm` stuff {type="npm"}

### npm-scripts {.command .json}

List all `npm` scripts in a `package.json`.

```bash
jq '.scripts | to_entries | map({ title: (.key + " → " + .value), value: .key })' package.json
```

### npm-run {.command}

```bash
npm run "$(nixon npm-scripts)"
```

### npm-install {.command}

```bash
npm install
```

## `yarn` stuff {type="yarn"}

### yarn-run {.command}

```bash
yarn run "$(nixon npm-scripts)"
```

### yarn-install {.command}

```bash
yarn install
```

## Cabal stuff {type="cabal"}

### cabal-build {.command}

```bash
cabal build
```

### cabal-run {.command}

```bash
cabal run
```

### cabal-test {.command}

```bash
cabal test
```

## Git stuff {type="git"}

### git-log {.command}

```bash
git log --oneline --color
```

### git-files {.command}

```bash
git ls-files
```

## Files

### rg-files {.command}

```bash
rg --files
```

### vim-file {.command}

```bash
vim "$(nixon rg-files)"
```
