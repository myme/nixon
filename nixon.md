# Nixon commands

## Haskell commands

### `run`

Run the main application.

```bash
cabal run nixon
```

### `tdd`

Run a reloading [ghcid](https://github.com/ndmitchell/ghcid) session of the test
suite.

```bash
ghcid --target=nixon --run=":! ghcid --target=nixon-test --run"
```
