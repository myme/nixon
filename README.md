# Envix

Project environment and command launcher.

## Usage

Query for a project and command:

``` shell
envix
```

Simply run a command from the current directory:

``` shell
envix run
```

## Build

The `build` sub-command allows building envix with a custom configuration:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Envix
import Envix.Config as Config
import Envix.Projects.Defaults
import Envix.Projects.Types

main :: IO ()
main = envix_with_config config
  where config = default_config { Config.project_types = projects }
        projects = default_projects ++
          [proj ["package.json"] "My JavaScript projects"
           ["prettier" ! desc "Run prettier"]
           ["tslint" ! desc "Run tslint"]
          ]
```

``` shell
# envix build INFLIE OUTFILE
envix build custom_envix.hs custom_envix
```

NB: `build` requires a working `ghc`.

