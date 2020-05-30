# Nixon

Project environment and command launcher.

## Usage

Query for a project and command:

``` shell
nixon
```

Simply run a command from the current directory:

``` shell
nixon run
```

## Build

The `build` sub-command allows building nixon with a custom configuration:

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Nixon
import Nixon.Config as Config
import Nixon.Project.Defaults
import Nixon.Project.Types

main :: IO ()
main = nixon_with_config config
  where config = default_config { Config.project_types = projects }
        projects = default_projects ++
          [proj ["package.json"] "My JavaScript projects"
           ["prettier" ! desc "Run prettier"]
           ["tslint" ! desc "Run tslint"]
          ]
```

``` shell
# nixon build INFLIE OUTFILE
nixon build custom_nixon.hs custom_nixon
```

NB: `build` requires a working `ghc`.

