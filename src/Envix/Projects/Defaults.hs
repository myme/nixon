module Envix.Projects.Defaults
  ( default_projects
  ) where

import Envix.Projects.Types
import Envix.Nix
import Prelude hiding (FilePath)


-- TODO: Parse e.g. package.json for npm scripts?
-- TODO: Add associated action with each project type
-- e.g. for *.nix invoke nix-shell
--      for .git do a fetch?
--      This should be configurable.
-- This can then be paired up with a `--type <type>` cli arg to allow override
-- which action to run. This can obsolete `--no-nix` with `--type plain`.
-- TODO: Add support for local overrides with an .envix project file
-- TODO: Record commands made within a project and add to list
-- TODO: List descriptions
default_projects :: [ProjectType]
default_projects =
  [proj ["cabal.project"] "Cabal new-style project"
   ["cabal new-build" <> desc "Cabal build"
   ,"cabal new-repl" <> desc "repl"
   ,"cabal new-run" <> desc "run"
   ,"cabal new-test" <> desc "test"
   ]
  ,proj ["package.json"] "NPM project"
   ["npm install" <> desc "install"
   ,"npm start" <> desc "run"
   ,"npm test" <> desc "test"
   ]
  ,proj (map ProjectPath nix_files) "Nix project"
   ["nix-build" <> desc "build"
   ,"nix-shell" <> desc "shell"
   ]
  ,proj [".envrc"] "Direnv project"
   ["direnv allow" <> desc "direnv allow"
   ,"direnv deny" <> desc "direnv deny"
   ,"direnv reload" <> desc "direnv reload"
   ]
  ,proj [".git"] "Git repository"
   ["git fetch" <> desc "Git fetch"
   ,"git log" <> desc "Git log"
   ,"git rebase" <> desc "Git rebase"
   ,"git status" <> desc "Git status"
   -- git log --color --pretty=format:"%C(green)%h %C(blue)%cr %Creset%s%C(yellow)%d %Creset%C(cyan)<%ae>%Creset" | fzf +s --ansi --preview='git show --color {1}'
   ]
  ,proj [".hg"] "Mercurial project" []
  ,proj [".project"] "Ad-hoc project" []
  ,proj [ProjectFunc (const (pure True))] "Generic project"
   ["x-terminal-emulator" <> desc "Terminal"
   ,"emacs" <> desc "Emacs"
   ,"vim" <> desc "Vim"
   ,("dolphin" <> path) <> desc "Files"
   ,"rofi -show run" <> desc "Run"
   ]
  ]
