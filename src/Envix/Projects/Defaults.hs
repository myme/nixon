module Envix.Projects.Defaults
  ( default_projects
  ) where

import Envix.Nix
import Envix.Projects.Types
import Prelude hiding (FilePath)


-- TODO: Parse e.g. package.json for npm scripts?
-- TODO: Add support for local overrides with an .envix project file
-- TODO: Record commands made within a project and add to list
-- TODO: List descriptions
default_projects :: [ProjectType]
default_projects =
  [proj ["cabal.project"] "Cabal new-style project"
   ["cabal new-build" ! desc "Cabal build"
   ,"cabal new-repl" ! desc "Cabal repl"
   ,"cabal new-run" ! desc "Cabal run"
   ,"cabal new-test" ! desc "Cabal test"
   ]
  ,proj ["package.json"] "NPM project"
   ["npm install" ! desc "NPM install"
   ,"npm start" ! desc "NPM run"
   ,"npm test" ! desc "NPM test"
   ]
  ,proj (map ProjectPath nix_files) "Nix project"
   ["nix-build" ! desc "Nix build"
   ,"nix-shell" ! desc "Nix shell"
   ]
  ,proj [".envrc"] "Direnv project"
   ["direnv allow" ! desc "Direnv allow"
   ,"direnv deny" ! desc "Direnv deny"
   ,"direnv reload" ! desc "Direnv reload"
   ]
  ,proj [".git"] "Git repository"
   ["git blame" <> revision <> file ! desc "Git blame"
   ,"git fetch" ! desc "Git fetch"
   ,"git log" ! desc "Git log"
   ,"git rebase" ! desc "Git rebase"
   ,"git status" ! desc "Git status"
   -- Uses "git ls-files"
   ,"vim" <> file ! desc "Vim"
   ,"bat" <> file ! desc "Preview file"
   -- git log --color --pretty=format:"%C(green)%h %C(blue)%cr %Creset%s%C(yellow)%d %Creset%C(cyan)<%ae>%Creset" | fzf +s --ansi --preview='git show --color {1}'
   ]
  ,proj [".hg"] "Mercurial project" []
  ,proj [".project"] "Ad-hoc project" []
  ,proj [] "Generic project"
   ["x-terminal-emulator" ! desc "Terminal" <> gui
   ,"emacs" ! desc "Emacs" <> gui
   ,"dolphin" <> path ! desc "Files" <> gui
   ,"rofi -show run" ! desc "Run" <> gui
   ]
  ]
