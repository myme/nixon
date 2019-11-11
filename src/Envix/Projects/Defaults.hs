module Envix.Projects.Defaults (project_types) where

import Envix.Nix
import Envix.Projects.Commands
import Envix.Projects.Types


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
project_types :: [ProjectType]
project_types =
  [proj ["cabal.project"] "Cabal new-style project"
   [term "cabal" ["new-build"] "build"
   ,term "cabal" ["new-repl"] "repl"
   ,term "cabal" ["new-run"] "run"
   ,term "cabal" ["new-test"] "test"
   ]
  ,proj ["package.json"] "NPM project"
   [term "npm" ["install"] "install"
   ,term "npm" ["start"] "run"
   ,term "npm" ["test"] "test"
   ]
  ,proj (map ProjectPath nix_files) "Nix project"
   [term "nix-build" [] "build"
   ,term "nix-shell" [] "shell"
   ]
  ,proj [".envrc"] "Direnv project"
   [term "direnv" ["allow"] "direnv allow"
   ,term "direnv" ["deny"] "direnv deny"
   ,term "direnv" ["reload"] "direnv reload"
   ]
  ,proj [".git"] "Git repository"
   [term "git" ["fetch"] "Git fetch"
   ,term "git" ["log"] "Git log"
   ,term "git" ["rebase"] "Git rebase"
   ,term "git" ["status"] "Git status"
   -- git log --color --pretty=format:"%C(green)%h %C(blue)%cr %Creset%s%C(yellow)%d %Creset%C(cyan)<%ae>%Creset" | fzf +s --ansi --preview='git show --color {1}'
   ]
  ,proj [".hg"] "Mercurial project" []
  ,proj [".project"] "Ad-hoc project" []
  ,proj [ProjectFunc (const (pure True))] "Generic project"
   [gui "x-terminal-emulator" [] "Terminal"
   ,gui "emacs" [] "Emacs"
   ,gui "vim" [] "Vim"
   ,gui "dolphin" [ArgPath] "Files"
   ,gui "rofi" ["-show", "run"] "Run"
   ]
  ]
