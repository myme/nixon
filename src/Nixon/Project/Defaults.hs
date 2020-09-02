module Nixon.Project.Defaults
  ( default_projects
  ) where

import Nixon.Nix
import Nixon.Project.Types

-- TODO: Add support for local overrides with an .nixon project file

-- | Definitions of project types, with an id, marker files and description.
default_projects :: [ProjectType]
default_projects =
  [proj "cabal"   ["cabal.project"] "Cabal new-style project"
  ,proj "npm"     ["package.json"] "NPM project"
  ,proj "yarn"    ["yarn.lock"] "Yarn project"
  ,proj "nix"     [ProjectOr $ map ProjectFile nix_files] "Nix project"
  ,proj "direnv"  [".envrc"] "Direnv project"
  ,proj "git"     [".git"] "Git repository"
  ,proj "hg"      [".hg"] "Mercurial project"
  ,proj "project" [".project"] "Generic project"
  ]
