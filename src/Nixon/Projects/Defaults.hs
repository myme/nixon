module Nixon.Projects.Defaults
  ( default_projects
  ) where

import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Nixon.Nix
import           Nixon.Projects
import           Nixon.Projects.Types
import qualified Nixon.Select as Select
import           Prelude hiding (FilePath)
import           Turtle

-- | Find NPM scripts from a project package.json file
npm_scripts :: Command
npm_scripts = Command [ShellPart "script" scripts] mempty
  where scripts project = fmap (Select.default_selection "") <$> Select.select $ do
          content <- liftIO $ runMaybeT $ do
            package <- MaybeT $ find_dominating_file (project_path project) "package.json"
            MaybeT $ decodeFileStrict (T.unpack $ format fp package)
          let keys = do
                map' <- parseMaybe parse_script =<< content
                mapM textToLine (Map.keys (map' :: Map.HashMap Text Value))
          select (fromMaybe [] keys)
        parse_script = withObject "package.json" (.: "scripts")

-- | Placeholder for a git revision
revision :: Command
revision = Command [ShellPart "revision" revisions] mempty
  where revisions project = do
          selection <- Select.select $ do
            pushd (project_path project)
            inshell "git log --oneline --color" mempty
          pure $ Select.default_selection "HEAD" (T.takeWhile (/= ' ') <$> selection)

-- TODO: Add support for local overrides with an .nixon project file
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
   ["npm run" <> npm_scripts ! desc "Run npm scripts"
   ,"npm install" ! desc "NPM install"
   ,"npm start" ! desc "NPM run"
   ,"npm test" ! desc "NPM test"
   ]
  ,proj [ProjectOr $ map ProjectFile nix_files] "Nix project"
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
