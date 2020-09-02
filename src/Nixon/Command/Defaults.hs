module Nixon.Command.Defaults
  ( default_commands
  ) where

import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Nixon.Command
import           Nixon.Project.Types
import           Nixon.Select (Candidate)
import qualified Nixon.Select as Select
import           Nixon.Utils
import           Turtle

candidate_lines :: Line -> Candidate
candidate_lines = Select.Identity . lineToText

-- | Find NPM scripts from a project package.json file
npm_scripts :: Command
npm_scripts = Command [ShellPart "script" scripts] mempty
  where
    parse_script = withObject "package.json" (.: "scripts")
    scripts project = Select.select $ do
      content <- liftIO $ runMaybeT $ do
        package <- MaybeT $ find_dominating_file (project_path project) "package.json"
        MaybeT $ decodeFileStrict (T.unpack $ format fp package)
      let elems = do
            map' <- parseMaybe parse_script =<< content
            pure (Map.toList (map' :: Map.HashMap Text Text))
      select $ maybe [] (map $ \(k, v) -> Select.WithTitle (format (s%" - "%s) k v) k) elems

-- | Placeholder for a git revision
revision :: Command
revision = Command [ShellPart "revision" revisions] mempty
  where revisions project = fmap (fmap takeToSpace) $ Select.select $ do
          pushd (project_path project)
          candidate_lines <$> inshell "git log --oneline --color" mempty

rg_files :: Command
rg_files = Command [ShellPart "filename" files] mempty
  where files project = Select.select $ do
          pushd (project_path project)
          candidate_lines <$> inshell "rg --files" mempty

git_files :: Command
git_files = Command [ShellPart "filename" files] mempty
  where files project = Select.select $ do
          pushd (project_path project)
          candidate_lines <$> inshell "git ls-files" mempty

default_commands :: [(Text, [Command])]
default_commands =
  [("cabal", ["cabal build" ! desc "Cabal build"
             ,"cabal repl" ! desc "Cabal repl"
             ,"cabal run" ! desc "Cabal run"
             ,"cabal test" ! desc "Cabal test"
             ])
  ,("npm", ["npm run" <> npm_scripts ! desc "Run npm scripts"
           ,"npm install" ! desc "NPM install"
           ,"npm start" ! desc "NPM run"
           ,"npm test" ! desc "NPM test"
           ])
  ,("yarn", ["yarn run" <> npm_scripts ! desc "Run yarn scripts"
            ,"yarn install" ! desc "Yarn install"
            ,"yarn start" ! desc "Yarn run"
            ,"yarn test" ! desc "Yarn test"
            ])
  ,("git", ["git blame" <> revision <> git_files ! desc "Git blame"
           ,"git fetch" ! desc "Git fetch"
           ,"git log" ! desc "Git log"
           ,"git rebase" ! desc "Git rebase"
           ,"git show" <> revision ! desc "Git show"
           ,"git status" ! desc "Git status"
           -- Uses "git ls-files"
           ,"vim" <> rg_files ! desc "Vim"
           ,"bat" <> rg_files ! desc "Preview file"
           -- git log --color --pretty=format:"%C(green)%h %C(blue)%cr %Creset%s%C(yellow)%d %Creset%C(cyan)<%ae>%Creset" | fzf +s --ansi --preview='git show --color {1}'
           ])
  ,("nix", ["nix-build" ! desc "Nix build"
           ,"nix-shell" ! desc "Nix shell"
           ])
  ,("direnv", ["direnv allow" ! desc "Direnv allow"
              ,"direnv deny" ! desc "Direnv deny"
              ,"direnv reload" ! desc "Direnv reload"
              ])
  ,("hg", [])
  ,("project", [])
  ]
