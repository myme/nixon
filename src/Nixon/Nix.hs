module Nixon.Nix
  ( find_nix_file
  , nix_files
  , nix_shell
  , nix_shell_spawn
  , nix_cmd
  ) where

import Control.Monad (filterM)
import Control.Monad.Trans.Maybe
import Data.Maybe (listToMaybe)
import Nixon.Types
import Nixon.Process
import Nixon.Project
import Nixon.Project.Types hiding (dir)
import Nixon.Utils
import Prelude hiding (FilePath)
import Turtle hiding (arg)

-- | Nix project files, in prioritized order
nix_files :: [FilePath]
nix_files = ["shell.nix"
            ,"default.nix"
            ]

-- | Return the path to a project's Nix file, if there is one
find_nix_file :: FilePath -> IO (Maybe FilePath)
find_nix_file dir = listToMaybe <$> filter_path nix_files
  where filter_path = filterM (testpath . (dir </>))

-- | Evaluate a command in a nix-shell
nix_shell :: FilePath -> Maybe Text -> IO ()
nix_shell = nix_run run

-- | Fork and evaluate a command in a nix-shell
nix_shell_spawn :: FilePath -> Maybe Text -> IO ()
nix_shell_spawn = nix_run spawn

type Runner = [Text] -> Maybe FilePath -> IO ()

nix_run :: Runner -> FilePath -> Maybe Text -> IO ()
nix_run run' nix_file cmd = do
  let nix_file' = format fp nix_file
      args = build_args [pure [nix_file']
                        , arg "--run" =<< cmd]
  run' ("nix-shell" : args) (Just $ parent nix_file)

nix_cmd :: Command -> FilePath -> Nixon (Maybe Command)
nix_cmd cmd path' = use_nix <$> ask >>= \case
  Just True -> liftIO $ runMaybeT $ do
    nix_file <-
      MaybeT (find_dominating_file path' "shell.nix") <|>
      MaybeT (find_dominating_file path' "default.nix")
    let parts =
          ["nix-shell"
          ,"--command"
          ,NestedPart (command_parts cmd)
          ,TextPart (format fp nix_file)
          ]
    pure cmd { command_parts = parts }
  _ -> pure Nothing
