module Envix.Nix
  ( find_nix_file
  , nix_files
  , nix_shell
  , nix_shell_spawn
  ) where

import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import Envix.Process
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
