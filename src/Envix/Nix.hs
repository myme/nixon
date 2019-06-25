module Envix.Nix
  ( find_nix_file
  , nix_files
  , nix_shell
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import Envix.Process
import Prelude hiding (FilePath)
import Turtle

-- | Nix project files, in prioritized order
nix_files :: [FilePath]
nix_files = ["shell.nix"
            ,"default.nix"
            ]

-- | Return the path to a project's Nix file, if there is one
find_nix_file :: FilePath -> IO (Maybe FilePath)
find_nix_file dir = do
  let candidates = map (dir </>) nix_files
  existing <- filterM check_existing candidates
  return $ listToMaybe existing
  where check_existing path = (isRegularFile <$> stat path) `catch` handle_error
        handle_error :: IOException -> IO Bool
        handle_error _ = return False

-- | Fork and evaluate a command in a nix-shell
nix_shell :: FilePath -> Text -> IO ()
nix_shell nix_file command = do
  let nix_file' = format fp nix_file
  spawn "nix-shell" [nix_file', "--run", command]
