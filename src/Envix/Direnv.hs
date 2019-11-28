module Envix.Direnv
  ( direnv_cmd
  ) where

import           Data.Bool (bool)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Envix.Config
import qualified Envix.Config as Config
import           Envix.Projects
import           Envix.Projects.Types
import           Prelude hiding (FilePath)
import           Turtle

-- | Convert a regular command to a direnv command
direnv_cmd :: Config -> Command -> FilePath -> IO Command
direnv_cmd config cmd path'
  | not (Config.use_direnv config) = pure cmd
  | otherwise = maybe True (not . path_in_env) <$> need "DIRENV_DIR" >>=
    bool (pure cmd) (
      isJust <$> find_dominating_file path' ".envrc" >>=
      bool (pure cmd) (
        let (cmd':args) = command_parts cmd
            parts = ["direnv exec" , TextPart (format fp path') , cmd'] ++ args
        in pure cmd { command_parts = parts }))
    where path_in_env = (`elem` parents path') . fromText . T.dropWhile (/= '/')
