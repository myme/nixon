module Nixon.Config.Types
  ( Backend(..)
  , LogLevel(..)
  , Config(..)
  , isGuiBackend
  ) where

import Nixon.Command (Command)
import Nixon.Logging
import Nixon.Project
import Prelude hiding (FilePath)
import Turtle (Text, FilePath)

data Backend = Fzf | Rofi deriving Show

isGuiBackend :: Backend -> Bool
isGuiBackend Fzf = False
isGuiBackend Rofi = True

data Config = Config
  { backend :: Maybe Backend
  , exact_match :: Maybe Bool
  , project_types :: [ProjectType]
  , commands :: [Command]
  , source_dirs :: [FilePath]
  , use_direnv :: Maybe Bool
  , use_nix :: Maybe Bool
  , terminal :: Maybe Text
  , loglevel :: LogLevel
  }
