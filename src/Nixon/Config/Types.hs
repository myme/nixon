module Nixon.Config.Types
  ( Backend(..)
  , LogLevel(..)
  , Config(..)
  ) where

import Nixon.Logging
import Nixon.Command (Command)
import Nixon.Project.Types
import Prelude hiding (FilePath)
import Turtle (FilePath)
import Data.Text (Text)

data Backend = Fzf | Rofi deriving Show

data Config = Config { backend :: Maybe Backend
                     , exact_match :: Maybe Bool
                     , project_types :: [ProjectType]
                     , commands :: [(Text, [Command])]
                     , source_dirs :: [FilePath]
                     , use_direnv :: Maybe Bool
                     , use_nix :: Maybe Bool
                     , loglevel :: LogLevel
                     }
