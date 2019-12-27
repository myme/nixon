module Nixon.Config.Types
  ( Backend(..)
  , LogLevel(..)
  , Config(..)
  ) where

import Nixon.Projects.Types
import Prelude hiding (FilePath)
import Turtle (FilePath)

data Backend = Fzf | Rofi deriving Show

data LogLevel = LogDebug | LogInfo | LogWarning | LogError
  deriving (Eq, Ord, Show)

data Config = Config { backend :: Maybe Backend
                     , project_types :: [ProjectType]
                     , source_dirs :: [FilePath]
                     , use_direnv :: Bool
                     , use_nix :: Bool
                     , loglevel :: LogLevel
                     }
