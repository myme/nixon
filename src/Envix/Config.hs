module Envix.Config
  ( Config(..)
  , default_config
  ) where

import           Envix.Options
import           Envix.Projects.Defaults
import           Envix.Projects.Types (ProjectType)
import           Prelude hiding (FilePath)

data Config = Config { options :: Options
                     , project_types :: [ProjectType]
                     }

default_config :: Config
default_config = Config { options = default_options
                        , project_types = default_projects
                        }
