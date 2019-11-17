module Envix.Config
  ( Config(..)
  , build_config
  , default_config
  ) where

import           Envix.Config.Options (Options, Backend(..))
import qualified Envix.Config.Options as Options
import           Envix.Projects.Defaults
import           Envix.Projects.Types (ProjectType)
import           Prelude hiding (FilePath)
import           Turtle

data Config = Config { backend :: Maybe Backend
                     , project_types :: [ProjectType]
                     , source_dirs :: [FilePath]
                     , use_direnv :: Bool
                     , use_nix :: Bool
                     }

default_config :: Config
default_config = Config { backend = Nothing
                        , project_types = default_projects
                        , source_dirs = []
                        , use_direnv = False
                        , use_nix = False
                        }

-- | Merge the mess of CLI args, config file + user overrides (custom build)
build_config :: Options -> Config -> Config
build_config opts config = config
  { backend = backend config <|> Options.backend opts
  , source_dirs = source_dirs config ++ Options.source_dirs opts
  , use_direnv = use_direnv config || Options.use_direnv opts
  , use_nix = use_nix config || Options.use_nix opts
  }
