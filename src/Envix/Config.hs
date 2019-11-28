module Envix.Config
  ( Config(..)
  , build_config
  ) where

import           Envix.Config.Options (Options, Backend(..))
import qualified Envix.Config.Options as Options
import           Envix.Projects.Types (ProjectType)
import           Prelude hiding (FilePath)
import           Turtle

data Config = Config { backend :: Maybe Backend
                     , project_types :: [ProjectType]
                     , source_dirs :: [FilePath]
                     , use_direnv :: Bool
                     , use_nix :: Bool
                     }

-- | Merge the mess of CLI args, config file + user overrides (custom build)
build_config :: Options -> Config -> Config
build_config opts config = config
  { backend = backend config <|> Options.backend opts
  , source_dirs = source_dirs config ++ Options.source_dirs opts
  , use_direnv = use_direnv config || Options.use_direnv opts
  , use_nix = use_nix config || Options.use_nix opts
  }
