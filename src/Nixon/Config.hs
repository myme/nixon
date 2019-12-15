module Nixon.Config
  ( Config(..)
  , LogLevel(..)
  , Nixon
  , ask
  , build_config
  , runNixon
  ) where

import           Control.Monad.Trans.Reader
import           Data.Maybe (fromMaybe)
import           Nixon.Config.Options (Options, Backend(..), LogLevel(..))
import qualified Nixon.Config.Options as Options
import           Nixon.Projects.Types (ProjectType)
import           Prelude hiding (FilePath)
import           Turtle

data Config = Config { backend :: Maybe Backend
                     , project_types :: [ProjectType]
                     , source_dirs :: [FilePath]
                     , use_direnv :: Bool
                     , use_nix :: Bool
                     , loglevel :: LogLevel
                     }

-- | Merge the mess of CLI args, config file + user overrides (custom build)
build_config :: Options -> Config -> Config
build_config opts config = config
  { backend = backend config <|> Options.backend opts
  , source_dirs = source_dirs config ++ Options.source_dirs opts
  , use_direnv = use_direnv config || Options.use_direnv opts
  , use_nix = use_nix config || Options.use_nix opts
  , loglevel = fromMaybe (loglevel config) (Options.loglevel opts)
  }

type Nixon = ReaderT Config IO

runNixon :: Config -> ReaderT Config m a -> m a
runNixon = flip runReaderT
