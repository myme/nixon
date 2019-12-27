module Nixon.Types
  ( Env(..)
  , Nixon
  , ask
  , runNixon
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import           Nixon.Config.Options (Options)
import qualified Nixon.Config.Options as Options
import           Nixon.Config.Types (Backend(..), LogLevel, Config)
import qualified Nixon.Config.Types as Config
import           Nixon.Projects.Types (ProjectType)
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (env)

data Env = Env { backend :: Backend
               , project_types :: [ProjectType]
               , source_dirs :: [FilePath]
               , use_direnv :: Bool
               , use_nix :: Bool
               , loglevel :: LogLevel
               }

get_backend :: Maybe Backend -> IO Backend
get_backend backend = do
  def_backend <- bool Rofi Fzf <$> IO.hIsTerminalDevice IO.stdin
  pure $ fromMaybe def_backend backend

-- | Merge the mess of CLI args, config file + user overrides (custom build)
build_env :: Options.Options -> Config.Config -> IO Env
build_env opts config = do
  backend <- get_backend (Config.backend config <|> Options.backend opts)
  pure Env
    { backend
    , project_types = Config.project_types config
    , source_dirs = Config.source_dirs config ++ Options.source_dirs opts
    , use_direnv = Config.use_direnv config || Options.use_direnv opts
    , use_nix = Config.use_nix config || Options.use_nix opts
    , loglevel = fromMaybe (Config.loglevel config) (Options.loglevel opts)
    }

type Nixon = ReaderT Env IO

runNixon :: MonadIO m => Options -> Config -> ReaderT Env m a -> m a
runNixon opts config action = do
  env <- liftIO (build_env opts config)
  runReaderT action env
