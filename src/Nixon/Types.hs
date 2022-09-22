{-# LANGUAGE FlexibleInstances #-}

module Nixon.Types
  ( Env (..),
    Nixon,
    NixonError (..),
    Config.Config (commands, exact_match, project_dirs, project_types, terminal, use_direnv, use_nix),
    ask,
    runNixon,
  )
where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Nixon.Config.Types (BackendType (..), Config)
import qualified Nixon.Config.Types as Config
import Nixon.Logging (HasLogging)
import qualified Nixon.Logging as Logging
import Nixon.Utils (printErr)
import qualified System.IO as IO
import Turtle (Text)
import Prelude hiding (FilePath)

data Env = Env
  { backend :: BackendType,
    config :: Config
  }

data NixonError
  = EmptyError Text
  | NixonError Text
  deriving (Show)

instance Exception NixonError

get_backend :: MonadIO m => Maybe BackendType -> m BackendType
get_backend backend = do
  def_backend <- liftIO $ bool Rofi Fzf <$> IO.hIsTerminalDevice IO.stdin
  pure $ fromMaybe def_backend backend

-- | Merge the mess of CLI args, config file + user overrides (custom build)
build_env :: MonadIO m => Config -> m Env
build_env config = do
  backend <- get_backend (Config.backend config)
  pure Env {backend, config = config}

type Nixon = ReaderT Env IO

instance HasLogging Nixon where
  loglevel = fromMaybe Logging.LogWarning . Config.loglevel . config <$> ask
  logout = printErr

runNixon :: MonadIO m => Config -> ReaderT Env m a -> m a
runNixon config action = do
  env <- liftIO (build_env config)
  runReaderT action env
