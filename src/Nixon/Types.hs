{-# LANGUAGE FlexibleInstances #-}

module Nixon.Types
  ( Env(..)
  , Nixon
  , NixonError(..)
  , ask
  , runNixon
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import           Nixon.Command (Command)
import           Nixon.Config.Options (Options)
import qualified Nixon.Config.Options as Options
import           Nixon.Config.Types (Backend(..), Config)
import qualified Nixon.Config.Types as Config
import           Nixon.Logging (HasLogging, LogLevel(..))
import qualified Nixon.Logging as Logging
import           Nixon.Project.Types (ProjectType)
import           Nixon.Utils
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (env)

data Env = Env { backend :: Backend
               , exact_match :: Maybe Bool
               , project_types :: [ProjectType]
               , commands :: [(Text, [Command])]
               , source_dirs :: [FilePath]
               , use_direnv :: Maybe Bool
               , use_nix :: Maybe Bool
               , loglevel :: LogLevel
               }

newtype NixonError = EmptyError Text deriving Show

instance Exception NixonError

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
    , exact_match =  Options.exact_match opts <|> Config.exact_match config
    , project_types = Config.project_types config
    , commands = Config.commands config
    , source_dirs = Config.source_dirs config ++ Options.source_dirs opts
    , use_direnv =  Options.use_direnv opts <|> Config.use_direnv config
    , use_nix =  Options.use_nix opts <|> Config.use_nix config
    , loglevel = fromMaybe (Config.loglevel config) (Options.loglevel opts)
    }

type Nixon = ReaderT Env IO

instance HasLogging Nixon where
  loglevel = loglevel <$> ask
  logout = printErr

runNixon :: MonadIO m => Options -> Config -> ReaderT Env m a -> m a
runNixon opts config action = do
  env <- liftIO (build_env opts config)
  runReaderT action env
