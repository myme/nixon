module Nixon.Logging
  ( log
  , log_debug
  , log_error
  , log_info
  , log_warn
  , printErr
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text
import qualified Data.Text.IO as T
import           Nixon.Types
import           Nixon.Config.Types (LogLevel(..))
import           Prelude hiding (log)
import qualified System.IO as IO

-- | Print a text message to stderr
printErr :: (MonadIO m) => Text -> m ()
printErr = liftIO . T.hPutStrLn IO.stderr

log :: LogLevel -> Text -> Nixon ()
log level msg = do
  should_log <- (level >=) . loglevel <$> ask
  when should_log $ printErr msg

log_debug :: Text -> Nixon ()
log_debug = log LogDebug

log_info :: Text -> Nixon ()
log_info = log LogInfo

log_warn :: Text -> Nixon ()
log_warn = log LogWarning

log_error :: Text -> Nixon ()
log_error = log LogError
