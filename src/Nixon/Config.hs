module Nixon.Config
  ( find_local_config,
    read_config,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import qualified Data.Text as T
import Nixon.Config.Markdown (parseMarkdown)
import Nixon.Config.Types (Config, ConfigError (..))
import Nixon.Utils (find_dominating_file)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Turtle
  ( FilePath,
    MonadIO (..),
    readTextFile,
  )
import Prelude hiding (FilePath)

find_local_config :: MonadIO m => FilePath -> m (Maybe Config)
find_local_config path = runMaybeT $ do
  local_config <- MaybeT $ find_dominating_file path ".nixon.md"
  res <- read_config local_config
  MaybeT $ either onError (pure . Just) res
  where
    onError (ParseError err) = liftIO $ throwIO (ParseError err)
    onError _ = pure Nothing

read_config :: MonadIO m => FilePath -> m (Either ConfigError Config)
read_config path = do
  liftIO $
    tryIOError (readTextFile path) >>= \case
      Left err
        | isDoesNotExistError err -> pure (Left NoSuchFile)
        | otherwise -> ioError err
      Right c
        | T.dropWhile isSpace c == "" -> pure (Left EmptyFile)
        | otherwise -> pure $ first ParseError (parseMarkdown path c)
