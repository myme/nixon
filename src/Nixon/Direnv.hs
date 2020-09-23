module Nixon.Direnv
  ( direnv_cmd
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List (elemIndex)
import           Data.Text (intercalate, words)
import qualified Data.Text as T
import           Nixon.Command
import           Nixon.Project
import           Nixon.Types
import           Nixon.Utils
import           Prelude hiding (FilePath, words)
import           Turtle hiding (find, root)

-- | Convert a regular command to a direnv command
direnv_cmd :: Command -> FilePath -> Nixon (Maybe Command)
direnv_cmd cmd path' = use_direnv <$> ask >>= \case
  Just True -> liftIO $ runMaybeT $ do
    _ <- MaybeT (fmap find_path <$> need "DIRENV_DIR")
    _ <- MaybeT (fmap dirname <$> find_dominating_file path' ".envrc")
    let parts = ["direnv exec", format fp path'] ++ words (cmdSrc cmd)
    lift . pure $ cmd { cmdSrc = intercalate " " parts }
  _ -> pure Nothing
  where find_path = flip elemIndex (parents path') . fromText . T.dropWhile (/= '/')
