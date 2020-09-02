module Nixon.Direnv
  ( direnv_cmd
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List (elemIndex)
import qualified Data.Text as T
import           Nixon.Command
import           Nixon.Types
import           Nixon.Project
import           Nixon.Utils
import           Prelude hiding (FilePath)
import           Turtle hiding (find, root)

-- | Convert a regular command to a direnv command
direnv_cmd :: Command -> FilePath -> Nixon (Maybe Command)
direnv_cmd cmd path' = use_direnv <$> ask >>= \case
  Just True -> liftIO $ runMaybeT $ do
    _ <- MaybeT (fmap find_path <$> need "DIRENV_DIR")
    _ <- MaybeT (fmap dirname <$> find_dominating_file path' ".envrc")
    let (cmd':args) = command_parts cmd
        parts = ["direnv exec", TextPart (format fp path'), cmd'] ++ args
    lift . pure $ cmd { command_parts = parts }
  _ -> pure Nothing
  where find_path = flip elemIndex (parents path') . fromText . T.dropWhile (/= '/')
