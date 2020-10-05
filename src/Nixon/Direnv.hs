module Nixon.Direnv
  ( direnv_cmd
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Text as T
import           Nixon.Command
import           Nixon.Project
import           Nixon.Types
import           Nixon.Utils
import           Prelude hiding (FilePath, words)
import           Turtle hiding (find, root)

-- | Convert a regular command to a direnv command
direnv_cmd :: Command -> FilePath -> Nixon (Maybe Command)
direnv_cmd cmd path' = use_direnv . config <$> ask >>= \case
  Just True -> liftIO $ runMaybeT $ do
    direnv_active <- maybe False find_path <$> need "DIRENV_DIR"
    if direnv_active
      then pure cmd
      else do
        _ <- MaybeT (fmap dirname <$> find_dominating_file path' ".envrc")
        let parts = ["direnv exec ", TextPart $ format fp path', " "] ++ cmdParts cmd
        lift . pure $ cmd { cmdParts = parts }
  _ -> pure Nothing
  where find_path = (`elem` parents path') . fromText . T.dropWhile (/= '/')
