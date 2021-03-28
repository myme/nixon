module Nixon.Direnv
  ( direnv_cmd
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.List.NonEmpty (NonEmpty((:|)), toList)
import qualified Data.Text as T
import           Nixon.Project
import           Nixon.Types
import           Nixon.Utils
import           Prelude hiding (FilePath, words)
import           Turtle hiding (find, root)

-- | Convert a regular command to a direnv command
direnv_cmd :: NonEmpty Text -> FilePath -> Nixon (Maybe (NonEmpty Text))
direnv_cmd cmd path' = use_direnv . config <$> ask >>= \case
  Just True -> liftIO $ runMaybeT $ do
    direnv_active <- maybe False find_path <$> need "DIRENV_DIR"
    if direnv_active
      then pure cmd
      else do
        _ <- MaybeT (fmap dirname <$> find_dominating_file path' ".envrc")
        lift . pure $ ("direnv" :| ["exec" , format fp path'] <> toList cmd)
  _ -> pure Nothing
  where find_path = (`elem` parents path') . fromText . T.dropWhile (/= '/')
