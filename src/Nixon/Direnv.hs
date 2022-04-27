module Nixon.Direnv
  ( direnv_cmd,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.Text as T
import Nixon.Project (parents)
import Nixon.Types (Config (use_direnv), Env (config), Nixon, ask)
import Nixon.Utils (find_dominating_file)
import Turtle
  ( FilePath,
    MonadIO (liftIO),
    Text,
    dirname,
    format,
    fp,
    fromText,
    need,
  )
import Prelude hiding (FilePath, words)

-- | Convert a regular command to a direnv command
direnv_cmd :: NonEmpty Text -> FilePath -> Nixon (Maybe (NonEmpty Text))
direnv_cmd cmd path' =
  use_direnv . config <$> ask >>= \case
    Just True -> liftIO $
      runMaybeT $ do
        direnv_active <- maybe False find_path <$> need "DIRENV_DIR"
        if direnv_active
          then pure cmd
          else do
            _ <- MaybeT (fmap dirname <$> find_dominating_file path' ".envrc")
            lift . pure $ ("direnv" :| ["exec", format fp path'] <> toList cmd)
    _ -> pure Nothing
  where
    find_path = (`elem` parents path') . fromText . T.dropWhile (/= '/')
