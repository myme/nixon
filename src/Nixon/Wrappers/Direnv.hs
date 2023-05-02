module Nixon.Wrappers.Direnv
  ( direnv_cmd,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.Text as T
import Nixon.Prelude
import Nixon.Project (parents)
import Nixon.Types (Config (use_direnv), Env (config), Nixon, ask)
import Nixon.Utils (find_dominating_file)
import Turtle
  ( dirname,
    format,
    fp,
    fromText,
    need,
  )

-- | Convert a regular command to a direnv command
direnv_cmd :: NonEmpty Text -> FilePath -> Nixon (Maybe (NonEmpty Text))
direnv_cmd cmd path' = ask >>= wrapCmd . use_direnv . config
  where
    wrapCmd = \case
      Just True -> liftIO $
        runMaybeT $ do
          direnv_active <- maybe False find_path <$> need "DIRENV_DIR"
          if direnv_active
            then pure cmd
            else do
              _ <- MaybeT (fmap dirname <$> find_dominating_file path' ".envrc")
              lift . pure $ ("direnv" :| ["exec", format fp path'] <> toList cmd)
      _ -> pure Nothing
    find_path = (`elem` parents path') . fromText . T.dropWhile (/= '/')
