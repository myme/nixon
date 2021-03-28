module Nixon.Nix
  ( find_nix_file
  , nix_files
  , nix_shell
  , nix_shell_spawn
  , nix_cmd
  ) where

import           Control.Monad (filterM)
import           Control.Monad.Trans.Maybe
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import           Nixon.Command
import           Nixon.Process
import           Nixon.Types hiding (Env)
import           Nixon.Utils
import           Prelude hiding (FilePath)
import           Turtle hiding (arg)

-- | Nix project files, in prioritized order
nix_files :: [FilePath]
nix_files = ["shell.nix"
            ,"default.nix"
            ]

-- | Return the path to a project's Nix file, if there is one
find_nix_file :: MonadIO m => FilePath -> m (Maybe FilePath)
find_nix_file dir = listToMaybe <$> filter_path nix_files
  where filter_path = filterM (testpath . (dir </>))

-- | Evaluate a command in a nix-shell
nix_shell :: MonadIO m => FilePath -> Maybe Text -> Env -> m ()
nix_shell = nix_run run

-- | Fork and evaluate a command in a nix-shell
nix_shell_spawn :: MonadIO m => FilePath -> Maybe Text -> Env -> m ()
nix_shell_spawn = nix_run spawn

nix_run :: MonadIO m => Run m a -> FilePath -> Maybe Text -> Env -> m a
nix_run run' nix_file cmd env' =
  let nix_file' = format fp nix_file
      cmd' = "nix-shell" :| build_args [
        pure [nix_file']
        , arg "--run" =<< cmd]
  in run' cmd' (Just $ parent nix_file) env'

nix_cmd :: Command -> FilePath -> Nixon (Maybe Command)
nix_cmd cmd path' = use_nix . config <$> ask >>= \case
  Just True -> liftIO $ runMaybeT $ do
    nix_file <-
      MaybeT (find_dominating_file path' "shell.nix") <|>
      MaybeT (find_dominating_file path' "default.nix")
    pure $ cmd {
      cmdSource = T.unwords
        ["nix-shell --command"
        ,quote $ cmdSource cmd
        ,format fp nix_file
        ]
    }
  _ -> pure Nothing
