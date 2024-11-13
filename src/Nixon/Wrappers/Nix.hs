module Nixon.Wrappers.Nix
  ( find_nix_file,
    nix_files,
    nix_shell,
    nix_shell_spawn,
    nix_cmd,
  )
where

import Control.Monad (filterM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Nixon.Prelude
import Nixon.Process (Env, RunArgs, arg, build_args, run, spawn)
import Nixon.Types (Config (use_nix), Nixon, ask, config)
import Nixon.Utils (find_dominating_file, quote)
import Turtle
  ( Line,
    Shell,
    format,
    fp,
    parent,
    testpath,
    (</>),
  )

-- | Nix project files, in prioritized order
nix_files :: [FilePath]
nix_files =
  [ "shell.nix",
    "default.nix"
  ]

-- | Return the path to a project's Nix file, if there is one
find_nix_file :: (MonadIO m) => FilePath -> m (Maybe FilePath)
find_nix_file dir = listToMaybe <$> filter_path nix_files
  where
    filter_path = filterM (testpath . (dir </>))

-- | Evaluate a command in a nix-shell
nix_shell :: (MonadIO m) => FilePath -> Maybe Text -> Env -> Maybe (Shell Line) -> m ()
nix_shell = nix_run run

-- | Fork and evaluate a command in a nix-shell
nix_shell_spawn :: (MonadIO m) => FilePath -> Maybe Text -> Env -> Maybe (Shell Line) -> m ()
nix_shell_spawn = nix_run spawn

nix_run :: (MonadIO m) => RunArgs input m a -> FilePath -> Maybe Text -> Env -> Maybe (Shell input) -> m a
nix_run run' nix_file cmd env' stdin =
  let nix_file' = format fp nix_file
      cmd' =
        "nix-shell"
          :| build_args
            [ pure [nix_file'],
              arg "--run" =<< cmd
            ]
   in run' cmd' (Just $ parent nix_file) env' stdin

nix_cmd :: NonEmpty Text -> FilePath -> Nixon (Maybe (NonEmpty Text))
nix_cmd cmd path' = ask >>= wrapCmd . use_nix . config
  where
    wrapCmd = \case
      Just True -> liftIO
        $ runMaybeT
        $ do
          nix_file <-
            MaybeT (find_dominating_file path' "shell.nix")
              <|> MaybeT (find_dominating_file path' "default.nix")
          pure ("nix-shell" :| ["--command", quote $ T.unwords $ toList cmd, format fp nix_file])
      _ -> pure Nothing
