module Nixon.Evaluator
  ( evaluate,
    garbageCollect,
    getCacheDir,
    getEvaluator,
    writeCommand,
  )
where

import Control.Monad.Catch (bracket)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ask)
import Crypto.Hash (SHA1, digestToHexByteString, hash)
import Crypto.Hash.Types (Digest)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T
import Nixon.Command (Command (..))
import qualified Nixon.Config.Types as Config
import Nixon.Language (extension, interpreter)
import Nixon.Logging (log_debug, log_info)
import Nixon.Prelude
import Nixon.Process (Cwd, Env, RunArgs)
import qualified Nixon.Process as Proc
import Nixon.Types (Nixon)
import qualified Nixon.Types as T
import Nixon.Utils (fromPath, fromText, quote, shell_to_list)
import Nixon.Wrappers.Direnv (direnv_cmd)
import Nixon.Wrappers.Nix (nix_cmd)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import qualified System.IO as IO
import System.Posix.Signals (Handler (Catch, Default), installHandler, sigINT)
import Turtle
  ( Line,
    Shell,
    die,
    format,
    fp,
    join,
    ls,
    mktree,
    need,
    rm,
    w,
    (%),
    (</>),
  )

getCacheDir :: (MonadIO m) => m FilePath
getCacheDir = liftIO $ fromText . T.pack <$> getXdgDirectory XdgCache "nixon"

writeCommand :: (MonadIO m) => Command -> m FilePath
writeCommand cmd = do
  let content = cmdSource cmd
      sha1 = digestToHexByteString (hash (encodeUtf8 content) :: Digest SHA1)
      basename =
        fromText
          $ decodeUtf8 sha1
          <> "-"
          <> cmdName cmd
          <> extension (cmdLang cmd)

  path <- liftIO $ do
    cacheDir <- getCacheDir
    mktree cacheDir
    pure (cacheDir </> basename)

  liftIO $ T.writeFile (fromPath path) content
  pure path

-- | Clean up all cache files in $XDG_CACHE_DIR/nixon
garbageCollect :: (MonadIO m) => Bool -> m [Text]
garbageCollect dryRun = shell_to_list $ do
  files <- ls =<< getCacheDir
  if dryRun
    then pure $ format ("would remove " % fp) files
    else do
      rm files
      pure $ format ("removed " % fp) files

-- | Maybe wrap a command in direnv/nix.
maybeWrapCmd :: Cwd -> NonEmpty Text -> Nixon (NonEmpty Text)
maybeWrapCmd Nothing cmd = pure cmd
maybeWrapCmd (Just path) cmd =
  fmap (fromMaybe cmd)
    $ runMaybeT
    $ MaybeT (direnv_cmd cmd path)
    <|> MaybeT (nix_cmd cmd path)

-- | Provide an evaluator for a command, possibly in a direnv/nix environment
getEvaluator :: RunArgs input m a -> Command -> [Text] -> Cwd -> Env -> Maybe (Shell input) -> Nixon (m a)
getEvaluator run cmd args cwd env stdin = do
  path <- writeCommand cmd
  int <-
    interpreter (cmdLang cmd) >>= \case
      Nothing -> die $ format ("No interpreter for " % w) (cmdLang cmd)
      Just int -> pure int
  int_cmd <- maybeWrapCmd cwd (int <> (format fp path :| args))
  log_info (format ("Evaluating " % fp) path)
  pure $ run int_cmd cwd env stdin

-- | Run the evaluator for a command
withEvaluator :: RunArgs input Nixon a -> Command -> [Text] -> Cwd -> Env -> Maybe (Shell input) -> Nixon a
withEvaluator run cmd args cwd env stdin = join (getEvaluator run cmd args cwd env stdin)

-- | Evaluate a command, possibly in a direnv/nix environment
evaluate :: Command -> [Text] -> Cwd -> Env -> Maybe (Shell Line) -> Nixon ()
evaluate cmd args path env' stdin = do
  let source = cmdSource cmd

  log_info (format ("Running command " % w) source)
  log_debug (format ("Args: " % w) args)
  log_debug (format ("Env: " % w) env')

  isNotGUIBackend <- not . Config.isGuiBackend . T.backend <$> ask
  isTTY <- liftIO $ IO.hIsTerminalDevice IO.stdin
  forceTTY <- fromMaybe False . Config.force_tty . T.config <$> ask
  let useTTY = isNotGUIBackend && isTTY

  if useTTY || forceTTY
    then do
      -- Catch SIGINT to avoid killing the terminal, seems like child processes
      -- still get killed on ^C. Might need more tweaks.
      let setupHandler = liftIO $ installHandler sigINT (Catch (pure ())) Nothing
      let teardownHandler = liftIO $ installHandler sigINT Default Nothing
      bracket setupHandler (const teardownHandler) $ \_ ->
        withEvaluator Proc.run cmd args path env' stdin
    else
      if cmdIsBg cmd
        then withEvaluator Proc.spawn cmd args path env' stdin
        else do
          let end =
                T.unlines
                  [ "",
                    "echo -e " <> quote "\n[Press Return to exit]\n",
                    "read"
                  ]
              cmd' = cmd {cmdSource = cmdSource cmd <> end}
          term <-
            fmap (fromMaybe "x-terminal-emulator")
              $ runMaybeT
              $ MaybeT (Config.terminal . T.config <$> ask)
              <|> MaybeT (need "TERMINAL")
          withEvaluator (Proc.spawn . ((term :| ["-e"]) <>)) cmd' args path env' stdin
