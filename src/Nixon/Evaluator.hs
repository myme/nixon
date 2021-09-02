module Nixon.Evaluator
  ( evaluate
  , getCacheDir
  , getEvaluator
  , writeCommand
  ) where

import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader (ask)
import           Crypto.Hash (hash, digestToHexByteString, SHA1)
import           Crypto.Hash.Types (Digest)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Nixon.Command (Command(..))
import qualified Nixon.Config.Types as Config
import           Nixon.Direnv (direnv_cmd)
import           Nixon.Language (extension, interpreter)
import           Nixon.Logging (log_info)
import           Nixon.Nix (nix_cmd)
import           Nixon.Process (Cwd, Env, Run)
import qualified Nixon.Process as Proc
import           Nixon.Types (Nixon)
import qualified Nixon.Types as T
import           Nixon.Utils (quote)
import           Prelude hiding (FilePath)
import           System.Directory (XdgDirectory(..), getXdgDirectory)
import qualified System.IO as IO
import           Turtle hiding (basename, env, extension)

getCacheDir :: MonadIO m => m FilePath
getCacheDir = liftIO $ decodeString <$> getXdgDirectory XdgCache "nixon"

writeCommand :: MonadIO m => Command -> m FilePath
writeCommand cmd = liftIO $ do
  cacheDir <- getCacheDir
  mktree cacheDir
  let content = cmdSource cmd
      sha1 = digestToHexByteString (hash (encodeUtf8 content) :: Digest SHA1)
      basename = fromText
        $ decodeUtf8 sha1
        <> "-"
        <> cmdName cmd
        <> extension (cmdLang cmd)
      path = cacheDir </> basename
  writeTextFile path content
  pure path

-- | Maybe wrap a command in direnv/nix.
maybeWrapCmd :: Cwd -> NonEmpty Text -> Nixon (NonEmpty Text)
maybeWrapCmd Nothing cmd = pure cmd
maybeWrapCmd (Just path) cmd = fmap (fromMaybe cmd) $ runMaybeT
   $  MaybeT (direnv_cmd cmd path)
  <|> MaybeT (nix_cmd cmd path)

-- | Provide an evaluator for a command, possibly in a direnv/nix environment
getEvaluator :: Run m a -> Command -> Cwd -> Env -> Nixon (m a)
getEvaluator run cmd cwd env = do
  path <- writeCommand cmd
  int  <- interpreter (cmdLang cmd) >>= \case
    Nothing  -> die $ format ("No interpreter for "%w) (cmdLang cmd)
    Just int -> pure int
  int_cmd <- maybeWrapCmd cwd (int :| [format fp path])
  pure $ run int_cmd cwd env

-- | Run the evaluator for a command
withEvaluator :: Run Nixon a -> Command -> Cwd -> Env -> Nixon a
withEvaluator run cmd cwd env = join (getEvaluator run cmd cwd env)

-- | Evaluate a command, possibly in a direnv/nix environment
evaluate :: Command -> Cwd -> Env -> Nixon ()
evaluate cmd path env' = do
  isTTY <- (&&) <$> (not . Config.isGuiBackend . T.backend <$> ask) <*> liftIO (IO.hIsTerminalDevice IO.stdin)
  forceTTY <- fromMaybe False . Config.force_tty . T.config <$> ask
  let source = cmdSource cmd
  log_info (format ("Running command "%w) source)
  if isTTY || forceTTY
    then withEvaluator Proc.run cmd path env'
    else if cmdIsBg cmd
      then withEvaluator Proc.spawn cmd path env'
      else do
        let end = T.unlines
                    [""
                    ,"echo -e " <> quote "\n[Press Return to exit]\n"
                    ,"read"
                    ]
            cmd' = cmd { cmdSource = cmdSource cmd <> end }
        term <- fmap (fromMaybe "x-terminal-emulator") $ runMaybeT
           $  MaybeT (Config.terminal . T.config <$> ask)
          <|> MaybeT (need "TERMINAL")
        withEvaluator (Proc.spawn . ((term :| ["-e"]) <>)) cmd' path env'
