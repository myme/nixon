module Nixon.Evaluator
  ( evaluate
  , getCacheDir
  , getEvaluator
  , writeCommand
  ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.Hash (hash, digestToHexByteString, SHA1)
import Crypto.Hash.Types (Digest)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Nixon.Command (Command(..), Language(..))
import Nixon.Direnv (direnv_cmd)
import Nixon.Nix (nix_cmd)
import Nixon.Process (Cwd, Env, Run)
import Nixon.Types (Nixon)
import Prelude hiding (FilePath)
import System.Directory (XdgDirectory(..), getXdgDirectory)
import Turtle hiding (basename, env, extension)

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

-- | Evaluate a command, possibly in a direnv/nix environment
evaluate :: Run Nixon a -> Command -> Cwd -> Env -> Nixon a
evaluate run cmd cwd env = join (getEvaluator run cmd cwd env)

extension :: Language -> Text
extension = \case
  Bash       -> ".sh"
  None       -> ".sh"
  Haskell    -> ".hs"
  JavaScript -> ".js"
  Python     -> ".py"
  Unknown _  -> ".txt"

interpreter :: MonadIO m => Language -> m (Maybe Text)
interpreter = \case
  Bash       -> pure $ Just "bash"
  None       -> need "SHELL" <&> (<|> Just "bash")
  Haskell    -> pure $ Just "runghc"
  JavaScript -> pure $ Just "node"
  Python     -> pure $ Just "python3"
  Unknown _  -> pure Nothing
