module Nixon.Evaluator
  ( evaluate
  , getCacheDir
  , writeCommand
  ) where

import Crypto.Hash (hash, digestToHexByteString, SHA1)
import Crypto.Hash.Types (Digest)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Nixon.Command (Command(..), Language(..))
import Nixon.Process (Cwd, Env, Run)
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

evaluate :: MonadIO m => Run m a -> Command -> Cwd -> Env -> m a
evaluate run cmd cwd env = do
  path <- writeCommand cmd
  interpreter (cmdLang cmd) >>= \case
    Nothing  -> die $ format ("No interpreter for "%w) (cmdLang cmd)
    Just int -> run (int :| [format fp path]) cwd env

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
