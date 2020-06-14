module Nixon.Utils
 ( find_dominating_file
 , printErr
 , shell_to_list
 , toLines
 , takeToSpace
 ) where

import           Data.Char (isSpace)
import           Data.List.NonEmpty (toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (toLines)

-- | Locate a file going up the filesystem hierarchy
find_dominating_file :: FilePath -> FilePath -> IO (Maybe FilePath)
find_dominating_file path' name = do
  let candidate = path' </> name
      is_root = parent path' == root path'
  (&&) <$> testdir path' <*> testpath candidate >>= \case
    True -> pure $ Just candidate
    False | is_root -> pure Nothing
          | otherwise -> find_dominating_file (parent path') name

-- | Print a text message to stderr
printErr :: (MonadIO m) => Text -> m ()
printErr = liftIO . T.hPutStrLn IO.stderr

-- | Convert a Shell of as to [a]
shell_to_list :: MonadIO m => Shell a -> m [a]
shell_to_list shell' = fold shell' (Fold (flip (:)) [] reverse)

toLines :: Shell Text -> Shell Line
toLines = ((select . toList . textToLines) =<<)

takeToSpace :: Text -> Text
takeToSpace = T.takeWhile (not . isSpace)
