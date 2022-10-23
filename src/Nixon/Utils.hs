module Nixon.Utils
  ( escape,
    find_dominating_file,
    first_word,
    quote,
    printErr,
    shell_to_list,
    toLines,
    takeToSpace,
    filter_elems,
    implode_home,
    (<<),
  )
where

import Data.Char (isSpace)
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import Turtle
  ( FilePath,
    Fold (Fold),
    Line,
    MonadIO (..),
    Shell,
    Text,
    fold,
    home,
    parent,
    root,
    select,
    stripPrefix,
    testdir,
    testpath,
    textToLines,
    (</>),
  )
import Prelude hiding (FilePath)

escape :: Text -> Text
escape = T.concatMap convert
  where
    convert '"' = "\\\""
    convert '\\' = "\\\\"
    convert x = T.singleton x

quote :: Text -> Text
quote input = "\"" <> escape input <> "\""

-- | Locate a file going up the filesystem hierarchy
find_dominating_file :: MonadIO m => FilePath -> FilePath -> m (Maybe FilePath)
find_dominating_file path' name = do
  let candidate = path' </> name
      is_root = parent path' == root path'
  (&&) <$> testdir path' <*> testpath candidate >>= \case
    True -> pure $ Just candidate
    False
      | is_root -> pure Nothing
      | otherwise -> find_dominating_file (parent path') name

first_word :: Text -> Text
first_word txt = fromMaybe txt $ listToMaybe $ T.words txt

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

filter_elems :: Eq a => [a] -> [(a, [b])] -> [b]
filter_elems x xs = concatMap (fromMaybe [] . flip lookup xs) x

-- | Replace the value of $HOME in a path with "~"
implode_home :: MonadIO m => FilePath -> m FilePath
implode_home path' = do
  home' <- home
  pure $ maybe path' ("~" </>) (stripPrefix (home' </> "") path')

-- | Flipped Monad sequence >>
(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)
infixr 1 <<
