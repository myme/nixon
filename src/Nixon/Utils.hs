{-# LANGUAGE CPP #-}

module Nixon.Utils
  ( escape,
    find_dominating_file,
    first_word,
    fromPath,
    fromText,
    quote,
    printErr,
    shell_to_list,
    toLines,
    takeToSpace,
    filter_elems,
    implode_home,
    (<<?),
  )
where

import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Nixon.Prelude
import qualified System.IO as IO
import qualified Turtle as Turtle
import Turtle
  ( Fold (Fold),
    Line,
    Shell,
    parent,
    root,
    select,
    stripPrefix,
    testdir,
    testpath,
    textToLines,
    (</>),
  )

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
shell_to_list shell' = Turtle.fold shell' (Fold (flip (:)) [] reverse)

toLines :: Shell Text -> Shell Line
toLines = ((select . toList . textToLines) =<<)

takeToSpace :: Text -> Text
takeToSpace = T.takeWhile (not . isSpace)

filter_elems :: Eq a => [a] -> [(a, [b])] -> [b]
filter_elems x xs = concatMap (fromMaybe [] . flip lookup xs) x

fromPath :: FilePath -> IO.FilePath
fromPath =
#if MIN_VERSION_turtle(1,6,0)
  id
#else
  Turtle.encodeString
#endif

fromText :: Text -> FilePath
fromText =
#if MIN_VERSION_turtle(1,6,0)
  T.unpack
#else
  Turtle.fromText
#endif

-- | Replace the value of $HOME in a path with "~"
implode_home :: MonadIO m => FilePath -> m FilePath
implode_home path' = do
  home' <- Turtle.home
  pure $ maybe path' ("~" </>) (stripPrefix (home' </> "") path')

-- | Return a if the bool within the Maybe is True
(<<?) :: a -> Maybe Bool -> Maybe a
(<<?) x f = bool Nothing (Just x) =<< f

infixr 1 <<?
