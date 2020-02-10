module Nixon.Utils
 ( find_dominating_file
 , toLines
 ) where

import Data.List.NonEmpty (toList)
import Prelude hiding (FilePath)
import Turtle

-- | Locate a file going up the filesystem hierarchy
find_dominating_file :: FilePath -> FilePath -> IO (Maybe FilePath)
find_dominating_file path' name = do
  let candidate = path' </> name
      is_root = parent path' == root path'
  (&&) <$> testdir path' <*> testpath candidate >>= \case
    True -> pure $ Just candidate
    False | is_root -> pure Nothing
          | otherwise -> find_dominating_file (parent path') name

toLines :: Shell Text -> Shell Line
toLines = join . fmap (select . toList . textToLines)
