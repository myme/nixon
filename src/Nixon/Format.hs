module Nixon.Format where

import Data.Char (isSpace)
import qualified Data.Text as T
import Nixon.Prelude

-- | Parse ouput in column format into a list of rows of columns.
parseColumns :: Text -> [[Text]]
parseColumns input = case T.lines input of
  [] -> []
  (header : rows) -> parseColumn (parseWidths header) <$> rows
  where
    parseWidths t
      | T.length t == 0 = []
      | otherwise =
          let (word, startOfSpace) = T.span (not . isSpace) t
              (space, rest) = T.span isSpace startOfSpace
           in T.length word + T.length space : parseWidths rest
    parseColumn [] _ = []
    -- The last column runs to the end of line
    parseColumn [_] row = [row]
    parseColumn (w : ws) row = case T.splitAt w row of
      (col, rest) -> T.strip col : parseColumn ws rest
