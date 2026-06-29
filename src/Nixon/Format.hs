module Nixon.Format
  ( parseColumns,
    pickColumns,
    pickFields,
    formatColumns,
  )
where

import Data.Char (isSpace)
import qualified Data.Text as T
import Nixon.Prelude

type Columns = [Text]

-- | Parse ouput in column format into a list of rows of columns.
parseColumns :: Bool -> [Text] -> [Columns]
parseColumns hasHeader = \case
  [] -> []
  (header : rows) -> parseColumn (parseWidths header) <$> if hasHeader then rows else header : rows
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

-- | Build (title, value) selection candidates from column-formatted output.
-- The title is the full original row as printed by the command; the value is
-- the extracted columns joined by spaces. When the output has a header row it
-- is dropped from both the titles and the values so the two stay aligned.
formatColumns :: Bool -> [Int] -> [Text] -> [(Text, Text)]
formatColumns hasHeader cols rows = zip titles values
  where
    titles = if hasHeader then drop 1 rows else rows
    values = map T.unwords . pickColumns cols $ parseColumns hasHeader rows

pickColumns :: [Int] -> [Columns] -> [Columns]
pickColumns cols = map (map snd . filter ((`elem` cols) . fst) . zip [1 ..])

pickFields :: [Int] -> [Text] -> [Text]
pickFields fields = map snd . filter ((`elem` fields) . fst) . zip [1 ..]
