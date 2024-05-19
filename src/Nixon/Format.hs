module Nixon.Format
  ( parseColumns,
    pickColumns,
    pickFields,
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

pickColumns :: [Int] -> [Columns] -> [Columns]
pickColumns cols = map (map snd . filter ((`elem` cols) . fst) . zip [1 ..])

pickFields :: [Int] -> [Text] -> [Text]
pickFields fields = map snd . filter ((`elem` fields) . fst) . zip [1 ..]
