module Envix.Fzf
  ( fzf
  ) where

import           Control.Arrow (second)
import           Data.List.NonEmpty (toList)
import qualified Data.Text as T
import           Turtle

fzf :: [Text] -> IO ()
fzf candidates = do
  let input' = concatMap (toList . textToLines) candidates
  (code, out) <- second T.strip <$> procStrict "fzf" ["--height", "40%"] (select input')
  print (code, out)
