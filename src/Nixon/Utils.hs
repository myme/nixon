module Nixon.Utils
 ( toLines
 ) where

import Data.List.NonEmpty (toList)
import Turtle

toLines :: Shell Text -> Shell Line
toLines = join . fmap (select . toList . textToLines)
