module Envix.Select
  ( Select
  , Selection (..)
  , SelectionType (..)
  , build_map
  , runSelect
  , select
  , text_to_line
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Turtle hiding (f, input, select)

data SelectionType = Default | Alternate Int deriving Show
data Selection = EmptySelection
               | CanceledSelection
               | Selection SelectionType Text
               deriving Show

type Selector = Shell Line -> IO Selection

type Select a = ReaderT Selector IO a

build_map :: (a -> Text) -> [a] -> Map.Map Text a
build_map f = Map.fromList . map (f &&& id)

runSelect :: Selector -> Select a -> IO a
runSelect = flip runReaderT

select :: Shell Line -> Select Selection
select input = do
  select' <- ask
  liftIO $ select' input

text_to_line :: Text -> Line
text_to_line = fromMaybe "" . textToLine
