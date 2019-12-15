module Nixon.Select
  ( Select
  , Selection (..)
  , SelectionType (..)
  , Selector
  , build_map
  , default_selection
  , runSelect
  , select
  , text_to_line
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Turtle hiding (f, x, input, select)

data SelectionType = Default | Alternate Int deriving Show
data Selection a = EmptySelection
                 | CanceledSelection
                 | Selection SelectionType a
                 deriving Show

instance Functor Selection where
  fmap f (Selection t x) = Selection t (f x)
  fmap _ EmptySelection = EmptySelection
  fmap _ CanceledSelection =  CanceledSelection

type Selector = Shell Line -> IO (Selection Text)

type Select a = ReaderT Selector IO a

default_selection :: a -> Selection a -> a
default_selection _ (Selection _ value) = value
default_selection def _ = def

build_map :: (a -> Text) -> [a] -> Map.Map Text a
build_map f = Map.fromList . map (f &&& id)

runSelect :: Selector -> Select a -> IO a
runSelect = flip runReaderT

select :: Shell Line -> Select (Selection Text)
select input = do
  select' <- ask
  liftIO $ select' input

text_to_line :: Text -> Line
text_to_line = fromMaybe "" . textToLine
