{-# LANGUAGE RankNTypes #-}
module Nixon.Select
  ( Select
  , Selection (..)
  , SelectionType (..)
  , Selector
  , Candidate (..)
  , build_map
  , candidate_title
  , candidate_value
  , default_selection
  , runSelect
  , select
  , selection
  , text_to_line
  ) where

import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Turtle hiding (f, x, input, select)

data SelectionType = Default | Alternate Int deriving (Eq, Show)
data Selection a = EmptySelection
                 | CanceledSelection
                 | Selection SelectionType a
                 deriving (Eq, Show)

selection :: s -> Selection s
selection = Selection Default

data Candidate = Identity Text
               | WithTitle Text Text -- ^ Title Value
               deriving Show

candidate_title :: Candidate -> Text
candidate_title (Identity t) = t
candidate_title (WithTitle t _) = t

candidate_value :: Candidate -> Text
candidate_value (Identity v) = v
candidate_value (WithTitle _ v) = v

instance Functor Selection where
  fmap f (Selection t x) = Selection t (f x)
  fmap _ EmptySelection = EmptySelection
  fmap _ CanceledSelection =  CanceledSelection

type Selector = Shell Candidate -> IO (Selection Text)

type Select m a = ReaderT Selector m a

default_selection :: a -> Selection a -> a
default_selection _ (Selection _ value) = value
default_selection def _ = def

build_map :: (a -> Text) -> [a] -> Map.Map Text a
build_map f = Map.fromList . map (f &&& id)

runSelect :: Selector -> Select m a -> m a
runSelect = flip runReaderT

select :: MonadIO m => Shell Candidate -> Select m (Selection Text)
select input = do
  selector <- ask
  liftIO $ selector input

text_to_line :: Text -> Line
text_to_line = fromMaybe "" . textToLine
