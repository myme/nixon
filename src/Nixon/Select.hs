{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Nixon.Select
  ( Select
  , Selection (..)
  , SelectionType (..)
  , Selector
  , SelectorOpts (..)
  , Candidate (..)
  , build_map
  , candidate_title
  , candidate_value
  , default_selection
  , runSelect
  , select
  , selection
  , text_to_line
  , search
  , title
  , defaults) where

import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types (unexpected)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Turtle hiding (f, x, input, select, s)

data SelectionType = Default | Alternate Int deriving (Eq, Show)
data Selection a = EmptySelection
                 | CanceledSelection
                 | Selection SelectionType a
                 deriving (Eq, Show)

selection :: s -> Selection s
selection = Selection Default

data Candidate = Identity Text
               | WithTitle Text Text -- ^ Title Value
               deriving (Generic, Show)

instance FromJSON Candidate where
  parseJSON (String value) = pure $ Identity value
  parseJSON (Object value) = WithTitle <$> value .: "title" <*> value .: "value"
  parseJSON value = unexpected value

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

data SelectorOpts = SelectorOpts
  { selector_title :: Maybe Text
  , selector_search :: Maybe Text
  }

defaults :: SelectorOpts
defaults = SelectorOpts
  { selector_title = Nothing
  , selector_search = Nothing
  }

instance Semigroup SelectorOpts where
  (<>) lhs rhs = SelectorOpts
    { selector_title = selector_title rhs <|> selector_title lhs
    , selector_search = selector_search rhs <|> selector_search lhs
    }

instance Monoid SelectorOpts where
  mempty = defaults

type Selector m = SelectorOpts -> Shell Candidate -> m (Selection Text)

type Select m a = ReaderT (Selector m) m a

default_selection :: a -> Selection a -> a
default_selection _ (Selection _ value) = value
default_selection def _ = def

title :: Text -> SelectorOpts
title t = defaults { selector_title = Just t }

search :: Text -> SelectorOpts
search s = defaults { selector_search = Just s }

build_map :: (a -> Text) -> [a] -> Map.Map Text a
build_map f = Map.fromList . map (f &&& id)

runSelect :: Selector m -> Select m a -> m a
runSelect = flip runReaderT

select :: MonadIO m => SelectorOpts -> Shell Candidate -> Select m (Selection Text)
select opts input = do
  selector <- ask
  lift $ selector opts input

text_to_line :: Text -> Line
text_to_line = fromMaybe "" . textToLine
