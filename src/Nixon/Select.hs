{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Nixon.Select
  ( Select,
    Selection (..),
    SelectionType (..),
    Selector,
    SelectorOpts (..),
    Candidate (..),
    build_map,
    candidate_title,
    candidate_value,
    default_selection,
    runSelect,
    select,
    selection,
    text_to_line,
    search,
    title,
    defaults,
    catMaybeSelection,
  )
where

import Control.Arrow ((&&&))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object, String),
    (.:),
  )
import Data.Aeson.Types (unexpected)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Generics (Generic)
import Turtle
  ( Alternative ((<|>)),
    Line,
    MonadIO,
    Shell,
    Text,
    textToLine,
  )

data SelectionType = Default | Edit | Show | Visit deriving (Eq, Show)

data Selection a
  = EmptySelection
  | CanceledSelection
  | Selection SelectionType [a]
  deriving (Eq, Show)

selection :: [s] -> Selection s
selection = Selection Default

catMaybeSelection :: Selection (Maybe a) -> Selection a
catMaybeSelection maybeSelection = case maybeSelection of
  Selection _ [] -> EmptySelection
  Selection selectionType sel -> Selection selectionType (catMaybes sel)
  EmptySelection -> EmptySelection
  CanceledSelection -> CanceledSelection

data Candidate
  = Identity Text
  | -- | Title Value
    WithTitle Text Text
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
  fmap f (Selection t x) = Selection t (fmap f x)
  fmap _ EmptySelection = EmptySelection
  fmap _ CanceledSelection = CanceledSelection

data SelectorOpts = SelectorOpts
  { selector_title :: Maybe Text,
    selector_search :: Maybe Text,
    selector_multiple :: Maybe ()
  }

defaults :: SelectorOpts
defaults =
  SelectorOpts
    { selector_title = Nothing,
      selector_search = Nothing,
      selector_multiple = Nothing
    }

instance Semigroup SelectorOpts where
  (<>) lhs rhs =
    SelectorOpts
      { selector_title = selector_title rhs <|> selector_title lhs,
        selector_search = selector_search rhs <|> selector_search lhs,
        selector_multiple = selector_multiple rhs <|> selector_multiple lhs
      }

instance Monoid SelectorOpts where
  mempty = defaults

type Selector m = SelectorOpts -> Shell Candidate -> m (Selection Text)

type Select m a = ReaderT (Selector m) m a

default_selection :: [a] -> Selection a -> [a]
default_selection _ (Selection _ value) = value
default_selection def _ = def

title :: Text -> SelectorOpts
title t = defaults {selector_title = Just t}

search :: Text -> SelectorOpts
search s = defaults {selector_search = Just s}

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
