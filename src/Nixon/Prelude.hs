-- | Custom Prelude
module Nixon.Prelude
  ( -- * "Prelude"
    module Prelude,
    -- * "Applicatives"
    module Applicative,
    -- * "Monads"
    module Monads,
    -- * "Text"
    module Text,
    FilePath
  )
where

import Prelude hiding (FilePath, fail, log)
import Control.Applicative as Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class as Monads (MonadIO (..), liftIO)
import Data.Text as Text (Text)
import Turtle (FilePath)
