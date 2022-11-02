module Nixon.Command.Placeholder
  ( Placeholder (..),
    PlaceholderType (..),
  )
where

import Data.Text (Text)

data PlaceholderType = Arg | EnvVar | Stdin
  deriving (Eq, Show)

-- | Placeholders for environment variables
data Placeholder = Placeholder
  { -- | Type of placeholder
    type_ :: PlaceholderType,
    -- | The command it's referencing
    name :: Text,
    -- | If the placeholder can select multiple
    multiple :: Bool
  }
  deriving (Eq, Show)
