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
  { type_ :: PlaceholderType,
    name :: Text,
    multiple :: Bool
  }
  deriving (Eq, Show)
