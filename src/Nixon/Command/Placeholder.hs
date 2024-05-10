module Nixon.Command.Placeholder
  ( Placeholder (..),
    PlaceholderField (..),
    PlaceholderType (..),
  )
where

import Nixon.Prelude

data PlaceholderType = Arg | EnvVar {_envName :: Text} | Stdin
  deriving (Eq, Show)

data PlaceholderField = Col Int | Field Int
  deriving (Eq, Show)

-- | Placeholders for environment variables
data Placeholder = Placeholder
  { -- | Type of placeholder
    type_ :: PlaceholderType,
    -- | The command it's referencing
    name :: Text,
    -- | The field numbers to extract
    fields :: [PlaceholderField],
    -- | If the placeholder can select multiple
    multiple :: Bool,
    -- | Pre-expanded value of the placeholder
    value :: [Text]
  }
  deriving (Eq, Show)
