module Nixon.Command.Placeholder
  ( Placeholder (..),
    PlaceholderFormat (..),
    PlaceholderType (..)
  )
where

import Nixon.Prelude

data PlaceholderType = Arg | EnvVar {_envName :: Text} | Stdin
  deriving (Eq, Show)

data PlaceholderFormat
  = -- | Interpret output as columns and extract the specified columns
    Columns [Int]
  | -- | Interpret output as fields and extract the specified fields
    Fields [Int]
  | -- | Interpret output as plain lines
    Lines
  | -- | Parse output as JSON
    JSON
  deriving (Eq, Show)

-- | Placeholders for environment variables
data Placeholder = Placeholder
  { -- | Type of placeholder
    type_ :: PlaceholderType,
    -- | The command it's referencing
    name :: Text,
    -- | The field numbers to extract
    format :: PlaceholderFormat,
    -- | If the placeholder can select multiple
    multiple :: Bool,
    -- | Pre-expanded value of the placeholder
    value :: [Text]
  }
  deriving (Eq, Show)
