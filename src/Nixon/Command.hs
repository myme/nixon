module Nixon.Command
  ( Command(..)
  , CommandEnv(..)
  , CommandOutput(..)
  , is_bg_command
  , mkcommand
  , parse
  , parse_args
  , (<!)
  , description
  , bg
  , json
  , show_command_oneline
  ) where

import           Data.Text (Text, pack, replace)
import qualified Text.Parsec as P
import           Text.Parsec hiding (parse)
import           Text.Parsec.Text

data Command = Command
  { cmdName :: Text
  , cmdDesc :: Maybe Text
  , cmdLang :: Text
  , cmdProjectTypes :: [Text]
  , cmdSource :: Text
  , cmdEnv :: [(Text, CommandEnv)]
  , cmdIsBg :: Bool
  , cmdOutput :: CommandOutput
  } deriving (Eq, Show)

-- | Placeholders for environment variables
newtype CommandEnv = Env Text deriving (Eq, Show)

data CommandOutput = Lines | JSON deriving (Eq, Show)

mkcommand :: Text -> Text -> [Text] -> Text -> Either Text Command
mkcommand name lang ptypes src = case parse parse_args name of
  Left err -> Left err
  Right args -> Right $ Command
    { cmdName = name
    , cmdDesc = Nothing
    , cmdLang = lang
    , cmdProjectTypes = ptypes
    , cmdSource = src
    , cmdEnv = args
    , cmdIsBg = False
    , cmdOutput = Lines
    }

show_command_oneline :: Command -> Text
show_command_oneline = cmdName

(<!) :: a -> (a -> b) -> b
(<!) cmd op = op cmd

description :: Text -> Command -> Command
description d cmd = cmd { cmdDesc = Just d }

bg :: Bool -> Command -> Command
bg g cmd = cmd { cmdIsBg = g }

json :: Bool -> Command -> Command
json j cmd = cmd { cmdOutput = if j then JSON else Lines }

is_bg_command :: Command -> Bool
is_bg_command _ = False

parse :: Show a => Parser a -> Text -> Either Text a
parse parser input = case P.parse parser "" input of
  Left err -> Left (pack $ show err)
  Right result -> Right result

parse_args :: Parser [(Text, CommandEnv)]
parse_args = choice
  [(:) <$> parse_arg <*> parse_args
  ,anyChar *> parse_args
  ,[]  <$  eof
  ]

parse_arg :: Parser (Text, CommandEnv)
parse_arg = do
  let prefix = try $ string "${"
      suffix = string "}"
  placeholder <- pack <$> between prefix suffix (many1 $ noneOf "}")
  pure (replace "-" "_" placeholder, Env placeholder)
