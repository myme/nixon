module Nixon.Command
  ( Command(..)
  , CommandEnv(..)
  , CommandOutput(..)
  , Language(..)
  , empty
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


import           Data.Text (Text, pack, replace, unpack)
import           Nixon.Utils (first_word)
import qualified Text.Parsec as P
import           Text.Parsec hiding (parse)
import           Text.Parsec.Text
import           Turtle ((%), format, s)

data Command = Command
  { cmdName :: Text
  , cmdDesc :: Maybe Text
  , cmdLang :: Language
  , cmdProjectTypes :: [Text]
  , cmdSource :: Text
  , cmdEnv :: [(Text, CommandEnv)]
  , cmdIsBg :: Bool
  , cmdOutput :: CommandOutput
  } deriving (Eq, Show)

empty :: Command
empty = Command
  { cmdName = ""
  , cmdDesc = Nothing
  , cmdLang = None
  , cmdProjectTypes = []
  , cmdSource = ""
  , cmdEnv = []
  , cmdIsBg = False
  , cmdOutput = Lines
  }

data Language = Bash
              | Haskell
              | JavaScript
              | Python
              | Unknown Text
              | None
              deriving Eq

instance Show Language where
  show = \case
    Bash       -> "bash"
    Haskell    -> "haskell"
    JavaScript -> "javascript"
    Python     -> "python"
    Unknown l  -> unpack l
    None       -> ""

parseLang :: Text -> Language
parseLang = \case
  "bash"       -> Bash
  "haskell"    -> Haskell
  "js"         -> JavaScript
  "javascript" -> JavaScript
  "python"     -> Python
  ""           -> None
  lang         -> Unknown lang

-- | Placeholders for environment variables
newtype CommandEnv = Env Text deriving (Eq, Show)

data CommandOutput = Lines | JSON deriving (Eq, Show)

mkcommand :: Text -> Maybe Text -> [Text] -> Text -> Either Text Command
mkcommand spec lang ptypes src = case parse parse_args spec of
  Left err -> Left err
  Right args -> Right $ Command
    { cmdName = first_word spec
    , cmdDesc = Nothing
    , cmdLang = maybe None parseLang lang
    , cmdProjectTypes = ptypes
    , cmdSource = src
    , cmdEnv = args
    , cmdIsBg = False
    , cmdOutput = Lines
    }

show_command_oneline :: Command -> Text
show_command_oneline cmd = format (s%s) (cmdName cmd) desc
  where desc = case cmdDesc cmd of
          Nothing -> ""
          Just txt -> format (" - "%s) txt

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
