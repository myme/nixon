module Nixon.Command
  ( Command (..),
    CommandEnv (..),
    CommandLocation (..),
    CommandOutput (..),
    Language (..),
    empty,
    is_bg_command,
    mkcommand,
    parse,
    parse_args,
    (<!),
    description,
    bg,
    json,
    show_command,
    show_command_with_description,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nixon.Language (Language (None), parseLang)
import Nixon.Utils (first_word)
import Text.Parsec (anyChar, between, choice, eof, many1, noneOf, string, try)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Turtle (FilePath, format, s, (%))

import Prelude hiding (FilePath)

data Command = Command
  { cmdName :: Text,
    cmdDesc :: Maybe Text,
    cmdLang :: Language,
    cmdProjectTypes :: [Text],
    cmdSource :: Text,
    cmdEnv :: [(Text, CommandEnv)],
    cmdIsBg :: Bool,
    cmdOutput :: CommandOutput,
    cmdLocation :: Maybe CommandLocation
  }
  deriving (Eq, Show)

data CommandLocation = CommandLocation
  { cmdFilePath :: FilePath,
    cmdLineNr :: Int
  }
  deriving (Eq, Show)

empty :: Command
empty =
  Command
    { cmdName = "",
      cmdDesc = Nothing,
      cmdLang = None,
      cmdProjectTypes = [],
      cmdSource = "",
      cmdEnv = [],
      cmdIsBg = False,
      cmdOutput = Lines,
      cmdLocation = Nothing
    }

-- | Placeholders for environment variables
newtype CommandEnv = Env Text deriving (Eq, Show)

data CommandOutput = Lines | JSON deriving (Eq, Show)

mkcommand :: CommandLocation -> Text -> Maybe Text -> [Text] -> Text -> Either Text Command
mkcommand loc spec lang ptypes src = case parse parse_args spec of
  Left err -> Left err
  Right args ->
    Right $
      Command
        { cmdName = first_word spec,
          cmdDesc = Nothing,
          cmdLang = maybe None parseLang lang,
          cmdProjectTypes = ptypes,
          cmdSource = src,
          cmdEnv = args,
          cmdIsBg = False,
          cmdOutput = Lines,
          cmdLocation = Just loc
        }

show_command :: Command -> Text
show_command cmd = T.unwords $ cmdName cmd : map (format ("${" % s % "}") . fst) (cmdEnv cmd)

show_command_with_description :: Command -> Text
show_command_with_description cmd = format (s % s) (cmdName cmd) desc
  where
    desc = case cmdDesc cmd of
      Nothing -> ""
      Just txt -> format (" - " % s) txt

(<!) :: a -> (a -> b) -> b
(<!) cmd op = op cmd

description :: Text -> Command -> Command
description d cmd = cmd {cmdDesc = Just d}

bg :: Bool -> Command -> Command
bg g cmd = cmd {cmdIsBg = g}

json :: Bool -> Command -> Command
json j cmd = cmd {cmdOutput = if j then JSON else Lines}

is_bg_command :: Command -> Bool
is_bg_command _ = False

parse :: Show a => Parser a -> Text -> Either Text a
parse parser input = case P.parse parser "" input of
  Left err -> Left (T.pack $ show err)
  Right result -> Right result

parse_args :: Parser [(Text, CommandEnv)]
parse_args =
  choice
    [ (:) <$> parse_arg <*> parse_args,
      anyChar *> parse_args,
      [] <$ eof
    ]

parse_arg :: Parser (Text, CommandEnv)
parse_arg = do
  let prefix = try $ string "${"
      suffix = string "}"
  placeholder <- T.pack <$> between prefix suffix (many1 $ noneOf "}")
  pure (T.replace "-" "_" placeholder, Env placeholder)
