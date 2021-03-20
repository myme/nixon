module Nixon.Command
  ( Command(..)
  , CommandPart(..)
  , CommandOutput(..)
  , list_commands
  , show_command
  , show_command_oneline
  , show_parts
  , is_gui_command
  , mkcommand
  , parse
  , parse_parts
  , parse_text_part
  , parse_placeholder
  , (<!)
  , description
  , gui
  , json
  ) where

import           Data.String (IsString(..))
import           Data.Text (pack, unpack, Text, intercalate)
import qualified Data.Text as T
import           Nixon.Utils (quote)
import qualified Text.Parsec as P
import           Text.Parsec hiding (parse)
import           Text.Parsec.Text
import           Turtle ((%), format, s)

data Command = Command
  { cmdName :: Text
  , cmdDesc :: Maybe Text
  , cmdLang :: Text
  , cmdProjectTypes :: [Text]
  , cmdParts :: [CommandPart]
  , cmdIsGui :: Bool
  , cmdOutput :: CommandOutput
  } deriving Eq


instance Show Command where
  show = unpack . show_command


data CommandPart = TextPart Text
                 | Placeholder Text
                 | NestedPart [CommandPart]
                 deriving (Eq, Show)


instance IsString CommandPart where
  fromString = TextPart . pack


data CommandOutput = Lines | JSON deriving Eq


mkcommand :: Text -> Text -> [Text] -> Text -> Either Text Command
mkcommand name lang ptypes src = case parse parse_parts src of
  Left err -> Left err
  Right parts -> Right $ Command
    { cmdName = name
    , cmdDesc = Nothing
    , cmdLang = lang
    , cmdProjectTypes = ptypes
    , cmdParts = parts
    , cmdIsGui = False
    , cmdOutput = Lines
    }

(<!) :: a -> (a -> b) -> b
(<!) cmd op = op cmd

description :: Text -> Command -> Command
description d cmd = cmd { cmdDesc = Just d }

gui :: Bool -> Command -> Command
gui g cmd = cmd { cmdIsGui = g }

json :: Bool -> Command -> Command
json j cmd = cmd { cmdOutput = if j then JSON else Lines }


show_command :: Command -> Text
show_command (Command name _ lang projectTypes parts _ _) = format fmt name pt lang (show_parts parts)
  where fmt = s%" ["%s%"]:\n"%"#!"%s%"\n"%s
        pt = intercalate ", " projectTypes


show_command_oneline :: Command -> Text
show_command_oneline cmd = format (s%" - "%s) (cmdName cmd) desc
  where desc = case cmdDesc cmd of
          Just d -> d
          _ -> T.unwords $ map T.strip $ T.lines $ show_parts $ cmdParts cmd


show_parts :: [CommandPart] -> Text
show_parts = T.unwords . map format_part
  where format_part (TextPart src) = src
        format_part (Placeholder n) = format ("<"%s%">") n
        format_part (NestedPart ns) = quote $ show_parts ns


list_commands :: [Command] -> Text
list_commands = intercalate "\n\n" . map show_command


is_gui_command :: Command -> Bool
is_gui_command _ = False


parse :: Show a => Parser a -> Text -> Either Text a
parse parser input = case P.parse parser "" input of
  Left err -> Left (pack $ show err)
  Right result -> Right result


parse_parts :: Parser [CommandPart]
parse_parts = collapse <$> choice
  [ (:) <$> parse_placeholder <*> parse_parts
  , (:) <$> parse_text_part <*> parse_parts
  , []  <$  eof
  ]
  where collapse (TextPart x : TextPart y : rest) = TextPart (x <> y) : collapse rest
        collapse (x : rest) = x : collapse rest
        collapse [] = []


parse_text_part :: Parser CommandPart
parse_text_part = TextPart . pack . pure <$> anyChar


parse_placeholder :: Parser CommandPart
parse_placeholder = Placeholder . pack <$> between (try $ string "$(nixon ") (char ')') (many1 $ noneOf ")")
