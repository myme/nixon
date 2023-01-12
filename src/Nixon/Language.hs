module Nixon.Language
  ( Language (..),
    extension,
    fromFilePath,
    interpreter,
    parseLang,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, unpack)
import Options.Applicative ((<|>))
import Turtle (FilePath, need)
import qualified Turtle
import Prelude hiding (FilePath)

data Language
  = Bash
  | Haskell
  | JavaScript
  | JSON
  | Plain
  | Python
  | Unknown Text
  | None
  deriving (Eq)

instance Show Language where
  show = \case
    Bash -> "bash"
    Haskell -> "haskell"
    JavaScript -> "javascript"
    JSON -> "json"
    Plain -> "plain"
    Python -> "python"
    Unknown l -> unpack l
    None -> ""

parseLang :: Text -> Language
parseLang = \case
  "sh" -> Bash
  "bash" -> Bash
  "haskell" -> Haskell
  "js" -> JavaScript
  "javascript" -> JavaScript
  "json" -> JSON
  "plain" -> Plain
  "python" -> Python
  "" -> None
  lang -> Unknown lang

fromFilePath :: FilePath -> Language
fromFilePath path = case Turtle.extension path of
  Just "sh" -> Bash
  Just "hs" -> Haskell
  Just "js" -> JavaScript
  Just "json" -> JSON
  Just "txt" -> Plain
  Just "py" -> Python
  Just lang -> Unknown lang
  Nothing -> Unknown ""

extension :: Language -> Text
extension = \case
  Bash -> ".sh"
  None -> ".sh"
  Haskell -> ".hs"
  JavaScript -> ".js"
  JSON -> ".json"
  Plain -> ".txt"
  Python -> ".py"
  Unknown _ -> ".txt"

interpreter :: MonadIO m => Language -> m (Maybe (NonEmpty Text))
interpreter = \case
  Bash -> pure $ Just (pure "bash")
  None -> do
    shell <- need "SHELL"
    pure $ (pure <$> shell) <|> Just (pure "bash")
  Haskell -> pure $ Just (pure "runghc")
  JavaScript -> pure $ Just (pure "node")
  JSON -> pure $ Just ("jq" :| ["-r", "."])
  Plain -> pure $ Just (pure "cat")
  Python -> pure $ Just (pure "python3")
  Unknown _ -> pure Nothing
