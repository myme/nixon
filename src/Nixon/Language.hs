module Nixon.Language
  ( Language (..),
    extension,
    interpreter,
    parseLang,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text, unpack)
import Options.Applicative ((<|>))
import Turtle (need)

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
  "bash" -> Bash
  "haskell" -> Haskell
  "js" -> JavaScript
  "javascript" -> JavaScript
  "json" -> JSON
  "plain" -> Plain
  "python" -> Python
  "" -> None
  lang -> Unknown lang

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
  JSON -> pure $ Just ("jq" :| ["-r", ".[]"])
  Plain -> pure $ Just (pure "cat")
  Python -> pure $ Just (pure "python3")
  Unknown _ -> pure Nothing
