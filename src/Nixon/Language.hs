{-# LANGUAGE CPP #-}

module Nixon.Language
  ( Language (..),
    extension,
    fromFilePath,
    interpreter,
    parseLang,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Nixon.Prelude
import Turtle (need)
import qualified Turtle

data Language
  = Bash
  | Haskell
  | JavaScript
  | JSON
  | Plain
  | Python
  | YAML
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
    YAML -> "yaml"
    Unknown l -> T.unpack l
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
  "yaml" -> YAML
  "" -> None
  lang -> Unknown lang

fromFilePath :: FilePath -> Language
fromFilePath path = case ext of
  Just "sh" -> Bash
  Just "hs" -> Haskell
  Just "js" -> JavaScript
  Just "json" -> JSON
  Just "txt" -> Plain
  Just "py" -> Python
  Just lang -> Unknown lang
  Nothing -> Unknown ""
  where
    ext =
#if MIN_VERSION_turtle(1,6,0)
      T.pack <$> Turtle.extension path
#else
      Turtle.extension path
#endif

extension :: Language -> Text
extension = \case
  Bash -> ".sh"
  None -> ".sh"
  Haskell -> ".hs"
  JavaScript -> ".js"
  JSON -> ".json"
  Plain -> ".txt"
  Python -> ".py"
  YAML -> ".yaml"
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
  YAML -> pure $ Just ("yq" :| ["-r", "."])
  Unknown _ -> pure Nothing
