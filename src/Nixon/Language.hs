module Nixon.Language
  ( Language(..)
  , extension
  , interpreter
  , parseLang
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, unpack)
import Options.Applicative ((<|>))
import Turtle (need)
import Data.List.NonEmpty (NonEmpty((:|)))

data Language = Bash
              | Haskell
              | JavaScript
              | JSON
              | Python
              | Unknown Text
              | None
              deriving Eq

instance Show Language where
  show = \case
    Bash       -> "bash"
    Haskell    -> "haskell"
    JavaScript -> "javascript"
    JSON       -> "json"
    Python     -> "python"
    Unknown l  -> unpack l
    None       -> ""

parseLang :: Text -> Language
parseLang = \case
  "bash"       -> Bash
  "haskell"    -> Haskell
  "js"         -> JavaScript
  "javascript" -> JavaScript
  "json"       -> JSON
  "python"     -> Python
  ""           -> None
  lang         -> Unknown lang

extension :: Language -> Text
extension = \case
  Bash       -> ".sh"
  None       -> ".sh"
  Haskell    -> ".hs"
  JavaScript -> ".js"
  JSON       -> ".json"
  Python     -> ".py"
  Unknown _  -> ".txt"

interpreter :: MonadIO m => Language -> m (Maybe (NonEmpty Text))
interpreter = \case
  Bash -> pure $ Just (singleton "bash")
  None -> do
    shell <- need "SHELL"
    pure $ (singleton <$> shell) <|> Just (singleton "bash")
  Haskell    -> pure $ Just (singleton "runghc")
  JavaScript -> pure $ Just (singleton "node")
  JSON       -> pure $ Just ("jq" :| ["-r", ".[]"])
  Python     -> pure $ Just (singleton "python3")
  Unknown _  -> pure Nothing
  where singleton = (:| [])
