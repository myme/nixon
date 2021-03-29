module Nixon.Language
  ( Language(..)
  , extension
  , interpreter
  , parseLang
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor
import Data.Text (Text, unpack)
import Options.Applicative ((<|>))
import Turtle (need)

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

extension :: Language -> Text
extension = \case
  Bash       -> ".sh"
  None       -> ".sh"
  Haskell    -> ".hs"
  JavaScript -> ".js"
  Python     -> ".py"
  Unknown _  -> ".txt"

interpreter :: MonadIO m => Language -> m (Maybe Text)
interpreter = \case
  Bash       -> pure $ Just "bash"
  None       -> need "SHELL" <&> (<|> Just "bash")
  Haskell    -> pure $ Just "runghc"
  JavaScript -> pure $ Just "node"
  Python     -> pure $ Just "python3"
  Unknown _  -> pure Nothing
