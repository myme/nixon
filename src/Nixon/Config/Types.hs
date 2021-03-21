module Nixon.Config.Types
  ( Backend(..)
  , LogLevel(..)
  , Config(..)
  , ConfigError(..)
  , isGuiBackend
  , defaultConfig
  ) where

import Control.Applicative
import Control.Exception
import Nixon.Command (Command)
import Nixon.Logging (LogLevel(..))
import Nixon.Project (ProjectType)
import Prelude hiding (FilePath)
import Turtle (Text, FilePath)

data Backend = Fzf | Rofi deriving (Eq, Show)

isGuiBackend :: Backend -> Bool
isGuiBackend Fzf = False
isGuiBackend Rofi = True

data Config = Config
  { backend :: Maybe Backend
  , exact_match :: Maybe Bool
  , ignore_case :: Maybe Bool
  , project_dirs :: [FilePath]
  , project_types :: [ProjectType]
  , commands :: [Command]
  , use_direnv :: Maybe Bool
  , use_nix :: Maybe Bool
  , terminal :: Maybe Text
  , loglevel :: Maybe LogLevel
  } deriving (Eq, Show)

instance Semigroup Config where
  (<>) lhs rhs = lhs
    { backend = backend rhs <|> backend lhs
    , exact_match = exact_match rhs <|> exact_match lhs
    , ignore_case = ignore_case rhs <|> ignore_case lhs
    , project_dirs = project_dirs lhs ++ project_dirs rhs
    , project_types = project_types lhs ++ project_types rhs
    , commands = commands rhs ++ commands lhs  -- Prioritize local commands first
    , use_direnv = use_direnv rhs <|> use_direnv lhs
    , use_nix = use_nix rhs <|> use_nix lhs
    , terminal = terminal rhs <|> terminal lhs
    , loglevel = loglevel rhs <|> loglevel lhs
    }

defaultConfig :: Config
defaultConfig = Config
  { backend = Nothing
  , exact_match = Nothing
  , ignore_case = Nothing
  , project_dirs = []
  , project_types = []
  , commands = []
  , use_direnv = Nothing
  , use_nix = Nothing
  , terminal = Nothing
  , loglevel = Just LogWarning
  }

data ConfigError = NoSuchFile
                 | EmptyFile
                 | ParseError Text
                 deriving Show

instance Exception ConfigError
