module Nixon.Config.Types
  ( BackendType (..),
    LogLevel (..),
    Config (..),
    ConfigError (..),
    isGuiBackend,
    defaultConfig,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception)
import Nixon.Command (Command)
import Nixon.Logging (LogLevel (..))
import Nixon.Project (ProjectType)
import Turtle (FilePath, Text)
import Prelude hiding (FilePath)

data BackendType = Fzf | Rofi deriving (Eq, Show)

isGuiBackend :: BackendType -> Bool
isGuiBackend Fzf = False
isGuiBackend Rofi = True

data Config = Config
  { backend :: Maybe BackendType,
    exact_match :: Maybe Bool,
    ignore_case :: Maybe Bool,
    force_tty :: Maybe Bool,
    project_dirs :: [FilePath],
    project_types :: [ProjectType],
    commands :: [Command],
    use_direnv :: Maybe Bool,
    use_nix :: Maybe Bool,
    terminal :: Maybe Text,
    loglevel :: Maybe LogLevel
  }
  deriving (Eq, Show)

instance Semigroup Config where
  (<>) lhs rhs =
    lhs
      { backend = backend rhs <|> backend lhs,
        exact_match = exact_match rhs <|> exact_match lhs,
        ignore_case = ignore_case rhs <|> ignore_case lhs,
        force_tty = force_tty rhs <|> force_tty lhs,
        project_dirs = project_dirs lhs ++ project_dirs rhs,
        project_types = project_types lhs ++ project_types rhs,
        commands = commands rhs ++ commands lhs, -- Prioritize local commands first
        use_direnv = use_direnv rhs <|> use_direnv lhs,
        use_nix = use_nix rhs <|> use_nix lhs,
        terminal = terminal rhs <|> terminal lhs,
        loglevel = loglevel rhs <|> loglevel lhs
      }

defaultConfig :: Config
defaultConfig =
  Config
    { backend = Nothing,
      exact_match = Nothing,
      ignore_case = Nothing,
      force_tty = Nothing,
      project_dirs = [],
      project_types = [],
      commands = [],
      use_direnv = Nothing,
      use_nix = Nothing,
      terminal = Nothing,
      loglevel = Just LogWarning
    }

data ConfigError
  = NoSuchFile
  | EmptyFile
  | ParseError Text
  deriving (Show)

instance Exception ConfigError
