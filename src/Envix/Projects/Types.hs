module Envix.Projects.Types
  ( Part (..)
  , Project (..)
  , ProjectType (..)
  , ProjectMarker (..)
  , desc
  , path
  , proj
  , project_path
  , resolve_command
  ) where

import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Turtle hiding (d, f)

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_types :: [ProjectType]
                       } deriving Show

-- | Full path to a project
project_path :: Project -> FilePath
project_path project = project_dir project </> project_name project

data ProjectType = ProjectType { project_markers :: [ProjectMarker]
                               , project_description :: Text
                               , project_commands :: [Command]
                               } deriving Show

-- | Construct a project description
proj :: [ProjectMarker] -> Text -> [Command] -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath
                   | ProjectFile FilePath
                   | ProjectDir FilePath
                   | ProjectFunc (FilePath -> IO Bool)

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectPath p) = "ProjectP" ++ show p
  show (ProjectFile p) = "ProjectFile" ++ show p
  show (ProjectDir p)  = "ProjectDir"  ++ show p

data Part = TextPart Text
          | Interpolation (Project -> Text)

-- | Placeholder for project path
path :: Command
path = Command [Interpolation (format fp . project_path)] ""

instance Show Part where
  show (TextPart t) = T.unpack t
  show (Interpolation _) = "<...>"

data Command = Command { command_parts :: [Part]
                       , command_desc :: Text
                       } deriving Show

-- | Add command description
desc :: Text -> Command
desc = Command []

instance IsString Command where
  fromString ss = Command (map TextPart $ T.words $ T.pack ss) ""

instance Semigroup Command where
  (Command a d) <> (Command b d') = Command (a <> b) (d <> d')

resolve_command :: Project -> Command -> Text
resolve_command project (Command parts _) = foldMap interpolate parts
  where interpolate (TextPart t) = t
        interpolate (Interpolation f) = f project
