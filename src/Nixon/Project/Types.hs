module Nixon.Project.Types
  ( Project (..)
  , ProjectType (..)
  , ProjectMarker (..)
  , from_path
  , proj
  , project_path
  ) where

import           Data.List (intercalate)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Turtle hiding (d, f, g, shell)

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_types :: [ProjectType]
                       } deriving Show

from_path :: FilePath -> Project
from_path path' = Project { project_name = filename path'
                          , project_dir = parent path'
                          , project_types = []
                          }

-- | Full path to a project
project_path :: Project -> FilePath
project_path project = project_dir project </> project_name project

data ProjectType = ProjectType { project_id :: Text
                               , project_markers :: [ProjectMarker]
                               , project_description :: Text
                               } deriving Show

-- | Construct a project description
proj :: Text -> [ProjectMarker] -> Text -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath -- ^ Check if path exists
                   | ProjectFile FilePath -- ^ Check if path is a file
                   | ProjectDir FilePath -- ^ Check if path is a directory
                   | ProjectOr [ProjectMarker] -- ^ Logical `or` two marker checks
                   | ProjectFunc (FilePath -> IO Bool) -- ^ Run arbitrary check on candidate dir

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectOr ms)  = "ProjectOr (" ++ intercalate ", " (map show ms) ++ ")"
  show (ProjectPath p) = "ProjectPath" ++ show p
  show (ProjectFile p) = "ProjectFile" ++ show p
  show (ProjectDir p)  = "ProjectDir"  ++ show p
