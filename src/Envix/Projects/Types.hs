module Envix.Projects.Types
  ( ProjectType (..)
  , ProjectMarker (..)
  , find_markers
  , proj
  , test_marker
  ) where

import           Control.Monad (filterM)
import qualified Data.Text as T
import           Envix.Projects.Commands
import           Prelude hiding (FilePath)
import           Turtle

data ProjectType = ProjectType { project_markers :: [ProjectMarker]
                               , project_description :: Text
                               , project_commands :: [CmdDesc]
                               }

proj :: [ProjectMarker] -> Text -> [CmdDesc] -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath
                   | ProjectFile FilePath
                   | ProjectDir FilePath
                   | ProjectFunc (FilePath -> IO Bool)

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectPath path) = "ProjectPath" ++ show path
  show (ProjectFile path) = "ProjectFile" ++ show path
  show (ProjectDir path)  = "ProjectDir"  ++ show path

-- | Test that a marker is valid for a path
test_marker :: ProjectMarker -> FilePath -> IO Bool
test_marker (ProjectPath marker) path = testpath (path </> marker)
test_marker (ProjectFile marker) path = testfile (path </> marker)
test_marker (ProjectDir  marker) path = testdir (path </> marker)
test_marker (ProjectFunc marker) path = marker path

-- | Given a path, find markers and associated commands.
find_markers :: FilePath -> [ProjectType] -> IO [CmdDesc]
find_markers path project_types = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return []
    else concatMap project_commands <$> filterM has_markers project_types
  where has_markers = fmap and . traverse (`test_marker` path) . project_markers
