module Envix.Projects.Types
  ( ProjectMarker(..)
  ) where

import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Turtle

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
