{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Nixon.Config.JSON
  ( Config (..),
    empty,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import GHC.Generics (Generic)
import Nixon.Project (ProjectMarker (..), ProjectType (..))
import Turtle (FilePath, Text, fromText)
import Prelude hiding (FilePath)

data Config = Config
  { exact_match :: Maybe Bool,
    ignore_case :: Maybe Bool,
    project_dirs :: [FilePath],
    project_types :: [ProjectType],
    use_direnv :: Maybe Bool,
    use_nix :: Maybe Bool,
    terminal :: Maybe Text
  }
  deriving (Generic, Show)

empty :: Config
empty =
  Config
    { exact_match = Nothing,
      ignore_case = Nothing,
      project_dirs = [],
      project_types = [],
      use_direnv = Nothing,
      use_nix = Nothing,
      terminal = Nothing
    }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config
      <$> v .:? "exact_match"
      <*> v .:? "ignore_case"
      <*> (maybe [] (fmap fromText) <$> v .:? "project_dirs")
      <*> (maybe [] (fmap mkptype) <$> v .:? "project_types")
      <*> v .:? "use_direnv"
      <*> v .:? "use_nix"
      <*> v .:? "terminal"

newtype JsonProjectType = JsonProjectType ProjectType

instance FromJSON JsonProjectType where
  parseJSON = withObject "projects" $ \v -> do
    project_id <- v .: "name"
    project_markers <- maybe [] (fmap $ ProjectPath . fromText) <$> v .:? "test"
    project_description <- v .: "desc"
    pure $ JsonProjectType (ProjectType {project_id, project_markers, project_description})

mkptype :: JsonProjectType -> ProjectType
mkptype (JsonProjectType ptype) = ptype
