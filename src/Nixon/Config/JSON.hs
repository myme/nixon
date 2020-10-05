{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Nixon.Config.JSON
  ( Config(..)
  , empty
  ) where

import Data.Aeson hiding (json)
import GHC.Generics
import Nixon.Project (ProjectMarker(..), ProjectType(..))
import Prelude hiding (FilePath)
import Turtle hiding (empty, err)

data Config = Config
  { exact_match :: Maybe Bool
  , source_dirs :: [FilePath]
  , project_types :: [ProjectType]
  , use_direnv :: Maybe Bool
  , use_nix :: Maybe Bool
  , terminal :: Maybe Text
  } deriving (Generic, Show)

empty :: Config
empty = Config
  { exact_match = Nothing
  , source_dirs = []
  , project_types = []
  , use_direnv = Nothing
  , use_nix = Nothing
  , terminal = Nothing
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "exact_match"
    <*> (maybe [] (fmap fromText) <$> v .:? "source_dirs")
    <*> (maybe [] (fmap mkptype) <$> v .:? "projects")
    <*> v .:? "use_direnv"
    <*> v .:? "use_nix"
    <*> v .:? "terminal"

newtype JsonProjectType = JsonProjectType ProjectType

instance FromJSON JsonProjectType where
  parseJSON = withObject "projects" $ \v -> do
    project_id <- v .: "name"
    project_markers <- maybe [] (fmap $ ProjectPath . fromText) <$> v .:? "test"
    project_description <- v .: "desc"
    pure $ JsonProjectType (ProjectType { project_id , project_markers , project_description})


mkptype :: JsonProjectType -> ProjectType
mkptype (JsonProjectType ptype) = ptype
