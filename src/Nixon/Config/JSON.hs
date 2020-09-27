{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Nixon.Config.JSON
  ( Config(..)
  , JSONError(..)
  , empty
  , find_local_config
  , read_config
  ) where

import           Control.Exception
import           Data.Aeson hiding (json)
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as ByteString
import           Data.Char
import qualified Data.Text as T
import           GHC.Generics
import           Nixon.Project.Types (ProjectMarker(..), ProjectType(..))
import           Nixon.Utils (find_dominating_file)
import           Prelude hiding (FilePath)
import           System.IO.Error
import           Turtle hiding (empty, err)

data Config = Config
  { exact_match :: Maybe Bool
  , source_dirs :: [FilePath]
  , project_types :: [ProjectType]
  , use_direnv :: Maybe Bool
  , use_nix :: Maybe Bool
  } deriving (Generic, Show)

empty :: Config
empty = Config { exact_match = Nothing
               , source_dirs = []
               , project_types = []
               , use_direnv = Nothing
               , use_nix = Nothing
               }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "exact_match"
    <*> (maybe [] (fmap fromText) <$> v .:? "source_dirs")
    <*> (maybe [] (fmap mkptype) <$> v .:? "projects")
    <*> v .:? "use_direnv"
    <*> v .:? "use_nix"

newtype JsonProjectType = JsonProjectType ProjectType

instance FromJSON JsonProjectType where
  parseJSON = withObject "projects" $ \v -> do
    project_id <- v .: "name"
    project_markers <- maybe [] (fmap $ ProjectPath . fromText) <$> v .:? "test"
    project_description <- v .: "desc"
    pure $ JsonProjectType (ProjectType { project_id , project_markers , project_description})


mkptype :: JsonProjectType -> ProjectType
mkptype (JsonProjectType ptype) = ptype

data JSONError = NoSuchFile
               | EmptyFile
               | ParseError Text
               deriving Show

instance Exception JSONError

read_config :: MonadIO m => FilePath -> m (Either JSONError Config)
read_config path = do
  let path' = T.unpack (format fp path)
  liftIO $ tryIOError (ByteString.readFile path') >>= \case
    Left err | isDoesNotExistError err -> pure (Left NoSuchFile)
             | otherwise -> ioError err
    Right c  | ByteString.dropWhile isSpace c == "" -> pure (Left EmptyFile)
             | otherwise -> pure $ first (ParseError . fromString) (eitherDecodeStrict c)

find_local_config :: MonadIO m => FilePath -> m (Maybe Config)
find_local_config path = do
  local_config <- find_dominating_file path ".nixon.json"
  case local_config of
    Nothing -> pure Nothing
    Just file  -> read_config file >>= \case
      Left (ParseError err) -> liftIO $ throwIO (ParseError err)
      Left _ -> pure Nothing
      Right json -> pure (Just json)
