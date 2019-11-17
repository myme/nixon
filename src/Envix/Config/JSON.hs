{-# LANGUAGE DeriveGeneric #-}

module Envix.Config.JSON
  ( Config(..)
  , JSONError(..)
  , default_path
  , read_config
  ) where

import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as ByteString
import           Data.Char
import qualified Data.Text as T
import           GHC.Generics
import           Prelude hiding (FilePath)
import qualified System.Directory as Directory
import           System.IO.Error
import           Turtle hiding (err)

data Config = Config
  { source_dirs :: [FilePath]
  , use_direnv :: Bool
  , use_nix :: Bool
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> (fmap fromText <$> v .: "source_dirs")
    <*> v .: "use_direnv"
    <*> v .: "use_nix"

default_path :: IO FilePath
default_path = fromString <$> Directory.getXdgDirectory Directory.XdgConfig "envix.json"

data JSONError = NoSuchFile
               | EmptyFile
               | ParseError Text

read_config :: Maybe FilePath -> IO (Either JSONError Config)
read_config Nothing = read_config =<< Just <$> default_path
read_config (Just path) = do
  let path' = T.unpack (format fp path)
  tryIOError (ByteString.readFile path') >>= \case
    Left err | isDoesNotExistError err -> pure (Left NoSuchFile)
             | otherwise -> ioError err
    Right c  | ByteString.dropWhile isSpace c == "" -> pure (Left EmptyFile)
             | otherwise -> pure $ first (ParseError . fromString) (eitherDecodeStrict c)
