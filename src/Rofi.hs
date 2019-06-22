module Rofi
  ( rofi
  , rofi_msg
  , rofi_prompt
  ) where

import           Control.Arrow (second)
import           Data.List.NonEmpty (toList)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Turtle

-- | Internal record type for cli options
data RofiOpts = RofiOpts
  { _msg :: Maybe Text
  , _prompt :: Maybe Text
  }

instance Semigroup RofiOpts where
  left <> right = RofiOpts { _msg = _msg right <|> _msg left
                           , _prompt = _prompt right <|> _prompt left
                           }

instance Monoid RofiOpts where
  mempty = RofiOpts Nothing Nothing

-- | Set -mesg command line option
rofi_msg :: Text -> RofiOpts
rofi_msg msg = mempty { _msg = Just msg }

-- | Set -p|--prompt command line option
rofi_prompt :: Text -> RofiOpts
rofi_prompt prompt = mempty { _prompt = Just prompt }

-- | Launch rofi with the given options and candidates
rofi :: RofiOpts -> [Text] -> IO (ExitCode, Text)
rofi opts candidates = do
  let input' = concatMap (toList . textToLines) candidates
      args = "-dmenu" : concat (
        catMaybes [ _msg opts >>= pure . (["-mesg"] <>) . pure
                  , _prompt opts >>= pure . (["-p"] <>) . pure
                  ])
  print args
  second T.strip <$> procStrict "rofi" args (select input')
