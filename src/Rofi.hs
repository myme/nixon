{-# LANGUAGE LambdaCase #-}

module Rofi
  ( rofi
  , rofi_format
  , rofi_markup
  , rofi_msg
  , rofi_prompt
  , s, i, d, q, f, f'
  ) where

import           Control.Arrow (second)
import           Data.List.NonEmpty (toList)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Turtle hiding (arg, s, d, f)

-- | Data type for command line options to rofi
data RofiOpts = RofiOpts
  { _format :: Maybe RofiFormat
  , _msg :: Maybe Text
  , _markup :: Bool
  , _prompt :: Maybe Text
  }

-- | Data type for supported -dmenu output formats
data RofiFormat = FmtString
                | FmtZeroIndex
                | FmtOneIndex
                | FmtQuote
                | FmtFilter
                | FmtFilterQuote

-- | Output the selected string
s :: RofiFormat
s = FmtString

-- | Output index (0 - (N-1))
i :: RofiFormat
i = FmtZeroIndex

-- | Output index (1 - N)
d :: RofiFormat
d = FmtOneIndex

-- | Quoted string
q :: RofiFormat
q = FmtQuote

-- | Filter string (user input)
f :: RofiFormat
f = FmtFilter

-- | Quoted filter string (user input)
f' :: RofiFormat
f' = FmtFilterQuote

instance Semigroup RofiOpts where
  left <> right = RofiOpts { _format = _format right <|> _format left
                           , _markup = _markup right || _markup left
                           , _msg = _msg right <|> _msg left
                           , _prompt = _prompt right <|> _prompt left
                           }

instance Monoid RofiOpts where
  mempty = RofiOpts { _format = Nothing
                    , _markup = False
                    , _msg = Nothing
                    , _prompt = Nothing
                    }

-- | Set the -format output of dmenu command line option
rofi_format :: RofiFormat -> RofiOpts
rofi_format fmt = mempty { _format = Just fmt }

rofi_markup :: RofiOpts
rofi_markup = mempty { _markup = True }

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
        catMaybes [ arg_fmt =<< _format opts
                  , flag "-markup-rows" (_markup opts)
                  , arg "-mesg" =<< _msg opts
                  , arg "-p" =<< _prompt opts
                  ])

  second T.strip <$> procStrict "rofi" args (select input')

  where
    flag key value = if value then Just [key] else Nothing
    arg key = pure . ([key] <>) . pure
    arg_fmt = pure . (["-format"] <>) . pure . \case
      FmtString -> "s"
      FmtZeroIndex -> "i"
      FmtOneIndex -> "d"
      FmtQuote -> "q"
      FmtFilter -> "f"
      FmtFilterQuote -> "F"
