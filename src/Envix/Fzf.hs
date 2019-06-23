module Envix.Fzf
  ( fzf
  , fzf_border
  , fzf_header
  , fzf_height
  ) where

import           Control.Arrow (second)
import           Data.List.NonEmpty (toList)
import qualified Data.Text as T
import           Envix.Process
import           Turtle hiding (arg, header)

data FzfOpts = FzfOpts
  { _border :: Bool
  , _header :: Maybe Text
  , _height :: Maybe Integer
  }

instance Semigroup FzfOpts where
  left <> right = FzfOpts { _border = _border right || _border left
                          , _header = _header right <|> _header left
                          , _height = _height right <|> _height left
                          }

instance Monoid FzfOpts where
  mempty = FzfOpts { _border = False, _header = Nothing, _height = Nothing }

fzf_border :: FzfOpts
fzf_border = mempty { _border = True }

fzf_header :: Text -> FzfOpts
fzf_header header = mempty { _header = Just header }

fzf_height :: Integer -> FzfOpts
fzf_height height = mempty { _height = Just height }

fzf :: FzfOpts -> [Text] -> IO ()
fzf opts candidates = do
  let input' = concatMap (toList . textToLines) candidates
      args = build_args
        [ flag "--border" (_border opts)
        , arg "--header" =<< _header opts
        , arg "--height" =<< format (d%"%") <$> _height opts
        ]
  (code, out) <- second T.strip <$> procStrict "fzf" args (select input')
  print (code, out)
