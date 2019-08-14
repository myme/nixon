module Envix.Rofi
  ( rofi
  , rofi_exec
  , rofi_format
  , rofi_format_project_name
  , rofi_markup
  , rofi_msg
  , rofi_projects
  , rofi_prompt
  , s, i, d, q, f, f'
  , RofiResult(..)
  ) where

import           Control.Arrow (second)
import           Data.List.NonEmpty (toList)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Envix.Nix
import           Envix.Process
import           Envix.Projects
import           Prelude hiding (FilePath)
import           Turtle hiding (arg, decimal, s, d, f)
import qualified Turtle as Tu

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

fmt_str :: IsString s => RofiFormat -> s
fmt_str = \case
  FmtString -> "s"
  FmtZeroIndex -> "i"
  FmtOneIndex -> "d"
  FmtQuote -> "q"
  FmtFilter -> "f"
  FmtFilterQuote -> "F"

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

data RofiResult = RofiCancel
                | RofiDefault Text
                | RofiAlternate Int Text
                deriving (Eq, Show)

-- | Launch rofi with the given options and candidates
rofi :: RofiOpts -> [Text] -> IO RofiResult
rofi opts candidates = do
  let input' = concatMap (toList . textToLines) candidates
      args = "-dmenu" : build_args
        [ flag "-markup-rows" (_markup opts)
        , arg "-mesg" =<< _msg opts
        , arg "-p" =<< _prompt opts
        , _format opts >>= arg_fmt "-format" fmt_str
        ]

  (code, out) <- second T.strip <$> procStrict "rofi" args (select input')
  pure $ case code of
    ExitSuccess -> RofiDefault out
    ExitFailure 1 -> RofiCancel
    ExitFailure c | c >= 10 && c < 20 -> RofiAlternate (c - 10) out
    _ -> undefined

-- | Format project names suited to rofi selection list
rofi_format_project_name :: Project -> IO Text
rofi_format_project_name project = do
  project' <- implode_home project
  let
    pad_width = 30
    name = format fp (project_name project)
    name_padded = name <> T.replicate (pad_width - T.length name) " "
    dir = project_dir project'
  return $ format (Tu.s % " <i>" % fp % "</i>") name_padded dir

-- | Build a help message of alternate commands with short description
rofi_build_message :: [(Text, Text)] -> Text
rofi_build_message = T.intercalate ", " . zipWith (curry format_command) [1 :: Int ..]
  where format_command (idx, (_, desc)) = format ("<b>Alt+"%Tu.d%"</b>: "%Tu.s) idx desc

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Commands -> [Project] -> IO (Maybe Selection)
rofi_projects commands projects = do
  let opts =
        rofi_prompt "Select project" <>
        rofi_format i <>
        rofi_markup <>
        rofi_msg (rofi_build_message commands)
      project = (projects !!) . either (const undefined) fst . decimal
  candidates <- traverse rofi_format_project_name projects
  rofi opts candidates >>= \case
    RofiCancel -> return Nothing
    RofiDefault idx -> return $ Just (project idx, "rofi -show run")
    RofiAlternate i' idx -> return $ Just (project idx, fst $ commands !! i')

rofi_exec :: Text -> Bool -> Project -> IO ()
rofi_exec command = project_exec plain with_nix
  where plain project = spawn "bash" ["-c", command] (Just $ project_path project)
        with_nix nix_file = nix_shell_spawn nix_file (Just command)
