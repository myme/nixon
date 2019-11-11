module Envix.Rofi
  ( rofi
  , rofi_exec
  , rofi_format
  , rofi_format_project_name
  , rofi_markup
  , rofi_msg
  , rofi_projects
  , rofi_prompt
  , rofi_project_command
  , s, i, d, q, f, f'
  , RofiResult(..)
  ) where

import           Control.Arrow (second)
import           Data.List.NonEmpty (toList)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Envix.Nix
import           Envix.Process
import           Envix.Projects
import           Prelude hiding (FilePath)
import qualified Turtle as Tu
import           Turtle hiding (arg, decimal, s, d, f, shell)

-- | Data type for command line options to rofi
data RofiOpts = RofiOpts
  { _format :: Maybe RofiFormat
  , _msg :: Maybe Text
  , _markup :: Bool
  , _prompt :: Maybe Text
  , _query :: Maybe Text
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
                           , _query = _query right <|> _query left
                           }

instance Monoid RofiOpts where
  mempty = RofiOpts { _format = Nothing
                    , _markup = False
                    , _msg = Nothing
                    , _prompt = Nothing
                    , _query = Nothing
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

rofi_query :: Text -> RofiOpts
rofi_query query = mempty { _query = Just query }

data RofiResult = RofiCancel
                | RofiDefault Text
                | RofiAlternate Int Text
                deriving (Eq, Show)

-- | Launch rofi with the given options and candidates
rofi :: RofiOpts -> [Text] -> IO RofiResult
rofi opts candidates = do
  let input' = concatMap (toList . textToLines) candidates
      args = "-dmenu" : "-matching" : "fuzzy" : build_args
        [ flag "-markup-rows" (_markup opts)
        , arg "-mesg" =<< _msg opts
        , arg "-p" =<< _prompt opts
        , _format opts >>= arg_fmt "-format" fmt_str
        , arg "-filter" =<< _query opts
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
  path <- implode_home (project_dir project)
  let
    pad_width = 30
    name = format fp (project_name project)
    name_padded = name <> T.replicate (pad_width - T.length name) " "
    dir = directory path
  return $ format (Tu.s % " <i>" % fp % "</i>") name_padded dir

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Maybe Text -> [Project] -> IO (Maybe Project)
rofi_projects query projects = do
  let opts =
        rofi_prompt "Select project" <>
        rofi_format i <>
        rofi_markup <>
        maybe mempty rofi_query query
      project = (projects !!) . either (const undefined) fst . decimal
  candidates <- traverse rofi_format_project_name projects
  rofi opts candidates >>= \case
    RofiCancel -> return Nothing
    RofiDefault idx -> return $ Just (project idx)
    RofiAlternate _ idx -> return $ Just (project idx)

rofi_exec :: Maybe Text -> Bool -> Project -> IO ()
rofi_exec cmd = project_exec plain with_nix
  where plain project = do
          shell <- fromMaybe "bash" <$> need "SHELL"
          spawn (shell : bash_args) (Just $ project_path project)
        with_nix nix_file = nix_shell_spawn nix_file cmd
        bash_args = build_args [arg "-c" =<< cmd]

rofi_project_command :: Maybe Text -> Project -> IO (Maybe Text)
rofi_project_command query project = do
  let commands = find_project_commands project
      opts = rofi_prompt "Select command"
        <> maybe mempty rofi_query query
  rofi opts commands >>= \case
    RofiDefault cmd -> return $ Just cmd
    _ -> return Nothing
