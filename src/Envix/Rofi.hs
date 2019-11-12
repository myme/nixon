module Envix.Rofi
  ( rofi
  , rofi_exec
  , rofi_format_project_name
  , rofi_markup
  , rofi_msg
  , rofi_projects
  , rofi_prompt
  , rofi_project_command
  , RofiResult(..)
  ) where

import           Control.Arrow (second)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Envix.Nix
import           Envix.Process
import           Envix.Projects
import           Envix.Projects.Types (show_command)
import           Prelude hiding (FilePath)
import qualified Turtle as Tu
import           Turtle hiding (arg, decimal, s, d, f, x, shell)

-- | Data type for command line options to rofi
data RofiOpts = RofiOpts
  {  _msg :: Maybe Text
  , _markup :: Bool
  , _prompt :: Maybe Text
  , _query :: Maybe Text
  }

instance Semigroup RofiOpts where
  left <> right = RofiOpts { _markup = _markup right || _markup left
                           , _msg = _msg right <|> _msg left
                           , _prompt = _prompt right <|> _prompt left
                           , _query = _query right <|> _query left
                           }

instance Monoid RofiOpts where
  mempty = RofiOpts { _markup = False
                    , _msg = Nothing
                    , _prompt = Nothing
                    , _query = Nothing
                    }

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
                | RofiDefault Int
                | RofiAlternate Int Int
                deriving (Eq, Show)

-- | Launch rofi with the given options and candidates
rofi :: RofiOpts -> [Line] -> IO RofiResult
rofi opts candidates = do
  let args = "-dmenu" : "-matching" : "fuzzy" : "-format" : "i" : build_args
        [ flag "-markup-rows" (_markup opts)
        , arg "-mesg" =<< _msg opts
        , arg "-p" =<< _prompt opts
        , arg "-filter" =<< _query opts
        ]

  (code, out) <- second (fmap fst . decimal) <$> procStrict "rofi" args (select candidates)
  pure $ case code of
    ExitSuccess -> either (const RofiCancel) RofiDefault out
    ExitFailure 1 -> RofiCancel
    ExitFailure c | c >= 10 && c < 20 -> either (const RofiCancel) (RofiAlternate (c - 10)) out
    _ -> undefined

text_to_line :: Text -> Line
text_to_line = fromMaybe "" . textToLine

-- | Format project names suited to rofi selection list
rofi_format_project_name :: Project -> IO Line
rofi_format_project_name project = do
  path <- implode_home (project_dir project)
  let
    pad_width = 30
    name = format fp (project_name project)
    name_padded = name <> T.replicate (pad_width - T.length name) " "
    dir = directory path
    fmt = format (Tu.s % " <i>" % fp % "</i>")
  return $ text_to_line $ fmt name_padded dir

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Maybe Text -> [Project] -> IO (Maybe Project)
rofi_projects query projects = do
  let opts =
        rofi_prompt "Select project" <>
        rofi_markup <>
        maybe mempty rofi_query query
  candidates <- traverse rofi_format_project_name projects
  rofi opts candidates >>= \case
    RofiCancel -> return Nothing
    RofiDefault idx -> return $ Just (projects !! idx)
    RofiAlternate _ idx -> return $ Just (projects !! idx)

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
  rofi opts (text_to_line . show_command <$> commands) >>= \case
    RofiDefault idx -> Just <$> resolve_command project (commands !! idx)
    _ -> return Nothing
