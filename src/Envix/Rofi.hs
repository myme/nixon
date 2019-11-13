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
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Envix.Process
import           Envix.Projects
import           Envix.Projects.Types (show_command)
import           Envix.Select hiding (select)
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
rofi :: RofiOpts -> Shell Line -> IO (Selection Text)
rofi opts candidates = do
  let args = "-dmenu" : "-matching" : "fuzzy" : build_args
        [ flag "-markup-rows" (_markup opts)
        , arg "-mesg" =<< _msg opts
        , arg "-p" =<< _prompt opts
        , arg "-filter" =<< _query opts
        ]

  (code, out) <- second T.strip <$> procStrict "rofi" args candidates
  pure $ case code of
    ExitSuccess -> Selection Default out
    ExitFailure 1 -> CanceledSelection
    ExitFailure c | c >= 10 && c < 20 -> Selection (Alternate (c - 10)) out
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
    fmt = format (Tu.s % " <i>" % fp % "</i>")
  return $ fmt name_padded dir

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Maybe Text -> [Project] -> IO (Maybe Project)
rofi_projects query projects = do
  let opts =
        rofi_prompt "Select project" <>
        rofi_markup <>
        maybe mempty rofi_query query
  candidates <- traverse rofi_format_project_name projects
  let map' = Map.fromList (zip candidates projects)
  rofi opts (select $ text_to_line <$> candidates) >>= \case
    Selection _ key -> pure $ Map.lookup key map'
    _ -> return Nothing

rofi_exec :: Command -> Project -> IO ()
rofi_exec cmd project = do
  cmd' <- runSelect (rofi mempty) $ resolve_command project cmd
  shell <- fromMaybe "bash" <$> need "SHELL"
  spawn (shell : ["-c", cmd']) (Just $ project_path project)

rofi_project_command :: Maybe Text -> Project -> IO (Maybe Command)
rofi_project_command query project = do
  let commands = build_map show_command $ find_project_commands project
      opts = rofi_prompt "Select command" <> maybe mempty rofi_query query
  rofi opts (select $ text_to_line <$> Map.keys commands) >>= \case
    Selection _ txt -> pure $ Map.lookup txt commands
    _ -> return Nothing
