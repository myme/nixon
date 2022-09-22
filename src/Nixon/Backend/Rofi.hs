module Nixon.Backend.Rofi
  ( rofi,
    rofi_format_project_name,
    rofi_exact,
    rofi_ignore_case,
    rofi_markup,
    rofi_msg,
    rofi_projects,
    rofi_prompt,
    rofi_project_command,
    rofi_query,
    RofiResult (..),
    rofiBackend,
  )
where

import Control.Arrow (second, (&&&))
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Nixon.Backend (Backend (..))
import Nixon.Command (Command, show_command_with_description)
import Nixon.Config.Options (RunOpts)
import qualified Nixon.Config.Options as Options
import Nixon.Config.Types (Config)
import qualified Nixon.Config.Types as Config
import Nixon.Process (arg, arg_fmt, build_args, flag)
import Nixon.Project (Project (project_dir, project_name))
import Nixon.Select (Candidate, Selection (..), SelectionType (..))
import qualified Nixon.Select as Select
import Nixon.Utils (implode_home, shell_to_list, toLines)
import Turtle
  ( Alternative ((<|>)),
    ExitCode (ExitFailure, ExitSuccess),
    MonadIO,
    Shell,
    Text,
    directory,
    format,
    fp,
    procStrict,
    select,
    (%),
    (<&>),
  )
import qualified Turtle as Tu
import Prelude hiding (FilePath)

rofiBackend :: Config -> Backend
rofiBackend cfg =
  let rofi_opts opts =
        mconcat $
          catMaybes
            [ rofi_exact <$> Config.exact_match cfg,
              rofi_ignore_case <$> Config.ignore_case cfg,
              rofi_query <$> Select.selector_search opts,
              rofi_prompt <$> Select.selector_title opts
            ]
      rofi_opts' = rofi_opts Select.defaults
   in Backend
        { projectSelector = rofi_projects rofi_opts',
          commandSelector = const $ rofi_project_command rofi_opts',
          selector = rofi . rofi_opts
        }

-- | Data type for command line options to rofi
data RofiOpts = RofiOpts
  { _exact :: Maybe Bool,
    _ignore_case :: Maybe Bool,
    _msg :: Maybe Text,
    _markup :: Bool,
    _prompt :: Maybe Text,
    _query :: Maybe Text
  }
  deriving (Show)

instance Semigroup RofiOpts where
  left <> right =
    RofiOpts
      { _exact = _exact right <|> _exact left,
        _ignore_case = _ignore_case right <|> _ignore_case left,
        _markup = _markup right || _markup left,
        _msg = _msg right <|> _msg left,
        _prompt = _prompt right <|> _prompt left,
        _query = _query right <|> _query left
      }

instance Monoid RofiOpts where
  mempty =
    RofiOpts
      { _exact = Nothing,
        _ignore_case = Nothing,
        _markup = False,
        _msg = Nothing,
        _prompt = Nothing,
        _query = Nothing
      }

rofi_exact :: Bool -> RofiOpts
rofi_exact exact = mempty {_exact = Just exact}

rofi_ignore_case :: Bool -> RofiOpts
rofi_ignore_case ignore_case = mempty {_ignore_case = Just ignore_case}

rofi_markup :: RofiOpts
rofi_markup = mempty {_markup = True}

-- | Set -mesg command line option
rofi_msg :: Text -> RofiOpts
rofi_msg msg = mempty {_msg = Just msg}

-- | Set -p|--prompt command line option
rofi_prompt :: Text -> RofiOpts
rofi_prompt prompt = mempty {_prompt = Just prompt}

rofi_query :: Text -> RofiOpts
rofi_query query = mempty {_query = Just query}

data RofiResult
  = RofiCancel
  | RofiDefault Int
  | RofiAlternate Int Int
  deriving (Eq, Show)

-- | Launch rofi with the given options and candidates
rofi :: MonadIO m => RofiOpts -> Shell Candidate -> m (Selection Text)
rofi opts candidates = do
  let args =
        "-dmenu" :
        build_args
          [ arg_fmt "-matching" (bool "fuzzy" "normal" . fromMaybe False) (_exact opts),
            flag "-i" =<< _ignore_case opts,
            flag "-markup-rows" (_markup opts),
            arg "-mesg" =<< _msg opts,
            arg "-p" =<< _prompt opts,
            arg "-filter" =<< _query opts
          ]

  map' <- Map.fromList . fmap (Select.candidate_title &&& Select.candidate_value) <$> shell_to_list candidates
  (code, out) <- second (flip Map.lookup map' . T.strip) <$> procStrict "rofi" args (toLines $ select $ Map.keys map')
  pure $ case code of
    ExitSuccess -> mkselection Default out
    ExitFailure 1 -> CanceledSelection
    ExitFailure c
      | c >= 10 && c < 20 -> mkselection (Alternate (c - 10)) out
      | otherwise -> error $ "Exit error: " <> show c
  where
    mkselection _ Nothing = EmptySelection
    mkselection type' (Just selection) = Selection type' selection

-- | Format project names suited to rofi selection list
rofi_format_project_name :: MonadIO m => Project -> m Text
rofi_format_project_name project = do
  path <- implode_home (project_dir project)
  let pad_width = 30
      name = format fp (project_name project)
      name_padded = name <> T.replicate (pad_width - T.length name) " "
      dir = directory path
      fmt = format (Tu.s % " <i>" % fp % "</i>")
  pure $ fmt name_padded dir

-- | Launch rofi with a list of projects as candidates
rofi_projects :: MonadIO m => RofiOpts -> Maybe Text -> [Project] -> m (Maybe Project)
rofi_projects opts query projects = do
  let opts' =
        opts
          <> rofi_prompt "Select project"
          <> rofi_markup
          <> maybe mempty rofi_query query
  candidates <- traverse rofi_format_project_name projects
  let map' = Map.fromList (zip candidates projects)
  rofi opts' (Select.Identity <$> select candidates)
    <&> ( \case
            Selection _ key -> Map.lookup key map'
            _ -> Nothing
        )

rofi_project_command :: MonadIO m => RofiOpts -> RunOpts -> [Command] -> m (Maybe Command)
rofi_project_command opts popts commands = do
  let candidates = Select.build_map show_command_with_description commands
      opts' = opts <> rofi_prompt "Select command" <> maybe mempty rofi_query (Options.run_command popts)
  rofi opts' (Select.Identity <$> select (Map.keys candidates))
    <&> ( \case
            Selection _ txt -> Map.lookup txt candidates
            _ -> Nothing
        )
