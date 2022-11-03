module Nixon.Backend.Rofi
  ( rofi,
    rofiFormatProjectName,
    rofiExact,
    rofiIgnoreCase,
    rofiMarkup,
    rofiMsg,
    rofiProjects,
    rofiPrompt,
    rofiProjectCommand,
    rofiQuery,
    RofiResult (..),
    rofiBackend,
  )
where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Nixon.Backend (Backend (..))
import Nixon.Command (Command, show_command_with_description)
import Nixon.Config.Types (Config)
import qualified Nixon.Config.Types as Config
import Nixon.Process (arg, arg_fmt, build_args, flag)
import Nixon.Project (Project (project_dir, project_name))
import Nixon.Select (Candidate, Selection (..), SelectionType (..))
import qualified Nixon.Select as Select
import Nixon.Utils (implode_home, shell_to_list, toLines, (<<?))
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
  )
import qualified Turtle as Tu
import Prelude hiding (FilePath)

rofiBackend :: Config -> Backend
rofiBackend cfg =
  let rofi_opts opts =
        mconcat $
          catMaybes
            [ rofiExact <$> Config.exact_match cfg,
              rofiIgnoreCase <$> Config.ignore_case cfg,
              rofiQuery <$> Select.selector_search opts,
              rofiPrompt <$> Select.selector_title opts,
              rofiMultiple <<? Select.selector_multiple opts
            ]
      rofi_opts' = rofi_opts Select.defaults
   in Backend
        { projectSelector = rofiProjects rofi_opts',
          commandSelector = const $ rofiProjectCommand rofi_opts',
          selector = rofi . rofi_opts
        }

-- | Data type for command line options to rofi
data RofiOpts = RofiOpts
  { _exact :: Maybe Bool,
    _ignore_case :: Maybe Bool,
    _msg :: Maybe Text,
    _markup :: Bool,
    _multi :: Bool,
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
        _multi = _multi right || _multi left,
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
        _multi = False,
        _prompt = Nothing,
        _query = Nothing
      }

rofiExact :: Bool -> RofiOpts
rofiExact exact = mempty {_exact = Just exact}

rofiIgnoreCase :: Bool -> RofiOpts
rofiIgnoreCase ignore_case = mempty {_ignore_case = Just ignore_case}

rofiMarkup :: RofiOpts
rofiMarkup = mempty {_markup = True}

-- | Set -mesg command line option
rofiMsg :: Text -> RofiOpts
rofiMsg msg = mempty {_msg = Just msg}

-- | Set -p|--prompt command line option
rofiPrompt :: Text -> RofiOpts
rofiPrompt prompt = mempty {_prompt = Just prompt}

rofiQuery :: Text -> RofiOpts
rofiQuery query = mempty {_query = Just query}

rofiMultiple :: RofiOpts
rofiMultiple = mempty {_multi = True}

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
            flag "-multi-select" (_multi opts),
            arg "-mesg" =<< _msg opts,
            arg "-p" =<< _prompt opts,
            arg "-filter" =<< _query opts
          ]

  map' <- Map.fromList . fmap (Select.candidate_title &&& Select.candidate_value) <$> shell_to_list candidates
  (code, out) <- procStrict "rofi" args (toLines $ select $ Map.keys map')
  let out' = (fmap (flip Map.lookup map' . T.strip) . T.lines) out
  pure $ case code of
    ExitSuccess -> mkselection Default out'
    ExitFailure 1 -> CanceledSelection
    ExitFailure c
      | c == 10 -> mkselection Edit out'
      | c == 11 -> mkselection Show out'
      | c == 12 -> mkselection Visit out'
      | otherwise -> error $ "Exit error: " <> show c
  where
    mkselection _ [] = EmptySelection
    mkselection type' sel = Selection type' (catMaybes sel)

-- | Format project names suited to rofi selection list
rofiFormatProjectName :: MonadIO m => Project -> m Text
rofiFormatProjectName project = do
  path <- implode_home (project_dir project)
  let pad_width = 30
      name = format fp (project_name project)
      name_padded = name <> T.replicate (pad_width - T.length name) " "
      dir = directory path
      fmt = format (Tu.s % " <i>" % fp % "</i>")
  pure $ fmt name_padded dir

-- | Launch rofi with a list of projects as candidates
rofiProjects :: MonadIO m => RofiOpts -> Maybe Text -> [Project] -> m (Selection Project)
rofiProjects opts query projects = do
  let opts' =
        opts
          <> rofiPrompt "Select project"
          <> rofiMarkup
          <> maybe mempty rofiQuery query
  candidates <- traverse rofiFormatProjectName projects
  let map' = Map.fromList (zip candidates projects)
  selection <- rofi opts' (Select.Identity <$> select candidates)
  pure $ Select.catMaybeSelection ((`Map.lookup` map') <$> selection)

rofiProjectCommand :: MonadIO m => RofiOpts -> Maybe Text -> [Command] -> m (Selection Command)
rofiProjectCommand opts query commands = do
  let candidates = Select.build_map show_command_with_description commands
      opts' = opts <> rofiPrompt "Select command" <> maybe mempty rofiQuery query
  selection <- rofi opts' (Select.Identity <$> select (Map.keys candidates))
  pure $ Select.catMaybeSelection ((`Map.lookup` candidates) <$> selection)
