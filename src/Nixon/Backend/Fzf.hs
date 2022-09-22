module Nixon.Backend.Fzf
  ( FzfOpts,
    FieldIndex (..),
    fzfBackend,
    fzf,
    fzfBorder,
    fzfExact,
    fzfIgnoreCase,
    fzfHeader,
    fzfHeight,
    fzfFormatProjectName,
    fzfProjects,
    fzfQuery,
    fzfFilter,
    fzfPreview,
    fzfProjectCommand,
    fzfWithEdit,
    fzfWithNth,
    fzfNoSort,
    fzfDefaults,
    fzfBuildArgs,
  )
where

import Control.Arrow (second, (&&&))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.List (sort)
import qualified Data.Map as Map
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import qualified Data.Text as T
import Nixon.Backend (Backend (..))
import Nixon.Command (Command (cmdSource), show_command_with_description)
import Nixon.Config.Options (RunOpts)
import qualified Nixon.Config.Options as Options
import Nixon.Config.Types (Config)
import qualified Nixon.Config.Types as Config
import Nixon.Process (arg, build_args, flag)
import Nixon.Project
  ( Project (project_dir, project_name),
    project_path,
  )
import Nixon.Select (Candidate, Selection (..), SelectionType (..))
import qualified Nixon.Select as Select
import Nixon.Utils
  ( implode_home,
    shell_to_list,
    takeToSpace,
    toLines,
  )
import System.Console.Haskeline
  ( defaultSettings,
    getInputLineWithInitial,
    runInputT,
  )
import Turtle
  ( Alternative ((<|>)),
    ExitCode (ExitFailure, ExitSuccess),
    Line,
    MonadIO,
    Shell,
    Text,
    d,
    format,
    fp,
    procStrict,
    s,
    select,
    (%),
    (<&>),
  )
import Prelude hiding (FilePath, filter)
import Data.Maybe (catMaybes)

fzfBackend :: Config -> Backend
fzfBackend cfg =
  let fzf_opts opts =
        mconcat $
          catMaybes
            [ fzfExact <$> Config.exact_match cfg,
              fzfIgnoreCase <$> Config.ignore_case cfg,
              fzfQuery <$> Select.selector_search opts,
              fzfHeader <$> Select.selector_title opts
            ]
      fzf_opts' = fzf_opts Select.defaults
   in Backend
        { projectSelector = fzfProjects fzf_opts',
          commandSelector = fzfProjectCommand fzf_opts',
          selector = fzfWithEdit . fzf_opts
        }

data FzfOpts = FzfOpts
  { _border :: Bool,
    _exact :: Maybe Bool,
    _ignoreCase :: Maybe Bool,
    _header :: Maybe Text,
    _height :: Maybe Integer,
    _query :: Maybe Text,
    _filter :: Maybe Text,
    _preview :: Maybe Text,
    _withNth :: Maybe FieldIndex,
    _noSort :: Bool
  }
  deriving (Eq, Show)

data FieldIndex
  = FieldIndex Integer
  | FieldTo Integer
  | FieldFrom Integer
  | FieldRange Integer Integer
  | AllFields
  deriving (Eq, Show)

instance Semigroup FzfOpts where
  left <> right =
    FzfOpts
      { _border = _border right || _border left,
        _exact = _exact right <|> _exact left,
        _ignoreCase = _ignoreCase right <|> _ignoreCase left,
        _header = _header right <|> _header left,
        _height = _height right <|> _height left,
        _query = _query right <|> _query left,
        _filter = _filter right <|> _filter left,
        _preview = _preview right <|> _preview left,
        _withNth = _withNth right <|> _withNth left,
        _noSort = _noSort right || _noSort left
      }

fzfDefaults :: FzfOpts
fzfDefaults =
  FzfOpts
    { _border = False,
      _exact = Nothing,
      _ignoreCase = Nothing,
      _header = Nothing,
      _height = Nothing,
      _query = Nothing,
      _filter = Nothing,
      _preview = Nothing,
      _withNth = Nothing,
      _noSort = False
    }

instance Monoid FzfOpts where
  mempty = fzfDefaults

fzfBorder :: FzfOpts
fzfBorder = fzfDefaults {_border = True}

fzfExact :: Bool -> FzfOpts
fzfExact enable = fzfDefaults {_exact = Just enable}

fzfIgnoreCase :: Bool -> FzfOpts
fzfIgnoreCase enable = fzfDefaults {_ignoreCase = Just enable}

fzfHeader :: Text -> FzfOpts
fzfHeader header = fzfDefaults {_header = Just header}

fzfHeight :: Integer -> FzfOpts
fzfHeight height = fzfDefaults {_height = Just height}

fzfQuery :: Text -> FzfOpts
fzfQuery query = fzfDefaults {_query = Just query}

fzfFilter :: Text -> FzfOpts
fzfFilter filter = fzfDefaults {_filter = Just filter}

fzfPreview :: Text -> FzfOpts
fzfPreview cmd = fzfDefaults {_preview = Just cmd}

fzfWithNth :: FieldIndex -> FzfOpts
fzfWithNth with_nth = fzfDefaults {_withNth = Just with_nth}

fzfNoSort :: FzfOpts
fzfNoSort = fzfDefaults {_noSort = True}

formatFieldIndex :: FieldIndex -> Text
formatFieldIndex (FieldIndex idx) = format d idx
formatFieldIndex (FieldTo idx) = format (".." % d) idx
formatFieldIndex (FieldFrom idx) = format (d % "..") idx
formatFieldIndex (FieldRange start stop) = format (d % ".." % d) start stop
formatFieldIndex AllFields = ".."

fzfBuildArgs :: FzfOpts -> [Text]
fzfBuildArgs opts =
  build_args
    [ flag "--border" (_border opts),
      flag "--exact" =<< _exact opts,
      flag "-i" =<< _ignoreCase opts,
      arg "--header" =<< _header opts,
      arg "--height" . format (d % "%") =<< _height opts,
      arg "--query" =<< _query opts,
      arg "--filter" =<< _filter opts,
      arg "--preview" =<< _preview opts,
      arg "--with-nth" . formatFieldIndex =<< _withNth opts,
      flag "--no-sort" (_noSort opts)
    ]

fzfRaw :: MonadIO m => FzfOpts -> Shell Line -> m (Selection Text)
fzfRaw opts candidates = do
  let args = case _filter opts of
        Just filter -> ["--filter", filter]
        Nothing ->
          "-1" :
          "--expect=alt-enter" :
          "--ansi" :
          fzfBuildArgs opts
  (code, out) <- procStrict "fzf" args candidates
  pure $ case _filter opts of
    Just _ -> Selection Default out
    Nothing -> case code of
      ExitFailure 1 -> EmptySelection
      ExitFailure 130 -> CanceledSelection
      ExitFailure _ -> undefined
      ExitSuccess -> case T.lines out of
        ["", selection] -> Selection Default selection
        ["alt-enter", selection] -> Selection (Alternate 0) selection
        _ -> undefined

fzf :: MonadIO m => FzfOpts -> Shell Candidate -> m (Selection Text)
fzf opts candidates = do
  let mkidx = zip (map (T.pack . show) [1 :: Int ..])
      mkout = map (uncurry (format (s % " " % s)) . second Select.candidate_title)
      mkmap = Map.fromList . map (second Select.candidate_value)
  (cs_input, cs) <- (mkout &&& mkmap) . mkidx <$> shell_to_list candidates
  selection <- fzfRaw (opts <> fzfWithNth (FieldFrom 2)) (toLines $ select cs_input)
  let selected = fmap stripAnsiEscapeCodes . flip Map.lookup cs . takeToSpace <$> selection
  pure $ case selected of
    Selection t sel -> maybe EmptySelection (Selection t) sel
    CanceledSelection -> CanceledSelection
    EmptySelection -> EmptySelection

fzfWithEdit :: (MonadIO m, MonadMask m) => FzfOpts -> Shell Candidate -> m (Selection Text)
fzfWithEdit opts candidates =
  fzf opts candidates >>= \case
    Selection (Alternate idx) selection ->
      fzfEditSelection selection >>= \case
        Just selection' -> pure $ Selection (Alternate idx) selection'
        Nothing -> pure EmptySelection
    x -> pure x

fzfFormatProjectName :: MonadIO m => Project -> m (Text, Project)
fzfFormatProjectName project = do
  path <- implode_home (project_path project)
  pure (format fp path, project)

-- | Find projects
fzfProjects :: MonadIO m => FzfOpts -> Maybe Text -> [Project] -> m (Maybe Project)
fzfProjects opts query projects = do
  candidates <- Map.fromList <$> traverse fzfFormatProjectName projects
  let opts' =
        opts
          <> fzfHeader "Select project"
          <> fzfBorder
          -- <> fzfHeight 40
          <> maybe mempty fzfQuery query
  -- <> fzfPreview "ls $(eval echo {})"
  fzf opts' (Select.Identity <$> (select . sort $ Map.keys candidates))
    <&> ( \case
            Selection _ out -> Map.lookup out candidates
            _ -> Nothing
        )

-- | Find commands applicable to a project
fzfProjectCommand :: (MonadIO m, MonadMask m) => FzfOpts -> Project -> RunOpts -> [Command] -> m (Maybe Command)
fzfProjectCommand opts project popts commands = do
  let candidates = map (show_command_with_description &&& id) commands
      header = format ("Select command [" % fp % "] (" % fp % ")") (project_name project) (project_dir project)
      opts' = opts <> fzfHeader header <> maybe mempty fzfQuery (Options.run_command popts) <> fzfNoSort
      input' = Select.Identity <$> select (fst <$> candidates)
  selection <- fmap (`lookup` candidates) <$> fzf opts' input'
  case selection of
    Selection Default cmd -> pure cmd
    Selection (Alternate _) cmd -> runMaybeT $ do
      cmd' <- MaybeT (pure cmd)
      edited <- MaybeT $ fzfEditSelection (cmdSource cmd')
      pure cmd' {cmdSource = edited}
    _ -> pure Nothing

-- | Use readline to manipulate/change a fzf selection
fzfEditSelection :: (MonadIO m, MonadMask m) => Text -> m (Maybe Text)
fzfEditSelection selection = runInputT defaultSettings $ do
  line <- getInputLineWithInitial "> " (T.unpack selection, "")
  pure $ case line of
    Just "" -> Nothing
    line' -> T.pack <$> line'
