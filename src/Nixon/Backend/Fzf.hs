{-# LANGUAGE OverloadedRecordDot #-}

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
    fzfWithNth,
    fzfNoSort,
    fzfMultiple,
    fzfDefaults,
    fzfBuildArgs,
    fzfExpect,
  )
where

import Control.Arrow (second, (&&&))
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import qualified Data.Text as T
import Nixon.Backend (Backend (..))
import Nixon.Command (Command (), show_command_with_description)
import Nixon.Config.Types (Config)
import qualified Nixon.Config.Types as Config
import Nixon.Prelude hiding (filter)
import Nixon.Process (HasProc (..), arg, build_args, flag)
import Nixon.Project
  ( Project (projectDir, projectName),
    project_path,
  )
import Nixon.Select (Candidate, Selection (..), SelectionType (..))
import qualified Nixon.Select as Select
import Nixon.Utils
  ( implode_home,
    shell_to_list,
    takeToSpace,
    toLines,
    (<<?),
  )
import Turtle
  ( ExitCode (ExitFailure, ExitSuccess),
    Line,
    Shell,
    d,
    format,
    fp,
    s,
    select,
    (%),
  )

fzfBackend :: (HasProc m, MonadIO m) => Config -> Backend m
fzfBackend cfg =
  let fzf_opts opts =
        mconcat
          $ catMaybes
            [ fzfExact <$> Config.exact_match cfg,
              fzfIgnoreCase <$> Config.ignore_case cfg,
              fzfQuery <$> Select.selector_search opts,
              fzfHeader <$> Select.selector_title opts,
              fzfMultiple <<? Select.selector_multiple opts
            ]
      fzf_opts' = fzf_opts Select.defaults
   in Backend
        { projectSelector = fzfProjects . fzf_opts,
          commandSelector = fzfProjectCommand fzf_opts',
          selector = fzf . fzf_opts
        }

data FzfOpts = FzfOpts
  { _border :: Bool,
    _exact :: Maybe Bool,
    _expectKeys :: [(Text, SelectionType)],
    _ignoreCase :: Maybe Bool,
    _header :: Maybe Text,
    _height :: Maybe Integer,
    _query :: Maybe Text,
    _filter :: Maybe Text,
    _preview :: Maybe Text,
    _withNth :: Maybe FieldIndex,
    _noSort :: Bool,
    _multi :: Bool
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
        _expectKeys = _expectKeys right <|> _expectKeys left,
        _ignoreCase = _ignoreCase right <|> _ignoreCase left,
        _header = _header right <|> _header left,
        _height = _height right <|> _height left,
        _query = _query right <|> _query left,
        _filter = _filter right <|> _filter left,
        _preview = _preview right <|> _preview left,
        _withNth = _withNth right <|> _withNth left,
        _noSort = _noSort right || _noSort left,
        _multi = _multi right || _multi left
      }

fzfDefaults :: FzfOpts
fzfDefaults =
  FzfOpts
    { _border = False,
      _exact = Nothing,
      _expectKeys = [],
      _ignoreCase = Nothing,
      _header = Nothing,
      _height = Nothing,
      _query = Nothing,
      _filter = Nothing,
      _preview = Nothing,
      _withNth = Nothing,
      _noSort = False,
      _multi = False
    }

instance Monoid FzfOpts where
  mempty = fzfDefaults

fzfBorder :: FzfOpts
fzfBorder = fzfDefaults {_border = True}

fzfExact :: Bool -> FzfOpts
fzfExact enable = fzfDefaults {_exact = Just enable}

fzfExpect :: Text -> SelectionType -> FzfOpts
fzfExpect key selection = fzfDefaults {_expectKeys = [(key, selection)]}

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

fzfMultiple :: FzfOpts
fzfMultiple = fzfDefaults {_multi = True}

formatFieldIndex :: FieldIndex -> Text
formatFieldIndex (FieldIndex idx) = format d idx
formatFieldIndex (FieldTo idx) = format (".." % d) idx
formatFieldIndex (FieldFrom idx) = format (d % "..") idx
formatFieldIndex (FieldRange start stop) = format (d % ".." % d) start stop
formatFieldIndex AllFields = ".."

fzfBuildArgs :: FzfOpts -> [Text]
fzfBuildArgs opts =
  let expect =
        if null $ _expectKeys opts
          then Nothing
          else Just $ T.intercalate "," (map fst $ _expectKeys opts)
   in build_args
        [ flag "--border" (_border opts),
          flag "--exact" =<< _exact opts,
          arg "--expect" =<< expect,
          flag "-i" =<< _ignoreCase opts,
          arg "--header" =<< _header opts,
          arg "--height" . format (d % "%") =<< _height opts,
          arg "--query" =<< _query opts,
          arg "--filter" =<< _filter opts,
          arg "--preview" =<< _preview opts,
          arg "--with-nth" . formatFieldIndex =<< _withNth opts,
          flag "--no-sort" (_noSort opts),
          flag "--multi" (_multi opts)
        ]

fzfRaw :: (HasProc m) => FzfOpts -> Shell Line -> m (Selection Text)
fzfRaw opts candidates = do
  let args = case _filter opts of
        Just filter -> ["--filter", filter]
        Nothing ->
          "-1"
            : "--ansi"
            : fzfBuildArgs opts
  (code, out) <- second T.lines <$> proc' "fzf" args candidates
  pure $ case code of
    ExitFailure 1 -> EmptySelection
    ExitFailure 130 -> CanceledSelection
    ExitFailure _ -> undefined
    ExitSuccess ->
      if isJust (_filter opts) || null (_expectKeys opts)
        then Selection Default out
        else case out of
          [] -> EmptySelection
          [""] -> EmptySelection
          ("" : selection) -> Selection Default selection
          (key : selection) -> case lookup key $ _expectKeys opts of
            Just alternative -> Selection alternative selection
            Nothing -> EmptySelection

fzf :: (HasProc m, MonadIO m) => FzfOpts -> Shell Candidate -> m (Selection Text)
fzf opts candidates = do
  let mkidx = zip (map (T.pack . show) [1 :: Int ..])
      mkout = map (uncurry (format (s % " " % s)) . second Select.candidate_title)
      mkmap = Map.fromList . map (second Select.candidate_value)
  if isJust $ _filter opts
    then fzfRaw opts $ toLines $ Select.candidate_title <$> candidates
    else do
      (cs_input, cs) <- (mkout &&& mkmap) . mkidx <$> shell_to_list candidates
      selection <- fzfRaw (opts <> fzfWithNth (FieldFrom 2)) (toLines $ select cs_input)
      let selected = fmap stripAnsiEscapeCodes . flip Map.lookup cs . takeToSpace <$> selection
      pure $ case selected of
        Selection _ [] -> EmptySelection
        Selection t sel -> maybe EmptySelection (Selection t) (sequence sel)
        CanceledSelection -> CanceledSelection
        EmptySelection -> EmptySelection

fzfFormatProjectName :: (MonadIO m) => Project -> m (Text, Project)
fzfFormatProjectName project = do
  path <- implode_home (project_path project)
  pure (format fp path, project)

-- | Find projects
fzfProjects :: (HasProc m, MonadIO m) => FzfOpts -> Maybe Text -> [Project] -> m (Selection Project)
fzfProjects opts query projects = do
  candidates <- Map.fromList <$> traverse fzfFormatProjectName projects
  let opts' =
        opts
          <> fzfHeader "Select project"
          <> fzfBorder
          -- <> fzfHeight 40
          <> maybe mempty fzfQuery query
          <> fzfExpect "f1" Show
  -- <> fzfPreview "ls $(eval echo {})"
  selection <- fzf opts' (Select.Identity <$> (select . sort $ Map.keys candidates))
  pure $ Select.catMaybeSelection ((`Map.lookup` candidates) <$> selection)

-- | Find commands applicable to a project
fzfProjectCommand :: (HasProc m, MonadIO m) => FzfOpts -> Project -> Text -> Maybe Text -> [Command] -> m (Selection Command)
fzfProjectCommand opts project prompt query commands = do
  let candidates = map (show_command_with_description &&& id) commands
      header = format (s % " [" % fp % "] (" % fp % ")") prompt project.projectName project.projectDir
      opts' =
        opts
          <> fzfHeader header
          <> maybe mempty fzfQuery query
          <> fzfNoSort
          <> fzfExpect "alt-enter" Edit
          <> fzfExpect "f1" Show
          <> fzfExpect "f2" Visit
      input' = Select.Identity <$> select (fst <$> candidates)
  selection <- fmap (`lookup` candidates) <$> fzf opts' input'
  pure $ Select.catMaybeSelection selection
