module Nixon.Fzf
  ( FzfOpts
  , fzf
  , fzf_border
  , fzf_exact
  , fzf_header
  , fzf_height
  , fzf_format_project_name
  , fzf_projects
  , fzf_query
  , fzf_filter
  , fzf_preview
  , fzf_project_command
  , fzf_with_edit
  , fzf_with_nth
  , fzf_no_sort
  ) where

import           Control.Arrow ((&&&), second)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.List (sort)
import qualified Data.Map as Map
import           Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import qualified Data.Text as T
import           Nixon.Command (show_parts, Command(..))
import           Nixon.Config.Options (ProjectOpts)
import qualified Nixon.Config.Options as Options
import           Nixon.Process
import           Nixon.Project
import           Nixon.Select (Candidate, Selection(..), SelectionType(..))
import qualified Nixon.Select as Select
import           Nixon.Utils
import           Prelude hiding (FilePath, filter)
import           System.Console.Haskeline
import           Turtle hiding (arg, header, readline, sort, shell, toLines, f, x)

data FzfOpts = FzfOpts
  { _border :: Bool
  , _exact :: Maybe Bool
  , _header :: Maybe Text
  , _height :: Maybe Integer
  , _query :: Maybe Text
  , _filter :: Maybe Text
  , _preview :: Maybe Text
  , _with_nth :: Maybe FieldIndex
  , _no_sort :: Bool
  }

data FieldIndex = FieldIndex Integer
                | FieldTo Integer
                | FieldFrom Integer
                | FieldRange Integer Integer
                | AllFields

instance Semigroup FzfOpts where
  left <> right = FzfOpts { _border = _border right || _border left
                          , _exact = _exact right <|> _exact left
                          , _header = _header right <|> _header left
                          , _height = _height right <|> _height left
                          , _query = _query right <|> _query left
                          , _filter = _filter right <|> _filter left
                          , _preview = _preview right <|> _preview left
                          , _with_nth = _with_nth right <|> _with_nth left
                          , _no_sort = _no_sort right || _no_sort left
                          }

instance Monoid FzfOpts where
  mempty = FzfOpts { _border = False
                   , _exact = Nothing
                   , _header = Nothing
                   , _height = Nothing
                   , _query = Nothing
                   , _filter = Nothing
                   , _preview = Nothing
                   , _with_nth = Nothing
                   , _no_sort = False
                   }

fzf_border :: FzfOpts
fzf_border = mempty { _border = True }

fzf_exact :: Bool -> FzfOpts
fzf_exact enable = mempty { _exact = Just enable }

fzf_header :: Text -> FzfOpts
fzf_header header = mempty { _header = Just header }

fzf_height :: Integer -> FzfOpts
fzf_height height = mempty { _height = Just height }

fzf_query :: Text -> FzfOpts
fzf_query query = mempty { _query = Just query }

fzf_filter :: Text -> FzfOpts
fzf_filter filter = mempty { _filter = Just filter }

fzf_preview :: Text -> FzfOpts
fzf_preview cmd = mempty { _preview = Just cmd }

fzf_with_nth :: FieldIndex -> FzfOpts
fzf_with_nth with_nth = mempty { _with_nth = Just with_nth }

fzf_no_sort :: FzfOpts
fzf_no_sort = mempty { _no_sort = True }

format_field_index :: FieldIndex -> Text
format_field_index (FieldIndex idx) = format d idx
format_field_index (FieldTo idx) = format (".."%d) idx
format_field_index (FieldFrom idx) = format (d%"..") idx
format_field_index (FieldRange start stop) = format (d%".."%d) start stop
format_field_index AllFields = ".."

fzf_raw :: MonadIO m => FzfOpts -> Shell Line -> m (Selection Text)
fzf_raw opts candidates = do
  let args = case _filter opts of
        Just filter -> ["--filter", filter]
        Nothing -> "-1" : "--expect=alt-enter" : "--ansi" : build_args
          [ flag "--border" (_border opts)
          , flag "--exact" =<< _exact opts
          , arg "--header" =<< _header opts
          , arg "--height" =<< format (d%"%") <$> _height opts
          , arg "--query" =<< _query opts
          , arg "--preview" =<< _preview opts
          , arg "--with-nth" =<< format_field_index <$> _with_nth opts
          , flag "--no-sort" (_no_sort opts)
          ]
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
      mkout = map (uncurry (format (s%" "%s)) . second Select.candidate_title)
      mkmap = Map.fromList . map (second Select.candidate_value)
  (cs_input, cs) <- (mkout &&& mkmap) . mkidx <$> shell_to_list candidates
  selection <- fzf_raw (opts <> fzf_with_nth (FieldFrom 2)) (toLines $ select cs_input)
  let selected = fmap stripAnsiEscapeCodes . flip Map.lookup cs . takeToSpace <$> selection
  pure $ case selected of
    Selection t sel -> maybe EmptySelection (Selection t) sel
    CanceledSelection -> CanceledSelection
    EmptySelection -> EmptySelection

fzf_with_edit :: (MonadIO m, MonadException m) => FzfOpts -> Shell Candidate -> m (Selection Text)
fzf_with_edit opts candidates = fzf opts candidates >>= \case
  Selection (Alternate idx) selection -> fzf_edit_selection selection >>= \case
    Just selection' -> pure $ Selection (Alternate idx) selection'
    Nothing -> pure EmptySelection
  x -> pure x

fzf_format_project_name :: MonadIO m => Project -> m (Text, Project)
fzf_format_project_name project = do
  path <- implode_home (project_path project)
  pure (format fp path, project)

-- | Find projects
fzf_projects :: MonadIO m => FzfOpts -> Maybe Text -> [Project] -> m (Maybe Project)
fzf_projects opts query projects = do
  candidates <- Map.fromList <$> traverse fzf_format_project_name projects
  let opts' = opts
        <> fzf_header "Select project"
        <> fzf_border
        -- <> fzf_height 40
        <> maybe mempty fzf_query query
        -- <> fzf_preview "ls $(eval echo {})"
  fzf opts' (Select.Identity <$> (select . sort $ Map.keys candidates)) >>= pure . \case
    Selection _ out -> Map.lookup out candidates
    _ -> Nothing

-- TODO: Add "delete from history" (alt-delete)
-- TODO: Add to shell/zsh/bash history?
-- | Find commands applicable to a project
fzf_project_command :: (MonadIO m, MonadException m) => FzfOpts -> Project -> ProjectOpts -> [Command] -> m (Maybe Command)
fzf_project_command opts project popts commands = do
  let candidates = map (format_cmd &&& id) commands
      header = format ("Select command ["%fp%"] ("%fp%")") (project_name project) (project_dir project)
      opts' = opts <> fzf_header header <> maybe mempty fzf_query (Options.command popts) <> fzf_no_sort
      input' = Select.Identity <$> select (fst <$> candidates)
  fmap (`lookup` candidates) <$> fzf opts' input' >>= \case
    Selection Default cmd -> runMaybeT $ do
      cmd' <- MaybeT (pure cmd)
      pure cmd'
    Selection (Alternate _) cmd -> pure cmd
    _ -> pure Nothing
  where format_cmd cmd = format (s%" - "%s) (cmdName cmd) (show_parts $ cmdParts cmd)

-- | Use readline to manipulate/change a fzf selection
fzf_edit_selection :: (MonadIO m, MonadException m) => Text -> m (Maybe Text)
fzf_edit_selection selection = runInputT defaultSettings $ do
  line <- getInputLineWithInitial "> " (T.unpack selection , "")
  pure $ case line of
    Just "" -> Nothing
    line'   -> T.pack <$> line'
