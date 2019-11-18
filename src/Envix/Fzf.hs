module Envix.Fzf
  ( fzf
  , fzf_border
  , fzf_header
  , fzf_height
  , fzf_exec
  , fzf_format_project_name
  , fzf_projects
  , fzf_query
  , fzf_filter
  , fzf_preview
  , fzf_project_command
  , fzf_with_nth
  ) where

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Envix.Process
import           Envix.Projects
import           Envix.Projects.Types hiding (path)
import           Envix.Select hiding (select)
import           Prelude hiding (FilePath, filter)
import           System.Console.Haskeline
import           System.Console.Haskeline.History (historyLines, readHistory)
import           Turtle hiding (arg, header, readline, sort, shell, f, x)

data FzfOpts = FzfOpts
  { _border :: Bool
  , _header :: Maybe Text
  , _height :: Maybe Integer
  , _query :: Maybe Text
  , _filter :: Maybe Text
  , _preview :: Maybe Text
  , _with_nth :: Maybe FieldIndex
  }

data FieldIndex = FieldIndex Integer
                | FieldTo Integer
                | FieldFrom Integer
                | FieldRange Integer Integer
                | AllFields

instance Semigroup FzfOpts where
  left <> right = FzfOpts { _border = _border right || _border left
                          , _header = _header right <|> _header left
                          , _height = _height right <|> _height left
                          , _query = _query right <|> _query left
                          , _filter = _filter right <|> _filter left
                          , _preview = _preview right <|> _preview left
                          , _with_nth = _with_nth right <|> _with_nth left
                          }

instance Monoid FzfOpts where
  mempty = FzfOpts { _border = False
                   , _header = Nothing
                   , _height = Nothing
                   , _query = Nothing
                   , _filter = Nothing
                   , _preview = Nothing
                   , _with_nth = Nothing
                   }

fzf_border :: FzfOpts
fzf_border = mempty { _border = True }

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

format_field_index :: FieldIndex -> Text
format_field_index (FieldIndex idx) = format d idx
format_field_index (FieldTo idx) = format (".."%d) idx
format_field_index (FieldFrom idx) = format (d%"..") idx
format_field_index (FieldRange start stop) = format (d%".."%d) start stop
format_field_index AllFields = ".."

fzf :: FzfOpts -> Shell Line -> IO (Selection Text)
fzf opts candidates = do
  let args = case _filter opts of
        Just filter -> ["--filter", filter]
        Nothing -> "-1" : "--expect=alt-enter" : "--ansi" : build_args
          [ flag "--border" (_border opts)
          , arg "--header" =<< _header opts
          , arg "--height" =<< format (d%"%") <$> _height opts
          , arg "--query" =<< _query opts
          , arg "--preview" =<< _preview opts
          , arg "--with-nth" =<< format_field_index <$> _with_nth opts
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

fzf_exec :: Command -> Project -> IO ()
fzf_exec cmd project = do
  cmd' <- runSelect (fzf mempty) $ resolve_command project cmd
  run [cmd'] (Just $ project_path project)

fzf_format_project_name :: Project -> IO (Text, Project)
fzf_format_project_name project = do
  path <- implode_home (project_path project)
  pure (format fp path, project)

-- | Find projects
fzf_projects :: Maybe Text -> [Project] -> IO (Maybe Project)
fzf_projects query projects = do
  candidates <- Map.fromList <$> traverse fzf_format_project_name projects
  let opts = fzf_header "Select project"
        <> fzf_border
        -- <> fzf_height 40
        <> maybe mempty fzf_query query
        -- <> fzf_preview "ls $(eval echo {})"
  fzf opts (select . map text_to_line . sort $ Map.keys candidates) >>= \case
    Selection _ out -> pure (Map.lookup out candidates)
    _ -> pure Nothing

project_history_file :: FilePath -> FilePath
project_history_file = (</> ".envix_history")

-- TODO: Add "delete from history" (alt-delete)
-- | Find commands applicable to a project
fzf_project_command :: Maybe Text -> Project -> IO (Maybe Command)
fzf_project_command query project = do
  let path = project_path project
      history_file = T.unpack $ format fp $ project_history_file path
  history <- map fromString . historyLines <$> readHistory history_file
  let commands = build_map show_command $ (history ++) $ find_project_commands project
      header = format ("Select command ["%fp%"] ("%fp%")") (project_name project) (project_dir project)
      opts = fzf_header header <> maybe mempty fzf_query query
      input' = select $ text_to_line <$> Map.keys commands
  fmap (`Map.lookup` commands) <$> fzf opts input' >>= \case
    Selection Default cmd -> pure cmd
    Selection (Alternate _) cmd -> runMaybeT $ do
      cmd' <- MaybeT (pure cmd)
      resolved <- liftIO $ runSelect (fzf mempty) (resolve_command project cmd')
      edited <- fromString . T.unpack <$> MaybeT (fzf_edit_selection (Just path) resolved)
      pure $ edited { command_options = command_options cmd' }
    _ -> pure Nothing

-- | Use readline to manipulate/change a fzf selection
fzf_edit_selection :: Maybe FilePath -> Text -> IO (Maybe Text)
fzf_edit_selection path selection = runInputT settings $ do
  line <- getInputLineWithInitial "> " (T.unpack selection , "")
  pure $ case line of
    Just "" -> Nothing
    line'   -> T.pack <$> line'
  where
    settings = defaultSettings { historyFile = historyFile }
    historyFile = T.unpack . format fp . project_history_file <$> path
