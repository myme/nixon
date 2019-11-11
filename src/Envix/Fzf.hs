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
  , FzfResult(..)
  ) where

import           Control.Arrow (second)
import           Data.List (sort)
import           Data.List.NonEmpty (toList)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Envix.Nix
import           Envix.Process
import           Envix.Projects
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

data FzfSelectionType = FzfDefault | FzfAlternate
data FzfResult a = FzfCancel
                 | FzfEmpty
                 | FzfSelection FzfSelectionType a

instance Functor FzfResult where
  fmap f (FzfSelection t x) = FzfSelection t (f x)
  fmap _ FzfCancel          = FzfCancel
  fmap _ FzfEmpty           = FzfEmpty

fzf :: FzfOpts -> [Text] -> IO (FzfResult Text)
fzf opts candidates = do
  let input' = concatMap (toList . textToLines) candidates
      args = case _filter opts of
        Just filter -> ["--filter", filter]
        Nothing -> "-1" : "--expect=alt-enter" : build_args
          [ flag "--border" (_border opts)
          , arg "--header" =<< _header opts
          , arg "--height" =<< format (d%"%") <$> _height opts
          , arg "--query" =<< _query opts
          , arg "--preview" =<< _preview opts
          , arg "--with-nth" =<< format_field_index <$> _with_nth opts
          ]
  (code, out) <- second T.lines <$> procStrict "fzf" args (select input')
  pure $ case code of
    ExitFailure 1 -> FzfEmpty
    ExitFailure 130 -> FzfCancel
    ExitFailure _ -> undefined
    ExitSuccess -> case out of
      ["", selection] -> FzfSelection FzfDefault selection
      ["alt-enter", selection] -> FzfSelection FzfAlternate selection
      _ -> undefined

fzf_exec :: Maybe Text -> Bool -> Project -> IO ()
fzf_exec cmd = project_exec plain with_nix
  where plain project = do
          shell <- fromMaybe "bash" <$> need "SHELL"
          let cmd' = fromMaybe shell cmd
          run [cmd'] (Just $ project_path project)
        with_nix nix_file = nix_shell nix_file cmd

fzf_format_project_name :: Project -> IO (Text, Project)
fzf_format_project_name project = do
  path <- implode_home (project_path project)
  return (format fp path, project)

-- | Find projects
fzf_projects :: Maybe Text -> [Project] -> IO (Maybe Project)
fzf_projects query projects = do
  candidates <- Map.fromList <$> traverse fzf_format_project_name projects
  let opts = fzf_header "Select project"
        <> fzf_border
        -- <> fzf_height 40
        <> maybe mempty fzf_query query
        -- <> fzf_preview "ls $(eval echo {})"
  fzf opts (sort $ Map.keys candidates) >>= \case
    FzfCancel -> return Nothing
    FzfEmpty -> return Nothing
    FzfSelection _ out -> return (Map.lookup out candidates)

project_history_file :: FilePath -> FilePath
project_history_file = (</> ".envix_history")

text_numbers :: [Text]
text_numbers = format d <$> ([0..] :: [Integer])

-- | Find commands applicable to a project
fzf_project_command :: Maybe Text -> Project -> IO (Maybe Text)
fzf_project_command query project = do
  let path = project_path project
      history_file = T.unpack $ format fp $ project_history_file path
  history <- map T.pack . historyLines <$> readHistory history_file
  let commands = zip text_numbers . sort . (history ++) $ find_project_commands project
  let opts = fzf_header "Select command"
        <> maybe mempty fzf_query query
        <> fzf_with_nth (FieldFrom 2)
  let lookup_command = (`lookup` commands) . T.takeWhile (/= ':')
      format_line (num, cmd) = format (s%": "%s) num cmd
      text_commands = format_line <$> commands
  fmap lookup_command <$> fzf opts text_commands >>= \case
    FzfSelection FzfDefault cmd -> return cmd
    FzfSelection FzfAlternate cmd -> fzf_edit_selection path (fromMaybe "" cmd)
    _ -> return Nothing

-- | Use readline to manipulate/change a fzf selection
fzf_edit_selection :: FilePath -> Text -> IO (Maybe Text)
fzf_edit_selection path selection = runInputT settings $ do
  line <- getInputLineWithInitial "> " (T.unpack selection , "")
  case line of
    Just "" -> return Nothing
    line'   -> return (T.pack <$> line')
  where
    settings :: Settings IO
    settings = defaultSettings { historyFile = Just historyFile }
    historyFile = T.unpack $ format fp $ project_history_file path
