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
  , FzfResult(..)
  ) where

import           Control.Arrow (second)
import           Control.Exception (bracket)
import           Data.List (sort)
import           Data.List.NonEmpty (toList)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Envix.Nix
import           Envix.Process
import           Envix.Projects
import           Prelude hiding (FilePath, filter)
import           System.Console.Readline
import           Turtle hiding (arg, header, readline, sort, shell)

data FzfOpts = FzfOpts
  { _border :: Bool
  , _header :: Maybe Text
  , _height :: Maybe Integer
  , _query :: Maybe Text
  , _filter :: Maybe Text
  , _preview :: Maybe Text
  }

instance Semigroup FzfOpts where
  left <> right = FzfOpts { _border = _border right || _border left
                          , _header = _header right <|> _header left
                          , _height = _height right <|> _height left
                          , _query = _query right <|> _query left
                          , _filter = _filter right <|> _filter left
                          , _preview = _preview right <|> _preview left
                          }

instance Monoid FzfOpts where
  mempty = FzfOpts { _border = False
                   , _header = Nothing
                   , _height = Nothing
                   , _query = Nothing
                   , _filter = Nothing
                   , _preview = Nothing
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

data FzfSelectionType = FzfDefault | FzfAlternate
data FzfResult = FzfCancel
               | FzfEmpty
               | FzfSelection FzfSelectionType Text

fzf :: FzfOpts -> [Text] -> IO FzfResult
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

fzf_exec :: Maybe Command -> Bool -> Project -> IO ()
fzf_exec cmd = project_exec plain with_nix
  where plain project = do
          shell <- from_text . fromMaybe "bash" <$> need "SHELL"
          let cmd' = fromMaybe shell cmd
          run cmd' (Just $ project_path project)
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

-- | Find commands applicable to a project
fzf_project_command :: Maybe Text -> FilePath -> IO (Maybe Command)
fzf_project_command query path = do
  commands <- find_project_commands path
  let opts = fzf_header "Select command"
        <> maybe mempty fzf_query query
  fzf opts (to_text <$> commands) >>= \case
    FzfSelection FzfDefault cmd -> return $ Just (from_text cmd)
    FzfSelection FzfAlternate cmd -> fzf_edit_selection cmd
    _ -> return Nothing

-- | Use readline to manipulate/change a fzf selection
-- TODO: Add readline history
fzf_edit_selection :: Text -> IO (Maybe Command)
fzf_edit_selection selection = bracket setup teardown read_input
  where setup = setPreInputHook (Just fill_input)
        teardown _ = setPreInputHook Nothing
        fill_input = insertText (T.unpack selection) >> redisplay
        read_input _ = readline "> " >>= \case
          Just "" -> return Nothing
          line    -> return $ fmap (from_text . T.pack) line
