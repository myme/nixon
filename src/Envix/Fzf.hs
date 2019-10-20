module Envix.Fzf
  ( fzf
  , fzf_border
  , fzf_header
  , fzf_height
  , fzf_exec
  , fzf_format_project_name
  , fzf_projects
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
import           Turtle hiding (arg, header, sort)

data FzfOpts = FzfOpts
  { _border :: Bool
  , _header :: Maybe Text
  , _height :: Maybe Integer
  , _query :: Maybe Text
  }

instance Semigroup FzfOpts where
  left <> right = FzfOpts { _border = _border right || _border left
                          , _header = _header right <|> _header left
                          , _height = _height right <|> _height left
                          , _query = _query right <|> _query left
                          }

instance Monoid FzfOpts where
  mempty = FzfOpts { _border = False
                   , _header = Nothing
                   , _height = Nothing
                   , _query = Nothing
                   }

fzf_border :: FzfOpts
fzf_border = mempty { _border = True }

fzf_header :: Text -> FzfOpts
fzf_header header = mempty { _header = Just header }

fzf_height :: Integer -> FzfOpts
fzf_height height = mempty { _height = Just height }

fzf_query :: Text -> FzfOpts
fzf_query query = mempty { _query = Just query }

data FzfResult = FzfCancel
               | FzfEmpty
               | FzfDefault Text

fzf :: FzfOpts -> [Text] -> IO FzfResult
fzf opts candidates = do
  let input' = concatMap (toList . textToLines) candidates
      args = build_args
        [ flag "--border" (_border opts)
        , arg "--header" =<< _header opts
        , arg "--height" =<< format (d%"%") <$> _height opts
        , arg "--query" =<< _query opts
        ]
  (code, out) <- second T.strip <$> procStrict "fzf" args (select input')
  pure $ case code of
    ExitSuccess -> FzfDefault out
    ExitFailure 1 -> FzfEmpty
    ExitFailure 130 -> FzfCancel
    ExitFailure _ -> undefined

fzf_exec :: Maybe Text -> Bool -> Project -> IO ()
fzf_exec command = project_exec plain with_nix
  where plain project = run bash_command [] (Just $ project_path project)
        with_nix nix_file = nix_shell nix_file command
        bash_command = fromMaybe "bash" command

fzf_format_project_name :: Project -> IO (Text, Project)
fzf_format_project_name project = do
  project' <- implode_home project
  let
    dir = project_dir project'
    name = project_name project'
    path = format fp (dir </> name)
  return (path, project)

fzf_projects :: Maybe Text -> Commands -> [Project] -> IO (Maybe Selection)
fzf_projects query _ projects = do
  candidates <- Map.fromList <$> traverse fzf_format_project_name projects
  let opts = fzf_header "Select project"
        <> fzf_border
        -- <> fzf_height 40
        <> maybe mempty fzf_query query
  fzf opts (sort $ Map.keys candidates) >>= \case
    FzfCancel -> return Nothing
    FzfEmpty -> return Nothing
    FzfDefault out -> return ((, Nothing) <$> Map.lookup out candidates)
