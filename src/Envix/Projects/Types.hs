module Envix.Projects.Types
  ( ProjectType (..)
  , find_markers
  , proj
  , project_types
  , test_marker
  ) where

import           Control.Monad (filterM)
import qualified Data.Text as T
import           Envix.Nix
import           Envix.Process
import           Prelude hiding (FilePath)
import           Turtle

-- TODO: Parse e.g. package.json for npm scripts?
-- TODO: Add associated action with each project type
-- e.g. for *.nix invoke nix-shell
--      for .git do a fetch?
--      This should be configurable.
-- This can then be paired up with a `--type <type>` cli arg to allow override
-- which action to run. This can obsolete `--no-nix` with `--type plain`.
-- TODO: Add support for local overrides with an .envix project file
-- TODO: Record commands made within a project and add to list
-- TODO: List descriptions
-- TODO: Add command type: data CmdType = Terminal | GUI
project_types :: [ProjectType]
project_types =
  [proj ["cabal.project"] "Cabal new-style project"
   [command "cabal" ["new-build"] "build"
   ,command "cabal" ["new-repl"] "repl"
   ,command "cabal" ["new-run"] "run"
   ,command "cabal" ["new-test"] "test"
   ]
  ,proj ["package.json"] "NPM project"
   [command "npm" ["install"] "install"
   ,command "npm" ["start"] "run"
   ,command "npm" ["test"] "test"
   ]
  ,proj (map ProjectPath nix_files) "Nix project"
   [command "nix-build" [] "build"
   ,command "nix-shell" [] "shell"
   ]
  ,proj [".envrc"] "Direnv project"
   [command "direnv" ["allow"] "direnv allow"
   ,command "direnv" ["deny"] "direnv deny"
   ,command "direnv" ["reload"] "direnv reload"
   ]
  ,proj [".git"] "Git repository"
   [command "git" ["fetch"] "Git fetch"
   ,command "git" ["log"] "Git log"
   ,command "git" ["rebase"] "Git rebase"
   ,command "git" ["status"] "Git status"
   ]
  ,proj [".hg"] "Mercurial project" []
  ,proj [".project"] "Ad-hoc project" []
  ,proj [ProjectFunc . const $ pure True] "Generic project"
   [command "x-terminal-emulator" [] "Terminal"
   ,command "emacs" [] "Emacs"
   ,command "vim" [] "Vim"
   ,command "dolphin" [ArgPath] "Files"
   ,command "rofi" ["-show", "run"] "Run"
   ]
  ]

data ProjectType = ProjectType { project_markers :: [ProjectMarker]
                               , project_description :: Text
                               , project_commands :: [CmdDesc]
                               }

proj :: [ProjectMarker] -> Text -> [CmdDesc] -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath
                   | ProjectFile FilePath
                   | ProjectDir FilePath
                   | ProjectFunc (FilePath -> IO Bool)

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectPath path) = "ProjectPath" ++ show path
  show (ProjectFile path) = "ProjectFile" ++ show path
  show (ProjectDir path)  = "ProjectDir"  ++ show path

-- | Test that a marker is valid for a path
test_marker :: ProjectMarker -> FilePath -> IO Bool
test_marker (ProjectPath marker) path = testpath (path </> marker)
test_marker (ProjectFile marker) path = testfile (path </> marker)
test_marker (ProjectDir  marker) path = testdir (path </> marker)
test_marker (ProjectFunc marker) path = marker path

-- | Given a path, find markers and associated commands.
find_markers :: FilePath -> IO [CmdDesc]
find_markers path = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return []
    else concatMap project_commands <$> filterM has_markers project_types
  where has_markers = fmap and . traverse (`test_marker` path) . project_markers
