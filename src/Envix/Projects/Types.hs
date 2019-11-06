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
project_types :: [ProjectType]
project_types =
  [proj ["cabal.project"] "Cabal new-style project"
   [term "cabal" ["new-build"] "build"
   ,term "cabal" ["new-repl"] "repl"
   ,term "cabal" ["new-run"] "run"
   ,term "cabal" ["new-test"] "test"
   ]
  ,proj ["package.json"] "NPM project"
   [term "npm" ["install"] "install"
   ,term "npm" ["start"] "run"
   ,term "npm" ["test"] "test"
   ]
  ,proj (map ProjectPath nix_files) "Nix project"
   [term "nix-build" [] "build"
   ,term "nix-shell" [] "shell"
   ]
  ,proj [".envrc"] "Direnv project"
   [term "direnv" ["allow"] "direnv allow"
   ,term "direnv" ["deny"] "direnv deny"
   ,term "direnv" ["reload"] "direnv reload"
   ]
  ,proj [".git"] "Git repository"
   [term "git" ["fetch"] "Git fetch"
   ,term "git" ["log"] "Git log"
   ,term "git" ["rebase"] "Git rebase"
   ,term "git" ["status"] "Git status"
   ]
  ,proj [".hg"] "Mercurial project" []
  ,proj [".project"] "Ad-hoc project" []
  ,proj [ProjectFunc . const $ pure True] "Generic project"
   [gui "x-terminal-emulator" [] "Terminal"
   ,gui "emacs" [] "Emacs"
   ,gui "vim" [] "Vim"
   ,gui "dolphin" [ArgPath] "Files"
   ,gui "rofi" ["-show", "run"] "Run"
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
