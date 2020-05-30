module Nixon.Project.Types
  ( Part (..)
  , Project (..)
  , ProjectType (..)
  , ProjectMarker (..)
  , Command (..)
  , CommandOptions (..)
  , (!)
  , desc
  , dir
  , from_path
  , gui
  , path
  , proj
  , project_path
  , shell
  , show_command
  ) where

import           Data.List (intercalate)
import qualified Data.Text as T
import           Nixon.Select
import           Prelude hiding (FilePath)
import           Turtle hiding (d, f, g, shell)

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_types :: [ProjectType]
                       } deriving Show

from_path :: FilePath -> Project
from_path path' = Project { project_name = filename path'
                          , project_dir = parent path'
                          , project_types = []
                          }

-- | Full path to a project
project_path :: Project -> FilePath
project_path project = project_dir project </> project_name project

data ProjectType = ProjectType { project_markers :: [ProjectMarker]
                               , project_description :: Text
                               , project_commands :: [Command]
                               } deriving Show

-- | Construct a project description
proj :: [ProjectMarker] -> Text -> [Command] -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath -- ^ Check if path exists
                   | ProjectFile FilePath -- ^ Check if path is a file
                   | ProjectDir FilePath -- ^ Check if path is a directory
                   | ProjectOr [ProjectMarker] -- ^ Logical `or` two marker checks
                   | ProjectFunc (FilePath -> IO Bool) -- ^ Run arbitrary check on candidate dir

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectOr ms)  = "ProjectOr (" ++ intercalate ", " (map show ms) ++ ")"
  show (ProjectPath p) = "ProjectPath" ++ show p
  show (ProjectFile p) = "ProjectFile" ++ show p
  show (ProjectDir p)  = "ProjectDir"  ++ show p

data Part = TextPart Text
          | PathPart
          | DirPart
          | ShellPart Text (Project -> Select (Selection Text))
          | NestedPart [Part]

instance IsString Part where
  fromString = TextPart . T.pack

instance Show Part where
  show (TextPart t) = T.unpack t
  show PathPart = "<project>"
  show DirPart = "<dirname>"
  show (ShellPart placeholder _) = T.unpack $ format ("<"%s%">") placeholder
  show (NestedPart parts) = unwords (map show parts)

data Command = Command
             { command_parts :: [Part]
             , command_options :: CommandOptions
             } deriving Show

show_command :: Command -> Text
show_command (Command parts opts) = T.unwords (map (T.pack . show) parts) <> description
  where description = if not $ T.null (command_desc opts)
          then format (" ("%s%")") (command_desc opts)
          else ""

instance IsString Command where
  fromString ss = Command (map TextPart $ T.words $ T.pack ss) mempty

instance Semigroup Command where
  (Command a ao) <> (Command b bo) = Command (a <> b) (ao <> bo)

-- | Placeholder for project path
path :: Command
path = Command [PathPart] mempty

-- | Placeholder for project directory
dir :: Command
dir = Command [DirPart] mempty

-- | Placeholder for a shell command
shell :: Text -> (Project -> Select (Selection Text)) -> Command
shell placeholder action = Command [ShellPart placeholder action] mempty

data CommandOptions = CommandOptions
                    { command_desc :: Text
                    , command_gui :: Bool
                    } deriving Show

instance Semigroup CommandOptions where
  (CommandOptions d g) <> (CommandOptions d' g') = CommandOptions (d <> d') (g || g')

instance Monoid CommandOptions where
  mempty = CommandOptions "" False

-- | Add command description
desc :: Text -> CommandOptions
desc d = CommandOptions d False

-- | Tag command as a GUI command
gui :: CommandOptions
gui = CommandOptions "" True

-- | Add options to commands
(!) :: Command -> CommandOptions -> Command
(!) (Command parts opts) opts' = Command parts (opts <> opts')
infixr 4 !
