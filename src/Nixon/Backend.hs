module Nixon.Backend (Backend (..)) where

import Data.Text (Text)
import Nixon.Command (Command)
import Nixon.Config.Options (RunOpts)
import Nixon.Project (Project)
import Nixon.Select (Selector)
import Nixon.Types (Nixon)

type ProjectSelector = Maybe Text -> [Project] -> IO (Maybe Project)

type CommandSelector = Project -> RunOpts -> [Command] -> IO (Maybe Command)

data Backend = Backend
  { projectSelector :: ProjectSelector,
    commandSelector :: CommandSelector,
    selector :: Selector Nixon
  }
