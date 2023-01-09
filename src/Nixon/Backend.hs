module Nixon.Backend (Backend (..)) where

import Data.Text (Text)
import Nixon.Command (Command)
import Nixon.Project (Project)
import Nixon.Select (Selection (), Selector, SelectorOpts)
import Nixon.Types (Nixon)

type ProjectSelector = SelectorOpts -> Maybe Text -> [Project] -> IO (Selection Project)

type CommandSelector = Project -> Maybe Text -> [Command] -> IO (Selection Command)

data Backend = Backend
  { projectSelector :: ProjectSelector,
    commandSelector :: CommandSelector,
    selector :: Selector Nixon
  }
