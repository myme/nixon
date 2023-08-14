module Nixon.Backend (Backend (..)) where

import Nixon.Command (Command)
import Nixon.Prelude
import Nixon.Project (Project)
import Nixon.Select (Selection (), Selector, SelectorOpts)

type ProjectSelector m = SelectorOpts -> Maybe Text -> [Project] -> m (Selection Project)

type CommandSelector m = Project -> Maybe Text -> [Command] -> m (Selection Command)

data Backend m = Backend
  { projectSelector :: ProjectSelector m,
    commandSelector :: CommandSelector m,
    selector :: Selector m
  }
