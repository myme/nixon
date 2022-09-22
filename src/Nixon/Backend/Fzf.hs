module Nixon.Backend.Fzf (fzfBackend) where

import Data.Maybe (catMaybes)
import Nixon.Backend (Backend (..))
import Nixon.Config.Types (Config)
import qualified Nixon.Config.Types as Config
import Nixon.Fzf (fzf_exact, fzf_header, fzf_ignore_case, fzf_query, fzf_projects, fzf_project_command, fzf_with_edit)
import qualified Nixon.Select as Select

fzfBackend :: Config -> Backend
fzfBackend cfg =
  let fzf_opts opts =
        mconcat $
          catMaybes
            [ fzf_exact <$> Config.exact_match cfg,
              fzf_ignore_case <$> Config.ignore_case cfg,
              fzf_query <$> Select.selector_search opts,
              fzf_header <$> Select.selector_title opts
            ]
      fzf_opts' = fzf_opts Select.defaults
   in Backend
        { projectSelector = fzf_projects fzf_opts',
          commandSelector = fzf_project_command fzf_opts',
          selectionEdit = fzf_with_edit . fzf_opts
        }
