module Nixon.Backend.Rofi (rofiBackend) where

import Data.Maybe (catMaybes)
import Nixon.Backend (Backend (..))
import qualified Nixon.Config.Types as Config
import Nixon.Config.Types (Config)
import Nixon.Rofi (rofi, rofi_exact, rofi_ignore_case, rofi_project_command, rofi_projects, rofi_prompt, rofi_query)
import qualified Nixon.Select as Select

rofiBackend :: Config -> Backend
rofiBackend cfg =
  let rofi_opts opts =
        mconcat $
          catMaybes
            [ rofi_exact <$> Config.exact_match cfg,
              rofi_ignore_case <$> Config.ignore_case cfg,
              rofi_query <$> Select.selector_search opts,
              rofi_prompt <$> Select.selector_title opts
            ]
      rofi_opts' = rofi_opts Select.defaults
   in Backend
        { projectSelector = rofi_projects rofi_opts',
          commandSelector = const $ rofi_project_command rofi_opts',
          selectionEdit = rofi . rofi_opts
        }
