module Nixon.Command.Run
  ( resolveCmd,
    resolveEnv,
    runCmd,
  )
where

import Control.Monad (foldM)
import Data.Aeson (eitherDecodeStrict)
import Data.Foldable (find)
import qualified Data.Text as T
import Nixon.Command (Command, CommandOutput (..))
import qualified Nixon.Command as Cmd
import Nixon.Command.Find (findProjectCommands)
import qualified Nixon.Command.Placeholder as Cmd
import Nixon.Evaluator (evaluate, getEvaluator)
import Nixon.Prelude
import Nixon.Process (run_with_output)
import qualified Nixon.Process
import Nixon.Project (Project)
import qualified Nixon.Project as Project
import Nixon.Select (Selection (..), Selector, selector_fields, selector_multiple)
import qualified Nixon.Select as Select
import Nixon.Types (Nixon)
import Nixon.Utils (toLines)
import Turtle (Shell, cd, format, fp, select, stream)
import qualified Turtle.Bytes as BS
import Turtle.Line (lineToText)

-- | Actually run a command
runCmd :: Selector Nixon -> Project -> Command -> [Text] -> Nixon ()
runCmd selector project cmd args = do
  let projectPath = Project.project_path project
      project_selector select_opts shell' =
        cd projectPath
          >> selector (select_opts <> Select.title (Cmd.show_command cmd)) shell'
  (stdin, args', env') <- resolveEnv project project_selector cmd args
  let pwd = Cmd.cmdPwd cmd <|> Just projectPath
  evaluate cmd args' pwd env' (toLines <$> stdin)

-- | Resolve all command placeholders to either stdin input, positional arguments or env vars.
resolveEnv :: Project -> Selector Nixon -> Command -> [Text] -> Nixon (Maybe (Shell Text), [Text], Nixon.Process.Env)
resolveEnv project selector cmd args = do
  let mappedArgs = zipArgs (Cmd.cmdPlaceholders cmd) args
  (stdin, args', envs) <- resolveEnv' project selector mappedArgs
  pure (stdin, args', nixonEnvs ++ envs)
  where
    nixonEnvs = [("nixon_project_path", format fp (Project.project_path project))]

-- | Zip placeholders with arguments, filling in missing placeholders with overflow arguments.
zipArgs :: [Cmd.Placeholder] -> [Text] -> [(Cmd.Placeholder, Select.SelectorOpts)]
zipArgs [] args' = zip (map argOverflow args') (repeat Select.defaults)
  where
    argOverflow = Cmd.Placeholder Cmd.Arg "arg" [] False . pure
zipArgs placeholders [] = zip placeholders (repeat Select.defaults)
zipArgs (p : ps) (a : as) = (p, Select.search a) : zipArgs ps as

-- | Resolve all command placeholders to either stdin input, positional arguments or env vars.
resolveEnv' :: Project -> Selector Nixon -> [(Cmd.Placeholder, Select.SelectorOpts)] -> Nixon (Maybe (Shell Text), [Text], Nixon.Process.Env)
resolveEnv' project selector = foldM resolveEach (Nothing, [], [])
  where
    resolveEach (stdin, args', envs) (Cmd.Placeholder envType cmdName fields multiple value, select_opts) = do
      resolved <- case value of
        [] -> do
          cmd' <- assertCommand cmdName
          let select_opts' =
                select_opts
                  { selector_fields = fields,
                    selector_multiple = Just multiple
                  }
          resolveCmd project selector cmd' select_opts'
        _ -> pure value
      case envType of
        -- Standard inputs are concatenated
        Cmd.Stdin ->
          let stdinCombined = Just $ case stdin of
                Nothing -> select resolved
                Just prev -> prev <|> select resolved
           in pure (stdinCombined, args', envs)
        -- Each line counts as one positional argument
        Cmd.Arg -> pure (stdin, args' <> resolved, envs)
        -- Environment variables are concatenated into space-separated line
        Cmd.EnvVar name -> pure (stdin, args', envs <> [(name, T.unwords resolved)])

    assertCommand cmd_name = do
      cmd' <- find ((==) cmd_name . Cmd.cmdName) <$> findProjectCommands project
      maybe (error $ "Invalid argument: " <> T.unpack cmd_name) pure cmd'

-- | Resolve command to selectable output.
resolveCmd :: Project -> Selector Nixon -> Command -> Select.SelectorOpts -> Nixon [Text]
resolveCmd project selector cmd select_opts = do
  (stdin, args, env') <- resolveEnv project selector cmd []
  let projectPath = Just (Project.project_path project)
  linesEval <- getEvaluator (run_with_output stream) cmd args projectPath env' (toLines <$> stdin)
  jsonEval <- getEvaluator (run_with_output BS.stream) cmd args projectPath env' (BS.fromUTF8 <$> stdin)
  selection <- selector select_opts $ do
    case Cmd.cmdOutput cmd of
      Lines -> Select.Identity . lineToText <$> linesEval
      JSON -> do
        output <- BS.strict jsonEval
        case eitherDecodeStrict output :: Either String [Select.Candidate] of
          Left err -> error err
          Right candidates -> select candidates
  case selection of
    Selection _ result -> pure result
    _ -> error "Argument expansion aborted"
