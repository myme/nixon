module Nixon.Command.Run
  ( resolveCmd,
    resolveEnv,
    runCmd,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (eitherDecodeStrict)
import Data.Foldable (find)
import Data.Text (Text)
import qualified Data.Text as T
import Nixon.Command (Command, CommandOutput (..))
import qualified Nixon.Command as Cmd
import qualified Nixon.Command.Placeholder as Cmd
import Nixon.Evaluator (evaluate, getEvaluator)
import Nixon.Process (run_with_output)
import qualified Nixon.Process
import Nixon.Select (Selection (..), Selector, SelectorOpts (..))
import qualified Nixon.Select as Select
import Nixon.Types (Nixon)
import qualified Nixon.Types as Types
import Nixon.Utils (toLines)
import Turtle (FilePath, Shell, cd, format, fp, select, stream)
import qualified Turtle.Bytes as BS
import Turtle.Line (lineToText)
import Prelude hiding (FilePath)

-- | Actually run a command
runCmd :: Selector Nixon -> FilePath -> Command -> [Text] -> Nixon ()
runCmd selector path cmd args = do
  let project_selector select_opts shell' =
        cd path
          >> selector (select_opts <> Select.title (Cmd.show_command cmd)) shell'
  (stdin, args', env') <- resolveEnv path project_selector cmd args
  evaluate cmd args' (Just path) env' (toLines <$> stdin)

-- | Resolve all command placeholders to either stdin input, positional arguments or env vars.
resolveEnv :: FilePath -> Selector Nixon -> Command -> [Text] -> Nixon (Maybe (Shell Text), [Text], Nixon.Process.Env)
resolveEnv path selector cmd args = do
  let mappedArgs = zip (Cmd.cmdEnv cmd) (map Select.search args <> repeat Select.defaults)
  (stdin, args', envs) <- foldM resolveEach (Nothing, [], []) mappedArgs
  pure (stdin, args', nixonEnvs ++ envs)
  where
    nixonEnvs = [("nixon_project_path", format fp path)]

    resolveEach (stdin, args', envs) ((name, Cmd.Placeholder envType cmdName multiple), select_opts) = do
      cmd' <- assertCommand cmdName
      let select_opts' = select_opts {selector_multiple = Just multiple}
      resolved <- resolveCmd path selector cmd' select_opts'
      pure $ case envType of
        -- Standard inputs are concatenated
        Cmd.Stdin ->
          let stdinCombined = Just $ case stdin of
                Nothing -> select resolved
                Just prev -> prev <|> select resolved
           in (stdinCombined, args', envs)
        -- Each line counts as one positional argument
        Cmd.Arg -> (stdin, args' <> resolved, envs)
        -- Environment variables are concatenated into space-separated line
        Cmd.EnvVar -> (stdin, args', envs <> [(name, T.unwords resolved)])

    assertCommand cmd_name = do
      cmd' <- find ((==) cmd_name . Cmd.cmdName) . Types.commands . Types.config <$> ask
      maybe (error $ "Invalid argument: " <> T.unpack cmd_name) pure cmd'

-- | Resolve command to selectable output.
resolveCmd :: FilePath -> Selector Nixon -> Command -> Select.SelectorOpts -> Nixon [Text]
resolveCmd path selector cmd select_opts = do
  (stdin, args, env') <- resolveEnv path selector cmd []
  linesEval <- getEvaluator (run_with_output stream) cmd args (Just path) env' (toLines <$> stdin)
  jsonEval <- getEvaluator (run_with_output BS.stream) cmd args (Just path) env' (BS.fromUTF8 <$> stdin)
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
