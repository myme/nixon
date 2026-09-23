//! History selection and replay in the GUI worker.

use nixon::app::history::{DEFAULT_LIMIT, HistoryReadMode};
use nixon::app::{App, RunOpts};
use nixon::error::{NixonError, Result};
use nixon::process::ProcessRunner;
use nixon::project::Project;
use nixon::project::detect::inspect;
use nixon_gui::picker::GuiPicker;
use nixon_picker::{Selection, SelectionType};

use crate::cli::Commands;
use crate::gui_process::GuiProcessRunner;

use super::{
    CommandOutcome, project_detail, run_selected_command_with_args, visit_selected_command,
};

pub(super) fn pick_history<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
) -> CommandOutcome {
    let (config, candidates) =
        match app.history_candidates(Some(DEFAULT_LIMIT), HistoryReadMode::ReportErrors) {
            Err(NixonError::Io(error)) => {
                return CommandOutcome::Error(format!(
                    "Could not load history from {}: {error}",
                    app.dirs.history_file().display()
                ));
            }
            Err(NixonError::HistoryDisabled) => {
                return CommandOutcome::Error(NixonError::HistoryDisabled.to_string());
            }
            Ok(config) => config,
            Err(error) => return CommandOutcome::Error(format!("Could not load history: {error}")),
        };
    if candidates.is_empty() {
        return CommandOutcome::Canceled;
    }
    match app.pick_history_candidates(
        &config,
        candidates,
        None,
        Some("History (Enter replays; F1 or Alt-Enter shows)"),
    ) {
        Ok(Selection::Selected { kind, mut items }) if items.len() == 1 => {
            let line = items.remove(0).value;
            match kind {
                SelectionType::Show | SelectionType::Edit => CommandOutcome::Detail {
                    title: "History command".to_owned(),
                    body: line,
                },
                SelectionType::Default => replay_history_line(app, &line),
                SelectionType::Visit => CommandOutcome::Error(
                    "History entries cannot be visited in an editor.".to_owned(),
                ),
            }
        }
        Ok(Selection::Empty | Selection::Canceled) => CommandOutcome::Canceled,
        Ok(Selection::Selected { .. }) => {
            CommandOutcome::Error("Expected one history selection.".to_owned())
        }
        Err(error) => CommandOutcome::Error(format!("Could not select history: {error}")),
    }
}

fn replay_history_line<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    line: &str,
) -> CommandOutcome {
    let replay = || -> Result<crate::cli::Cli> {
        let mut words = shell_words::split(line)
            .map_err(|error| NixonError::NothingSelected(error.to_string()))?;
        if words.first().is_none_or(|word| word != "nixon") {
            return Err(NixonError::NothingSelected(
                "History entry is not a Nixon invocation.".to_owned(),
            ));
        }
        words.remove(0);
        crate::parse_history_cli(words)
    };
    let parsed = match replay() {
        Ok(parsed) => parsed,
        Err(error) => return CommandOutcome::Error(format!("Could not replay history: {error}")),
    };
    // As in the CLI's history rerun, parsed global flags do not rebuild the
    // already-running application.
    match parsed.command {
        Some(Commands::Run(args)) => {
            let project = app.current_project();
            replay_named_command(app, &project, &crate::run_opts(args))
        }
        Some(Commands::External(args)) => {
            let project = app.current_project();
            replay_named_command(app, &project, &crate::external_opts(args))
        }
        Some(Commands::Project(args)) => replay_project(app, &crate::project_opts(args)),
        Some(Commands::Eval(args)) => {
            let opts = match crate::try_eval_opts(args) {
                Ok(opts) => opts,
                Err((first, error)) => {
                    return CommandOutcome::Error(format!(
                        "Could not replay history: invalid value '{first}' for '[PLACEHOLDERS]...': {error}"
                    ));
                }
            };
            if opts.file.is_none() && opts.source.as_deref().is_none_or(str::is_empty) {
                return CommandOutcome::Error(
                    "Could not replay history: recorded eval has no source.".to_owned(),
                );
            }
            replay_eval(app, &opts)
        }
        _ => unsupported_history_action(),
    }
}

fn unsupported_history_action() -> CommandOutcome {
    CommandOutcome::Error("Could not replay history: unsupported Nixon action.".to_owned())
}

fn replay_named_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    project: &Project,
    opts: &RunOpts,
) -> CommandOutcome {
    if opts.list {
        return unsupported_history_action();
    }
    let selection = match app.choose_command(project, opts.command.as_deref()) {
        Ok(selection) => selection,
        Err(NixonError::Canceled) => return CommandOutcome::Canceled,
        Err(error) => return replay_error(error),
    };
    let (kind, command) = match selection {
        Selection::Selected { kind, mut items } if items.len() == 1 => (kind, items.remove(0)),
        Selection::Canceled => return CommandOutcome::Canceled,
        Selection::Empty => return replay_error("No command selected."),
        Selection::Selected { .. } => return replay_error("Multiple commands selected."),
    };
    if opts.insert {
        return CommandOutcome::Detail {
            title: format!("Command source: {}", command.name),
            body: command.source,
        };
    }
    if opts.select {
        return match app.select_from(project, &command) {
            Ok(values) => CommandOutcome::Detail {
                title: format!("Selected values: {}", command.name),
                body: values.join("\n"),
            },
            Err(NixonError::Canceled) => CommandOutcome::Canceled,
            Err(error) => replay_error(error),
        };
    }
    match kind {
        SelectionType::Default => {
            run_selected_command_with_args(app, project, &command, &opts.args)
        }
        SelectionType::Show => CommandOutcome::Detail {
            title: format!("Command: {}", command.name),
            body: command.source,
        },
        SelectionType::Edit => CommandOutcome::Edit {
            project: project.clone(),
            command: Box::new(command),
            args: opts.args.clone(),
        },
        SelectionType::Visit => visit_selected_command(app, project, &command),
    }
}

fn replay_project<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    opts: &nixon::app::project::ProjectOpts,
) -> CommandOutcome {
    if opts.list || opts.run.list {
        return unsupported_history_action();
    }
    let (kind, projects) =
        match app.pick_projects(opts.project.as_deref(), opts.select || opts.inspect) {
            Ok(selection) => selection,
            Err(NixonError::Canceled) => return CommandOutcome::Canceled,
            Err(error) => return replay_error(error),
        };
    if opts.select {
        return CommandOutcome::Detail {
            title: "Selected project paths".to_owned(),
            body: projects
                .iter()
                .map(|project| project.path().to_string_lossy().into_owned())
                .collect::<Vec<_>>()
                .join("\n"),
        };
    }
    if opts.inspect || kind == SelectionType::Show {
        return CommandOutcome::Detail {
            title: "Project inspection".to_owned(),
            body: inspect(&projects),
        };
    }
    let [project] = projects.as_slice() else {
        return replay_error("Multiple projects selected.");
    };
    replay_named_command(app, project, &opts.run)
}

fn replay_error(error: impl std::fmt::Display) -> CommandOutcome {
    CommandOutcome::Error(format!("Could not replay history: {error}"))
}

fn replay_eval<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    opts: &nixon::app::eval::EvalOpts,
) -> CommandOutcome {
    let project = match (&opts.project, opts.select_project) {
        (Some(path), _) => app.project_for_query_with_kind(Some(path)),
        (None, true) => app.project_for_query_with_kind(None),
        (None, false) => Ok((SelectionType::Default, app.current_project())),
    };
    let project = match project {
        Ok((SelectionType::Default, project)) => project,
        Ok((SelectionType::Show, project)) => return project_detail(&project),
        Ok(_) | Err(NixonError::Canceled) => return CommandOutcome::Canceled,
        Err(error) => return CommandOutcome::Error(format!("Could not replay history: {error}")),
    };
    let config = match app.config_for(&project) {
        Ok(config) => config,
        Err(error) => return CommandOutcome::Error(format!("Could not replay history: {error}")),
    };
    app.runner.set_configured_terminal(config.launcher.terminal);
    match app.eval_in_project(&project, opts) {
        Ok(_) => CommandOutcome::Launched,
        Err(NixonError::Canceled) => CommandOutcome::Canceled,
        Err(error) => CommandOutcome::Error(format!("Could not replay history: {error}")),
    }
}
