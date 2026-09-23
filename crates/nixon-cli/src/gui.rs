//! The graphical launcher in the existing `nixon` binary.

mod history;

use std::sync::mpsc::{self, Receiver, Sender};
use std::thread::{self, JoinHandle};

use eframe::egui;
use nixon::app::{App, Environment};
use nixon::command::Command;
use nixon::config::launcher::{DEFAULT_SEARCH_URL, LauncherAction, LauncherConfig, MprisOperation};
use nixon::config::{Config, ConfigError, load};
use nixon::error::{NixonError, Result};
use nixon::fs::Dirs;
use nixon::process::{Invocation, ProcessRunner, RealRunner};
use nixon::project::Project;
use nixon::project::detect::inspect;
use nixon::select;
use nixon_gui::browser::target_url;
use nixon_gui::picker::GuiPicker;
use nixon_gui::window::{BrowserInputEvent, EditEvent, MenuWindow, native_options};
use nixon_picker::{Picker, Selection, SelectionType};

use crate::cli::Commands;
use crate::gui_process::GuiProcessRunner;

/// The GUI only opens the root menu. No command is silently ignored.
pub fn reject_subcommand(command: Option<&Commands>) -> Result<()> {
    if command.is_some() {
        return Err(NixonError::NothingSelected(
            "GUI preview does not support subcommands; run `nixon --mode gui` without a subcommand."
                .to_owned(),
        ));
    }
    Ok(())
}

/// Opens the configured menu in the process's native GUI event loop.
pub fn run(config: Config, dirs: Dirs, env: Environment) -> Result<i32> {
    let app = PreviewApp::new(config, dirs, env)?;
    #[cfg(target_os = "linux")]
    if ["DISPLAY", "WAYLAND_DISPLAY", "WAYLAND_SOCKET"]
        .iter()
        .all(|name| std::env::var_os(name).is_none_or(|value| value.is_empty()))
    {
        return Err(NixonError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "No X11 or Wayland display is available; start a graphical session or set DISPLAY or WAYLAND_DISPLAY.",
        )));
    }
    eframe::run_native("Nixon", native_options(), Box::new(|_| Ok(Box::new(app))))
        .map_err(|err| NixonError::Io(std::io::Error::other(err.to_string())))?;
    Ok(0)
}

struct PreviewApp {
    menu: MenuWindow,
    actions: Receiver<LauncherAction>,
    command_requests: Sender<CommandRequest>,
    command_results: Receiver<CommandOutcome>,
    _worker: JoinHandle<()>,
    browser_requests: Sender<String>,
    browser_results: Receiver<BrowserResult>,
    _browser_worker: JoinHandle<()>,
    media_requests: Sender<MediaRequest>,
    media_results: Receiver<MediaResult>,
    _media_worker: JoinHandle<()>,
    busy: bool,
    browser_busy: bool,
    media_busy: bool,
    pending_edit: Option<(Project, Box<Command>, Vec<String>)>,
    status: String,
}

type BrowserResult = std::result::Result<bool, String>;
type MediaRequest = (MprisOperation, String);
type MediaResult = std::result::Result<(), String>;
type MediaWorker = (Sender<MediaRequest>, Receiver<MediaResult>, JoinHandle<()>);
const PREVIEW_STATUS: &str = "GUI preview: choose an action.";

enum CommandOutcome {
    Launched,
    Edit {
        project: Project,
        command: Box<Command>,
        args: Vec<String>,
    },
    Detail {
        title: String,
        body: String,
    },
    Empty,
    Canceled,
    Error(String),
}

enum CommandRequest {
    PickCurrent,
    PickProject,
    PickHistory,
    Quick {
        name: String,
        project: Option<String>,
    },
    RunEdited {
        project: Project,
        command: Box<Command>,
        source: String,
        args: Vec<String>,
    },
}

impl PreviewApp {
    fn new(config: Config, dirs: Dirs, env: Environment) -> Result<Self> {
        Self::with_workers(config, dirs, env, RealRunner, SessionMediaTransport)
    }

    #[cfg(test)]
    fn with_browser_runner<R: ProcessRunner + Send + 'static>(
        config: Config,
        dirs: Dirs,
        env: Environment,
        browser_runner: R,
    ) -> Result<Self> {
        Self::with_workers(config, dirs, env, browser_runner, SessionMediaTransport)
    }

    fn with_workers<R: ProcessRunner + Send + 'static, M: MediaTransport>(
        config: Config,
        dirs: Dirs,
        env: Environment,
        browser_runner: R,
        media_transport: M,
    ) -> Result<Self> {
        let runner = GuiProcessRunner::from_environment(
            RealRunner,
            config.launcher.terminal.clone(),
            env.exe.clone().unwrap_or_default(),
        );
        Self::with_command_runner(config, dirs, env, browser_runner, media_transport, runner)
    }

    fn with_command_runner<
        R: ProcessRunner + Send + 'static,
        M: MediaTransport,
        C: ProcessRunner + Send + 'static,
    >(
        config: Config,
        dirs: Dirs,
        env: Environment,
        browser_runner: R,
        media_transport: M,
        command_runner: GuiProcessRunner<C>,
    ) -> Result<Self> {
        let (picker, requests) = GuiPicker::channel();
        let mut app = App::new(config, dirs, env, picker, command_runner);
        let launcher = startup_launcher(&app)?;
        app.runner
            .set_configured_terminal(launcher.terminal.clone());
        let (sender, actions) = mpsc::channel();
        let mut menu = MenuWindow::new(&launcher, sender).ok_or_else(|| {
            NixonError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "GUI launcher menu has no items",
            ))
        })?;
        menu.attach_picker(requests);
        let search_url = launcher
            .search_url
            .as_deref()
            .unwrap_or(DEFAULT_SEARCH_URL)
            .to_owned();
        let (command_requests, worker_requests) = mpsc::channel();
        let (worker_results, command_results) = mpsc::channel();
        let worker = thread::spawn(move || {
            while let Ok(request) = worker_requests.recv() {
                let outcome = match request {
                    CommandRequest::PickCurrent => pick_current_command(&mut app),
                    CommandRequest::PickProject => pick_project_command(&mut app),
                    CommandRequest::PickHistory => history::pick_history(&mut app),
                    CommandRequest::Quick { name, project } => {
                        launch_quick_command(&mut app, &name, project.as_deref())
                    }
                    CommandRequest::RunEdited {
                        project,
                        command,
                        source,
                        args,
                    } => run_edited_command(&mut app, &project, *command, &source, &args),
                };
                if worker_results.send(outcome).is_err() {
                    break;
                }
            }
        });
        let (browser_requests, browser_results, browser_worker) =
            start_browser_worker(browser_runner, search_url);
        let (media_requests, media_results, media_worker) = start_media_worker(media_transport);
        Ok(Self {
            menu,
            actions,
            command_requests,
            command_results,
            _worker: worker,
            browser_requests,
            browser_results,
            _browser_worker: browser_worker,
            media_requests,
            media_results,
            _media_worker: media_worker,
            busy: false,
            browser_busy: false,
            media_busy: false,
            pending_edit: None,
            status: PREVIEW_STATUS.to_owned(),
        })
    }

    fn show(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::bottom("preview_status").show(ctx, |ui| {
            ui.label(&self.status);
        });
        self.menu.show(ctx);
        if self.menu.take_detail_closed() {
            PREVIEW_STATUS.clone_into(&mut self.status);
        }
        while let Ok(action) = self.actions.try_recv() {
            self.handle_action(action);
            ctx.request_repaint();
        }
        if let Some(event) = self.menu.take_browser_event() {
            match event {
                BrowserInputEvent::Canceled => {
                    PREVIEW_STATUS.clone_into(&mut self.status);
                }
                BrowserInputEvent::Submitted(input) => {
                    if self.browser_requests.send(input).is_ok() {
                        self.browser_busy = true;
                        "Opening browser…".clone_into(&mut self.status);
                    } else {
                        "Browser opener is unavailable.".clone_into(&mut self.status);
                        tracing::error!("{}", self.status);
                    }
                }
            }
            ctx.request_repaint();
        }
        if let Some(event) = self.menu.take_edit_event() {
            self.handle_edit_event(event);
            ctx.request_repaint();
        }
        for outcome in self.command_results.try_iter() {
            self.busy = false;
            match outcome {
                CommandOutcome::Launched => ctx.send_viewport_cmd(egui::ViewportCommand::Close),
                CommandOutcome::Edit {
                    project,
                    command,
                    args,
                } => {
                    self.status = format!("Editing {}.", command.name);
                    self.menu.open_edit(
                        format!("Edit command: {}", command.name),
                        command.source.clone(),
                    );
                    self.pending_edit = Some((project, command, args));
                }
                CommandOutcome::Detail { title, body } => {
                    self.status = format!("Showing {title}.");
                    self.menu.open_detail(title, body);
                }
                CommandOutcome::Empty => "No commands available.".clone_into(&mut self.status),
                CommandOutcome::Canceled => PREVIEW_STATUS.clone_into(&mut self.status),
                CommandOutcome::Error(error) => {
                    tracing::error!("{error}");
                    self.status = error;
                }
            }
            ctx.request_repaint();
        }
        for result in self.browser_results.try_iter() {
            self.browser_busy = false;
            match result {
                Ok(true) => ctx.send_viewport_cmd(egui::ViewportCommand::Close),
                Ok(false) => {
                    PREVIEW_STATUS.clone_into(&mut self.status);
                }
                Err(message) => {
                    tracing::error!("{message}");
                    self.status = message;
                }
            }
            ctx.request_repaint();
        }
        for result in self.media_results.try_iter() {
            self.media_busy = false;
            match result {
                Ok(()) => ctx.send_viewport_cmd(egui::ViewportCommand::Close),
                Err(message) => {
                    tracing::error!("{message}");
                    self.status = message;
                }
            }
            ctx.request_repaint();
        }
        if self.busy || self.browser_busy || self.media_busy {
            ctx.request_repaint_after(std::time::Duration::from_millis(30));
        }
    }

    fn handle_action(&mut self, action: LauncherAction) {
        match action {
            LauncherAction::Commands => {
                if !self.busy
                    && !self.browser_busy
                    && !self.media_busy
                    && self
                        .command_requests
                        .send(CommandRequest::PickCurrent)
                        .is_ok()
                {
                    self.busy = true;
                    "Loading commands…".clone_into(&mut self.status);
                }
            }
            LauncherAction::Projects => {
                if !self.busy && !self.browser_busy && !self.media_busy {
                    if self
                        .command_requests
                        .send(CommandRequest::PickProject)
                        .is_ok()
                    {
                        self.busy = true;
                        "Loading projects…".clone_into(&mut self.status);
                    } else {
                        "Command worker is unavailable.".clone_into(&mut self.status);
                        tracing::error!("{}", self.status);
                    }
                }
            }
            LauncherAction::BrowserInput => {
                if !self.busy && !self.browser_busy && !self.media_busy {
                    self.menu.open_browser_input();
                    "Enter a URL or search terms.".clone_into(&mut self.status);
                }
            }
            LauncherAction::Mpris { operation, player } => {
                if !self.busy && !self.browser_busy && !self.media_busy {
                    if self.media_requests.send((operation, player)).is_ok() {
                        self.media_busy = true;
                        "Controlling media…".clone_into(&mut self.status);
                    } else {
                        "Media controller is unavailable.".clone_into(&mut self.status);
                        tracing::error!("{}", self.status);
                    }
                }
            }
            LauncherAction::Command { name, project } => {
                if !self.busy && !self.browser_busy && !self.media_busy {
                    if self
                        .command_requests
                        .send(CommandRequest::Quick {
                            name: name.clone(),
                            project,
                        })
                        .is_ok()
                    {
                        self.busy = true;
                        self.status = format!("Launching {name}…");
                    } else {
                        "Command worker is unavailable.".clone_into(&mut self.status);
                        tracing::error!("{}", self.status);
                    }
                }
            }
            LauncherAction::History => {
                if !self.busy && !self.browser_busy && !self.media_busy {
                    if self
                        .command_requests
                        .send(CommandRequest::PickHistory)
                        .is_ok()
                    {
                        self.busy = true;
                        "Loading history…".clone_into(&mut self.status);
                    } else {
                        "Command worker is unavailable.".clone_into(&mut self.status);
                        tracing::error!("{}", self.status);
                    }
                }
            }
        }
    }

    fn handle_edit_event(&mut self, event: EditEvent) {
        match event {
            EditEvent::Canceled => {
                self.pending_edit = None;
                PREVIEW_STATUS.clone_into(&mut self.status);
            }
            EditEvent::Submitted(source) => {
                if let Some((project, command, args)) = self.pending_edit.take() {
                    let name = command.name.clone();
                    if self
                        .command_requests
                        .send(CommandRequest::RunEdited {
                            project,
                            command,
                            source,
                            args,
                        })
                        .is_ok()
                    {
                        self.busy = true;
                        self.status = format!("Running edited {name}…");
                    } else {
                        "Command worker is unavailable.".clone_into(&mut self.status);
                        tracing::error!("{}", self.status);
                    }
                }
            }
        }
    }
}

fn startup_launcher<P: Picker, R: ProcessRunner>(app: &App<P, R>) -> Result<LauncherConfig> {
    let project = app.current_project();
    let config = app.config_for(&project).map_err(|error| match error {
        NixonError::Config(ConfigError::Markdown(markdown)) => NixonError::Markdown(markdown),
        NixonError::Config(other) => {
            let path = load::find_local_file(&project.path())
                .unwrap_or_else(|| project.path().join("nixon.md"));
            NixonError::Config(ConfigError::ParseError(format!(
                "{}: {other}",
                path.display()
            )))
        }
        other => other,
    })?;
    Ok(config.launcher)
}

const MPRIS_PATH: &str = "/org/mpris/MediaPlayer2";
const MPRIS_INTERFACE: &str = "org.mpris.MediaPlayer2.Player";

trait MediaTransport: Send + 'static {
    fn call(
        &mut self,
        destination: &str,
        path: &str,
        interface: &str,
        method: &str,
    ) -> std::result::Result<(), String>;
}

struct SessionMediaTransport;

impl MediaTransport for SessionMediaTransport {
    fn call(
        &mut self,
        destination: &str,
        path: &str,
        interface: &str,
        method: &str,
    ) -> std::result::Result<(), String> {
        #[cfg(target_os = "linux")]
        {
            let connection =
                zbus::blocking::Connection::session().map_err(|err| err.to_string())?;
            connection
                .call_method(Some(destination), path, Some(interface), method, &())
                .map_err(|err| err.to_string())?;
            Ok(())
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = (destination, path, interface, method);
            Err("MPRIS media control is available only on Linux.".to_owned())
        }
    }
}

fn start_media_worker<M: MediaTransport>(mut transport: M) -> MediaWorker {
    let (requests, input) = mpsc::channel::<MediaRequest>();
    let (output, results) = mpsc::channel();
    let worker = thread::spawn(move || {
        while let Ok((operation, player)) = input.recv() {
            let result = control_media(&mut transport, operation, &player);
            if output.send(result).is_err() {
                break;
            }
        }
    });
    (requests, results, worker)
}

fn control_media<M: MediaTransport>(
    transport: &mut M,
    operation: MprisOperation,
    player: &str,
) -> std::result::Result<(), String> {
    let destination = format!("org.mpris.MediaPlayer2.{player}");
    let method = match operation {
        MprisOperation::PlayPause => "PlayPause",
        MprisOperation::Previous => "Previous",
        MprisOperation::Next => "Next",
    };
    transport
        .call(&destination, MPRIS_PATH, MPRIS_INTERFACE, method)
        .map_err(|err| format!("Could not control {player}: {err}"))
}

fn start_browser_worker<R: ProcessRunner + Send + 'static>(
    mut runner: R,
    search_url: String,
) -> (Sender<String>, Receiver<BrowserResult>, JoinHandle<()>) {
    let (requests, input) = mpsc::channel::<String>();
    let (output, results) = mpsc::channel();
    let worker = thread::spawn(move || {
        while let Ok(text) = input.recv() {
            let result = launch_browser(&mut runner, &text, &search_url);
            if output.send(result).is_err() {
                break;
            }
        }
    });
    (requests, results, worker)
}

fn launch_browser<R: ProcessRunner>(
    runner: &mut R,
    input: &str,
    search_url: &str,
) -> BrowserResult {
    let Some(url) = target_url(input, search_url) else {
        return Ok(false);
    };
    let invocation = Invocation {
        argv: vec![browser_opener().to_owned(), url],
        stdin: Some(Vec::new()),
        ..Invocation::default()
    };
    let result = runner
        .run_capture(&invocation)
        .map_err(|err| format!("Failed to launch browser: {err}"))?;
    if result.code == 0 {
        Ok(true)
    } else {
        Err(format!("Browser opener exited with status {}", result.code))
    }
}

const fn browser_opener() -> &'static str {
    if cfg!(target_os = "macos") {
        "open"
    } else {
        "xdg-open"
    }
}

fn pick_current_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
) -> CommandOutcome {
    let project = app.current_project();
    pick_command_for_project(app, &project)
}

fn pick_project_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
) -> CommandOutcome {
    let projects = app.projects();
    if projects.is_empty() {
        return CommandOutcome::Canceled;
    }
    let mut options = select::project_options(&app.config, None, false);
    options.select_one = false;
    let candidates = select::project_candidates(&projects, &app.dirs.home);
    let selection = match app.picker.pick(&options, candidates) {
        Ok(selection) => selection,
        Err(error) => return CommandOutcome::Error(format!("Could not select project: {error}")),
    };
    let (kind, mut items) = match selection {
        Selection::Selected { kind, items } => (kind, items),
        Selection::Empty | Selection::Canceled => return CommandOutcome::Canceled,
    };
    if items.len() != 1 {
        return CommandOutcome::Error("Expected one project selection.".to_owned());
    }
    let picked = items.remove(0);
    let Some(project) = projects
        .into_iter()
        .find(|project| project.path().to_string_lossy() == picked.value)
    else {
        return CommandOutcome::Error(format!("Selected project disappeared: {}", picked.value));
    };
    match kind {
        SelectionType::Default => pick_command_for_project(app, &project),
        SelectionType::Show => project_detail(&project),
        SelectionType::Edit | SelectionType::Visit => CommandOutcome::Canceled,
    }
}

fn project_detail(project: &Project) -> CommandOutcome {
    CommandOutcome::Detail {
        title: format!("Project: {}", project.name.display()),
        body: inspect(std::slice::from_ref(project)),
    }
}

fn pick_command_for_project<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    project: &Project,
) -> CommandOutcome {
    let result = app.commands_for(project).and_then(|commands| {
        let visible: Vec<Command> = commands
            .into_iter()
            .filter(|command| !command.is_hidden)
            .collect();
        if visible.is_empty() {
            return Ok(Selection::Empty);
        }
        app.pick_command(project, &visible, "Select command", None)
    });
    match result {
        Ok(Selection::Selected {
            kind: SelectionType::Default,
            mut items,
        }) => {
            if items.len() != 1 {
                return CommandOutcome::Error("Expected one command selection.".to_owned());
            }
            let command = items.remove(0);
            run_selected_command(app, project, &command)
        }
        Ok(Selection::Selected {
            kind: SelectionType::Show,
            mut items,
        }) => {
            if items.len() != 1 {
                return CommandOutcome::Error("Expected one command selection.".to_owned());
            }
            let command = items.remove(0);
            CommandOutcome::Detail {
                title: format!("Command: {}", command.name),
                body: command.source,
            }
        }
        Ok(Selection::Selected {
            kind: SelectionType::Visit,
            mut items,
        }) => {
            if items.len() != 1 {
                return CommandOutcome::Error("Expected one command selection.".to_owned());
            }
            let command = items.remove(0);
            visit_selected_command(app, project, &command)
        }
        Ok(Selection::Selected {
            kind: SelectionType::Edit,
            mut items,
        }) => {
            if items.len() != 1 {
                return CommandOutcome::Error("Expected one command selection.".to_owned());
            }
            CommandOutcome::Edit {
                project: project.clone(),
                command: Box::new(items.remove(0)),
                args: Vec::new(),
            }
        }
        Ok(Selection::Empty) => CommandOutcome::Empty,
        Ok(Selection::Canceled) => CommandOutcome::Canceled,
        Err(error) => CommandOutcome::Error(format!("Could not load commands: {error}")),
    }
}

fn launch_quick_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    name: &str,
    project_query: Option<&str>,
) -> CommandOutcome {
    let project = match project_query {
        Some(query) => app.project_for_query_with_kind(Some(query)),
        None => Ok((SelectionType::Default, app.current_project())),
    };
    let project = match project {
        Ok((SelectionType::Default, project)) => project,
        Ok((SelectionType::Show, project)) => return project_detail(&project),
        Ok(_) | Err(NixonError::Canceled) => return CommandOutcome::Canceled,
        Err(error) => return CommandOutcome::Error(format!("Could not launch {name}: {error}")),
    };
    let command = app
        .commands_for(&project)
        .and_then(|commands| App::<GuiPicker, GuiProcessRunner<R>>::find_named(&commands, name));
    match command {
        Ok(command) => run_selected_command(app, &project, &command),
        Err(error) => CommandOutcome::Error(format!("Could not launch {name}: {error}")),
    }
}

fn run_selected_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    project: &nixon::project::Project,
    command: &Command,
) -> CommandOutcome {
    run_selected_command_with_args(app, project, command, &[])
}

fn run_selected_command_with_args<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    project: &Project,
    command: &Command,
    args: &[String],
) -> CommandOutcome {
    let config = match app.config_for(project) {
        Ok(config) => config,
        Err(error) => {
            return CommandOutcome::Error(format!("Could not load command config: {error}"));
        }
    };
    app.runner.set_configured_terminal(config.launcher.terminal);
    match app.run_cmd(project, command, args) {
        Ok(_) => CommandOutcome::Launched,
        Err(NixonError::Canceled) => CommandOutcome::Canceled,
        Err(error) => CommandOutcome::Error(format!("Could not run {}: {error}", command.name)),
    }
}

fn run_edited_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    project: &Project,
    command: Command,
    source: &str,
    args: &[String],
) -> CommandOutcome {
    let config = match app.config_for(project) {
        Ok(config) => config,
        Err(error) => {
            return CommandOutcome::Error(format!("Could not load command config: {error}"));
        }
    };
    app.runner.set_configured_terminal(config.launcher.terminal);
    let name = command.name.clone();
    match app.run_edited_cmd(project, command, source, args) {
        Ok(_) => CommandOutcome::Launched,
        Err(NixonError::Canceled) => CommandOutcome::Canceled,
        Err(error) => CommandOutcome::Error(format!("Could not run edited {name}: {error}")),
    }
}

fn visit_selected_command<R: ProcessRunner>(
    app: &mut App<GuiPicker, GuiProcessRunner<R>>,
    project: &Project,
    command: &Command,
) -> CommandOutcome {
    let config = match app.config_for(project) {
        Ok(config) => config,
        Err(error) => {
            return CommandOutcome::Error(format!("Could not load command config: {error}"));
        }
    };
    app.runner.set_configured_terminal(config.launcher.terminal);
    if command.location.is_some()
        && let Err(error) = app.runner.ensure_editor_available(app.env.editor())
    {
        return CommandOutcome::Error(format!("Could not visit {}: {error}", command.name));
    }
    match app.visit_cmd(command) {
        Ok(()) => CommandOutcome::Launched,
        Err(NixonError::Canceled) => CommandOutcome::Canceled,
        Err(error) => CommandOutcome::Error(format!("Could not visit {}: {error}", command.name)),
    }
}

impl eframe::App for PreviewApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.show(ctx);
    }
}

#[cfg(test)]
mod tests;
