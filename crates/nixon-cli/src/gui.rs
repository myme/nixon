//! The graphical menu preview in the existing `nixon` binary.

use std::sync::mpsc::{self, Receiver, Sender};
use std::thread::{self, JoinHandle};

use eframe::egui;
use nixon::app::{App, Environment};
use nixon::command::Command;
use nixon::config::Config;
use nixon::config::launcher::{DEFAULT_SEARCH_URL, LauncherAction};
use nixon::error::{NixonError, Result};
use nixon::fs::Dirs;
use nixon::process::{Invocation, ProcessRunner, RealRunner};
use nixon_gui::browser::target_url;
use nixon_gui::picker::GuiPicker;
use nixon_gui::window::{BrowserInputEvent, MenuWindow, native_options};
use nixon_picker::{Selection, SelectionType};

use crate::cli::Commands;

/// The preview only opens the root menu. No command is silently ignored.
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
    eframe::run_native("Nixon", native_options(), Box::new(|_| Ok(Box::new(app))))
        .map_err(|err| NixonError::Io(std::io::Error::other(err.to_string())))?;
    Ok(0)
}

struct PreviewApp {
    menu: MenuWindow,
    actions: Receiver<LauncherAction>,
    command_requests: Sender<()>,
    command_results: Receiver<CommandOutcome>,
    _worker: JoinHandle<()>,
    browser_requests: Sender<String>,
    browser_results: Receiver<BrowserResult>,
    _browser_worker: JoinHandle<()>,
    busy: bool,
    browser_busy: bool,
    status: String,
}

type BrowserResult = std::result::Result<bool, String>;
const PREVIEW_STATUS: &str = "GUI preview: choose an action.";

enum CommandOutcome {
    Selected { name: String, kind: SelectionType },
    Empty,
    Canceled,
    Error(String),
}

impl PreviewApp {
    fn new(config: Config, dirs: Dirs, env: Environment) -> Result<Self> {
        Self::with_browser_runner(config, dirs, env, RealRunner)
    }

    fn with_browser_runner<R: ProcessRunner + Send + 'static>(
        config: Config,
        dirs: Dirs,
        env: Environment,
        browser_runner: R,
    ) -> Result<Self> {
        let (sender, actions) = mpsc::channel();
        let mut menu = MenuWindow::new(&config.launcher, sender).ok_or_else(|| {
            NixonError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "GUI launcher menu has no items",
            ))
        })?;
        let (picker, requests) = GuiPicker::channel();
        menu.attach_picker(requests);
        let search_url = config
            .launcher
            .search_url
            .as_deref()
            .unwrap_or(DEFAULT_SEARCH_URL)
            .to_owned();
        let (command_requests, worker_requests) = mpsc::channel();
        let (worker_results, command_results) = mpsc::channel();
        let worker = thread::spawn(move || {
            let mut app = App::new(config, dirs, env, picker, RealRunner);
            while worker_requests.recv().is_ok() {
                let outcome = pick_current_command(&mut app);
                if worker_results.send(outcome).is_err() {
                    break;
                }
            }
        });
        let (browser_requests, browser_results, browser_worker) =
            start_browser_worker(browser_runner, search_url);
        Ok(Self {
            menu,
            actions,
            command_requests,
            command_results,
            _worker: worker,
            browser_requests,
            browser_results,
            _browser_worker: browser_worker,
            busy: false,
            browser_busy: false,
            status: PREVIEW_STATUS.to_owned(),
        })
    }

    fn show(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::bottom("preview_status").show(ctx, |ui| {
            ui.label(&self.status);
        });
        self.menu.show(ctx);
        for action in self.actions.try_iter() {
            if action == LauncherAction::Commands {
                if !self.busy && !self.browser_busy && self.command_requests.send(()).is_ok() {
                    self.busy = true;
                    "Loading commands…".clone_into(&mut self.status);
                }
            } else if action == LauncherAction::BrowserInput {
                if !self.busy && !self.browser_busy {
                    self.menu.open_browser_input();
                    "Enter a URL or search terms.".clone_into(&mut self.status);
                }
            } else {
                self.status = format!(
                    "{} selected. Execution is not wired yet.",
                    action_name(&action)
                );
            }
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
        for outcome in self.command_results.try_iter() {
            self.busy = false;
            self.status = match outcome {
                CommandOutcome::Selected { name, kind } => {
                    format!("Selected command: {name} ({kind:?}). Execution is not wired yet.")
                }
                CommandOutcome::Empty => "No commands available.".to_owned(),
                CommandOutcome::Canceled => PREVIEW_STATUS.to_owned(),
                CommandOutcome::Error(error) => format!("Could not load commands: {error}"),
            };
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
        if self.busy || self.browser_busy {
            ctx.request_repaint_after(std::time::Duration::from_millis(30));
        }
    }
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

fn pick_current_command(app: &mut App<GuiPicker, RealRunner>) -> CommandOutcome {
    let project = app.current_project();
    let result = app.commands_for(&project).and_then(|commands| {
        let visible: Vec<Command> = commands
            .into_iter()
            .filter(|command| !command.is_hidden)
            .collect();
        app.pick_command(&project, &visible, "Select command", None)
    });
    match result {
        Ok(Selection::Selected { kind, items }) => {
            let name = items
                .into_iter()
                .map(|command| command.name)
                .collect::<Vec<_>>()
                .join(", ");
            CommandOutcome::Selected { name, kind }
        }
        Ok(Selection::Empty) => CommandOutcome::Empty,
        Ok(Selection::Canceled) => CommandOutcome::Canceled,
        Err(error) => CommandOutcome::Error(error.to_string()),
    }
}

impl eframe::App for PreviewApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.show(ctx);
    }
}

const fn action_name(action: &LauncherAction) -> &str {
    match action {
        LauncherAction::Commands => "Commands",
        LauncherAction::Projects => "Projects",
        LauncherAction::History => "History",
        LauncherAction::BrowserInput => "Browser",
        LauncherAction::Mpris { .. } => "Media control",
        LauncherAction::Command { .. } => "Command",
    }
}

#[cfg(test)]
mod tests {
    use std::io;
    use std::sync::mpsc::TryRecvError;
    use std::sync::{Arc, Mutex};
    use std::thread;
    use std::time::{Duration, Instant};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;
    use eframe::egui;
    use nixon::app::{App, Environment};
    use nixon::command::Command;
    use nixon::config::Config;
    use nixon::fs::Dirs;
    use nixon::process::{Captured, Invocation, ProcessRunner, RealRunner, Running};
    use nixon_gui::picker::{GuiPicker, PickerRequest};
    use nixon_picker::Selection;
    use nixon_picker::matcher::Case;

    use super::{CommandOutcome, PreviewApp, browser_opener, launch_browser, pick_current_command};

    struct BrowserRunner {
        calls: Arc<Mutex<Vec<Invocation>>>,
        result: std::result::Result<i32, io::ErrorKind>,
    }

    impl ProcessRunner for BrowserRunner {
        fn run(&mut self, _invocation: &Invocation) -> io::Result<i32> {
            Err(io::ErrorKind::Unsupported.into())
        }

        fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
            self.calls.lock().unwrap().push(invocation.clone());
            self.result.map_or_else(
                |kind| Err(io::Error::new(kind, "opener unavailable")),
                |code| {
                    Ok(Captured {
                        code,
                        stdout: Vec::new(),
                    })
                },
            )
        }

        fn spawn_detached(&mut self, _invocation: &Invocation) -> io::Result<()> {
            Err(io::ErrorKind::Unsupported.into())
        }

        fn run_streaming(
            &mut self,
            _invocation: &Invocation,
            _sink: Box<dyn FnMut(String) + Send>,
        ) -> io::Result<Box<dyn Running>> {
            Err(io::ErrorKind::Unsupported.into())
        }
    }

    fn browser_runner(
        result: std::result::Result<i32, io::ErrorKind>,
    ) -> (BrowserRunner, Arc<Mutex<Vec<Invocation>>>) {
        let calls = Arc::new(Mutex::new(Vec::new()));
        (
            BrowserRunner {
                calls: Arc::clone(&calls),
                result,
            },
            calls,
        )
    }

    const LOCAL_COMMANDS: &str = "\
```yaml config
exact_match: true
ignore_case: false
```

# `zeta`

```bash
echo zeta
```

# `alpha`

```bash
touch selected-marker
```

# `_hidden`

```bash
echo hidden
```
";

    fn fixture(local: &str) -> (TempDir, Config, Dirs, Environment) {
        let temp = TempDir::new().unwrap();
        let project = temp.child("project");
        project.create_dir_all().unwrap();
        project.child("nixon.md").write_str(local).unwrap();
        let mut config = Config::defaults();
        config.commands.push(Command {
            name: "beta".to_owned(),
            ..Command::default()
        });
        let dirs = Dirs {
            home: temp.path().to_path_buf(),
            config: temp.child("config").to_path_buf(),
            cache: temp.child("cache").to_path_buf(),
            state: temp.child("state").to_path_buf(),
        };
        let env = Environment {
            cwd: project.to_path_buf(),
            ..Environment::default()
        };
        (temp, config, dirs, env)
    }

    fn key(key: egui::Key) -> egui::Event {
        egui::Event::Key {
            key,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: egui::Modifiers::default(),
        }
    }

    fn frame(
        app: &mut PreviewApp,
        ctx: &egui::Context,
        events: Vec<egui::Event>,
    ) -> egui::FullOutput {
        ctx.run(
            egui::RawInput {
                screen_rect: Some(egui::Rect::from_min_size(
                    egui::Pos2::ZERO,
                    egui::vec2(480.0, 360.0),
                )),
                events,
                ..egui::RawInput::default()
            },
            |ctx| app.show(ctx),
        )
    }

    fn texts(output: &egui::FullOutput) -> Vec<String> {
        output
            .shapes
            .iter()
            .filter_map(|clipped| match &clipped.shape {
                egui::Shape::Text(shape) => Some(shape.galley.text().to_owned()),
                _ => None,
            })
            .collect()
    }

    fn wait_for_text(app: &mut PreviewApp, ctx: &egui::Context, wanted: &str) -> egui::FullOutput {
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            let output = frame(app, ctx, Vec::new());
            if texts(&output).iter().any(|text| text.contains(wanted)) {
                return output;
            }
            assert!(Instant::now() < deadline, "did not render {wanted}");
            thread::sleep(Duration::from_millis(1));
        }
    }

    fn open_browser_input(app: &mut PreviewApp, ctx: &egui::Context) {
        let _ = frame(app, ctx, vec![key(egui::Key::W)]);
        let _ = frame(app, ctx, vec![key(egui::Key::O)]);
        wait_for_text(app, ctx, "Open URL or search");
    }

    fn closes(output: &egui::FullOutput) -> bool {
        output.viewport_output.values().any(|viewport| {
            viewport
                .commands
                .iter()
                .any(|command| matches!(command, egui::ViewportCommand::Close))
        })
    }

    #[test]
    fn browser_runner_receives_one_argv_and_empty_input_spawns_nothing() {
        let (mut runner, calls) = browser_runner(Ok(0));
        assert_eq!(
            launch_browser(
                &mut runner,
                "café + tea",
                "https://search.example/?q={query}"
            ),
            Ok(true)
        );
        assert_eq!(
            launch_browser(&mut runner, "  ", "https://search.example/?q={query}"),
            Ok(false)
        );
        let calls = calls.lock().unwrap();
        assert_eq!(calls.len(), 1);
        assert_eq!(
            calls[0].argv,
            [
                browser_opener().to_owned(),
                "https://search.example/?q=caf%C3%A9%20%2B%20tea".to_owned()
            ]
        );
        assert_eq!(calls[0].stdin, Some(Vec::new()));
    }

    #[test]
    fn browser_spawn_error_is_reported() {
        let (mut runner, calls) = browser_runner(Err(io::ErrorKind::NotFound));
        let error = launch_browser(&mut runner, "example.com", "unused {query}").unwrap_err();
        assert!(error.contains("Failed to launch browser"), "{error}");
        assert_eq!(calls.lock().unwrap().len(), 1);
    }

    #[test]
    fn browser_success_closes_the_window_with_configured_search_url() {
        let (_temp, mut config, dirs, env) = fixture(LOCAL_COMMANDS);
        config.launcher.search_url = Some("https://search.example/?q={query}".to_owned());
        let (runner, calls) = browser_runner(Ok(0));
        let mut app = PreviewApp::with_browser_runner(config, dirs, env, runner).unwrap();
        let ctx = egui::Context::default();
        open_browser_input(&mut app, &ctx);
        let _ = frame(
            &mut app,
            &ctx,
            vec![egui::Event::Text("cat + tea".to_owned())],
        );
        let mut output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
        let deadline = Instant::now() + Duration::from_secs(5);
        while !closes(&output) {
            assert!(
                Instant::now() < deadline,
                "browser did not close the window"
            );
            thread::sleep(Duration::from_millis(1));
            output = frame(&mut app, &ctx, Vec::new());
        }
        assert_eq!(
            calls.lock().unwrap()[0].argv[1],
            "https://search.example/?q=cat%20%2B%20tea"
        );
    }

    #[test]
    fn browser_failure_stays_open_and_shows_error() {
        let (_temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let (runner, calls) = browser_runner(Ok(3));
        let mut app = PreviewApp::with_browser_runner(config, dirs, env, runner).unwrap();
        let ctx = egui::Context::default();
        open_browser_input(&mut app, &ctx);
        let _ = frame(
            &mut app,
            &ctx,
            vec![egui::Event::Text("example.com".to_owned())],
        );
        let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
        assert!(!closes(&output));
        let output = wait_for_text(&mut app, &ctx, "Browser opener exited with status 3");
        assert!(!closes(&output));
        assert_eq!(calls.lock().unwrap().len(), 1);
    }

    #[test]
    fn worker_uses_real_discovery_local_matching_and_visible_order() {
        let (_temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let (picker, requests) = GuiPicker::channel();
        let mut app = App::new(config, dirs, env, picker, RealRunner);
        let worker = thread::spawn(move || pick_current_command(&mut app));
        let deadline = Instant::now() + Duration::from_secs(5);
        let request = loop {
            match requests.try_recv() {
                Ok(request) => break request,
                Err(TryRecvError::Empty) => {
                    assert!(Instant::now() < deadline, "command pick did not arrive");
                    thread::sleep(Duration::from_millis(1));
                }
                Err(TryRecvError::Disconnected) => panic!("worker closed the picker bridge"),
            }
        };
        let PickerRequest::Pick(request) = request else {
            panic!("expected a command pick");
        };
        assert!(request.options.matching.exact);
        assert_eq!(request.options.matching.case, Case::Respect);
        assert!(!request.options.matching.sort);
        assert_eq!(
            request
                .candidates
                .iter()
                .map(|candidate| candidate.value.as_str())
                .collect::<Vec<_>>(),
            ["alpha", "beta", "zeta"]
        );
        request.respond(Selection::Canceled).unwrap();
        assert!(matches!(worker.join().unwrap(), CommandOutcome::Canceled));
    }

    #[test]
    fn selected_command_shows_name_and_selection_type_without_running() {
        let (temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let mut app = PreviewApp::new(config, dirs, env).unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
        let output = wait_for_text(&mut app, &ctx, "alpha");
        let shown = texts(&output);
        let alpha = shown.iter().position(|text| text == "alpha").unwrap();
        let beta = shown.iter().position(|text| text == "beta").unwrap();
        let zeta = shown.iter().position(|text| text == "zeta").unwrap();
        assert!(alpha < beta && beta < zeta);
        assert!(!shown.iter().any(|text| text.contains("_hidden")));

        let _ = frame(&mut app, &ctx, vec![key(egui::Key::F1)]);
        wait_for_text(&mut app, &ctx, "Selected command: alpha (Show)");
        assert!(!temp.child("project/selected-marker").path().exists());
    }

    #[test]
    fn default_command_selection_does_not_execute() {
        let (temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let mut app = PreviewApp::new(config, dirs, env).unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
        wait_for_text(&mut app, &ctx, "Select command");
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
        wait_for_text(&mut app, &ctx, "Selected command: alpha (Default)");
        assert!(!temp.child("project/selected-marker").path().exists());
    }

    #[test]
    fn canceling_command_pick_returns_to_the_menu_quietly() {
        let (_temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let mut app = PreviewApp::new(config, dirs, env).unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
        wait_for_text(&mut app, &ctx, "alpha");
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
        let output = wait_for_text(&mut app, &ctx, "GUI preview: choose an action.");
        let shown = texts(&output);
        assert!(shown.iter().any(|text| text == "Commands"));
        assert!(!shown.iter().any(|text| text.contains("Selection canceled")));
    }

    #[test]
    fn discovery_error_is_visible_in_the_window() {
        let (_temp, config, dirs, env) = fixture("# `broken`\n\nno source block\n");
        let mut app = PreviewApp::new(config, dirs, env).unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
        let output = wait_for_text(&mut app, &ctx, "Could not load commands:");
        assert!(texts(&output).iter().any(|text| text == "Commands"));
    }

    #[test]
    fn other_actions_keep_the_preview_status() {
        let (_temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let mut app = PreviewApp::new(config, dirs, env).unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::P)]);
        wait_for_text(
            &mut app,
            &ctx,
            "Projects selected. Execution is not wired yet.",
        );
    }
}
