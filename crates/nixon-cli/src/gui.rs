//! The graphical menu preview in the existing `nixon` binary.

use std::sync::mpsc::{self, Receiver, Sender};
use std::thread::{self, JoinHandle};

use eframe::egui;
use nixon::app::{App, Environment};
use nixon::command::Command;
use nixon::config::Config;
use nixon::config::launcher::LauncherAction;
use nixon::error::{NixonError, Result};
use nixon::fs::Dirs;
use nixon::process::RealRunner;
use nixon_gui::picker::GuiPicker;
use nixon_gui::window::{MenuWindow, native_options};
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
    busy: bool,
    status: String,
}

enum CommandOutcome {
    Selected { name: String, kind: SelectionType },
    Empty,
    Canceled,
    Error(String),
}

impl PreviewApp {
    fn new(config: Config, dirs: Dirs, env: Environment) -> Result<Self> {
        let (sender, actions) = mpsc::channel();
        let mut menu = MenuWindow::new(&config.launcher, sender).ok_or_else(|| {
            NixonError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "GUI launcher menu has no items",
            ))
        })?;
        let (picker, requests) = GuiPicker::channel();
        menu.attach_picker(requests);
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
        Ok(Self {
            menu,
            actions,
            command_requests,
            command_results,
            _worker: worker,
            busy: false,
            status: "GUI preview: actions are not wired yet.".to_owned(),
        })
    }

    fn show(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::bottom("preview_status").show(ctx, |ui| {
            ui.label(&self.status);
        });
        self.menu.show(ctx);
        for action in self.actions.try_iter() {
            if action == LauncherAction::Commands {
                if !self.busy && self.command_requests.send(()).is_ok() {
                    self.busy = true;
                    "Loading commands…".clone_into(&mut self.status);
                }
            } else {
                self.status = format!(
                    "{} selected. Execution is not wired yet.",
                    action_name(&action)
                );
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
                CommandOutcome::Canceled => "GUI preview: actions are not wired yet.".to_owned(),
                CommandOutcome::Error(error) => format!("Could not load commands: {error}"),
            };
            ctx.request_repaint();
        }
        if self.busy {
            ctx.request_repaint_after(std::time::Duration::from_millis(30));
        }
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
    use std::sync::mpsc::TryRecvError;
    use std::thread;
    use std::time::{Duration, Instant};

    use assert_fs::TempDir;
    use assert_fs::prelude::*;
    use eframe::egui;
    use nixon::app::{App, Environment};
    use nixon::command::Command;
    use nixon::config::Config;
    use nixon::fs::Dirs;
    use nixon::process::RealRunner;
    use nixon_gui::picker::{GuiPicker, PickerRequest};
    use nixon_picker::Selection;
    use nixon_picker::matcher::Case;

    use super::{CommandOutcome, PreviewApp, pick_current_command};

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
        let output = wait_for_text(&mut app, &ctx, "GUI preview: actions are not wired yet.");
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
