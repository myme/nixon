use std::ffi::OsString;
use std::fs;
use std::io;
use std::path::PathBuf;
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
use nixon::config::launcher::{LauncherAction, MenuItem, MenuKey, MprisOperation};
use nixon::fs::Dirs;
use nixon::history::Entry;
use nixon::process::{Captured, Invocation, ProcessRunner, RealRunner, Running};
use nixon::project::{ProjectMarker, ProjectType};
use nixon_gui::picker::{GuiPicker, PickerRequest};
use nixon_picker::Selection;
use nixon_picker::matcher::Case;

use crate::gui_exec::read_payload;
use crate::gui_process::GuiProcessRunner;

use super::{
    CommandOutcome, MediaTransport, PreviewApp, browser_opener, launch_browser,
    pick_current_command,
};

type MediaCall = (String, String, String, String);
type InvocationCalls = Arc<Mutex<Vec<Invocation>>>;

struct FakeMediaTransport {
    calls: Arc<Mutex<Vec<MediaCall>>>,
    result: std::result::Result<(), String>,
}

impl MediaTransport for FakeMediaTransport {
    fn call(
        &mut self,
        destination: &str,
        path: &str,
        interface: &str,
        method: &str,
    ) -> std::result::Result<(), String> {
        self.calls.lock().unwrap().push((
            destination.to_owned(),
            path.to_owned(),
            interface.to_owned(),
            method.to_owned(),
        ));
        self.result.clone()
    }
}

fn fake_media(
    result: std::result::Result<(), String>,
) -> (FakeMediaTransport, Arc<Mutex<Vec<MediaCall>>>) {
    let calls = Arc::new(Mutex::new(Vec::new()));
    (
        FakeMediaTransport {
            calls: Arc::clone(&calls),
            result,
        },
        calls,
    )
}

struct BrowserRunner {
    calls: Arc<Mutex<Vec<Invocation>>>,
    result: std::result::Result<i32, io::ErrorKind>,
}

struct CommandRunner {
    calls: Arc<Mutex<Vec<Invocation>>>,
    captures: Arc<Mutex<Vec<Invocation>>>,
    capture_output: Option<Vec<u8>>,
    spawn_error: bool,
}

struct FinishedStream;

impl Running for FinishedStream {
    fn wait(&mut self) -> io::Result<i32> {
        Ok(0)
    }

    fn kill(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl ProcessRunner for CommandRunner {
    fn run(&mut self, _invocation: &Invocation) -> io::Result<i32> {
        Err(io::ErrorKind::Unsupported.into())
    }

    fn run_capture(&mut self, invocation: &Invocation) -> io::Result<Captured> {
        self.captures.lock().unwrap().push(invocation.clone());
        self.capture_output.as_ref().map_or_else(
            || Err(io::ErrorKind::Unsupported.into()),
            |stdout| {
                Ok(Captured {
                    code: 0,
                    stdout: stdout.clone(),
                })
            },
        )
    }

    fn spawn_detached(&mut self, invocation: &Invocation) -> io::Result<()> {
        self.calls.lock().unwrap().push(invocation.clone());
        if self.spawn_error {
            Err(io::Error::new(io::ErrorKind::NotFound, "terminal failed"))
        } else {
            Ok(())
        }
    }

    fn run_streaming(
        &mut self,
        invocation: &Invocation,
        mut sink: Box<dyn FnMut(String) + Send>,
    ) -> io::Result<Box<dyn Running>> {
        let Some(stdout) = self.capture_output.as_ref() else {
            return Err(io::ErrorKind::Unsupported.into());
        };
        self.captures.lock().unwrap().push(invocation.clone());
        for line in String::from_utf8_lossy(stdout).lines() {
            sink(line.to_owned());
        }
        Ok(Box::new(FinishedStream))
    }
}

fn command_runner(
    terminal: Option<Vec<String>>,
    exe: PathBuf,
    spawn_error: bool,
) -> (GuiProcessRunner<CommandRunner>, Arc<Mutex<Vec<Invocation>>>) {
    command_runner_with_output(terminal, exe, spawn_error, None)
}

fn command_runner_with_output(
    terminal: Option<Vec<String>>,
    exe: PathBuf,
    spawn_error: bool,
    capture_output: Option<Vec<u8>>,
) -> (GuiProcessRunner<CommandRunner>, Arc<Mutex<Vec<Invocation>>>) {
    let calls = Arc::new(Mutex::new(Vec::new()));
    (
        GuiProcessRunner::new(
            CommandRunner {
                calls: Arc::clone(&calls),
                captures: Arc::new(Mutex::new(Vec::new())),
                capture_output,
                spawn_error,
            },
            terminal,
            None,
            Some(OsString::from("/nonexistent")),
            exe,
        ),
        calls,
    )
}

fn executable(temp: &TempDir, name: &str) -> PathBuf {
    let path = temp.child(name);
    path.write_str("").unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt as _;

        fs::set_permissions(path.path(), fs::Permissions::from_mode(0o755)).unwrap();
    }
    path.path().to_path_buf()
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

const PLACEHOLDER_COMMANDS: &str = "\
# `_choices`

```bash
printf 'unused producer source\\n'
```

# `alpha ${_choices | json}`

```bash
printf '%s\\n' \"$1\"
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

fn write_history(temp: &TempDir, entries: &[Entry]) {
    let path = temp.child("state/nixon/history");
    fs::create_dir_all(path.path().parent().unwrap()).unwrap();
    fs::write(
        path.path(),
        entries.iter().map(Entry::line).collect::<String>(),
    )
    .unwrap();
}

fn history_entry(cwd: &str, words: &[&str]) -> Entry {
    Entry {
        at: 1_700_000_000,
        cwd: cwd.to_owned(),
        invocation: words.iter().map(|word| (*word).to_owned()).collect(),
    }
}

fn add_quick_action(config: &mut Config, name: &str, project: Option<String>) {
    config
        .launcher
        .items
        .as_mut()
        .unwrap()
        .push(MenuItem::Action {
            key: MenuKey::Character('Q'),
            label: "Quick command".to_owned(),
            description: None,
            action: LauncherAction::Command {
                name: name.to_owned(),
                project,
            },
        });
}

fn preview_with_fake_command(
    config: Config,
    dirs: Dirs,
    env: Environment,
) -> (PreviewApp, InvocationCalls) {
    let exe = env.exe.clone().unwrap();
    let (runner, calls) = command_runner(config.launcher.terminal.clone(), exe, false);
    let app = PreviewApp::with_command_runner(
        config,
        dirs,
        env,
        RealRunner,
        super::SessionMediaTransport,
        runner,
    )
    .unwrap();
    (app, calls)
}

fn discovered_projects(temp: &TempDir, config: &mut Config) -> (PathBuf, PathBuf) {
    let source = temp.child("projects");
    let first = source.child("work-one");
    let second = source.child("work-two");
    for project in [&first, &second] {
        project.create_dir_all().unwrap();
        project.child(".git").create_dir_all().unwrap();
        project
            .child("nixon.md")
            .write_str("# `jump`\n\n```bash\ntouch jump-marker\n```\n")
            .unwrap();
    }
    config.project_dirs.push(source.path().to_path_buf());
    config.project_types.push(ProjectType {
        id: "git".to_owned(),
        markers: vec![ProjectMarker::Path(PathBuf::from(".git"))],
        description: "Git".to_owned(),
    });
    (first.path().to_path_buf(), second.path().to_path_buf())
}

fn placeholder_fixture(
    quick: bool,
    projects: bool,
) -> (TempDir, PreviewApp, InvocationCalls, InvocationCalls) {
    let (temp, mut config, dirs, mut env) = fixture(PLACEHOLDER_COMMANDS);
    if quick {
        add_quick_action(&mut config, "alpha", None);
    }
    if projects {
        let (first, _) = discovered_projects(&temp, &mut config);
        let local = PLACEHOLDER_COMMANDS.replacen(
            "# `alpha ${_choices | json}`",
            "# `alpha --fast ${_choices | json}`\n\n- `--fast`: on",
            1,
        );
        fs::write(first.join("nixon.md"), local).unwrap();
    }
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    let exe = temp.child("nixon").path().to_path_buf();
    env.exe = Some(exe.clone());
    let calls = Arc::new(Mutex::new(Vec::new()));
    let captures = Arc::new(Mutex::new(Vec::new()));
    let runner = GuiProcessRunner::new(
        CommandRunner {
            calls: Arc::clone(&calls),
            captures: Arc::clone(&captures),
            capture_output: Some(br#"["choice with spaces","quoted 'choice'"]"#.to_vec()),
            spawn_error: false,
        },
        config.launcher.terminal.clone(),
        None,
        Some(OsString::from("/nonexistent")),
        exe,
    );
    let app = PreviewApp::with_command_runner(
        config,
        dirs,
        env,
        RealRunner,
        super::SessionMediaTransport,
        runner,
    )
    .unwrap();
    (temp, app, calls, captures)
}

fn open_placeholder_pick(app: &mut PreviewApp, ctx: &egui::Context) {
    let _ = frame(app, ctx, vec![key(egui::Key::C)]);
    wait_for_text(app, ctx, "Select command");
    let _ = frame(app, ctx, vec![key(egui::Key::Enter)]);
    let _ = frame(app, ctx, vec![key_release(egui::Key::Enter)]);
    wait_for_text(app, ctx, "choice with spaces");
}

fn key(key: egui::Key) -> egui::Event {
    key_with_modifiers(key, egui::Modifiers::default())
}

fn key_with_modifiers(key: egui::Key, modifiers: egui::Modifiers) -> egui::Event {
    egui::Event::Key {
        key,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers,
    }
}

fn key_release(key: egui::Key) -> egui::Event {
    egui::Event::Key {
        key,
        physical_key: None,
        pressed: false,
        repeat: false,
        modifiers: egui::Modifiers::default(),
    }
}

fn frame(app: &mut PreviewApp, ctx: &egui::Context, events: Vec<egui::Event>) -> egui::FullOutput {
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

fn copied_text(output: &egui::FullOutput) -> Option<&str> {
    output.platform_output.commands.iter().find_map(|command| {
        if let egui::OutputCommand::CopyText(text) = command {
            Some(text.as_str())
        } else {
            None
        }
    })
}

fn copy_detail(app: &mut PreviewApp, ctx: &egui::Context) -> egui::FullOutput {
    frame(
        app,
        ctx,
        vec![key_with_modifiers(
            egui::Key::C,
            egui::Modifiers {
                ctrl: true,
                ..egui::Modifiers::default()
            },
        )],
    )
}

fn click_button(
    app: &mut PreviewApp,
    ctx: &egui::Context,
    output: &egui::FullOutput,
    label: &str,
) -> egui::FullOutput {
    let at = output
        .shapes
        .iter()
        .find_map(|shape| match &shape.shape {
            egui::Shape::Text(text) if text.galley.text() == label => {
                Some(text.pos + text.galley.size() / 2.0)
            }
            _ => None,
        })
        .unwrap();
    frame(
        app,
        ctx,
        vec![
            egui::Event::PointerMoved(at),
            egui::Event::PointerButton {
                pos: at,
                button: egui::PointerButton::Primary,
                pressed: true,
                modifiers: egui::Modifiers::default(),
            },
        ],
    );
    frame(
        app,
        ctx,
        vec![egui::Event::PointerButton {
            pos: at,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::default(),
        }],
    )
}

fn replace_edit_text(app: &mut PreviewApp, ctx: &egui::Context, source: &str) {
    let ctrl = egui::Modifiers {
        ctrl: true,
        command: true,
        ..egui::Modifiers::default()
    };
    let _ = frame(app, ctx, vec![key_with_modifiers(egui::Key::A, ctrl)]);
    let _ = frame(app, ctx, vec![key_release(egui::Key::A)]);
    let _ = frame(app, ctx, vec![egui::Event::Text(source.to_owned())]);
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

fn replay_first_history_entry(app: &mut PreviewApp, ctx: &egui::Context) {
    let output = choose_first_history_entry(app, ctx);
    if !closes(&output) {
        wait_for_close(app, ctx);
    }
}

fn choose_first_history_entry(app: &mut PreviewApp, ctx: &egui::Context) -> egui::FullOutput {
    let _ = frame(app, ctx, vec![key(egui::Key::H)]);
    wait_for_text(app, ctx, "History (Enter replays");
    frame(app, ctx, vec![key(egui::Key::Enter)])
}

fn closes(output: &egui::FullOutput) -> bool {
    output.viewport_output.values().any(|viewport| {
        viewport
            .commands
            .iter()
            .any(|command| matches!(command, egui::ViewportCommand::Close))
    })
}

fn wait_for_close(app: &mut PreviewApp, ctx: &egui::Context) {
    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        let output = frame(app, ctx, Vec::new());
        if closes(&output) {
            return;
        }
        assert!(
            Instant::now() < deadline,
            "action did not close the window; visible text: {:?}",
            texts(&output)
        );
        thread::sleep(Duration::from_millis(1));
    }
}

fn wait_for_idle(app: &mut PreviewApp, ctx: &egui::Context) -> egui::FullOutput {
    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        let _ = frame(app, ctx, Vec::new());
        if !app.busy {
            return frame(app, ctx, Vec::new());
        }
        assert!(Instant::now() < deadline, "command worker did not finish");
        thread::sleep(Duration::from_millis(1));
    }
}

#[test]
fn default_spotify_shortcuts_call_exact_mpris_methods_and_close() {
    for (shortcut, method) in [
        (egui::Key::Space, "PlayPause"),
        (egui::Key::P, "Previous"),
        (egui::Key::N, "Next"),
    ] {
        let (_temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
        let (transport, calls) = fake_media(Ok(()));
        let (browser, _) = browser_runner(Ok(0));
        let mut app = PreviewApp::with_workers(config, dirs, env, browser, transport).unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::S)]);
        let output = frame(&mut app, &ctx, vec![key(shortcut)]);
        if !closes(&output) {
            wait_for_close(&mut app, &ctx);
        }
        assert_eq!(
            *calls.lock().unwrap(),
            [(
                "org.mpris.MediaPlayer2.spotify".to_owned(),
                "/org/mpris/MediaPlayer2".to_owned(),
                "org.mpris.MediaPlayer2.Player".to_owned(),
                method.to_owned(),
            )]
        );
    }
}

#[test]
fn configured_player_failure_is_visible_and_keeps_menu_open() {
    let (_temp, mut config, dirs, env) = fixture(LOCAL_COMMANDS);
    let Some(items) = config.launcher.items.as_mut() else {
        panic!("default launcher has no items");
    };
    let Some(MenuItem::Submenu { items, .. }) = items
        .iter_mut()
        .find(|item| matches!(item, MenuItem::Submenu { label, .. } if label == "Spotify"))
    else {
        panic!("default launcher has no Spotify menu");
    };
    let Some(MenuItem::Action {
        action: LauncherAction::Mpris { player, operation },
        ..
    }) = items.first_mut()
    else {
        panic!("default launcher has no PlayPause action");
    };
    *player = "vlc".to_owned();
    assert_eq!(*operation, MprisOperation::PlayPause);

    let (transport, calls) = fake_media(Err("service unavailable".to_owned()));
    let (browser, _) = browser_runner(Ok(0));
    let mut app = PreviewApp::with_workers(config, dirs, env, browser, transport).unwrap();
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::S)]);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Space)]);
    assert!(!closes(&output));
    let output = wait_for_text(&mut app, &ctx, "Could not control vlc: service unavailable");
    assert!(!closes(&output));
    assert!(texts(&output).iter().any(|text| text == "Play/Pause"));
    let output = frame(&mut app, &ctx, vec![key(egui::Key::N)]);
    assert!(!closes(&output));
    let output = wait_for_text(
        &mut app,
        &ctx,
        "Could not control spotify: service unavailable",
    );
    assert!(!closes(&output));
    let calls = calls.lock().unwrap();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].0, "org.mpris.MediaPlayer2.vlc");
    assert_eq!(calls[1].0, "org.mpris.MediaPlayer2.spotify");
    assert_eq!(calls[1].3, "Next");
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
    let (temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
    let (picker, requests) = GuiPicker::channel();
    let runner = GuiProcessRunner::new(RealRunner, None, None, None, temp.path().join("nixon"));
    let mut app = App::new(config, dirs, env, picker, runner);
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
fn command_show_displays_exact_source_and_returns_to_menu() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
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
    wait_for_text(&mut app, &ctx, "Command: alpha");
    let output = copy_detail(&mut app, &ctx);
    assert_eq!(copied_text(&output), Some("touch selected-marker\n"));
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::C)]);
    assert!(calls.lock().unwrap().is_empty());
    assert!(!temp.child("project/selected-marker").path().exists());
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
    wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Escape)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
}

#[test]
fn edited_command_runs_exact_source_with_original_language_and_project() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    let output = wait_for_text(&mut app, &ctx, "Edit command: alpha");
    assert!(
        texts(&output)
            .iter()
            .any(|text| text.contains("touch selected-marker"))
    );
    assert!(calls.lock().unwrap().is_empty());
    replace_edit_text(&mut app, &ctx, "printf 'edited result'\n\n");
    let output = frame(&mut app, &ctx, Vec::new());
    let output = click_button(&mut app, &ctx, &output, "Submit");
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    assert_eq!(payload.argv[0], "bash");
    assert_eq!(
        fs::read_to_string(&payload.argv[1]).unwrap(),
        "printf 'edited result'\n"
    );
    assert!(!temp.child("project/selected-marker").path().exists());
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("nixon run alpha"), "{history}");
}

#[test]
fn canceling_edit_returns_to_menu_without_running() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    let output = wait_for_text(&mut app, &ctx, "Edit command: alpha");
    click_button(&mut app, &ctx, &output, "Back");
    let _ = wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    assert!(calls.lock().unwrap().is_empty());
    assert!(!temp.child("state/nixon/history").path().exists());
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::C)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
}

#[test]
fn closing_window_during_edit_releases_command_worker() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    wait_for_text(&mut app, &ctx, "Edit command: alpha");
    let PreviewApp {
        _worker: worker,
        command_requests,
        menu,
        ..
    } = app;
    drop(menu);
    drop(command_requests);
    let (sender, receiver) = std::sync::mpsc::channel();
    thread::spawn(move || sender.send(worker.join()).ok());
    assert!(
        receiver
            .recv_timeout(Duration::from_secs(2))
            .unwrap()
            .is_ok()
    );
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn empty_edit_stays_open_until_valid_source_is_submitted() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    wait_for_text(&mut app, &ctx, "Edit command: alpha");
    replace_edit_text(&mut app, &ctx, "  \n ");
    let output = frame(&mut app, &ctx, Vec::new());
    click_button(&mut app, &ctx, &output, "Submit");
    let output = wait_for_text(&mut app, &ctx, "Empty command.");
    assert!(
        texts(&output)
            .iter()
            .any(|text| text == "Edit command: alpha")
    );
    assert!(calls.lock().unwrap().is_empty());
    assert!(!temp.child("state/nixon/history").path().exists());
    replace_edit_text(&mut app, &ctx, "echo valid");
    let output = frame(&mut app, &ctx, Vec::new());
    let output = click_button(&mut app, &ctx, &output, "Submit");
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    assert_eq!(calls.lock().unwrap().len(), 1);
}

#[test]
fn edited_command_continues_through_placeholder_pick() {
    let (temp, mut app, calls, captures) = placeholder_fixture(false, false);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Enter)]);
    wait_for_text(&mut app, &ctx, "Edit command: alpha");
    replace_edit_text(&mut app, &ctx, "printf 'edited %s\\n' \"$1\"");
    let output = frame(&mut app, &ctx, Vec::new());
    click_button(&mut app, &ctx, &output, "Submit");
    wait_for_text(&mut app, &ctx, "choice with spaces");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    assert_eq!(captures.lock().unwrap().len(), 1);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert!(payload.argv.iter().any(|arg| arg == "choice with spaces"));
    assert_eq!(
        fs::read_to_string(&payload.argv[1]).unwrap(),
        "printf 'edited %s\\n' \"$1\"\n"
    );
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("alpha"), "{history}");
}

#[test]
fn visit_current_command_hands_exact_editor_file_and_line_to_terminal() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let project = temp.child("project with 'spaces'");
    project.create_dir_all().unwrap();
    let source = project.child("nixon.md");
    source
        .write_str("\n# `alpha`\n\n```bash\ntouch should-not-run\n```\n")
        .unwrap();
    let terminal = executable(&temp, "terminal with 'quotes'");
    let editor = executable(&temp, "editor with 'quotes'");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    env.cwd = project.path().to_path_buf();
    env.editor = Some(editor.to_string_lossy().into_owned());
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("alpha".to_owned())]);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::F2)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], terminal.to_string_lossy());
    assert_eq!(calls[0].argv[1], "-e");
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.argv,
        vec![
            editor.to_string_lossy().into_owned(),
            "+2".to_owned(),
            source.path().to_string_lossy().into_owned(),
        ]
    );
    assert_eq!(payload.cwd, None);
    assert!(!project.child("should-not-run").path().exists());
    assert!(!temp.child("state/nixon/history").path().exists());
}

#[test]
fn visit_errors_stay_visible_without_running_a_command() {
    for scenario in [
        "missing-location",
        "missing-editor",
        "missing-terminal",
        "spawn-error",
    ] {
        let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let terminal = executable(&temp, "terminal");
        let editor = executable(&temp, "editor");
        if scenario != "missing-terminal" {
            config.launcher.terminal = Some(vec![
                terminal.to_string_lossy().into_owned(),
                "-e".to_owned(),
            ]);
        }
        env.editor = Some(if scenario == "missing-editor" {
            temp.child("missing editor")
                .path()
                .to_string_lossy()
                .into_owned()
        } else {
            editor.to_string_lossy().into_owned()
        });
        let exe = temp.child("nixon").path().to_path_buf();
        env.exe = Some(exe.clone());
        let (runner, calls) = command_runner(
            config.launcher.terminal.clone(),
            exe,
            scenario == "spawn-error",
        );
        let mut app = PreviewApp::with_command_runner(
            config,
            dirs,
            env,
            RealRunner,
            super::SessionMediaTransport,
            runner,
        )
        .unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
        wait_for_text(&mut app, &ctx, "Select command");
        let name = if scenario == "missing-location" {
            "beta"
        } else {
            "alpha"
        };
        let _ = frame(&mut app, &ctx, vec![egui::Event::Text(name.to_owned())]);
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::F2)]);
        let expected = match scenario {
            "missing-location" => "Unable to find command location",
            "missing-editor" => "Editor executable",
            "missing-terminal" => "No terminal launcher is available",
            _ => "Could not launch terminal",
        };
        let output = wait_for_text(&mut app, &ctx, expected);
        assert!(!closes(&output));
        let calls = calls.lock().unwrap().clone();
        if scenario == "spawn-error" {
            assert_eq!(calls.len(), 1);
            assert!(!std::path::Path::new(calls[0].argv.last().unwrap()).exists());
        } else {
            assert!(calls.is_empty());
        }
        assert!(!temp.child("project/selected-marker").path().exists());
        assert!(!temp.child("state/nixon/history").path().exists());
    }
}

#[test]
fn default_command_uses_local_terminal_and_prepared_payload() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let global_terminal = executable(&temp, "global-terminal");
    let local_terminal = executable(&temp, "local terminal with 'quotes'");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    let local_config = LOCAL_COMMANDS.replacen(
        "ignore_case: false",
        &format!(
            "ignore_case: false\nlauncher:\n  terminal: [{}, '-e']",
            serde_json::to_string(&local_terminal.to_string_lossy()).unwrap()
        ),
        1,
    );
    temp.child("project/nixon.md")
        .write_str(&local_config)
        .unwrap();
    let exe = temp
        .child("nixon binary with 'quotes'")
        .path()
        .to_path_buf();
    env.exe = Some(exe.clone());
    let (command_runner, calls) =
        command_runner(config.launcher.terminal.clone(), exe.clone(), false);
    let mut app = PreviewApp::with_command_runner(
        config,
        dirs,
        env,
        RealRunner,
        super::SessionMediaTransport,
        command_runner,
    )
    .unwrap();
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv.len(), 6);
    assert_eq!(calls[0].argv[0], local_terminal.to_string_lossy());
    assert_eq!(calls[0].argv[1], "-e");
    assert_eq!(calls[0].argv[2], exe.to_string_lossy());
    assert_eq!(calls[0].argv[3], "internal");
    assert_eq!(calls[0].argv[4], "gui-exec");
    let payload = read_payload(std::path::Path::new(&calls[0].argv[5])).unwrap();
    assert_eq!(
        payload.cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    assert!(payload.argv.iter().any(|arg| arg == "bash"));
    assert!(payload.env.iter().any(|(name, value)| {
        name == "nixon_project_path" && value == &temp.child("project").path().display().to_string()
    }));
    assert!(payload.stdin.is_none());
    assert!(!temp.child("project/selected-marker").path().exists());
}

#[test]
fn command_option_confirm_continues_into_terminal_handoff() {
    let local = "# `alpha --fast`\n\n- `--fast`: off\n\n```bash\necho option\n```\n";
    let (temp, mut config, dirs, mut env) = fixture(local);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    let exe = temp.child("nixon").path().to_path_buf();
    env.exe = Some(exe.clone());
    let (runner, calls) = command_runner(config.launcher.terminal.clone(), exe, false);
    let mut app = PreviewApp::with_command_runner(
        config,
        dirs,
        env,
        RealRunner,
        super::SessionMediaTransport,
        runner,
    )
    .unwrap();
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Enter)]);
    wait_for_text(&mut app, &ctx, "Cancel");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Num1, egui::Modifiers::ALT)],
    );
    wait_for_text(&mut app, &ctx, "[x] --fast");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    assert!(
        !texts(&output).iter().any(|text| text == "Cancel"),
        "confirm screen stayed open after Enter: {:?}",
        texts(&output)
    );
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert!(payload.argv.iter().any(|arg| arg == "--fast"));
    assert!(
        payload
            .env
            .iter()
            .any(|(name, value)| name == "nixon_opt_fast" && value == "1")
    );
}

#[test]
fn nested_placeholder_pick_reaches_prepared_terminal_payload() {
    let (temp, mut app, calls, captures) = placeholder_fixture(false, false);
    let ctx = egui::Context::default();
    open_placeholder_pick(&mut app, &ctx);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let producer_calls = captures.lock().unwrap().clone();
    assert_eq!(producer_calls.len(), 1);
    assert_eq!(
        producer_calls[0].cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[3], "internal");
    assert_eq!(calls[0].argv[4], "gui-exec");
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    assert!(payload.argv.iter().any(|arg| arg == "choice with spaces"));
    assert!(!payload.argv.iter().any(|arg| arg == "quoted 'choice'"));
}

#[test]
fn canceling_nested_placeholder_pick_does_not_spawn_terminal() {
    let (_temp, mut app, calls, captures) = placeholder_fixture(false, false);
    let ctx = egui::Context::default();
    open_placeholder_pick(&mut app, &ctx);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
    assert!(!closes(&output));
    let output = wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    assert!(!closes(&output));
    assert!(texts(&output).iter().any(|text| text == "Commands"));
    assert_eq!(captures.lock().unwrap().len(), 1);
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn missing_terminal_and_failed_spawn_stay_visible_without_closing() {
    for spawn_error in [false, true] {
        let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let terminal = executable(&temp, "terminal");
        if spawn_error {
            config.launcher.terminal = Some(vec![
                terminal.to_string_lossy().into_owned(),
                "-e".to_owned(),
            ]);
        }
        let exe = temp.child("nixon").path().to_path_buf();
        env.exe = Some(exe.clone());
        let (runner, calls) = command_runner(config.launcher.terminal.clone(), exe, spawn_error);
        let mut app = PreviewApp::with_command_runner(
            config,
            dirs,
            env,
            RealRunner,
            super::SessionMediaTransport,
            runner,
        )
        .unwrap();
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
        wait_for_text(&mut app, &ctx, "Select command");
        let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
        assert!(!closes(&output));
        let expected = if spawn_error {
            "Could not launch terminal"
        } else {
            "No terminal launcher is available"
        };
        let output = wait_for_text(&mut app, &ctx, expected);
        assert!(!closes(&output));
        if spawn_error {
            let calls = calls.lock().unwrap().clone();
            assert_eq!(calls.len(), 1);
            assert!(!std::path::Path::new(calls[0].argv.last().unwrap()).exists());
        } else {
            assert!(calls.lock().unwrap().is_empty());
        }
    }
}

#[test]
fn background_command_keeps_detached_semantics_without_terminal() {
    let local = "# `alpha &`\n\n```bash\ntouch selected-marker\n```\n";
    let (temp, config, dirs, mut env) = fixture(local);
    let exe = temp.child("nixon").path().to_path_buf();
    env.exe = Some(exe.clone());
    let (runner, calls) = command_runner(None, exe, false);
    let mut app = PreviewApp::with_command_runner(
        config,
        dirs,
        env,
        RealRunner,
        super::SessionMediaTransport,
        runner,
    )
    .unwrap();
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert!(calls[0].argv.iter().any(|part| part == "bash"));
    assert!(!calls[0].argv.iter().any(|part| part == "gui-exec"));
    assert!(!temp.child("project/selected-marker").path().exists());
}

#[test]
fn canceling_command_pick_returns_to_the_menu_quietly() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let exe = temp.child("nixon").path().to_path_buf();
    env.exe = Some(exe.clone());
    let (runner, calls) = command_runner(None, exe, false);
    let mut app = PreviewApp::with_command_runner(
        config,
        dirs,
        env,
        RealRunner,
        super::SessionMediaTransport,
        runner,
    )
    .unwrap();
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    wait_for_text(&mut app, &ctx, "alpha");
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
    let output = wait_for_text(&mut app, &ctx, "GUI preview: choose an action.");
    let shown = texts(&output);
    assert!(shown.iter().any(|text| text == "Commands"));
    assert!(!shown.iter().any(|text| text.contains("Selection canceled")));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn discovery_error_is_visible_in_the_window() {
    let (temp, mut config, dirs, env) = fixture(LOCAL_COMMANDS);
    let (first, _) = discovered_projects(&temp, &mut config);
    fs::write(first.join("nixon.md"), "# `broken`\n\nno source block\n").unwrap();
    let mut app = PreviewApp::new(config, dirs, env).unwrap();
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    let output = wait_for_text(&mut app, &ctx, "Could not load commands:");
    assert!(texts(&output).iter().any(|text| text == "Projects"));
}

#[test]
fn history_picker_keeps_recent_order_duplicates_and_filters_query() {
    let (temp, mut config, dirs, mut env) = fixture("```yaml config\nhistory: true\n```\n");
    config.history = Some(false);
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    let other = temp.child("other").path().to_string_lossy().into_owned();
    write_history(
        &temp,
        &[
            history_entry(&cwd, &["run", "alpha"]),
            history_entry(&cwd, &["run", "alpha"]),
            history_entry(&other, &["run", "beta", "two words"]),
            history_entry(&cwd, &["run", "alpha"]),
        ],
    );
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
    let output = wait_for_text(&mut app, &ctx, "History (Enter replays");
    let rows: Vec<String> = texts(&output)
        .into_iter()
        .filter(|text| text.contains("nixon run "))
        .collect();
    assert_eq!(rows.len(), 3, "{rows:?}");
    assert_eq!(rows[0], rows[2], "separate duplicate entries were lost");
    assert!(rows[0].contains("~/project") && rows[0].contains("nixon run alpha"));
    assert!(rows[1].contains("~/other") && rows[1].contains("nixon run beta"));
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("beta".to_owned())]);
    let output = wait_for_text(&mut app, &ctx, "1/3");
    let filtered: Vec<String> = texts(&output)
        .into_iter()
        .filter(|text| text.contains("nixon run "))
        .collect();
    assert_eq!(filtered.len(), 1, "{filtered:?}");
    assert!(filtered[0].contains("nixon run beta"));
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::F1)]);
    let output = wait_for_text(&mut app, &ctx, "History command");
    assert!(
        texts(&output)
            .iter()
            .any(|text| text == "nixon run beta 'two words'")
    );
    let output = click_button(&mut app, &ctx, &output, "Copy");
    assert_eq!(copied_text(&output), Some("nixon run beta 'two words'"));
    click_button(&mut app, &ctx, &output, "Back");
    wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn history_alt_enter_shows_without_replay() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    write_history(&temp, &[history_entry(&cwd, &["run", "alpha"])]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
    wait_for_text(&mut app, &ctx, "History (Enter replays");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    let output = wait_for_text(&mut app, &ctx, "History command");
    assert!(texts(&output).iter().any(|text| text == "nixon run alpha"));
    assert!(calls.lock().unwrap().is_empty());
    assert_eq!(
        fs::read_to_string(temp.child("state/nixon/history").path())
            .unwrap()
            .lines()
            .count(),
        1
    );
}

#[test]
fn history_run_replays_quoted_argument_with_local_terminal_and_records_it() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let global_terminal = executable(&temp, "global-terminal");
    let local_terminal = executable(&temp, "local-terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".into(),
    ]);
    let local = format!(
        "```yaml config\nlauncher:\n  terminal: [{}, '-e']\n```\n\n# `alpha`\n\n```bash\nprintf '%s\\n' \"$@\"\n```\n",
        serde_json::to_string(&local_terminal.to_string_lossy()).unwrap()
    );
    temp.child("project/nixon.md").write_str(&local).unwrap();
    let cwd = temp
        .child("recorded elsewhere")
        .path()
        .to_string_lossy()
        .into_owned();
    let arg = "folder/a file 'quoted'.txt";
    write_history(&temp, &[history_entry(&cwd, &["run", "alpha", arg])]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    replay_first_history_entry(&mut app, &ctx);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], local_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    assert!(
        payload.argv.iter().any(|word| word == arg),
        "{:?}",
        payload.argv
    );
    let recorded =
        nixon::history::read_checked(temp.child("state/nixon/history").path(), None).unwrap();
    assert_eq!(recorded.len(), 2);
    assert_eq!(recorded[1].invocation, vec!["run", "alpha", arg]);
    assert_eq!(
        recorded[1].cwd,
        temp.child("project").path().to_string_lossy()
    );
}

#[test]
fn history_project_replay_uses_selected_projects_local_terminal() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let global_terminal = executable(&temp, "global-terminal");
    let project_terminal = executable(&temp, "project-terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".into(),
    ]);
    let target = temp.child("work with spaces");
    target.create_dir_all().unwrap();
    let local = format!(
        "```yaml config\nlauncher:\n  terminal: [{}, '-e']\n```\n\n# `jump`\n\n```bash\necho project\n```\n",
        serde_json::to_string(&project_terminal.to_string_lossy()).unwrap()
    );
    target.child("nixon.md").write_str(&local).unwrap();
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    let target_path = target.path().to_string_lossy().into_owned();
    write_history(
        &temp,
        &[history_entry(&cwd, &["project", &target_path, "jump"])],
    );
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    replay_first_history_entry(&mut app, &ctx);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], project_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(target.path().to_path_buf()));
}

#[test]
fn history_insert_replays_show_source_with_copy_for_run_and_project_aliases() {
    for project_form in [false, true] {
        let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let cwd = temp.child("project").path().to_string_lossy().into_owned();
        let words = if project_form {
            vec!["project", cwd.as_str(), "-i", "alpha"]
        } else {
            vec!["run", "--insert", "alpha"]
        };
        write_history(&temp, &[history_entry(&cwd, &words)]);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, calls) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        choose_first_history_entry(&mut app, &ctx);
        let output = wait_for_text(&mut app, &ctx, "Command source: alpha");
        assert!(
            texts(&output)
                .iter()
                .any(|text| text == "touch selected-marker\n")
        );
        let output = click_button(&mut app, &ctx, &output, "Copy");
        assert_eq!(copied_text(&output), Some("touch selected-marker\n"));
        assert!(calls.lock().unwrap().is_empty());
    }
}

#[test]
fn history_project_select_and_inspect_show_paths_and_details() {
    for inspect_project in [false, true] {
        let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let cwd = temp.child("project").path().to_string_lossy().into_owned();
        let flag = if inspect_project { "-I" } else { "--select" };
        write_history(&temp, &[history_entry(&cwd, &["project", flag, &cwd])]);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, calls) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        choose_first_history_entry(&mut app, &ctx);
        let title = if inspect_project {
            "Project inspection"
        } else {
            "Selected project paths"
        };
        let output = wait_for_text(&mut app, &ctx, title);
        let body = texts(&output)
            .into_iter()
            .find(|text| text.contains(&cwd))
            .expect("project detail contains path");
        if !inspect_project {
            assert_eq!(body, cwd);
        }
        let output = click_button(&mut app, &ctx, &output, "Copy");
        assert_eq!(copied_text(&output), Some(body.as_str()));
        assert!(calls.lock().unwrap().is_empty());
    }
}

#[test]
fn history_project_select_can_return_multiple_paths() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, second) = discovered_projects(&temp, &mut config);
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    write_history(&temp, &[history_entry(&cwd, &["project", "-s"])]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    choose_first_history_entry(&mut app, &ctx);
    wait_for_text(&mut app, &ctx, "work-one");
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Tab)]);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Tab)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Tab)]);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Enter)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    let output = wait_for_text(&mut app, &ctx, "Selected project paths");
    let expected = format!("{}\n{}", first.display(), second.display());
    assert!(texts(&output).iter().any(|text| text == &expected));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn history_run_select_uses_gui_value_picker_and_cancellation() {
    for canceled in [false, true] {
        let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let cwd = temp.child("project").path().to_string_lossy().into_owned();
        write_history(&temp, &[history_entry(&cwd, &["run", "-s", "alpha"])]);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let exe = env.exe.clone().unwrap();
        let (runner, calls) =
            command_runner_with_output(None, exe, false, Some(b"first\nsecond\n".to_vec()));
        let mut app = PreviewApp::with_command_runner(
            config,
            dirs,
            env,
            RealRunner,
            super::SessionMediaTransport,
            runner,
        )
        .unwrap();
        let ctx = egui::Context::default();
        choose_first_history_entry(&mut app, &ctx);
        wait_for_text(&mut app, &ctx, "first");
        let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Enter)]);
        let action_key = if canceled {
            egui::Key::Escape
        } else {
            egui::Key::Enter
        };
        let _ = frame(&mut app, &ctx, vec![key(action_key)]);
        let output = wait_for_text(
            &mut app,
            &ctx,
            if canceled {
                super::PREVIEW_STATUS
            } else {
                "Selected values: alpha"
            },
        );
        if !canceled {
            assert!(texts(&output).iter().any(|text| text == "first"));
        }
        assert!(calls.lock().unwrap().is_empty());
    }
}

#[test]
fn history_run_select_errors_stay_visible() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    write_history(&temp, &[history_entry(&cwd, &["run", "--select", "alpha"])]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    choose_first_history_entry(&mut app, &ctx);
    let output = wait_for_text(&mut app, &ctx, "Could not replay history:");
    assert!(!closes(&output));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn history_data_replay_keeps_stdout_empty() {
    const SENTINEL: &str = "NIXON_GUI_DETAIL_OUTPUT_9D31";
    const CHILD: &str = "NIXON_GUI_STDOUT_TEST_CHILD";
    if std::env::var_os(CHILD).is_some() {
        let local = format!("# `alpha`\n\n```bash\n{SENTINEL}\n```\n");
        let (temp, config, dirs, mut env) = fixture(&local);
        let cwd = temp.child("project").path().to_string_lossy().into_owned();
        write_history(&temp, &[history_entry(&cwd, &["run", "--insert", "alpha"])]);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, _) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        choose_first_history_entry(&mut app, &ctx);
        let output = wait_for_text(&mut app, &ctx, "Command source: alpha");
        assert!(texts(&output).iter().any(|text| text.contains(SENTINEL)));
        return;
    }
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .args([
            "--exact",
            "gui::tests::history_data_replay_keeps_stdout_empty",
            "--nocapture",
        ])
        .env(CHILD, "1")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !String::from_utf8_lossy(&output.stdout).contains(SENTINEL),
        "GUI data leaked to stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
}

#[test]
fn history_replay_keeps_background_command_detached() {
    let (temp, config, dirs, mut env) =
        fixture("# `alpha &`\n\n```bash\ntouch selected-marker\n```\n");
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    write_history(&temp, &[history_entry(&cwd, &["run", "alpha"])]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    replay_first_history_entry(&mut app, &ctx);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert!(calls[0].argv.iter().any(|word| word == "bash"));
    assert!(!calls[0].argv.iter().any(|word| word == "gui-exec"));
}

#[test]
fn history_replay_uses_gui_placeholder_picker() {
    let (temp, mut app, calls, captures) = placeholder_fixture(false, false);
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    write_history(&temp, &[history_entry(&cwd, &["run", "alpha"])]);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
    wait_for_text(&mut app, &ctx, "History (Enter replays");
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Enter)]);
    wait_for_text(&mut app, &ctx, "choice with spaces");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    assert_eq!(captures.lock().unwrap().len(), 1);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert!(payload.argv.iter().any(|word| word == "choice with spaces"));
}

#[test]
fn history_eval_replay_uses_explicit_project_and_quoted_source() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let global_terminal = executable(&temp, "global-terminal");
    let project_terminal = executable(&temp, "project-terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".into(),
    ]);
    let target = temp.child("eval project");
    target.create_dir_all().unwrap();
    let local = format!(
        "```yaml config\nlauncher:\n  terminal: [{}, '-e']\n```\n",
        serde_json::to_string(&project_terminal.to_string_lossy()).unwrap()
    );
    target.child("nixon.md").write_str(&local).unwrap();
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    let target_path = target.path().to_string_lossy().into_owned();
    let source = "printf '%s' 'a quoted word'";
    write_history(
        &temp,
        &[history_entry(
            &cwd,
            &[
                "eval",
                &format!("--project={target_path}"),
                "-l",
                "bash",
                source,
            ],
        )],
    );
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    replay_first_history_entry(&mut app, &ctx);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], project_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(target.path().to_path_buf()));
    assert_eq!(fs::read_to_string(&payload.argv[1]).unwrap(), source);
    let recorded =
        nixon::history::read_checked(temp.child("state/nixon/history").path(), None).unwrap();
    assert_eq!(recorded.len(), 2);
    assert_eq!(recorded[1].invocation[0], "eval");
}

#[test]
fn history_replay_rejects_recursive_malformed_and_nonexecution_entries() {
    for words in [
        vec!["history"],
        vec!["eval", "--not-a-real-flag"],
        vec!["run", "--list"],
        vec!["gc"],
        vec!["alpha"],
        vec!["eval"],
        vec!["eval", "--file", "missing", "${unterminated"],
    ] {
        let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let cwd = temp.child("project").path().to_string_lossy().into_owned();
        write_history(&temp, &[history_entry(&cwd, &words)]);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, calls) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
        wait_for_text(&mut app, &ctx, "History (Enter replays");
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
        let output = wait_for_text(&mut app, &ctx, "Could not replay history:");
        assert!(!closes(&output), "{words:?}");
        assert!(calls.lock().unwrap().is_empty(), "{words:?}");
        assert_eq!(
            nixon::history::read_checked(temp.child("state/nixon/history").path(), None)
                .unwrap()
                .len(),
            1
        );
    }
}

#[test]
fn history_empty_and_cancel_return_to_menu() {
    for scenario in ["empty", "cancel"] {
        let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        if scenario == "cancel" {
            let cwd = temp.child("project").path().to_string_lossy().into_owned();
            write_history(&temp, &[history_entry(&cwd, &["run", "alpha"])]);
        }
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, calls) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
        if scenario == "cancel" {
            wait_for_text(&mut app, &ctx, "History (Enter replays");
            let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
        }
        let output = wait_for_idle(&mut app, &ctx);
        let shown = texts(&output);
        assert!(shown.iter().any(|text| text == "History"), "{shown:?}");
        assert!(shown.iter().any(|text| text == super::PREVIEW_STATUS));
        assert!(calls.lock().unwrap().is_empty());
    }
}

#[test]
fn history_disabled_in_local_config_is_visible() {
    let (temp, config, dirs, mut env) = fixture("```yaml config\nhistory: false\n```\n");
    let cwd = temp.child("project").path().to_string_lossy().into_owned();
    write_history(&temp, &[history_entry(&cwd, &["run", "alpha"])]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
    let output = wait_for_text(&mut app, &ctx, "history is disabled in the configuration");
    assert!(!closes(&output));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn history_read_error_is_visible_in_gui() {
    let (temp, config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    temp.child("state/nixon/history").create_dir_all().unwrap();
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::H)]);
    let output = wait_for_text(&mut app, &ctx, "Could not load history from");
    assert!(
        texts(&output)
            .iter()
            .any(|text| text.contains("state/nixon/history"))
    );
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn startup_local_items_replace_default_hotkeys_and_run_nested_command() {
    let local = "```yaml config\nlauncher:\n  items:\n    - key: X\n      label: Pick commands\n      action: commands\n    - key: 'N'\n      label: Tools\n      items:\n        - key: J\n          label: Jump\n          action: { command: alpha }\n```\n\n# `alpha`\n\n```bash\ntouch selected-marker\n```\n";
    let (temp, mut config, dirs, mut env) = fixture(local);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![terminal.to_string_lossy().into_owned(), "-e".into()]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = frame(&mut app, &ctx, Vec::new());
    let shown = texts(&output);
    assert!(shown.iter().any(|text| text == "Pick commands"));
    assert!(shown.iter().any(|text| text == "Tools"));
    assert!(!shown.iter().any(|text| text == "Commands"));
    let output = frame(&mut app, &ctx, vec![key(egui::Key::C)]);
    assert!(texts(&output).iter().any(|text| text == "Pick commands"));
    assert!(calls.lock().unwrap().is_empty());
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::C)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::X)]);
    wait_for_text(&mut app, &ctx, "Select command");
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::X)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
    wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    assert!(!app.busy);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Escape)]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::N)]);
    wait_for_text(&mut app, &ctx, "Jump");
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::N)]);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::J)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    assert!(payload.argv.iter().any(|arg| arg == "bash"));
}

#[test]
fn startup_local_launcher_merges_fields_and_browser_search() {
    let local = "```yaml config\nlauncher:\n  search_url: https://local.example/?q={query}\n```\n";
    let (temp, mut config, dirs, env) = fixture(local);
    let terminal = executable(&temp, "global-terminal");
    config.launcher.terminal = Some(vec![terminal.to_string_lossy().into_owned(), "-e".into()]);
    let launcher = super::startup_launcher(&config, &env).unwrap();
    assert_eq!(launcher.items, config.launcher.items);
    assert_eq!(launcher.terminal, config.launcher.terminal);
    assert_eq!(
        launcher.search_url.as_deref(),
        Some("https://local.example/?q={query}")
    );
    let (runner, calls) = browser_runner(Ok(0));
    let mut app = PreviewApp::with_browser_runner(config, dirs, env, runner).unwrap();
    let ctx = egui::Context::default();
    let shown = texts(&frame(&mut app, &ctx, Vec::new()));
    assert!(shown.iter().any(|text| text == "Commands"));
    open_browser_input(&mut app, &ctx);
    let _ = frame(
        &mut app,
        &ctx,
        vec![egui::Event::Text("cat tea".to_owned())],
    );
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    assert_eq!(
        calls.lock().unwrap()[0].argv[1],
        "https://local.example/?q=cat%20tea"
    );
}

#[test]
fn startup_local_terminal_does_not_leak_to_selected_project() {
    let local = "```yaml config\nlauncher:\n  terminal: [STARTUP_TERMINAL, '-e']\n  items:\n    - key: Q\n      label: Jump to work\n      action: { command: jump, project: work-one }\n```\n";
    let (temp, mut config, dirs, mut env) = fixture(local);
    let (first, _) = discovered_projects(&temp, &mut config);
    temp.child("project/.git").create_dir_all().unwrap();
    let global_terminal = executable(&temp, "global-terminal");
    let startup_terminal = executable(&temp, "startup-terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".into(),
    ]);
    temp.child("project/nixon.md")
        .write_str(&local.replace("STARTUP_TERMINAL", &startup_terminal.to_string_lossy()))
        .unwrap();
    let launcher = super::startup_launcher(&config, &env).unwrap();
    assert!(
        launcher
            .items
            .as_ref()
            .unwrap()
            .iter()
            .any(|item| matches!(item, MenuItem::Action { label, .. } if label == "Jump to work"))
    );
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], global_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(first));
}

#[test]
fn invalid_startup_local_launcher_reports_file_and_line_before_opening() {
    let (temp, config, dirs, env) = fixture(
        "```yaml config\nlauncher:\n  items:\n    - key: C\n      label: One\n      action: commands\n    - key: C\n      label: Two\n      action: projects\n```\n",
    );
    match PreviewApp::new(config, dirs, env) {
        Err(nixon::error::NixonError::Markdown(error)) => {
            assert_eq!(
                error.file,
                temp.child("project/nixon.md").path().to_string_lossy()
            );
            assert!(error.line.is_some());
            assert!(
                error
                    .message
                    .contains("launcher.items: duplicate sibling key 'C'")
            );
        }
        Err(other) => panic!("unexpected error: {other}"),
        Ok(_) => panic!("invalid local launcher opened a window"),
    }
}

#[test]
fn missing_startup_local_file_uses_default_menu() {
    let (temp, config, dirs, env) = fixture(LOCAL_COMMANDS);
    fs::remove_file(temp.child("project/nixon.md").path()).unwrap();
    assert_eq!(
        super::startup_launcher(&config, &env).unwrap(),
        config.launcher
    );
    let mut app = PreviewApp::new(config, dirs, env).unwrap();
    let ctx = egui::Context::default();
    let shown = texts(&frame(&mut app, &ctx, Vec::new()));
    assert!(shown.iter().any(|text| text == "Commands"));
    assert!(shown.iter().any(|text| text == "Browser"));
}

#[test]
fn quick_command_uses_current_project_local_terminal_and_history() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let global_terminal = executable(&temp, "global-terminal");
    let local_terminal = executable(&temp, "local terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    add_quick_action(&mut config, "alpha", None);
    let local_config = LOCAL_COMMANDS.replacen(
        "ignore_case: false",
        &format!(
            "ignore_case: false\nlauncher:\n  terminal: [{}, '-e']",
            serde_json::to_string(&local_terminal.to_string_lossy()).unwrap()
        ),
        1,
    );
    temp.child("project/nixon.md")
        .write_str(&local_config)
        .unwrap();
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], local_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.cwd,
        Some(temp.child("project").path().to_path_buf())
    );
    assert!(payload.argv.iter().any(|arg| arg == "bash"));
    assert!(!temp.child("project/selected-marker").path().exists());
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("nixon run alpha"), "{history}");
}

#[test]
fn quick_command_accepts_explicit_project_path() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let target = temp.child("another project with 'quotes'");
    target.create_dir_all().unwrap();
    target
        .child("nixon.md")
        .write_str("# `jump`\n\n```bash\ntouch jump-marker\n```\n")
        .unwrap();
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    add_quick_action(
        &mut config,
        "jump",
        Some(target.path().to_string_lossy().into_owned()),
    );
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(target.path().to_path_buf()));
    assert!(!target.child("jump-marker").path().exists());
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("nixon project"), "{history}");
    assert!(history.contains("jump"), "{history}");
}

#[test]
fn quick_command_resolves_discovered_project_query() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _second) = discovered_projects(&temp, &mut config);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    add_quick_action(&mut config, "jump", Some("work-one".to_owned()));
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(first));
}

#[test]
fn quick_command_errors_for_unknown_command_or_project() {
    for missing_project in [false, true] {
        let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        let project = missing_project.then(|| {
            temp.child("does-not-exist")
                .path()
                .to_string_lossy()
                .into_owned()
        });
        add_quick_action(&mut config, "missing-command", project);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, calls) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        let output = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
        assert!(!closes(&output));
        let expected = if missing_project {
            "no such project"
        } else {
            "Invalid argument: missing-command"
        };
        let output = wait_for_text(&mut app, &ctx, expected);
        assert!(!closes(&output));
        assert!(calls.lock().unwrap().is_empty());
    }
}

#[test]
fn canceling_quick_project_pick_returns_to_menu_without_running() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    discovered_projects(&temp, &mut config);
    add_quick_action(&mut config, "jump", Some("work".to_owned()));
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    wait_for_text(&mut app, &ctx, "Select project");
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
    let output = wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    assert!(!closes(&output));
    assert!(texts(&output).iter().any(|text| text == "Quick command"));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn inspecting_quick_project_pick_does_not_run_command() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    discovered_projects(&temp, &mut config);
    add_quick_action(&mut config, "jump", Some("work".to_owned()));
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    wait_for_text(&mut app, &ctx, "Select project");
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::F1)]);
    let output = wait_for_text(&mut app, &ctx, "Project: work-one");
    assert!(!closes(&output));
    assert!(copied_text(&copy_detail(&mut app, &ctx)).is_some());
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn quick_background_command_runs_detached_without_terminal() {
    let local = "# `alpha &`\n\n```bash\ntouch selected-marker\n```\n";
    let (temp, mut config, dirs, mut env) = fixture(local);
    add_quick_action(&mut config, "alpha", None);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert!(calls[0].argv.iter().any(|part| part == "bash"));
    assert!(!calls[0].argv.iter().any(|part| part == "gui-exec"));
}

#[test]
fn quick_command_prompts_for_options() {
    let local = "# `alpha --fast`\n\n- `--fast`: off\n\n```bash\necho option\n```\n";
    let (temp, mut config, dirs, mut env) = fixture(local);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    add_quick_action(&mut config, "alpha", None);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    wait_for_text(&mut app, &ctx, "Cancel");
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Num1, egui::Modifiers::ALT)],
    );
    wait_for_text(&mut app, &ctx, "[x] --fast");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert!(payload.argv.iter().any(|arg| arg == "--fast"));
}

#[test]
fn quick_command_resolves_nested_placeholder() {
    let (_temp, mut app, calls, captures) = placeholder_fixture(true, false);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Q)]);
    wait_for_text(&mut app, &ctx, "choice with spaces");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    assert_eq!(captures.lock().unwrap().len(), 1);
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert!(payload.argv.iter().any(|arg| arg == "choice with spaces"));
}

fn open_projects(app: &mut PreviewApp, ctx: &egui::Context) -> egui::FullOutput {
    let _ = frame(app, ctx, vec![key(egui::Key::P)]);
    wait_for_text(app, ctx, "Select project")
}

fn choose_first_project(app: &mut PreviewApp, ctx: &egui::Context) -> egui::FullOutput {
    let _ = frame(app, ctx, vec![key(egui::Key::Enter)]);
    let _ = frame(app, ctx, vec![key_release(egui::Key::Enter)]);
    wait_for_text(app, ctx, "Select command")
}

#[test]
fn projects_pick_then_command_runs_in_chosen_project() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _second) = discovered_projects(&temp, &mut config);
    let terminal = executable(&temp, "terminal");
    config.launcher.terminal = Some(vec![
        terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let output = open_projects(&mut app, &ctx);
    let rows = texts(&output);
    assert!(
        rows.iter().any(|row| row == "~/projects/work-one"),
        "{rows:?}"
    );
    assert!(
        rows.iter().any(|row| row == "~/projects/work-two"),
        "{rows:?}"
    );
    choose_first_project(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("jump".to_owned())]);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(first));
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("nixon project"), "{history}");
    assert!(history.contains("jump"), "{history}");
}

#[test]
fn projects_pick_command_and_placeholder_before_running() {
    let (temp, mut app, calls, captures) = placeholder_fixture(false, true);
    let first = temp.child("projects/work-one").path().to_path_buf();
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    choose_first_project(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("alpha".to_owned())]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    let _ = frame(&mut app, &ctx, vec![key_release(egui::Key::Enter)]);
    wait_for_text(&mut app, &ctx, "choice with spaces");
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let producer_calls = captures.lock().unwrap().clone();
    assert_eq!(producer_calls.len(), 1);
    assert_eq!(producer_calls[0].cwd, Some(first.clone()));
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(first.clone()));
    assert!(payload.argv.iter().any(|arg| arg == "choice with spaces"));
    assert!(payload.argv.iter().any(|arg| arg == "--fast"));
    assert!(
        payload
            .env
            .iter()
            .any(|(name, value)| { name == "nixon_opt_fast" && value == "1" })
    );
    assert!(payload.env.iter().any(|(name, value)| {
        name == "nixon_project_path" && value == &first.display().to_string()
    }));
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("nixon project"), "{history}");
    assert!(history.contains("alpha"), "{history}");
}

#[test]
fn inspecting_project_shows_details_without_running() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _) = discovered_projects(&temp, &mut config);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::F1)]);
    let output = wait_for_text(&mut app, &ctx, "Project: work-one");
    assert!(texts(&output).iter().any(|row| row.contains("Types: git")));
    let expected = format!("Name: work-one\nPath: {}\nTypes: git\n", first.display());
    assert_eq!(
        copied_text(&copy_detail(&mut app, &ctx)),
        Some(expected.as_str())
    );
    assert!(!closes(&output));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn project_command_show_displays_exact_source_without_running() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    discovered_projects(&temp, &mut config);
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    choose_first_project(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("jump".to_owned())]);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::F1)]);
    let output = wait_for_text(&mut app, &ctx, "Command: jump");
    assert!(!closes(&output));
    assert_eq!(
        copied_text(&copy_detail(&mut app, &ctx)),
        Some("touch jump-marker\n")
    );
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn visit_project_command_uses_local_terminal_and_recorded_location() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _) = discovered_projects(&temp, &mut config);
    let global_terminal = executable(&temp, "global-terminal");
    let local_terminal = executable(&temp, "local terminal");
    let editor = executable(&temp, "editor");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    fs::write(
            first.join("nixon.md"),
            format!(
                "```yaml config\nlauncher:\n  terminal: [{}, '-e']\n```\n\n# `jump`\n\n```bash\ntouch should-not-run\n```\n",
                serde_json::to_string(&local_terminal.to_string_lossy()).unwrap()
            ),
        ).unwrap();
    env.editor = Some(editor.to_string_lossy().into_owned());
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    choose_first_project(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("jump".to_owned())]);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::F2)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], local_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(
        payload.argv,
        vec![
            editor.to_string_lossy().into_owned(),
            "+6".to_owned(),
            first.join("nixon.md").to_string_lossy().into_owned(),
        ]
    );
    assert!(!first.join("should-not-run").exists());
    assert!(!temp.child("state/nixon/history").path().exists());
}

#[test]
fn editing_project_command_keeps_local_terminal_and_python_language() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _) = discovered_projects(&temp, &mut config);
    let global_terminal = executable(&temp, "global-terminal");
    let local_terminal = executable(&temp, "local terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    fs::write(
            first.join("nixon.md"),
            format!(
                "```yaml config\nlauncher:\n  terminal: [{}, '-e']\n```\n\n# `jump`\n\n```python\nprint('original')\n```\n",
                serde_json::to_string(&local_terminal.to_string_lossy()).unwrap()
            ),
        ).unwrap();
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    choose_first_project(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("jump".to_owned())]);
    let _ = frame(
        &mut app,
        &ctx,
        vec![key_with_modifiers(egui::Key::Enter, egui::Modifiers::ALT)],
    );
    wait_for_text(&mut app, &ctx, "Edit command: jump");
    replace_edit_text(&mut app, &ctx, "print('edited')");
    let output = frame(&mut app, &ctx, Vec::new());
    let output = click_button(&mut app, &ctx, &output, "Submit");
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], local_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(first));
    assert_eq!(payload.argv[0], "python3");
    assert_eq!(
        fs::read_to_string(&payload.argv[1]).unwrap(),
        "print('edited')\n"
    );
    let history = fs::read_to_string(temp.child("state/nixon/history").path()).unwrap();
    assert!(history.contains("nixon project"), "{history}");
    assert!(history.contains("jump"), "{history}");
}

#[test]
fn canceling_either_projects_pick_returns_to_menu() {
    for at_command in [false, true] {
        let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
        discovered_projects(&temp, &mut config);
        env.exe = Some(temp.child("nixon").path().to_path_buf());
        let (mut app, calls) = preview_with_fake_command(config, dirs, env);
        let ctx = egui::Context::default();
        open_projects(&mut app, &ctx);
        if at_command {
            choose_first_project(&mut app, &ctx);
        }
        let _ = frame(&mut app, &ctx, vec![key(egui::Key::Escape)]);
        let output = wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
        assert!(!closes(&output));
        assert!(calls.lock().unwrap().is_empty());
    }
}

#[test]
fn no_discovered_projects_returns_to_menu_quietly() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    config.project_dirs.clear();
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::P)]);
    let output = wait_for_text(&mut app, &ctx, super::PREVIEW_STATUS);
    assert!(!closes(&output));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn project_without_visible_commands_returns_without_running() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _) = discovered_projects(&temp, &mut config);
    config.commands.clear();
    fs::write(first.join("nixon.md"), "").unwrap();
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    let _ = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    let output = wait_for_text(&mut app, &ctx, "No commands available.");
    assert!(!closes(&output));
    assert!(calls.lock().unwrap().is_empty());
}

#[test]
fn projects_use_chosen_local_config_and_hide_private_commands() {
    let (temp, mut config, dirs, mut env) = fixture(LOCAL_COMMANDS);
    let (first, _) = discovered_projects(&temp, &mut config);
    let global_terminal = executable(&temp, "global-terminal");
    let local_terminal = executable(&temp, "local terminal");
    config.launcher.terminal = Some(vec![
        global_terminal.to_string_lossy().into_owned(),
        "-e".to_owned(),
    ]);
    fs::write(
            first.join("nixon.md"),
            format!(
                "```yaml config\nlauncher:\n  terminal: [{}, '-e']\n```\n\n# `jump`\n\n```bash\necho local\n```\n\n# `_secret`\n\n```bash\necho secret\n```\n",
                serde_json::to_string(&local_terminal.to_string_lossy()).unwrap()
            ),
        ).unwrap();
    env.exe = Some(temp.child("nixon").path().to_path_buf());
    let (mut app, calls) = preview_with_fake_command(config, dirs, env);
    let ctx = egui::Context::default();
    open_projects(&mut app, &ctx);
    let output = choose_first_project(&mut app, &ctx);
    assert!(!texts(&output).iter().any(|row| row.contains("_secret")));
    let _ = frame(&mut app, &ctx, vec![egui::Event::Text("jump".to_owned())]);
    let output = frame(&mut app, &ctx, vec![key(egui::Key::Enter)]);
    if !closes(&output) {
        wait_for_close(&mut app, &ctx);
    }
    let calls = calls.lock().unwrap().clone();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].argv[0], local_terminal.to_string_lossy());
    let payload = read_payload(std::path::Path::new(calls[0].argv.last().unwrap())).unwrap();
    assert_eq!(payload.cwd, Some(first));
}
