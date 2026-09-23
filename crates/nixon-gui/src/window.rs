//! A compact egui window for configured launcher menus.

use std::sync::mpsc::Sender;

use eframe::egui;
use nixon::config::launcher::{LauncherAction, LauncherConfig, MenuItem, MenuKey};

use crate::browser_view::{BrowserInputOutcome, BrowserInputView};
use crate::detail_view::{DetailOutcome, DetailView};
use crate::edit_view::{EditOutcome, EditView};
use crate::menu::{InputFocus, MenuInput, MenuOutcome, MenuState};
use crate::picker::GuiPickerRequests;
use crate::picker_view::PickerView;

/// The menu view and its action output channel.
pub struct MenuWindow {
    menu: MenuState,
    actions: Sender<LauncherAction>,
    focus_first_row: bool,
    picker: Option<PickerView>,
    browser: Option<BrowserInputView>,
    browser_event: Option<BrowserInputEvent>,
    detail: Option<DetailView>,
    detail_closed: bool,
    edit: Option<EditView>,
    edit_event: Option<EditEvent>,
}

/// The browser input screen's result.
#[derive(Debug, Eq, PartialEq)]
pub enum BrowserInputEvent {
    /// User submitted nonblank text.
    Submitted(String),
    /// User returned to the menu.
    Canceled,
}

/// An in-window command editor submission or cancellation.
#[derive(Debug, Eq, PartialEq)]
pub enum EditEvent {
    /// User submitted nonempty source text.
    Submitted(String),
    /// User returned to the menu without running anything.
    Canceled,
}

impl MenuWindow {
    /// Builds a window from a resolved launcher config.
    #[must_use]
    pub fn new(config: &LauncherConfig, actions: Sender<LauncherAction>) -> Option<Self> {
        Some(Self {
            menu: MenuState::new(config)?,
            actions,
            focus_first_row: true,
            picker: None,
            browser: None,
            browser_event: None,
            detail: None,
            detail_closed: false,
            edit: None,
            edit_event: None,
        })
    }

    /// Attaches the UI side of a worker's picker bridge.
    pub fn attach_picker(&mut self, requests: GuiPickerRequests) {
        self.picker = Some(PickerView::new(requests));
    }

    /// Opens the browser input screen after its menu action is selected.
    pub fn open_browser_input(&mut self) {
        self.browser = Some(BrowserInputView::new());
        self.browser_event = None;
    }

    /// Takes a browser submission or cancellation from the input screen.
    pub const fn take_browser_event(&mut self) -> Option<BrowserInputEvent> {
        self.browser_event.take()
    }

    /// Opens a read-only detail screen over the menu.
    pub fn open_detail(&mut self, title: String, body: String) {
        self.detail = Some(DetailView::new(title, body));
        self.detail_closed = false;
    }

    /// Whether the user returned from the detail screen since the last check.
    pub const fn take_detail_closed(&mut self) -> bool {
        std::mem::replace(&mut self.detail_closed, false)
    }

    /// Opens a multiline editor initialized with the selected source.
    pub fn open_edit(&mut self, title: String, source: String) {
        self.edit = Some(EditView::new(title, source));
        self.edit_event = None;
    }

    /// Takes a submitted edit or cancellation from the editor.
    pub const fn take_edit_event(&mut self) -> Option<EditEvent> {
        self.edit_event.take()
    }

    /// The menu currently shown by the window.
    #[must_use]
    pub const fn menu(&self) -> &MenuState {
        &self.menu
    }

    /// Handles one egui frame without running the selected action.
    pub fn show(&mut self, ctx: &egui::Context) {
        if let Some(edit) = self.edit.as_mut() {
            match edit.show(ctx) {
                EditOutcome::Pending => {}
                EditOutcome::Canceled => {
                    self.edit = None;
                    self.edit_event = Some(EditEvent::Canceled);
                    self.focus_first_row = true;
                    ctx.request_repaint();
                }
                EditOutcome::Submitted(source) => {
                    self.edit = None;
                    self.edit_event = Some(EditEvent::Submitted(source));
                    self.focus_first_row = true;
                    ctx.request_repaint();
                }
            }
            return;
        }
        if let Some(detail) = self.detail.as_mut() {
            if matches!(detail.show(ctx), DetailOutcome::Back) {
                self.detail = None;
                self.detail_closed = true;
                self.focus_first_row = true;
                ctx.request_repaint();
            }
            return;
        }
        if let Some(browser) = self.browser.as_mut() {
            match browser.show(ctx) {
                BrowserInputOutcome::Pending => {}
                BrowserInputOutcome::Cancel => {
                    self.browser_event = Some(BrowserInputEvent::Canceled);
                    self.browser = None;
                    self.focus_first_row = true;
                    ctx.request_repaint();
                }
                BrowserInputOutcome::Submit(input) => {
                    self.browser_event = Some(BrowserInputEvent::Submitted(input));
                    self.browser = None;
                    self.focus_first_row = true;
                    ctx.request_repaint();
                }
            }
            return;
        }
        if self.picker.as_mut().is_some_and(|picker| picker.show(ctx)) {
            self.focus_first_row = true;
            return;
        }

        self.show_menu(ctx);
    }

    fn show_menu(&mut self, ctx: &egui::Context) {
        let inputs = ctx.input(|input| {
            input
                .events
                .iter()
                .filter_map(menu_input)
                .collect::<Vec<_>>()
        });
        for input in inputs {
            let outcome = self.menu.handle(input, InputFocus::Menu);
            let stop = matches!(outcome, MenuOutcome::Action(_) | MenuOutcome::Close);
            self.apply(ctx, outcome);
            if stop {
                break;
            }
        }

        let mut clicked = None;
        let focus_first_row = self.focus_first_row;
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading(self.menu.current_label().unwrap_or("Nixon"));
            ui.label(if self.menu.depth() == 0 {
                "Backspace or Esc closes · Ctrl-C closes"
            } else {
                "Backspace or Esc goes back · Ctrl-C closes"
            });
            ui.add_space(12.0);

            egui::ScrollArea::vertical().show(ui, |ui| {
                for (index, item) in self.menu.items().iter().enumerate() {
                    let response = menu_row(ui, item);
                    if index == 0 && focus_first_row {
                        response.request_focus();
                    }
                    if response.clicked_by(egui::PointerButton::Primary)
                        && ui.input(|input| input.pointer.any_click())
                    {
                        clicked = Some(index);
                    }
                    ui.add_space(4.0);
                }
            });
        });
        self.focus_first_row = false;

        if let Some(index) = clicked
            && let Some(key) = self
                .menu
                .items()
                .get(index)
                .map(|item| item.key().normalized())
        {
            let outcome = self
                .menu
                .handle(MenuInput::Character(key), InputFocus::Menu);
            self.apply(ctx, outcome);
        }
    }

    fn apply(&mut self, ctx: &egui::Context, outcome: MenuOutcome) {
        match outcome {
            MenuOutcome::Action(action) => {
                let _ = self.actions.send(action);
            }
            MenuOutcome::Close => ctx.send_viewport_cmd(egui::ViewportCommand::Close),
            MenuOutcome::EnteredSubmenu | MenuOutcome::Back => {
                self.focus_first_row = true;
                ctx.request_repaint();
            }
            MenuOutcome::Ignored | MenuOutcome::LeaveTextInput => {}
        }
    }
}

impl eframe::App for MenuWindow {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.show(ctx);
    }
}

/// Default viewport settings for the menu window.
#[must_use]
pub fn native_options() -> eframe::NativeOptions {
    eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([480.0, 360.0])
            .with_min_inner_size([360.0, 260.0])
            .with_window_level(egui::WindowLevel::AlwaysOnTop),
        ..eframe::NativeOptions::default()
    }
}

fn menu_input(event: &egui::Event) -> Option<MenuInput> {
    let egui::Event::Key {
        key,
        pressed: true,
        repeat: false,
        modifiers,
        ..
    } = event
    else {
        return None;
    };

    if modifiers.ctrl {
        return match key {
            egui::Key::C => Some(MenuInput::CtrlC),
            egui::Key::H => Some(MenuInput::CtrlH),
            _ => None,
        };
    }
    if modifiers.alt || modifiers.command {
        return None;
    }
    match key {
        egui::Key::Backspace => Some(MenuInput::Backspace),
        egui::Key::Escape => Some(MenuInput::Escape),
        egui::Key::Space => Some(MenuInput::Character(' ')),
        other => {
            let name = other.name();
            let mut chars = name.chars();
            match (chars.next(), chars.next()) {
                (Some(c), None) if c.is_ascii_alphanumeric() => Some(MenuInput::Character(c)),
                _ => None,
            }
        }
    }
}

fn menu_row(ui: &mut egui::Ui, item: &MenuItem) -> egui::Response {
    let (key, label, description, submenu) = match item {
        MenuItem::Submenu {
            key,
            label,
            description,
            ..
        } => (key, label, description, true),
        MenuItem::Action {
            key,
            label,
            description,
            ..
        } => (key, label, description, false),
    };
    let height = if description.is_some() { 58.0 } else { 46.0 };
    let (rect, response) = ui.allocate_exact_size(
        egui::vec2(ui.available_width(), height),
        egui::Sense::click(),
    );
    let response = response.on_hover_cursor(egui::CursorIcon::PointingHand);
    let fill = if response.hovered() || response.has_focus() {
        egui::Color32::from_gray(54)
    } else {
        egui::Color32::from_gray(34)
    };
    let painter = ui.painter();
    painter.rect_filled(rect, 8, fill);

    let badge = egui::Rect::from_min_size(
        rect.min + egui::vec2(10.0, 8.0),
        egui::vec2(56.0, height - 16.0),
    );
    painter.rect_filled(badge, 5, egui::Color32::from_gray(80));
    let key_text = match key {
        MenuKey::Character(character) => character.to_string(),
        MenuKey::Space => "Space".to_owned(),
    };
    painter.text(
        badge.center(),
        egui::Align2::CENTER_CENTER,
        key_text,
        egui::FontId::monospace(14.0),
        egui::Color32::WHITE,
    );

    let text_x = badge.max.x + 12.0;
    let label_y = if description.is_some() {
        rect.center().y - 9.0
    } else {
        rect.center().y
    };
    painter.text(
        egui::pos2(text_x, label_y),
        egui::Align2::LEFT_CENTER,
        label,
        egui::FontId::proportional(16.0),
        egui::Color32::WHITE,
    );
    if let Some(description) = description {
        painter.text(
            egui::pos2(text_x, rect.center().y + 12.0),
            egui::Align2::LEFT_CENTER,
            description,
            egui::FontId::proportional(12.0),
            egui::Color32::LIGHT_GRAY,
        );
    }
    if submenu {
        painter.text(
            egui::pos2(rect.max.x - 18.0, rect.center().y),
            egui::Align2::CENTER_CENTER,
            "›",
            egui::FontId::proportional(22.0),
            egui::Color32::LIGHT_GRAY,
        );
    }
    response
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::mpsc;
    use std::thread;
    use std::time::{Duration, Instant};

    use eframe::egui;
    use nixon::config::Config;
    use nixon::config::launcher::{LauncherAction, MprisOperation};
    use nixon::config::parse_block;
    use nixon::project::Project;
    use nixon::select;
    use nixon_picker::{
        Candidate, CandidateStream, Picker, PickerOption, PickerOptions, Selection, SelectionType,
    };

    use super::{BrowserInputEvent, MenuWindow};
    use crate::picker::GuiPicker;

    fn frame(
        ctx: &egui::Context,
        window: &mut MenuWindow,
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
            |ctx| window.show(ctx),
        )
    }

    fn key(key: egui::Key, modifiers: egui::Modifiers) -> egui::Event {
        egui::Event::Key {
            key,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers,
        }
    }

    fn release(key: egui::Key, modifiers: egui::Modifiers) -> egui::Event {
        egui::Event::Key {
            key,
            physical_key: None,
            pressed: false,
            repeat: false,
            modifiers,
        }
    }

    fn candidates(values: &[&str]) -> Vec<Candidate> {
        values
            .iter()
            .map(|value| Candidate::identity(*value))
            .collect()
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

    fn click_button(
        ctx: &egui::Context,
        window: &mut MenuWindow,
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
            ctx,
            window,
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
            ctx,
            window,
            vec![egui::Event::PointerButton {
                pos: at,
                button: egui::PointerButton::Primary,
                pressed: false,
                modifiers: egui::Modifiers::default(),
            }],
        )
    }

    fn wait_for_text(
        ctx: &egui::Context,
        window: &mut MenuWindow,
        expected: &str,
    ) -> egui::FullOutput {
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            let output = frame(ctx, window, Vec::new());
            if texts(&output).iter().any(|text| text.contains(expected)) {
                return output;
            }
            assert!(Instant::now() < deadline, "did not render {expected}");
            thread::sleep(Duration::from_millis(1));
        }
    }

    fn wait_for_reply<T>(
        ctx: &egui::Context,
        window: &mut MenuWindow,
        receiver: &mpsc::Receiver<T>,
    ) -> T {
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            if let Ok(reply) = receiver.try_recv() {
                return reply;
            }
            assert!(Instant::now() < deadline, "picker reply timed out");
            frame(ctx, window, Vec::new());
            thread::sleep(Duration::from_millis(1));
        }
    }

    fn window_with_picker() -> (MenuWindow, GuiPicker) {
        let (actions, _receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let (picker, requests) = GuiPicker::channel();
        window.attach_picker(requests);
        (window, picker)
    }

    fn action_pick_options() -> [PickerOptions; 4] {
        let config = Config::defaults();
        let project = Project::from_path(std::path::Path::new("/tmp/picker-project"), Vec::new());
        [
            select::command_options(&config, &project, "Commands", None).header("Command actions"),
            select::project_options(&config, None, false).header("Project actions"),
            select::history_options(&config, None).header("History actions"),
            PickerOptions::default()
                .header("Placeholder actions")
                .options(vec![PickerOption::new("--force", false)]),
        ]
    }

    #[test]
    fn detail_screen_copies_exact_text_and_returns_to_menu() {
        let (actions, _receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let ctx = egui::Context::default();
        let body = "echo 'quoted value'\n\n";
        window.open_detail("Command: alpha".to_owned(), body.to_owned());
        let output = frame(&ctx, &mut window, Vec::new());
        let shown = texts(&output);
        assert!(shown.iter().any(|text| text == "Command: alpha"));
        assert!(shown.iter().any(|text| text == "Copy"));
        let output = click_button(&ctx, &mut window, &output, "Copy");
        assert!(
            output.platform_output.commands.iter().any(
                |command| matches!(command, egui::OutputCommand::CopyText(text) if text == body)
            )
        );
        let ctrl = egui::Modifiers {
            ctrl: true,
            ..egui::Modifiers::default()
        };
        let output = frame(&ctx, &mut window, vec![key(egui::Key::C, ctrl)]);
        assert!(
            output.platform_output.commands.iter().any(
                |command| matches!(command, egui::OutputCommand::CopyText(text) if text == body)
            )
        );
        click_button(&ctx, &mut window, &output, "Back");
        assert!(window.take_detail_closed());
        let output = frame(&ctx, &mut window, Vec::new());
        assert!(texts(&output).iter().any(|text| text == "Commands"));
        window.open_detail(
            "Project: work-one".to_owned(),
            "Name: work-one\n".to_owned(),
        );
        frame(&ctx, &mut window, Vec::new());
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Escape, egui::Modifiers::default())],
        );
        assert!(window.take_detail_closed());
    }

    #[test]
    fn configured_rows_render_key_label_and_description_and_take_focus() {
        let config = parse_block(
            "yaml",
            "launcher:\n  items:\n    - key: a\n      label: Alpha\n      description: Open the alpha command\n      action: commands\n    - key: Space\n      label: Pause\n      action: history\n",
        )
        .unwrap();
        let (actions, _receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&config.launcher, actions).unwrap();
        let ctx = egui::Context::default();
        let output = frame(&ctx, &mut window, Vec::new());
        let texts = output
            .shapes
            .iter()
            .filter_map(|clipped| match &clipped.shape {
                egui::Shape::Text(shape) => Some(shape.galley.text()),
                _ => None,
            })
            .collect::<Vec<_>>();
        for expected in ["a", "Alpha", "Open the alpha command", "Space", "Pause"] {
            assert!(texts.contains(&expected), "missing {expected}: {texts:?}");
        }
        assert!(ctx.memory(|memory| memory.focused().is_some()));
    }

    #[test]
    fn keyboard_uses_the_menu_model_and_emits_typed_actions() {
        let (actions, receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let ctx = egui::Context::default();
        frame(&ctx, &mut window, Vec::new());

        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::W, egui::Modifiers::default())],
        );
        assert_eq!(window.menu().depth(), 1);
        assert_eq!(window.menu().current_label(), Some("Browser"));
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::O, egui::Modifiers::default())],
        );
        assert_eq!(receiver.try_recv().unwrap(), LauncherAction::BrowserInput);
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Escape, egui::Modifiers::default())],
        );
        assert_eq!(window.menu().depth(), 0);
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::C, egui::Modifiers::default())],
        );
        assert_eq!(receiver.try_recv().unwrap(), LauncherAction::Commands);

        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::W, egui::Modifiers::default())],
        );
        let ctrl = egui::Modifiers {
            ctrl: true,
            ..egui::Modifiers::default()
        };
        frame(&ctx, &mut window, vec![key(egui::Key::H, ctrl)]);
        assert_eq!(window.menu().depth(), 0);

        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::S, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Space, egui::Modifiers::default())],
        );
        assert_eq!(
            receiver.try_recv().unwrap(),
            LauncherAction::Mpris {
                operation: MprisOperation::PlayPause,
                player: "spotify".to_owned(),
            }
        );
        assert!(receiver.try_recv().is_err());
    }

    #[test]
    fn browser_input_focus_ignores_mnemonics_and_submits_text() {
        let (actions, receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let ctx = egui::Context::default();
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::W, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::O, egui::Modifiers::default())],
        );
        assert_eq!(receiver.try_recv().unwrap(), LauncherAction::BrowserInput);
        window.open_browser_input();
        let output = frame(&ctx, &mut window, Vec::new());
        assert!(
            texts(&output)
                .iter()
                .any(|text| text == "Open URL or search")
        );
        assert!(ctx.memory(|memory| memory.focused().is_some()));
        frame(
            &ctx,
            &mut window,
            vec![
                release(egui::Key::W, egui::Modifiers::default()),
                release(egui::Key::O, egui::Modifiers::default()),
            ],
        );

        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        assert!(window.take_browser_event().is_none());
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Enter, egui::Modifiers::default())],
        );

        let typed = frame(
            &ctx,
            &mut window,
            vec![
                key(egui::Key::W, egui::Modifiers::default()),
                key(egui::Key::O, egui::Modifiers::default()),
                egui::Event::Text("web search".to_owned()),
            ],
        );
        assert!(
            texts(&typed).iter().any(|text| text.contains("web search")),
            "{:?}",
            texts(&typed)
        );
        assert_eq!(window.menu().depth(), 1);
        assert!(receiver.try_recv().is_err());
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        assert_eq!(
            window.take_browser_event(),
            Some(BrowserInputEvent::Submitted("web search".to_owned()))
        );
    }

    #[test]
    fn configured_browser_action_opens_input_and_escape_returns_to_menu() {
        let config = parse_block(
            "yaml",
            "launcher:\n  items:\n    - key: b\n      label: Browse\n      action: browser_input\n",
        )
        .unwrap();
        let (actions, receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&config.launcher, actions).unwrap();
        let ctx = egui::Context::default();
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::B, egui::Modifiers::default())],
        );
        assert_eq!(receiver.try_recv().unwrap(), LauncherAction::BrowserInput);
        window.open_browser_input();
        frame(&ctx, &mut window, Vec::new());
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Escape, egui::Modifiers::default())],
        );
        assert!(matches!(
            window.take_browser_event(),
            Some(BrowserInputEvent::Canceled)
        ));
        let output = frame(&ctx, &mut window, Vec::new());
        assert!(texts(&output).iter().any(|text| text == "Browse"));
        assert!(receiver.try_recv().is_err());
    }

    #[test]
    fn clicking_a_row_emits_its_action() {
        let (actions, receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let ctx = egui::Context::default();
        let output = frame(&ctx, &mut window, Vec::new());
        let center = output
            .shapes
            .iter()
            .find_map(|clipped| match &clipped.shape {
                egui::Shape::Rect(shape)
                    if shape.rect.width() > 300.0 && shape.rect.height() < 100.0 =>
                {
                    Some(shape.rect.center())
                }
                _ => None,
            })
            .unwrap();
        frame(
            &ctx,
            &mut window,
            vec![
                egui::Event::PointerMoved(center),
                egui::Event::PointerButton {
                    pos: center,
                    button: egui::PointerButton::Primary,
                    pressed: true,
                    modifiers: egui::Modifiers::default(),
                },
            ],
        );
        frame(
            &ctx,
            &mut window,
            vec![egui::Event::PointerButton {
                pos: center,
                button: egui::PointerButton::Primary,
                pressed: false,
                modifiers: egui::Modifiers::default(),
            }],
        );
        assert_eq!(receiver.try_recv().unwrap(), LauncherAction::Commands);
    }

    #[test]
    fn shortcut_modifiers_do_not_trigger_menu_mnemonics() {
        let (actions, receiver) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let ctx = egui::Context::default();
        let ctrl = egui::Modifiers {
            ctrl: true,
            ..egui::Modifiers::default()
        };
        frame(&ctx, &mut window, vec![key(egui::Key::P, ctrl)]);
        assert!(receiver.try_recv().is_err());
        assert_eq!(window.menu().depth(), 0);
    }

    #[test]
    fn gui_pick_filters_query_and_returns_to_menu() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let result = picker
                .pick(
                    &PickerOptions::default().header("Choose a command"),
                    candidates(&["alpha", "beta", "delta"]),
                )
                .unwrap();
            done.send(result).unwrap();
        });
        let ctx = egui::Context::default();
        let output = wait_for_text(&ctx, &mut window, "alpha");
        assert!(texts(&output).iter().any(|text| text.contains("3/3")));
        frame(
            &ctx,
            &mut window,
            vec![
                key(egui::Key::D, egui::Modifiers::default()),
                egui::Event::Text("d".to_owned()),
                key(egui::Key::E, egui::Modifiers::default()),
                egui::Event::Text("e".to_owned()),
            ],
        );
        let output = wait_for_text(&ctx, &mut window, "delta");
        assert!(texts(&output).iter().any(|text| text == "> de▏"));
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        let answer = wait_for_reply(&ctx, &mut window, &replies);
        assert_eq!(answer.items()[0].value, "delta");
        wait_for_text(&ctx, &mut window, "Nixon");
        worker.join().unwrap();
    }

    #[test]
    fn paste_into_empty_picker_query_filters_line_breaks_and_selects_unicode() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            done.send(
                picker
                    .pick(
                        &PickerOptions::default().header("Paste choices"),
                        candidates(&["東京", "大阪"]),
                    )
                    .unwrap(),
            )
            .unwrap();
        });
        let ctx = egui::Context::default();
        wait_for_text(&ctx, &mut window, "Paste choices");
        frame(
            &ctx,
            &mut window,
            vec![egui::Event::Paste(
                "東\n\u{0000}\u{2028}京\u{2029}".to_owned(),
            )],
        );
        let output = wait_for_text(&ctx, &mut window, "> 東京▏");
        assert!(texts(&output).iter().any(|text| text == "1/2"));
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        let answer = wait_for_reply(&ctx, &mut window, &replies);
        assert_eq!(answer.items()[0].value, "東京");
        wait_for_text(&ctx, &mut window, "Nixon");
        worker.join().unwrap();
    }

    #[test]
    fn paste_at_query_cursor_preserves_mark_and_escape_cancels() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            done.send(
                picker
                    .pick(
                        &PickerOptions::default()
                            .header("Paste at cursor")
                            .query("bld")
                            .multi(true)
                            .no_sort(),
                        candidates(&["build", "bald", "other"]),
                    )
                    .unwrap(),
            )
            .unwrap();
        });
        let ctx = egui::Context::default();
        wait_for_text(&ctx, &mut window, "> bld▏");
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![
                key(egui::Key::Home, egui::Modifiers::default()),
                key(egui::Key::ArrowRight, egui::Modifiers::default()),
            ],
        );
        frame(&ctx, &mut window, vec![egui::Event::Paste("ui".to_owned())]);
        let output = wait_for_text(&ctx, &mut window, "> bui▏ld");
        assert!(texts(&output).iter().any(|text| text == "1/3 (1)"));
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Escape, egui::Modifiers::default())],
        );
        assert_eq!(
            wait_for_reply(&ctx, &mut window, &replies),
            Selection::Canceled
        );
        wait_for_text(&ctx, &mut window, "Nixon");
        worker.join().unwrap();
    }

    #[test]
    fn typing_and_pasting_in_a_pick_do_not_trigger_menu_mnemonics() {
        let (actions, menu_actions) = mpsc::channel();
        let mut window = MenuWindow::new(&Config::defaults().launcher, actions).unwrap();
        let (mut picker, requests) = GuiPicker::channel();
        window.attach_picker(requests);
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            done.send(
                picker
                    .pick(
                        &PickerOptions::default().header("Picker owns input"),
                        candidates(&["paste", "other"]),
                    )
                    .unwrap(),
            )
            .unwrap();
        });
        let ctx = egui::Context::default();
        wait_for_text(&ctx, &mut window, "Picker owns input");
        frame(
            &ctx,
            &mut window,
            vec![
                key(egui::Key::P, egui::Modifiers::default()),
                egui::Event::Text("p".to_owned()),
                egui::Event::Paste("aste".to_owned()),
            ],
        );
        wait_for_text(&ctx, &mut window, "> paste▏");
        assert!(menu_actions.try_recv().is_err());
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        assert_eq!(
            wait_for_reply(&ctx, &mut window, &replies).items()[0].value,
            "paste"
        );
        worker.join().unwrap();
    }

    #[test]
    fn duplicate_visible_titles_keep_distinct_values_and_marks() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let options = PickerOptions::default()
                .header("Duplicate titles")
                .multi(true)
                .no_sort();
            let choices = vec![
                Candidate::with_title("same", "first value"),
                Candidate::with_title("same", "second value"),
            ];
            done.send(picker.pick(&options, choices).unwrap()).unwrap();
        });
        let ctx = egui::Context::default();
        let output = wait_for_text(&ctx, &mut window, "Duplicate titles");
        assert_eq!(
            texts(&output)
                .iter()
                .filter(|text| *text == "[ ] same")
                .count(),
            2
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        let output = wait_for_text(&ctx, &mut window, "(2)");
        assert_eq!(
            texts(&output)
                .iter()
                .filter(|text| *text == "[x] same")
                .count(),
            2
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        let selection = wait_for_reply(&ctx, &mut window, &replies);
        let items = selection.items();
        assert_eq!(
            items
                .iter()
                .map(|item| item.value.as_str())
                .collect::<Vec<_>>(),
            ["first value", "second value"]
        );
        assert_eq!(items.iter().map(|item| item.id).collect::<Vec<_>>(), [0, 1]);
        worker.join().unwrap();
    }

    #[test]
    fn ansi_titles_match_and_render_as_visible_text_with_initial_query() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let mut options = PickerOptions::default()
                .header("ANSI choices")
                .query("Alpha");
            options.matching.exact = true;
            let choices = vec![
                Candidate::with_title("A\u{1b}[31ml\u{1b}[0mpha", "colored value"),
                Candidate::with_title("Beta", "other value"),
            ];
            done.send(picker.pick(&options, choices).unwrap()).unwrap();
        });
        let ctx = egui::Context::default();
        let output = wait_for_text(&ctx, &mut window, "1/2");
        let shown = texts(&output);
        assert!(shown.iter().any(|text| text == "> Alpha▏"), "{shown:?}");
        assert!(shown.iter().any(|text| text == "Alpha"), "{shown:?}");
        assert!(
            !shown.iter().any(|text| text.contains('\u{1b}')),
            "{shown:?}"
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        let selection = wait_for_reply(&ctx, &mut window, &replies);
        assert_eq!(selection.items()[0].value, "colored value");
        worker.join().unwrap();
    }

    #[test]
    fn ctrl_c_cancels_the_picker_without_closing_the_window() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            done.send(
                picker
                    .pick(
                        &PickerOptions::default().header("Cancel pick"),
                        candidates(&["alpha", "beta"]),
                    )
                    .unwrap(),
            )
            .unwrap();
        });
        let ctx = egui::Context::default();
        wait_for_text(&ctx, &mut window, "Cancel pick");
        let control = egui::Modifiers {
            ctrl: true,
            ..egui::Modifiers::default()
        };
        let output = frame(&ctx, &mut window, vec![key(egui::Key::C, control)]);
        assert!(!output.viewport_output.values().any(|viewport| {
            viewport
                .commands
                .iter()
                .any(|command| matches!(command, egui::ViewportCommand::Close))
        }));
        assert_eq!(
            wait_for_reply(&ctx, &mut window, &replies),
            Selection::Canceled
        );
        wait_for_text(&ctx, &mut window, "Nixon");
        worker.join().unwrap();
    }

    #[test]
    fn picker_buttons_follow_action_options_and_mouse_clicks() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            for options in action_pick_options() {
                let selection = picker
                    .pick(&options, candidates(&["alpha", "beta"]))
                    .unwrap();
                done.send(selection).unwrap();
            }
        });
        let ctx = egui::Context::default();
        let cases: [(&str, &[&str], &str, SelectionType); 4] = [
            (
                "Command actions",
                &["Run", "Edit", "Show", "Visit"],
                "Visit",
                SelectionType::Visit,
            ),
            (
                "Project actions",
                &["Select", "Inspect"],
                "Inspect",
                SelectionType::Show,
            ),
            (
                "History actions",
                &["Replay", "Show"],
                "Replay",
                SelectionType::Default,
            ),
            (
                "Placeholder actions",
                &["Select"],
                "Select",
                SelectionType::Default,
            ),
        ];
        for (title, expected, clicked, kind) in cases {
            let output = wait_for_text(&ctx, &mut window, title);
            let shown = texts(&output);
            for label in [
                "Run", "Edit", "Show", "Visit", "Select", "Inspect", "Replay",
            ] {
                assert_eq!(
                    shown.iter().any(|text| text == label),
                    expected.contains(&label),
                    "{title}: {shown:?}"
                );
            }
            if title == "Placeholder actions" {
                assert!(shown.iter().any(|text| text == "[ ] --force"));
            }
            click_button(&ctx, &mut window, &output, clicked);
            let answer = wait_for_reply(&ctx, &mut window, &replies);
            assert!(matches!(answer, Selection::Selected { kind: actual, .. } if actual == kind));
        }
        worker.join().unwrap();
    }

    #[test]
    fn picker_actions_keep_contextual_keyboard_results() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            for options in action_pick_options() {
                let selection = picker
                    .pick(&options, candidates(&["alpha", "beta"]))
                    .unwrap();
                done.send(selection).unwrap();
            }
        });
        let ctx = egui::Context::default();
        let alt = egui::Modifiers {
            alt: true,
            ..egui::Modifiers::default()
        };
        for (title, pressed, modifiers, kind) in [
            (
                "Command actions",
                egui::Key::Enter,
                alt,
                SelectionType::Edit,
            ),
            (
                "Project actions",
                egui::Key::F1,
                egui::Modifiers::default(),
                SelectionType::Show,
            ),
            (
                "History actions",
                egui::Key::F1,
                egui::Modifiers::default(),
                SelectionType::Show,
            ),
            (
                "Placeholder actions",
                egui::Key::Enter,
                egui::Modifiers::default(),
                SelectionType::Default,
            ),
        ] {
            wait_for_text(&ctx, &mut window, title);
            frame(&ctx, &mut window, vec![release(pressed, modifiers)]);
            frame(&ctx, &mut window, vec![key(pressed, modifiers)]);
            let answer = wait_for_reply(&ctx, &mut window, &replies);
            assert!(matches!(answer, Selection::Selected { kind: actual, .. } if actual == kind));
        }
        worker.join().unwrap();
    }

    #[test]
    fn gui_pick_preserves_edit_show_and_visit_bindings() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            for title in ["Edit pick", "Show pick", "Visit pick"] {
                let result = picker
                    .pick(
                        &PickerOptions::default().header(title),
                        candidates(&["alpha", "beta"]),
                    )
                    .unwrap();
                done.send(result).unwrap();
            }
        });
        let ctx = egui::Context::default();
        let alt = egui::Modifiers {
            alt: true,
            ..egui::Modifiers::default()
        };
        for (title, event, expected) in [
            ("Edit pick", key(egui::Key::Enter, alt), SelectionType::Edit),
            (
                "Show pick",
                key(egui::Key::F1, egui::Modifiers::default()),
                SelectionType::Show,
            ),
            (
                "Visit pick",
                key(egui::Key::F2, egui::Modifiers::default()),
                SelectionType::Visit,
            ),
        ] {
            wait_for_text(&ctx, &mut window, title);
            frame(&ctx, &mut window, vec![event]);
            let answer = wait_for_reply(&ctx, &mut window, &replies);
            assert!(matches!(answer, Selection::Selected { kind, .. } if kind == expected));
        }
        worker.join().unwrap();
    }

    #[test]
    fn clicking_a_picker_row_selects_it() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let selection = picker
                .pick(&PickerOptions::default(), candidates(&["alpha", "beta"]))
                .unwrap();
            done.send(selection).unwrap();
        });
        let ctx = egui::Context::default();
        let output = wait_for_text(&ctx, &mut window, "alpha");
        let position = output
            .shapes
            .iter()
            .find_map(|clipped| match &clipped.shape {
                egui::Shape::Text(shape) if shape.galley.text() == "alpha" => {
                    Some(shape.pos + egui::vec2(5.0, 5.0))
                }
                _ => None,
            })
            .unwrap();
        frame(
            &ctx,
            &mut window,
            vec![
                egui::Event::PointerMoved(position),
                egui::Event::PointerButton {
                    pos: position,
                    button: egui::PointerButton::Primary,
                    pressed: true,
                    modifiers: egui::Modifiers::default(),
                },
            ],
        );
        frame(
            &ctx,
            &mut window,
            vec![egui::Event::PointerButton {
                pos: position,
                button: egui::PointerButton::Primary,
                pressed: false,
                modifiers: egui::Modifiers::default(),
            }],
        );
        let selection = wait_for_reply(&ctx, &mut window, &replies);
        assert_eq!(selection.items()[0].value, "alpha");
        worker.join().unwrap();
    }

    #[test]
    fn gui_pick_marks_multiple_rows_and_returns_option_changes() {
        let (mut window, mut picker) = window_with_picker();
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let options = PickerOptions::default()
                .header("Multi pick")
                .multi(true)
                .options(vec![PickerOption::new("--force", false)]);
            let result = picker
                .pick_options(&options, candidates(&["alpha", "beta"]))
                .unwrap();
            done.send(result).unwrap();
        });
        let ctx = egui::Context::default();
        wait_for_text(&ctx, &mut window, "Multi pick");
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(
                egui::Key::Num1,
                egui::Modifiers {
                    alt: true,
                    ..egui::Modifiers::default()
                },
            )],
        );
        let output = wait_for_text(&ctx, &mut window, "[x] --force");
        assert!(
            texts(&output).iter().any(|text| text.contains("(2)")),
            "{:?}",
            texts(&output)
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        let (selection, toggles) = wait_for_reply(&ctx, &mut window, &replies);
        assert_eq!(
            selection
                .items()
                .iter()
                .map(|item| item.value.as_str())
                .collect::<Vec<_>>(),
            vec!["alpha", "beta"]
        );
        assert_eq!(toggles, [true]);
        worker.join().unwrap();
    }

    #[test]
    fn gui_confirm_and_second_request_use_the_same_window() {
        let (mut window, mut picker) = window_with_picker();
        let (confirm_done, confirm_replies) = mpsc::channel();
        let (pick_done, pick_replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let first = picker
                .confirm(
                    &PickerOptions::default()
                        .header("Confirm flags")
                        .options(vec![PickerOption::new("--force", false)]),
                )
                .unwrap();
            confirm_done.send(first).unwrap();
            let second = picker
                .pick(
                    &PickerOptions::default().header("Second pick"),
                    candidates(&["one", "two"]),
                )
                .unwrap();
            pick_done.send(second).unwrap();
        });
        let ctx = egui::Context::default();
        wait_for_text(&ctx, &mut window, "Confirm flags");
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Space, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        assert_eq!(
            wait_for_reply(&ctx, &mut window, &confirm_replies),
            Some(vec![true])
        );
        wait_for_text(&ctx, &mut window, "Second pick");
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Escape, egui::Modifiers::default())],
        );
        assert_eq!(
            wait_for_reply(&ctx, &mut window, &pick_replies),
            Selection::Canceled
        );
        worker.join().unwrap();
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "one streamed pick checks query, marks, options, and early selection together"
    )]
    fn streaming_picker_stays_interactive_as_candidates_arrive() {
        let (mut window, mut picker) = window_with_picker();
        let (producer, receiver) = mpsc::channel();
        let stops = Arc::new(AtomicUsize::new(0));
        let count = Arc::clone(&stops);
        let (done, replies) = mpsc::channel();
        let worker = thread::spawn(move || {
            let mut stream = CandidateStream::new(
                receiver,
                Box::new(move || {
                    count.fetch_add(1, Ordering::SeqCst);
                }),
            );
            let options = PickerOptions::default()
                .header("Streaming choices")
                .multi(true)
                .options(vec![PickerOption::new("--force", false)]);
            done.send(picker.pick_stream_options(&options, &mut stream).unwrap())
                .unwrap();
        });
        let ctx = egui::Context::default();
        let output = wait_for_text(&ctx, &mut window, "Candidates arriving");
        assert!(
            texts(&output)
                .iter()
                .any(|text| text == "Streaming choices")
        );
        assert!(replies.try_recv().is_err());
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        assert!(
            replies.try_recv().is_err(),
            "Enter settled an empty active stream"
        );
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Enter, egui::Modifiers::default())],
        );
        producer.send(Candidate::identity("alpha")).unwrap();
        wait_for_text(&ctx, &mut window, "alpha");
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(&ctx, &mut window, vec![egui::Event::Text("bet".to_owned())]);
        wait_for_text(&ctx, &mut window, "> bet▏");
        producer.send(Candidate::identity("beta")).unwrap();
        let output = wait_for_text(&ctx, &mut window, "beta");
        assert!(texts(&output).iter().any(|text| text.contains("(1)")));
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Tab, egui::Modifiers::default())],
        );
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Tab, egui::Modifiers::default())],
        );
        producer.send(Candidate::identity("betamax")).unwrap();
        let output = wait_for_text(&ctx, &mut window, "betamax");
        assert!(texts(&output).iter().any(|text| text.contains("(2)")));
        frame(
            &ctx,
            &mut window,
            vec![key(
                egui::Key::Num1,
                egui::Modifiers {
                    alt: true,
                    ..egui::Modifiers::default()
                },
            )],
        );
        wait_for_text(&ctx, &mut window, "[x] --force");
        frame(
            &ctx,
            &mut window,
            vec![release(egui::Key::Num1, egui::Modifiers::ALT)],
        );
        frame(
            &ctx,
            &mut window,
            vec![key(egui::Key::Enter, egui::Modifiers::default())],
        );
        let (selection, toggles) = wait_for_reply(&ctx, &mut window, &replies);
        assert_eq!(
            selection
                .items()
                .iter()
                .map(|item| item.value.as_str())
                .collect::<Vec<_>>(),
            ["alpha", "beta"]
        );
        assert_eq!(toggles, [true]);
        worker.join().unwrap();
        assert_eq!(stops.load(Ordering::SeqCst), 1);
        drop(producer);
    }

    #[test]
    fn streaming_select_one_waits_for_finish_but_exact_value_can_settle_early() {
        for exact in [false, true] {
            let (mut window, mut picker) = window_with_picker();
            let (producer, receiver) = mpsc::channel();
            let mut producer = Some(producer);
            let stops = Arc::new(AtomicUsize::new(0));
            let count = Arc::clone(&stops);
            let (done, replies) = mpsc::channel();
            let worker = thread::spawn(move || {
                let mut stream = CandidateStream::new(
                    receiver,
                    Box::new(move || {
                        count.fetch_add(1, Ordering::SeqCst);
                    }),
                );
                let options = PickerOptions::default()
                    .header("Unique stream")
                    .query("alpha")
                    .select_one(true)
                    .select_exact(exact);
                done.send(picker.pick_stream_options(&options, &mut stream).unwrap().0)
                    .unwrap();
            });
            let ctx = egui::Context::default();
            wait_for_text(&ctx, &mut window, "Candidates arriving");
            producer
                .as_ref()
                .unwrap()
                .send(Candidate::identity("alpha"))
                .unwrap();
            wait_for_text(&ctx, &mut window, "alpha");
            if !exact {
                assert!(replies.try_recv().is_err(), "select_one settled before EOF");
                drop(producer.take());
            }
            let selection = wait_for_reply(&ctx, &mut window, &replies);
            assert_eq!(selection.items()[0].value, "alpha");
            worker.join().unwrap();
            if exact {
                assert_eq!(stops.load(Ordering::SeqCst), 1);
                drop(producer.take());
            }
        }
    }

    #[test]
    fn streaming_escape_and_window_close_release_worker() {
        for close in [false, true] {
            let (mut window, mut picker) = window_with_picker();
            let (_producer, receiver) = mpsc::channel();
            let stops = Arc::new(AtomicUsize::new(0));
            let count = Arc::clone(&stops);
            let (done, replies) = mpsc::channel();
            let worker = thread::spawn(move || {
                let mut stream = CandidateStream::new(
                    receiver,
                    Box::new(move || {
                        count.fetch_add(1, Ordering::SeqCst);
                    }),
                );
                done.send(
                    picker
                        .pick_stream_options(&PickerOptions::default(), &mut stream)
                        .unwrap()
                        .0,
                )
                .unwrap();
            });
            let ctx = egui::Context::default();
            wait_for_text(&ctx, &mut window, "Candidates arriving");
            if close {
                drop(window);
                assert_eq!(
                    replies.recv_timeout(Duration::from_secs(2)).unwrap(),
                    Selection::Canceled
                );
            } else {
                frame(
                    &ctx,
                    &mut window,
                    vec![key(egui::Key::Escape, egui::Modifiers::default())],
                );
                assert_eq!(
                    wait_for_reply(&ctx, &mut window, &replies),
                    Selection::Canceled
                );
            }
            worker.join().unwrap();
            assert_eq!(stops.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn streaming_picker_shows_finished_state_and_reports_producer_error() {
        for code in [0, 7] {
            let (mut window, mut picker) = window_with_picker();
            let (producer, receiver) = mpsc::channel();
            let (done, replies) = mpsc::channel();
            let worker = thread::spawn(move || {
                let mut stream = CandidateStream::new(receiver, Box::new(|| {}))
                    .with_completion(Box::new(move || Ok(Some(code))));
                done.send(picker.pick_stream_options(&PickerOptions::default(), &mut stream))
                    .unwrap();
            });
            let ctx = egui::Context::default();
            wait_for_text(&ctx, &mut window, "Candidates arriving");
            producer.send(Candidate::identity("alpha")).unwrap();
            producer.send(Candidate::identity("beta")).unwrap();
            wait_for_text(&ctx, &mut window, "beta");
            drop(producer);
            if code == 0 {
                wait_for_text(&ctx, &mut window, "Candidates complete");
                frame(
                    &ctx,
                    &mut window,
                    vec![key(egui::Key::Enter, egui::Modifiers::default())],
                );
                let result = wait_for_reply(&ctx, &mut window, &replies).unwrap();
                assert_eq!(result.0.items()[0].value, "alpha");
            } else {
                let error = wait_for_reply(&ctx, &mut window, &replies).unwrap_err();
                assert!(error.to_string().contains("status 7"));
            }
            worker.join().unwrap();
        }
    }
}
