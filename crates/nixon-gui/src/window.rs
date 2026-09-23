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
    use std::sync::mpsc;
    use std::thread;
    use std::time::{Duration, Instant};

    use eframe::egui;
    use nixon::config::Config;
    use nixon::config::launcher::{LauncherAction, MprisOperation};
    use nixon::config::parse_block;
    use nixon_picker::{Candidate, Picker, PickerOption, PickerOptions, Selection, SelectionType};

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
}
