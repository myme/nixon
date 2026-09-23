//! A compact egui window for configured launcher menus.

use std::sync::mpsc::Sender;

use eframe::egui;
use nixon::config::launcher::{LauncherAction, LauncherConfig, MenuItem, MenuKey};

use crate::menu::{InputFocus, MenuInput, MenuOutcome, MenuState};

/// The menu view and its action output channel.
pub struct MenuWindow {
    menu: MenuState,
    actions: Sender<LauncherAction>,
    focus_first_row: bool,
}

impl MenuWindow {
    /// Builds a window from a resolved launcher config.
    #[must_use]
    pub fn new(config: &LauncherConfig, actions: Sender<LauncherAction>) -> Option<Self> {
        Some(Self {
            menu: MenuState::new(config)?,
            actions,
            focus_first_row: true,
        })
    }

    /// The menu currently shown by the window.
    #[must_use]
    pub const fn menu(&self) -> &MenuState {
        &self.menu
    }

    /// Handles one egui frame without running the selected action.
    pub fn show(&mut self, ctx: &egui::Context) {
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

    use eframe::egui;
    use nixon::config::Config;
    use nixon::config::launcher::{LauncherAction, MprisOperation};
    use nixon::config::parse_block;

    use super::MenuWindow;

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
}
