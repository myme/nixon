//! The graphical menu preview in the existing `nixon` binary.

use std::sync::mpsc::{self, Receiver};

use eframe::egui;
use nixon::config::launcher::{LauncherAction, LauncherConfig};
use nixon::error::{NixonError, Result};
use nixon_gui::window::{MenuWindow, native_options};

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
pub fn run(config: &LauncherConfig) -> Result<i32> {
    let app = PreviewApp::new(config)?;
    eframe::run_native("Nixon", native_options(), Box::new(|_| Ok(Box::new(app))))
        .map_err(|err| NixonError::Io(std::io::Error::other(err.to_string())))?;
    Ok(0)
}

struct PreviewApp {
    menu: MenuWindow,
    actions: Receiver<LauncherAction>,
    status: String,
}

impl PreviewApp {
    fn new(config: &LauncherConfig) -> Result<Self> {
        let (sender, actions) = mpsc::channel();
        let menu = MenuWindow::new(config, sender).ok_or_else(|| {
            NixonError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "GUI launcher menu has no items",
            ))
        })?;
        Ok(Self {
            menu,
            actions,
            status: "GUI preview: actions are not wired yet.".to_owned(),
        })
    }

    fn show(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::bottom("preview_status").show(ctx, |ui| {
            ui.label(&self.status);
        });
        self.menu.show(ctx);
        for action in self.actions.try_iter() {
            self.status = format!(
                "{} selected. Execution is not wired yet.",
                action_name(&action)
            );
            ctx.request_repaint();
        }
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
    use eframe::egui;
    use nixon::config::Config;

    use super::PreviewApp;

    #[test]
    fn selected_action_shows_visible_status_without_execution() {
        let mut app = PreviewApp::new(&Config::defaults().launcher).unwrap();
        let ctx = egui::Context::default();
        let screen_rect = Some(egui::Rect::from_min_size(
            egui::Pos2::ZERO,
            egui::vec2(480.0, 360.0),
        ));
        let _ = ctx.run(
            egui::RawInput {
                screen_rect,
                events: vec![egui::Event::Key {
                    key: egui::Key::C,
                    physical_key: None,
                    pressed: true,
                    repeat: false,
                    modifiers: egui::Modifiers::default(),
                }],
                ..egui::RawInput::default()
            },
            |ctx| app.show(ctx),
        );
        let output = ctx.run(
            egui::RawInput {
                screen_rect,
                ..egui::RawInput::default()
            },
            |ctx| app.show(ctx),
        );
        assert!(output.shapes.iter().any(|clipped| {
            match &clipped.shape {
                egui::Shape::Text(shape) => shape
                    .galley
                    .text()
                    .contains("Commands selected. Execution is not wired yet."),
                _ => false,
            }
        }));
    }
}
