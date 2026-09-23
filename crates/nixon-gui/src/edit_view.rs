//! In-window source editor for a selected command.

use eframe::egui;

#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) struct EditView {
    title: String,
    text: String,
    error: Option<String>,
    focus_text: bool,
}

#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) enum EditOutcome {
    Pending,
    Canceled,
    Submitted(String),
}

impl EditView {
    pub(crate) const fn new(title: String, source: String) -> Self {
        Self {
            title,
            text: source,
            error: None,
            focus_text: true,
        }
    }

    pub(crate) fn show(&mut self, ctx: &egui::Context) -> EditOutcome {
        let mut submit = false;
        let mut cancel = false;
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading(&self.title);
            ui.horizontal(|ui| {
                submit = ui.button("Submit").clicked();
                cancel = ui.button("Back").clicked();
                ui.label("Ctrl-Enter submits · Esc returns to menu");
            });
            if let Some(error) = &self.error {
                ui.colored_label(egui::Color32::LIGHT_RED, error);
            }
            ui.separator();
            egui::ScrollArea::vertical().show(ui, |ui| {
                let response = ui.add(
                    egui::TextEdit::multiline(&mut self.text)
                        .desired_width(f32::INFINITY)
                        .desired_rows(12)
                        .code_editor(),
                );
                if self.focus_text {
                    response.request_focus();
                    self.focus_text = false;
                }
                if response.changed() {
                    self.error = None;
                }
            });
        });
        ctx.input(|input| {
            for event in &input.events {
                if let egui::Event::Key {
                    key,
                    pressed: true,
                    modifiers,
                    ..
                } = event
                {
                    if *key == egui::Key::Escape {
                        cancel = true;
                    } else if *key == egui::Key::Enter && modifiers.ctrl {
                        submit = true;
                    }
                }
            }
        });
        if cancel {
            EditOutcome::Canceled
        } else if submit {
            if self.text.trim().is_empty() {
                self.error = Some("Empty command.".to_owned());
                self.focus_text = true;
                ctx.request_repaint();
                EditOutcome::Pending
            } else {
                EditOutcome::Submitted(self.text.clone())
            }
        } else {
            EditOutcome::Pending
        }
    }
}
