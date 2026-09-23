//! Read-only detail screen shared by command source and project inspection.

use eframe::egui;

#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) struct DetailView {
    title: String,
    body: String,
}

#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) enum DetailOutcome {
    Pending,
    Back,
}

impl DetailView {
    pub(crate) const fn new(title: String, body: String) -> Self {
        Self { title, body }
    }

    pub(crate) fn show(&mut self, ctx: &egui::Context) -> DetailOutcome {
        let mut back = false;
        let mut copy = false;
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading(&self.title);
            ui.horizontal(|ui| {
                back = ui.button("Back").clicked();
                copy = ui.button("Copy").clicked();
                ui.label("Esc goes back · Ctrl-C copies");
            });
            ui.separator();
            egui::ScrollArea::vertical().show(ui, |ui| {
                ui.add(
                    egui::TextEdit::multiline(&mut self.body)
                        .desired_width(f32::INFINITY)
                        .interactive(false)
                        .code_editor(),
                );
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
                        back = true;
                    } else if *key == egui::Key::C && (modifiers.command || modifiers.ctrl) {
                        copy = true;
                    }
                }
            }
        });
        if copy {
            ctx.copy_text(self.body.clone());
        }
        if back {
            DetailOutcome::Back
        } else {
            DetailOutcome::Pending
        }
    }
}
