//! Focused browser input screen in the launcher window.

use eframe::egui;

#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) enum BrowserInputOutcome {
    Pending,
    Cancel,
    Submit(String),
}

#[derive(Default)]
#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) struct BrowserInputView {
    text: String,
}

impl BrowserInputView {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn show(&mut self, ctx: &egui::Context) -> BrowserInputOutcome {
        let mut outcome = BrowserInputOutcome::Pending;
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Open URL or search");
            ui.add_space(12.0);
            let response = ui.add(
                egui::TextEdit::singleline(&mut self.text)
                    .desired_width(f32::INFINITY)
                    .hint_text("URL or search terms"),
            );
            response.request_focus();
            ui.label("Enter opens · Esc returns to menu");

            if pressed(ctx, egui::Key::Escape) {
                outcome = BrowserInputOutcome::Cancel;
            } else if pressed(ctx, egui::Key::Enter) && !self.text.trim().is_empty() {
                outcome = BrowserInputOutcome::Submit(std::mem::take(&mut self.text));
            }
        });
        outcome
    }
}

fn pressed(ctx: &egui::Context, wanted: egui::Key) -> bool {
    ctx.input(|input| {
        input.events.iter().any(
            |event| matches!(event, egui::Event::Key { key, pressed: true, .. } if *key == wanted),
        )
    })
}
