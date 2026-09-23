//! egui screens for the picker's channel requests.

use std::collections::VecDeque;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::time::Duration;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use eframe::egui;
use nixon_picker::ui::App;
use nixon_picker::{Candidate, PickerOptions, Selection};

use crate::picker::{
    ConfirmRequest, GuiPickerRequests, PickOptionsReply, PickOptionsRequest, PickRequest,
    PickerRequest,
};

const POLL_INTERVAL: Duration = Duration::from_millis(30);
const KEYS_PER_FRAME: usize = 2;
const ROW_HEIGHT: f32 = 26.0;

/// Picker request state owned by the launcher window.
#[expect(
    clippy::redundant_pub_crate,
    reason = "sibling window module needs this type while unreachable_pub forbids pub"
)]
pub(crate) struct PickerView {
    requests: GuiPickerRequests,
    screen: Option<Screen>,
}

enum Screen {
    Loading {
        ready: Receiver<App>,
        reply: PickReply,
        pending: VecDeque<KeyEvent>,
    },
    Pick {
        app: App,
        reply: PickReply,
        pending: VecDeque<KeyEvent>,
    },
    Confirm {
        app: App,
        reply: Sender<Option<Vec<bool>>>,
        pending: VecDeque<KeyEvent>,
    },
}

enum PickReply {
    Basic(Sender<Selection<Candidate>>),
    Options(Sender<PickOptionsReply>),
}

impl PickReply {
    fn respond(self, selection: Selection<Candidate>, toggles: Vec<bool>) {
        match self {
            Self::Basic(reply) => {
                let _ = reply.send(selection);
            }
            Self::Options(reply) => {
                let _ = reply.send(PickOptionsReply { selection, toggles });
            }
        }
    }
}

impl PickerView {
    pub(super) const fn new(requests: GuiPickerRequests) -> Self {
        Self {
            requests,
            screen: None,
        }
    }

    /// Polls without waiting, returning whether a picker owns this frame.
    pub(super) fn show(&mut self, ctx: &egui::Context) -> bool {
        ctx.request_repaint_after(POLL_INTERVAL);
        if self.screen.is_none()
            && let Ok(request) = self.requests.try_recv()
        {
            self.screen = Some(start(request));
        }
        let Some(screen) = self.screen.take() else {
            return false;
        };
        self.screen = update_screen(ctx, screen);
        if self.screen.is_none() {
            ctx.request_repaint();
        }
        true
    }
}

fn start(request: PickerRequest) -> Screen {
    match request {
        PickerRequest::Pick(PickRequest {
            options,
            candidates,
            reply,
        }) => loading(options, candidates, PickReply::Basic(reply)),
        PickerRequest::PickOptions(PickOptionsRequest {
            options,
            candidates,
            reply,
        }) => loading(options, candidates, PickReply::Options(reply)),
        PickerRequest::Confirm(ConfirmRequest { options, reply }) => {
            let mut app = App::empty(options);
            app.toggle_option_focus();
            Screen::Confirm {
                app,
                reply,
                pending: VecDeque::new(),
            }
        }
    }
}

fn loading(options: PickerOptions, candidates: Vec<Candidate>, reply: PickReply) -> Screen {
    let (sender, ready) = mpsc::channel();
    let _ = std::thread::spawn(move || {
        let _ = sender.send(App::new(candidates, options));
    });
    Screen::Loading {
        ready,
        reply,
        pending: VecDeque::new(),
    }
}

fn update_screen(ctx: &egui::Context, screen: Screen) -> Option<Screen> {
    match screen {
        Screen::Loading {
            ready,
            reply,
            mut pending,
        } => {
            pending.extend(picker_events(ctx));
            if pending.iter().any(is_cancel) {
                reply.respond(Selection::Canceled, Vec::new());
                return None;
            }
            match ready.try_recv() {
                Ok(app) => update_pick(ctx, app, reply, pending),
                Err(TryRecvError::Empty) => {
                    egui::CentralPanel::default().show(ctx, |ui| {
                        ui.heading("Loading candidates…");
                    });
                    Some(Screen::Loading {
                        ready,
                        reply,
                        pending,
                    })
                }
                Err(TryRecvError::Disconnected) => None,
            }
        }
        Screen::Pick {
            app,
            reply,
            mut pending,
        } => {
            pending.extend(picker_events(ctx));
            update_pick(ctx, app, reply, pending)
        }
        Screen::Confirm {
            app,
            reply,
            mut pending,
        } => {
            pending.extend(picker_events(ctx));
            update_confirm(ctx, app, reply, pending)
        }
    }
}

fn update_pick(
    ctx: &egui::Context,
    mut app: App,
    reply: PickReply,
    mut pending: VecDeque<KeyEvent>,
) -> Option<Screen> {
    app.tick();
    for _ in 0..KEYS_PER_FRAME {
        let Some(key) = pending.pop_front() else {
            break;
        };
        if app.is_matching() && is_confirm(key) {
            pending.push_front(key);
            break;
        }
        app.handle(key);
        if app.is_done() {
            break;
        }
    }
    if let Some(selection) = app.outcome.take() {
        reply.respond(selection, app.option_state());
        return None;
    }

    let (row_clicked, action_clicked) = render_pick(ctx, &mut app);

    if !app.is_matching() {
        if let Some(index) = row_clicked {
            app.cursor = index;
            app.handle(KeyEvent::new(
                if app.multi() {
                    KeyCode::Tab
                } else {
                    KeyCode::Enter
                },
                KeyModifiers::NONE,
            ));
        }
        if let Some(key) = action_clicked {
            app.handle(key);
        }
    }

    if let Some(selection) = app.outcome.take() {
        reply.respond(selection, app.option_state());
        return None;
    }
    if app.is_matching() || !pending.is_empty() {
        ctx.request_repaint();
    }
    Some(Screen::Pick {
        app,
        reply,
        pending,
    })
}

fn render_pick(ctx: &egui::Context, app: &mut App) -> (Option<usize>, Option<KeyEvent>) {
    let mut row_clicked = None;
    let mut action_clicked = None;
    egui::CentralPanel::default().show(ctx, |ui| {
        if let Some(header) = app.header() {
            ui.heading(header);
        } else {
            ui.heading("Select");
        }
        option_row(ui, app);
        ui.horizontal(|ui| {
            ui.label(egui::RichText::new(query_display(app)).monospace());
            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                ui.label(count_label(app));
            });
        });
        ui.add_space(8.0);

        let available = (ui.available_height() - 48.0).max(ROW_HEIGHT);
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_sign_loss,
            reason = "viewport height is positive and a fractional row is omitted"
        )]
        let visible_rows = (available / ROW_HEIGHT).floor() as usize;
        app.set_height(visible_rows);
        let offset = app.offset;
        for (index, row) in app.rows().into_iter().enumerate() {
            let mark = if app.multi() {
                if row.marked { "[x] " } else { "[ ] " }
            } else {
                ""
            };
            let label = format!("{mark}{}", row.candidate.plain());
            let mut button = egui::Button::new(egui::RichText::new(label).monospace());
            if row.is_cursor {
                button = button.fill(egui::Color32::from_gray(64));
            }
            let response = ui.add_sized([ui.available_width(), ROW_HEIGHT - 2.0], button);
            if response.clicked_by(egui::PointerButton::Primary)
                && ui.input(|input| input.pointer.any_click())
            {
                row_clicked = Some(offset + index);
            }
        }
        if app.matched_count() == 0 && !app.is_matching() {
            ui.label("No matches");
        }
        if app.is_matching() {
            ui.label("Matching…");
        }
        ui.add_space(8.0);
        ui.horizontal(|ui| {
            for (label, code, modifiers) in [
                ("Run", KeyCode::Enter, KeyModifiers::NONE),
                ("Edit", KeyCode::Enter, KeyModifiers::ALT),
                ("Show", KeyCode::F(1), KeyModifiers::NONE),
                ("Visit", KeyCode::F(2), KeyModifiers::NONE),
            ] {
                let response = ui.button(label);
                if response.clicked_by(egui::PointerButton::Primary)
                    && ui.input(|input| input.pointer.any_click())
                {
                    action_clicked = Some(KeyEvent::new(code, modifiers));
                }
            }
        });
    });
    (row_clicked, action_clicked)
}

fn update_confirm(
    ctx: &egui::Context,
    mut app: App,
    reply: Sender<Option<Vec<bool>>>,
    mut pending: VecDeque<KeyEvent>,
) -> Option<Screen> {
    for _ in 0..KEYS_PER_FRAME {
        let Some(key) = pending.pop_front() else {
            break;
        };
        if is_cancel(&key) {
            let _ = reply.send(None);
            return None;
        }
        if key.code == KeyCode::Enter {
            let _ = reply.send(Some(app.option_state()));
            return None;
        }
        app.handle(key);
    }

    let mut accept = false;
    let mut cancel = false;
    egui::CentralPanel::default().show(ctx, |ui| {
        ui.heading(app.header().unwrap_or("Options"));
        option_row(ui, &mut app);
        ui.horizontal(|ui| {
            accept = ui.button("Run").clicked() && ui.input(|input| input.pointer.any_click());
            cancel = ui.button("Cancel").clicked() && ui.input(|input| input.pointer.any_click());
        });
    });
    if accept || cancel {
        let _ = reply.send(accept.then(|| app.option_state()));
        return None;
    }
    if !pending.is_empty() {
        ctx.request_repaint();
    }
    Some(Screen::Confirm {
        app,
        reply,
        pending,
    })
}

fn option_row(ui: &mut egui::Ui, app: &mut App) {
    let options = app.option_row().to_vec();
    if options.is_empty() {
        return;
    }
    ui.horizontal_wrapped(|ui| {
        for (index, option) in options.iter().enumerate() {
            let check = if option.on { "[x]" } else { "[ ]" };
            let label = format!("{check} {}", option.label);
            let response = ui.selectable_label(app.option_focus == Some(index), label);
            if response.clicked_by(egui::PointerButton::Primary)
                && ui.input(|input| input.pointer.any_click())
            {
                app.toggle_option(index);
            }
        }
    });
    if let Some(description) = app
        .option_focus
        .and_then(|index| options.get(index))
        .and_then(|option| option.description.as_deref())
    {
        ui.label(description);
    }
}

fn query_display(app: &App) -> String {
    let query = app.query.text();
    let (_, column) = app.query.cursor();
    let at = query
        .char_indices()
        .nth(column)
        .map_or(query.len(), |(at, _)| at);
    format!("> {}▏{}", &query[..at], &query[at..])
}

fn count_label(app: &App) -> String {
    let count = format!("{}/{}", app.matched_count(), app.total_count());
    if app.multi() && !app.marked.is_empty() {
        format!("{count} ({})", app.marked.len())
    } else {
        count
    }
}

const fn is_confirm(key: KeyEvent) -> bool {
    matches!(key.code, KeyCode::Enter | KeyCode::F(1 | 2))
}

fn is_cancel(key: &KeyEvent) -> bool {
    key.code == KeyCode::Esc
        || matches!(key.code, KeyCode::Char('c' | 'g'))
            && key.modifiers.contains(KeyModifiers::CONTROL)
}

fn picker_events(ctx: &egui::Context) -> Vec<KeyEvent> {
    ctx.input(|input| {
        let text_has_space = input
            .events
            .iter()
            .any(|event| matches!(event, egui::Event::Text(text) if text.contains(' ')));
        input
            .events
            .iter()
            .flat_map(|event| match event {
                egui::Event::Text(text) => text
                    .chars()
                    .filter(|c| !c.is_control())
                    .map(|c| KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE))
                    .collect::<Vec<_>>(),
                egui::Event::Key {
                    key,
                    pressed: true,
                    repeat: false,
                    modifiers,
                    ..
                } => egui_key(*key, *modifiers, text_has_space)
                    .into_iter()
                    .collect(),
                egui::Event::MouseWheel { delta, .. } if delta.y > 0.0 => {
                    vec![KeyEvent::new(KeyCode::Up, KeyModifiers::NONE)]
                }
                egui::Event::MouseWheel { delta, .. } if delta.y < 0.0 => {
                    vec![KeyEvent::new(KeyCode::Down, KeyModifiers::NONE)]
                }
                _ => Vec::new(),
            })
            .collect()
    })
}

fn egui_key(key: egui::Key, modifiers: egui::Modifiers, text_has_space: bool) -> Option<KeyEvent> {
    let mut mods = KeyModifiers::NONE;
    if modifiers.ctrl {
        mods |= KeyModifiers::CONTROL;
    }
    if modifiers.alt {
        mods |= KeyModifiers::ALT;
    }
    if modifiers.shift {
        mods |= KeyModifiers::SHIFT;
    }
    let code = match key {
        egui::Key::Enter => KeyCode::Enter,
        egui::Key::Escape => KeyCode::Esc,
        egui::Key::Tab if modifiers.shift => KeyCode::BackTab,
        egui::Key::Tab => KeyCode::Tab,
        egui::Key::Backspace => KeyCode::Backspace,
        egui::Key::Delete => KeyCode::Delete,
        egui::Key::ArrowUp => KeyCode::Up,
        egui::Key::ArrowDown => KeyCode::Down,
        egui::Key::ArrowLeft => KeyCode::Left,
        egui::Key::ArrowRight => KeyCode::Right,
        egui::Key::PageUp => KeyCode::PageUp,
        egui::Key::PageDown => KeyCode::PageDown,
        egui::Key::Home => KeyCode::Home,
        egui::Key::End => KeyCode::End,
        egui::Key::F1 => KeyCode::F(1),
        egui::Key::F2 => KeyCode::F(2),
        egui::Key::Space if !text_has_space => KeyCode::Char(' '),
        egui::Key::Space => return None,
        other if modifiers.ctrl || modifiers.alt => {
            let name = other.name();
            let mut chars = name.chars();
            match (chars.next(), chars.next()) {
                (Some(c), None) if c.is_ascii_alphanumeric() => {
                    KeyCode::Char(c.to_ascii_lowercase())
                }
                _ => return None,
            }
        }
        _ => return None,
    };
    Some(KeyEvent::new(code, mods))
}
