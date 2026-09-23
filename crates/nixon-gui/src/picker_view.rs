//! egui screens for the picker's channel requests.

use std::collections::VecDeque;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::time::Duration;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use eframe::egui;
use nixon_picker::ui::App;
use nixon_picker::{Candidate, PickerOptions, Selection, SelectionType, exact_selection};

use crate::picker::{
    ConfirmRequest, GuiPickerRequests, PickOptionsReply, PickOptionsRequest, PickRequest,
    PickerRequest, StreamOptionsRequest, StreamUpdate,
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
    ime: ImeState,
}

#[derive(Default)]
struct ImeState {
    preedit: String,
    composing: bool,
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
        stream: Option<StreamUi>,
    },
    Confirm {
        app: App,
        reply: Sender<Option<Vec<bool>>>,
        pending: VecDeque<KeyEvent>,
    },
}

struct StreamUi {
    updates: Receiver<StreamUpdate>,
    options: PickerOptions,
    finished: bool,
    untouched: bool,
}

enum PickReply {
    Basic(Sender<Selection<Candidate>>),
    Options(Sender<PickOptionsReply>),
    Stream(Sender<std::io::Result<PickOptionsReply>>),
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
            Self::Stream(reply) => {
                let _ = reply.send(Ok(PickOptionsReply { selection, toggles }));
            }
        }
    }

    fn error(self, message: String) {
        if let Self::Stream(reply) = self {
            let _ = reply.send(Err(std::io::Error::other(message)));
        }
    }
}

impl PickerView {
    pub(super) fn new(requests: GuiPickerRequests) -> Self {
        Self {
            requests,
            screen: None,
            ime: ImeState::default(),
        }
    }

    /// Polls without waiting, returning whether a picker owns this frame.
    pub(super) fn show(&mut self, ctx: &egui::Context) -> bool {
        ctx.request_repaint_after(POLL_INTERVAL);
        if self.screen.is_none()
            && let Ok(request) = self.requests.try_recv()
        {
            self.ime = ImeState::default();
            self.screen = Some(start(request));
        }
        let Some(screen) = self.screen.take() else {
            return false;
        };
        self.screen = update_screen(ctx, screen, &mut self.ime);
        if self.screen.is_none() {
            self.ime = ImeState::default();
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
        PickerRequest::StreamOptions(StreamOptionsRequest {
            options,
            updates,
            reply,
        }) => Screen::Pick {
            app: App::empty(options.clone()),
            reply: PickReply::Stream(reply),
            pending: VecDeque::new(),
            stream: Some(StreamUi {
                updates,
                options,
                finished: false,
                untouched: true,
            }),
        },
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

fn update_screen(ctx: &egui::Context, screen: Screen, ime: &mut ImeState) -> Option<Screen> {
    match screen {
        Screen::Loading {
            ready,
            reply,
            mut pending,
        } => {
            pending.extend(picker_events(ctx, ime, true));
            if pending.iter().any(is_cancel) {
                reply.respond(Selection::Canceled, Vec::new());
                return None;
            }
            match ready.try_recv() {
                Ok(app) => update_pick(ctx, app, reply, pending, None, ime),
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
            stream,
        } => {
            pending.extend(picker_events(ctx, ime, app.option_focus.is_none()));
            update_pick(ctx, app, reply, pending, stream, ime)
        }
        Screen::Confirm {
            app,
            reply,
            mut pending,
        } => {
            pending.extend(picker_events(ctx, ime, false));
            update_confirm(ctx, app, reply, pending)
        }
    }
}

fn update_pick(
    ctx: &egui::Context,
    mut app: App,
    reply: PickReply,
    mut pending: VecDeque<KeyEvent>,
    mut stream: Option<StreamUi>,
    ime: &mut ImeState,
) -> Option<Screen> {
    if let Some(stream) = stream.as_mut() {
        if !pending.is_empty() {
            stream.untouched = false;
        }
        if let Some(result) = feed_stream(&mut app, stream) {
            match result {
                Ok(selection) => reply.respond(selection, app.option_state()),
                Err(error) => reply.error(error),
            }
            return None;
        }
    }
    app.tick();
    for _ in 0..KEYS_PER_FRAME {
        let Some(key) = pending.pop_front() else {
            break;
        };
        if stream.as_ref().is_some_and(|stream| !stream.finished)
            && app.matched_count() == 0
            && app.marked.is_empty()
            && is_confirm(key)
        {
            continue;
        }
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

    if let Some(stream) = stream.as_ref()
        && stream.finished
        && stream.untouched
        && stream.options.select_one
        && !app.is_matching()
        && app.matched_count() <= 1
    {
        let selection = app.current().map_or(Selection::Empty, |candidate| {
            Selection::selected(SelectionType::Default, vec![candidate])
        });
        reply.respond(selection, app.option_state());
        return None;
    }

    let (row_clicked, action_clicked) = render_pick(
        ctx,
        &mut app,
        stream.as_ref().map(|stream| stream.finished),
        ime,
    );

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
        if let Some(key) = action_clicked
            && (stream.as_ref().is_none_or(|stream| stream.finished)
                || app.matched_count() > 0
                || !app.marked.is_empty()
                || !is_confirm(key))
        {
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
        stream,
    })
}

fn feed_stream(
    app: &mut App,
    stream: &mut StreamUi,
) -> Option<Result<Selection<Candidate>, String>> {
    match stream.updates.try_recv() {
        Ok(StreamUpdate::Candidates(candidates)) => {
            for candidate in candidates {
                if stream.untouched
                    && let Some(selection) =
                        exact_selection(&stream.options, std::slice::from_ref(&candidate))
                {
                    return Some(Ok(selection));
                }
                app.push(candidate);
            }
            None
        }
        Ok(StreamUpdate::Finished(Ok(()))) => {
            stream.finished = true;
            None
        }
        Ok(StreamUpdate::Finished(Err(error))) => Some(Err(error)),
        Err(TryRecvError::Disconnected) if !stream.finished => Some(Err(
            "Candidate producer disconnected unexpectedly.".to_owned(),
        )),
        Err(TryRecvError::Empty | TryRecvError::Disconnected) => None,
    }
}

fn render_pick(
    ctx: &egui::Context,
    app: &mut App,
    stream_finished: Option<bool>,
    ime: &mut ImeState,
) -> (Option<usize>, Option<KeyEvent>) {
    let mut row_clicked = None;
    let mut action_clicked = None;
    egui::CentralPanel::default().show(ctx, |ui| {
        if let Some(header) = app.header() {
            ui.heading(header);
        } else {
            ui.heading("Select");
        }
        if let Some(finished) = stream_finished {
            ui.label(if finished {
                "Candidates complete"
            } else {
                "Candidates arriving…"
            });
        }
        option_row(ui, app);
        ui.horizontal(|ui| {
            if app.option_focus.is_none() {
                let response =
                    ui.label(egui::RichText::new(query_display(app, &ime.preedit)).monospace());
                let query = app.query.text();
                let (_, column) = app.query.cursor();
                let at = query_cursor_byte_index(&query, column);
                let prefix = format!("> {}{}", &query[..at], ime.preedit);
                let width = ui
                    .painter()
                    .layout_no_wrap(
                        prefix,
                        egui::TextStyle::Monospace.resolve(ui.style()),
                        ui.visuals().text_color(),
                    )
                    .size()
                    .x;
                let caret = egui::Rect::from_min_size(
                    egui::pos2(response.rect.min.x + width, response.rect.min.y),
                    egui::vec2(2.0, response.rect.height()),
                );
                ctx.output_mut(|output| {
                    output.ime = Some(egui::output::IMEOutput {
                        rect: response.rect,
                        cursor_rect: caret,
                    });
                });
            } else {
                ime.preedit.clear();
                ime.composing = false;
                ui.label(egui::RichText::new(query_display(app, "")).monospace());
            }
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
            for action in app.actions() {
                let response = ui.button(&action.label);
                if response.clicked_by(egui::PointerButton::Primary)
                    && ui.input(|input| input.pointer.any_click())
                {
                    action_clicked = Some(action.key);
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

fn query_cursor_byte_index(query: &str, column: usize) -> usize {
    query
        .char_indices()
        .nth(column)
        .map_or(query.len(), |(at, _)| at)
}

fn query_display(app: &App, preedit: &str) -> String {
    let query = app.query.text();
    let (_, column) = app.query.cursor();
    let at = query_cursor_byte_index(&query, column);
    format!("> {}{preedit}▏{}", &query[..at], &query[at..])
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

fn picker_events(ctx: &egui::Context, ime: &mut ImeState, accepts_ime: bool) -> Vec<KeyEvent> {
    let events = ctx.input(|input| input.events.clone());
    if !accepts_ime {
        ime.preedit.clear();
        ime.composing = false;
    }
    let text_has_space = events
        .iter()
        .any(|event| matches!(event, egui::Event::Text(text) if text.contains(' ')));
    let commits: Vec<_> = if accepts_ime {
        events
            .iter()
            .filter_map(|event| match event {
                egui::Event::Ime(egui::ImeEvent::Commit(text)) => Some(text.clone()),
                _ => None,
            })
            .collect()
    } else {
        Vec::new()
    };
    let ime_events = accepts_ime
        && events
            .iter()
            .any(|event| matches!(event, egui::Event::Ime(_)));
    let was_composing = ime.composing;
    let mut keys = Vec::new();
    for event in events {
        match event {
            egui::Event::Ime(egui::ImeEvent::Enabled) if accepts_ime => {
                ime.composing = true;
            }
            egui::Event::Ime(egui::ImeEvent::Preedit(text)) if accepts_ime => {
                ime.composing = true;
                ime.preedit = filtered_text(&text);
            }
            egui::Event::Ime(egui::ImeEvent::Commit(text)) if accepts_ime => {
                ime.composing = false;
                ime.preedit.clear();
                keys.extend(text_keys(&text));
            }
            egui::Event::Ime(egui::ImeEvent::Disabled) if accepts_ime => {
                ime.composing = false;
                ime.preedit.clear();
            }
            egui::Event::Text(text) => {
                let duplicate =
                    accepts_ime && (ime.composing || commits.iter().any(|commit| commit == &text));
                if !duplicate {
                    keys.extend(text_keys(&text));
                }
            }
            // egui-winit sends Paste instead of Text for a paste shortcut.
            egui::Event::Paste(text) => keys.extend(text_keys(&text)),
            egui::Event::Key {
                key,
                pressed: true,
                repeat: false,
                ..
            } if accepts_ime && (ime_events || was_composing || ime.composing) => {
                if key == egui::Key::Escape {
                    ime.preedit.clear();
                    ime.composing = false;
                }
            }
            egui::Event::Key {
                key,
                pressed: true,
                repeat: false,
                modifiers,
                ..
            } => keys.extend(egui_key(key, modifiers, text_has_space)),
            egui::Event::MouseWheel { delta, .. } if delta.y > 0.0 => {
                keys.push(KeyEvent::new(KeyCode::Up, KeyModifiers::NONE));
            }
            egui::Event::MouseWheel { delta, .. } if delta.y < 0.0 => {
                keys.push(KeyEvent::new(KeyCode::Down, KeyModifiers::NONE));
            }
            _ => {}
        }
    }
    keys
}

fn filtered_text(text: &str) -> String {
    text.chars()
        .filter(|c| !c.is_control() && !matches!(c, '\u{2028}' | '\u{2029}'))
        .collect()
}

fn text_keys(text: &str) -> impl Iterator<Item = KeyEvent> + '_ {
    text.chars()
        .filter(|c| !c.is_control() && !matches!(c, '\u{2028}' | '\u{2029}'))
        .map(|c| KeyEvent::new(KeyCode::Char(c), KeyModifiers::NONE))
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
