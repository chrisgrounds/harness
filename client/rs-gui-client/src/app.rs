use crate::worker::{Event, Worker};
use eframe::egui::{self, Color32, FontId, RichText, Stroke, Vec2};
use rs_gui_client::protocol::{MAX_REQUEST_BYTES, Message, encode_request, validate_request};
use std::time::Instant;

const BG: Color32 = Color32::from_rgb(22, 26, 34);
const SURFACE: Color32 = Color32::from_rgb(30, 35, 43);
const BORDER: Color32 = Color32::from_rgb(60, 60, 60);
const MUTED: Color32 = Color32::from_rgb(128, 128, 128);
const ACCENT: Color32 = Color32::from_rgb(236, 91, 43);
const BLUE: Color32 = Color32::from_rgb(86, 182, 194);
const AMBER: Color32 = Color32::from_rgb(229, 192, 123);
const ERROR: Color32 = Color32::from_rgb(224, 108, 117);

#[derive(PartialEq)]
enum Status {
    Connecting,
    Ready,
    Waiting,
    Offline,
}

#[derive(Default)]
struct History {
    entries: Vec<String>,
    cursor: Option<usize>,
    draft: String,
}

impl History {
    fn recall(&mut self, input: &mut String, previous: bool) {
        if self.entries.is_empty() {
            return;
        }
        if previous {
            let index = match self.cursor {
                None => {
                    self.draft = input.clone();
                    self.entries.len() - 1
                }
                Some(index) => index.saturating_sub(1),
            };
            *input = self.entries[index].clone();
            self.cursor = Some(index);
        } else if let Some(index) = self.cursor {
            if index + 1 < self.entries.len() {
                self.cursor = Some(index + 1);
                *input = self.entries[index + 1].clone();
            } else {
                self.cursor = None;
                *input = self.draft.clone();
            }
        }
    }
}

pub struct ChatApp {
    socket_path: String,
    worker: Worker,
    status: Status,
    messages: Vec<Message>,
    input: String,
    history: History,
    error: Option<String>,
    model: Option<String>,
    started: Option<Instant>,
    focus_input: bool,
    session: usize,
}

impl ChatApp {
    pub fn new(cc: &eframe::CreationContext<'_>, socket_path: String) -> Self {
        configure_theme(&cc.egui_ctx);
        let worker = Worker::connect(socket_path.clone(), cc.egui_ctx.clone());
        Self {
            socket_path,
            worker,
            status: Status::Connecting,
            messages: Vec::new(),
            input: String::new(),
            history: History::default(),
            error: None,
            model: None,
            started: None,
            focus_input: true,
            session: 1,
        }
    }

    fn poll(&mut self) {
        while let Ok(event) = self.worker.events.try_recv() {
            match event {
                Event::Connected => {
                    self.status = Status::Ready;
                    self.error = None;
                }
                Event::Reply(reply) => {
                    self.model = reply.model.or(self.model.take());
                    self.messages.extend(reply.messages);
                    self.status = Status::Ready;
                    self.started = None;
                }
                Event::Failed(error) => {
                    let detail = if self.status == Status::Waiting {
                        " The last message may have reached the server; it has not been retried."
                    } else {
                        ""
                    };
                    self.error = Some(format!("{error}{detail}"));
                    self.status = Status::Offline;
                    self.started = None;
                }
            }
        }
    }

    fn submit(&mut self, ctx: &egui::Context) {
        if self.input.trim() == "/quit" {
            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
            return;
        }
        if self.status != Status::Ready {
            return;
        }
        if let Err(error) = validate_request(&self.input) {
            self.error = Some(error);
            return;
        }
        if self.worker.commands.send(self.input.clone()).is_err() {
            self.error = Some("Connection closed. Reconnect to start a new session.".into());
            self.status = Status::Offline;
            return;
        }
        self.messages.push(Message {
            role: "user".into(),
            content: Some(self.input.clone()),
            tool_calls: Vec::new(),
            tokens_per_second: None,
        });
        self.history.entries.push(std::mem::take(&mut self.input));
        self.history.cursor = None;
        self.history.draft.clear();
        self.error = None;
        self.status = Status::Waiting;
        self.started = Some(Instant::now());
        self.focus_input = true;
    }

    fn reconnect(&mut self, ctx: &egui::Context) {
        // The server creates fresh history for every connection; reset the view too.
        self.worker = Worker::connect(self.socket_path.clone(), ctx.clone());
        self.messages.clear();
        self.model = None;
        self.error = None;
        self.status = Status::Connecting;
        self.session += 1;
    }

    fn connection(&mut self, ui: &mut egui::Ui) {
        ui.label(RichText::new("CONNECTION").size(10.0).color(MUTED).strong());
        ui.add_space(10.0);
        let (label, color) = match self.status {
            Status::Connecting => ("Connecting", AMBER),
            Status::Ready => ("Connected", ACCENT),
            Status::Waiting => ("Agent is working", ACCENT),
            Status::Offline => ("Disconnected", AMBER),
        };
        ui.horizontal(|ui| {
            let (rect, _) = ui.allocate_exact_size(Vec2::splat(8.0), egui::Sense::hover());
            ui.painter().circle_filled(rect.center(), 3.0, color);
            ui.label(label);
        });
        ui.add_space(6.0);
        ui.add(
            egui::Label::new(
                RichText::new(&self.socket_path)
                    .monospace()
                    .size(11.0)
                    .color(MUTED),
            )
            .wrap(),
        );
        if self.status == Status::Offline {
            ui.add_space(8.0);
            if ui.button("Reconnect").clicked() {
                self.reconnect(ui.ctx());
            }
            ui.label(
                RichText::new("Reconnect starts a fresh conversation.")
                    .size(11.0)
                    .color(MUTED),
            );
        }
    }

    fn composer(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::bottom("composer")
            .frame(egui::Frame::new().fill(BG).inner_margin(egui::Margin::symmetric(28, 18)))
            .show(ctx, |ui| {
                if let Some(error) = &self.error {
                    egui::Frame::new().fill(Color32::from_rgb(76, 43, 44)).corner_radius(8)
                        .inner_margin(12).show(ui, |ui| {
                            ui.set_width(ui.available_width());
                            ui.label(RichText::new(error).color(ERROR));
                        });
                    ui.add_space(10.0);
                }
                let input_id = ui.make_persistent_id("message_input");
                let focused = ui.memory(|memory| memory.has_focus(input_id));
                let send_key = focused && ui.input_mut(|input| consume_send_key(&mut input.events));
                if focused {
                    let (previous, next) = ui.input_mut(|i| {
                        let prev = i.consume_key(egui::Modifiers::ALT, egui::Key::ArrowUp);
                        let next = i.consume_key(egui::Modifiers::ALT, egui::Key::ArrowDown);
                        (prev, next)
                    });
                    if previous || next {
                        self.history.recall(&mut self.input, previous);
                        if let Some(mut state) = egui::TextEdit::load_state(ctx, input_id) {
                            state.cursor.set_char_range(Some(egui::text::CCursorRange::one(
                                egui::text::CCursor::new(self.input.chars().count()),
                            )));
                            state.store(ctx, input_id);
                        }
                    }
                }
                egui::Frame::new().fill(SURFACE).stroke(Stroke::new(1.0_f32, BORDER))
                    .corner_radius(12).inner_margin(16).show(ui, |ui| {
                        let edit = egui::ScrollArea::vertical().id_salt("draft_scroll")
                            .max_height(130.0).show(ui, |ui| {
                                ui.add(egui::TextEdit::multiline(&mut self.input)
                                    .id(input_id).hint_text("Ask Harness anything…")
                                    .font(FontId::proportional(15.0)).desired_rows(3)
                                    .desired_width(f32::INFINITY).frame(false))
                            }).inner;
                        if self.focus_input { edit.request_focus(); self.focus_input = false; }
                        ui.add_space(8.0);
                        ui.horizontal(|ui| {
                            let bytes = encode_request(&self.input).len();
                            ui.label(RichText::new(format!("{bytes} / {MAX_REQUEST_BYTES} bytes"))
                                .size(11.0).color(if bytes > MAX_REQUEST_BYTES { AMBER } else { MUTED }))
                                .on_hover_text("Encoded message size, including JSON. The server accepts up to 1024 bytes.");
                            ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                                let can_send = self.input.trim() == "/quit" ||
                                    (self.status == Status::Ready && !self.input.trim().is_empty() && bytes <= MAX_REQUEST_BYTES);
                                let button = egui::Button::new(RichText::new("Send").color(BG).strong())
                                    .fill(ACCENT).min_size(Vec2::new(92.0, 32.0)).corner_radius(7);
                                if ui.add_enabled(can_send, button).clicked() || send_key { self.submit(ctx); }
                            });
                        });
                    });
                ui.add_space(8.0);
                ui.label(RichText::new("Enter to send  ·  Shift+Enter for a new line  ·  Alt+Up/Down for history").size(11.0).color(MUTED));
            });
    }
}

impl eframe::App for ChatApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.poll();
        egui::TopBottomPanel::top("header")
            .frame(
                egui::Frame::new()
                    .fill(BG)
                    .inner_margin(egui::Margin::symmetric(28, 20)),
            )
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label(RichText::new("H / ").size(24.0).strong().color(ACCENT));
                    ui.label(RichText::new("harness").size(23.0).strong());
                });
            });

        self.composer(ctx);
        egui::CentralPanel::default()
            .frame(egui::Frame::new().fill(BG).inner_margin(egui::Margin::symmetric(28, 12)))
            .show(ctx, |ui| {
                self.connection(ui);
                ui.add_space(12.0);
                ui.horizontal(|ui| {
                    ui.label(RichText::new("Conversation").size(19.0).strong());
                    ui.add_space(12.0);
                    if let Some(model) = &self.model {
                        ui.label(RichText::new(model).size(11.0).color(MUTED));
                    }
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        ui.label(RichText::new(format!("{} messages", self.messages.len())).size(11.0).color(MUTED));
                    });
                });
                ui.add_space(18.0);
                egui::ScrollArea::vertical().id_salt("conversation").auto_shrink([false, false])
                    .stick_to_bottom(true).show(ui, |ui| {
                        ui.set_width(ui.available_width());
                        if self.messages.is_empty() {
                            ui.add_space(45.0);
                            ui.label(RichText::new("Explore your code, ask a question, or work through an idea.\nYour conversation and tool activity will appear here.").size(15.0).color(MUTED));
                        }
                        for (index, message) in self.messages.iter().enumerate() {
                            ui.push_id((self.session, index), |ui| message_card(ui, message));
                            ui.add_space(12.0);
                        }
                        if let Some(started) = self.started {
                            ui.horizontal(|ui| {
                                ui.spinner();
                                ui.label(RichText::new(format!("Harness is working · {:.0}s", started.elapsed().as_secs_f32())).color(MUTED));
                            });
                            ctx.request_repaint_after(std::time::Duration::from_millis(100));
                        }
                    });
            });
    }
}

// consume_key matches extra Shift/Alt modifiers too, which would send multiline
// drafts on Shift+Enter. Consume only the unmodified event, leaving others intact.
fn consume_send_key(events: &mut Vec<egui::Event>) -> bool {
    let mut send = false;
    events.retain(|event| {
        if matches!(event, egui::Event::Key {
            key: egui::Key::Enter, pressed: true, modifiers, ..
        } if modifiers.is_none())
        {
            send = true;
            false
        } else {
            true
        }
    });
    send
}

fn message_card(ui: &mut egui::Ui, message: &Message) {
    let (label, color) = match message.role.as_str() {
        "user" => ("YOU", BLUE),
        "tool" => ("TOOL RESULT", AMBER),
        _ => ("HARNESS", ACCENT),
    };
    egui::Frame::new()
        .fill(SURFACE)
        .stroke(Stroke::new(1.0_f32, BORDER))
        .corner_radius(10)
        .inner_margin(18)
        .show(ui, |ui| {
            ui.set_width(ui.available_width());
            ui.horizontal(|ui| {
                ui.label(RichText::new(label).size(10.0).strong().color(color));
                if let Some(speed) = message.tokens_per_second {
                    ui.label(
                        RichText::new(format!("·  {speed:.1} tokens/s"))
                            .size(11.0)
                            .color(MUTED),
                    );
                }
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if let Some(content) = &message.content
                        && ui.small_button("Copy").clicked()
                    {
                        ui.ctx().copy_text(content.clone());
                    }
                });
            });
            ui.add_space(8.0);
            if message.role == "tool" {
                egui::CollapsingHeader::new("Output")
                    .default_open(true)
                    .show(ui, |ui| {
                        tool_text(ui, message.content.as_deref().unwrap_or("(no tool output)"));
                    });
            } else if let Some(content) = message.content.as_deref().filter(|s| !s.is_empty()) {
                ui.add(
                    egui::Label::new(RichText::new(content).size(15.0))
                        .wrap()
                        .selectable(true),
                );
            } else if message.tool_calls.is_empty() {
                ui.label(RichText::new("(no text reply)").color(MUTED));
            }
            for (index, call) in message.tool_calls.iter().enumerate() {
                ui.add_space(6.0);
                egui::CollapsingHeader::new(
                    RichText::new(format!("Tool call · {}", call.function.name)).color(AMBER),
                )
                .id_salt(index)
                .default_open(true)
                .show(ui, |ui| {
                    tool_text(ui, &call.function.arguments);
                });
            }
        });
}

fn tool_text(ui: &mut egui::Ui, text: &str) {
    egui::ScrollArea::vertical()
        .max_height(260.0)
        .show(ui, |ui| {
            ui.add(
                egui::Label::new(RichText::new(text).monospace().size(12.0))
                    .wrap()
                    .selectable(true),
            );
        });
}

fn configure_theme(ctx: &egui::Context) {
    let mut visuals = egui::Visuals::dark();
    visuals.panel_fill = BG;
    visuals.window_fill = SURFACE;
    visuals.override_text_color = Some(Color32::from_rgb(238, 238, 238));
    visuals.selection.bg_fill = Color32::from_rgb(73, 42, 36);
    visuals.selection.stroke = Stroke::new(1.0_f32, ACCENT);
    visuals.widgets.inactive.bg_fill = SURFACE;
    visuals.widgets.inactive.weak_bg_fill = SURFACE;
    visuals.widgets.inactive.bg_stroke = Stroke::new(1.0_f32, BORDER);
    visuals.widgets.hovered.bg_fill = Color32::from_rgb(37, 43, 53);
    visuals.widgets.hovered.weak_bg_fill = Color32::from_rgb(37, 43, 53);
    ctx.set_visuals(visuals);
    ctx.style_mut(|style| {
        style.spacing.item_spacing = Vec2::new(10.0, 8.0);
        style.spacing.button_padding = Vec2::new(12.0, 7.0);
    });
}

#[cfg(test)]
mod tests {
    use super::{History, consume_send_key};
    use eframe::egui;

    #[test]
    fn shift_enter_remains_available_for_newlines() {
        let enter = |modifiers| egui::Event::Key {
            key: egui::Key::Enter,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers,
        };
        let mut events = vec![enter(egui::Modifiers::SHIFT)];
        assert!(!consume_send_key(&mut events));
        assert_eq!(events.len(), 1);
        events.push(enter(egui::Modifiers::NONE));
        assert!(consume_send_key(&mut events));
        assert_eq!(events, vec![enter(egui::Modifiers::SHIFT)]);
    }

    #[test]
    fn history_restores_unsent_draft_and_stops_at_boundaries() {
        let mut history = History {
            entries: vec!["first".into(), "second".into()],
            ..Default::default()
        };
        let mut input = "unfinished".into();
        history.recall(&mut input, true);
        assert_eq!(input, "second");
        history.recall(&mut input, true);
        history.recall(&mut input, true);
        assert_eq!(input, "first");
        history.recall(&mut input, false);
        assert_eq!(input, "second");
        history.recall(&mut input, false);
        assert_eq!(input, "unfinished");
    }
}
