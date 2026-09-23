//! A channel bridge between Nixon's worker and a future GUI event loop.

use std::io;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{
    self, Receiver, RecvError, SendError, Sender, SyncSender, TryRecvError, TrySendError,
};
use std::thread;
use std::time::Duration;

use nixon_picker::{
    Candidate, CandidateStream, Picker, PickerOptions, Selection, exact_selection, unique_selection,
};

const CLOSED_CHECK_INTERVAL: Duration = Duration::from_millis(20);
const STREAM_BATCH: usize = 256;

/// Worker-side picker that waits for the GUI to answer each request.
///
/// Streaming picks forward bounded batches while the GUI remains interactive.
#[derive(Debug)]
pub struct GuiPicker {
    requests: Sender<PickerRequest>,
    closed: Arc<AtomicBool>,
}

/// UI-side receiver for pick and confirm requests.
#[derive(Debug)]
pub struct GuiPickerRequests {
    requests: Receiver<PickerRequest>,
    closed: Arc<AtomicBool>,
}

/// A typed request sent to the GUI event loop.
#[derive(Debug)]
pub enum PickerRequest {
    /// Choose from candidates without option toggles.
    Pick(PickRequest),
    /// Choose candidates and return the final toggle state.
    PickOptions(PickOptionsRequest),
    /// Pick from candidates that are still arriving.
    StreamOptions(StreamOptionsRequest),
    /// Ask about toggles without a candidate list.
    Confirm(ConfirmRequest),
}

/// One update from a live candidate source.
#[derive(Debug)]
pub enum StreamUpdate {
    /// Candidates in producer order.
    Candidates(Vec<Candidate>),
    /// The source closed, with its final status.
    Finished(std::result::Result<(), String>),
}

/// A streaming selection request.
#[derive(Debug)]
pub struct StreamOptionsRequest {
    /// Picker matching and controls.
    pub options: PickerOptions,
    /// Live candidate batches.
    pub updates: Receiver<StreamUpdate>,
    pub(crate) reply: Sender<io::Result<PickOptionsReply>>,
}

/// A candidate selection request.
#[derive(Debug)]
pub struct PickRequest {
    /// Picker prompt, query, matching rules, and controls.
    pub options: PickerOptions,
    /// Rows offered for selection.
    pub candidates: Vec<Candidate>,
    pub(crate) reply: Sender<Selection<Candidate>>,
}

impl PickRequest {
    /// Returns the chosen rows or cancellation to the worker.
    pub fn respond(
        self,
        selection: Selection<Candidate>,
    ) -> Result<(), SendError<Selection<Candidate>>> {
        self.reply.send(selection)
    }
}

/// A selection request with option toggles.
#[derive(Debug)]
pub struct PickOptionsRequest {
    /// Picker prompt, query, matching rules, and initial toggles.
    pub options: PickerOptions,
    /// Rows offered for selection.
    pub candidates: Vec<Candidate>,
    pub(crate) reply: Sender<PickOptionsReply>,
}

impl PickOptionsRequest {
    /// Returns the selection and final toggle state to the worker.
    pub fn respond(self, reply: PickOptionsReply) -> Result<(), SendError<PickOptionsReply>> {
        self.reply.send(reply)
    }
}

/// The result of a selection with toggles.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PickOptionsReply {
    /// Selected rows, empty result, or cancellation.
    pub selection: Selection<Candidate>,
    /// Toggle values in the order of [`PickerOptions::options`].
    pub toggles: Vec<bool>,
}

/// An option confirmation request without candidates.
#[derive(Debug)]
pub struct ConfirmRequest {
    /// Prompt and initial toggle state.
    pub options: PickerOptions,
    pub(crate) reply: Sender<Option<Vec<bool>>>,
}

impl ConfirmRequest {
    /// Returns final toggles, or `None` for cancellation.
    pub fn respond(self, toggles: Option<Vec<bool>>) -> Result<(), SendError<Option<Vec<bool>>>> {
        self.reply.send(toggles)
    }
}

impl GuiPicker {
    /// Creates the worker-side picker and UI-side request receiver.
    #[must_use]
    pub fn channel() -> (Self, GuiPickerRequests) {
        let (requests, receiver) = mpsc::channel();
        let closed = Arc::new(AtomicBool::new(false));
        (
            Self {
                requests,
                closed: Arc::clone(&closed),
            },
            GuiPickerRequests {
                requests: receiver,
                closed,
            },
        )
    }

    fn exchange<T>(&self, request: impl FnOnce(Sender<T>) -> PickerRequest) -> Option<T> {
        if self.closed.load(Ordering::Acquire) {
            return None;
        }
        let (reply, receiver) = mpsc::channel();
        self.requests.send(request(reply)).ok()?;
        loop {
            match receiver.try_recv() {
                Ok(answer) => return Some(answer),
                Err(TryRecvError::Disconnected) => return None,
                Err(TryRecvError::Empty) => {}
            }
            if self.closed.load(Ordering::Acquire) {
                return None;
            }
            match receiver.recv_timeout(CLOSED_CHECK_INTERVAL) {
                Ok(answer) => return Some(answer),
                Err(mpsc::RecvTimeoutError::Disconnected) => return None,
                Err(mpsc::RecvTimeoutError::Timeout) => {}
            }
        }
    }
}

impl GuiPickerRequests {
    /// Waits for the next worker request.
    pub fn recv(&self) -> Result<PickerRequest, RecvError> {
        self.requests.recv()
    }

    /// Checks for a request without blocking the GUI event loop.
    pub fn try_recv(&self) -> Result<PickerRequest, TryRecvError> {
        self.requests.try_recv()
    }
}

impl Drop for GuiPickerRequests {
    fn drop(&mut self) {
        self.closed.store(true, Ordering::Release);
    }
}

impl Picker for GuiPicker {
    fn pick_stream(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<Selection<Candidate>> {
        self.pick_stream_options(options, stream)
            .map(|(selection, _)| selection)
    }

    fn pick(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<Selection<Candidate>> {
        if let Some(selection) = settled(options, &candidates) {
            return Ok(selection);
        }
        Ok(self
            .exchange(|reply| {
                PickerRequest::Pick(PickRequest {
                    options: options.clone(),
                    candidates,
                    reply,
                })
            })
            .unwrap_or(Selection::Canceled))
    }

    fn pick_options(
        &mut self,
        options: &PickerOptions,
        candidates: Vec<Candidate>,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        if let Some(selection) = settled(options, &candidates) {
            return Ok((selection, option_state(options)));
        }
        let answer = self.exchange(|reply| {
            PickerRequest::PickOptions(PickOptionsRequest {
                options: options.clone(),
                candidates,
                reply,
            })
        });
        Ok(match answer {
            Some(reply) => (reply.selection, reply.toggles),
            None => (Selection::Canceled, option_state(options)),
        })
    }

    fn pick_stream_options(
        &mut self,
        options: &PickerOptions,
        stream: &mut CandidateStream,
    ) -> io::Result<(Selection<Candidate>, Vec<bool>)> {
        let (updates_sender, updates) = mpsc::sync_channel(4);
        let (stop_sender, stop) = mpsc::channel();
        let answer = thread::scope(|scope| {
            let forwarder = scope.spawn(|| forward_stream(stream, updates_sender, stop));
            let answer = self.exchange(|reply| {
                PickerRequest::StreamOptions(StreamOptionsRequest {
                    options: options.clone(),
                    updates,
                    reply,
                })
            });
            let _ = stop_sender.send(());
            let _ = forwarder.join();
            answer
        });
        match answer {
            Some(Ok(reply)) => Ok((reply.selection, reply.toggles)),
            Some(Err(error)) => Err(error),
            None => Ok((Selection::Canceled, option_state(options))),
        }
    }

    fn confirm(&mut self, options: &PickerOptions) -> io::Result<Option<Vec<bool>>> {
        if options.options.is_empty() {
            return Ok(Some(Vec::new()));
        }
        Ok(self
            .exchange(|reply| {
                PickerRequest::Confirm(ConfirmRequest {
                    options: options.clone(),
                    reply,
                })
            })
            .flatten())
    }
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "the forwarding thread must own both channels so disconnect wakes the GUI"
)]
fn forward_stream(
    stream: &mut CandidateStream,
    updates: SyncSender<StreamUpdate>,
    stop: Receiver<()>,
) {
    loop {
        if stop.try_recv().is_ok() {
            stream.cancel();
            return;
        }
        let candidates = stream.drain_up_to(STREAM_BATCH);
        if !candidates.is_empty()
            && !send_update(&updates, &stop, StreamUpdate::Candidates(candidates))
        {
            stream.cancel();
            return;
        }
        if let Some(result) = stream.completion() {
            let exited = result.is_ok();
            let status = match result {
                Ok(0) => Ok(()),
                Ok(code) => Err(format!("Candidate producer exited with status {code}")),
                Err(error) => Err(format!("Candidate producer failed: {error}")),
            };
            if exited {
                stream.disarm();
            }
            let _ = send_update(&updates, &stop, StreamUpdate::Finished(status));
            return;
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn send_update(
    updates: &SyncSender<StreamUpdate>,
    stop: &Receiver<()>,
    mut update: StreamUpdate,
) -> bool {
    loop {
        match updates.try_send(update) {
            Ok(()) => return true,
            Err(TrySendError::Full(pending)) => update = pending,
            Err(TrySendError::Disconnected(_)) => return false,
        }
        if stop.try_recv().is_ok() {
            return false;
        }
        thread::sleep(Duration::from_millis(10));
    }
}

fn settled(options: &PickerOptions, candidates: &[Candidate]) -> Option<Selection<Candidate>> {
    exact_selection(options, candidates).or_else(|| unique_selection(options, candidates))
}

fn option_state(options: &PickerOptions) -> Vec<bool> {
    options.options.iter().map(|option| option.on).collect()
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    use nixon_picker::{
        Candidate, CandidateStream, Picker, PickerOption, PickerOptions, Selection, SelectionType,
    };

    use super::{GuiPicker, PickOptionsReply, PickerRequest, StreamUpdate};

    fn candidates(values: &[&str]) -> Vec<Candidate> {
        values
            .iter()
            .map(|value| Candidate::identity(*value))
            .collect()
    }

    fn selected(candidate: Candidate) -> Selection<Candidate> {
        Selection::selected(SelectionType::Default, vec![candidate])
    }

    #[test]
    fn fake_ui_thread_answers_a_pick() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            let PickerRequest::Pick(request) = requests.recv().unwrap() else {
                panic!("expected pick request");
            };
            assert_eq!(request.options.header.as_deref(), Some("Commands"));
            assert_eq!(request.candidates.len(), 2);
            let choice = request.candidates[1].clone();
            request.respond(selected(choice)).unwrap();
        });

        let answer = picker
            .pick(
                &PickerOptions::default().header("Commands"),
                candidates(&["one", "two"]),
            )
            .unwrap();
        assert_eq!(answer.items()[0].value, "two");
        ui.join().unwrap();
    }

    #[test]
    fn one_ui_receiver_serves_nested_sequential_picks() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            for expected in ["Projects", "Commands"] {
                let PickerRequest::Pick(request) = requests.recv().unwrap() else {
                    panic!("expected pick request");
                };
                assert_eq!(request.options.header.as_deref(), Some(expected));
                let choice = request.candidates[0].clone();
                request.respond(selected(choice)).unwrap();
            }
        });

        let project = picker
            .pick(
                &PickerOptions::default().header("Projects"),
                candidates(&["alpha"]),
            )
            .unwrap();
        assert_eq!(project.items()[0].value, "alpha");
        let command = picker
            .pick(
                &PickerOptions::default().header("Commands"),
                candidates(&["build"]),
            )
            .unwrap();
        assert_eq!(command.items()[0].value, "build");
        ui.join().unwrap();
    }

    #[test]
    fn fake_ui_thread_returns_toggles_from_pick_and_confirm() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            let PickerRequest::PickOptions(request) = requests.recv().unwrap() else {
                panic!("expected pick with options");
            };
            assert_eq!(
                request
                    .options
                    .options
                    .iter()
                    .map(|option| option.on)
                    .collect::<Vec<_>>(),
                vec![false, true]
            );
            let choice = request.candidates[0].clone();
            request
                .respond(PickOptionsReply {
                    selection: selected(choice),
                    toggles: vec![true, false],
                })
                .unwrap();

            let PickerRequest::Confirm(request) = requests.recv().unwrap() else {
                panic!("expected confirm request");
            };
            assert_eq!(request.options.options.len(), 2);
            request.respond(Some(vec![true, true])).unwrap();
        });

        let options = PickerOptions::default().options(vec![
            PickerOption::new("--force", false),
            PickerOption::new("--dry-run", true),
        ]);
        let (selection, toggles) = picker.pick_options(&options, candidates(&["run"])).unwrap();
        assert_eq!(selection.items()[0].value, "run");
        assert_eq!(toggles, [true, false]);
        assert_eq!(picker.confirm(&options).unwrap(), Some(vec![true, true]));
        ui.join().unwrap();
    }

    #[test]
    fn exact_and_unique_initial_queries_settle_without_ui_requests() {
        let (mut picker, requests) = GuiPicker::channel();
        let exact = PickerOptions::default().query("build").select_exact(true);
        let selection = picker
            .pick(&exact, candidates(&["build-all", "build"]))
            .unwrap();
        assert_eq!(selection.items()[0].value, "build");

        let unique = PickerOptions::default()
            .query("depl")
            .select_one(true)
            .options(vec![PickerOption::new("--force", true)]);
        let (selection, toggles) = picker
            .pick_options(&unique, candidates(&["build", "deploy"]))
            .unwrap();
        assert_eq!(selection.items()[0].value, "deploy");
        assert_eq!(toggles, [true]);

        let missing = PickerOptions::default().query("missing").select_one(true);
        assert_eq!(
            picker.pick(&missing, candidates(&["build"])).unwrap(),
            Selection::Empty
        );
        assert_eq!(
            picker.confirm(&PickerOptions::default()).unwrap(),
            Some(vec![])
        );
        assert!(matches!(
            requests.try_recv(),
            Err(mpsc::TryRecvError::Empty)
        ));
    }

    #[test]
    fn ready_stream_uses_live_request_and_keeps_selection() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            let PickerRequest::StreamOptions(request) = requests.recv().unwrap() else {
                panic!("expected stream request");
            };
            let StreamUpdate::Candidates(candidates) = request
                .updates
                .recv_timeout(Duration::from_secs(1))
                .unwrap()
            else {
                panic!("expected candidates");
            };
            assert_eq!(candidates.len(), 2);
            request
                .reply
                .send(Ok(PickOptionsReply {
                    selection: selected(candidates[1].clone()),
                    toggles: Vec::new(),
                }))
                .unwrap();
        });
        let mut stream = CandidateStream::of(candidates(&["one", "two"]));
        let answer = picker
            .pick_stream(&PickerOptions::default(), &mut stream)
            .unwrap();
        assert_eq!(answer.items()[0].value, "two");
        ui.join().unwrap();
    }

    #[test]
    fn stream_with_options_returns_changed_toggles() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            let PickerRequest::StreamOptions(request) = requests.recv().unwrap() else {
                panic!("expected stream with options");
            };
            let StreamUpdate::Candidates(candidates) = request
                .updates
                .recv_timeout(Duration::from_secs(1))
                .unwrap()
            else {
                panic!("expected candidates");
            };
            assert_eq!(candidates.len(), 2);
            request
                .reply
                .send(Ok(PickOptionsReply {
                    selection: selected(candidates[1].clone()),
                    toggles: vec![true],
                }))
                .unwrap();
        });
        let options = PickerOptions::default().options(vec![PickerOption::new("--force", false)]);
        let mut stream = CandidateStream::of(candidates(&["one", "two"]));
        let (selection, toggles) = picker.pick_stream_options(&options, &mut stream).unwrap();
        assert_eq!(selection.items()[0].value, "two");
        assert_eq!(toggles, [true]);
        ui.join().unwrap();
    }

    #[test]
    fn early_stream_selection_and_cancel_stop_producer_once() {
        for canceled in [false, true] {
            let (mut picker, requests) = GuiPicker::channel();
            let (sender, receiver) = mpsc::channel();
            let stops = Arc::new(AtomicUsize::new(0));
            let count = Arc::clone(&stops);
            let mut stream = CandidateStream::new(
                receiver,
                Box::new(move || {
                    count.fetch_add(1, Ordering::SeqCst);
                }),
            );
            sender.send(Candidate::identity("first")).unwrap();
            let ui = thread::spawn(move || {
                let PickerRequest::StreamOptions(request) = requests.recv().unwrap() else {
                    panic!("expected stream request");
                };
                let StreamUpdate::Candidates(candidates) = request
                    .updates
                    .recv_timeout(Duration::from_secs(1))
                    .unwrap()
                else {
                    panic!("expected first candidate");
                };
                assert_eq!(candidates[0].value, "first");
                request
                    .reply
                    .send(Ok(PickOptionsReply {
                        selection: if canceled {
                            Selection::Canceled
                        } else {
                            selected(candidates[0].clone())
                        },
                        toggles: Vec::new(),
                    }))
                    .unwrap();
            });
            let (selection, _) = picker
                .pick_stream_options(&PickerOptions::default(), &mut stream)
                .unwrap();
            assert_eq!(matches!(selection, Selection::Canceled), canceled);
            drop(stream);
            assert_eq!(stops.load(Ordering::SeqCst), 1);
            ui.join().unwrap();
            drop(sender);
        }
    }

    #[test]
    fn closing_stream_ui_releases_worker_and_stops_producer_once() {
        let (mut picker, requests) = GuiPicker::channel();
        let (_sender, receiver) = mpsc::channel();
        let stops = Arc::new(AtomicUsize::new(0));
        let count = Arc::clone(&stops);
        let worker = thread::spawn(move || {
            let mut stream = CandidateStream::new(
                receiver,
                Box::new(move || {
                    count.fetch_add(1, Ordering::SeqCst);
                }),
            );
            picker
                .pick_stream_options(&PickerOptions::default(), &mut stream)
                .unwrap()
                .0
        });
        let held = requests.recv().unwrap();
        drop(requests);
        assert_eq!(worker.join().unwrap(), Selection::Canceled);
        assert_eq!(stops.load(Ordering::SeqCst), 1);
        drop(held);
    }

    #[test]
    fn cancellation_and_disconnection_return_without_blocking() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            let PickerRequest::Pick(request) = requests.recv().unwrap() else {
                panic!("expected pick request");
            };
            request.respond(Selection::Canceled).unwrap();
        });
        assert_eq!(
            picker
                .pick(&PickerOptions::default(), candidates(&["one"]))
                .unwrap(),
            Selection::Canceled
        );
        ui.join().unwrap();
        assert_eq!(
            picker
                .pick(&PickerOptions::default(), candidates(&["one"]))
                .unwrap(),
            Selection::Canceled
        );
    }

    #[test]
    fn closing_the_ui_cancels_even_when_it_still_holds_a_request() {
        let (mut picker, requests) = GuiPicker::channel();
        let (done_sender, done_receiver) = mpsc::channel();
        let worker = thread::spawn(move || {
            let selection = picker
                .pick(&PickerOptions::default(), candidates(&["one"]))
                .unwrap();
            done_sender.send(selection).unwrap();
        });
        let held_request = requests.recv().unwrap();
        drop(requests);
        assert_eq!(
            done_receiver.recv_timeout(Duration::from_secs(1)).unwrap(),
            Selection::Canceled
        );
        drop(held_request);
        worker.join().unwrap();
    }

    #[test]
    fn dropped_reply_cancels_confirm() {
        let (mut picker, requests) = GuiPicker::channel();
        let ui = thread::spawn(move || {
            let PickerRequest::Confirm(request) = requests.recv().unwrap() else {
                panic!("expected confirm request");
            };
            drop(request);
        });
        let options = PickerOptions::default().options(vec![PickerOption::new("--force", true)]);
        assert_eq!(picker.confirm(&options).unwrap(), None);
        ui.join().unwrap();
    }
}
