//! Candidates that arrive over time.
//!
//! Lets the picker open and stay interactive while whatever produces its
//! candidates is still running.

use std::sync::mpsc::{Receiver, TryRecvError};

use crate::candidate::Candidate;

/// A source of candidates that is still filling.
///
/// The picker knows nothing about processes: it drains a channel and, if the
/// user cancels, calls `cancel` so the producer can stop.
pub struct CandidateStream {
    /// Candidates as they arrive.
    receiver: Receiver<Candidate>,
    /// Stops the producer; called once, when the pick is cancelled.
    cancel: Option<Box<dyn FnOnce() + Send>>,
    /// Set once the sender has been dropped.
    finished: bool,
}

impl CandidateStream {
    /// Builds a stream from a receiver and the way to stop its producer.
    pub fn new(receiver: Receiver<Candidate>, cancel: Box<dyn FnOnce() + Send>) -> Self {
        Self {
            receiver,
            cancel: Some(cancel),
            finished: false,
        }
    }

    /// A stream over candidates that are already all present.
    pub fn of(candidates: Vec<Candidate>) -> Self {
        let (sender, receiver) = std::sync::mpsc::channel();
        for candidate in candidates {
            // The receiver is alive, so this cannot fail.
            let _ = sender.send(candidate);
        }
        drop(sender);
        Self::new(receiver, Box::new(|| {}))
    }

    /// Takes whatever has arrived, without waiting.
    pub fn drain(&mut self) -> Vec<Candidate> {
        let mut out = Vec::new();
        loop {
            match self.receiver.try_recv() {
                Ok(candidate) => out.push(candidate),
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    self.finished = true;
                    break;
                }
            }
        }
        out
    }

    /// Blocks until the producer is done, returning everything it sent.
    pub fn collect(&mut self) -> Vec<Candidate> {
        let mut out = Vec::new();
        while let Ok(candidate) = self.receiver.recv() {
            out.push(candidate);
        }
        self.finished = true;
        out
    }

    /// Whether the producer has finished.
    pub const fn is_finished(&self) -> bool {
        self.finished
    }

    /// Stops the producer. Doing it twice is harmless.
    pub fn cancel(&mut self) {
        if let Some(cancel) = self.cancel.take() {
            cancel();
        }
    }
}

/// Stopping the producer is the stream's responsibility, not its caller's.
///
/// A pick that fails rather than returning a selection used to leave the
/// command feeding it running — and since it leads its own process group, it
/// outlived nixon.
impl Drop for CandidateStream {
    fn drop(&mut self) {
        self.cancel();
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};

    use super::CandidateStream;
    use crate::candidate::Candidate;

    #[test]
    fn a_ready_stream_drains_everything_at_once() {
        let mut stream =
            CandidateStream::of(vec![Candidate::identity("one"), Candidate::identity("two")]);
        let drained: Vec<String> = stream.drain().into_iter().map(|c| c.value).collect();
        assert_eq!(drained, ["one", "two"]);
        assert!(stream.is_finished());
    }

    #[test]
    fn draining_takes_only_what_has_arrived() {
        let (sender, receiver) = std::sync::mpsc::channel();
        let mut stream = CandidateStream::new(receiver, Box::new(|| {}));

        assert!(stream.drain().is_empty());
        assert!(!stream.is_finished());

        sender.send(Candidate::identity("one")).unwrap();
        assert_eq!(stream.drain().len(), 1);
        assert!(!stream.is_finished(), "the producer is still alive");

        drop(sender);
        assert!(stream.drain().is_empty());
        assert!(stream.is_finished());
    }

    #[test]
    fn collecting_waits_for_the_producer() {
        let (sender, receiver) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            for value in ["one", "two", "three"] {
                std::thread::sleep(std::time::Duration::from_millis(5));
                let _ = sender.send(Candidate::identity(value));
            }
        });

        let mut stream = CandidateStream::new(receiver, Box::new(|| {}));
        let collected: Vec<String> = stream.collect().into_iter().map(|c| c.value).collect();
        assert_eq!(collected, ["one", "two", "three"]);
        assert!(stream.is_finished());
    }

    #[test]
    fn dropping_a_stream_stops_its_producer() {
        let stopped = Arc::new(AtomicBool::new(false));
        let flag = Arc::clone(&stopped);
        let (_sender, receiver) = std::sync::mpsc::channel();

        drop(CandidateStream::new(
            receiver,
            Box::new(move || flag.store(true, Ordering::SeqCst)),
        ));

        assert!(
            stopped.load(Ordering::SeqCst),
            "the producer outlived the stream it was feeding"
        );
    }

    #[test]
    fn cancelling_stops_the_producer_once() {
        let stopped = Arc::new(AtomicBool::new(false));
        let flag = Arc::clone(&stopped);
        let (_sender, receiver) = std::sync::mpsc::channel();
        let mut stream = CandidateStream::new(
            receiver,
            Box::new(move || flag.store(true, Ordering::SeqCst)),
        );

        stream.cancel();
        assert!(stopped.load(Ordering::SeqCst));
        // A second cancel must not panic on the consumed callback.
        stream.cancel();
    }
}
