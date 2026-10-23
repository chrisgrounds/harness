use eframe::egui;
use rs_gui_client::protocol::{Reply, Session};
use std::{
    net::Shutdown,
    os::unix::net::UnixStream,
    sync::{Arc, Mutex, mpsc},
    thread,
};

pub enum Event {
    Connected,
    Reply(Reply),
    Failed(String),
}

/// All socket I/O stays off the UI thread. Dropping the worker cancels a read.
pub struct Worker {
    pub commands: mpsc::Sender<String>,
    pub events: mpsc::Receiver<Event>,
    socket: Arc<Mutex<Option<UnixStream>>>,
}

impl Worker {
    pub fn connect(path: String, ctx: egui::Context) -> Self {
        let (commands, requests) = mpsc::channel::<String>();
        let (events, receiver) = mpsc::channel();
        let socket = Arc::new(Mutex::new(None));
        let shared = Arc::clone(&socket);
        thread::spawn(move || {
            let emit = |event| {
                let _ = events.send(event);
                ctx.request_repaint();
            };
            let run = || -> Result<(), String> {
                let stream = UnixStream::connect(&path)
                    .map_err(|e| format!("Cannot connect to {path}: {e}"))?;
                *shared.lock().unwrap() = Some(stream.try_clone().map_err(|e| e.to_string())?);
                emit(Event::Connected);
                let mut session = Session::new(stream);
                while let Ok(input) = requests.recv() {
                    emit(Event::Reply(session.send(&input)?));
                }
                Ok(())
            };
            if let Err(error) = run() {
                emit(Event::Failed(error));
            }
        });
        Self {
            commands,
            events: receiver,
            socket,
        }
    }
}

impl Drop for Worker {
    fn drop(&mut self) {
        if let Some(socket) = self.socket.lock().unwrap().take() {
            let _ = socket.shutdown(Shutdown::Both);
        }
    }
}
