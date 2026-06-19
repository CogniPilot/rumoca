//! WebSocket broadcast server for Rumoca viewers.
//!
//! A single long-running server: for each incoming WS connection it
//! streams the latest `String` messages from an mpsc channel. Accepts
//! inbound JSON commands with a `realtime` boolean to toggle a shared
//! atomic flag the sim loop observes.

use std::io::{ErrorKind, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, mpsc};
use std::thread;
use std::time::Duration;

use serde::Deserialize;
use tungstenite::{Message, WebSocket, accept};

/// Configuration deserialized from `[transport.websocket]`.
#[derive(Debug, Clone, Deserialize)]
pub struct WsConfig {
    pub port: u16,
}

type WsStream = WebSocket<TcpStream>;

/// Latest simulation frame, shared across every connected viewer. `generation`
/// bumps on each new frame so a client only resends when there's something new;
/// `closed` is set when the sim's state channel ends.
#[derive(Default)]
struct Latest {
    generation: u64,
    frame: Option<String>,
    closed: bool,
}

/// Shared latest-frame slot plus a condvar clients block on for new frames.
type SharedLatest = Arc<(Mutex<Latest>, Condvar)>;

/// Run a broadcast WS server on `port`. Consumes `state_rx` and fans the latest
/// message out to *all* connected viewers, keeping only the latest if the loop
/// produces faster than the browsers consume.
///
/// Accepts client→server JSON commands:
/// - `{"realtime": true|false}` — flips the shared `realtime` flag
/// - `{"quit": true}` — sets `quit` so the sim loop breaks cleanly
pub fn run_broadcast_server(
    port: u16,
    state_rx: mpsc::Receiver<String>,
    control_tx: Option<mpsc::Sender<String>>,
    ready_tx: Option<mpsc::Sender<Result<(), String>>>,
    realtime: Arc<AtomicBool>,
    quit: Arc<AtomicBool>,
) {
    let listener = match TcpListener::bind(format!("0.0.0.0:{port}")) {
        Ok(l) => l,
        Err(e) => {
            let message = format!("Failed to bind WS port {port}: {e}");
            if let Some(tx) = ready_tx {
                let _ = tx.send(Err(message.clone()));
            }
            status_line(&message);
            return;
        }
    };
    status_line(&format!("  WebSocket: ws://0.0.0.0:{port}"));
    if let Some(tx) = ready_tx {
        let _ = tx.send(Ok(()));
    }
    // Fan the single-consumer state channel out to a shared latest-frame slot,
    // so every connected viewer can read it (not just the first one).
    let shared: SharedLatest = Arc::new((Mutex::new(Latest::default()), Condvar::new()));
    {
        let shared = Arc::clone(&shared);
        thread::spawn(move || {
            for json in state_rx.iter() {
                let (lock, cvar) = &*shared;
                let mut latest = lock.lock().unwrap();
                latest.generation = latest.generation.wrapping_add(1);
                latest.frame = Some(json);
                cvar.notify_all();
            }
            // Sender dropped (sim ended): wake clients so they can exit.
            let (lock, cvar) = &*shared;
            lock.lock().unwrap().closed = true;
            cvar.notify_all();
        });
    }

    // One thread per connection. The handshake and per-client streaming run off
    // the accept path, so a slow, half-open, or stale viewer can neither block
    // the accept loop nor starve any other viewer.
    for stream in listener.incoming().flatten() {
        if quit.load(Ordering::Relaxed) {
            return;
        }
        let shared = Arc::clone(&shared);
        let control_tx = control_tx.clone();
        let realtime = Arc::clone(&realtime);
        let quit = Arc::clone(&quit);
        thread::spawn(move || {
            // Bound the handshake: a connection that never upgrades (port scan,
            // browser probe, half-open socket) is dropped instead of hanging.
            stream.set_read_timeout(Some(Duration::from_secs(5))).ok();
            if let Some(ws) = accept_ws(stream) {
                log_status("viewer connected");
                serve_client(ws, &shared, control_tx.as_ref(), &realtime, &quit);
                log_status("viewer disconnected");
            }
        });
    }
}

fn accept_ws(stream: TcpStream) -> Option<WsStream> {
    let ws = match accept(stream) {
        Ok(ws) => ws,
        Err(e) => {
            // Browsers and port scanners may probe the WS port with plain HTTP.
            // Ignore those non-upgrade requests unless verbose WS logging is enabled.
            if tracing::enabled!(target: "rumoca_transport_websocket::ws", tracing::Level::DEBUG) {
                log_status(&format!("handshake error: {e}"));
            }
            return None;
        }
    };
    ws.get_ref().set_nonblocking(true).ok();
    ws.get_ref()
        .set_write_timeout(Some(Duration::from_millis(100)))
        .ok();
    Some(ws)
}

fn serve_client(
    mut ws: WsStream,
    shared: &SharedLatest,
    control_tx: Option<&mpsc::Sender<String>>,
    realtime: &Arc<AtomicBool>,
    quit: &Arc<AtomicBool>,
) {
    let mut last_sent = 0u64;
    loop {
        if !drain_inbound(&mut ws, control_tx, realtime, quit) {
            return;
        }
        if quit.load(Ordering::Relaxed) {
            return;
        }
        match next_frame(shared, &mut last_sent) {
            FrameWait::Closed => return,
            FrameWait::None => {}
            FrameWait::New(json) => {
                if ws.send(Message::Text(json.into())).is_err() {
                    return;
                }
            }
        }
    }
}

enum FrameWait {
    /// A frame newer than the client's last is ready to send.
    New(String),
    /// No new frame within this tick; loop to re-check inbound/quit.
    None,
    /// The sim's state channel closed; the client should stop.
    Closed,
}

/// Block up to ~16 ms (≈60 fps) for a frame newer than `last_sent`, advancing
/// `last_sent` when one is found. Holds only the shared lock, never the socket.
fn next_frame(shared: &SharedLatest, last_sent: &mut u64) -> FrameWait {
    let (lock, cvar) = &**shared;
    let mut latest = lock.lock().unwrap();
    while latest.generation == *last_sent && !latest.closed {
        let (guard, timeout) = cvar
            .wait_timeout(latest, Duration::from_millis(16))
            .unwrap();
        latest = guard;
        if timeout.timed_out() {
            break;
        }
    }
    if latest.generation != *last_sent {
        *last_sent = latest.generation;
        // Cloned so every other connected viewer still sees this frame.
        match latest.frame.clone() {
            Some(json) => FrameWait::New(json),
            None => FrameWait::None,
        }
    } else if latest.closed {
        FrameWait::Closed
    } else {
        FrameWait::None
    }
}

/// Drain pending client→server messages. Returns `false` if the connection closed.
fn drain_inbound(
    ws: &mut WsStream,
    control_tx: Option<&mpsc::Sender<String>>,
    realtime: &Arc<AtomicBool>,
    quit: &Arc<AtomicBool>,
) -> bool {
    loop {
        match ws.read() {
            Ok(Message::Text(text)) => apply_command(&text, control_tx, realtime, quit),
            Ok(Message::Close(_)) => return false,
            Err(tungstenite::Error::Io(ref e)) if e.kind() == ErrorKind::WouldBlock => {
                return true;
            }
            Err(_) => return false,
            _ => {}
        }
    }
}

fn apply_command(
    text: &str,
    control_tx: Option<&mpsc::Sender<String>>,
    realtime: &Arc<AtomicBool>,
    quit: &Arc<AtomicBool>,
) {
    let Ok(cmd) = serde_json::from_str::<serde_json::Value>(text) else {
        return;
    };
    let forwarded_to_runner = if let Some(tx) = control_tx
        && (cmd.get("key").is_some() || cmd.get("quit").and_then(|v| v.as_bool()) == Some(true))
    {
        if tracing::enabled!(target: "rumoca_transport_websocket::viewer_input", tracing::Level::DEBUG)
        {
            log_status("viewer control");
        }
        let _ = tx.send(text.to_string());
        true
    } else {
        false
    };
    if let Some(rt) = cmd.get("realtime").and_then(|v| v.as_bool()) {
        realtime.store(rt, Ordering::Relaxed);
        log_status(&format!("realtime: {rt}"));
    }
    if cmd.get("quit").and_then(|v| v.as_bool()) == Some(true) && !forwarded_to_runner {
        quit.store(true, Ordering::Relaxed);
        log_status("quit requested by viewer");
    }
}

fn log_status(message: &str) {
    eprintln!("\r[WS] {message}                    ");
}

fn status_line(message: &str) {
    let _ = write!(std::io::stderr(), "{message}\r\n");
}
