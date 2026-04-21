//! WebSocket broadcast server for Rumoca viewers.
//!
//! A single long-running server: for each incoming WS connection it
//! streams the latest `String` messages from an mpsc channel. Accepts
//! inbound JSON commands with a `realtime` boolean to toggle a shared
//! atomic flag the sim loop observes.

use std::io::ErrorKind;
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, mpsc};
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

/// Run a broadcast WS server on `port`. Consumes `state_rx` — forwards each
/// message to the currently-connected client (keeping only the latest if the
/// loop produces faster than the browser consumes).
///
/// Accepts client→server JSON commands:
/// - `{"realtime": true|false}` — flips the shared `realtime` flag
/// - `{"quit": true}` — sets `quit` so the sim loop breaks cleanly
pub fn run_broadcast_server(
    port: u16,
    state_rx: mpsc::Receiver<String>,
    realtime: Arc<AtomicBool>,
    quit: Arc<AtomicBool>,
) {
    let listener = match TcpListener::bind(format!("0.0.0.0:{port}")) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to bind WS port {port}: {e}");
            return;
        }
    };
    eprintln!("  WebSocket: ws://0.0.0.0:{port}");
    for stream in listener.incoming().flatten() {
        eprintln!("[WS] viewer connected");
        if let Some(ws) = accept_ws(stream) {
            serve_client(ws, &state_rx, &realtime, &quit);
        }
        eprintln!("[WS] viewer disconnected");
        if quit.load(Ordering::Relaxed) {
            return;
        }
    }
}

fn accept_ws(stream: TcpStream) -> Option<WsStream> {
    let ws = match accept(stream) {
        Ok(ws) => ws,
        Err(e) => {
            eprintln!("[WS] handshake error: {e}");
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
    state_rx: &mpsc::Receiver<String>,
    realtime: &Arc<AtomicBool>,
    quit: &Arc<AtomicBool>,
) {
    loop {
        if !drain_inbound(&mut ws, realtime, quit) {
            return;
        }
        if quit.load(Ordering::Relaxed) {
            return;
        }
        if !push_latest(&mut ws, state_rx) {
            return;
        }
        thread::sleep(Duration::from_millis(16)); // ~60 fps
    }
}

/// Drain pending client→server messages. Returns `false` if the connection closed.
fn drain_inbound(ws: &mut WsStream, realtime: &Arc<AtomicBool>, quit: &Arc<AtomicBool>) -> bool {
    loop {
        match ws.read() {
            Ok(Message::Text(text)) => apply_command(&text, realtime, quit),
            Ok(Message::Close(_)) => return false,
            Err(tungstenite::Error::Io(ref e)) if e.kind() == ErrorKind::WouldBlock => {
                return true;
            }
            Err(_) => return false,
            _ => {}
        }
    }
}

fn apply_command(text: &str, realtime: &Arc<AtomicBool>, quit: &Arc<AtomicBool>) {
    let Ok(cmd) = serde_json::from_str::<serde_json::Value>(text) else {
        return;
    };
    if let Some(rt) = cmd.get("realtime").and_then(|v| v.as_bool()) {
        realtime.store(rt, Ordering::Relaxed);
        eprintln!("\r[WS] realtime: {rt}                    \r");
    }
    if cmd.get("quit").and_then(|v| v.as_bool()) == Some(true) {
        quit.store(true, Ordering::Relaxed);
        eprintln!("\r[WS] quit requested by viewer                    \r");
    }
}

/// Drain the state channel and push only the latest JSON. Returns `false` if
/// the send failed.
fn push_latest(ws: &mut WsStream, state_rx: &mpsc::Receiver<String>) -> bool {
    let mut latest: Option<String> = None;
    while let Ok(json) = state_rx.try_recv() {
        latest = Some(json);
    }
    match latest {
        Some(json) => ws.send(Message::Text(json.into())).is_ok(),
        None => true,
    }
}
