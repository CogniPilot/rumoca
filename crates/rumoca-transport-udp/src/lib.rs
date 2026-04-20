//! UDP lockstep transport.
//!
//! Owns a bound `UdpSocket` plus the destination send address. Handles the
//! non-blocking drain pattern so callers don't have to toggle socket state.

use std::net::UdpSocket;
use std::time::Duration;

use anyhow::{Context, Result};
use serde::Deserialize;

/// Configuration deserialized from `[transport.udp]` or the legacy flat
/// `[udp]` TOML section.
#[derive(Debug, Clone, Deserialize)]
pub struct UdpConfig {
    pub listen: String,
    pub send: String,
}

/// Live UDP transport: bound listen socket + remembered destination.
#[derive(Debug)]
pub struct UdpTransport {
    socket: UdpSocket,
    send_addr: String,
}

impl UdpTransport {
    /// Bind the listen socket and prepare to send to `send`.
    /// Sets a 100 ms read timeout so blocking recv calls don't hang forever.
    pub fn bind(cfg: &UdpConfig) -> Result<Self> {
        let socket = UdpSocket::bind(&cfg.listen)
            .with_context(|| format!("Bind UDP {}", cfg.listen))?;
        socket.set_read_timeout(Some(Duration::from_millis(100)))?;
        Ok(Self {
            socket,
            send_addr: cfg.send.clone(),
        })
    }

    /// Drain any queued datagrams non-blocking. Calls `handle` with each
    /// received slice. Restores blocking mode on exit.
    pub fn drain<F: FnMut(&[u8])>(&self, buf: &mut [u8], mut handle: F) {
        self.socket.set_nonblocking(true).ok();
        while let Ok((n, _)) = self.socket.recv_from(buf) {
            handle(&buf[..n]);
        }
        self.socket.set_nonblocking(false).ok();
    }

    /// Block until a datagram arrives or the socket's read timeout fires.
    /// Returns the byte count, or `None` on timeout/error. Used by lockstep
    /// loops where each physics step is gated on an inbound packet.
    pub fn recv_blocking(&self, buf: &mut [u8]) -> Option<usize> {
        self.socket.recv_from(buf).ok().map(|(n, _)| n)
    }

    /// Send one datagram to the configured destination. Errors are swallowed
    /// — lockstep loops should not abort on transient UDP send failures.
    pub fn send(&self, data: &[u8]) {
        let _ = self.socket.send_to(data, &self.send_addr);
    }

    pub fn listen_addr(&self) -> String {
        self.socket
            .local_addr()
            .map(|a| a.to_string())
            .unwrap_or_default()
    }

    pub fn send_addr(&self) -> &str {
        &self.send_addr
    }
}
