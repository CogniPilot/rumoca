//! UDP lockstep transport.
//!
//! Owns a bound `UdpSocket` plus the destination send address. Handles the
//! non-blocking drain pattern so callers don't have to toggle socket state.

use std::io::ErrorKind;
use std::net::UdpSocket;
use std::time::Duration;

use anyhow::{Context, Result, anyhow, ensure};
use serde::Deserialize;

/// Configuration deserialized from `[transport.udp]`.
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
        let socket =
            UdpSocket::bind(&cfg.listen).with_context(|| format!("Bind UDP {}", cfg.listen))?;
        socket.set_read_timeout(Some(Duration::from_millis(100)))?;
        Ok(Self {
            socket,
            send_addr: cfg.send.clone(),
        })
    }

    /// Drain any queued datagrams non-blocking. Calls `handle` with each
    /// received slice and restores blocking mode before returning. Queue
    /// exhaustion is successful; socket failures remain transport errors.
    pub fn drain<F: FnMut(&[u8])>(&self, buf: &mut [u8], mut handle: F) -> Result<()> {
        self.socket
            .set_nonblocking(true)
            .context("enable non-blocking UDP drain")?;
        let drain_result = loop {
            match self.socket.recv_from(buf) {
                Ok((n, _)) => handle(&buf[..n]),
                Err(error) if error.kind() == ErrorKind::WouldBlock => break Ok(()),
                Err(error) if error.kind() == ErrorKind::Interrupted => {}
                Err(error) => break Err(error).context("drain UDP datagram"),
            }
        };
        let restore_result = self.socket.set_nonblocking(false);
        match (drain_result, restore_result) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(error), Ok(())) => Err(error),
            (Ok(()), Err(error)) => Err(error).context("restore blocking UDP receive"),
            (Err(receive), Err(restore)) => Err(anyhow!(
                "{receive:#}; restoring blocking UDP receive also failed: {restore}"
            )),
        }
    }

    /// Block until a datagram arrives or the socket's read timeout fires.
    /// Returns the byte count, `None` on timeout, and an error for every other
    /// receive failure. Used by lockstep loops where each physics step is gated
    /// on an inbound packet.
    pub fn recv_blocking(&self, buf: &mut [u8]) -> Result<Option<usize>> {
        loop {
            match self.socket.recv_from(buf) {
                Ok((n, _)) => return Ok(Some(n)),
                Err(error)
                    if matches!(error.kind(), ErrorKind::WouldBlock | ErrorKind::TimedOut) =>
                {
                    return Ok(None);
                }
                Err(error) if error.kind() == ErrorKind::Interrupted => {}
                Err(error) => return Err(error).context("receive blocking UDP datagram"),
            }
        }
    }

    /// Send one complete datagram to the configured destination.
    pub fn send(&self, data: &[u8]) -> Result<()> {
        let sent = self
            .socket
            .send_to(data, &self.send_addr)
            .with_context(|| format!("send UDP datagram to {}", self.send_addr))?;
        ensure!(
            sent == data.len(),
            "UDP send reported {sent} bytes for a {}-byte datagram",
            data.len()
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn local_transport(send: impl Into<String>) -> UdpTransport {
        UdpTransport::bind(&UdpConfig {
            listen: "127.0.0.1:0".to_owned(),
            send: send.into(),
        })
        .expect("bind local UDP transport")
    }

    #[test]
    fn blocking_timeout_remains_distinct_from_receive_failure() {
        let transport = local_transport("127.0.0.1:9");
        let mut buffer = [0; 8];
        assert_eq!(
            transport
                .recv_blocking(&mut buffer)
                .expect("an idle socket reaches its configured timeout"),
            None
        );
    }

    #[test]
    fn invalid_send_destination_is_not_reported_as_success() {
        let transport = local_transport("not a socket address");
        let error = transport
            .send(b"frame")
            .expect_err("destination resolution failure remains visible");
        assert!(error.to_string().contains("send UDP datagram"));
    }

    #[test]
    fn drain_treats_only_queue_exhaustion_as_success() {
        let transport = local_transport("127.0.0.1:9");
        let destination = transport.socket.local_addr().expect("local socket address");
        let sender = UdpSocket::bind("127.0.0.1:0").expect("bind UDP sender");
        sender
            .send_to(b"one", destination)
            .expect("send first frame");
        sender
            .send_to(b"two", destination)
            .expect("send second frame");

        let mut buffer = [0; 8];
        let mut frames = Vec::new();
        transport
            .drain(&mut buffer, |frame| frames.push(frame.to_vec()))
            .expect("WouldBlock terminates an otherwise successful drain");
        assert_eq!(frames, [b"one".to_vec(), b"two".to_vec()]);
    }
}
