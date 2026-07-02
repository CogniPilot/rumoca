//! Zenoh packet transport.
//!
//! The runner hands this crate already-packed FlatBuffer bytes. This transport
//! owns the Zenoh session, declared publishers, and declared subscribers.

use std::collections::HashMap;
use std::time::Duration;

use anyhow::Result;
use serde::Deserialize;
use zenoh::handlers::{FifoChannel, FifoChannelHandler};
use zenoh::{
    Wait,
    pubsub::{Publisher, Subscriber},
    sample::Sample,
};

/// Configuration deserialized from `[transport.zenoh]`.
#[derive(Debug, Clone, Deserialize)]
pub struct ZenohConfig {
    /// Zenoh mode: `peer`, `client`, or `router`. Defaults to Zenoh's default.
    #[serde(default)]
    pub mode: Option<String>,
    /// Endpoint to connect to, e.g. `tcp/192.168.10.50:7447`.
    #[serde(default)]
    pub endpoint: Option<String>,
}

/// Live Zenoh transport with declared publishers for configured messages.
#[derive(Debug)]
pub struct ZenohTransport {
    _session: zenoh::Session,
    publishers: HashMap<String, Publisher<'static>>,
    subscribers: HashMap<String, Subscriber<FifoChannelHandler<Sample>>>,
    publish_keys: HashMap<String, String>,
    subscribe_keys: HashMap<String, String>,
}

impl ZenohTransport {
    /// Open a Zenoh session and declare one publisher per `publish` entry.
    pub fn open(
        cfg: &ZenohConfig,
        publish: &HashMap<String, String>,
        subscribe: &HashMap<String, String>,
    ) -> Result<Self> {
        let mut zcfg = zenoh::Config::default();
        if let Some(mode) = &cfg.mode {
            zcfg.insert_json5("mode", &serde_json::to_string(mode)?)
                .map_err(|err| anyhow::anyhow!("Set Zenoh mode '{mode}': {err}"))?;
        }
        if let Some(endpoint) = &cfg.endpoint {
            let endpoints = serde_json::to_string(&[endpoint])?;
            zcfg.insert_json5("connect/endpoints", &endpoints)
                .map_err(|err| anyhow::anyhow!("Set Zenoh endpoint '{endpoint}': {err}"))?;
        }

        let session = zenoh::open(zcfg)
            .wait()
            .map_err(|err| anyhow::anyhow!("Open Zenoh session: {err}"))?;
        let mut publishers = HashMap::new();
        let mut publish_keys = HashMap::new();
        for (message, key) in publish {
            let publisher = session
                .declare_publisher(key.to_owned())
                .wait()
                .map_err(|err| anyhow::anyhow!("Declare Zenoh publisher '{key}': {err}"))?;
            publishers.insert(message.to_owned(), publisher);
            publish_keys.insert(message.to_owned(), key.to_owned());
        }
        let mut subscribers = HashMap::new();
        let mut subscribe_keys = HashMap::new();
        for (message, key) in subscribe {
            let subscriber = session
                .declare_subscriber(key.to_owned())
                .with(FifoChannel::default())
                .wait()
                .map_err(|err| anyhow::anyhow!("Declare Zenoh subscriber '{key}': {err}"))?;
            subscribers.insert(message.to_owned(), subscriber);
            subscribe_keys.insert(message.to_owned(), key.to_owned());
        }
        Ok(Self {
            _session: session,
            publishers,
            subscribers,
            publish_keys,
            subscribe_keys,
        })
    }

    /// Publish one byte payload for the configured message name.
    pub fn publish(&self, message: &str, data: &[u8]) {
        if let Some(publisher) = self.publishers.get(message) {
            let _ = publisher.put(data.to_vec()).wait();
        }
    }

    /// Drain queued samples for a subscribed message.
    pub fn drain<F: FnMut(&[u8])>(&self, message: &str, mut handle: F) {
        let Some(subscriber) = self.subscribers.get(message) else {
            return;
        };
        for sample in subscriber.handler().drain() {
            let bytes = sample.payload().to_bytes();
            handle(bytes.as_ref());
        }
    }

    /// Wait for a subscribed message, then collapse any queued backlog to the
    /// latest sample. Returns `None` on timeout or receive error.
    pub fn recv_latest_blocking(&self, message: &str, timeout: Duration) -> Option<Vec<u8>> {
        let subscriber = self.subscribers.get(message)?;
        let sample = subscriber.handler().recv_timeout(timeout).ok()??;
        let mut latest = sample.payload().to_bytes().into_owned();
        for sample in subscriber.handler().drain() {
            latest = sample.payload().to_bytes().into_owned();
        }
        Some(latest)
    }

    pub fn publish_key_for(&self, message: &str) -> Option<&str> {
        self.publish_keys.get(message).map(String::as_str)
    }

    pub fn subscribe_key_for(&self, message: &str) -> Option<&str> {
        self.subscribe_keys.get(message).map(String::as_str)
    }
}
