//! Fail-closed WebSocket broadcast transport for Rumoca viewers.
//!
//! The listener owns socket-failure classification. Callers receive closed,
//! typed peer-failure records or typed terminal server failures; they never
//! inspect OS or Tungstenite error kinds.

use std::io;
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, mpsc};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use serde_json::Value;
use tungstenite::error::Error as WsError;
use tungstenite::handshake::HandshakeError;
use tungstenite::protocol::WebSocketConfig;
use tungstenite::{Message, WebSocket, accept_with_config};

type WsStream = WebSocket<TcpStream>;

const ACCEPT_POLL_INTERVAL: Duration = Duration::from_millis(10);
const FRAME_WAIT_INTERVAL: Duration = Duration::from_millis(16);
const HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(5);
const WRITE_TIMEOUT: Duration = Duration::from_millis(100);
const EVENT_QUEUE_CAPACITY: usize = 256;
const MAX_CONNECTION_WORKERS: usize = 64;
const MAX_INBOUND_MESSAGES_PER_PASS: usize = 32;
const MAX_INBOUND_MESSAGE_BYTES: usize = 64 * 1024;

/// Socket operation whose failure prevents the server from satisfying its
/// contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SocketSetupOperation {
    ListenerNonblocking,
    ListenerAddressLookup,
    HandshakeReadTimeout,
    PeerWriteTimeout,
    PeerReadTimeoutClear,
    PeerNonblocking,
}

impl std::fmt::Display for SocketSetupOperation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::ListenerNonblocking => "listener nonblocking mode",
            Self::ListenerAddressLookup => "listener address lookup",
            Self::HandshakeReadTimeout => "handshake read timeout",
            Self::PeerWriteTimeout => "peer write timeout",
            Self::PeerReadTimeoutClear => "peer read-timeout clear",
            Self::PeerNonblocking => "peer nonblocking mode",
        };
        formatter.write_str(name)
    }
}

/// Terminal server failures. These failures stop scheduled simulation.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum WebSocketServerError {
    #[error("failed to bind WebSocket listener on port {port}: {detail}")]
    ListenerBind { port: u16, detail: String },
    #[error("failed to configure {operation}: {detail}")]
    ListenerSetup {
        operation: SocketSetupOperation,
        detail: String,
    },
    #[error("WebSocket listener accept failed: {detail}")]
    ListenerAccept { detail: String },
    #[error("failed to configure {operation} for accepted peer {peer}: {detail}")]
    AcceptedSocketSetup {
        peer: SocketAddr,
        operation: SocketSetupOperation,
        detail: String,
    },
    #[error("failed to spawn WebSocket state-fanout thread: {detail}")]
    StateFanoutThreadSpawn { detail: String },
    #[error("WebSocket state-fanout startup observer closed before readiness was acknowledged")]
    StateFanoutStartupObserverClosed,
    #[error("WebSocket state-fanout thread panicked")]
    StateFanoutThreadPanicked,
    #[error("WebSocket state source closed while the server was live")]
    StateSourceClosed,
    #[error("WebSocket latest-state lock was poisoned")]
    LatestStatePoisoned,
    #[error("WebSocket frame generation counter overflowed")]
    FrameGenerationOverflow,
    #[error("failed to spawn WebSocket connection thread for {peer}: {detail}")]
    ConnectionThreadSpawn { peer: SocketAddr, detail: String },
    #[error("WebSocket connection thread for {peer} panicked")]
    ConnectionThreadPanicked { peer: SocketAddr },
    #[error("WebSocket server thread spawn failed: {detail}")]
    ServerThreadSpawn { detail: String },
    #[error("WebSocket server thread panicked")]
    ServerThreadPanicked,
    #[error("WebSocket startup observer closed before readiness was reported")]
    StartupObserverClosed,
    #[error("WebSocket state publisher is unavailable because the server stopped")]
    StatePublisherClosed,
    #[error("WebSocket state-publication mailbox lock was poisoned")]
    StatePublisherPoisoned,
    #[error("WebSocket control fanout for peer {peer} closed while the server was live")]
    ControlFanoutClosed { peer: SocketAddr },
    #[error("WebSocket handshake for peer {peer} failed outside the isolated peer list: {detail}")]
    HandshakeFailure { peer: SocketAddr, detail: String },
    #[error("blocking WebSocket handshake for peer {peer} was unexpectedly interrupted")]
    HandshakeInterrupted { peer: SocketAddr },
    #[error("WebSocket {stage} for peer {peer} failed outside the isolated peer list: {detail}")]
    PeerOperationFailure {
        peer: SocketAddr,
        stage: PeerOperationStage,
        detail: String,
    },
    #[error("WebSocket received an internal raw frame for peer {peer}")]
    UnexpectedRawFrame { peer: SocketAddr },
}

/// Stage at which a terminal accepted-peer operation failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PeerOperationStage {
    Read,
    Write,
}

impl std::fmt::Display for PeerOperationStage {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Read => "read",
            Self::Write => "write",
        })
    }
}

/// Closed list of accepted-peer failures that may be isolated without stopping
/// the server.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PeerFailureKind {
    HandshakeRejected,
    HandshakeClosed,
    CleanClose,
    TransportClosed,
    SlowConsumer,
    ProtocolViolation,
    CapacityExceeded,
    InvalidUtf8,
    AttackAttempt,
    InvalidCommand,
    ControlQueueSaturated,
    ConnectionCapacitySaturated,
    UnsupportedBinaryCommand,
}

/// Observable record for one isolated accepted-peer failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PeerFailureRecord {
    peer: SocketAddr,
    kind: PeerFailureKind,
    detail: String,
}

impl PeerFailureRecord {
    pub fn peer(&self) -> SocketAddr {
        self.peer
    }

    pub fn classification(&self) -> PeerFailureKind {
        self.kind
    }

    pub fn detail(&self) -> &str {
        &self.detail
    }
}

/// Closed failure vocabulary for the bounded server-event observer. Rejected
/// peer records remain owned by the observer failure that prevented delivery.
/// Acceptance stops on the first rejected delivery; joined cleanup can add at
/// most one record per already-capped connection worker.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EventObserverFailure {
    #[error(
        "WebSocket event observer closed with {} unobserved peer record(s)",
        .unobserved.len()
    )]
    Closed {
        unobserved: Box<[PeerFailureRecord]>,
    },
    #[error(
        "WebSocket event queue reached its fixed capacity of {capacity} with {} unobserved peer record(s)",
        .unobserved.len()
    )]
    Saturated {
        capacity: usize,
        unobserved: Box<[PeerFailureRecord]>,
    },
}

impl EventObserverFailure {
    pub fn unobserved_peer_failures(&self) -> &[PeerFailureRecord] {
        match self {
            Self::Closed { unobserved } | Self::Saturated { unobserved, .. } => unobserved,
        }
    }

    fn push_unobserved(&mut self, record: PeerFailureRecord) {
        let unobserved = match self {
            Self::Closed { unobserved } | Self::Saturated { unobserved, .. } => unobserved,
        };
        let mut records = Vec::from(std::mem::take(unobserved));
        records.push(record);
        *unobserved = records.into_boxed_slice();
    }
}

/// Closed key-code vocabulary carried across the transport boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViewerKeyCode {
    Up,
    Down,
    Left,
    Right,
    Enter,
    Tab,
    Escape,
    Backspace,
    Delete,
    Character(ViewerCharacter),
}

impl std::fmt::Display for ViewerKeyCode {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Up => formatter.write_str("ArrowUp"),
            Self::Down => formatter.write_str("ArrowDown"),
            Self::Left => formatter.write_str("ArrowLeft"),
            Self::Right => formatter.write_str("ArrowRight"),
            Self::Enter => formatter.write_str("Enter"),
            Self::Tab => formatter.write_str("Tab"),
            Self::Escape => formatter.write_str("Escape"),
            Self::Backspace => formatter.write_str("Backspace"),
            Self::Delete => formatter.write_str("Delete"),
            Self::Character(character) => character.fmt(formatter),
        }
    }
}

/// A browser character admitted only by the closed physical-code parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ViewerCharacter(char);

impl ViewerCharacter {
    pub fn get(self) -> char {
        self.0
    }
}

impl std::fmt::Display for ViewerCharacter {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ' ' => formatter.write_str("Space"),
            character if character.is_ascii_lowercase() => {
                write!(formatter, "Key{}", character.to_ascii_uppercase())
            }
            character => write!(formatter, "Digit{character}"),
        }
    }
}

/// Rejection from checked browser-key construction.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("unsupported browser key code `{code}`")]
pub struct InvalidViewerKey {
    code: String,
}

/// Validated keyboard command carried from the browser to scheduled
/// simulation. Private fields make an unmappable command unrepresentable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ViewerKeyCommand {
    code: ViewerKeyCode,
    pressed: bool,
    shift: bool,
    ctrl: bool,
    alt: bool,
}

impl ViewerKeyCommand {
    /// Construct the one accepted browser-key representation.
    pub fn try_new(
        code: String,
        pressed: bool,
        shift: bool,
        ctrl: bool,
        alt: bool,
    ) -> Result<Self, InvalidViewerKey> {
        let mapped =
            map_browser_key_code(&code).ok_or_else(|| InvalidViewerKey { code: code.clone() })?;
        Ok(Self {
            code: mapped,
            pressed,
            shift,
            ctrl,
            alt,
        })
    }

    pub fn code(&self) -> ViewerKeyCode {
        self.code
    }

    pub fn pressed(&self) -> bool {
        self.pressed
    }

    pub fn shift(&self) -> bool {
        self.shift
    }

    pub fn ctrl(&self) -> bool {
        self.ctrl
    }

    pub fn alt(&self) -> bool {
        self.alt
    }
}

/// Closed, validated viewer-to-simulation control vocabulary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ViewerControlCommand {
    Key(ViewerKeyCommand),
    Realtime(bool),
    Quit,
}

/// Events exposed by a running server. Terminal causes are retained exactly
/// once by the join-owned shutdown aggregate; this event only tells the scoped
/// operation to stop promptly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BroadcastServerEvent {
    PeerFailure(PeerFailureRecord),
    TerminalFailure,
}

/// One terminal failure produced by a dynamic accepted-peer worker.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PeerWorkerTerminalFailureRecord {
    peer: SocketAddr,
    failure: WebSocketServerError,
}

impl PeerWorkerTerminalFailureRecord {
    pub fn peer(&self) -> SocketAddr {
        self.peer
    }

    pub fn failure(&self) -> &WebSocketServerError {
        &self.failure
    }
}

/// Closed terminal-failure product for the fixed server participants plus the
/// dynamic accepted-peer workers. A fixed participant can contribute at most
/// one failure, so a freely extensible error list cannot hide ownership.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct BroadcastServerTerminalFailures {
    accept_loop: Option<Box<WebSocketServerError>>,
    state_source_close: Option<Box<WebSocketServerError>>,
    latest_state_close: Option<Box<WebSocketServerError>>,
    state_fanout: Option<Box<WebSocketServerError>>,
    event_observer: Option<Box<EventObserverFailure>>,
    startup_observer: Option<Box<WebSocketServerError>>,
    server_thread: Option<Box<WebSocketServerError>>,
    peer_workers: Box<[PeerWorkerTerminalFailureRecord]>,
}

impl BroadcastServerTerminalFailures {
    fn record_peer_worker(&mut self, peer: SocketAddr, failure: WebSocketServerError) {
        let mut records = Vec::from(std::mem::take(&mut self.peer_workers));
        records.push(PeerWorkerTerminalFailureRecord { peer, failure });
        self.peer_workers = records.into_boxed_slice();
    }

    pub fn is_empty(&self) -> bool {
        self.accept_loop.is_none()
            && self.state_source_close.is_none()
            && self.latest_state_close.is_none()
            && self.state_fanout.is_none()
            && self.event_observer.is_none()
            && self.startup_observer.is_none()
            && self.server_thread.is_none()
            && self.peer_workers.is_empty()
    }

    pub fn accept_loop(&self) -> Option<&WebSocketServerError> {
        self.accept_loop.as_deref()
    }

    pub fn state_source_close(&self) -> Option<&WebSocketServerError> {
        self.state_source_close.as_deref()
    }

    pub fn latest_state_close(&self) -> Option<&WebSocketServerError> {
        self.latest_state_close.as_deref()
    }

    pub fn state_fanout(&self) -> Option<&WebSocketServerError> {
        self.state_fanout.as_deref()
    }

    pub fn event_observer(&self) -> Option<&EventObserverFailure> {
        self.event_observer.as_deref()
    }

    pub fn startup_observer(&self) -> Option<&WebSocketServerError> {
        self.startup_observer.as_deref()
    }

    pub fn server_thread(&self) -> Option<&WebSocketServerError> {
        self.server_thread.as_deref()
    }

    pub fn peer_workers(&self) -> &[PeerWorkerTerminalFailureRecord] {
        &self.peer_workers
    }
}

impl std::fmt::Display for BroadcastServerTerminalFailures {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut separator = "";
        for (participant, failure) in [
            ("accept-loop", self.accept_loop.as_deref()),
            ("state-source-close", self.state_source_close.as_deref()),
            ("latest-state-close", self.latest_state_close.as_deref()),
            ("state-fanout", self.state_fanout.as_deref()),
            ("startup-observer", self.startup_observer.as_deref()),
            ("server-thread", self.server_thread.as_deref()),
        ] {
            if let Some(failure) = failure {
                write!(formatter, "{separator}{participant}: {failure}")?;
                separator = "; ";
            }
        }
        for record in &self.peer_workers {
            write!(
                formatter,
                "{separator}peer-worker {}: {}",
                record.peer, record.failure
            )?;
            separator = "; ";
        }
        if let Some(failure) = self.event_observer.as_deref() {
            write!(formatter, "{separator}event-observer: {failure}")?;
            separator = "; ";
        }
        if separator.is_empty() {
            formatter.write_str("no terminal WebSocket failures")?;
        }
        Ok(())
    }
}

impl std::error::Error for BroadcastServerTerminalFailures {}

/// Result of joining one scoped server invocation.
#[derive(Debug)]
pub struct BroadcastServerShutdown {
    peer_failures: Box<[PeerFailureRecord]>,
    terminal_failures: BroadcastServerTerminalFailures,
}

impl BroadcastServerShutdown {
    pub fn peer_failures(&self) -> &[PeerFailureRecord] {
        &self.peer_failures
    }

    pub fn terminal_failures(&self) -> &BroadcastServerTerminalFailures {
        &self.terminal_failures
    }

    pub fn into_parts(self) -> (Box<[PeerFailureRecord]>, BroadcastServerTerminalFailures) {
        (self.peer_failures, self.terminal_failures)
    }
}

/// Operation output paired inseparably with the joined server outcome.
#[derive(Debug)]
pub struct ScopedBroadcastServerRun<T> {
    output: T,
    shutdown: BroadcastServerShutdown,
}

impl<T> ScopedBroadcastServerRun<T> {
    pub fn into_parts(self) -> (T, BroadcastServerShutdown) {
        (self.output, self.shutdown)
    }
}

/// Panic payload emitted only after a scoped operation has shut down and joined
/// its server. The original payload and the typed shutdown outcome remain
/// recoverable by a caller that catches the unwind.
pub struct ScopedBroadcastServerPanic {
    original: Box<dyn std::any::Any + Send>,
    shutdown: BroadcastServerShutdown,
}

impl std::fmt::Debug for ScopedBroadcastServerPanic {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ScopedBroadcastServerPanic")
            .field("shutdown", &self.shutdown)
            .finish_non_exhaustive()
    }
}

impl ScopedBroadcastServerPanic {
    pub fn shutdown(&self) -> &BroadcastServerShutdown {
        &self.shutdown
    }

    pub fn into_parts(self) -> (Box<dyn std::any::Any + Send>, BroadcastServerShutdown) {
        (self.original, self.shutdown)
    }
}

/// Bound listener. Binding and listener setup happen synchronously before any
/// server-owned thread can be spawned.
pub struct BroadcastServer {
    listener: TcpListener,
    local_addr: SocketAddr,
}

impl BroadcastServer {
    /// Bind a broadcast server to every interface on `port`.
    pub fn bind(port: u16) -> Result<Self, WebSocketServerError> {
        let listener = TcpListener::bind(("0.0.0.0", port)).map_err(|error| {
            WebSocketServerError::ListenerBind {
                port,
                detail: error.to_string(),
            }
        })?;
        listener
            .set_nonblocking(true)
            .map_err(|error| WebSocketServerError::ListenerSetup {
                operation: SocketSetupOperation::ListenerNonblocking,
                detail: error.to_string(),
            })?;
        let local_addr =
            listener
                .local_addr()
                .map_err(|error| WebSocketServerError::ListenerSetup {
                    operation: SocketSetupOperation::ListenerAddressLookup,
                    detail: error.to_string(),
                })?;
        Ok(Self {
            listener,
            local_addr,
        })
    }

    /// Resolved bound address, including the kernel-selected port when `0` was
    /// requested.
    pub fn local_addr(&self) -> SocketAddr {
        self.local_addr
    }

    /// Run one operation while lending the non-escaping live server. The server
    /// is shut down and joined before this method returns or resumes a panic.
    pub fn run_scoped<T, F>(
        self,
        control_tx: mpsc::SyncSender<ViewerControlCommand>,
        operation: F,
    ) -> Result<ScopedBroadcastServerRun<T>, BroadcastServerTerminalFailures>
    where
        F: for<'scope> FnOnce(&'scope RunningBroadcastServer) -> T,
    {
        let running = self.start(control_tx)?;
        let output = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| operation(&running)));
        let shutdown = running.finish();
        match output {
            Ok(output) => Ok(ScopedBroadcastServerRun { output, shutdown }),
            Err(original) => {
                std::panic::panic_any(ScopedBroadcastServerPanic { original, shutdown })
            }
        }
    }

    fn start(
        self,
        control_tx: mpsc::SyncSender<ViewerControlCommand>,
    ) -> Result<RunningBroadcastServer, BroadcastServerTerminalFailures> {
        let state_source = Arc::new(StatePublicationMailbox::default());
        let server_state_source = Arc::clone(&state_source);
        let (event_tx, event_rx) = mpsc::sync_channel(EVENT_QUEUE_CAPACITY);
        let (startup_tx, startup_rx) = mpsc::sync_channel(1);
        let quit = Arc::new(AtomicBool::new(false));
        let server_quit = Arc::clone(&quit);
        let thread = thread::Builder::new()
            .name("rumoca-websocket-listener".to_string())
            .spawn(move || {
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let mut failures = self.run(
                        server_state_source,
                        control_tx,
                        &event_tx,
                        startup_tx,
                        &server_quit,
                    );
                    notify_terminal_failure(&mut failures, &event_tx, &server_quit);
                    failures
                }))
                .unwrap_or_else(|_| {
                    server_quit.store(true, Ordering::Release);
                    BroadcastServerTerminalFailures {
                        server_thread: Some(Box::new(WebSocketServerError::ServerThreadPanicked)),
                        ..BroadcastServerTerminalFailures::default()
                    }
                })
            })
            .map_err(|error| BroadcastServerTerminalFailures {
                server_thread: Some(Box::new(WebSocketServerError::ServerThreadSpawn {
                    detail: error.to_string(),
                })),
                ..BroadcastServerTerminalFailures::default()
            })?;
        match startup_rx.recv() {
            Ok(BroadcastServerStartup::Ready) => {}
            Ok(BroadcastServerStartup::Failed) => {
                return Err(join_server_failures(thread));
            }
            Err(_) => {
                let mut failures = join_server_failures(thread);
                if failures.startup_observer.is_none() {
                    failures.startup_observer =
                        Some(Box::new(WebSocketServerError::StartupObserverClosed));
                }
                return Err(failures);
            }
        }
        Ok(RunningBroadcastServer {
            state_source,
            event_rx,
            quit,
            thread,
        })
    }

    fn run(
        self,
        state_source: Arc<StatePublicationMailbox>,
        control_tx: mpsc::SyncSender<ViewerControlCommand>,
        event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
        startup_tx: mpsc::SyncSender<BroadcastServerStartup>,
        quit: &Arc<AtomicBool>,
    ) -> BroadcastServerTerminalFailures {
        eprintln!("  WebSocket: ws://{}", self.local_addr);
        let shared: SharedLatest = Arc::new((Mutex::new(Latest::default()), Condvar::new()));
        let mut failures = BroadcastServerTerminalFailures::default();
        let workers = Vec::new();
        let fanout = match spawn_state_fanout(
            Arc::clone(&state_source),
            Arc::clone(&shared),
            Arc::clone(quit),
        ) {
            Ok(fanout) => fanout,
            Err(failure) => {
                failures.state_fanout = Some(Box::new(failure));
                failures.state_source_close = state_source.close().err().map(Box::new);
                failures.latest_state_close = close_latest(&shared).err().map(Box::new);
                if startup_tx.send(BroadcastServerStartup::Failed).is_err() {
                    failures.startup_observer =
                        Some(Box::new(WebSocketServerError::StartupObserverClosed));
                }
                return failures;
            }
        };
        run_guarded_server_runtime(
            RuntimeCleanupContext {
                state_source: &state_source,
                shared: &shared,
                event_tx,
                quit,
            },
            fanout,
            workers,
            &mut failures,
            |fanout, workers, failures| {
                if startup_tx.send(BroadcastServerStartup::Ready).is_err() {
                    failures.startup_observer =
                        Some(Box::new(WebSocketServerError::StartupObserverClosed));
                    return;
                }
                self.accept_loop(
                    AcceptContext {
                        shared: &shared,
                        control_tx: &control_tx,
                        event_tx,
                        quit,
                        fanout,
                    },
                    workers,
                    failures,
                );
            },
        );
        failures
    }

    fn accept_loop(
        &self,
        context: AcceptContext<'_>,
        workers: &mut Vec<ConnectionWorker>,
        failures: &mut BroadcastServerTerminalFailures,
    ) {
        while !context.quit.load(Ordering::Acquire) {
            if context.fanout.thread.is_finished()
                || reap_finished_workers(workers, context.event_tx, failures)
            {
                return;
            }
            let accepted = self.listener.accept();
            if handle_accept_result(accepted, &context, workers, failures)
                == AcceptDisposition::Terminate
            {
                return;
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AcceptDisposition {
    Continue,
    Terminate,
}

fn handle_accept_result(
    accepted: io::Result<(TcpStream, SocketAddr)>,
    context: &AcceptContext<'_>,
    workers: &mut Vec<ConnectionWorker>,
    failures: &mut BroadcastServerTerminalFailures,
) -> AcceptDisposition {
    match accepted {
        Ok((stream, peer)) => handle_accepted_socket(stream, peer, context, workers, failures),
        Err(error) if listener_not_ready(&error) => {
            thread::sleep(ACCEPT_POLL_INTERVAL);
            AcceptDisposition::Continue
        }
        Err(error) => {
            failures.accept_loop = Some(Box::new(WebSocketServerError::ListenerAccept {
                detail: error.to_string(),
            }));
            AcceptDisposition::Terminate
        }
    }
}

fn handle_accepted_socket(
    stream: TcpStream,
    peer: SocketAddr,
    context: &AcceptContext<'_>,
    workers: &mut Vec<ConnectionWorker>,
    failures: &mut BroadcastServerTerminalFailures,
) -> AcceptDisposition {
    if !connection_worker_capacity_available(workers.len()) {
        let record = PeerFailureRecord {
            peer,
            kind: PeerFailureKind::ConnectionCapacitySaturated,
            detail: format!(
                "WebSocket server reached its fixed worker capacity of {MAX_CONNECTION_WORKERS}"
            ),
        };
        return if report_peer_failure(record, context.event_tx, failures) {
            AcceptDisposition::Terminate
        } else {
            AcceptDisposition::Continue
        };
    }
    match spawn_connection_worker(
        stream,
        peer,
        Arc::clone(context.shared),
        context.control_tx.clone(),
        Arc::clone(context.quit),
    ) {
        Ok(worker) => {
            workers.push(worker);
            AcceptDisposition::Continue
        }
        Err(failure) => {
            failures.record_peer_worker(peer, failure);
            AcceptDisposition::Terminate
        }
    }
}

fn connection_worker_capacity_available(active_workers: usize) -> bool {
    active_workers < MAX_CONNECTION_WORKERS
}

fn notify_terminal_failure(
    failures: &mut BroadcastServerTerminalFailures,
    event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
    quit: &Arc<AtomicBool>,
) {
    if failures.is_empty() {
        return;
    }
    quit.store(true, Ordering::Release);
    if let Err(failure) = try_send_event(event_tx, BroadcastServerEvent::TerminalFailure) {
        record_event_observer_failure(failures, failure, None);
    }
}

#[derive(Clone, Copy)]
enum EventDeliveryFailure {
    Closed,
    Saturated,
}

fn try_send_event(
    event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
    event: BroadcastServerEvent,
) -> Result<(), EventDeliveryFailure> {
    match event_tx.try_send(event) {
        Ok(()) => Ok(()),
        Err(mpsc::TrySendError::Full(_)) => Err(EventDeliveryFailure::Saturated),
        Err(mpsc::TrySendError::Disconnected(_)) => Err(EventDeliveryFailure::Closed),
    }
}

fn record_event_observer_failure(
    failures: &mut BroadcastServerTerminalFailures,
    failure: EventDeliveryFailure,
    unobserved: Option<PeerFailureRecord>,
) {
    if let Some(existing) = failures.event_observer.as_deref_mut() {
        if let Some(record) = unobserved {
            existing.push_unobserved(record);
        }
        return;
    }
    let unobserved = unobserved
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice();
    failures.event_observer = Some(Box::new(match failure {
        EventDeliveryFailure::Closed => EventObserverFailure::Closed { unobserved },
        EventDeliveryFailure::Saturated => EventObserverFailure::Saturated {
            capacity: EVENT_QUEUE_CAPACITY,
            unobserved,
        },
    }));
}

fn report_peer_failure(
    record: PeerFailureRecord,
    event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
    failures: &mut BroadcastServerTerminalFailures,
) -> bool {
    match try_send_event(event_tx, BroadcastServerEvent::PeerFailure(record.clone())) {
        Ok(()) => false,
        Err(failure) => {
            record_event_observer_failure(failures, failure, Some(record));
            true
        }
    }
}

struct RuntimeCleanupContext<'a> {
    state_source: &'a StatePublicationMailbox,
    shared: &'a SharedLatest,
    event_tx: &'a mpsc::SyncSender<BroadcastServerEvent>,
    quit: &'a Arc<AtomicBool>,
}

fn run_guarded_server_runtime<F>(
    context: RuntimeCleanupContext<'_>,
    fanout: StateFanoutWorker,
    mut workers: Vec<ConnectionWorker>,
    failures: &mut BroadcastServerTerminalFailures,
    body: F,
) where
    F: FnOnce(&StateFanoutWorker, &mut Vec<ConnectionWorker>, &mut BroadcastServerTerminalFailures),
{
    let body_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        body(&fanout, &mut workers, failures);
    }));
    if body_result.is_err() {
        failures.server_thread = Some(Box::new(WebSocketServerError::ServerThreadPanicked));
    }
    context.quit.store(true, Ordering::Release);
    failures.state_source_close = context.state_source.close().err().map(Box::new);
    failures.latest_state_close = close_latest(context.shared).err().map(Box::new);
    join_runtime_threads(fanout, workers, context.event_tx, failures);
}

enum BroadcastServerStartup {
    Ready,
    Failed,
}

/// Live server ownership. State publication and event observation remain bound
/// to the server lifetime.
pub struct RunningBroadcastServer {
    state_source: Arc<StatePublicationMailbox>,
    event_rx: mpsc::Receiver<BroadcastServerEvent>,
    quit: Arc<AtomicBool>,
    thread: JoinHandle<BroadcastServerTerminalFailures>,
}

impl RunningBroadcastServer {
    /// Publish the latest simulation state. Closure is a typed terminal server
    /// failure, never a disconnected-viewer success.
    pub fn publish_state(&self, json: String) -> Result<(), WebSocketServerError> {
        self.state_source.publish(json)
    }

    /// Poll the next typed server event without exposing channel or OS error
    /// classification to the caller.
    pub fn try_next_event(&self) -> Result<Option<BroadcastServerEvent>, WebSocketServerError> {
        match self.event_rx.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(mpsc::TryRecvError::Empty) if self.quit.load(Ordering::Acquire) => {
                Ok(Some(BroadcastServerEvent::TerminalFailure))
            }
            Err(mpsc::TryRecvError::Empty) => Ok(None),
            Err(mpsc::TryRecvError::Disconnected) => {
                Ok(Some(BroadcastServerEvent::TerminalFailure))
            }
        }
    }

    /// Stop and join the owned listener, retaining every final peer record and
    /// any terminal failure for the simulation owner.
    fn finish(self) -> BroadcastServerShutdown {
        let Self {
            state_source,
            event_rx,
            quit,
            thread,
        } = self;
        quit.store(true, Ordering::Release);
        let state_source_close = state_source.close().err().map(Box::new);
        let mut terminal_failures = join_server_failures(thread);
        if terminal_failures.state_source_close.is_none() {
            terminal_failures.state_source_close = state_source_close;
        }
        let mut peer_failures = Vec::new();
        while let Ok(event) = event_rx.try_recv() {
            match event {
                BroadcastServerEvent::PeerFailure(record) => peer_failures.push(record),
                BroadcastServerEvent::TerminalFailure => {}
            }
        }
        BroadcastServerShutdown {
            peer_failures: peer_failures.into_boxed_slice(),
            terminal_failures,
        }
    }
}

fn join_server_failures(
    thread: JoinHandle<BroadcastServerTerminalFailures>,
) -> BroadcastServerTerminalFailures {
    thread
        .join()
        .unwrap_or_else(|_| BroadcastServerTerminalFailures {
            server_thread: Some(Box::new(WebSocketServerError::ServerThreadPanicked)),
            ..BroadcastServerTerminalFailures::default()
        })
}

#[derive(Default)]
struct Latest {
    generation: u64,
    frame: Option<String>,
    closed: bool,
}

type SharedLatest = Arc<(Mutex<Latest>, Condvar)>;

#[derive(Default)]
struct StatePublicationSlot {
    latest: Option<String>,
    closed: bool,
}

impl StatePublicationSlot {
    fn replace_latest(&mut self, json: String) {
        self.latest = Some(json);
    }
}

#[derive(Default)]
struct StatePublicationMailbox {
    slot: Mutex<StatePublicationSlot>,
    wake: Condvar,
}

impl StatePublicationMailbox {
    fn publish(&self, json: String) -> Result<(), WebSocketServerError> {
        let mut slot = self
            .slot
            .lock()
            .map_err(|_| WebSocketServerError::StatePublisherPoisoned)?;
        if slot.closed {
            return Err(WebSocketServerError::StatePublisherClosed);
        }
        slot.replace_latest(json);
        self.wake.notify_one();
        Ok(())
    }

    fn close(&self) -> Result<(), WebSocketServerError> {
        let mut slot = self
            .slot
            .lock()
            .map_err(|_| WebSocketServerError::StatePublisherPoisoned)?;
        slot.closed = true;
        self.wake.notify_all();
        Ok(())
    }

    fn take(&self) -> Result<StatePublicationWait, WebSocketServerError> {
        let slot = self
            .slot
            .lock()
            .map_err(|_| WebSocketServerError::StatePublisherPoisoned)?;
        let (mut slot, _) = self
            .wake
            .wait_timeout_while(slot, ACCEPT_POLL_INTERVAL, |slot| {
                slot.latest.is_none() && !slot.closed
            })
            .map_err(|_| WebSocketServerError::StatePublisherPoisoned)?;
        if let Some(json) = slot.latest.take() {
            return Ok(StatePublicationWait::New(json));
        }
        Ok(if slot.closed {
            StatePublicationWait::Closed
        } else {
            StatePublicationWait::None
        })
    }
}

struct ConnectionWorker {
    peer: SocketAddr,
    thread: JoinHandle<PeerWorkerOutcome>,
}

struct StateFanoutWorker {
    thread: JoinHandle<Result<(), WebSocketServerError>>,
}

struct StateFanoutReady;

struct AcceptContext<'a> {
    shared: &'a SharedLatest,
    control_tx: &'a mpsc::SyncSender<ViewerControlCommand>,
    event_tx: &'a mpsc::SyncSender<BroadcastServerEvent>,
    quit: &'a Arc<AtomicBool>,
    fanout: &'a StateFanoutWorker,
}

enum PeerWorkerOutcome {
    ServerStopped,
    Isolated(PeerFailureRecord),
    Terminal(WebSocketServerError),
}

enum FrameWait {
    New(String),
    None,
    Closed,
}

enum StatePublicationWait {
    New(String),
    None,
    Closed,
}

enum ReadDisposition {
    Pending,
    Isolated(PeerFailureKind, String),
    Terminal(String),
}

fn listener_not_ready(error: &io::Error) -> bool {
    error.kind() == io::ErrorKind::WouldBlock
}

fn spawn_state_fanout(
    state_source: Arc<StatePublicationMailbox>,
    shared: SharedLatest,
    quit: Arc<AtomicBool>,
) -> Result<StateFanoutWorker, WebSocketServerError> {
    let (ready_tx, ready_rx) = mpsc::sync_channel(0);
    let startup_quit = Arc::clone(&quit);
    let thread = thread::Builder::new()
        .name("rumoca-websocket-state-fanout".to_string())
        .spawn(move || {
            ready_tx
                .send(StateFanoutReady)
                .map_err(|_| WebSocketServerError::StateFanoutStartupObserverClosed)?;
            state_fanout_loop(&state_source, &shared, &quit)
        })
        .map_err(|error| WebSocketServerError::StateFanoutThreadSpawn {
            detail: error.to_string(),
        })?;
    match ready_rx.recv() {
        Ok(StateFanoutReady) => Ok(StateFanoutWorker { thread }),
        Err(_) => {
            startup_quit.store(true, Ordering::Release);
            Err(match thread.join() {
                Ok(Err(failure)) => failure,
                Ok(Ok(())) => WebSocketServerError::StateFanoutStartupObserverClosed,
                Err(_) => WebSocketServerError::StateFanoutThreadPanicked,
            })
        }
    }
}

fn state_fanout_loop(
    state_source: &StatePublicationMailbox,
    shared: &SharedLatest,
    quit: &Arc<AtomicBool>,
) -> Result<(), WebSocketServerError> {
    while !quit.load(Ordering::Acquire) {
        match state_source.take()? {
            StatePublicationWait::New(json) => replace_latest(shared, json)?,
            StatePublicationWait::None => {}
            StatePublicationWait::Closed => {
                if quit.load(Ordering::Acquire) {
                    return Ok(());
                }
                return Err(WebSocketServerError::StateSourceClosed);
            }
        }
    }
    Ok(())
}

fn replace_latest(shared: &SharedLatest, json: String) -> Result<(), WebSocketServerError> {
    let (lock, cvar) = &**shared;
    let mut latest = lock
        .lock()
        .map_err(|_| WebSocketServerError::LatestStatePoisoned)?;
    latest.generation = latest
        .generation
        .checked_add(1)
        .ok_or(WebSocketServerError::FrameGenerationOverflow)?;
    latest.frame = Some(json);
    cvar.notify_all();
    Ok(())
}

fn close_latest(shared: &SharedLatest) -> Result<(), WebSocketServerError> {
    let (lock, cvar) = &**shared;
    let mut latest = lock
        .lock()
        .map_err(|_| WebSocketServerError::LatestStatePoisoned)?;
    latest.closed = true;
    cvar.notify_all();
    Ok(())
}

fn spawn_connection_worker(
    stream: TcpStream,
    peer: SocketAddr,
    shared: SharedLatest,
    control_tx: mpsc::SyncSender<ViewerControlCommand>,
    quit: Arc<AtomicBool>,
) -> Result<ConnectionWorker, WebSocketServerError> {
    prepare_accepted_socket(&stream, peer)?;
    let thread = thread::Builder::new()
        .name(format!("rumoca-websocket-peer-{peer}"))
        .spawn(move || serve_accepted_peer(stream, peer, &shared, &control_tx, &quit))
        .map_err(|error| WebSocketServerError::ConnectionThreadSpawn {
            peer,
            detail: error.to_string(),
        })?;
    Ok(ConnectionWorker { peer, thread })
}

fn prepare_accepted_socket(
    stream: &TcpStream,
    peer: SocketAddr,
) -> Result<(), WebSocketServerError> {
    configure_accepted_socket(
        stream.set_read_timeout(Some(HANDSHAKE_TIMEOUT)),
        peer,
        SocketSetupOperation::HandshakeReadTimeout,
    )?;
    configure_accepted_socket(
        stream.set_write_timeout(Some(WRITE_TIMEOUT)),
        peer,
        SocketSetupOperation::PeerWriteTimeout,
    )
}

fn configure_accepted_socket(
    result: io::Result<()>,
    peer: SocketAddr,
    operation: SocketSetupOperation,
) -> Result<(), WebSocketServerError> {
    result.map_err(|error| WebSocketServerError::AcceptedSocketSetup {
        peer,
        operation,
        detail: error.to_string(),
    })
}

fn serve_accepted_peer(
    stream: TcpStream,
    peer: SocketAddr,
    shared: &SharedLatest,
    control_tx: &mpsc::SyncSender<ViewerControlCommand>,
    quit: &Arc<AtomicBool>,
) -> PeerWorkerOutcome {
    let mut ws = match accept_with_config(stream, Some(viewer_websocket_config())) {
        Ok(ws) => ws,
        Err(HandshakeError::Interrupted(_)) => {
            return PeerWorkerOutcome::Terminal(WebSocketServerError::HandshakeInterrupted {
                peer,
            });
        }
        Err(HandshakeError::Failure(error)) => return classify_handshake_failure(peer, error),
    };
    if let Err(error) = configure_peer_after_handshake(&ws, peer) {
        return PeerWorkerOutcome::Terminal(error);
    }
    eprintln!("[WS] viewer connected: {peer}");
    let outcome = serve_client(&mut ws, peer, shared, control_tx, quit);
    eprintln!("[WS] viewer disconnected: {peer}");
    outcome
}

fn viewer_websocket_config() -> WebSocketConfig {
    WebSocketConfig::default()
        .max_message_size(Some(MAX_INBOUND_MESSAGE_BYTES))
        .max_frame_size(Some(MAX_INBOUND_MESSAGE_BYTES))
}

fn configure_peer_after_handshake(
    ws: &WsStream,
    peer: SocketAddr,
) -> Result<(), WebSocketServerError> {
    configure_accepted_socket(
        ws.get_ref().set_read_timeout(None),
        peer,
        SocketSetupOperation::PeerReadTimeoutClear,
    )?;
    configure_accepted_socket(
        ws.get_ref().set_nonblocking(true),
        peer,
        SocketSetupOperation::PeerNonblocking,
    )
}

fn serve_client(
    ws: &mut WsStream,
    peer: SocketAddr,
    shared: &SharedLatest,
    control_tx: &mpsc::SyncSender<ViewerControlCommand>,
    quit: &Arc<AtomicBool>,
) -> PeerWorkerOutcome {
    let mut last_sent = 0_u64;
    loop {
        if quit.load(Ordering::Acquire) {
            return PeerWorkerOutcome::ServerStopped;
        }
        if let Some(outcome) = drain_inbound(ws, peer, control_tx, quit) {
            return outcome;
        }
        match next_frame(shared, &mut last_sent) {
            Ok(FrameWait::Closed) => return PeerWorkerOutcome::ServerStopped,
            Ok(FrameWait::None) => {}
            Ok(FrameWait::New(json)) => {
                if let Some(outcome) =
                    classify_write_result(ws.send(Message::Text(json.into())), peer)
                {
                    return outcome;
                }
            }
            Err(error) => return PeerWorkerOutcome::Terminal(error),
        }
    }
}

fn next_frame(
    shared: &SharedLatest,
    last_sent: &mut u64,
) -> Result<FrameWait, WebSocketServerError> {
    let (lock, cvar) = &**shared;
    let mut latest = lock
        .lock()
        .map_err(|_| WebSocketServerError::LatestStatePoisoned)?;
    while latest.generation == *last_sent && !latest.closed {
        let (guard, timeout) = cvar
            .wait_timeout(latest, FRAME_WAIT_INTERVAL)
            .map_err(|_| WebSocketServerError::LatestStatePoisoned)?;
        latest = guard;
        if timeout.timed_out() {
            break;
        }
    }
    if latest.generation != *last_sent {
        *last_sent = latest.generation;
        return Ok(match latest.frame.clone() {
            Some(json) => FrameWait::New(json),
            None => FrameWait::None,
        });
    }
    Ok(if latest.closed {
        FrameWait::Closed
    } else {
        FrameWait::None
    })
}

fn drain_inbound(
    ws: &mut WsStream,
    peer: SocketAddr,
    control_tx: &mpsc::SyncSender<ViewerControlCommand>,
    quit: &Arc<AtomicBool>,
) -> Option<PeerWorkerOutcome> {
    for _ in 0..MAX_INBOUND_MESSAGES_PER_PASS {
        if quit.load(Ordering::Acquire) {
            return Some(PeerWorkerOutcome::ServerStopped);
        }
        match ws.read() {
            Ok(Message::Text(text)) => {
                if let Err(outcome) = apply_command(&text, peer, control_tx) {
                    return Some(outcome);
                }
            }
            Ok(Message::Close(frame)) => {
                let detail = frame
                    .map(|close| format!("{} {}", close.code, close.reason))
                    .unwrap_or_else(|| "peer sent a close frame".to_string());
                return Some(isolated(peer, PeerFailureKind::CleanClose, detail));
            }
            Ok(Message::Binary(_)) => {
                return Some(isolated(
                    peer,
                    PeerFailureKind::UnsupportedBinaryCommand,
                    "viewer commands must be UTF-8 JSON text".to_string(),
                ));
            }
            Ok(Message::Ping(_) | Message::Pong(_)) => {}
            Ok(Message::Frame(_)) => {
                return Some(PeerWorkerOutcome::Terminal(
                    WebSocketServerError::UnexpectedRawFrame { peer },
                ));
            }
            Err(error) => match classify_read_error(error) {
                ReadDisposition::Pending => return None,
                ReadDisposition::Isolated(kind, detail) => {
                    return Some(isolated(peer, kind, detail));
                }
                ReadDisposition::Terminal(detail) => {
                    return Some(PeerWorkerOutcome::Terminal(
                        WebSocketServerError::PeerOperationFailure {
                            peer,
                            stage: PeerOperationStage::Read,
                            detail,
                        },
                    ));
                }
            },
        }
    }
    None
}

fn apply_command(
    text: &str,
    peer: SocketAddr,
    control_tx: &mpsc::SyncSender<ViewerControlCommand>,
) -> Result<(), PeerWorkerOutcome> {
    let command: Value = serde_json::from_str(text).map_err(|error| {
        isolated(
            peer,
            PeerFailureKind::InvalidCommand,
            format!("invalid viewer JSON: {error}"),
        )
    })?;
    let object = command.as_object().ok_or_else(|| {
        isolated(
            peer,
            PeerFailureKind::InvalidCommand,
            "viewer command root must be an object".to_string(),
        )
    })?;
    if let Some(unknown) = object
        .keys()
        .find(|key| !matches!(key.as_str(), "realtime" | "quit" | "key"))
    {
        return Err(invalid_command(
            peer,
            &format!("unknown viewer command field `{unknown}`"),
        ));
    }
    if object.len() != 1 {
        return Err(invalid_command(
            peer,
            "viewer command must contain exactly one supported field",
        ));
    }
    if let Some(value) = object.get("realtime") {
        let enabled = value
            .as_bool()
            .ok_or_else(|| invalid_command(peer, "`realtime` must be boolean"))?;
        return send_control(control_tx, peer, ViewerControlCommand::Realtime(enabled));
    }
    if let Some(value) = object.get("quit") {
        if value.as_bool() != Some(true) {
            return Err(invalid_command(peer, "`quit` must be exactly true"));
        }
        return send_control(control_tx, peer, ViewerControlCommand::Quit);
    }
    let key = object
        .get("key")
        .ok_or_else(|| invalid_command(peer, "viewer command has no supported field"))
        .and_then(|value| {
            parse_key_command(value).map_err(|detail| invalid_command(peer, &detail))
        })?;
    send_control(control_tx, peer, ViewerControlCommand::Key(key))
}

fn parse_key_command(value: &Value) -> Result<ViewerKeyCommand, String> {
    let object = value
        .as_object()
        .ok_or_else(|| "`key` must be an object".to_string())?;
    if let Some(unknown) = object
        .keys()
        .find(|key| !matches!(key.as_str(), "code" | "pressed" | "shift" | "ctrl" | "alt"))
    {
        return Err(format!("unknown viewer key field `{unknown}`"));
    }
    ViewerKeyCommand::try_new(
        required_string(object, "code")?,
        required_bool(object, "pressed")?,
        required_bool(object, "shift")?,
        required_bool(object, "ctrl")?,
        required_bool(object, "alt")?,
    )
    .map_err(|error| error.to_string())
}

fn map_browser_key_code(code: &str) -> Option<ViewerKeyCode> {
    let fixed = match code {
        "ArrowUp" => Some(ViewerKeyCode::Up),
        "ArrowDown" => Some(ViewerKeyCode::Down),
        "ArrowLeft" => Some(ViewerKeyCode::Left),
        "ArrowRight" => Some(ViewerKeyCode::Right),
        "Enter" => Some(ViewerKeyCode::Enter),
        "Tab" => Some(ViewerKeyCode::Tab),
        "Escape" => Some(ViewerKeyCode::Escape),
        "Backspace" => Some(ViewerKeyCode::Backspace),
        "Delete" => Some(ViewerKeyCode::Delete),
        "Space" => Some(ViewerKeyCode::Character(ViewerCharacter(' '))),
        _ => None,
    };
    if fixed.is_some() {
        return fixed;
    }
    if let Some(letter) = code.strip_prefix("Key")
        && letter.len() == 1
        && letter.as_bytes()[0].is_ascii_alphabetic()
    {
        return Some(ViewerKeyCode::Character(ViewerCharacter(
            char::from(letter.as_bytes()[0]).to_ascii_lowercase(),
        )));
    }
    if let Some(digit) = code.strip_prefix("Digit")
        && digit.len() == 1
        && digit.as_bytes()[0].is_ascii_digit()
    {
        return Some(ViewerKeyCode::Character(ViewerCharacter(char::from(
            digit.as_bytes()[0],
        ))));
    }
    None
}

fn required_string(object: &serde_json::Map<String, Value>, field: &str) -> Result<String, String> {
    object
        .get(field)
        .and_then(Value::as_str)
        .map(str::to_string)
        .ok_or_else(|| format!("viewer key `{field}` must be an explicit string"))
}

fn required_bool(object: &serde_json::Map<String, Value>, field: &str) -> Result<bool, String> {
    object
        .get(field)
        .and_then(Value::as_bool)
        .ok_or_else(|| format!("viewer key `{field}` must be an explicit boolean"))
}

fn send_control(
    control_tx: &mpsc::SyncSender<ViewerControlCommand>,
    peer: SocketAddr,
    command: ViewerControlCommand,
) -> Result<(), PeerWorkerOutcome> {
    match control_tx.try_send(command) {
        Ok(()) => Ok(()),
        Err(mpsc::TrySendError::Full(_)) => Err(isolated(
            peer,
            PeerFailureKind::ControlQueueSaturated,
            "viewer control queue reached its fixed capacity".to_string(),
        )),
        Err(mpsc::TrySendError::Disconnected(_)) => Err(PeerWorkerOutcome::Terminal(
            WebSocketServerError::ControlFanoutClosed { peer },
        )),
    }
}

fn invalid_command(peer: SocketAddr, detail: &str) -> PeerWorkerOutcome {
    isolated(peer, PeerFailureKind::InvalidCommand, detail.to_string())
}

fn classify_handshake_failure(peer: SocketAddr, error: WsError) -> PeerWorkerOutcome {
    let detail = error.to_string();
    match error {
        WsError::ConnectionClosed => isolated(peer, PeerFailureKind::HandshakeClosed, detail),
        WsError::Io(error) if peer_closed_io(error.kind()) || handshake_timeout(error.kind()) => {
            isolated(peer, PeerFailureKind::HandshakeClosed, detail)
        }
        WsError::Capacity(_)
        | WsError::Protocol(_)
        | WsError::Utf8
        | WsError::AttackAttempt
        | WsError::Http(_)
        | WsError::HttpFormat(_) => isolated(peer, PeerFailureKind::HandshakeRejected, detail),
        _ => PeerWorkerOutcome::Terminal(WebSocketServerError::HandshakeFailure { peer, detail }),
    }
}

fn classify_read_error(error: WsError) -> ReadDisposition {
    let detail = error.to_string();
    match error {
        WsError::Io(error) if error.kind() == io::ErrorKind::WouldBlock => ReadDisposition::Pending,
        WsError::ConnectionClosed => ReadDisposition::Isolated(PeerFailureKind::CleanClose, detail),
        WsError::Io(error) if peer_closed_io(error.kind()) => {
            ReadDisposition::Isolated(PeerFailureKind::TransportClosed, detail)
        }
        WsError::Protocol(_) => {
            ReadDisposition::Isolated(PeerFailureKind::ProtocolViolation, detail)
        }
        WsError::Capacity(_) => {
            ReadDisposition::Isolated(PeerFailureKind::CapacityExceeded, detail)
        }
        WsError::Utf8 => ReadDisposition::Isolated(PeerFailureKind::InvalidUtf8, detail),
        WsError::AttackAttempt => ReadDisposition::Isolated(PeerFailureKind::AttackAttempt, detail),
        _ => ReadDisposition::Terminal(detail),
    }
}

fn classify_write_result(
    result: Result<(), WsError>,
    peer: SocketAddr,
) -> Option<PeerWorkerOutcome> {
    let error = result.err()?;
    let detail = error.to_string();
    Some(match error {
        WsError::ConnectionClosed => isolated(peer, PeerFailureKind::CleanClose, detail),
        WsError::Io(error) if peer_closed_io(error.kind()) => {
            isolated(peer, PeerFailureKind::TransportClosed, detail)
        }
        WsError::Io(error) if error.kind() == io::ErrorKind::WouldBlock => {
            isolated(peer, PeerFailureKind::SlowConsumer, detail)
        }
        WsError::WriteBufferFull(_) => isolated(peer, PeerFailureKind::SlowConsumer, detail),
        other => PeerWorkerOutcome::Terminal(WebSocketServerError::PeerOperationFailure {
            peer,
            stage: PeerOperationStage::Write,
            detail: other.to_string(),
        }),
    })
}

fn peer_closed_io(kind: io::ErrorKind) -> bool {
    matches!(
        kind,
        io::ErrorKind::BrokenPipe
            | io::ErrorKind::ConnectionAborted
            | io::ErrorKind::ConnectionReset
            | io::ErrorKind::NotConnected
            | io::ErrorKind::TimedOut
            | io::ErrorKind::UnexpectedEof
    )
}

fn handshake_timeout(kind: io::ErrorKind) -> bool {
    matches!(kind, io::ErrorKind::TimedOut | io::ErrorKind::WouldBlock)
}

fn isolated(peer: SocketAddr, kind: PeerFailureKind, detail: String) -> PeerWorkerOutcome {
    PeerWorkerOutcome::Isolated(PeerFailureRecord { peer, kind, detail })
}

fn reap_finished_workers(
    workers: &mut Vec<ConnectionWorker>,
    event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
    failures: &mut BroadcastServerTerminalFailures,
) -> bool {
    let mut index = 0;
    let mut terminal = false;
    while index < workers.len() {
        if workers[index].thread.is_finished() {
            let worker = workers.swap_remove(index);
            terminal |= process_worker_join(worker, event_tx, failures);
        } else {
            index += 1;
        }
    }
    terminal
}

fn process_worker_join(
    worker: ConnectionWorker,
    event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
    failures: &mut BroadcastServerTerminalFailures,
) -> bool {
    let peer = worker.peer;
    let outcome = match worker.thread.join() {
        Ok(outcome) => outcome,
        Err(_) => {
            failures.record_peer_worker(
                peer,
                WebSocketServerError::ConnectionThreadPanicked { peer },
            );
            return true;
        }
    };
    match outcome {
        PeerWorkerOutcome::ServerStopped => false,
        PeerWorkerOutcome::Isolated(record) => report_peer_failure(record, event_tx, failures),
        PeerWorkerOutcome::Terminal(failure) => {
            failures.record_peer_worker(peer, failure);
            true
        }
    }
}

fn join_runtime_threads(
    fanout: StateFanoutWorker,
    workers: Vec<ConnectionWorker>,
    event_tx: &mpsc::SyncSender<BroadcastServerEvent>,
    failures: &mut BroadcastServerTerminalFailures,
) {
    failures.state_fanout = match fanout.thread.join() {
        Ok(result) => result.err().map(Box::new),
        Err(_) => Some(Box::new(WebSocketServerError::StateFanoutThreadPanicked)),
    };
    for worker in workers {
        process_worker_join(worker, event_tx, failures);
    }
}

#[cfg(test)]
mod tests;
