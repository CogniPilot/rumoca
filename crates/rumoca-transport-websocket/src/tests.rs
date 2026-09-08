#![cfg(test)]

use super::*;
use std::io::Write;
use std::time::Instant;

fn running_server() -> (RunningBroadcastServer, SocketAddr) {
    let server = BroadcastServer::bind(0).expect("bind test server");
    let address = server.local_addr();
    let address = SocketAddr::new("127.0.0.1".parse().unwrap(), address.port());
    let (control_tx, _control_rx) = mpsc::sync_channel(8);
    let running = server.start(control_tx).expect("start test server");
    (running, address)
}

fn wait_for_peer_failure(server: &RunningBroadcastServer) -> PeerFailureRecord {
    let deadline = Instant::now() + Duration::from_secs(2);
    while Instant::now() < deadline {
        match server.try_next_event().expect("poll server event") {
            Some(BroadcastServerEvent::PeerFailure(record)) => return record,
            Some(BroadcastServerEvent::TerminalFailure) => {
                panic!("unexpected terminal failure")
            }
            None => thread::sleep(Duration::from_millis(10)),
        }
    }
    panic!("timed out waiting for peer-failure record")
}

#[test]
fn bind_failure_is_typed_before_start() {
    let occupied = TcpListener::bind(("127.0.0.1", 0)).unwrap();
    let port = occupied.local_addr().unwrap().port();

    let error = BroadcastServer::bind(port).err().expect("bind must fail");

    assert!(matches!(
        error,
        WebSocketServerError::ListenerBind { port: failed, .. } if failed == port
    ));
}

#[test]
fn scoped_operation_is_lent_only_after_state_fanout_is_ready() {
    let server = BroadcastServer::bind(0).expect("bind scoped test server");
    let (control_tx, _control_rx) = mpsc::sync_channel(8);

    let scoped = server
        .run_scoped(control_tx, |running| {
            running.publish_state("{}".to_string())
        })
        .expect("fixed participants must start");
    let (publish, shutdown) = scoped.into_parts();

    assert!(publish.is_ok());
    assert!(shutdown.terminal_failures().is_empty());
}

#[test]
fn read_would_block_is_pending_not_a_peer_failure() {
    let error = WsError::Io(io::Error::from(io::ErrorKind::WouldBlock));
    assert!(matches!(
        classify_read_error(error),
        ReadDisposition::Pending
    ));
}

#[test]
fn connection_reset_is_in_the_closed_peer_list() {
    let error = WsError::Io(io::Error::from(io::ErrorKind::ConnectionReset));
    assert!(matches!(
        classify_read_error(error),
        ReadDisposition::Isolated(PeerFailureKind::TransportClosed, _)
    ));
}

#[test]
fn unrelated_io_error_is_terminal() {
    let error = WsError::Io(io::Error::from(io::ErrorKind::PermissionDenied));
    assert!(matches!(
        classify_read_error(error),
        ReadDisposition::Terminal(_)
    ));
}

#[test]
fn live_state_source_closure_is_terminal() {
    let state_source = StatePublicationMailbox::default();
    state_source.close().expect("close test source");
    let shared: SharedLatest = Arc::new((Mutex::new(Latest::default()), Condvar::new()));
    let quit = Arc::new(AtomicBool::new(false));

    let error = state_fanout_loop(&state_source, &shared, &quit).unwrap_err();

    assert_eq!(error, WebSocketServerError::StateSourceClosed);
}

#[test]
fn state_publication_mailbox_retains_only_the_latest_frame() {
    let state_source = StatePublicationMailbox::default();
    state_source
        .publish("first".to_string())
        .expect("publish first frame");
    state_source
        .publish("latest".to_string())
        .expect("replace pending frame");

    assert!(matches!(
        state_source.take().expect("take latest frame"),
        StatePublicationWait::New(frame) if frame == "latest"
    ));
    assert!(matches!(
        state_source.take().expect("poll empty mailbox"),
        StatePublicationWait::None
    ));
}

#[test]
fn saturated_event_queue_retains_rejected_record_and_signals_terminal() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let queued = PeerFailureRecord {
        peer,
        kind: PeerFailureKind::HandshakeRejected,
        detail: "queued".to_string(),
    };
    let rejected = PeerFailureRecord {
        peer,
        kind: PeerFailureKind::ConnectionCapacitySaturated,
        detail: "rejected".to_string(),
    };
    let (event_tx, event_rx) = mpsc::sync_channel(EVENT_QUEUE_CAPACITY);
    for _ in 0..EVENT_QUEUE_CAPACITY {
        event_tx
            .try_send(BroadcastServerEvent::PeerFailure(queued.clone()))
            .expect("fill fixed event queue");
    }
    let mut failures = BroadcastServerTerminalFailures::default();
    assert!(report_peer_failure(
        rejected.clone(),
        &event_tx,
        &mut failures
    ));
    let quit = Arc::new(AtomicBool::new(true));
    let state_source = Arc::new(StatePublicationMailbox::default());
    let thread = thread::spawn(move || failures);
    let server = RunningBroadcastServer {
        state_source,
        event_rx,
        quit,
        thread,
    };

    for _ in 0..EVENT_QUEUE_CAPACITY {
        assert!(matches!(
            server.try_next_event().expect("drain fixed event queue"),
            Some(BroadcastServerEvent::PeerFailure(_))
        ));
    }
    assert_eq!(
        server.try_next_event().expect("observe terminal token"),
        Some(BroadcastServerEvent::TerminalFailure)
    );
    let shutdown = server.finish();
    let observer_failure = shutdown
        .terminal_failures()
        .event_observer()
        .expect("typed event saturation");
    assert!(matches!(
        observer_failure,
        EventObserverFailure::Saturated { capacity, .. }
            if *capacity == EVENT_QUEUE_CAPACITY
    ));
    assert_eq!(observer_failure.unobserved_peer_failures(), &[rejected]);
}

#[test]
fn accepted_worker_capacity_is_finite_before_spawn() {
    assert!(connection_worker_capacity_available(
        MAX_CONNECTION_WORKERS - 1
    ));
    assert!(!connection_worker_capacity_available(
        MAX_CONNECTION_WORKERS
    ));
}

#[test]
fn inbound_websocket_limits_are_explicit_and_finite() {
    let config = viewer_websocket_config();
    assert_eq!(config.max_message_size, Some(MAX_INBOUND_MESSAGE_BYTES));
    assert_eq!(config.max_frame_size, Some(MAX_INBOUND_MESSAGE_BYTES));
}

#[test]
fn validated_key_command_crosses_as_typed_control() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, control_rx) = mpsc::sync_channel(8);
    let json = r#"{"key":{"code":"KeyW","pressed":true,"shift":false,"ctrl":false,"alt":false}}"#;

    assert!(apply_command(json, peer, &control_tx).is_ok());

    assert_eq!(
        control_rx.recv().unwrap(),
        ViewerControlCommand::Key(
            ViewerKeyCommand::try_new("KeyW".to_string(), true, false, false, false,).unwrap()
        )
    );
}

#[test]
fn realtime_policy_crosses_as_typed_control_without_transport_effect() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, control_rx) = mpsc::sync_channel(1);

    assert!(apply_command(r#"{"realtime":false}"#, peer, &control_tx).is_ok());

    assert_eq!(
        control_rx.recv().unwrap(),
        ViewerControlCommand::Realtime(false)
    );
}

#[test]
fn saturated_control_queue_is_typed_and_nonblocking() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, _control_rx) = mpsc::sync_channel(1);
    control_tx
        .try_send(ViewerControlCommand::Realtime(true))
        .expect("fill bounded control queue");

    let error = apply_command(r#"{"quit":true}"#, peer, &control_tx)
        .expect_err("saturated control queue must isolate the peer");

    assert!(matches!(
        error,
        PeerWorkerOutcome::Isolated(PeerFailureRecord {
            kind: PeerFailureKind::ControlQueueSaturated,
            ..
        })
    ));
}

#[test]
fn omitted_key_field_is_an_isolated_invalid_command() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, _control_rx) = mpsc::sync_channel(8);
    let json = r#"{"key":{"code":"KeyW","shift":false,"ctrl":false,"alt":false}}"#;

    let error = apply_command(json, peer, &control_tx).expect_err("omitted `pressed` must reject");

    assert!(matches!(
        error,
        PeerWorkerOutcome::Isolated(PeerFailureRecord {
            kind: PeerFailureKind::InvalidCommand,
            ..
        })
    ));
}

#[test]
fn unsupported_key_cannot_cross_the_checked_control_boundary() {
    let error =
        ViewerKeyCommand::try_new("F13".to_string(), true, false, false, false).unwrap_err();

    assert_eq!(error.code, "F13");
}

#[test]
fn composite_command_is_rejected_before_any_effect() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, control_rx) = mpsc::sync_channel(8);
    let json = r#"{"key":{"code":"KeyW","pressed":true,"shift":false,"ctrl":false,"alt":false},"quit":true}"#;

    let error = apply_command(json, peer, &control_tx).expect_err("composite command must reject");

    assert!(matches!(
        error,
        PeerWorkerOutcome::Isolated(PeerFailureRecord {
            kind: PeerFailureKind::InvalidCommand,
            ..
        })
    ));
    assert!(matches!(
        control_rx.try_recv(),
        Err(mpsc::TryRecvError::Empty)
    ));
}

#[test]
fn false_quit_is_rejected_as_a_noop_command() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, control_rx) = mpsc::sync_channel(8);

    let error =
        apply_command(r#"{"quit":false}"#, peer, &control_tx).expect_err("false quit must reject");

    assert!(matches!(
        error,
        PeerWorkerOutcome::Isolated(PeerFailureRecord {
            kind: PeerFailureKind::InvalidCommand,
            ..
        })
    ));
    assert!(matches!(
        control_rx.try_recv(),
        Err(mpsc::TryRecvError::Empty)
    ));
}

#[test]
fn closed_control_fanout_is_terminal() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let (control_tx, control_rx) = mpsc::sync_channel(8);
    drop(control_rx);
    let json = r#"{"quit":true}"#;

    let error =
        apply_command(json, peer, &control_tx).expect_err("closed control fanout must reject");

    assert!(matches!(
        error,
        PeerWorkerOutcome::Terminal(WebSocketServerError::ControlFanoutClosed {
            peer: failed_peer
        }) if failed_peer == peer
    ));
}

#[test]
fn rejected_handshake_emits_typed_observable_record() {
    let (server, address) = running_server();
    let mut stream = TcpStream::connect(address).expect("connect plain TCP peer");
    stream
        .write_all(b"GET / HTTP/1.0\r\n\r\n")
        .expect("write invalid handshake");

    let record = wait_for_peer_failure(&server);

    assert_eq!(record.kind, PeerFailureKind::HandshakeRejected);
    let shutdown = server.finish();
    assert!(shutdown.terminal_failures().is_empty());
}

#[test]
fn invalid_text_command_emits_typed_observable_record() {
    let (server, address) = running_server();
    let url = format!("ws://{address}");
    let (mut client, _) = tungstenite::connect(url).expect("connect WebSocket peer");
    client
        .send(Message::Text("not-json".into()))
        .expect("send invalid command");

    let record = wait_for_peer_failure(&server);

    assert_eq!(record.kind, PeerFailureKind::InvalidCommand);
    let shutdown = server.finish();
    assert!(shutdown.terminal_failures().is_empty());
}

#[test]
fn inbound_flood_cannot_prevent_owned_shutdown_join() {
    let (server, address) = running_server();
    let url = format!("ws://{address}");
    let (mut client, _) = tungstenite::connect(url).expect("connect flood witness peer");
    for _ in 0..(MAX_INBOUND_MESSAGES_PER_PASS * 4) {
        client
            .send(Message::Ping(Vec::new().into()))
            .expect("queue flood witness ping");
    }

    let started = Instant::now();
    let shutdown = server.finish();

    assert!(started.elapsed() < Duration::from_secs(2));
    assert!(shutdown.terminal_failures().is_empty());
}

#[test]
fn terminal_aggregate_preserves_fixed_and_dynamic_participants() {
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let mut failures = BroadcastServerTerminalFailures {
        accept_loop: Some(Box::new(WebSocketServerError::ListenerAccept {
            detail: "accept".to_string(),
        })),
        state_source_close: Some(Box::new(WebSocketServerError::StatePublisherPoisoned)),
        latest_state_close: Some(Box::new(WebSocketServerError::LatestStatePoisoned)),
        state_fanout: Some(Box::new(WebSocketServerError::StateSourceClosed)),
        event_observer: Some(Box::new(EventObserverFailure::Closed {
            unobserved: Box::default(),
        })),
        startup_observer: Some(Box::new(WebSocketServerError::StartupObserverClosed)),
        server_thread: Some(Box::new(WebSocketServerError::ServerThreadPanicked)),
        peer_workers: Box::default(),
    };
    failures.record_peer_worker(
        peer,
        WebSocketServerError::ConnectionThreadPanicked { peer },
    );

    assert!(failures.accept_loop().is_some());
    assert!(failures.state_source_close().is_some());
    assert!(failures.latest_state_close().is_some());
    assert!(failures.state_fanout().is_some());
    assert!(failures.event_observer().is_some());
    assert!(failures.startup_observer().is_some());
    assert!(failures.server_thread().is_some());
    assert_eq!(failures.peer_workers().len(), 1);
    assert_eq!(failures.peer_workers()[0].peer(), peer);
}

#[test]
fn panic_after_fixed_startup_joins_fanout_and_peer_workers() {
    let state_source = Arc::new(StatePublicationMailbox::default());
    let shared: SharedLatest = Arc::new((Mutex::new(Latest::default()), Condvar::new()));
    let quit = Arc::new(AtomicBool::new(false));
    let fanout = spawn_state_fanout(
        Arc::clone(&state_source),
        Arc::clone(&shared),
        Arc::clone(&quit),
    )
    .expect("spawn test fanout");
    let peer_joined = Arc::new(AtomicBool::new(false));
    let peer_joined_in_thread = Arc::clone(&peer_joined);
    let peer: SocketAddr = "127.0.0.1:12345".parse().unwrap();
    let worker_quit = Arc::clone(&quit);
    let worker = ConnectionWorker {
        peer,
        thread: thread::spawn(move || {
            while !worker_quit.load(Ordering::Acquire) {
                thread::yield_now();
            }
            peer_joined_in_thread.store(true, Ordering::Release);
            PeerWorkerOutcome::ServerStopped
        }),
    };
    let (event_tx, _event_rx) = mpsc::sync_channel(EVENT_QUEUE_CAPACITY);
    let mut failures = BroadcastServerTerminalFailures::default();

    run_guarded_server_runtime(
        RuntimeCleanupContext {
            state_source: &state_source,
            shared: &shared,
            event_tx: &event_tx,
            quit: &quit,
        },
        fanout,
        vec![worker],
        &mut failures,
        |_fanout, _workers, _failures| panic!("injected accept-loop panic"),
    );

    assert!(matches!(
        failures.server_thread(),
        Some(WebSocketServerError::ServerThreadPanicked)
    ));
    assert!(quit.load(Ordering::Acquire));
    assert!(peer_joined.load(Ordering::Acquire));
}
