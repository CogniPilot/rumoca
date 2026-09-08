#![cfg(test)]

use super::viewer_input::browser_key_to_event;
use super::*;

struct HorizonSession {
    time: f64,
    omit_last_batch_value: bool,
    include_unexpected_batch_value: bool,
}

impl SimulationSessionApi for HorizonSession {
    type Error = std::convert::Infallible;

    fn retime(&mut self, t_start: f64) -> Result<(), Self::Error> {
        self.time = t_start;
        Ok(())
    }

    fn set_input(&mut self, _name: &str, _value: f64) -> Result<(), Self::Error> {
        Ok(())
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error> {
        self.time = target_time;
        Ok(())
    }

    fn time(&self) -> f64 {
        self.time
    }

    fn values_for(&self, names: &[String]) -> Result<indexmap::IndexMap<String, f64>, Self::Error> {
        let mut values: indexmap::IndexMap<String, f64> = names
            .iter()
            .take(
                names
                    .len()
                    .saturating_sub(usize::from(self.omit_last_batch_value)),
            )
            .map(|name| (name.clone(), self.time))
            .collect();
        if self.include_unexpected_batch_value {
            values.insert("unexpected".to_owned(), self.time);
        }
        Ok(values)
    }

    fn max_schedule_advance_dt(&self) -> Option<f64> {
        None
    }
}

#[test]
fn scheduled_advance_uses_the_admitted_session_horizon() {
    let mut session = HorizonSession {
        time: 0.0,
        omit_last_batch_value: false,
        include_unexpected_batch_value: false,
    };

    advance_session_to(&mut session, 0.1).expect("live session should advance");

    assert!((session.time - 0.1).abs() <= f64::EPSILON);
}

#[test]
fn frame_snapshot_requires_one_batch_value_for_every_requested_name() {
    let requested = vec!["x".to_owned(), "y".to_owned()];
    let incomplete = HorizonSession {
        time: 2.0,
        omit_last_batch_value: true,
        include_unexpected_batch_value: false,
    };
    let error = SessionFrameSnapshot::new(&incomplete, &requested, "pre-input")
        .err()
        .expect("an incomplete batch read must be refused at admission");
    assert!(error.to_string().contains("pre-input"));
    assert!(error.to_string().contains("did not exactly cover"));
    assert!(error.to_string().contains("y"));

    let excessive = HorizonSession {
        time: 2.0,
        omit_last_batch_value: false,
        include_unexpected_batch_value: true,
    };
    let error = SessionFrameSnapshot::new(&excessive, &requested, "post-advance payload")
        .err()
        .expect("an excessive batch read must be refused at admission");
    assert!(error.to_string().contains("post-advance payload"));
    assert!(error.to_string().contains("unexpected"));

    let complete = HorizonSession {
        time: 2.0,
        omit_last_batch_value: false,
        include_unexpected_batch_value: false,
    };
    let snapshot = SessionFrameSnapshot::new(&complete, &requested, "post-advance payload")
        .expect("the exact batch read is admitted");
    assert_eq!(snapshot.get("x").unwrap(), Some(2.0));
    assert_eq!(snapshot.get("not-requested").unwrap(), None);
}

#[test]
fn payload_snapshot_inventory_includes_trace_observations_not_pre_input_reads() {
    use std::collections::HashMap;

    use rumoca_input::config::{SignalSpec, SignalsConfig};

    let mapper = SignalMapper::new(
        &SignalsConfig {
            send: HashMap::from([(
                "payload".to_owned(),
                SignalSpec::Ref("model:payload_value".to_owned()),
            )]),
            viewer: HashMap::new(),
            model_inputs: HashMap::from([(
                "command".to_owned(),
                SignalSpec::Ref("model:pre_input_value".to_owned()),
            )]),
        },
        &HashMap::new(),
    )
    .expect("the role-split mapper constructs");
    let payload_names = payload_observation_lookup_names(
        &mapper,
        ["model:trace_value", "model:time", "local:ignored"],
    );

    assert_eq!(mapper.model_input_lookup_names(), &["pre_input_value"]);
    assert_eq!(payload_names, vec!["payload_value", "trace_value"]);
}

fn browser_key(code: &str) -> ViewerKeyCommand {
    browser_key_with_pressed(code, true)
}

fn browser_key_with_pressed(code: &str, pressed: bool) -> ViewerKeyCommand {
    ViewerKeyCommand::try_new(code.to_string(), pressed, false, false, false).unwrap()
}

#[test]
fn browser_arrow_key_maps_to_keyboard_event() {
    let event = browser_key_to_event(&browser_key("ArrowUp"));
    assert_eq!(event.code, KeyCode::Up);
    assert_eq!(event.modifiers, KeyModifiers::NONE);
}

#[test]
fn browser_letter_key_maps_to_lowercase_keyboard_event() {
    let event = browser_key_to_event(&browser_key("KeyW"));
    assert_eq!(event.code, KeyCode::Char('w'));
    assert_eq!(event.modifiers, KeyModifiers::NONE);
}

#[test]
fn browser_space_key_maps_to_space_keyboard_event() {
    let event = browser_key_to_event(&browser_key("Space"));
    assert_eq!(event.code, KeyCode::Char(' '));
}

#[test]
fn viewer_input_drain_preserves_keys_before_quit() {
    let (tx, rx) = mpsc::channel();
    tx.send(ViewerControlCommand::Key(browser_key("Space")))
        .unwrap();
    tx.send(ViewerControlCommand::Quit).unwrap();

    let drained = drain_viewer_input(&rx, None, false);
    assert!(drained.quit);
    assert_eq!(drained.keys.len(), 1);
    assert_eq!(drained.labels, ["Space"]);
    assert_eq!(drained.keys[0].code, KeyCode::Char(' '));
}

#[test]
fn viewer_input_drain_preserves_key_release() {
    let (tx, rx) = mpsc::channel();
    let key = browser_key_with_pressed("ArrowUp", false);
    tx.send(ViewerControlCommand::Key(key)).unwrap();

    let drained = drain_viewer_input(&rx, None, false);
    assert_eq!(drained.keys.len(), 1);
    assert_eq!(drained.labels, ["ArrowUp up"]);
    assert_eq!(drained.keys[0].code, KeyCode::Up);
    assert!(!drained.keys[0].pressed);
}

#[test]
fn websocket_peer_failure_counter_overflow_is_typed() {
    assert!(matches!(
        next_peer_failure_count(u64::MAX),
        Err(PeerFailureObservationFailureKind::CounterOverflow)
    ));
}

#[cfg(unix)]
#[test]
fn forced_exit_is_refused_when_process_group_cleanup_fails() {
    let target = ExternalInterfaceStopTarget::ProcessGroup {
        leader_pid: 12345,
        pgid: 12345,
    };
    let action = signal_action_after_cleanup(
        SignalStage::Forced,
        Err(ExternalInterfaceStopFailure::Timeout { target }),
    );

    assert!(matches!(
        action,
        Err(ExternalInterfaceStopFailure::Timeout { target: failed_target })
            if failed_target == target
    ));
}

#[cfg(unix)]
#[test]
fn setup_and_all_owned_cleanup_failures_are_aggregated() {
    let target = ExternalInterfaceStopTarget::ProcessGroup {
        leader_pid: 12345,
        pgid: 12345,
    };
    let signal_failure = ExternalInterfaceStopFailure::Timeout { target };
    let external_failure = ExternalInterfaceStopFailure::Kill {
        target,
        detail: "injected kill failure".to_string(),
    };
    let mut state = FrameState {
        recv_buf: [0; 512],
        pkt_count: 0,
        send_count: 0,
        frame_num: 0,
        websocket_peer_failures: 0,
        last_poll: Instant::now(),
        trace: None,
        lockstep_schedule_initialized: false,
        next_lockstep_send_time: 0.0,
        next_lockstep_control_time: 0.0,
    };

    let error = complete_websocket_run(
        ScheduledWebSocketPrimary::SetupFailure(anyhow::anyhow!("injected setup failure")),
        Some(SignalControllerShutdownFailure::CleanupFailures {
            failures: vec![signal_failure].into_boxed_slice(),
        }),
        Some(external_failure),
        &mut state,
    )
    .expect_err("all setup and cleanup failures must be returned")
    .to_string();

    assert!(error.contains("injected setup failure"));
    assert!(error.contains("signal cleanup attempt"));
    assert!(error.contains("injected kill failure"));
}

#[cfg(unix)]
#[test]
fn panic_payload_retains_both_owned_cleanup_failures() {
    let target = ExternalInterfaceStopTarget::ProcessGroup {
        leader_pid: 12345,
        pgid: 12345,
    };
    let unwind = std::panic::catch_unwind(|| {
        resume_scheduled_panic(
            Box::new("injected primary panic"),
            Some(SignalControllerShutdownFailure::CleanupFailures {
                failures: vec![ExternalInterfaceStopFailure::Timeout { target }].into_boxed_slice(),
            }),
            Some(ExternalInterfaceStopFailure::Kill {
                target,
                detail: "injected panic cleanup failure".to_string(),
            }),
        )
    })
    .expect_err("resume_scheduled_panic must unwind");
    let payload = unwind
        .downcast::<ScheduledSimulationPanic>()
        .expect("typed scheduled panic payload");

    assert!(payload.signal_shutdown_failure.is_some());
    assert!(payload.external_cleanup_failure.is_some());
    assert!(payload.original.downcast_ref::<&str>().is_some());
}

#[cfg(target_os = "linux")]
#[test]
fn successful_kill_is_not_reissued_after_wait_failure() {
    use std::os::unix::process::CommandExt;

    let mut command = Command::new("sleep");
    command
        .arg("30")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .process_group(0);
    let child = command
        .spawn()
        .expect("spawn external-interface retention witness");
    let pid = child.id();
    let target = ExternalInterfaceStopTarget::ProcessGroup {
        leader_pid: pid,
        pgid: pid,
    };
    let mut process = ExternalInterfaceProcess {
        state: ExternalInterfaceProcessState::Owned {
            child,
            target,
            stop_phase: ExternalInterfaceStopPhase::Running,
        },
        command: "sleep".to_string(),
    };

    let first = process.stop_with(
        |child, observed_target| {
            child
                .kill()
                .map_err(|error| ExternalInterfaceStopFailure::Kill {
                    target: observed_target,
                    detail: error.to_string(),
                })
        },
        |_child, observed_target| {
            Err(ExternalInterfaceStopFailure::Wait {
                target: observed_target,
                detail: "injected inspection failure".to_string(),
            })
        },
    );

    assert!(matches!(
        first,
        Err(ExternalInterfaceStopFailure::Wait { target: failed_target, .. })
            if failed_target == target
    ));
    assert!(matches!(
        &process.state,
        ExternalInterfaceProcessState::Owned {
            child,
            target: retained_target,
            stop_phase: ExternalInterfaceStopPhase::KillIssued,
        } if child.id() == pid && *retained_target == target
    ));
    let mut kill_reissued = false;
    process
        .stop_with(
            |_child, _target| {
                kill_reissued = true;
                Ok(())
            },
            |child, observed_target| {
                child
                    .wait()
                    .map(|_| ())
                    .map_err(|error| ExternalInterfaceStopFailure::Wait {
                        target: observed_target,
                        detail: error.to_string(),
                    })
            },
        )
        .expect("retry must reap the already-killed child");
    assert!(!kill_reissued);
    assert!(matches!(process.state, ExternalInterfaceProcessState::Idle));
}

#[test]
fn signal_cleanup_cannot_observe_an_unowned_configured_process_during_start() {
    let owner = ExternalInterfaceOwner::new(Some("injected-command"));
    let start_handle = owner.handle().clone();
    let observed_handle = owner.handle().clone();
    let (entered_tx, entered_rx) = mpsc::channel();
    let (release_tx, release_rx) = mpsc::channel();
    let start_thread = thread::spawn(move || {
        start_handle.start_with(|process| {
            assert_eq!(process.command, "injected-command");
            entered_tx.send(()).expect("publish held startup lock");
            release_rx.recv().expect("release held startup lock");
            Ok(())
        })
    });

    entered_rx
        .recv()
        .expect("startup acquired shared ownership");
    assert!(matches!(
        observed_handle.lifecycle.try_lock(),
        Err(std::sync::TryLockError::WouldBlock)
    ));
    release_tx.send(()).expect("release startup witness");
    start_thread
        .join()
        .expect("startup witness thread must not panic")
        .expect("injected startup must succeed");
    owner.stop().expect("configured idle owner cleanup");
}

#[test]
fn cleanup_before_start_terminally_refuses_later_start_and_reset_restart() {
    let owner = ExternalInterfaceOwner::new(Some("must-not-spawn"));
    owner.stop().expect("idle cancellation must succeed");

    assert!(matches!(
        owner.start(),
        Err(ExternalInterfaceStartFailure::ShutdownRequested)
    ));
    assert!(matches!(
        owner.handle().start(),
        Err(ExternalInterfaceStartFailure::ShutdownRequested)
    ));
    let lifecycle = owner
        .handle()
        .lifecycle
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(lifecycle.cancelled);
    assert!(matches!(
        lifecycle.process.as_ref().map(|process| &process.state),
        Some(ExternalInterfaceProcessState::Idle)
    ));
}

#[cfg(unix)]
#[test]
fn owned_signal_controller_releases_thread_capture_on_drop() {
    let external_interface = ExternalInterfaceHandle::configured(None);
    let quit = Arc::new(AtomicBool::new(false));
    let controller = SignalController::install(external_interface.clone(), Arc::clone(&quit))
        .expect("install signal-controller witness");
    assert_eq!(Arc::strong_count(&external_interface.lifecycle), 2);

    drop(controller);

    assert_eq!(Arc::strong_count(&external_interface.lifecycle), 1);
    assert_eq!(Arc::strong_count(&quit), 1);
}

#[cfg(target_os = "linux")]
#[test]
fn scoped_websocket_operation_panic_stops_external_process_group() {
    use std::io::{BufRead, BufReader};
    use std::os::unix::process::CommandExt;

    let mut command = Command::new("sh");
    command
        .args(["-c", "sleep 30 & echo $!; wait"])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .process_group(0);
    let mut child = command.spawn().expect("spawn external-interface witness");
    let leader_pid = child.id();
    let mut descendant_line = String::new();
    BufReader::new(
        child
            .stdout
            .take()
            .expect("capture descendant process identifier"),
    )
    .read_line(&mut descendant_line)
    .expect("read descendant process identifier");
    let descendant_pid: u32 = descendant_line
        .trim()
        .parse()
        .expect("parse descendant process identifier");
    let target = ExternalInterfaceStopTarget::ProcessGroup {
        leader_pid,
        pgid: leader_pid,
    };
    let owner = ExternalInterfaceOwner::new(None);
    owner
        .handle()
        .lifecycle
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .process = Some(ExternalInterfaceProcess {
        state: ExternalInterfaceProcessState::Owned {
            child,
            target,
            stop_phase: ExternalInterfaceStopPhase::Running,
        },
        command: "sh".to_string(),
    });
    let server = BroadcastServer::bind(0).expect("bind panic witness server");
    let (control_tx, _control_rx) = mpsc::sync_channel(8);

    let unwind = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let _owner = owner;
        server
            .run_scoped(control_tx, |_running| {
                panic!("injected scoped-operation panic")
            })
            .expect("operation panic must unwind before returning");
    }));

    assert!(unwind.is_err());
    assert!(
        !linux_process_is_running(leader_pid),
        "external-interface leader survived scoped-operation panic"
    );
    assert!(
        !linux_process_is_running(descendant_pid),
        "external-interface descendant survived scoped-operation panic"
    );
}

#[cfg(target_os = "linux")]
fn linux_process_is_running(pid: u32) -> bool {
    let Ok(stat) = std::fs::read_to_string(std::path::PathBuf::from(format!("/proc/{pid}/stat")))
    else {
        return false;
    };
    let Some(command_end) = stat.rfind(')') else {
        return true;
    };
    !matches!(
        stat[command_end + 1..].trim_start().chars().next(),
        Some('Z' | 'X')
    )
}
