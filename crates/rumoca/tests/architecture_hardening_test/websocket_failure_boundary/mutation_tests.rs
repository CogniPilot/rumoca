use super::*;

#[test]
fn browser_has_no_implicit_retry_or_silent_payload_drop() {
    let viewer = fs::read_to_string(workspace_root().join("packages/rumoca-web/viz/viewer.html"))
        .expect("read browser viewer");
    let violations = viewer_violations(&viewer);

    assert!(
        violations.is_empty(),
        "browser WebSocket handling must fail visibly without an implicit retry policy:\n{}",
        violations.join("\n")
    );
}

#[test]
fn moved_or_renamed_socket_helpers_cannot_escape_the_inventory() {
    let transport = vec![RustSource::fixture(
        "transport/nested/moved.rs",
        r#"
            mod moved {
                fn renamed_wrapper(socket: std::net::TcpStream) {
                    socket.set_nonblocking(true).unwrap();
                }
            }
        "#,
    )];

    let violations = rust_boundary_violations(&transport, &valid_sim_fixture());

    assert!(violations.iter().any(|hit| {
        hit.contains("transport/nested/moved.rs")
            && hit.contains("set_nonblocking")
            && hit.contains("renamed_wrapper")
    }));
}

#[test]
fn ignored_and_dropped_results_cannot_escape_the_inventory() {
    let transport = vec![RustSource::fixture(
        "transport/lib.rs",
        r#"
            fn prepare_accepted_socket(socket: std::net::TcpStream) {
                let _ignored = socket.set_read_timeout(None);
                drop(socket.set_write_timeout(None));
            }
        "#,
    )];

    let violations = rust_boundary_violations(&transport, &valid_sim_fixture());

    assert!(violations.iter().any(|hit| hit.contains("ignored result")));
    assert!(violations.iter().any(|hit| hit.contains("drop(...)")));
}

#[test]
fn missing_completion_and_error_precedence_cannot_escape_the_inventory() {
    let missing_completion = vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            fn run_sim_loop(server: BroadcastServer) -> anyhow::Result<()> {
                let trace = open_trace_logger()?;
                let scoped_run = server.run_scoped(|running| run_frames(running))?;
                Ok(())
            }
        "#,
    )];
    let eager_precedence = vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            fn run_sim_loop(server: BroadcastServer) -> anyhow::Result<()> {
                let trace = open_trace_logger()?;
                let scoped_run = server.run_scoped(|running| run_frames(running))?;
                let (loop_result, shutdown) = scoped_run.into_parts();
                loop_result?;
                complete_websocket_run(Ok(()), shutdown)
            }
        "#,
    )];

    let missing = rust_boundary_violations(&valid_transport_fixture(), &missing_completion);
    let eager = rust_boundary_violations(&valid_transport_fixture(), &eager_precedence);

    assert!(
        missing
            .iter()
            .any(|hit| hit.contains("complete_websocket_run"))
    );
    assert!(
        eager
            .iter()
            .any(|hit| hit.contains("loop_result") && hit.contains("before"))
    );
}

#[test]
fn automatic_retry_and_empty_json_catch_mutations_are_rejected() {
    let retry = r#"
        ws.onclose = () => { setTimeout(connectWs, 2000); };
        ws.onmessage = (evt) => { try { JSON.parse(evt.data); } catch (error) {
            console.error(error); pipeline.ws = "invalid-payload"; ws.close();
        }};
    "#;
    let silent = r#"
        ws.onmessage = (evt) => { try { JSON.parse(evt.data); } catch (_) {} };
    "#;
    let redundant_key = r#"
        activeWs.send(JSON.stringify({ key: { code: event.code, key: event.key } }));
    "#;

    assert!(
        viewer_violations(retry)
            .iter()
            .any(|hit| hit.contains("automatic reconnect"))
    );
    assert!(
        viewer_violations(silent)
            .iter()
            .any(|hit| hit.contains("empty catch"))
    );
    assert!(
        viewer_violations(redundant_key)
            .iter()
            .any(|hit| hit.contains("duplicates unchecked key text"))
    );
}

#[test]
fn unacknowledged_fanout_startup_mutation_is_rejected() {
    let violations = rust_boundary_violations(&valid_transport_fixture(), &valid_sim_fixture());

    assert!(violations.iter().any(|hit| {
        hit.contains("spawn_state_fanout") && hit.contains("positively acknowledge")
    }));
}

#[test]
fn detached_signal_and_taken_child_authority_mutations_are_rejected() {
    let simulation = vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            struct ExternalInterfaceProcess { child: Option<std::process::Child> }
            impl ExternalInterfaceProcess {
                fn stop_with(&mut self) { let child = self.child.take(); }
            }
            fn spawn_sigint_handler() { std::thread::spawn(|| {}); }
            fn spawn_cleanup_thread() { std::thread::spawn(|| {}); }
            struct SignalController;
            impl SignalController {
                fn install(mut failures: Vec<Failure>) { failures.push(Failure); }
            }
        "#,
    )];

    let violations = rust_boundary_violations(&valid_transport_fixture(), &simulation);

    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("must never remove live Child"))
    );
    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("detached legacy signal owner"))
    );
    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("unbounded failure log"))
    );
}

#[test]
fn composite_command_and_key_text_fallback_mutations_are_rejected() {
    let transport = vec![RustSource::fixture(
        "transport/lib.rs",
        r#"
            enum ViewerControlCommand { Key(u8), Quit }
            fn apply_command(object: Object) { optional_bool(object.get("quit")); }
            fn optional_bool(value: Value) -> Option<bool> { None }
            fn map_browser_key_code(code: &str, key: &str) -> Option<char> {
                key.chars().next()
            }
        "#,
    )];

    let violations = rust_boundary_violations(&transport, &valid_sim_fixture());

    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("exactly one top-level field"))
    );
    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("successful no-op command"))
    );
    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("closed physical-code catalog"))
    );
}

#[test]
fn early_scoped_return_and_restart_log_only_mutations_are_rejected() {
    let simulation = vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            fn run_sim_loop(server: Server) -> anyhow::Result<()> {
                let scoped = server.run_scoped(|| {})?;
                Ok(())
            }
            fn handle_reset(process: &mut Process) -> anyhow::Result<()> {
                if let Err(error) = process.start() { eprintln!("{error}"); }
                Ok(())
            }
        "#,
    )];

    let violations = rust_boundary_violations(&valid_transport_fixture(), &simulation);

    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("capture run_scoped startup refusal"))
    );
    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("propagate external-interface restart failure"))
    );
}

#[test]
fn unbounded_control_and_inbound_work_mutations_are_rejected() {
    let transport = vec![RustSource::fixture(
        "transport/lib.rs",
        r#"
            fn apply_command() {}
            fn send_control(tx: Sender<Command>, command: Command) { tx.send(command); }
            fn drain_inbound(socket: Socket, quit: AtomicBool) {
                loop { socket.read(); }
            }
        "#,
    )];

    let violations = rust_boundary_violations(&transport, &valid_sim_fixture());

    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("nonblocking bounded-queue delivery")),
        "{violations:#?}"
    );
    assert!(violations.iter().any(|hit| hit.contains("bound each pass")));
}

#[test]
fn unbounded_transport_resource_mutations_are_rejected() {
    let transport = vec![RustSource::fixture(
        "transport/lib.rs",
        r#"
            struct StatePublicationMailbox;
            impl StatePublicationMailbox { fn publish(&self) {} }
            enum EventObserverFailure { Closed, Saturated }
            fn start() { let _events = channel(); }
            fn try_send_event(tx: Sender<Event>, event: Event) { tx.send(event); }
            fn report_peer_failure() { try_send_event(); }
            struct BroadcastServer;
            impl BroadcastServer {
                fn accept_loop(listener: Listener) {
                    spawn_connection_worker();
                    listener.accept();
                    reap_finished_workers();
                }
            }
            fn connection_worker_capacity_available() -> bool { true }
            fn viewer_websocket_config() { max_message_size(None); }
        "#,
    )];

    let violations = rust_boundary_violations(&transport, &valid_sim_fixture());

    for expected in [
        "unbounded mpsc channel",
        "fixed capacity",
        "latest-state overwrite mailbox",
        "closed typed aggregate",
        "nonblocking bounded-queue delivery",
        "retain each rejected peer record",
        "reap churn and prove worker capacity before each spawn",
        "terminate acceptance before a second capacity rejection",
        "one acceptance refusal plus capped workers",
        "fixed maximum",
        "fixed inbound frame and message limits",
    ] {
        assert!(
            violations.iter().any(|hit| hit.contains(expected)),
            "missing `{expected}` in {violations:#?}"
        );
    }
}

#[test]
fn missing_kill_issued_wait_mutation_is_rejected() {
    let simulation = vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            fn stop_external_interface(child: &mut Child, target: Target) {
                child.try_wait();
                kill_external_interface_target(target);
            }
        "#,
    )];

    let violations = rust_boundary_violations(&valid_transport_fixture(), &simulation);

    assert!(violations.iter().any(|hit| hit.contains("KillIssued wait")));
}

#[test]
fn kill_state_transition_must_precede_wait() {
    let ordered = stop_transition_fixture(
        "sim/ordered_stop.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        "",
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "",
    );
    let moved_after_wait = stop_transition_fixture(
        "sim/moved_stop.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        "",
        "",
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
    );

    let expected = "successful external kill must consume Running into KillIssued";
    let ordered_violations = rust_boundary_violations(&valid_transport_fixture(), &ordered);
    let moved_violations = rust_boundary_violations(&valid_transport_fixture(), &moved_after_wait);

    assert!(!ordered_violations.iter().any(|hit| hit.contains(expected)));
    assert!(moved_violations.iter().any(|hit| hit.contains(expected)));
}

#[test]
fn disabled_or_widened_stop_transition_is_rejected() {
    let disabled = stop_transition_fixture(
        "sim/disabled_stop.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        "",
        "#[cfg(any())] *stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "",
    );
    let widened = stop_transition_fixture(
        "sim/widened_stop.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running || *stop_phase == ExternalInterfaceStopPhase::KillIssued",
        "",
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "",
    );
    let reactivated = stop_transition_fixture(
        "sim/reactivated_stop.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        "",
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "#[cfg_attr(test, cfg(any()))] *stop_phase = ExternalInterfaceStopPhase::Running;",
    );
    let shadowed = stop_transition_fixture(
        "sim/shadowed_kill.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        r#"
            #[cfg_attr(test, cfg(any()))]
            let kill = |_child: &mut Child, _target: Target| Ok::<(), Failure>(());
        "#,
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "",
    );

    let expected = "successful external kill must consume Running into KillIssued";
    for (name, mutation) in [
        ("disabled", &disabled),
        ("widened", &widened),
        ("reactivated", &reactivated),
        ("shadowed", &shadowed),
    ] {
        let parsed = inventory(mutation);
        let stop = parsed
            .functions
            .iter()
            .find(|function| function.name == "stop_with");
        let violations = rust_boundary_violations(&valid_transport_fixture(), mutation);
        assert!(
            violations.iter().any(|hit| hit.contains(expected)),
            "{name} mutation escaped: body={:?}, structural={:?}, violations={violations:#?}",
            stop.map(|function| &function.body_tokens),
            stop.map(|function| function.consumes_running_before_wait),
        );
    }
}

#[test]
fn substituted_stop_arguments_and_shadowed_guard_macro_are_rejected() {
    let wrong_kill_target = stop_transition_fixture_with_calls(
        "sim/wrong_kill_target.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        "let wrong_target = *target;",
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "",
        "child, wrong_target",
        "child, *target",
    );
    let wrong_wait_target = stop_transition_fixture_with_calls(
        "sim/wrong_wait_target.rs",
        "*stop_phase == ExternalInterfaceStopPhase::Running",
        "let wrong_target = *target;",
        "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
        "",
        "child, *target",
        "child, wrong_target",
    );
    let shadowed_source = format!(
        "{}\n{}",
        r#"
            macro_rules! matches {
                ($value:expr, $pattern:pat) => { true };
            }
        "#,
        stop_transition_source(
            "matches!(stop_phase, ExternalInterfaceStopPhase::Running)",
            "",
            "*stop_phase = ExternalInterfaceStopPhase::KillIssued;",
            "",
            "child, *target",
            "child, *target",
        ),
    );
    let shadowed_guard = vec![RustSource::fixture(
        "sim/shadowed_guard_macro.rs",
        &shadowed_source,
    )];

    let expected = "successful external kill must consume Running into KillIssued";
    for mutation in [&wrong_kill_target, &wrong_wait_target, &shadowed_guard] {
        let violations = rust_boundary_violations(&valid_transport_fixture(), mutation);
        assert!(violations.iter().any(|hit| hit.contains(expected)));
    }
}

fn stop_transition_fixture(
    path: &str,
    guard: &str,
    before_guard: &str,
    inside_guard: &str,
    after_wait: &str,
) -> Vec<RustSource> {
    stop_transition_fixture_with_calls(
        path,
        guard,
        before_guard,
        inside_guard,
        after_wait,
        "child, *target",
        "child, *target",
    )
}

fn stop_transition_fixture_with_calls(
    path: &str,
    guard: &str,
    before_guard: &str,
    inside_guard: &str,
    after_wait: &str,
    kill_arguments: &str,
    wait_arguments: &str,
) -> Vec<RustSource> {
    let source = stop_transition_source(
        guard,
        before_guard,
        inside_guard,
        after_wait,
        kill_arguments,
        wait_arguments,
    );
    vec![RustSource::fixture(path, &source)]
}

fn stop_transition_source(
    guard: &str,
    before_guard: &str,
    inside_guard: &str,
    after_wait: &str,
    kill_arguments: &str,
    wait_arguments: &str,
) -> String {
    r#"
        struct ExternalInterfaceProcess;
        impl ExternalInterfaceProcess {
            fn stop_with(
                &mut self,
                kill: K,
                wait: W,
            ) -> Result<(), Failure> {
                let ExternalInterfaceProcessState::Owned {
                    child,
                    target,
                    stop_phase,
                } = &mut self.state
                else {
                    return Ok(());
                };
                $BEFORE_GUARD
                if $GUARD {
                    kill($KILL_ARGUMENTS)?;
                    $INSIDE_GUARD
                }
                let result = wait($WAIT_ARGUMENTS);
                $AFTER_WAIT
                if result.is_ok() {
                    self.state = ExternalInterfaceProcessState::Idle;
                }
                result
            }
        }
    "#
    .replace("$GUARD", guard)
    .replace("$BEFORE_GUARD", before_guard)
    .replace("$INSIDE_GUARD", inside_guard)
    .replace("$AFTER_WAIT", after_wait)
    .replace("$KILL_ARGUMENTS", kill_arguments)
    .replace("$WAIT_ARGUMENTS", wait_arguments)
}

#[test]
fn fallible_spawn_to_ownership_corridor_is_rejected() {
    let closed = spawn_corridor_fixture("sim/closed_spawn.rs", "", "", "", "");
    let fallible_gap = spawn_corridor_fixture(
        "sim/fallible_spawn.rs",
        "",
        "",
        "inspect_child(&child)?;",
        "",
    );

    let expected = "spawned Child must enter owned state";
    let closed_violations = rust_boundary_violations(&valid_transport_fixture(), &closed);
    let gap_violations = rust_boundary_violations(&valid_transport_fixture(), &fallible_gap);

    assert!(!closed_violations.iter().any(|hit| hit.contains(expected)));
    assert!(gap_violations.iter().any(|hit| hit.contains(expected)));
}

#[test]
fn disabled_ownership_decoy_and_extra_spawn_are_rejected() {
    let disabled_decoy = spawn_corridor_fixture(
        "sim/disabled_ownership.rs",
        "",
        "",
        "",
        r#"
            #[cfg(any())]
            self.state = ExternalInterfaceProcessState::Owned {
                child,
                target,
                stop_phase: ExternalInterfaceStopPhase::Running,
            };
            inspect_child(&child)?;
        "#,
    );
    let orphan = spawn_corridor_fixture(
        "sim/orphan_spawn.rs",
        "",
        "let _orphan = cmd.spawn().map_err(|source| Failure::Spawn { source })?;",
        "",
        "",
    );
    let aliased = spawn_corridor_fixture(
        "sim/aliased_spawn.rs",
        "",
        r#"
            let spawn_alias = Command::spawn;
            #[cfg_attr(test, cfg(any()))]
            let _orphan = spawn_alias(&mut cmd)
                .map_err(|source| Failure::Spawn { source })?;
        "#,
        "",
        "",
    );
    let macro_hidden = spawn_corridor_fixture(
        "sim/macro_spawn.rs",
        r#"
            macro_rules! launch_external {
                ($command:expr) => {{
                    let _orphan = $command.spawn();
                }};
            }
        "#,
        "launch_external!(cmd);",
        "",
        "",
    );
    let helper_hidden = spawn_corridor_fixture(
        "sim/helper_spawn.rs",
        r#"
            fn launch_external(command: &mut Command) -> Result<(), Failure> {
                let _orphan = command
                    .spawn()
                    .map_err(|source| Failure::Spawn { source })?;
                Ok(())
            }
        "#,
        "launch_external(&mut cmd)?;",
        "",
        "",
    );
    let expected = "spawned Child must enter owned state";
    for mutation in [
        &disabled_decoy,
        &orphan,
        &aliased,
        &macro_hidden,
        &helper_hidden,
    ] {
        let violations = rust_boundary_violations(&valid_transport_fixture(), mutation);
        assert!(violations.iter().any(|hit| hit.contains(expected)));
    }
}

#[test]
fn spawn_authority_hidden_behind_admitted_prelude_calls_is_rejected() {
    let stop_hidden = spawn_corridor_fixture(
        "sim/stop_hidden_spawn.rs",
        r#"
            fn spawn_during_stop() {
                let mut command = Command::new("hidden");
                let _orphan = command.spawn();
            }
            impl ExternalInterfaceProcess {
                fn stop(&mut self) -> Result<(), Failure> {
                    spawn_during_stop();
                    Ok(())
                }
            }
        "#,
        "",
        "",
        "",
    );
    let pdeathsig_hidden = spawn_corridor_fixture(
        "sim/pdeathsig_hidden_spawn.rs",
        r#"
            fn install_pdeathsig(command: &mut Command) {
                let _orphan = command.spawn();
            }
        "#,
        "",
        "",
        "",
    );

    let expected = "spawned Child must enter owned state";
    for mutation in [&stop_hidden, &pdeathsig_hidden] {
        let violations = rust_boundary_violations(&valid_transport_fixture(), mutation);
        assert!(violations.iter().any(|hit| hit.contains(expected)));
    }
}

#[test]
fn process_group_setup_moved_after_ownership_is_rejected() {
    let moved = spawn_corridor_fixture_with_layout(
        "sim/process_group_after_ownership.rs",
        "",
        "",
        "",
        "",
        "",
        PROCESS_GROUP_PRELUDE,
    );

    let violations = rust_boundary_violations(&valid_transport_fixture(), &moved);

    assert!(
        violations
            .iter()
            .any(|hit| hit.contains("spawned Child must enter owned state"))
    );
}

const PROCESS_GROUP_PRELUDE: &str = r#"
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        cmd.process_group(0);
    }
"#;

fn spawn_corridor_fixture(
    path: &str,
    support: &str,
    before_child: &str,
    after_child: &str,
    before_ownership: &str,
) -> Vec<RustSource> {
    spawn_corridor_fixture_with_layout(
        path,
        support,
        before_child,
        after_child,
        before_ownership,
        PROCESS_GROUP_PRELUDE,
        "",
    )
}

fn spawn_corridor_fixture_with_layout(
    path: &str,
    support: &str,
    before_child: &str,
    after_child: &str,
    before_ownership: &str,
    process_group: &str,
    after_ownership: &str,
) -> Vec<RustSource> {
    let source = r#"
        struct ExternalInterfaceProcess;
        $SUPPORT
        impl ExternalInterfaceProcess {
            fn start(&mut self) -> Result<(), Failure> {
                self.stop()?;
                write_control_diagnostic(format_args!(
                    "[external_interface] starting: {}",
                    self.command
                ));
                let mut cmd = Command::new(&self.command);
                cmd.stdin(Stdio::null());
                if tracing::enabled!(target: "rumoca_sim::external_interface", tracing::Level::DEBUG)
                    || tracing::enabled!(target: "rumoca_sim::autopilot", tracing::Level::DEBUG)
                {
                    cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
                } else {
                    cmd.stdout(Stdio::null()).stderr(Stdio::null());
                }
                $PROCESS_GROUP
                #[cfg(target_os = "linux")]
                install_pdeathsig(&mut cmd);
                $BEFORE_CHILD
                let child = cmd.spawn().map_err(|source| Failure::Spawn { source })?;
                $AFTER_CHILD
                let pid = child.id();
                #[cfg(unix)]
                let target = ExternalInterfaceStopTarget::ProcessGroup {
                    leader_pid: pid,
                    pgid: pid,
                };
                #[cfg(not(unix))]
                let target = ExternalInterfaceStopTarget::DirectChild { pid };
                $BEFORE_OWNERSHIP
                self.state = ExternalInterfaceProcessState::Owned {
                    child,
                    target,
                    stop_phase: ExternalInterfaceStopPhase::Running,
                };
                $AFTER_OWNERSHIP
                write_control_diagnostic("spawned");
                Ok(())
            }
        }
    "#
    .replace("$SUPPORT", support)
    .replace("$BEFORE_CHILD", before_child)
    .replace("$AFTER_CHILD", after_child)
    .replace("$BEFORE_OWNERSHIP", before_ownership)
    .replace("$PROCESS_GROUP", process_group)
    .replace("$AFTER_OWNERSHIP", after_ownership);
    vec![RustSource::fixture(path, &source)]
}

#[test]
fn cancellation_and_spawn_ownership_order_mutations_are_rejected() {
    let simulation = vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            struct ExternalInterfaceProcess;
            struct ExternalInterfaceHandle;
            enum ExternalInterfaceStartFailure { ShutdownRequested }
            enum ExternalInterfaceProcessState { Idle, Owned { child: Child, target: Target, stop_phase: Phase } }
            impl ExternalInterfaceProcess {
                fn start(&mut self, mut cmd: Command) {
                    let child = cmd.spawn();
                    write_control_diagnostic("spawned");
                    self.state = ExternalInterfaceProcessState::Owned { child, target, stop_phase };
                }
            }
            impl ExternalInterfaceHandle {
                fn start_with(&self) { start_process(); }
                fn cancel(&self) { process.stop(); lifecycle.cancelled = true; }
            }
            fn signal_controller_loop() {
                write_control_diagnostic("signal");
                request_shutdown();
                stop_external_interface_shared();
                signal_action_after_cleanup();
                std::process::exit(130);
            }
        "#,
    )];

    let violations = rust_boundary_violations(&valid_transport_fixture(), &simulation);

    for expected in [
        "spawned Child must enter owned state",
        "start must refuse the terminal cancelled state",
        "cancellation must become terminal before process cleanup",
        "signal loop must terminally request shutdown",
    ] {
        assert!(
            violations.iter().any(|hit| hit.contains(expected)),
            "missing `{expected}` in {violations:#?}"
        );
    }
}

#[test]
fn linux_only_libc_dependency_does_not_claim_cross_unix_process_control() {
    let linux_only = r#"
        [target.'cfg(all(target_os = "linux", not(target_arch = "wasm32")))'.dependencies]
        libc = "0.2"
    "#;
    let cross_unix = r#"
        [target.'cfg(all(unix, not(target_arch = "wasm32")))'.dependencies]
        libc = "0.2"
    "#;

    assert!(!manifest_has_cross_unix_libc(linux_only));
    assert!(manifest_has_cross_unix_libc(cross_unix));
}
