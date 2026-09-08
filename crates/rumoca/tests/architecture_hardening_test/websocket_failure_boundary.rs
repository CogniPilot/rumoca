//! SPEC_0029 §12 / SPEC_0041 §4 WebSocket fail-closed ownership gate.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use quote::ToTokens;
use syn::visit::{self, Visit};

use super::architecture_hardening_support::{
    attributes_require_test, collect_rs_files, workspace_root,
};

const SOCKET_SETUP_OWNERS: &[(&str, &[&str])] = &[
    (
        "set_nonblocking",
        &["bind", "configure_peer_after_handshake"],
    ),
    (
        "set_read_timeout",
        &["prepare_accepted_socket", "configure_peer_after_handshake"],
    ),
    ("set_write_timeout", &["prepare_accepted_socket"]),
];

const MUST_OBSERVE_METHODS: &[&str] = &[
    "set_nonblocking",
    "set_read_timeout",
    "set_write_timeout",
    "run_scoped",
    "try_next_event",
    "publish_state",
    "into_parts",
];

mod mutation_tests;

#[test]
fn websocket_listener_and_sim_caller_are_fail_closed() {
    let root = workspace_root();
    let transport = rust_sources_below(&root.join("crates/rumoca-transport-websocket/src"));
    let simulation = rust_sources_below(&root.join("crates/rumoca-sim/src"));
    let violations = rust_boundary_violations(&transport, &simulation);

    assert!(
        violations.is_empty(),
        "WebSocket ownership must stay scoped, typed, and fail closed:\n{}",
        violations.join("\n")
    );
}

#[test]
fn process_group_control_declares_libc_for_every_supported_unix_target() {
    let manifest = fs::read_to_string(workspace_root().join("crates/rumoca-sim/Cargo.toml"))
        .expect("read rumoca-sim manifest");
    assert!(
        manifest_has_cross_unix_libc(&manifest),
        "Unix process-group control requires libc on every non-Wasm Unix target"
    );
}

fn manifest_has_cross_unix_libc(manifest: &str) -> bool {
    let Ok(manifest) = toml::from_str::<toml::Value>(manifest) else {
        return false;
    };
    manifest
        .get("target")
        .and_then(|target| target.get("cfg(all(unix, not(target_arch = \"wasm32\")))"))
        .and_then(|target| target.get("dependencies"))
        .is_some_and(|dependencies| dependencies.get("libc").is_some())
}

#[derive(Clone)]
struct RustSource {
    path: PathBuf,
    text: String,
}

impl RustSource {
    fn fixture(path: &str, text: &str) -> Self {
        syn::parse_file(text).unwrap_or_else(|error| panic!("parse fixture {path}: {error}"));
        Self {
            path: PathBuf::from(path),
            text: text.to_string(),
        }
    }
}

fn rust_sources_below(root: &Path) -> Vec<RustSource> {
    let mut paths = Vec::new();
    collect_rs_files(root, &mut paths);
    paths.sort();
    paths
        .into_iter()
        .map(|path| RustSource {
            text: fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display())),
            path,
        })
        .collect()
}

#[derive(Default)]
struct RustInventory {
    methods: Vec<MethodRecord>,
    functions: Vec<FunctionRecord>,
    calls: Vec<CallRecord>,
    spawn_authorities: Vec<PathBuf>,
    ignored_results: Vec<String>,
    forbidden_error_vectors: Vec<String>,
    structs: BTreeMap<String, StructRecord>,
    enums: BTreeMap<String, EnumRecord>,
}

#[derive(Clone)]
struct MethodRecord {
    path: PathBuf,
    implementation: String,
    name: String,
    public: bool,
    argument_count: usize,
    result_error: Option<String>,
}

#[derive(Clone)]
struct FunctionRecord {
    path: PathBuf,
    name: String,
    implementation: Option<String>,
    argument_count: usize,
    body_tokens: String,
    identifiers: BTreeSet<String>,
    top_level_call_order: Vec<String>,
    consumes_running_before_wait: bool,
    stop_lifecycle_corridor_is_closed: bool,
    spawn_ownership_corridor_is_closed: bool,
    requires_single_command_field: bool,
    terminates_on_peer_report_failure: bool,
    tries_loop_result: bool,
    tries_run_scoped: bool,
    tries_start: bool,
}

#[derive(Clone)]
struct CallRecord {
    path: PathBuf,
    implementation: Option<String>,
    owner: String,
    name: String,
    method: bool,
    argument_count: usize,
}

struct StructRecord {
    fields: BTreeMap<String, FieldRecord>,
}

struct FieldRecord {
    public: bool,
    shape: TypeShape,
}

struct EnumRecord {
    variants: BTreeMap<String, usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeShape {
    Plain(String),
    Option(String),
    OptionalBox(String),
    BoxedSlice(String),
    Other,
}

fn rust_boundary_violations(
    transport_sources: &[RustSource],
    simulation_sources: &[RustSource],
) -> Vec<String> {
    let transport = inventory(transport_sources);
    let simulation = inventory(simulation_sources);
    let mut violations = Vec::new();

    require_scoped_owner(&transport, &mut violations);
    require_closed_terminal_product(&transport, &mut violations);
    require_closed_command_protocol(&transport, &mut violations);
    require_finite_resource_ownership(&transport, &mut violations);
    require_socket_owners(&transport, &mut violations);
    require_simulation_completion(&simulation, &mut violations);
    require_closed_peer_observer(&simulation, &mut violations);
    require_owned_external_shutdown(&simulation, &mut violations);
    require_owned_signal_controller(&simulation, &mut violations);
    violations.extend(transport.ignored_results);
    violations.extend(simulation.ignored_results);
    violations.extend(transport.forbidden_error_vectors);
    violations.extend(simulation.forbidden_error_vectors);
    violations
}

fn require_closed_command_protocol(inventory: &RustInventory, violations: &mut Vec<String>) {
    require_command_vocabulary(inventory, violations);
    if !require_apply_command_owner(inventory, violations) {
        return;
    }
    require_command_delivery(inventory, violations);
    require_key_command_construction(inventory, violations);
}

fn require_command_vocabulary(inventory: &RustInventory, violations: &mut Vec<String>) {
    let expected_commands = BTreeMap::from([
        ("Key".into(), 1),
        ("Quit".into(), 0),
        ("Realtime".into(), 1),
    ]);
    if inventory
        .enums
        .get("ViewerControlCommand")
        .map(|record| &record.variants)
        != Some(&expected_commands)
    {
        violations
            .push("ViewerControlCommand must remain a closed typed command vocabulary".into());
    }
    let expected_key_fields = BTreeMap::from([
        ("alt", TypeShape::Plain("bool".into())),
        ("code", TypeShape::Plain("ViewerKeyCode".into())),
        ("ctrl", TypeShape::Plain("bool".into())),
        ("pressed", TypeShape::Plain("bool".into())),
        ("shift", TypeShape::Plain("bool".into())),
    ]);
    let key_fields = inventory.structs.get("ViewerKeyCommand").map(|record| {
        record
            .fields
            .iter()
            .map(|(name, field)| (name.as_str(), field.shape.clone()))
            .collect::<BTreeMap<_, _>>()
    });
    if key_fields.as_ref() != Some(&expected_key_fields)
        || inventory.methods.iter().any(|method| {
            method.implementation == "ViewerKeyCommand" && method.name == "code_label"
        })
    {
        violations.push(
            "ViewerKeyCommand must retain only the parsed code and typed flags, never redundant key text"
                .into(),
        );
    }
}

fn require_apply_command_owner(inventory: &RustInventory, violations: &mut Vec<String>) -> bool {
    let Some(apply_command) = inventory
        .functions
        .iter()
        .find(|function| function.name == "apply_command")
    else {
        violations.push("missing fail-closed apply_command owner".into());
        return false;
    };
    if !apply_command.requires_single_command_field {
        violations.push(
            "apply_command must require exactly one top-level field before any effect".into(),
        );
    }
    true
}

fn require_command_delivery(inventory: &RustInventory, violations: &mut Vec<String>) {
    if inventory
        .functions
        .iter()
        .any(|function| function.name == "optional_bool")
    {
        violations.push("optional_bool reintroduces successful no-op command states".into());
    }
    let apply_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "apply_command")
        .map(|call| call.name.as_str())
        .collect();
    if apply_calls.contains("store") {
        violations.push("transport apply_command must not mutate simulation policy state".into());
    }
    let send_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "send_control")
        .map(|call| call.name.as_str())
        .collect();
    if !send_calls.contains("try_send") || send_calls.contains("send") {
        violations.push(
            "send_control must use nonblocking bounded-queue delivery with typed saturation".into(),
        );
    }
    if inventory
        .enums
        .get("PeerFailureKind")
        .and_then(|record| record.variants.get("ControlQueueSaturated"))
        != Some(&0)
    {
        violations
            .push("bounded viewer-control saturation must remain typed and observable".into());
    }
    let drain_inbound = inventory
        .functions
        .iter()
        .find(|function| function.name == "drain_inbound");
    let drain_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "drain_inbound")
        .map(|call| call.name.as_str())
        .collect();
    if drain_inbound.is_none_or(|function| {
        !function
            .identifiers
            .contains("MAX_INBOUND_MESSAGES_PER_PASS")
            || !function.identifiers.contains("quit")
    }) || !drain_calls.contains("load")
    {
        violations.push(
            "drain_inbound must bound each pass and observe internal shutdown per message".into(),
        );
    }
}

fn require_key_command_construction(inventory: &RustInventory, violations: &mut Vec<String>) {
    let try_new = inventory
        .methods
        .iter()
        .find(|method| method.implementation == "ViewerKeyCommand" && method.name == "try_new");
    if try_new.is_none_or(|method| method.argument_count != 5) {
        violations.push(
            "ViewerKeyCommand construction must contain physical code plus four typed flags only"
                .into(),
        );
    }
    let map_functions: Vec<_> = inventory
        .functions
        .iter()
        .filter(|function| function.name == "map_browser_key_code")
        .collect();
    if map_functions.len() != 1 || map_functions[0].argument_count != 1 {
        violations.push(
            "map_browser_key_code must classify only the closed physical-code catalog".into(),
        );
    }
    for call in inventory
        .calls
        .iter()
        .filter(|call| call.name == "map_browser_key_code")
    {
        if call.argument_count != 1 {
            violations.push(format!(
                "{}: map_browser_key_code call reintroduced a key-text fallback",
                call.path.display()
            ));
        }
    }
}

fn require_scoped_owner(inventory: &RustInventory, violations: &mut Vec<String>) {
    require_run_scoped_surface(inventory, violations);
    require_private_lifecycle_helpers(inventory, violations);
    require_scoped_startup_operations(inventory, violations);
    require_state_fanout_startup(inventory, violations);
}

fn require_run_scoped_surface(inventory: &RustInventory, violations: &mut Vec<String>) {
    let public_run_scoped = inventory
        .methods
        .iter()
        .filter(|method| {
            method.implementation == "BroadcastServer"
                && method.name == "run_scoped"
                && method.public
        })
        .count();
    if public_run_scoped != 1 {
        violations.push(format!(
            "BroadcastServer must expose exactly one public run_scoped method, found {public_run_scoped}"
        ));
    }
    if inventory
        .methods
        .iter()
        .find(|method| {
            method.implementation == "BroadcastServer"
                && method.name == "run_scoped"
                && method.public
        })
        .is_none_or(|method| method.argument_count != 3)
    {
        violations.push(
            "run_scoped must accept only the bounded control sender and scoped operation".into(),
        );
    }
    if inventory
        .methods
        .iter()
        .find(|method| {
            method.implementation == "BroadcastServer"
                && method.name == "run_scoped"
                && method.public
        })
        .and_then(|method| method.result_error.as_deref())
        != Some("BroadcastServerTerminalFailures")
    {
        violations.push(
            "run_scoped startup refusal must return BroadcastServerTerminalFailures intact".into(),
        );
    }
}

fn require_private_lifecycle_helpers(inventory: &RustInventory, violations: &mut Vec<String>) {
    for method in &inventory.methods {
        if method.name == "start" && method.public {
            violations.push(format!(
                "{}: public start makes WebSocket ownership detachable",
                method.path.display()
            ));
        }
        if method.implementation == "RunningBroadcastServer"
            && method.name == "finish"
            && method.public
        {
            violations.push(format!(
                "{}: public finish exposes the detachable lifecycle",
                method.path.display()
            ));
        }
    }
}

fn require_scoped_startup_operations(inventory: &RustInventory, violations: &mut Vec<String>) {
    let run_scoped_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "run_scoped")
        .map(|call| call.name.as_str())
        .collect();
    for required in ["catch_unwind", "finish", "panic_any"] {
        if !run_scoped_calls.contains(required) {
            violations.push(format!(
                "run_scoped must own `{required}` so panic and return paths both join"
            ));
        }
    }
    let start_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "start")
        .map(|call| call.name.as_str())
        .collect();
    for required in ["sync_channel", "recv", "join_server_failures"] {
        if !start_calls.contains(required) {
            violations.push(format!(
                "start must own readiness operation `{required}` before lending the server"
            ));
        }
    }
    if !start_calls.contains("store") {
        violations.push(
            "server-thread panic catch must publish internal shutdown before returning its typed slot"
                .into(),
        );
    }
}

fn require_state_fanout_startup(inventory: &RustInventory, violations: &mut Vec<String>) {
    let run_calls: Vec<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "run")
        .map(|call| call.name.as_str())
        .collect();
    if !run_calls.contains(&"spawn_state_fanout")
        || run_calls.iter().filter(|call| **call == "send").count() < 2
    {
        violations.push(
            "run must spawn state fanout and report both Ready and Failed through the startup channel"
                .into(),
        );
    }
    let fanout_start_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "spawn_state_fanout")
        .map(|call| call.name.as_str())
        .collect();
    for required in ["sync_channel", "spawn", "send", "recv", "join"] {
        if !fanout_start_calls.contains(required) {
            violations.push(format!(
                "spawn_state_fanout must positively acknowledge worker startup through `{required}`"
            ));
        }
    }
    if !inventory.structs.contains_key("StateFanoutReady") {
        violations.push("state-fanout startup must have a typed readiness witness".into());
    }
}

fn require_finite_resource_ownership(inventory: &RustInventory, violations: &mut Vec<String>) {
    require_finite_transport_queues(inventory, violations);
    let Some(accept_loop) = bounded_accept_loop_owner(inventory, violations) else {
        return;
    };
    require_acceptance_ordering(inventory, accept_loop, violations);
    require_bounded_peer_failure_retention(inventory, violations);
    require_bounded_websocket_config(inventory, violations);
}

fn require_finite_transport_queues(inventory: &RustInventory, violations: &mut Vec<String>) {
    if inventory.calls.iter().any(|call| call.name == "channel") {
        violations.push("WebSocket transport must not own an unbounded mpsc channel".into());
    }
    let start = inventory
        .functions
        .iter()
        .find(|function| function.name == "start");
    if start.is_none_or(|function| !function.identifiers.contains("EVENT_QUEUE_CAPACITY")) {
        violations.push("start must construct the server-event queue with a fixed capacity".into());
    }
    let mailbox_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "publish")
        .map(|call| call.name.as_str())
        .collect();
    if !inventory.structs.contains_key("StatePublicationMailbox")
        || !mailbox_calls.contains("replace_latest")
        || !mailbox_calls.contains("notify_one")
    {
        violations
            .push("state publication must use the finite latest-state overwrite mailbox".into());
    }
    let expected_observer = BTreeMap::from([("Closed".into(), 1), ("Saturated".into(), 2)]);
    if inventory
        .enums
        .get("EventObserverFailure")
        .map(|record| &record.variants)
        != Some(&expected_observer)
    {
        violations.push("event delivery failure must remain a closed typed aggregate".into());
    }
    let event_delivery_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "try_send_event")
        .map(|call| call.name.as_str())
        .collect();
    if !event_delivery_calls.contains("try_send") || event_delivery_calls.contains("send") {
        violations.push("server events must use nonblocking bounded-queue delivery".into());
    }
    let report_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "report_peer_failure")
        .map(|call| call.name.as_str())
        .collect();
    for required in ["try_send_event", "record_event_observer_failure"] {
        if !report_calls.contains(required) {
            violations.push(format!(
                "event saturation must retain each rejected peer record through `{required}`"
            ));
        }
    }
}

fn bounded_accept_loop_owner<'a>(
    inventory: &'a RustInventory,
    violations: &mut Vec<String>,
) -> Option<&'a FunctionRecord> {
    let accept_loop = inventory.functions.iter().find(|function| {
        function.name == "accept_loop"
            && function.implementation.as_deref() == Some("BroadcastServer")
    });
    if accept_loop.is_none() {
        violations.push("missing bounded accepted-worker owner".into());
    }
    accept_loop
}

fn require_acceptance_ordering(
    inventory: &RustInventory,
    accept_loop: &FunctionRecord,
    violations: &mut Vec<String>,
) {
    let accept_position = |name: &str| {
        accept_loop
            .top_level_call_order
            .iter()
            .position(|call| call == name)
    };
    if !matches!(
        (
            accept_position("reap_finished_workers"),
            accept_position("accept"),
            accept_position("handle_accept_result"),
        ),
        (Some(reap), Some(accept), Some(handle)) if reap < accept && accept < handle
    ) {
        violations.push(format!(
            "accept_loop must reap churn and prove worker capacity before each spawn: {:?}",
            accept_loop.top_level_call_order
        ));
    }
    let accepted_handler = inventory
        .functions
        .iter()
        .find(|function| function.name == "handle_accepted_socket");
    let handler_position = |name: &str| {
        accepted_handler.and_then(|function| {
            function
                .top_level_call_order
                .iter()
                .position(|call| call == name)
        })
    };
    if !matches!(
        (
            handler_position("connection_worker_capacity_available"),
            handler_position("spawn_connection_worker"),
        ),
        (Some(capacity), Some(spawn)) if capacity < spawn
    ) {
        violations.push("accepted-socket handling must prove worker capacity before spawn".into());
    }
    if !accept_loop.terminates_on_peer_report_failure {
        violations.push(
            "event saturation must terminate acceptance before a second capacity rejection".into(),
        );
    }
}

fn require_bounded_peer_failure_retention(inventory: &RustInventory, violations: &mut Vec<String>) {
    let peer_report_owners: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.name == "report_peer_failure")
        .map(|call| call.owner.as_str())
        .collect();
    if peer_report_owners != BTreeSet::from(["handle_accepted_socket", "process_worker_join"]) {
        violations.push(format!(
            "unobserved peer records must remain bounded by one acceptance refusal plus capped workers: {peer_report_owners:?}"
        ));
    }
    let capacity = inventory
        .functions
        .iter()
        .find(|function| function.name == "connection_worker_capacity_available");
    if capacity.is_none_or(|function| !function.identifiers.contains("MAX_CONNECTION_WORKERS")) {
        violations.push("accepted WebSocket workers must have a fixed maximum".into());
    }
}

fn require_bounded_websocket_config(inventory: &RustInventory, violations: &mut Vec<String>) {
    let config_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "viewer_websocket_config")
        .map(|call| call.name.as_str())
        .collect();
    if !config_calls.contains("max_message_size")
        || !config_calls.contains("max_frame_size")
        || inventory
            .functions
            .iter()
            .find(|function| function.name == "viewer_websocket_config")
            .is_none_or(|function| !function.identifiers.contains("MAX_INBOUND_MESSAGE_BYTES"))
    {
        violations
            .push("accepted WebSockets must have fixed inbound frame and message limits".into());
    }
}

fn require_closed_terminal_product(inventory: &RustInventory, violations: &mut Vec<String>) {
    let expected = BTreeMap::from([
        (
            "accept_loop",
            TypeShape::OptionalBox("WebSocketServerError".into()),
        ),
        (
            "state_source_close",
            TypeShape::OptionalBox("WebSocketServerError".into()),
        ),
        (
            "latest_state_close",
            TypeShape::OptionalBox("WebSocketServerError".into()),
        ),
        (
            "state_fanout",
            TypeShape::OptionalBox("WebSocketServerError".into()),
        ),
        (
            "event_observer",
            TypeShape::OptionalBox("EventObserverFailure".into()),
        ),
        (
            "startup_observer",
            TypeShape::OptionalBox("WebSocketServerError".into()),
        ),
        (
            "server_thread",
            TypeShape::OptionalBox("WebSocketServerError".into()),
        ),
        (
            "peer_workers",
            TypeShape::BoxedSlice("PeerWorkerTerminalFailureRecord".into()),
        ),
    ]);
    let Some(product) = inventory.structs.get("BroadcastServerTerminalFailures") else {
        violations.push("missing BroadcastServerTerminalFailures closed product".into());
        return;
    };
    let actual: BTreeMap<_, _> = product
        .fields
        .iter()
        .map(|(name, field)| (name.as_str(), field.shape.clone()))
        .collect();
    if actual != expected {
        violations.push(format!(
            "BroadcastServerTerminalFailures fields drifted from the closed participant product: {actual:?}"
        ));
    }
    for (name, field) in &product.fields {
        if field.public {
            violations.push(format!(
                "BroadcastServerTerminalFailures.{name} must remain private"
            ));
        }
    }
    let Some(events) = inventory.enums.get("BroadcastServerEvent") else {
        violations.push("missing BroadcastServerEvent".into());
        return;
    };
    if events.variants.get("TerminalFailure") != Some(&0) {
        violations.push(
            "BroadcastServerEvent::TerminalFailure must be a payload-free signal; the join aggregate owns causes"
                .into(),
        );
    }
    let Some(startup) = inventory.enums.get("BroadcastServerStartup") else {
        violations.push("missing closed BroadcastServerStartup readiness vocabulary".into());
        return;
    };
    if startup.variants != BTreeMap::from([("Failed".into(), 0), ("Ready".into(), 0)]) {
        violations.push("BroadcastServerStartup must remain exactly Ready | Failed".into());
    }
}

fn require_socket_owners(inventory: &RustInventory, violations: &mut Vec<String>) {
    for (method, allowed) in SOCKET_SETUP_OWNERS {
        for call in inventory.calls.iter().filter(|call| call.name == *method) {
            if !allowed.contains(&call.owner.as_str()) {
                violations.push(format!(
                    "{}: socket setup `{method}` moved into unapproved owner `{}`",
                    call.path.display(),
                    call.owner
                ));
            }
        }
    }
    for call in inventory
        .calls
        .iter()
        .filter(|call| call.method && call.name == "accept")
    {
        if call.owner != "accept_loop" {
            violations.push(format!(
                "{}: listener accept moved into unapproved owner `{}`",
                call.path.display(),
                call.owner
            ));
        }
    }
}

fn require_closed_peer_observer(inventory: &RustInventory, violations: &mut Vec<String>) {
    let Some(product) = inventory.structs.get("PeerFailureObservationFailure") else {
        violations.push("missing PeerFailureObservationFailure closed product".into());
        return;
    };
    let expected = BTreeMap::from([
        (
            "kind",
            TypeShape::Plain("PeerFailureObservationFailureKind".into()),
        ),
        ("failed", TypeShape::Plain("PeerFailureRecord".into())),
        (
            "unobserved",
            TypeShape::BoxedSlice("PeerFailureRecord".into()),
        ),
    ]);
    let actual: BTreeMap<_, _> = product
        .fields
        .iter()
        .map(|(name, field)| (name.as_str(), field.shape.clone()))
        .collect();
    if actual != expected {
        violations.push(format!(
            "PeerFailureObservationFailure must retain the failed record and boxed remainder: {actual:?}"
        ));
    }
    if product.fields.values().any(|field| field.public) {
        violations.push("PeerFailureObservationFailure fields must remain private".into());
    }
}

fn require_owned_external_shutdown(inventory: &RustInventory, violations: &mut Vec<String>) {
    require_external_shutdown_vocabulary(inventory, violations);
    require_external_process_transitions(inventory, violations);
    require_unix_process_group_shutdown(inventory, violations);
}

fn require_external_shutdown_vocabulary(inventory: &RustInventory, violations: &mut Vec<String>) {
    let expected_stop_failures = BTreeMap::from([
        ("Inspect".into(), 2),
        ("Kill".into(), 2),
        ("Timeout".into(), 1),
        ("Wait".into(), 2),
    ]);
    if inventory
        .enums
        .get("ExternalInterfaceStopFailure")
        .map(|failure| &failure.variants)
        != Some(&expected_stop_failures)
    {
        violations
            .push("external-interface stop failures must remain a closed typed vocabulary".into());
    }
    for (name, expected) in [
        (
            "ExternalInterfaceProcessState",
            BTreeMap::from([("Idle".into(), 0), ("Owned".into(), 3)]),
        ),
        (
            "ExternalInterfaceStopPhase",
            BTreeMap::from([("KillIssued".into(), 0), ("Running".into(), 0)]),
        ),
        (
            "ExternalInterfaceStartFailure",
            BTreeMap::from([
                ("ShutdownRequested".into(), 0),
                ("Spawn".into(), 2),
                ("Stop".into(), 1),
            ]),
        ),
        (
            "ExternalInterfaceStopTarget",
            BTreeMap::from([("DirectChild".into(), 1), ("ProcessGroup".into(), 2)]),
        ),
    ] {
        if inventory.enums.get(name).map(|record| &record.variants) != Some(&expected) {
            violations.push(format!("{name} must remain a closed ownership vocabulary"));
        }
    }
    let expected_lifecycle = BTreeMap::from([
        ("cancelled", TypeShape::Plain("bool".into())),
        (
            "process",
            TypeShape::Option("ExternalInterfaceProcess".into()),
        ),
    ]);
    let actual_lifecycle = inventory
        .structs
        .get("ExternalInterfaceLifecycle")
        .map(|record| {
            record
                .fields
                .iter()
                .map(|(name, field)| (name.as_str(), field.shape.clone()))
                .collect::<BTreeMap<_, _>>()
        });
    if actual_lifecycle.as_ref() != Some(&expected_lifecycle)
        || inventory
            .structs
            .get("ExternalInterfaceLifecycle")
            .is_some_and(|record| record.fields.values().any(|field| field.public))
    {
        violations.push(
            "ExternalInterfaceLifecycle must privately bind cancellation to optional process ownership"
                .into(),
        );
    }
}

fn require_external_process_transitions(inventory: &RustInventory, violations: &mut Vec<String>) {
    let stop_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| {
            call.implementation.as_deref() == Some("ExternalInterfaceProcess")
                && call.owner == "stop"
        })
        .map(|call| call.name.as_str())
        .collect();
    if !stop_calls.contains("stop_with") {
        violations.push(
            "ExternalInterfaceProcess::stop must transition through the retaining stop_with owner"
                .into(),
        );
    }
    let stop_with_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| {
            call.implementation.as_deref() == Some("ExternalInterfaceProcess")
                && call.owner == "stop_with"
        })
        .map(|call| call.name.as_str())
        .collect();
    if stop_with_calls.contains("take") || stop_with_calls.contains("replace") {
        violations.push(
            "ExternalInterfaceProcess::stop_with must never remove live Child authority before termination is proven"
                .into(),
        );
    }
    let stop_with = inventory.functions.iter().find(|function| {
        function.implementation.as_deref() == Some("ExternalInterfaceProcess")
            && function.name == "stop_with"
    });
    let stop_transition_is_ordered = stop_with.is_some_and(|function| {
        function.consumes_running_before_wait
            && function.stop_lifecycle_corridor_is_closed
            && function.top_level_call_order == ["Ok", "kill", "wait", "is_ok"]
            && function
                .top_level_call_order
                .iter()
                .filter(|call| call.as_str() == "kill")
                .count()
                == 1
            && function
                .top_level_call_order
                .iter()
                .filter(|call| call.as_str() == "wait")
                .count()
                == 1
    });
    if !stop_transition_is_ordered {
        violations.push(format!(
            "successful external kill must consume Running into KillIssued before the sole wait: calls={:?}, identifiers={:?}",
            stop_with.map(|function| &function.top_level_call_order),
            stop_with.map(|function| &function.identifiers),
        ));
    }
    require_external_spawn_transition(inventory, violations);
    require_external_handle_transitions(inventory, violations);
}

fn require_external_spawn_transition(inventory: &RustInventory, violations: &mut Vec<String>) {
    let start_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| {
            call.implementation.as_deref() == Some("ExternalInterfaceProcess")
                && call.owner == "start"
        })
        .map(|call| call.name.as_str())
        .collect();
    if !start_calls.contains("process_group") {
        violations.push(
            "Unix external-interface construction must create its owned process group".into(),
        );
    }
    let start = inventory.functions.iter().find(|function| {
        function.implementation.as_deref() == Some("ExternalInterfaceProcess")
            && function.name == "start"
    });
    let child_is_owned_before_diagnostic = start.is_some_and(|function| {
        let spawn_authorities = inventory
            .spawn_authorities
            .iter()
            .filter(|path| path.as_path() == function.path.as_path())
            .count();
        function.spawn_ownership_corridor_is_closed
            && spawn_authorities == 1
            && token_identifier_count(&function.body_tokens, "spawn") == 1
            && function
                .top_level_call_order
                .iter()
                .filter(|call| call.as_str() == "spawn")
                .count()
                == 1
            && ordered_token_positions(
                &function.body_tokens,
                &[
                    "self . state = ExternalInterfaceProcessState :: Owned",
                    "write_control_diagnostic",
                ],
            )
            .is_some()
    });
    if !child_is_owned_before_diagnostic {
        violations.push(format!(
            "spawned Child must enter owned state before any post-spawn diagnostic can unwind: body={:?}",
            start.map(|function| &function.body_tokens),
        ));
    }
}

fn require_external_handle_transitions(inventory: &RustInventory, violations: &mut Vec<String>) {
    let handle_start = inventory.functions.iter().find(|function| {
        function.implementation.as_deref() == Some("ExternalInterfaceHandle")
            && function.name == "start_with"
    });
    if handle_start.is_none_or(|function| {
        !function.body_tokens.contains("if lifecycle . cancelled")
            || !function.identifiers.contains("ShutdownRequested")
    }) {
        violations.push("external-interface start must refuse the terminal cancelled state".into());
    }
    let cancel = inventory.functions.iter().find(|function| {
        function.implementation.as_deref() == Some("ExternalInterfaceHandle")
            && function.name == "cancel"
    });
    if cancel.is_none_or(|function| {
        ordered_token_positions(
            &function.body_tokens,
            &["lifecycle . cancelled = true", ". stop ("],
        )
        .is_none()
    }) {
        violations.push(
            "external-interface cancellation must become terminal before process cleanup".into(),
        );
    }
}

fn require_unix_process_group_shutdown(inventory: &RustInventory, violations: &mut Vec<String>) {
    let group_kill_calls = inventory
        .calls
        .iter()
        .filter(|call| {
            call.owner == "kill_external_interface_target" && call.name == "kill" && !call.method
        })
        .count();
    if group_kill_calls != 1 {
        violations.push(format!(
            "Unix external-interface shutdown must own exactly one process-group kill syscall, found {group_kill_calls}"
        ));
    }
    let unix_wait = inventory.functions.iter().find(|function| {
        function.name == "wait_for_external_interface_exit"
            && function
                .identifiers
                .contains("process_group_has_live_members")
    });
    let Some(unix_wait) = unix_wait else {
        violations.push(
            "Unix KillIssued wait must prove leader exit and process-group extinction".into(),
        );
        return;
    };
    let reap = unix_wait
        .top_level_call_order
        .iter()
        .position(|call| call == "try_wait");
    let extinction = unix_wait
        .top_level_call_order
        .iter()
        .position(|call| call == "process_group_has_live_members");
    if reap.is_none() || extinction.is_none() {
        violations.push(
            "Unix KillIssued wait must prove leader exit and process-group extinction".into(),
        );
    }
}

fn require_owned_signal_controller(inventory: &RustInventory, violations: &mut Vec<String>) {
    require_signal_controller_owner(inventory, violations);
    require_signal_controller_install(inventory, violations);
    require_signal_controller_shutdown(inventory, violations);
    require_signal_controller_vocabulary(inventory, violations);
}

fn require_signal_controller_owner(inventory: &RustInventory, violations: &mut Vec<String>) {
    for forbidden in ["spawn_sigint_handler", "spawn_cleanup_thread"] {
        if inventory
            .functions
            .iter()
            .any(|function| function.name == forbidden)
        {
            violations.push(format!(
                "detached legacy signal owner `{forbidden}` is forbidden"
            ));
        }
    }
    let has_drop = inventory
        .methods
        .iter()
        .any(|method| method.implementation == "SignalController" && method.name == "drop");
    if !inventory.structs.contains_key("SignalController") || !has_drop {
        violations
            .push("simulation signals must remain owned by a dropping SignalController".into());
    }
}

fn signal_controller_calls<'a>(inventory: &'a RustInventory, owner: &str) -> BTreeSet<&'a str> {
    inventory
        .calls
        .iter()
        .filter(|call| {
            call.implementation.as_deref() == Some("SignalController") && call.owner == owner
        })
        .map(|call| call.name.as_str())
        .collect()
}

fn require_signal_controller_install(inventory: &RustInventory, violations: &mut Vec<String>) {
    let install_calls = signal_controller_calls(inventory, "install");
    for required in ["new", "handle", "spawn"] {
        if !install_calls.contains(required) {
            violations.push(format!(
                "SignalController::install must own typed signal setup operation `{required}`"
            ));
        }
    }
    if install_calls.contains("push") {
        violations.push(
            "owned signal cleanup must stop on its first retained failure, never grow an unbounded failure log"
                .into(),
        );
    }
    let signal_loop = inventory
        .functions
        .iter()
        .find(|function| function.name == "signal_controller_loop");
    if signal_loop.is_none_or(|function| {
        ordered_calls(
            function,
            &[
                "request_shutdown",
                "stop_external_interface_shared",
                "write_control_diagnostic",
                "signal_action_after_cleanup",
                "exit",
            ],
        )
        .is_none()
    }) {
        violations.push(
            "signal loop must terminally request shutdown and clean owned process state before diagnostics or forced exit"
                .into(),
        );
    }
    let request = inventory
        .functions
        .iter()
        .find(|function| function.name == "request_shutdown");
    if request.is_none_or(|function| {
        !function
            .top_level_call_order
            .iter()
            .any(|call| call == "store")
    }) {
        violations.push("initial signal must publish shutdown before cleanup".into());
    }
}

fn ordered_calls(function: &FunctionRecord, names: &[&str]) -> Option<()> {
    let mut previous = None;
    for name in names {
        let position = function
            .top_level_call_order
            .iter()
            .position(|call| call == name)?;
        if previous.is_some_and(|previous| previous >= position) {
            return None;
        }
        previous = Some(position);
    }
    Some(())
}

fn consumes_running_before_wait(block: &syn::Block) -> bool {
    block.stmts.windows(2).any(|pair| {
        let syn::Stmt::Expr(syn::Expr::If(phase_guard), _) = &pair[0] else {
            return false;
        };
        phase_guard.attrs.is_empty()
            && running_phase_guard(&phase_guard.cond)
            && phase_guard.then_branch.stmts.len() == 2
            && tried_free_call(&phase_guard.then_branch.stmts[0], "kill")
            && assigns_stop_phase(
                &phase_guard.then_branch.stmts[1],
                "ExternalInterfaceStopPhase",
                "KillIssued",
            )
            && plain_local_call(&pair[1], "result", "wait")
    })
}

fn stop_lifecycle_corridor_is_closed(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    block: &syn::Block,
) -> bool {
    let parameter_names = inputs
        .iter()
        .filter_map(|input| match input {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(argument) => pattern_identifier(&argument.pat),
        })
        .collect::<BTreeSet<_>>();
    let mut attributes = FunctionAttributes(0);
    attributes.visit_block(block);
    let body = block.to_token_stream().to_string();
    parameter_names.contains("kill")
        && parameter_names.contains("wait")
        && attributes.0 == 0
        && function_macros(block).is_empty()
        && token_identifier_count(&body, "stop_phase") == 3
        && token_identifier_count(&body, "kill") == 1
        && token_identifier_count(&body, "wait") == 1
}

fn pattern_identifier(pattern: &syn::Pat) -> Option<String> {
    let syn::Pat::Ident(identifier) = pattern else {
        return None;
    };
    Some(identifier.ident.to_string())
}

struct FunctionAttributes(usize);

impl<'ast> Visit<'ast> for FunctionAttributes {
    fn visit_attribute(&mut self, _attribute: &'ast syn::Attribute) {
        self.0 += 1;
    }
}

#[derive(Default)]
struct FunctionMacros(Vec<(String, String)>);

impl<'ast> Visit<'ast> for FunctionMacros {
    fn visit_macro(&mut self, expression: &'ast syn::Macro) {
        let name = expression
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .unwrap_or_default();
        self.0.push((name, expression.tokens.to_string()));
        visit::visit_macro(self, expression);
    }
}

fn function_macros(block: &syn::Block) -> Vec<(String, String)> {
    let mut macros = FunctionMacros::default();
    macros.visit_block(block);
    macros.0
}

fn token_identifier_count(tokens: &str, expected: &str) -> usize {
    tokens
        .split(|character: char| !(character.is_ascii_alphanumeric() || character == '_'))
        .filter(|token| *token == expected)
        .count()
}

fn running_phase_guard(expression: &syn::Expr) -> bool {
    let syn::Expr::Binary(comparison) = expression else {
        return false;
    };
    expression_is_attribute_free(expression)
        && matches!(comparison.op, syn::BinOp::Eq(_))
        && dereferences_identifier(&comparison.left, "stop_phase")
        && expression_path_ends_with(
            &comparison.right,
            &["ExternalInterfaceStopPhase", "Running"],
        )
}

fn tried_free_call(statement: &syn::Stmt, name: &str) -> bool {
    let syn::Stmt::Expr(tried @ syn::Expr::Try(expression), Some(_)) = statement else {
        return false;
    };
    expression_is_attribute_free(tried)
        && matches!(expression.expr.as_ref(), syn::Expr::Call(call)
            if call.attrs.is_empty()
                && bare_expression_identifier(&call.func).as_deref() == Some(name)
                && call_has_owned_child_target_arguments(call))
}

fn assigns_stop_phase(statement: &syn::Stmt, owner: &str, variant: &str) -> bool {
    let syn::Stmt::Expr(assigned @ syn::Expr::Assign(assignment), Some(_)) = statement else {
        return false;
    };
    expression_is_attribute_free(assigned)
        && dereferences_identifier(&assignment.left, "stop_phase")
        && expression_path_ends_with(&assignment.right, &[owner, variant])
}

fn plain_local_call(statement: &syn::Stmt, binding: &str, name: &str) -> bool {
    let syn::Stmt::Local(local) = statement else {
        return false;
    };
    local.attrs.is_empty()
        && pattern_is_identifier(&local.pat, binding)
        && local.init.as_ref().is_some_and(|init| {
            expression_is_attribute_free(&init.expr)
                && matches!(init.expr.as_ref(), syn::Expr::Call(call)
                if call.attrs.is_empty()
                    && bare_expression_identifier(&call.func).as_deref() == Some(name)
                    && call_has_owned_child_target_arguments(call))
        })
}

fn call_has_owned_child_target_arguments(call: &syn::ExprCall) -> bool {
    let mut arguments = call.args.iter();
    let Some(child) = arguments.next() else {
        return false;
    };
    let Some(target) = arguments.next() else {
        return false;
    };
    arguments.next().is_none()
        && bare_expression_identifier(child).as_deref() == Some("child")
        && dereferences_identifier(target, "target")
}

fn spawn_ownership_corridor_is_closed(block: &syn::Block) -> bool {
    let spawn_indices = block
        .stmts
        .iter()
        .enumerate()
        .filter_map(|(index, statement)| successful_spawn_local(statement).then_some(index))
        .collect::<Vec<_>>();
    let [spawn_index] = spawn_indices.as_slice() else {
        return false;
    };
    if !pre_spawn_surface_is_admitted(&block.stmts[..*spawn_index]) {
        return false;
    }
    let ownership_indices = block
        .stmts
        .iter()
        .enumerate()
        .filter_map(|(index, statement)| owns_spawned_child(statement).then_some(index))
        .collect::<Vec<_>>();
    let [ownership_index] = ownership_indices.as_slice() else {
        return false;
    };
    if ownership_index <= spawn_index {
        return false;
    }
    let mut saw_pid = false;
    let mut target_variants = BTreeSet::new();
    for statement in &block.stmts[spawn_index + 1..*ownership_index] {
        if pid_local(statement) && !saw_pid && target_variants.is_empty() {
            saw_pid = true;
            continue;
        }
        if let Some(variant) = target_local_variant(statement)
            && saw_pid
            && target_variants.insert(variant)
        {
            continue;
        }
        return false;
    }
    saw_pid && target_variants == BTreeSet::from(["DirectChild", "ProcessGroup"])
}

fn pre_spawn_surface_is_admitted(statements: &[syn::Stmt]) -> bool {
    let [
        stop,
        diagnostic,
        command,
        stdin,
        trace_stdio,
        process_group,
        pdeathsig,
    ] = statements
    else {
        return false;
    };
    if !self_stop_statement(stop)
        || !starting_diagnostic_statement(diagnostic)
        || !command_local_statement(command)
        || !stdin_statement(stdin)
        || !trace_stdio_statement(trace_stdio)
        || !process_group_statement(process_group)
        || !pdeathsig_statement(pdeathsig)
    {
        return false;
    }

    struct Surface {
        admitted: bool,
    }

    impl<'ast> Visit<'ast> for Surface {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            self.admitted &= admitted_pre_spawn_call(call);
            visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.admitted &= admitted_pre_spawn_method_call(call);
            visit::visit_expr_method_call(self, call);
        }

        fn visit_macro(&mut self, expression: &'ast syn::Macro) {
            self.admitted &= admitted_pre_spawn_macro(expression);
            visit::visit_macro(self, expression);
        }
    }

    let mut surface = Surface { admitted: true };
    for statement in statements {
        surface.visit_stmt(statement);
    }
    surface.admitted
}

fn self_stop_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(syn::Expr::Try(tried), Some(_)) = statement else {
        return false;
    };
    matches!(tried.expr.as_ref(), syn::Expr::MethodCall(call)
        if call.attrs.is_empty()
            && call.method == "stop"
            && call.args.is_empty()
            && bare_expression_identifier(&call.receiver).as_deref() == Some("self"))
}

fn starting_diagnostic_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(syn::Expr::Call(call), Some(_)) = statement else {
        return false;
    };
    let Some(syn::Expr::Macro(arguments)) = call.args.first() else {
        return false;
    };
    call.attrs.is_empty()
        && call.args.len() == 1
        && exact_expression_path(&call.func, &["write_control_diagnostic"])
        && admitted_pre_spawn_macro(&arguments.mac)
}

fn command_local_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Local(local) = statement else {
        return false;
    };
    let syn::Pat::Ident(pattern) = &local.pat else {
        return false;
    };
    let Some(initializer) = &local.init else {
        return false;
    };
    let syn::Expr::Call(call) = initializer.expr.as_ref() else {
        return false;
    };
    local.attrs.is_empty()
        && pattern.ident == "cmd"
        && pattern.mutability.is_some()
        && pattern.subpat.is_none()
        && initializer.diverge.is_none()
        && call.attrs.is_empty()
        && admitted_pre_spawn_call(call)
        && exact_expression_path(&call.func, &["Command", "new"])
}

fn stdin_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(syn::Expr::MethodCall(call), Some(_)) = statement else {
        return false;
    };
    call.attrs.is_empty()
        && call.method == "stdin"
        && bare_expression_identifier(&call.receiver).as_deref() == Some("cmd")
        && call.args.len() == 1
        && call
            .args
            .first()
            .is_some_and(|argument| stdio_constructor(argument, "null"))
}

fn trace_stdio_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(syn::Expr::If(branch), None) = statement else {
        return false;
    };
    let mut condition_macros = FunctionMacros::default();
    condition_macros.visit_expr(&branch.cond);
    let expected_macros = [
        (
            "enabled".to_string(),
            r#"target : "rumoca_sim::external_interface" , tracing :: Level :: DEBUG"#.to_string(),
        ),
        (
            "enabled".to_string(),
            r#"target : "rumoca_sim::autopilot" , tracing :: Level :: DEBUG"#.to_string(),
        ),
    ];
    let Some((_, alternate)) = &branch.else_branch else {
        return false;
    };
    let syn::Expr::Block(alternate) = alternate.as_ref() else {
        return false;
    };
    branch.attrs.is_empty()
        && condition_macros.0 == expected_macros
        && matches!(branch.then_branch.stmts.as_slice(), [statement]
            if stdio_output_pair_statement(statement, "inherit"))
        && matches!(alternate.block.stmts.as_slice(), [statement]
            if stdio_output_pair_statement(statement, "null"))
}

fn stdio_output_pair_statement(statement: &syn::Stmt, constructor: &str) -> bool {
    let syn::Stmt::Expr(syn::Expr::MethodCall(stderr), Some(_)) = statement else {
        return false;
    };
    let syn::Expr::MethodCall(stdout) = stderr.receiver.as_ref() else {
        return false;
    };
    stderr.attrs.is_empty()
        && stderr.method == "stderr"
        && stderr.args.len() == 1
        && stderr
            .args
            .first()
            .is_some_and(|argument| stdio_constructor(argument, constructor))
        && stdout.attrs.is_empty()
        && stdout.method == "stdout"
        && stdout.args.len() == 1
        && stdout
            .args
            .first()
            .is_some_and(|argument| stdio_constructor(argument, constructor))
        && bare_expression_identifier(&stdout.receiver).as_deref() == Some("cmd")
}

fn stdio_constructor(expression: &syn::Expr, constructor: &str) -> bool {
    matches!(expression, syn::Expr::Call(call)
        if call.attrs.is_empty()
            && call.args.is_empty()
            && exact_expression_path(&call.func, &["Stdio", constructor]))
}

fn process_group_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(syn::Expr::Block(scoped), None) = statement else {
        return false;
    };
    let [syn::Stmt::Item(syn::Item::Use(_)), group] = scoped.block.stmts.as_slice() else {
        return false;
    };
    let syn::Stmt::Expr(syn::Expr::MethodCall(call), Some(_)) = group else {
        return false;
    };
    cfg_predicate_is(&scoped.attrs, "unix")
        && call.attrs.is_empty()
        && call.method == "process_group"
        && bare_expression_identifier(&call.receiver).as_deref() == Some("cmd")
        && call.args.len() == 1
        && call.args.first().is_some_and(integer_literal_is_zero)
}

fn pdeathsig_statement(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(syn::Expr::Call(call), Some(_)) = statement else {
        return false;
    };
    cfg_predicate_is(&call.attrs, r#"target_os = "linux""#)
        && exact_expression_path(&call.func, &["install_pdeathsig"])
        && call.args.len() == 1
        && call
            .args
            .first()
            .is_some_and(|argument| mutable_reference_to_identifier(argument, "cmd"))
}

fn admitted_pre_spawn_call(call: &syn::ExprCall) -> bool {
    if exact_expression_path(&call.func, &["write_control_diagnostic"]) {
        return call.args.len() == 1;
    }
    if exact_expression_path(&call.func, &["install_pdeathsig"]) {
        return call.args.len() == 1
            && call
                .args
                .first()
                .is_some_and(|argument| mutable_reference_to_identifier(argument, "cmd"));
    }
    if exact_expression_path(&call.func, &["Command", "new"]) {
        return call.args.len() == 1 && call.args.first().is_some_and(reference_to_self_command);
    }
    (exact_expression_path(&call.func, &["Stdio", "null"])
        || exact_expression_path(&call.func, &["Stdio", "inherit"]))
        && call.args.is_empty()
}

fn admitted_pre_spawn_method_call(call: &syn::ExprMethodCall) -> bool {
    let method = call.method.to_string();
    if method == "stop" {
        return call.args.is_empty()
            && bare_expression_identifier(&call.receiver).as_deref() == Some("self");
    }
    if matches!(method.as_str(), "stdin" | "stdout" | "stderr") {
        return call.args.len() == 1
            && method_receiver_root_identifier(&call.receiver).as_deref() == Some("cmd");
    }
    method == "process_group"
        && method_receiver_root_identifier(&call.receiver).as_deref() == Some("cmd")
        && call.args.len() == 1
        && call.args.first().is_some_and(integer_literal_is_zero)
}

fn method_receiver_root_identifier(expression: &syn::Expr) -> Option<String> {
    match expression {
        syn::Expr::MethodCall(call) => method_receiver_root_identifier(&call.receiver),
        _ => bare_expression_identifier(expression),
    }
}

fn exact_expression_path(expression: &syn::Expr, expected: &[&str]) -> bool {
    let syn::Expr::Path(path) = expression else {
        return false;
    };
    path.qself.is_none()
        && path.path.leading_colon.is_none()
        && path.path.segments.len() == expected.len()
        && path
            .path
            .segments
            .iter()
            .zip(expected)
            .all(|(segment, expected)| segment.ident == *expected)
}

fn mutable_reference_to_identifier(expression: &syn::Expr, expected: &str) -> bool {
    matches!(expression, syn::Expr::Reference(reference)
        if reference.mutability.is_some()
            && bare_expression_identifier(&reference.expr).as_deref() == Some(expected))
}

fn reference_to_self_command(expression: &syn::Expr) -> bool {
    let syn::Expr::Reference(reference) = expression else {
        return false;
    };
    if reference.mutability.is_some() {
        return false;
    }
    let syn::Expr::Field(field) = reference.expr.as_ref() else {
        return false;
    };
    bare_expression_identifier(&field.base).as_deref() == Some("self")
        && matches!(&field.member, syn::Member::Named(name) if name == "command")
}

fn integer_literal_is_zero(expression: &syn::Expr) -> bool {
    matches!(expression, syn::Expr::Lit(literal)
        if matches!(&literal.lit, syn::Lit::Int(value) if value.base10_digits() == "0"))
}

fn admitted_pre_spawn_macro(expression: &syn::Macro) -> bool {
    if expression.path.is_ident("format_args") {
        return expression.tokens.to_string()
            == r#""[external_interface] starting: {}" , self . command"#;
    }
    expression.path.segments.len() == 2
        && expression.path.segments[0].ident == "tracing"
        && expression.path.segments[1].ident == "enabled"
        && matches!(
            expression.tokens.to_string().as_str(),
            r#"target : "rumoca_sim::external_interface" , tracing :: Level :: DEBUG"#
                | r#"target : "rumoca_sim::autopilot" , tracing :: Level :: DEBUG"#
        )
}

fn successful_spawn_local(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Local(local) = statement else {
        return false;
    };
    if !local.attrs.is_empty() || !pattern_is_identifier(&local.pat, "child") {
        return false;
    }
    let Some(initializer) = &local.init else {
        return false;
    };
    let syn::Expr::Try(tried) = initializer.expr.as_ref() else {
        return false;
    };
    let syn::Expr::MethodCall(map_error) = tried.expr.as_ref() else {
        return false;
    };
    let syn::Expr::MethodCall(spawn) = map_error.receiver.as_ref() else {
        return false;
    };
    expression_is_attribute_free(&initializer.expr)
        && tried.attrs.is_empty()
        && map_error.attrs.is_empty()
        && spawn.attrs.is_empty()
        && map_error.method == "map_err"
        && spawn.method == "spawn"
        && spawn.args.is_empty()
        && expression_identifier(&spawn.receiver).as_deref() == Some("cmd")
}

fn pid_local(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Local(local) = statement else {
        return false;
    };
    local.attrs.is_empty()
        && pattern_is_identifier(&local.pat, "pid")
        && local.init.as_ref().is_some_and(|initializer| {
            expression_is_attribute_free(&initializer.expr)
                && matches!(initializer.expr.as_ref(), syn::Expr::MethodCall(call)
                if call.attrs.is_empty()
                    && call.method == "id"
                    && call.args.is_empty()
                    && expression_identifier(&call.receiver).as_deref() == Some("child"))
        })
}

fn target_local_variant(statement: &syn::Stmt) -> Option<&'static str> {
    let syn::Stmt::Local(local) = statement else {
        return None;
    };
    if !pattern_is_identifier(&local.pat, "target") {
        return None;
    }
    let syn::Expr::Struct(target) = local.init.as_ref()?.expr.as_ref() else {
        return None;
    };
    if target.rest.is_some()
        || !expression_is_attribute_free(&local.init.as_ref()?.expr)
        || !target.attrs.is_empty()
        || !target_fields_are_pid(&target.fields)
    {
        return None;
    }
    let field_names = target
        .fields
        .iter()
        .filter_map(|field| match &field.member {
            syn::Member::Named(name) => Some(name.to_string()),
            syn::Member::Unnamed(_) => None,
        })
        .collect::<BTreeSet<_>>();
    match target.path.segments.last()?.ident.to_string().as_str() {
        "DirectChild"
            if cfg_predicate_is(&local.attrs, "not (unix)")
                && field_names == BTreeSet::from(["pid".into()]) =>
        {
            Some("DirectChild")
        }
        "ProcessGroup" if field_names == BTreeSet::from(["leader_pid".into(), "pgid".into()]) => {
            cfg_predicate_is(&local.attrs, "unix").then_some("ProcessGroup")
        }
        _ => None,
    }
}

fn target_fields_are_pid(
    fields: &syn::punctuated::Punctuated<syn::FieldValue, syn::Token![,]>,
) -> bool {
    fields
        .iter()
        .all(|field| expression_identifier(&field.expr).as_deref() == Some("pid"))
}

fn cfg_predicate_is(attributes: &[syn::Attribute], expected: &str) -> bool {
    let [attribute] = attributes else {
        return false;
    };
    let syn::Meta::List(configuration) = &attribute.meta else {
        return false;
    };
    configuration.path.is_ident("cfg") && configuration.tokens.to_string() == expected
}

fn owns_spawned_child(statement: &syn::Stmt) -> bool {
    let syn::Stmt::Expr(assigned @ syn::Expr::Assign(assignment), Some(_)) = statement else {
        return false;
    };
    let syn::Expr::Field(left) = assignment.left.as_ref() else {
        return false;
    };
    let syn::Expr::Struct(owned) = assignment.right.as_ref() else {
        return false;
    };
    expression_is_attribute_free(assigned)
        && assignment.attrs.is_empty()
        && left.attrs.is_empty()
        && owned.attrs.is_empty()
        && expression_identifier(&left.base).as_deref() == Some("self")
        && matches!(&left.member, syn::Member::Named(name) if name == "state")
        && owned
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "Owned")
        && owned.rest.is_none()
        && owned.fields.len() == 3
        && owned.fields.iter().all(owned_field_is_exact)
}

fn owned_field_is_exact(field: &syn::FieldValue) -> bool {
    let syn::Member::Named(name) = &field.member else {
        return false;
    };
    match name.to_string().as_str() {
        "child" => expression_identifier(&field.expr).as_deref() == Some("child"),
        "target" => expression_identifier(&field.expr).as_deref() == Some("target"),
        "stop_phase" => {
            expression_path_ends_with(&field.expr, &["ExternalInterfaceStopPhase", "Running"])
        }
        _ => false,
    }
}

fn pattern_is_identifier(pattern: &syn::Pat, expected: &str) -> bool {
    matches!(pattern, syn::Pat::Ident(identifier) if identifier.ident == expected)
}

fn bare_expression_identifier(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    if path.qself.is_some() || path.path.leading_colon.is_some() || path.path.segments.len() != 1 {
        return None;
    }
    path.path
        .segments
        .first()
        .map(|segment| segment.ident.to_string())
}

fn expression_is_attribute_free(expression: &syn::Expr) -> bool {
    struct AttributeFinder(bool);
    impl<'ast> Visit<'ast> for AttributeFinder {
        fn visit_attribute(&mut self, _attribute: &'ast syn::Attribute) {
            self.0 = true;
        }
    }
    let mut finder = AttributeFinder(false);
    finder.visit_expr(expression);
    !finder.0
}

fn dereferences_identifier(expression: &syn::Expr, identifier: &str) -> bool {
    matches!(expression, syn::Expr::Unary(unary)
        if matches!(unary.op, syn::UnOp::Deref(_))
            && bare_expression_identifier(&unary.expr).as_deref() == Some(identifier))
}

fn expression_path_ends_with(expression: &syn::Expr, suffix: &[&str]) -> bool {
    let syn::Expr::Path(path) = expression else {
        return false;
    };
    let segments = path
        .path
        .segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>();
    segments.len() >= suffix.len()
        && segments[segments.len() - suffix.len()..]
            .iter()
            .map(String::as_str)
            .eq(suffix.iter().copied())
}

fn ordered_token_positions(tokens: &str, needles: &[&str]) -> Option<()> {
    let mut previous = None;
    for needle in needles {
        let search_start = previous.map_or(0, |position| position + 1);
        let position = search_start + tokens.get(search_start..)?.find(needle)?;
        previous = Some(position);
    }
    Some(())
}

fn require_signal_controller_shutdown(inventory: &RustInventory, violations: &mut Vec<String>) {
    let shutdown_calls = signal_controller_calls(inventory, "shutdown");
    for required in ["close", "join"] {
        if !shutdown_calls.contains(required) {
            violations.push(format!(
                "SignalController::shutdown must close and join through `{required}`"
            ));
        }
    }
    let drop_calls = signal_controller_calls(inventory, "drop");
    if !drop_calls.contains("shutdown") {
        violations
            .push("SignalController::drop must invoke joined shutdown on unwind paths".into());
    }
}

fn require_signal_controller_vocabulary(inventory: &RustInventory, violations: &mut Vec<String>) {
    for (name, expected) in [
        (
            "SignalControllerInstallFailure",
            BTreeMap::from([("Register".into(), 1), ("ThreadSpawn".into(), 1)]),
        ),
        (
            "SignalControllerShutdownFailure",
            BTreeMap::from([("CleanupFailures".into(), 1), ("ThreadPanicked".into(), 0)]),
        ),
        (
            "SignalAction",
            BTreeMap::from([("Continue".into(), 0), ("ExitAfterCleanup".into(), 0)]),
        ),
        (
            "SignalStage",
            BTreeMap::from([("Forced".into(), 0), ("Initial".into(), 0)]),
        ),
    ] {
        if inventory.enums.get(name).map(|record| &record.variants) != Some(&expected) {
            violations.push(format!("{name} must remain a closed typed vocabulary"));
        }
    }
}

fn require_simulation_completion(inventory: &RustInventory, violations: &mut Vec<String>) {
    require_simulation_call_ownership(inventory, violations);
    let Some(run_loop) = simulation_run_loop_owner(inventory, violations) else {
        return;
    };
    require_simulation_cleanup_order(inventory, run_loop, violations);
    require_simulation_failure_products(inventory, violations);
    require_simulation_restart_ownership(inventory, violations);
}

fn require_simulation_call_ownership(inventory: &RustInventory, violations: &mut Vec<String>) {
    let run_scoped: Vec<_> = inventory
        .calls
        .iter()
        .filter(|call| call.name == "run_scoped")
        .collect();
    if run_scoped.len() != 1 || run_scoped[0].owner != "run_sim_loop" {
        violations.push(format!(
            "scheduled simulation must have one run_sim_loop-owned run_scoped call, found {}",
            run_scoped.len()
        ));
    }
    let completion_calls: Vec<_> = inventory
        .calls
        .iter()
        .filter(|call| call.name == "complete_websocket_run")
        .collect();
    if completion_calls.len() != 1 || completion_calls[0].owner != "run_sim_loop" {
        violations.push(format!(
            "run_sim_loop must call complete_websocket_run exactly once, found {}",
            completion_calls.len()
        ));
    }
}

fn simulation_run_loop_owner<'a>(
    inventory: &'a RustInventory,
    violations: &mut Vec<String>,
) -> Option<&'a FunctionRecord> {
    let run_loop = inventory
        .functions
        .iter()
        .find(|function| function.name == "run_sim_loop");
    if run_loop.is_none() {
        violations.push("scheduled simulation has no run_sim_loop owner".into());
    }
    run_loop
}

fn require_simulation_cleanup_order(
    inventory: &RustInventory,
    run_loop: &FunctionRecord,
    violations: &mut Vec<String>,
) {
    for (earlier, later) in [
        ("open_trace_logger", "run_scoped"),
        ("install", "run_scoped"),
        ("run_scoped", "into_parts"),
        ("into_parts", "complete_websocket_run"),
        ("shutdown", "complete_websocket_run"),
        ("stop", "complete_websocket_run"),
    ] {
        let first = run_loop
            .top_level_call_order
            .iter()
            .position(|call| call == earlier);
        let second = run_loop
            .top_level_call_order
            .iter()
            .position(|call| call == later);
        if !matches!((first, second), (Some(first), Some(second)) if first < second) {
            violations.push(format!(
                "run_sim_loop must order `{earlier}` before `{later}`"
            ));
        }
    }
    if run_loop.tries_loop_result {
        violations.push(
            "run_sim_loop applies `?` to loop_result before WebSocket shutdown completion".into(),
        );
    }
    if run_loop.tries_run_scoped {
        violations.push(
            "run_sim_loop must capture run_scoped startup refusal before explicit owned cleanup"
                .into(),
        );
    }
    let run_calls: BTreeSet<_> = inventory
        .calls
        .iter()
        .filter(|call| call.owner == "run_sim_loop")
        .map(|call| call.name.as_str())
        .collect();
    for required in ["catch_unwind", "resume_scheduled_panic"] {
        if !run_calls.contains(required) {
            violations.push(format!(
                "run_sim_loop must preserve owned cleanup across panic through `{required}`"
            ));
        }
    }
    if !run_calls.contains("sync_channel") {
        violations.push("run_sim_loop must own a fixed-capacity viewer-control queue".into());
    }
}

fn require_simulation_failure_products(inventory: &RustInventory, violations: &mut Vec<String>) {
    let expected_primary = BTreeMap::from([
        ("Scoped".into(), 2),
        ("SetupFailure".into(), 1),
        ("StartupFailure".into(), 1),
    ]);
    if inventory
        .enums
        .get("ScheduledWebSocketPrimary")
        .map(|record| &record.variants)
        != Some(&expected_primary)
    {
        violations.push(
            "ScheduledWebSocketPrimary must retain setup, startup, and scoped outcomes".into(),
        );
    }
    let expected_panic_fields = BTreeMap::from([
        (
            "external_cleanup_failure",
            TypeShape::Option("ExternalInterfaceStopFailure".into()),
        ),
        ("original", TypeShape::Other),
        (
            "signal_shutdown_failure",
            TypeShape::Option("SignalControllerShutdownFailure".into()),
        ),
    ]);
    let panic_fields = inventory
        .structs
        .get("ScheduledSimulationPanic")
        .map(|record| {
            record
                .fields
                .iter()
                .map(|(name, field)| (name.as_str(), field.shape.clone()))
                .collect::<BTreeMap<_, _>>()
        });
    if panic_fields.as_ref() != Some(&expected_panic_fields) {
        violations.push(
            "ScheduledSimulationPanic must retain original, signal, and external cleanup causes"
                .into(),
        );
    }
}

fn require_simulation_restart_ownership(inventory: &RustInventory, violations: &mut Vec<String>) {
    let reset = inventory
        .functions
        .iter()
        .find(|function| function.name == "handle_reset");
    if reset.is_none_or(|function| !function.tries_start) {
        violations.push("handle_reset must propagate external-interface restart failure".into());
    }
    let owner_has_drop = inventory
        .methods
        .iter()
        .any(|method| method.implementation == "ExternalInterfaceOwner" && method.name == "drop");
    if !inventory.structs.contains_key("ExternalInterfaceOwner") || !owner_has_drop {
        violations.push(
            "scheduled simulation external child must remain owned by ExternalInterfaceOwner with Drop cleanup"
                .into(),
        );
    }
}

fn inventory(sources: &[RustSource]) -> RustInventory {
    let mut inventory = RustInventory::default();
    for source in sources {
        let syntax = syn::parse_file(&source.text)
            .unwrap_or_else(|error| panic!("parse {}: {error}", source.path.display()));
        if attributes_require_test(&syntax.attrs) {
            continue;
        }
        let mut visitor = InventoryVisitor {
            path: &source.path,
            inventory: &mut inventory,
            current_owner: Vec::new(),
            current_impl: None,
        };
        visitor.visit_file(&syntax);
    }
    inventory
}

struct InventoryVisitor<'a> {
    path: &'a Path,
    inventory: &'a mut RustInventory,
    current_owner: Vec<String>,
    current_impl: Option<String>,
}

impl InventoryVisitor<'_> {
    fn owner(&self) -> String {
        self.current_owner
            .last()
            .cloned()
            .unwrap_or_else(|| "<module>".into())
    }

    fn record_function(
        &mut self,
        name: &str,
        inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
        block: &syn::Block,
    ) {
        let top_level_call_order = block.stmts.iter().flat_map(statement_call_names).collect();
        let mut loop_result_try = LoopResultTry(false);
        loop_result_try.visit_block(block);
        let mut required_tries = RequiredCallTries::default();
        required_tries.visit_block(block);
        let mut single_field = SingleCommandFieldGate(false);
        single_field.visit_block(block);
        let mut identifiers = FunctionIdentifiers(BTreeSet::new());
        identifiers.visit_block(block);
        let mut peer_report_termination = PeerReportTermination(false);
        peer_report_termination.visit_block(block);
        self.inventory.functions.push(FunctionRecord {
            path: self.path.to_path_buf(),
            name: name.to_string(),
            implementation: self.current_impl.clone(),
            argument_count: inputs.len(),
            body_tokens: block.to_token_stream().to_string(),
            identifiers: identifiers.0,
            top_level_call_order,
            consumes_running_before_wait: consumes_running_before_wait(block),
            stop_lifecycle_corridor_is_closed: stop_lifecycle_corridor_is_closed(inputs, block),
            spawn_ownership_corridor_is_closed: spawn_ownership_corridor_is_closed(block),
            requires_single_command_field: single_field.0,
            terminates_on_peer_report_failure: peer_report_termination.0,
            tries_loop_result: loop_result_try.0,
            tries_run_scoped: required_tries.run_scoped,
            tries_start: required_tries.start,
        });
    }
}

impl<'ast> Visit<'ast> for InventoryVisitor<'_> {
    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        if !attributes_require_test(&item.attrs) {
            visit::visit_item_mod(self, item);
        }
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let previous = self.current_impl.replace(type_name(&item.self_ty));
        visit::visit_item_impl(self, item);
        self.current_impl = previous;
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        let name = item.sig.ident.to_string();
        self.record_function(&name, &item.sig.inputs, &item.block);
        self.current_owner.push(name);
        visit::visit_item_fn(self, item);
        self.current_owner.pop();
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        let name = item.sig.ident.to_string();
        self.inventory.methods.push(MethodRecord {
            path: self.path.to_path_buf(),
            implementation: self.current_impl.clone().unwrap_or_default(),
            name: name.clone(),
            public: matches!(item.vis, syn::Visibility::Public(_)),
            argument_count: item.sig.inputs.len(),
            result_error: result_error_name(&item.sig.output),
        });
        self.record_function(&name, &item.sig.inputs, &item.block);
        self.current_owner.push(name);
        visit::visit_impl_item_fn(self, item);
        self.current_owner.pop();
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        let fields = item
            .fields
            .iter()
            .filter_map(|field| {
                Some((
                    field.ident.as_ref()?.to_string(),
                    FieldRecord {
                        public: matches!(field.vis, syn::Visibility::Public(_)),
                        shape: type_shape(&field.ty),
                    },
                ))
            })
            .collect();
        self.inventory
            .structs
            .insert(item.ident.to_string(), StructRecord { fields });
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        let variants = item
            .variants
            .iter()
            .map(|variant| (variant.ident.to_string(), variant.fields.len()))
            .collect();
        self.inventory
            .enums
            .insert(item.ident.to_string(), EnumRecord { variants });
        visit::visit_item_enum(self, item);
    }

    fn visit_type(&mut self, ty: &'ast syn::Type) {
        if is_vec_of(ty, "WebSocketServerError") {
            self.inventory.forbidden_error_vectors.push(format!(
                "{}: Vec<WebSocketServerError> is an open terminal-failure list",
                self.path.display()
            ));
        }
        visit::visit_type(self, ty);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == "spawn" {
            self.inventory
                .spawn_authorities
                .push(self.path.to_path_buf());
        }
        if call.method == "ok" && expression_has_must_observe_call(&call.receiver) {
            self.inventory.ignored_results.push(format!(
                "{}: `{}` discards a required result through .ok()",
                self.path.display(),
                self.owner()
            ));
        }
        self.inventory.calls.push(CallRecord {
            path: self.path.to_path_buf(),
            implementation: self.current_impl.clone(),
            owner: self.owner(),
            name: call.method.to_string(),
            method: true,
            argument_count: call.args.len(),
        });
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        let name = call_path_name(&call.func);
        if name.as_deref() == Some("drop") && call.args.iter().any(expression_has_must_observe_call)
        {
            self.inventory.ignored_results.push(format!(
                "{}: `{}` discards a required result through drop(...)",
                self.path.display(),
                self.owner()
            ));
        }
        if let Some(name) = name {
            self.inventory.calls.push(CallRecord {
                path: self.path.to_path_buf(),
                implementation: self.current_impl.clone(),
                owner: self.owner(),
                name,
                method: false,
                argument_count: call.args.len(),
            });
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_path(&mut self, expression: &'ast syn::ExprPath) {
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "spawn")
        {
            self.inventory
                .spawn_authorities
                .push(self.path.to_path_buf());
        }
        visit::visit_expr_path(self, expression);
    }

    fn visit_macro(&mut self, expression: &'ast syn::Macro) {
        let spawn_count = token_identifier_count(&expression.tokens.to_string(), "spawn");
        for _ in 0..spawn_count {
            self.inventory
                .spawn_authorities
                .push(self.path.to_path_buf());
        }
        visit::visit_macro(self, expression);
    }

    fn visit_local(&mut self, local: &'ast syn::Local) {
        if local
            .init
            .as_ref()
            .is_some_and(|init| expression_has_must_observe_call(&init.expr))
            && ignored_pattern(&local.pat)
        {
            self.inventory.ignored_results.push(format!(
                "{}: `{}` stores a required result in an ignored result binding",
                self.path.display(),
                self.owner()
            ));
        }
        visit::visit_local(self, local);
    }
}

struct LoopResultTry(bool);

impl<'ast> Visit<'ast> for LoopResultTry {
    fn visit_expr_try(&mut self, expression: &'ast syn::ExprTry) {
        if expression_identifier(&expression.expr).as_deref() == Some("loop_result") {
            self.0 = true;
        }
        visit::visit_expr_try(self, expression);
    }
}

#[derive(Default)]
struct RequiredCallTries {
    run_scoped: bool,
    start: bool,
}

struct RequiredTryCalls {
    run_scoped: bool,
    start: bool,
}

impl<'ast> Visit<'ast> for RequiredTryCalls {
    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        match call.method.to_string().as_str() {
            "run_scoped" => self.run_scoped = true,
            "start" => self.start = true,
            _ => {}
        }
        visit::visit_expr_method_call(self, call);
    }
}

impl<'ast> Visit<'ast> for RequiredCallTries {
    fn visit_expr_try(&mut self, expression: &'ast syn::ExprTry) {
        let mut calls = RequiredTryCalls {
            run_scoped: false,
            start: false,
        };
        calls.visit_expr(&expression.expr);
        self.run_scoped |= calls.run_scoped;
        self.start |= calls.start;
        visit::visit_expr_try(self, expression);
    }
}

struct SingleCommandFieldGate(bool);

impl<'ast> Visit<'ast> for SingleCommandFieldGate {
    fn visit_expr_binary(&mut self, expression: &'ast syn::ExprBinary) {
        if matches!(expression.op, syn::BinOp::Ne(_) | syn::BinOp::Eq(_))
            && ((is_object_len(&expression.left) && is_integer_one(&expression.right))
                || (is_object_len(&expression.right) && is_integer_one(&expression.left)))
        {
            self.0 = true;
        }
        visit::visit_expr_binary(self, expression);
    }
}

struct FunctionIdentifiers(BTreeSet<String>);

impl<'ast> Visit<'ast> for FunctionIdentifiers {
    fn visit_expr_path(&mut self, expression: &'ast syn::ExprPath) {
        if let Some(identifier) = expression.path.segments.last() {
            self.0.insert(identifier.ident.to_string());
        }
        visit::visit_expr_path(self, expression);
    }
}

struct PeerReportTermination(bool);

struct PeerReportCall(bool);

impl<'ast> Visit<'ast> for PeerReportCall {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if matches!(
            call_path_name(&call.func).as_deref(),
            Some("report_peer_failure" | "handle_accepted_socket" | "handle_accept_result")
        ) {
            self.0 = true;
        }
        visit::visit_expr_call(self, call);
    }
}

struct ReturnExpression(bool);

impl<'ast> Visit<'ast> for ReturnExpression {
    fn visit_expr_return(&mut self, expression: &'ast syn::ExprReturn) {
        self.0 = true;
        visit::visit_expr_return(self, expression);
    }
}

impl<'ast> Visit<'ast> for PeerReportTermination {
    fn visit_expr_if(&mut self, expression: &'ast syn::ExprIf) {
        let mut report = PeerReportCall(false);
        report.visit_expr(&expression.cond);
        let mut returns = ReturnExpression(false);
        returns.visit_block(&expression.then_branch);
        self.0 |= report.0 && returns.0;
        visit::visit_expr_if(self, expression);
    }
}

fn is_object_len(expression: &syn::Expr) -> bool {
    let syn::Expr::MethodCall(call) = expression else {
        return false;
    };
    call.method == "len" && expression_identifier(&call.receiver).as_deref() == Some("object")
}

fn is_integer_one(expression: &syn::Expr) -> bool {
    matches!(expression, syn::Expr::Lit(literal) if matches!(&literal.lit, syn::Lit::Int(value) if value.base10_digits() == "1"))
}

fn statement_call_names(statement: &syn::Stmt) -> Vec<String> {
    struct Names(Vec<String>);
    impl<'ast> Visit<'ast> for Names {
        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let Some(name) = call_path_name(&call.func) {
                self.0.push(name);
            }
            visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.0.push(call.method.to_string());
            visit::visit_expr_method_call(self, call);
        }
    }
    let mut names = Names(Vec::new());
    names.visit_stmt(statement);
    names.0
}

fn expression_has_must_observe_call(expression: &syn::Expr) -> bool {
    struct Required(bool);
    impl<'ast> Visit<'ast> for Required {
        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            if MUST_OBSERVE_METHODS.contains(&call.method.to_string().as_str()) {
                self.0 = true;
            }
            visit::visit_expr_method_call(self, call);
        }
    }
    let mut required = Required(false);
    required.visit_expr(expression);
    required.0
}

fn ignored_pattern(pattern: &syn::Pat) -> bool {
    match pattern {
        syn::Pat::Wild(_) => true,
        syn::Pat::Ident(ident) => ident.ident.to_string().starts_with('_'),
        _ => false,
    }
}

fn call_path_name(expression: &syn::Expr) -> Option<String> {
    let syn::Expr::Path(path) = expression else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn expression_identifier(expression: &syn::Expr) -> Option<String> {
    match expression {
        syn::Expr::Path(path) => path
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string()),
        syn::Expr::Reference(reference) => expression_identifier(&reference.expr),
        _ => None,
    }
}

fn type_name(ty: &syn::Type) -> String {
    let syn::Type::Path(path) = ty else {
        return String::new();
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
        .unwrap_or_default()
}

fn result_error_name(output: &syn::ReturnType) -> Option<String> {
    let syn::ReturnType::Type(_, ty) = output else {
        return None;
    };
    let syn::Type::Path(path) = ty.as_ref() else {
        return None;
    };
    let result = path.path.segments.last()?;
    if result.ident != "Result" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &result.arguments else {
        return None;
    };
    let syn::GenericArgument::Type(error) = arguments.args.iter().nth(1)? else {
        return None;
    };
    Some(type_name(error))
}

fn type_shape(ty: &syn::Type) -> TypeShape {
    let syn::Type::Path(path) = ty else {
        return TypeShape::Other;
    };
    let Some(segment) = path.path.segments.last() else {
        return TypeShape::Other;
    };
    if segment.ident == "Option" {
        return optional_type_shape(&segment.arguments);
    }
    if segment.ident == "Box" {
        return boxed_slice_name(&segment.arguments)
            .map(TypeShape::BoxedSlice)
            .unwrap_or(TypeShape::Other);
    }
    TypeShape::Plain(segment.ident.to_string())
}

fn optional_type_shape(arguments: &syn::PathArguments) -> TypeShape {
    let syn::PathArguments::AngleBracketed(arguments) = arguments else {
        return TypeShape::Other;
    };
    let Some(syn::GenericArgument::Type(ty)) = arguments.args.first() else {
        return TypeShape::Other;
    };
    let syn::Type::Path(path) = ty else {
        return TypeShape::Other;
    };
    let Some(segment) = path.path.segments.last() else {
        return TypeShape::Other;
    };
    if segment.ident == "Box" {
        return generic_type_name(&segment.arguments)
            .map(TypeShape::OptionalBox)
            .unwrap_or(TypeShape::Other);
    }
    TypeShape::Option(segment.ident.to_string())
}

fn generic_type_name(arguments: &syn::PathArguments) -> Option<String> {
    let syn::PathArguments::AngleBracketed(arguments) = arguments else {
        return None;
    };
    let syn::GenericArgument::Type(ty) = arguments.args.first()? else {
        return None;
    };
    Some(type_name(ty))
}

fn boxed_slice_name(arguments: &syn::PathArguments) -> Option<String> {
    let syn::PathArguments::AngleBracketed(arguments) = arguments else {
        return None;
    };
    let syn::GenericArgument::Type(syn::Type::Slice(slice)) = arguments.args.first()? else {
        return None;
    };
    Some(type_name(&slice.elem))
}

fn is_vec_of(ty: &syn::Type, element: &str) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    path.path.segments.last().is_some_and(|segment| {
        segment.ident == "Vec" && generic_type_name(&segment.arguments).as_deref() == Some(element)
    })
}

fn viewer_violations(viewer: &str) -> Vec<String> {
    let compact: String = viewer
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect();
    let mut violations = Vec::new();
    if compact.contains("setTimeout(connectWs") || compact.contains("reconnectTimer") {
        violations.push("browser contains an automatic reconnect policy".into());
    }
    if compact.contains("key:event.key") {
        violations
            .push("browser key command duplicates unchecked key text beside physical code".into());
    }
    if contains_empty_catch(&compact) {
        violations.push("browser contains an empty catch that silently drops invalid JSON".into());
    }
    if compact.contains("JSON.parse(evt.data)") {
        for required in [
            "console.error(",
            "pipeline.ws=\"invalid-payload\"",
            "ws.close(",
        ] {
            if !compact.contains(required) {
                violations.push(format!(
                    "browser JSON rejection path is missing visible fail-closed action `{required}`"
                ));
            }
        }
    }
    violations
}

fn contains_empty_catch(compact: &str) -> bool {
    let mut remainder = compact;
    while let Some(offset) = remainder.find("catch(") {
        remainder = &remainder[offset + "catch(".len()..];
        let Some(body_offset) = remainder.find('{') else {
            return false;
        };
        let body = &remainder[body_offset + 1..];
        if body.starts_with('}') {
            return true;
        }
        remainder = body;
    }
    false
}

fn valid_transport_fixture() -> Vec<RustSource> {
    vec![RustSource::fixture(
        "transport/lib.rs",
        r#"
            struct BroadcastServer;
            struct RunningBroadcastServer;
            struct PeerWorkerTerminalFailureRecord;
            struct WebSocketServerError;
            struct BroadcastServerTerminalFailures {
                accept_loop: Option<WebSocketServerError>,
                latest_state_close: Option<WebSocketServerError>,
                state_fanout: Option<WebSocketServerError>,
                event_observer: Option<WebSocketServerError>,
                startup_observer: Option<WebSocketServerError>,
                server_thread: Option<WebSocketServerError>,
                peer_workers: Box<[PeerWorkerTerminalFailureRecord]>,
            }
            enum BroadcastServerEvent { PeerFailure(u8), TerminalFailure }
            enum BroadcastServerStartup { Ready, Failed }
            impl BroadcastServer {
                pub fn run_scoped(&self) {
                    catch_unwind(|| operation());
                    running.finish();
                    panic_any(());
                }
                fn start(&self) {}
                fn bind(socket: std::net::TcpStream) {
                    socket.set_nonblocking(true).unwrap();
                }
                fn accept_loop(listener: std::net::TcpListener) {
                    listener.accept().unwrap();
                }
            }
            impl RunningBroadcastServer { fn finish(&self) {} }
            fn prepare_accepted_socket(socket: std::net::TcpStream) {
                socket.set_read_timeout(None).unwrap();
                socket.set_write_timeout(None).unwrap();
            }
            fn configure_peer_after_handshake(socket: std::net::TcpStream) {
                socket.set_read_timeout(None).unwrap();
                socket.set_nonblocking(true).unwrap();
            }
        "#,
    )]
}

fn valid_sim_fixture() -> Vec<RustSource> {
    vec![RustSource::fixture(
        "sim/executor.rs",
        r#"
            struct PeerFailureRecord;
            enum PeerFailureObservationFailureKind { CounterOverflow }
            struct PeerFailureObservationFailure {
                kind: PeerFailureObservationFailureKind,
                failed: PeerFailureRecord,
                unobserved: Box<[PeerFailureRecord]>,
            }
            fn run_sim_loop(server: BroadcastServer) -> anyhow::Result<()> {
                let trace = open_trace_logger()?;
                let scoped_run = server.run_scoped(|running| run_frames(running))?;
                let (loop_result, shutdown) = scoped_run.into_parts();
                complete_websocket_run(loop_result, shutdown)
            }
        "#,
    )]
}
