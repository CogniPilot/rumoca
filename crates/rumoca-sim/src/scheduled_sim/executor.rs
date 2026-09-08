//! Interactive simulation loop driven entirely by the TOML config.
//!
//! Per-frame orchestration (transports live in sibling crates):
//!   1. poll input engine (config-driven gamepad/keyboard)
//!   2. drain incoming UDP, apply unpacked values to session / locals
//!   3. advance physics
//!   4. build outgoing `SignalFrame` via signal mapper
//!   5. pack + send UDP
//!   6. build viewer JSON via signal mapper
//!   7. push to WebSocket
//!   8. optional realtime pacing

mod signal_controller;
mod trace_logger;

use signal_controller::{SignalController, SignalControllerShutdownFailure};
#[cfg(test)]
use signal_controller::{SignalStage, signal_action_after_cleanup};
use std::collections::BTreeSet;
use std::io::Write;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use trace_logger::{TraceLogger, open_trace_logger};

use crate::{SimPacingMode, SimulationSessionApi};
use anyhow::{Context, Result};
use rumoca_codec::{PackCodec, UnpackCodec};
use rumoca_input::{
    InputEngine, KeyCode, KeyModifiers, KeyboardEvent, RuntimeContext, SignalMapper,
};
use rumoca_transport_udp::{UdpConfig, UdpTransport};
use rumoca_transport_websocket::{
    BroadcastServer, BroadcastServerEvent, BroadcastServerShutdown,
    BroadcastServerTerminalFailures, PeerFailureRecord, RunningBroadcastServer,
    ViewerControlCommand, ViewerKeyCode, ViewerKeyCommand,
};
use rumoca_transport_zenoh::ZenohTransport;
use serde_json::{Map as JsonMap, Value as JsonValue};

use crate::scheduled_sim::devices::Devices;

use crate::scenario_config::{LockstepConfig, ResetConfig, SimulationConfig};

const VIEWER_CONTROL_QUEUE_CAPACITY: usize = 64;

fn wall_ms_since_unix_epoch() -> Result<f64> {
    Ok(SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .context("read wall clock time since Unix epoch")?
        .as_millis() as f64)
}

// ── External-interface subprocess ──────────────────────────────────────────

struct ExternalInterfaceProcess {
    state: ExternalInterfaceProcessState,
    command: String,
}

enum ExternalInterfaceProcessState {
    Idle,
    Owned {
        child: Child,
        target: ExternalInterfaceStopTarget,
        stop_phase: ExternalInterfaceStopPhase,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExternalInterfaceStopPhase {
    Running,
    KillIssued,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExternalInterfaceStopTarget {
    #[cfg(not(unix))]
    DirectChild { pid: u32 },
    #[cfg(unix)]
    ProcessGroup { leader_pid: u32, pgid: u32 },
}

impl std::fmt::Display for ExternalInterfaceStopTarget {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            #[cfg(not(unix))]
            Self::DirectChild { pid } => write!(formatter, "pid {pid}"),
            #[cfg(unix)]
            Self::ProcessGroup { leader_pid, pgid } => {
                write!(formatter, "leader pid {leader_pid}, process group {pgid}")
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum ExternalInterfaceStopFailure {
    #[cfg(not(unix))]
    #[error("failed to inspect external-interface {target} before shutdown: {detail}")]
    Inspect {
        target: ExternalInterfaceStopTarget,
        detail: String,
    },
    #[error("failed to kill external-interface {target}: {detail}")]
    Kill {
        target: ExternalInterfaceStopTarget,
        detail: String,
    },
    #[error("failed to wait for external-interface {target}: {detail}")]
    Wait {
        target: ExternalInterfaceStopTarget,
        detail: String,
    },
    #[error("external-interface {target} did not exit within 500ms")]
    Timeout { target: ExternalInterfaceStopTarget },
}

#[derive(Debug, thiserror::Error)]
enum ExternalInterfaceStartFailure {
    #[error("external-interface startup refused because shutdown was requested")]
    ShutdownRequested,
    #[error(transparent)]
    Stop(#[from] ExternalInterfaceStopFailure),
    #[error("failed to start external interface `{command}`: {source}")]
    Spawn {
        command: String,
        #[source]
        source: std::io::Error,
    },
}

impl ExternalInterfaceProcess {
    fn new(command: &str) -> Self {
        Self {
            state: ExternalInterfaceProcessState::Idle,
            command: command.to_string(),
        }
    }

    fn start(&mut self) -> std::result::Result<(), ExternalInterfaceStartFailure> {
        self.stop()?;
        write_control_diagnostic(format_args!(
            "[external_interface] starting: {}",
            self.command
        ));
        let mut cmd = Command::new(&self.command);
        cmd.stdin(Stdio::null());
        // Enabling the `rumoca_sim::external_interface` (or `::autopilot`) trace
        // target lets the child's stdout/stderr through to this terminal — handy
        // for debugging Cerebri boot issues.
        if tracing::enabled!(target: "rumoca_sim::external_interface", tracing::Level::DEBUG)
            || tracing::enabled!(target: "rumoca_sim::autopilot", tracing::Level::DEBUG)
        {
            cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
        } else {
            cmd.stdout(Stdio::null()).stderr(Stdio::null());
        }
        // Put the child in its own process group so a terminal Ctrl-C
        // (which targets the tty's foreground pgrp) can't route into zephyr
        // — only rumoca receives SIGINT.
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            cmd.process_group(0);
        }
        // On Linux, ask the kernel to signal only the direct child if rumoca
        // dies before owned cleanup runs. PR_SET_PDEATHSIG is not inherited by
        // descendants, so crash-path descendant cleanup is not claimed here;
        // handled shutdown paths explicitly terminate and inspect the PGID.
        #[cfg(target_os = "linux")]
        install_pdeathsig(&mut cmd);
        let child = cmd
            .spawn()
            .map_err(|source| ExternalInterfaceStartFailure::Spawn {
                command: self.command.clone(),
                source,
            })?;
        let pid = child.id();
        #[cfg(unix)]
        let target = ExternalInterfaceStopTarget::ProcessGroup {
            leader_pid: pid,
            pgid: pid,
        };
        #[cfg(not(unix))]
        let target = ExternalInterfaceStopTarget::DirectChild { pid };
        self.state = ExternalInterfaceProcessState::Owned {
            child,
            target,
            stop_phase: ExternalInterfaceStopPhase::Running,
        };
        write_control_diagnostic(format_args!("[external_interface] pid {pid}"));
        Ok(())
    }

    fn stop(&mut self) -> std::result::Result<(), ExternalInterfaceStopFailure> {
        self.stop_with(kill_external_interface, wait_for_external_interface_exit)
    }

    fn stop_with<K, W>(
        &mut self,
        kill: K,
        wait: W,
    ) -> std::result::Result<(), ExternalInterfaceStopFailure>
    where
        K: FnOnce(
            &mut Child,
            ExternalInterfaceStopTarget,
        ) -> std::result::Result<(), ExternalInterfaceStopFailure>,
        W: FnOnce(
            &mut Child,
            ExternalInterfaceStopTarget,
        ) -> std::result::Result<(), ExternalInterfaceStopFailure>,
    {
        let ExternalInterfaceProcessState::Owned {
            child,
            target,
            stop_phase,
        } = &mut self.state
        else {
            return Ok(());
        };
        if *stop_phase == ExternalInterfaceStopPhase::Running {
            kill(child, *target)?;
            *stop_phase = ExternalInterfaceStopPhase::KillIssued;
        }
        let result = wait(child, *target);
        if result.is_ok() {
            self.state = ExternalInterfaceProcessState::Idle;
        }
        result
    }
}

#[cfg(not(unix))]
fn kill_external_interface(
    child: &mut Child,
    target: ExternalInterfaceStopTarget,
) -> std::result::Result<(), ExternalInterfaceStopFailure> {
    let leader_exited =
        child
            .try_wait()
            .map_err(|error| ExternalInterfaceStopFailure::Inspect {
                target,
                detail: error.to_string(),
            })?;
    if leader_exited.is_some() {
        return Ok(());
    }
    write_control_diagnostic(format_args!("[external_interface] killing {target}"));
    if let Err(kill_error) = kill_external_interface_target(child, target) {
        return match child.try_wait() {
            Ok(Some(_)) => Ok(()),
            Ok(None) => Err(ExternalInterfaceStopFailure::Kill {
                target,
                detail: kill_error.to_string(),
            }),
            Err(wait_error) => Err(ExternalInterfaceStopFailure::Inspect {
                target,
                detail: format!(
                    "kill failed ({kill_error}); follow-up inspection failed ({wait_error})"
                ),
            }),
        };
    }
    Ok(())
}

#[cfg(not(unix))]
fn wait_for_external_interface_exit(
    child: &mut Child,
    target: ExternalInterfaceStopTarget,
) -> std::result::Result<(), ExternalInterfaceStopFailure> {
    let deadline = Instant::now() + Duration::from_millis(500);
    while Instant::now() < deadline {
        match child.try_wait() {
            Ok(Some(_)) => return Ok(()),
            Ok(None) => thread::sleep(Duration::from_millis(20)),
            Err(error) => {
                return Err(ExternalInterfaceStopFailure::Wait {
                    target,
                    detail: error.to_string(),
                });
            }
        }
    }
    Err(ExternalInterfaceStopFailure::Timeout { target })
}

#[cfg(unix)]
fn kill_external_interface(
    _child: &mut Child,
    target: ExternalInterfaceStopTarget,
) -> std::result::Result<(), ExternalInterfaceStopFailure> {
    write_control_diagnostic(format_args!("[external_interface] killing {target}"));
    kill_external_interface_target(target).map_err(|error| ExternalInterfaceStopFailure::Kill {
        target,
        detail: error.to_string(),
    })
}

#[cfg(unix)]
fn wait_for_external_interface_exit(
    child: &mut Child,
    target: ExternalInterfaceStopTarget,
) -> std::result::Result<(), ExternalInterfaceStopFailure> {
    let deadline = Instant::now() + Duration::from_millis(500);
    let mut leader_exited = false;
    while Instant::now() < deadline {
        if !leader_exited {
            leader_exited = child
                .try_wait()
                .map_err(|error| ExternalInterfaceStopFailure::Wait {
                    target,
                    detail: error.to_string(),
                })?
                .is_some();
        }
        let group_is_live = process_group_has_live_members(target).map_err(|error| {
            ExternalInterfaceStopFailure::Wait {
                target,
                detail: error.to_string(),
            }
        })?;
        if leader_exited && !group_is_live {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(20));
    }
    Err(ExternalInterfaceStopFailure::Timeout { target })
}

#[cfg(not(unix))]
fn kill_external_interface_target(
    child: &mut Child,
    target: ExternalInterfaceStopTarget,
) -> std::io::Result<()> {
    let ExternalInterfaceStopTarget::DirectChild { .. } = target;
    child.kill()
}

#[cfg(unix)]
#[allow(unsafe_code)]
fn kill_external_interface_target(target: ExternalInterfaceStopTarget) -> std::io::Result<()> {
    let ExternalInterfaceStopTarget::ProcessGroup { pgid, .. } = target;
    let pgid = libc::pid_t::try_from(pgid).map_err(|error| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("process-group identifier is outside pid_t: {error}"),
        )
    })?;
    let result = unsafe { libc::kill(-pgid, libc::SIGKILL) };
    if result == 0 {
        return Ok(());
    }
    let error = std::io::Error::last_os_error();
    if error.raw_os_error() == Some(libc::ESRCH) {
        return Ok(());
    }
    Err(error)
}

#[cfg(target_os = "linux")]
fn process_group_has_live_members(target: ExternalInterfaceStopTarget) -> std::io::Result<bool> {
    let ExternalInterfaceStopTarget::ProcessGroup { pgid, .. } = target;
    for entry in std::fs::read_dir("/proc")? {
        let entry = entry?;
        if !entry
            .file_name()
            .as_encoded_bytes()
            .iter()
            .all(u8::is_ascii_digit)
        {
            continue;
        }
        let stat = match std::fs::read_to_string(entry.path().join("stat")) {
            Ok(stat) => stat,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => return Err(error),
        };
        let (state, process_group) = parse_linux_process_stat(&stat)?;
        if process_group == pgid && !matches!(state, 'Z' | 'X') {
            return Ok(true);
        }
    }
    Ok(false)
}

#[cfg(target_os = "linux")]
fn parse_linux_process_stat(stat: &str) -> std::io::Result<(char, u32)> {
    let command_end = stat.rfind(')').ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Linux process stat lacks command terminator",
        )
    })?;
    let mut fields = stat[command_end + 1..].split_whitespace();
    let state = fields
        .next()
        .and_then(|field| field.chars().next())
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Linux process stat lacks state",
            )
        })?;
    let _parent = fields.next().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Linux process stat lacks parent pid",
        )
    })?;
    let process_group = fields
        .next()
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Linux process stat lacks process group",
            )
        })?
        .parse()
        .map_err(|error| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Linux process stat has invalid process group: {error}"),
            )
        })?;
    Ok((state, process_group))
}

#[cfg(all(unix, not(target_os = "linux")))]
#[allow(unsafe_code)]
fn process_group_has_live_members(target: ExternalInterfaceStopTarget) -> std::io::Result<bool> {
    let ExternalInterfaceStopTarget::ProcessGroup { pgid, .. } = target;
    let pgid = libc::pid_t::try_from(pgid).map_err(|error| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("process-group identifier is outside pid_t: {error}"),
        )
    })?;
    let result = unsafe { libc::kill(-pgid, 0) };
    if result == 0 {
        return Ok(true);
    }
    let error = std::io::Error::last_os_error();
    match error.raw_os_error() {
        Some(libc::ESRCH) => Ok(false),
        Some(libc::EPERM) => Ok(true),
        _ => Err(error),
    }
}

impl Drop for ExternalInterfaceProcess {
    fn drop(&mut self) {
        if let Err(failure) = self.stop() {
            write_control_diagnostic(format_args!(
                "[external_interface] shutdown failure during drop: {failure}"
            ));
        }
    }
}

struct ExternalInterfaceLifecycle {
    cancelled: bool,
    process: Option<ExternalInterfaceProcess>,
}

#[derive(Clone)]
struct ExternalInterfaceHandle {
    lifecycle: Arc<Mutex<ExternalInterfaceLifecycle>>,
}

impl ExternalInterfaceHandle {
    fn configured(command: Option<&str>) -> Self {
        Self {
            lifecycle: Arc::new(Mutex::new(ExternalInterfaceLifecycle {
                cancelled: false,
                process: command.map(ExternalInterfaceProcess::new),
            })),
        }
    }

    fn start(&self) -> std::result::Result<(), ExternalInterfaceStartFailure> {
        self.start_with(ExternalInterfaceProcess::start)
    }

    fn start_with<F>(&self, start: F) -> std::result::Result<(), ExternalInterfaceStartFailure>
    where
        F: FnOnce(
            &mut ExternalInterfaceProcess,
        ) -> std::result::Result<(), ExternalInterfaceStartFailure>,
    {
        let mut lifecycle = self
            .lifecycle
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if lifecycle.cancelled {
            return Err(ExternalInterfaceStartFailure::ShutdownRequested);
        }
        match lifecycle.process.as_mut() {
            Some(process) => start(process),
            None => Ok(()),
        }
    }

    fn cancel(&self) -> std::result::Result<(), ExternalInterfaceStopFailure> {
        let mut lifecycle = self
            .lifecycle
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        lifecycle.cancelled = true;
        match lifecycle.process.as_mut() {
            Some(process) => process.stop(),
            None => Ok(()),
        }
    }
}

struct ExternalInterfaceOwner {
    handle: ExternalInterfaceHandle,
}

impl ExternalInterfaceOwner {
    fn new(command: Option<&str>) -> Self {
        Self {
            handle: ExternalInterfaceHandle::configured(command),
        }
    }

    fn handle(&self) -> &ExternalInterfaceHandle {
        &self.handle
    }

    fn start(&self) -> std::result::Result<(), ExternalInterfaceStartFailure> {
        self.handle.start()
    }

    fn stop(&self) -> std::result::Result<(), ExternalInterfaceStopFailure> {
        self.handle.cancel()
    }
}

impl Drop for ExternalInterfaceOwner {
    fn drop(&mut self) {
        if let Err(failure) = self.stop() {
            write_control_diagnostic(format_args!(
                "[external_interface] owner cleanup failure: {failure}"
            ));
        }
    }
}

fn write_control_diagnostic(arguments: std::fmt::Arguments<'_>) {
    if let Err(error) = writeln!(std::io::stderr().lock(), "{arguments}") {
        tracing::debug!(%error, "control diagnostic stream unavailable");
    }
}

/// Set `PR_SET_PDEATHSIG = SIGKILL` on the direct child via `pre_exec`.
/// Linux clears this setting in forked descendants, so this is a direct-child
/// crash-path backstop, not a process-group or descendant cleanup proof.
#[cfg(target_os = "linux")]
#[allow(unsafe_code)]
fn install_pdeathsig(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    let expected_parent = unsafe { libc::getpid() };
    unsafe {
        cmd.pre_exec(move || {
            if libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if libc::getppid() != expected_parent {
                libc::raise(libc::SIGKILL);
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Interrupted,
                    "simulation parent exited before PR_SET_PDEATHSIG installation",
                ));
            }
            Ok(())
        });
    }
}

// ── UDP config resolution ───────────────────────────────────────────────────

fn resolve_udp(cfg: &SimulationConfig) -> Option<&UdpConfig> {
    cfg.transport.as_ref().and_then(|t| t.udp.as_ref())
}

fn resolve_zenoh(cfg: &SimulationConfig) -> Option<&rumoca_transport_zenoh::ZenohConfig> {
    cfg.transport.as_ref().and_then(|t| t.zenoh.as_ref())
}

fn insert_u64(obj: &mut JsonMap<String, JsonValue>, key: &str, value: u64) {
    obj.insert(key.to_string(), JsonValue::from(value));
}

fn snake_case_type_leaf(root_type: &str) -> String {
    // `root_type` is a FlatBuffer fully-qualified type (e.g. `cerebri2.topic.Foo`);
    // take the segment after the last dot without string tokenization. `.` is
    // ASCII so byte-index slicing is safe.
    let leaf = match root_type.rfind('.') {
        Some(dot) => &root_type[dot + 1..],
        None => root_type,
    };
    let mut out = String::new();
    let mut prev_lower_or_digit = false;
    for ch in leaf.chars() {
        if ch.is_ascii_uppercase() {
            if prev_lower_or_digit {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
            prev_lower_or_digit = false;
        } else {
            prev_lower_or_digit = ch.is_ascii_lowercase() || ch.is_ascii_digit();
            out.push(ch);
        }
    }
    out
}

fn map_message_name(
    messages: &std::collections::HashMap<String, String>,
    alias: &str,
    root_type: &str,
) -> Option<String> {
    let leaf = snake_case_type_leaf(root_type);
    [alias, root_type, leaf.as_str()]
        .into_iter()
        .find(|name| messages.contains_key(*name))
        .map(str::to_owned)
}

mod viewer_input;
use viewer_input::{ViewerInputDrain, drain_viewer_input};

// ── Main loop ──────────────────────────────────────────────────────────────

/// Bundle of per-frame FB transport state. Present only when `[schema]` +
/// `[receive]` + `[send]` are configured (external coupling). Absent in
/// standalone mode (e.g. rover demo).
struct FbTransport {
    udp: Option<UdpTransport>,
    zenoh: Option<ZenohTransport>,
    pack: Box<dyn PackCodec>,
    unpack: Box<dyn UnpackCodec>,
    recv_expected: usize,
    send_publish: Option<String>,
    receive_publish: Option<String>,
    receive_subscribe: Option<String>,
}

/// Immutable per-frame context: FB transport (if any), mapper, and channels.
struct FrameCtx<'a> {
    cfg: &'a SimulationConfig,
    fb: Option<&'a FbTransport>,
    mapper: &'a SignalMapper,
    payload_observation_lookup_names: &'a [String],
    viewer_input_rx: &'a mpsc::Receiver<ViewerControlCommand>,
    websocket: &'a RunningBroadcastServer,
    realtime: &'a Arc<AtomicBool>,
    quit: &'a Arc<AtomicBool>,
    external_interface: &'a ExternalInterfaceHandle,
    debug: bool,
    dt: f64,
    mode: SimPacingMode,
    steps_per_packet: usize,
    lockstep_schedule: Option<LockstepSchedule>,
}

/// Mutable per-frame state that carries across iterations.
struct FrameState {
    recv_buf: [u8; 512],
    pkt_count: u64,
    send_count: u64,
    frame_num: u64,
    websocket_peer_failures: u64,
    last_poll: Instant,
    trace: Option<TraceLogger>,
    lockstep_schedule_initialized: bool,
    next_lockstep_send_time: f64,
    next_lockstep_control_time: f64,
}

impl FrameState {
    fn new(trace: Option<TraceLogger>) -> Self {
        Self {
            recv_buf: [0u8; 512],
            pkt_count: 0,
            send_count: 0,
            frame_num: 0,
            websocket_peer_failures: 0,
            last_poll: Instant::now(),
            trace,
            lockstep_schedule_initialized: false,
            next_lockstep_send_time: 0.0,
            next_lockstep_control_time: 0.0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct LockstepSchedule {
    send_dt: f64,
    receive_dt: f64,
    max_advance_dt: f64,
}

impl LockstepSchedule {
    fn from_config(lockstep: &LockstepConfig, sim_dt: f64) -> Self {
        Self {
            send_dt: 1.0 / lockstep.send_rate_hz,
            receive_dt: 1.0 / lockstep.receive_rate_hz,
            max_advance_dt: lockstep.max_advance_dt.unwrap_or(sim_dt),
        }
    }
}

pub struct SimLoopArgs<'a> {
    pub cfg: &'a SimulationConfig,
    pub http_port: u16,
    pub ws_port: u16,
    pub debug: bool,
}

struct SessionFrameSnapshot {
    values: indexmap::IndexMap<String, f64>,
}

impl SessionFrameSnapshot {
    fn new(
        session: &impl SimulationSessionApi,
        names: &[String],
        purpose: &'static str,
    ) -> Result<Self> {
        let values = session.values_for(names)?;
        let missing: Vec<&str> = names
            .iter()
            .filter(|name| !values.contains_key(name.as_str()))
            .map(String::as_str)
            .collect();
        let unexpected: Vec<&str> = values
            .keys()
            .filter(|name| !names.iter().any(|requested| requested == *name))
            .map(String::as_str)
            .collect();
        if values.len() != names.len() || !missing.is_empty() || !unexpected.is_empty() {
            anyhow::bail!(
                "{purpose} session batch read did not exactly cover the {} requested names: \
                 received {}, missing {missing:?}, unexpected {unexpected:?}",
                names.len(),
                values.len()
            );
        }
        Ok(Self { values })
    }

    fn get(&self, name: &str) -> Result<Option<f64>> {
        Ok(self.values.get(name).copied())
    }
}

fn payload_observation_lookup_names<'a>(
    mapper: &SignalMapper,
    trace_fields: impl IntoIterator<Item = &'a str>,
) -> Vec<String> {
    let mut names: BTreeSet<String> = mapper
        .payload_observation_lookup_names()
        .iter()
        .cloned()
        .collect();
    names.extend(trace_fields.into_iter().filter_map(|field| {
        field
            .strip_prefix("model:")
            .filter(|name| *name != "time")
            .map(str::to_owned)
    }));
    names.into_iter().collect()
}

enum FrameControl {
    Continue,
    Break,
    WebSocketTerminated,
}

enum ScheduledLoopCompletion {
    Complete,
    WebSocketTerminated,
}

enum ScheduledWebSocketPrimary {
    SetupFailure(anyhow::Error),
    StartupFailure(BroadcastServerTerminalFailures),
    Scoped {
        loop_result: Result<ScheduledLoopCompletion>,
        shutdown: BroadcastServerShutdown,
    },
}

#[derive(Debug)]
struct ScheduledWebSocketRunFailure {
    setup_failure: Option<anyhow::Error>,
    loop_failure: Option<anyhow::Error>,
    peer_observation_failure: Option<PeerFailureObservationFailure>,
    signal_shutdown_failure: Option<SignalControllerShutdownFailure>,
    external_cleanup_failure: Option<ExternalInterfaceStopFailure>,
    termination_without_failure: bool,
    terminal_failures: BroadcastServerTerminalFailures,
}

impl std::fmt::Display for ScheduledWebSocketRunFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut separator = "";
        if let Some(failure) = &self.setup_failure {
            write!(formatter, "scheduled setup: {failure}")?;
            separator = "; ";
        }
        if let Some(failure) = &self.loop_failure {
            write!(formatter, "scheduled loop: {failure}")?;
            separator = "; ";
        }
        if let Some(failure) = &self.peer_observation_failure {
            write!(formatter, "{separator}peer-failure observer: {failure}")?;
            separator = "; ";
        }
        if let Some(failure) = &self.signal_shutdown_failure {
            write!(
                formatter,
                "{separator}signal-controller shutdown: {failure}"
            )?;
            separator = "; ";
        }
        if let Some(failure) = &self.external_cleanup_failure {
            write!(
                formatter,
                "{separator}external-interface cleanup: {failure}"
            )?;
            separator = "; ";
        }
        if self.termination_without_failure {
            write!(
                formatter,
                "{separator}WebSocket startup/termination had no typed terminal cause"
            )?;
            separator = "; ";
        }
        if !self.terminal_failures.is_empty() {
            write!(
                formatter,
                "{separator}WebSocket shutdown: {}",
                self.terminal_failures
            )?;
        }
        Ok(())
    }
}

impl std::error::Error for ScheduledWebSocketRunFailure {}

struct ScheduledSimulationPanic {
    original: Box<dyn std::any::Any + Send>,
    signal_shutdown_failure: Option<SignalControllerShutdownFailure>,
    external_cleanup_failure: Option<ExternalInterfaceStopFailure>,
}

impl std::fmt::Debug for ScheduledSimulationPanic {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ScheduledSimulationPanic")
            .field("original_type_id", &(*self.original).type_id())
            .field("signal_shutdown_failure", &self.signal_shutdown_failure)
            .field("external_cleanup_failure", &self.external_cleanup_failure)
            .finish_non_exhaustive()
    }
}

fn resume_scheduled_panic(
    original: Box<dyn std::any::Any + Send>,
    signal_shutdown_failure: Option<SignalControllerShutdownFailure>,
    external_cleanup_failure: Option<ExternalInterfaceStopFailure>,
) -> ! {
    std::panic::panic_any(ScheduledSimulationPanic {
        original,
        signal_shutdown_failure,
        external_cleanup_failure,
    })
}

#[derive(Debug)]
enum PeerFailureObservationFailureKind {
    CounterOverflow,
}

#[derive(Debug)]
struct PeerFailureObservationFailure {
    kind: PeerFailureObservationFailureKind,
    failed: PeerFailureRecord,
    unobserved: Box<[PeerFailureRecord]>,
}

impl std::fmt::Display for PeerFailureObservationFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "{:?} while recording peer={} kind={:?}; {} later peer records retained",
            self.kind,
            self.failed.peer(),
            self.failed.classification(),
            self.unobserved.len()
        )
    }
}

impl std::error::Error for PeerFailureObservationFailure {}

/// Log the selected schedule. In standalone mode (no `[schema]`/`[receive]`/
/// `[send]` in config) the UDP socket and codecs are not created and no
/// external-interface coupling happens.
fn log_pacing_status(
    mode: SimPacingMode,
    lockstep_schedule: Option<LockstepSchedule>,
    steps_per_packet: usize,
    dt: f64,
) {
    status_line(&format!(
        "  Pacing: {}",
        match mode {
            SimPacingMode::AsFastAsPossible => "as_fast_as_possible",
            SimPacingMode::Realtime => "realtime",
            SimPacingMode::Lockstep if lockstep_schedule.is_some() => {
                "lockstep (multirate scheduled)"
            }
            SimPacingMode::Lockstep if steps_per_packet > 1 => {
                "lockstep (input-packet-paced, multistep)"
            }
            SimPacingMode::Lockstep => "lockstep (input-packet-paced)",
        }
    ));
    if matches!(mode, SimPacingMode::Lockstep)
        && let Some(schedule) = lockstep_schedule
    {
        status_line(&format!(
            "  Lockstep send: {:.3} Hz, receive barrier: {:.3} Hz, max advance dt: {} s",
            1.0 / schedule.send_dt,
            1.0 / schedule.receive_dt,
            schedule.max_advance_dt
        ));
    } else if matches!(mode, SimPacingMode::Lockstep) && steps_per_packet > 1 {
        status_line(&format!(
            "  Lockstep advances/packet: {steps_per_packet} ({} s simulated per packet)",
            dt * steps_per_packet as f64
        ));
    }
}

struct ScheduledSimulationScope<'a> {
    cfg: &'a SimulationConfig,
    http_port: u16,
    debug: bool,
    websocket_server: BroadcastServer,
    external_interface: &'a ExternalInterfaceOwner,
    quit: &'a Arc<AtomicBool>,
    realtime: &'a Arc<AtomicBool>,
    mode: SimPacingMode,
    steps_per_packet: usize,
    lockstep_schedule: Option<LockstepSchedule>,
}

impl ScheduledSimulationScope<'_> {
    fn run(
        self,
        session: &mut impl SimulationSessionApi,
        state: &mut FrameState,
    ) -> Result<ScheduledWebSocketPrimary> {
        let fb = setup_fb_transport(self.cfg)?;
        let input_cfg = self
            .cfg
            .input
            .as_ref()
            .context("Config missing [input] section")?;
        let signals_cfg = self
            .cfg
            .signals
            .as_ref()
            .context("Config missing [signals] section")?;
        let mut engine = InputEngine::new(input_cfg, &self.cfg.locals, &self.cfg.derive)
            .context("Build input engine")?;
        let mut input_runtime =
            Devices::new(input_cfg.mode.as_str()).context("Initialize input devices")?;
        engine.set_mode(input_runtime.mode());
        let mapper =
            SignalMapper::new(signals_cfg, &self.cfg.locals).context("Compile signal mapper")?;
        let payload_observation_lookup_names = payload_observation_lookup_names(
            &mapper,
            state
                .trace
                .as_ref()
                .into_iter()
                .flat_map(TraceLogger::field_names),
        );
        let (viewer_input_tx, viewer_input_rx) =
            mpsc::sync_channel::<ViewerControlCommand>(VIEWER_CONTROL_QUEUE_CAPACITY);
        let scoped_run = match self
            .websocket_server
            .run_scoped(viewer_input_tx, |websocket| {
                self.external_interface.start()?;
                log_pacing_status(
                    self.mode,
                    self.lockstep_schedule,
                    self.steps_per_packet,
                    self.cfg.sim.dt,
                );
                status_line("");
                status_line("Ready. Simulation running.");
                status_line(&format!(
                    "  Open http://localhost:{} in a browser.",
                    self.http_port
                ));
                notify_editor_viewer_ready(self.http_port);
                let ctx = FrameCtx {
                    cfg: self.cfg,
                    fb: fb.as_ref(),
                    mapper: &mapper,
                    payload_observation_lookup_names: &payload_observation_lookup_names,
                    viewer_input_rx: &viewer_input_rx,
                    websocket,
                    realtime: self.realtime,
                    quit: self.quit,
                    external_interface: self.external_interface.handle(),
                    debug: self.debug,
                    dt: self.cfg.sim.dt,
                    mode: self.mode,
                    steps_per_packet: self.steps_per_packet,
                    lockstep_schedule: self.lockstep_schedule,
                };
                run_frames(&ctx, state, session, &mut engine, &mut input_runtime)
            }) {
            Ok(scoped_run) => scoped_run,
            Err(failures) => return Ok(ScheduledWebSocketPrimary::StartupFailure(failures)),
        };
        let (loop_result, shutdown) = scoped_run.into_parts();
        Ok(ScheduledWebSocketPrimary::Scoped {
            loop_result,
            shutdown,
        })
    }
}

pub(crate) fn run_sim_loop<S>(session: &mut S, args: SimLoopArgs<'_>) -> Result<()>
where
    S: SimulationSessionApi,
{
    let SimLoopArgs {
        cfg,
        http_port,
        ws_port,
        debug,
    } = args;
    let trace = open_trace_logger(cfg)?;
    let websocket_server = BroadcastServer::bind(ws_port).context("Bind WebSocket server")?;

    // Install the owned signal controller before any input, WebSocket, or
    // external-interface worker can start. Setup below is captured so every
    // returned path explicitly closes and joins this controller.
    let external_interface = ExternalInterfaceOwner::new(
        cfg.external_interface
            .as_ref()
            .map(|interface| interface.command.as_str()),
    );
    let quit = Arc::new(AtomicBool::new(false));
    let mut signal_controller =
        SignalController::install(external_interface.handle().clone(), Arc::clone(&quit))
            .context("Install owned simulation signal controller")?;

    let mode = cfg.effective_pacing_mode();
    let realtime = Arc::new(AtomicBool::new(matches!(mode, SimPacingMode::Realtime)));
    let steps_per_packet = cfg.sim.steps_per_packet;
    let lockstep_schedule = cfg
        .lockstep
        .as_ref()
        .map(|lockstep| LockstepSchedule::from_config(lockstep, cfg.sim.dt));
    let mut state = FrameState::new(trace);
    let scope = ScheduledSimulationScope {
        cfg,
        http_port,
        debug,
        websocket_server,
        external_interface: &external_interface,
        quit: &quit,
        realtime: &realtime,
        mode,
        steps_per_packet,
        lockstep_schedule,
    };
    let primary = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        scope.run(session, &mut state)
    }));

    let signal_shutdown_failure = signal_controller.shutdown().err();
    let external_cleanup_failure = external_interface.stop().err();
    match primary {
        Ok(primary) => complete_websocket_run(
            primary.unwrap_or_else(ScheduledWebSocketPrimary::SetupFailure),
            signal_shutdown_failure,
            external_cleanup_failure,
            &mut state,
        ),
        Err(original) => {
            resume_scheduled_panic(original, signal_shutdown_failure, external_cleanup_failure)
        }
    }
}

fn run_frames(
    ctx: &FrameCtx<'_>,
    state: &mut FrameState,
    session: &mut impl SimulationSessionApi,
    engine: &mut InputEngine,
    input_runtime: &mut Devices,
) -> Result<ScheduledLoopCompletion> {
    loop {
        match ctx.run_one_frame(state, session, engine, input_runtime)? {
            FrameControl::Continue => {}
            FrameControl::Break => return Ok(ScheduledLoopCompletion::Complete),
            FrameControl::WebSocketTerminated => {
                return Ok(ScheduledLoopCompletion::WebSocketTerminated);
            }
        }
    }
}

fn complete_websocket_run(
    primary: ScheduledWebSocketPrimary,
    signal_shutdown_failure: Option<SignalControllerShutdownFailure>,
    external_cleanup_failure: Option<ExternalInterfaceStopFailure>,
    state: &mut FrameState,
) -> Result<()> {
    let (
        setup_failure,
        loop_failure,
        peer_failures,
        terminal_failures,
        termination_without_failure,
    ) = match primary {
        ScheduledWebSocketPrimary::SetupFailure(failure) => (
            Some(failure),
            None,
            Box::default(),
            BroadcastServerTerminalFailures::default(),
            false,
        ),
        ScheduledWebSocketPrimary::StartupFailure(terminal_failures) => {
            let missing = terminal_failures.is_empty();
            (None, None, Box::default(), terminal_failures, missing)
        }
        ScheduledWebSocketPrimary::Scoped {
            loop_result,
            shutdown,
        } => {
            let (peer_failures, terminal_failures) = shutdown.into_parts();
            let (loop_failure, missing) = match loop_result {
                Ok(ScheduledLoopCompletion::Complete) => (None, false),
                Ok(ScheduledLoopCompletion::WebSocketTerminated) => {
                    (None, terminal_failures.is_empty())
                }
                Err(failure) => (Some(failure), false),
            };
            (
                None,
                loop_failure,
                peer_failures,
                terminal_failures,
                missing,
            )
        }
    };
    let peer_observation_failure = observe_joined_peer_failures(state, peer_failures).err();
    if setup_failure.is_none()
        && loop_failure.is_none()
        && peer_observation_failure.is_none()
        && signal_shutdown_failure.is_none()
        && external_cleanup_failure.is_none()
        && !termination_without_failure
        && terminal_failures.is_empty()
    {
        return Ok(());
    }
    Err(ScheduledWebSocketRunFailure {
        setup_failure,
        loop_failure,
        peer_observation_failure,
        signal_shutdown_failure,
        external_cleanup_failure,
        termination_without_failure,
        terminal_failures,
    }
    .into())
}

/// Emit a machine-parseable readiness marker on stderr once the HTTP server is
/// up, so an editor launching the browser viewer can detect when to open the
/// webview. Always printed (it is a benign status line); editors grep for it.
fn notify_editor_viewer_ready(http_port: u16) {
    status_line(&format!(
        "rumoca-viewer-ready http://127.0.0.1:{http_port}/"
    ));
}

fn status_line(message: &str) {
    let _status_result = write!(std::io::stderr(), "{message}\r\n");
}

fn setup_fb_transport(cfg: &SimulationConfig) -> Result<Option<FbTransport>> {
    if !cfg.has_fb() {
        eprintln!("  Mode: standalone (no UDP/codec)");
        return Ok(None);
    }
    let schema_cfg = cfg
        .schema
        .as_ref()
        .context("FB config present but missing [schema] section")?;
    let send_cfg = cfg
        .send
        .as_ref()
        .context("FB config present but missing [send] section")?;
    let recv_cfg = cfg
        .receive
        .as_ref()
        .context("FB config present but missing [receive] section")?;
    let pack = rumoca_codec::build_pack(schema_cfg, send_cfg).context("Build pack codec")?;
    let unpack = rumoca_codec::build_unpack(schema_cfg, recv_cfg).context("Build unpack codec")?;
    let recv_expected = unpack.expected_size();
    eprintln!("  Expecting {recv_expected}-byte receive packets");
    let udp = match resolve_udp(cfg) {
        Some(udp_cfg) => {
            eprintln!("  UDP listen: {}", udp_cfg.listen);
            eprintln!("  UDP send:   {}", udp_cfg.send);
            Some(UdpTransport::bind(udp_cfg)?)
        }
        None => None,
    };
    let send_publish = map_message_name(&cfg.publish, "send", &send_cfg.root_type);
    let receive_publish = map_message_name(&cfg.publish, "receive", &recv_cfg.root_type);
    let receive_subscribe = map_message_name(&cfg.subscribe, "receive", &recv_cfg.root_type);
    let zenoh = match resolve_zenoh(cfg) {
        Some(zenoh_cfg) => {
            let transport = ZenohTransport::open(zenoh_cfg, &cfg.publish, &cfg.subscribe)?;
            if let Some(name) = &send_publish
                && let Some(key) = transport.publish_key_for(name)
            {
                eprintln!("  Zenoh publish send '{name}': {key}");
            }
            if let Some(name) = &receive_publish
                && let Some(key) = transport.publish_key_for(name)
            {
                eprintln!("  Zenoh publish receive '{name}': {key}");
            }
            if let Some(name) = &receive_subscribe
                && let Some(key) = transport.subscribe_key_for(name)
            {
                eprintln!("  Zenoh subscribe receive '{name}': {key}");
            }
            Some(transport)
        }
        None => None,
    };
    if udp.is_none() && receive_subscribe.is_none() {
        anyhow::bail!(
            "FB config needs an inbound command transport: configure [transport.udp] or \
             [transport.zenoh] plus [subscribe] for the [receive] message"
        );
    }
    if udp.is_none() && send_publish.is_none() {
        anyhow::bail!(
            "FB config without [transport.udp] needs an outbound sensor transport: \
             configure [publish] for the [send] message"
        );
    }
    Ok(Some(FbTransport {
        udp,
        zenoh,
        pack,
        unpack,
        recv_expected,
        send_publish,
        receive_publish,
        receive_subscribe,
    }))
}

impl FrameCtx<'_> {
    fn run_one_frame(
        &self,
        state: &mut FrameState,
        session: &mut impl SimulationSessionApi,
        engine: &mut InputEngine,
        input_runtime: &mut Devices,
    ) -> Result<FrameControl> {
        if let FrameControl::WebSocketTerminated = self.observe_websocket_events(state)? {
            return Ok(FrameControl::WebSocketTerminated);
        }
        let frame_start = Instant::now();

        let first_packet_timeout =
            if matches!(self.mode, SimPacingMode::Lockstep) && self.fb.is_none() {
                Some(Duration::from_millis(50))
            } else {
                None
            };
        let viewer_input =
            drain_viewer_input(self.viewer_input_rx, first_packet_timeout, self.debug);
        let viewer_packet = !viewer_input.keys.is_empty();

        if let Some(enabled) = viewer_input.realtime {
            self.realtime.store(enabled, Ordering::Release);
            eprintln!("[sim] realtime: {enabled}");
        }
        if viewer_input.quit {
            self.quit.store(true, Ordering::Relaxed);
        }
        if self.quit.load(Ordering::Relaxed) {
            eprintln!("\n[sim] quit requested");
            return Ok(FrameControl::Break);
        }
        if matches!(self.mode, SimPacingMode::Lockstep)
            && let Some(schedule) = self.lockstep_schedule
        {
            return self.run_scheduled_lockstep_frame(
                schedule,
                state,
                session,
                engine,
                input_runtime,
                viewer_input,
            );
        }

        // Pacing-specific receive + advance gate.
        match self.mode {
            SimPacingMode::AsFastAsPossible | SimPacingMode::Realtime => {
                self.drain_udp(state, session, engine)?;
            }
            SimPacingMode::Lockstep => {
                let transport_packet = self.wait_for_command(state, session, engine)?;
                if !viewer_packet && !transport_packet {
                    // No input packet arrived — try again. Physics and input
                    // integrators stay paused (lockstep semantics).
                    return Ok(FrameControl::Continue);
                }
            }
        }

        let steps_this_frame = if matches!(self.mode, SimPacingMode::Lockstep) {
            self.steps_per_packet
        } else {
            1
        };
        let planned_dt = self.dt * steps_this_frame as f64;
        let poll_dt = if matches!(self.mode, SimPacingMode::Lockstep | SimPacingMode::Realtime) {
            planned_dt
        } else {
            let elapsed = state.last_poll.elapsed().as_secs_f64();
            state.last_poll = Instant::now();
            elapsed
        };
        input_runtime.poll_with_keyboard_events(engine, poll_dt, viewer_input.keys);
        if let FrameControl::Break = self.handle_signals(engine, session)? {
            return Ok(FrameControl::Break);
        }
        self.apply_model_inputs(state, session, engine, input_runtime)?;
        let mut advances_done = 0_u64;
        for _ in 0..steps_this_frame {
            let target = session.time() + self.dt;
            advance_session_to(session, target)?;
            advances_done += 1;
        }

        self.emit_payloads(state, session, engine, input_runtime)?;

        self.emit_status(state, session);

        // Realtime pacing is an explicit mode. Lockstep is paced by input
        // arrival, and as-fast-as-possible intentionally never sleeps here.
        if matches!(self.mode, SimPacingMode::Realtime) && self.realtime.load(Ordering::Relaxed) {
            let elapsed = frame_start.elapsed();
            let target = Duration::from_secs_f64(self.dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }
        state.frame_num += advances_done;
        Ok(FrameControl::Continue)
    }

    fn run_scheduled_lockstep_frame(
        &self,
        schedule: LockstepSchedule,
        state: &mut FrameState,
        session: &mut impl SimulationSessionApi,
        engine: &mut InputEngine,
        input_runtime: &mut Devices,
        viewer_input: ViewerInputDrain,
    ) -> Result<FrameControl> {
        // Slack for comparing accumulated send/control times against each other:
        // the schedule advances `next_lockstep_send_time` by `send_dt` each send,
        // so exact `==` would be defeated by float rounding. 1 ns is far below any
        // realistic sim dt yet large enough to absorb that accumulation drift.
        const EPS: f64 = 1e-9;

        let now = session.time();
        if !state.lockstep_schedule_initialized {
            state.lockstep_schedule_initialized = true;
            state.next_lockstep_control_time = now + schedule.receive_dt;
            state.next_lockstep_send_time = now + schedule.send_dt;
        }

        let control_time = state.next_lockstep_control_time;
        let poll_dt = (control_time - now).max(0.0);
        input_runtime.poll_with_keyboard_events(engine, poll_dt, viewer_input.keys);
        if let FrameControl::Break = self.handle_signals(engine, session)? {
            return Ok(FrameControl::Break);
        }
        self.apply_model_inputs(state, session, engine, input_runtime)?;

        while state.next_lockstep_send_time <= control_time + EPS {
            let target = state.next_lockstep_send_time.min(control_time);
            let advance_dt = target - session.time();
            if advance_dt > EPS {
                advance_session_with_max_dt(session, advance_dt, schedule.max_advance_dt)?;
            }
            self.emit_payloads(state, session, engine, input_runtime)?;
            // In this scheduled path `frame_num` counts *sends* (one per emitted
            // payload), whereas the packet-paced path counts internal solver
            // steps. The viewer only needs a monotonic frame counter, so the
            // differing units are intentional.
            state.frame_num += 1;
            self.emit_status(state, session);
            state.next_lockstep_send_time += schedule.send_dt;
        }

        let remaining_dt = control_time - session.time();
        if remaining_dt > EPS {
            advance_session_with_max_dt(session, remaining_dt, schedule.max_advance_dt)?;
        }

        let transport_packet = self.wait_for_command(state, session, engine)?;
        if transport_packet {
            state.next_lockstep_control_time += schedule.receive_dt;
        }
        Ok(FrameControl::Continue)
    }

    /// Apply configured local/runtime signal routes into model inputs before advancing.
    fn apply_model_inputs(
        &self,
        state: &FrameState,
        session: &mut impl SimulationSessionApi,
        engine: &mut InputEngine,
        input_runtime: &Devices,
    ) -> Result<()> {
        let wall_ms = wall_ms_since_unix_epoch()?;
        let model_time = session.time();
        let model_inputs = {
            let snapshot = SessionFrameSnapshot::new(
                session,
                self.mapper.model_input_lookup_names(),
                "pre-input",
            )?;
            let model_get = |name: &str| snapshot.get(name);
            let rt = RuntimeContext {
                frame_num: state.frame_num,
                wall_ms,
                input_connected: input_runtime.is_connected(),
                input_mode: input_runtime.mode(),
                input_message: engine.last_message(),
                model_time,
                model_get: &model_get,
            };
            self.mapper.build_model_inputs(engine, &rt)?
        };
        for (name, val) in model_inputs {
            session
                .set_input(&name, val)
                .with_context(|| format!("set session input '{name}'"))?;
        }
        Ok(())
    }

    /// Build payloads + send FB + push viewer JSON.
    /// Shared by all pacing paths.
    fn emit_payloads(
        &self,
        state: &mut FrameState,
        session: &mut impl SimulationSessionApi,
        engine: &mut InputEngine,
        input_runtime: &Devices,
    ) -> Result<()> {
        let wall_ms = wall_ms_since_unix_epoch()?;
        let (send_frame, json) = {
            let model_time = session.time();
            let snapshot = SessionFrameSnapshot::new(
                session,
                self.payload_observation_lookup_names,
                "post-advance payload",
            )?;
            let model_get = |name: &str| snapshot.get(name);
            let rt = RuntimeContext {
                frame_num: state.frame_num,
                wall_ms,
                input_connected: input_runtime.is_connected(),
                input_mode: input_runtime.mode(),
                input_message: engine.last_message(),
                model_time,
                model_get: &model_get,
            };
            let send_frame = self
                .fb
                .map(|_| self.mapper.build_send(engine, &rt))
                .transpose()?;
            let json = self.mapper.build_viewer_json(engine, &rt)?;
            if let Some(trace) = state.trace.as_mut() {
                trace.record(engine, &rt)?;
            }
            (send_frame, json)
        };
        if let (Some(fb), Some(frame)) = (self.fb, send_frame) {
            let bytes = fb.pack.pack(&frame);
            if let Some(udp) = &fb.udp {
                udp.send(&bytes)?;
            }
            if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.send_publish) {
                zenoh.publish(message, &bytes)?;
            }
            state.send_count += 1;
        }
        let json = self.with_runtime_transport_fields(json, state, session.time())?;
        self.websocket.publish_state(json)?;
        Ok(())
    }

    fn with_runtime_transport_fields(
        &self,
        json: String,
        state: &FrameState,
        model_time: f64,
    ) -> Result<String> {
        let mut value: JsonValue =
            serde_json::from_str(&json).context("parse viewer JSON for runtime fields")?;
        let obj = value
            .as_object_mut()
            .context("viewer JSON root must be an object")?;
        insert_u64(obj, "runtime_tx_count", state.send_count);
        insert_u64(obj, "runtime_rx_count", state.pkt_count);
        insert_u64(
            obj,
            "runtime_ws_peer_failure_count",
            state.websocket_peer_failures,
        );
        if model_time > 0.0 {
            obj.insert(
                "runtime_tx_actual_hz".to_string(),
                JsonValue::from(state.send_count as f64 / model_time),
            );
            obj.insert(
                "runtime_rx_actual_hz".to_string(),
                JsonValue::from(state.pkt_count as f64 / model_time),
            );
        }
        if let Some(schedule) = self.lockstep_schedule {
            obj.insert(
                "runtime_tx_target_hz".to_string(),
                JsonValue::from(1.0 / schedule.send_dt),
            );
            obj.insert(
                "runtime_rx_target_hz".to_string(),
                JsonValue::from(1.0 / schedule.receive_dt),
            );
        }
        Ok(value.to_string())
    }

    fn observe_websocket_events(&self, state: &mut FrameState) -> Result<FrameControl> {
        loop {
            match self.websocket.try_next_event()? {
                Some(BroadcastServerEvent::PeerFailure(record)) => {
                    observe_peer_failure(state, record)?;
                }
                Some(BroadcastServerEvent::TerminalFailure) => {
                    return Ok(FrameControl::WebSocketTerminated);
                }
                None => return Ok(FrameControl::Continue),
            }
        }
    }

    fn emit_status(&self, state: &FrameState, session: &impl SimulationSessionApi) {
        // Status line (~1 Hz). In lockstep the period is approximate because
        // frame rate depends on external input pacing.
        let status_period = (1.0_f64 / self.dt).max(1.0) as u64;
        if state.frame_num.is_multiple_of(status_period) {
            eprint!(
                "\r[sim] t={:.1}s frame={} pkts={}            ",
                session.time(),
                state.frame_num,
                state.pkt_count
            );
        }
    }

    /// Lockstep receive: consume one transport packet, apply to session/locals.
    /// Returns `true` if a packet was consumed, `false` on timeout.
    fn wait_for_command(
        &self,
        state: &mut FrameState,
        session: &mut impl SimulationSessionApi,
        engine: &mut InputEngine,
    ) -> Result<bool> {
        let Some(fb) = self.fb else {
            return Ok(false);
        };
        if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.receive_subscribe) {
            let Some(datagram) = zenoh.recv_latest_blocking(message, Duration::from_millis(100))
            else {
                return Ok(false);
            };
            state.pkt_count += 1;
            apply_fb_datagram(fb, &datagram, session, engine)?;
            return Ok(true);
        }
        let Some(udp) = &fb.udp else {
            return Ok(false);
        };
        let Some(n) = udp.recv_blocking(&mut state.recv_buf)? else {
            return Ok(false);
        };
        state.pkt_count += 1;
        let datagram = &state.recv_buf[..n];
        if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.receive_publish) {
            zenoh.publish(message, datagram)?;
        }
        apply_fb_datagram(fb, datagram, session, engine)?;
        Ok(true)
    }

    fn handle_signals(
        &self,
        engine: &mut InputEngine,
        session: &mut impl SimulationSessionApi,
    ) -> Result<FrameControl> {
        if engine.take_signal("quit") {
            eprintln!("\n[sim] quit requested");
            return Ok(FrameControl::Break);
        }
        if self.quit.load(Ordering::Relaxed) {
            eprintln!("\n[sim] quit requested by viewer");
            return Ok(FrameControl::Break);
        }
        if let Some(reset_cfg) = self.cfg.reset.as_ref()
            && engine.take_signal(&reset_cfg.on_signal)
        {
            handle_reset(
                reset_cfg,
                engine,
                session,
                ResetRuntime {
                    external_handle: self.external_interface,
                },
            )?;
        }
        if self.debug
            && let Some(dbg) = self.cfg.debug_log.as_ref()
            && engine.take_signal(&dbg.trigger_signal)
        {
            // Debug triggers report that no buffered log is available.
            eprintln!("[debug] log trigger — ring buffer not yet implemented");
        }
        Ok(FrameControl::Continue)
    }

    fn drain_udp(
        &self,
        state: &mut FrameState,
        session: &mut impl SimulationSessionApi,
        engine: &mut InputEngine,
    ) -> Result<()> {
        let Some(fb) = self.fb else {
            return Ok(());
        };
        let expected = fb.recv_expected;
        if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.receive_subscribe) {
            return drain_zenoh_subscribe(fb, zenoh, message, expected, state, session, engine);
        }
        let Some(udp) = &fb.udp else {
            return Ok(());
        };
        let mut error = None;
        udp.drain(&mut state.recv_buf, |datagram| {
            state.pkt_count += 1;
            if datagram.len() != expected || error.is_some() {
                return;
            }
            if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.receive_publish)
                && let Err(err) = zenoh.publish(message, datagram)
            {
                error = Some(err);
                return;
            }
            error = apply_fb_datagram(fb, datagram, session, engine).err();
        })?;
        if let Some(error) = error {
            return Err(error);
        }
        Ok(())
    }
}

fn observe_joined_peer_failures(
    state: &mut FrameState,
    peer_failures: Box<[PeerFailureRecord]>,
) -> std::result::Result<(), PeerFailureObservationFailure> {
    let mut records = Vec::from(peer_failures).into_iter();
    while let Some(record) = records.next() {
        if let Err(mut failure) = observe_peer_failure(state, record) {
            failure.unobserved = records.collect::<Vec<_>>().into_boxed_slice();
            return Err(failure);
        }
    }
    Ok(())
}

fn observe_peer_failure(
    state: &mut FrameState,
    record: PeerFailureRecord,
) -> std::result::Result<(), PeerFailureObservationFailure> {
    let next_count = match next_peer_failure_count(state.websocket_peer_failures) {
        Ok(next_count) => next_count,
        Err(kind) => {
            return Err(PeerFailureObservationFailure {
                kind,
                failed: record,
                unobserved: Box::default(),
            });
        }
    };
    state.websocket_peer_failures = next_count;
    eprintln!(
        "[WS] isolated peer failure #{}: peer={} kind={:?} detail={}",
        state.websocket_peer_failures,
        record.peer(),
        record.classification(),
        record.detail()
    );
    Ok(())
}

fn next_peer_failure_count(
    current: u64,
) -> std::result::Result<u64, PeerFailureObservationFailureKind> {
    current
        .checked_add(1)
        .ok_or(PeerFailureObservationFailureKind::CounterOverflow)
}

/// Drain the Zenoh subscribe key, applying each datagram to the session. Split
/// out of `drain_udp` so the receive closure isn't nested under the transport
/// match arm.
fn drain_zenoh_subscribe(
    fb: &FbTransport,
    zenoh: &ZenohTransport,
    message: &str,
    expected: usize,
    state: &mut FrameState,
    session: &mut impl SimulationSessionApi,
    engine: &mut InputEngine,
) -> Result<()> {
    let mut error = None;
    zenoh.drain(message, |datagram| {
        state.pkt_count += 1;
        if datagram.len() != expected || error.is_some() {
            return;
        }
        error = apply_fb_datagram(fb, datagram, session, engine).err();
    });
    if let Some(error) = error {
        return Err(error);
    }
    Ok(())
}

// ── Helpers ────────────────────────────────────────────────────────────────

/// Decode and apply a received flatbuffer datagram when it matches the schema.
fn apply_fb_datagram(
    fb: &FbTransport,
    datagram: &[u8],
    session: &mut impl SimulationSessionApi,
    engine: &mut InputEngine,
) -> Result<()> {
    if datagram.len() != fb.recv_expected {
        return Ok(());
    }
    let values = fb.unpack.unpack(datagram);
    apply_received(&values, session, engine)
}

/// Apply a received SignalFrame to the session or locals based on the key
/// prefix. Keys like `"model:omega_m1"` are applied to the session; keys
/// like `"local:armed"` go to the engine's locals; bare names default to
/// the session for convenience.
fn apply_received(
    values: &rumoca_codec::SignalFrame,
    session: &mut impl SimulationSessionApi,
    engine: &mut InputEngine,
) -> Result<()> {
    for (key, val) in values.iter() {
        if let Some(rest) = key.strip_prefix("model:") {
            session
                .set_input(rest, val)
                .with_context(|| format!("set received session input '{rest}'"))?;
        } else if let Some(rest) = key.strip_prefix("local:") {
            engine.set_local(rest, val);
        } else {
            session
                .set_input(key, val)
                .with_context(|| format!("set received session input '{key}'"))?;
        }
    }
    Ok(())
}

struct ResetRuntime<'a> {
    external_handle: &'a ExternalInterfaceHandle,
}

fn handle_reset<S>(
    reset_cfg: &ResetConfig,
    engine: &mut InputEngine,
    session: &mut S,
    runtime: ResetRuntime<'_>,
) -> Result<()>
where
    S: SimulationSessionApi,
{
    eprintln!("\n[reset] triggered");
    if reset_cfg.reset_locals {
        engine.reset();
    }
    if reset_cfg.restart_external_interface {
        runtime
            .external_handle
            .start()
            .context("reset: external-interface restart failed")?;
    }
    if reset_cfg.reset_session {
        let reset_time = session.time();
        session
            .retime(reset_time)
            .context("reset: session reset failed")?;
        eprintln!("[reset] session reset");
    }
    Ok(())
}

// ── Advance helper ─────────────────────────────────────────────────────────

fn advance_session_to(session: &mut impl SimulationSessionApi, target: f64) -> Result<()> {
    let dt = target - session.time();
    if dt <= 0.0 {
        return Ok(());
    }
    let max_advance_dt = session.max_schedule_advance_dt().unwrap_or(dt);
    advance_session_with_max_dt(session, dt, max_advance_dt)
}

fn advance_session_with_max_dt(
    session: &mut impl SimulationSessionApi,
    dt: f64,
    max_advance_dt: f64,
) -> Result<()> {
    let target = session.time() + dt;
    let advance_dt = target - session.time();
    if advance_dt <= 0.0 {
        return Ok(());
    }
    let max_sub_dt = session
        .max_schedule_advance_dt()
        .map(|session_dt| session_dt.min(max_advance_dt))
        .unwrap_or(max_advance_dt)
        .min(advance_dt);
    let n_steps = ((advance_dt / max_sub_dt).ceil() as usize).max(1);
    let sub_dt = advance_dt / n_steps as f64;
    for i in 0..n_steps {
        let sub_target = if i + 1 == n_steps {
            target
        } else {
            session.time() + sub_dt
        };
        if let Err(e) = session.advance_to(sub_target) {
            eprintln!(
                "\r[sim] advance {}/{n_steps} failed (sub_dt={sub_dt:.4}): {e}",
                i + 1,
            );
            return Err(anyhow::anyhow!(
                "simulation advance {}/{n_steps} failed at t={:.9}: {e}",
                i + 1,
                session.time()
            ));
        }
    }
    Ok(())
}

// WebSocket server lives in rumoca-transport-websocket.
// HTTP viewer server lives in rumoca-sim::web.

#[cfg(test)]
mod tests;
