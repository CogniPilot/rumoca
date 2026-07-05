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

use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use crate::{SimPacingMode, SimulationSessionApi, stop_time_reached_with_tol};
use anyhow::{Context, Result};
use rumoca_codec::{PackCodec, UnpackCodec};
use rumoca_input::{
    InputEngine, KeyCode, KeyModifiers, KeyboardEvent, RuntimeContext, SignalMapper,
};
use rumoca_transport_udp::{UdpConfig, UdpTransport};
use rumoca_transport_websocket::run_broadcast_server;
use rumoca_transport_zenoh::ZenohTransport;
use serde::Deserialize;
use serde_json::{Map as JsonMap, Value as JsonValue};

use crate::runner::devices::{self, Devices};

use crate::runner::config::{LockstepConfig, ResetConfig, SimulationConfig};

fn wall_ms_since_unix_epoch() -> Result<f64> {
    Ok(SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .context("read wall clock time since Unix epoch")?
        .as_millis() as f64)
}

// ── External-interface subprocess ──────────────────────────────────────────

struct ExternalInterfaceProcess {
    child: Option<Child>,
    command: String,
}

impl ExternalInterfaceProcess {
    fn new(command: &str) -> Self {
        Self {
            child: None,
            command: command.to_string(),
        }
    }

    fn start(&mut self) -> Result<()> {
        self.stop();
        eprintln!("[external_interface] starting: {}", self.command);
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
        // On Linux: tell the kernel to send SIGKILL to this child if rumoca
        // ever dies — covers SIGKILL, panic, OOM, anything that skips Drop.
        // Without this, process_group(0) actually makes orphaning worse: the
        // child outlives us in its own pgrp with no one to clean it up.
        #[cfg(target_os = "linux")]
        install_pdeathsig(&mut cmd);
        let child = cmd
            .spawn()
            .with_context(|| format!("Failed to start external interface: {}", self.command))?;
        eprintln!("[external_interface] pid {}", child.id());
        self.child = Some(child);
        Ok(())
    }

    fn stop(&mut self) {
        let Some(mut child) = self.child.take() else {
            return;
        };
        let pid = child.id();
        eprintln!("[external_interface] killing pid {pid}");
        let _ = child.kill();
        // Best-effort wait; if the child won't die within 500ms (e.g., a
        // ptrace'd debugger is eating SIGKILL), abandon the wait rather
        // than blocking shutdown.
        let deadline = Instant::now() + Duration::from_millis(500);
        while Instant::now() < deadline {
            match child.try_wait() {
                Ok(Some(_)) => return,
                Ok(None) => thread::sleep(Duration::from_millis(20)),
                Err(_) => return,
            }
        }
        eprintln!("[external_interface] pid {pid} did not exit within 500ms; abandoning");
    }
}

impl Drop for ExternalInterfaceProcess {
    fn drop(&mut self) {
        self.stop();
    }
}

/// Set `PR_SET_PDEATHSIG = SIGKILL` on the child via `pre_exec`, so the
/// kernel reaps the child if the parent dies for any reason (SIGKILL,
/// panic, OOM) — not just clean shutdown paths that run Drop.
#[cfg(target_os = "linux")]
#[allow(unsafe_code)]
fn install_pdeathsig(cmd: &mut Command) {
    use std::os::unix::process::CommandExt;
    unsafe {
        cmd.pre_exec(|| {
            if libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGKILL) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
}

// ── Trace log (streaming CSV of captured fields, one row per frame) ───────
//
// Activated by a `[debug_log]` section in the scenario config (path defaults to
// `rumoca_trace.csv`, override with `path = ...`). Fields come from
// `debug_log.capture`, evaluated
// each frame. Writes through a BufWriter and flushes on Drop. Designed
// for offline plotting — load into a notebook with pandas.read_csv.

struct TraceLogger {
    writer: BufWriter<File>,
    fields: Vec<String>,
    path: PathBuf,
}

impl TraceLogger {
    fn open(path: PathBuf, fields: Vec<String>) -> Result<Self> {
        let file =
            File::create(&path).with_context(|| format!("Open trace log {}", path.display()))?;
        let mut writer = BufWriter::new(file);
        let header = fields.join(",");
        writeln!(writer, "{header}")?;
        eprintln!("  Trace log: {} ({} columns)", path.display(), fields.len());
        Ok(Self {
            writer,
            fields,
            path,
        })
    }

    fn record(&mut self, engine: &InputEngine, rt: &RuntimeContext<'_>) -> Result<()> {
        let mut first = true;
        for name in &self.fields {
            if !first {
                self.writer.write_all(b",")?;
            }
            first = false;
            let v = resolve_trace_field(name, engine, rt)?;
            write!(self.writer, "{v}")?;
        }
        self.writer.write_all(b"\n")?;
        Ok(())
    }
}

fn open_trace_logger(cfg: &SimulationConfig) -> Result<Option<TraceLogger>> {
    let Some(dbg) = cfg.debug_log.as_ref() else {
        return Ok(None);
    };
    // Default: drop `rumoca_trace.csv` in the cwd so you always have a log to
    // share with no setup. Override with `path = "/path/other.csv"` under the
    // scenario's [debug_log] config.
    let logger = TraceLogger::open(PathBuf::from(dbg.path.clone()), dbg.capture.clone())?;
    Ok(Some(logger))
}

impl Drop for TraceLogger {
    fn drop(&mut self) {
        let _ = self.writer.flush();
        eprintln!("[trace] flushed to {}", self.path.display());
    }
}

/// Resolve a `debug_log.capture` field to an f64 using the same prefix
/// scheme as signal mapper: `model:`, `local:` (supports `.idx`),
/// `runtime:frame_num|wall_ms|input_connected|model_time`. Missing
fn resolve_trace_field(name: &str, engine: &InputEngine, rt: &RuntimeContext<'_>) -> Result<f64> {
    if let Some(rest) = name.strip_prefix("model:") {
        if rest == "time" {
            return Ok(rt.model_time);
        }
        return (rt.model_get)(rest)?
            .ok_or_else(|| anyhow::anyhow!("trace field model:{rest} did not resolve"));
    }
    if let Some(rest) = name.strip_prefix("local:") {
        return engine
            .get(rest)
            .ok_or_else(|| anyhow::anyhow!("trace field local:{rest} did not resolve"));
    }
    if let Some(rest) = name.strip_prefix("runtime:") {
        return match rest {
            "frame_num" => Ok(rt.frame_num as f64),
            "wall_ms" => Ok(rt.wall_ms),
            "input_connected" => Ok(f64::from(u8::from(rt.input_connected))),
            "model_time" => Ok(rt.model_time),
            _ => Err(anyhow::anyhow!("unknown trace runtime field '{rest}'")),
        };
    }
    Err(anyhow::anyhow!(
        "trace field '{name}' must use model:, local:, or runtime:"
    ))
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

#[derive(Deserialize)]
struct ViewerInputCommand {
    key: Option<ViewerKeyCommand>,
    #[serde(default)]
    quit: bool,
}

#[derive(Deserialize)]
struct ViewerKeyCommand {
    code: String,
    key: Option<String>,
    #[serde(default = "default_key_pressed")]
    pressed: bool,
    #[serde(default)]
    shift: bool,
    #[serde(default)]
    ctrl: bool,
    #[serde(default)]
    alt: bool,
}

fn default_key_pressed() -> bool {
    true
}

#[derive(Default)]
struct ViewerInputDrain {
    keys: Vec<KeyboardEvent>,
    labels: Vec<String>,
    quit: bool,
}

fn drain_viewer_input(
    rx: &mpsc::Receiver<String>,
    first_packet_timeout: Option<Duration>,
    debug: bool,
) -> ViewerInputDrain {
    let mut drained = ViewerInputDrain::default();
    let mut events = Vec::new();
    if let Some(timeout) = first_packet_timeout
        && let Ok(text) = rx.recv_timeout(timeout)
    {
        drain_viewer_command_text(text, &mut drained, &mut events);
    }
    while let Ok(text) = rx.try_recv() {
        drain_viewer_command_text(text, &mut drained, &mut events);
    }
    if debug && !events.is_empty() {
        eprintln!(
            "\r[input] viewer keys: {}                    ",
            drained.labels.join(", ")
        );
    }
    drained.keys = events;
    drained
}

fn drain_viewer_command_text(
    text: String,
    drained: &mut ViewerInputDrain,
    events: &mut Vec<KeyboardEvent>,
) {
    let Ok(command) = serde_json::from_str::<ViewerInputCommand>(&text) else {
        return;
    };
    if command.quit {
        drained.quit = true;
    }
    if let Some(key) = command.key
        && let Some(event) = browser_key_to_event(&key)
    {
        let suffix = if key.pressed { "" } else { " up" };
        drained.labels.push(format!("{}{}", key.code, suffix));
        events.push(event);
    }
}

fn browser_key_to_event(key: &ViewerKeyCommand) -> Option<KeyboardEvent> {
    let code = match key.code.as_str() {
        "ArrowUp" => KeyCode::Up,
        "ArrowDown" => KeyCode::Down,
        "ArrowLeft" => KeyCode::Left,
        "ArrowRight" => KeyCode::Right,
        "Enter" => KeyCode::Enter,
        "Tab" => KeyCode::Tab,
        "Escape" => KeyCode::Esc,
        "Backspace" => KeyCode::Backspace,
        "Delete" => KeyCode::Delete,
        "Space" => KeyCode::Char(' '),
        code if code.starts_with("Key") && code.len() == 4 => {
            KeyCode::Char(code.chars().nth(3)?.to_ascii_lowercase())
        }
        code if code.starts_with("Digit") && code.len() == 6 => KeyCode::Char(code.chars().nth(5)?),
        _ => {
            let key_text = key.key.as_deref()?;
            if key_text.chars().count() == 1 {
                KeyCode::Char(key_text.chars().next()?.to_ascii_lowercase())
            } else {
                return None;
            }
        }
    };
    let mut modifiers = KeyModifiers::NONE;
    if key.shift {
        modifiers |= KeyModifiers::SHIFT;
    }
    if key.ctrl {
        modifiers |= KeyModifiers::CONTROL;
    }
    if key.alt {
        modifiers |= KeyModifiers::ALT;
    }
    Some(if key.pressed {
        KeyboardEvent::holdable_press(code, modifiers)
    } else {
        KeyboardEvent::released(code, modifiers)
    })
}

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
    viewer_input_rx: &'a mpsc::Receiver<String>,
    state_tx: &'a mpsc::Sender<String>,
    realtime: &'a Arc<AtomicBool>,
    quit: &'a Arc<AtomicBool>,
    external_interface: &'a Arc<Mutex<Option<ExternalInterfaceProcess>>>,
    debug: bool,
    dt: f64,
    t_end: f64,
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
    last_poll: Instant,
    trace: Option<TraceLogger>,
    lockstep_schedule_initialized: bool,
    next_lockstep_send_time: f64,
    next_lockstep_control_time: f64,
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
    values: Option<indexmap::IndexMap<String, f64>>,
}

impl SessionFrameSnapshot {
    fn new(session: &impl SimulationSessionApi, names: &[String]) -> Result<Self> {
        Ok(Self {
            values: session.values_for(names)?,
        })
    }

    fn get(&self, session: &impl SimulationSessionApi, name: &str) -> Result<Option<f64>> {
        if let Some(value) = self
            .values
            .as_ref()
            .and_then(|values| values.get(name).copied())
        {
            return Ok(Some(value));
        }
        session.get(name).map_err(Into::into)
    }
}

enum FrameControl {
    Continue,
    Break,
}

/// Run the interactive simulation app. Blocks the calling thread. In standalone
/// mode (no `[schema]`/`[receive]`/`[send]` in config) the UDP socket and
/// codecs are not created and no external-interface coupling happens.
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

    // ── Signal handler FIRST, before any other thread or child spawns. ───
    // signal_hook masks the target signals on threads spawned after it, so
    // installing it ahead of the input engine / WS thread / external child
    // ensures SIGINT/SIGTERM funnel to our dedicated signal thread — not a
    // gilrs worker, not zephyr's pgrp.
    let external_interface: Arc<Mutex<Option<ExternalInterfaceProcess>>> =
        Arc::new(Mutex::new(None));
    let quit = Arc::new(AtomicBool::new(false));
    spawn_sigint_handler(Arc::clone(&external_interface), Arc::clone(&quit));

    let fb = setup_fb_transport(cfg)?;

    // ── Input engine + signal mapper (config-driven) ──────────────────────
    let input_cfg = cfg
        .input
        .as_ref()
        .context("Config missing [input] section")?;
    let signals_cfg = cfg
        .signals
        .as_ref()
        .context("Config missing [signals] section")?;
    let mut engine =
        InputEngine::new(input_cfg, &cfg.locals, &cfg.derive).context("Build input engine")?;
    let mut input_runtime =
        Devices::new(input_cfg.mode.as_str()).context("Initialize input devices")?;
    engine.set_mode(input_runtime.mode());
    let mapper = SignalMapper::new(signals_cfg, &cfg.locals).context("Compile signal mapper")?;

    // ── External interface + WS thread ────────────────────────────────────
    start_external_interface_into(cfg, &external_interface)?;
    let (state_tx, state_rx) = mpsc::channel::<String>();
    let (viewer_input_tx, viewer_input_rx) = mpsc::channel::<String>();
    let mode = cfg.effective_pacing_mode();
    let realtime = Arc::new(AtomicBool::new(matches!(mode, SimPacingMode::Realtime)));
    let realtime_ws = Arc::clone(&realtime);
    let quit_ws = Arc::clone(&quit);
    let (ws_ready_tx, ws_ready_rx) = mpsc::channel();
    thread::spawn(move || {
        run_broadcast_server(
            ws_port,
            state_rx,
            Some(viewer_input_tx),
            Some(ws_ready_tx),
            realtime_ws,
            quit_ws,
        )
    });
    let ws_ready = ws_ready_rx
        .recv()
        .context("WebSocket server exited before reporting readiness")?;
    if let Err(error) = ws_ready {
        anyhow::bail!(error);
    }

    let steps_per_packet = cfg.sim.steps_per_packet;
    let lockstep_schedule = cfg
        .lockstep
        .as_ref()
        .map(|lockstep| LockstepSchedule::from_config(lockstep, cfg.sim.dt));
    log_pacing_status(mode, lockstep_schedule, steps_per_packet, cfg.sim.dt);
    status_line("");
    status_line("Ready. Simulation running.");
    status_line(&format!(
        "  Open http://localhost:{http_port} in a browser."
    ));
    notify_editor_viewer_ready(http_port);

    // ── Loop ──────────────────────────────────────────────────────────────
    let ctx = FrameCtx {
        cfg,
        fb: fb.as_ref(),
        mapper: &mapper,
        viewer_input_rx: &viewer_input_rx,
        state_tx: &state_tx,
        realtime: &realtime,
        quit: &quit,
        external_interface: &external_interface,
        debug,
        dt: cfg.sim.dt,
        t_end: cfg.sim.t_end,
        mode,
        steps_per_packet,
        lockstep_schedule,
    };
    let trace = open_trace_logger(cfg)?;
    let mut state = FrameState {
        recv_buf: [0u8; 512],
        pkt_count: 0,
        send_count: 0,
        frame_num: 0,
        last_poll: Instant::now(),
        trace,
        lockstep_schedule_initialized: false,
        next_lockstep_send_time: 0.0,
        next_lockstep_control_time: 0.0,
    };
    while let FrameControl::Continue =
        ctx.run_one_frame(&mut state, session, &mut engine, &mut input_runtime)?
    {}

    // Explicit stop: the signal-handler thread still holds an Arc clone of
    // `external_interface`, so Drop would not fire on normal exit and a child
    // process could orphan. Kill the child here, deterministically.
    if let Ok(mut ap) = external_interface.lock()
        && let Some(proc) = ap.as_mut()
    {
        proc.stop();
    }
    Ok(())
}

/// Emit a machine-parseable readiness marker on stderr once the HTTP server is
/// up, so an editor launching the interactive viewer can detect when to open the
/// webview. Always printed (it is a benign status line); editors grep for it.
fn notify_editor_viewer_ready(http_port: u16) {
    status_line(&format!(
        "rumoca-viewer-ready http://127.0.0.1:{http_port}/"
    ));
}

fn status_line(message: &str) {
    let _ = write!(std::io::stderr(), "{message}\r\n");
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

        if viewer_input.quit {
            self.quit.store(true, Ordering::Relaxed);
        }
        if self.quit.load(Ordering::Relaxed) {
            eprintln!("\n[sim] quit requested");
            return Ok(FrameControl::Break);
        }
        if self.final_time_reached(session) {
            eprintln!("\n[sim] reached t_end={:.6}", self.t_end);
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
        let planned_dt = self
            .remaining_dt(session)
            .min(self.dt * steps_this_frame as f64);
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
            if self.final_time_reached(session) {
                break;
            }
            let target = (session.time() + self.dt).min(self.t_end);
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
        if self.final_time_reached(session) {
            eprintln!("\n[sim] reached t_end={:.6}", self.t_end);
            return Ok(FrameControl::Break);
        }
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

        let control_time = state.next_lockstep_control_time.min(self.t_end);
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
            if self.final_time_reached(session) {
                self.emit_payloads(state, session, engine, input_runtime)?;
                self.emit_status(state, session);
                eprintln!("\n[sim] reached t_end={:.6}", self.t_end);
                return Ok(FrameControl::Break);
            }
        } else if self.final_time_reached(session) {
            eprintln!("\n[sim] reached t_end={:.6}", self.t_end);
            return Ok(FrameControl::Break);
        }

        let transport_packet = self.wait_for_command(state, session, engine)?;
        if transport_packet {
            state.next_lockstep_control_time += schedule.receive_dt;
        }
        Ok(FrameControl::Continue)
    }

    fn final_time_reached(&self, session: &impl SimulationSessionApi) -> bool {
        stop_time_reached_with_tol(session.time(), self.t_end)
    }

    fn remaining_dt(&self, session: &impl SimulationSessionApi) -> f64 {
        (self.t_end - session.time()).max(0.0)
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
        let model_get = |name: &str| session.get(name).map_err(Into::into);
        let rt = RuntimeContext {
            frame_num: state.frame_num,
            wall_ms,
            input_connected: input_runtime.is_connected(),
            input_mode: input_runtime.mode(),
            input_message: engine.last_message(),
            model_time,
            model_get: &model_get,
        };
        for (name, val) in self.mapper.build_model_inputs(engine, &rt)? {
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
            let snapshot = SessionFrameSnapshot::new(session, self.mapper.model_lookup_names())?;
            let model_get = |name: &str| snapshot.get(session, name);
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
                udp.send(&bytes);
            }
            if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.send_publish) {
                zenoh.publish(message, &bytes)?;
            }
            state.send_count += 1;
        }
        let json = self.with_runtime_transport_fields(json, state, session.time())?;
        let _ = self.state_tx.send(json);
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
        let Some(n) = udp.recv_blocking(&mut state.recv_buf) else {
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
            // TODO(phase 4b): ring-buffer-backed debug log dump.
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
            if let (Some(zenoh), Some(message)) = (&fb.zenoh, &fb.receive_publish) {
                if let Err(err) = zenoh.publish(message, datagram) {
                    error = Some(err);
                    return;
                }
            }
            error = apply_fb_datagram(fb, datagram, session, engine).err();
        });
        if let Some(error) = error {
            return Err(error);
        }
        Ok(())
    }
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

fn start_external_interface_into(
    cfg: &SimulationConfig,
    external_interface: &Arc<Mutex<Option<ExternalInterfaceProcess>>>,
) -> Result<()> {
    if let Some(interface_cfg) = &cfg.external_interface {
        let mut ap = ExternalInterfaceProcess::new(&interface_cfg.command);
        ap.start()?;
        *external_interface
            .lock()
            .map_err(|_| anyhow::anyhow!("external-interface lock poisoned"))? = Some(ap);
    }
    Ok(())
}

/// Set up a robust shutdown path for SIGINT/SIGTERM:
/// - 1st signal: clean shutdown (kill external interface with timeout, disable raw
///   mode, ask the main loop to exit)
/// - 2nd signal: hard exit 130 (skip cleanup)
///
/// Uses `signal_hook::iterator::Signals` — a blocking iterator that wakes
/// exactly when a signal arrives. No polling, no race windows. Unix-only;
/// on Windows the std runtime's default Ctrl-C handler is used.
#[cfg(not(unix))]
fn spawn_sigint_handler(
    _external_interface: Arc<Mutex<Option<ExternalInterfaceProcess>>>,
    _quit: Arc<AtomicBool>,
) {
}

#[cfg(unix)]
fn spawn_sigint_handler(
    external_interface: Arc<Mutex<Option<ExternalInterfaceProcess>>>,
    quit: Arc<AtomicBool>,
) {
    use signal_hook::consts::{SIGINT, SIGTERM};
    use signal_hook::iterator::Signals;

    let mut signals = match Signals::new([SIGINT, SIGTERM]) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Warning: could not install signal handler: {e}");
            return;
        }
    };
    eprintln!("  Shutdown: Ctrl-C once for clean exit; twice to force quit.");

    thread::spawn(move || {
        let mut presses: u32 = 0;
        for sig in signals.forever() {
            presses += 1;
            eprintln!("\r[sim] signal {sig} received (press {presses})                    \r");
            if presses == 1 {
                eprintln!("[sim] shutdown requested — press Ctrl-C again to force quit");
                quit.store(true, Ordering::Relaxed);
                spawn_cleanup_thread(Arc::clone(&external_interface));
            } else {
                eprintln!("[sim] force quit");
                devices::disable_terminal_raw_mode();
                std::process::exit(130);
            }
        }
    });
}

#[cfg(unix)]
fn spawn_cleanup_thread(external_interface: Arc<Mutex<Option<ExternalInterfaceProcess>>>) {
    thread::spawn(move || {
        if let Ok(mut ap) = external_interface.lock()
            && let Some(proc) = ap.as_mut()
        {
            proc.stop();
        }
        devices::disable_terminal_raw_mode();
    });
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
    external_handle: &'a Arc<Mutex<Option<ExternalInterfaceProcess>>>,
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
    if reset_cfg.restart_external_interface
        && let Ok(mut ap) = runtime.external_handle.lock()
        && let Some(proc) = ap.as_mut()
        && let Err(e) = proc.start()
    {
        eprintln!("[reset] external-interface restart failed: {e}");
    }
    if reset_cfg.reset_session {
        let reset_time = session.time();
        session
            .reset(reset_time)
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
    let max_advance_dt = session.max_runner_advance_dt().unwrap_or(dt);
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
        .max_runner_advance_dt()
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
mod tests {
    use super::*;

    fn browser_key(code: &str, key: &str) -> ViewerKeyCommand {
        ViewerKeyCommand {
            code: code.to_string(),
            key: Some(key.to_string()),
            pressed: true,
            shift: false,
            ctrl: false,
            alt: false,
        }
    }

    #[test]
    fn browser_arrow_key_maps_to_keyboard_event() {
        let event = browser_key_to_event(&browser_key("ArrowUp", "ArrowUp")).unwrap();
        assert_eq!(event.code, KeyCode::Up);
        assert_eq!(event.modifiers, KeyModifiers::NONE);
    }

    #[test]
    fn browser_letter_key_maps_to_lowercase_keyboard_event() {
        let event = browser_key_to_event(&browser_key("KeyW", "W")).unwrap();
        assert_eq!(event.code, KeyCode::Char('w'));
        assert_eq!(event.modifiers, KeyModifiers::NONE);
    }

    #[test]
    fn browser_space_key_maps_to_space_keyboard_event() {
        let event = browser_key_to_event(&browser_key("Space", " ")).unwrap();
        assert_eq!(event.code, KeyCode::Char(' '));
    }

    #[test]
    fn viewer_input_drain_preserves_keys_before_quit() {
        let (tx, rx) = mpsc::channel();
        tx.send(
            r#"{"key":{"code":"Space","key":" ","shift":false,"ctrl":false,"alt":false}}"#
                .to_string(),
        )
        .unwrap();
        tx.send(r#"{"quit":true}"#.to_string()).unwrap();

        let drained = drain_viewer_input(&rx, None, false);
        assert!(drained.quit);
        assert_eq!(drained.keys.len(), 1);
        assert_eq!(drained.labels, ["Space"]);
        assert_eq!(drained.keys[0].code, KeyCode::Char(' '));
    }

    #[test]
    fn viewer_input_drain_preserves_key_release() {
        let (tx, rx) = mpsc::channel();
        tx.send(
            r#"{"key":{"code":"ArrowUp","key":"ArrowUp","pressed":false,"shift":false,"ctrl":false,"alt":false}}"#
                .to_string(),
        )
        .unwrap();

        let drained = drain_viewer_input(&rx, None, false);
        assert_eq!(drained.keys.len(), 1);
        assert_eq!(drained.labels, ["ArrowUp up"]);
        assert_eq!(drained.keys[0].code, KeyCode::Up);
        assert!(!drained.keys[0].pressed);
    }
}
