//! Interactive simulation loop driven entirely by the TOML config.
//!
//! Per-frame orchestration (transports live in sibling crates):
//!   1. poll input engine (config-driven gamepad/keyboard)
//!   2. drain incoming UDP, apply unpacked values to stepper / locals
//!   3. step physics
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

use crate::{InteractiveStepper, SimPacingMode};
use anyhow::{Context, Result};
use rumoca_codec::{PackCodec, UnpackCodec};
use rumoca_input::{
    InputEngine, KeyCode, KeyModifiers, KeyboardEvent, RuntimeContext, SignalMapper,
};
use rumoca_transport_udp::{UdpConfig, UdpTransport};
use rumoca_transport_websocket::run_broadcast_server;
use serde::Deserialize;

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
/// scheme as signal mapper: `stepper:`, `local:` (supports `.idx`),
/// `runtime:frame_num|wall_ms|input_connected|stepper_time`. Missing
fn resolve_trace_field(name: &str, engine: &InputEngine, rt: &RuntimeContext<'_>) -> Result<f64> {
    if let Some(rest) = name.strip_prefix("stepper:") {
        if rest == "time" {
            return Ok(rt.stepper_time);
        }
        return (rt.stepper_get)(rest)?
            .ok_or_else(|| anyhow::anyhow!("trace field stepper:{rest} did not resolve"));
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
            "stepper_time" => Ok(rt.stepper_time),
            _ => Err(anyhow::anyhow!("unknown trace runtime field '{rest}'")),
        };
    }
    Err(anyhow::anyhow!(
        "trace field '{name}' must use stepper:, local:, or runtime:"
    ))
}

// ── UDP config resolution ───────────────────────────────────────────────────

fn resolve_udp(cfg: &SimulationConfig) -> Option<&UdpConfig> {
    cfg.transport.as_ref().and_then(|t| t.udp.as_ref())
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
    udp: UdpTransport,
    pack: Box<dyn PackCodec>,
    unpack: Box<dyn UnpackCodec>,
    recv_expected: usize,
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
    mode: SimPacingMode,
    steps_per_packet: usize,
    lockstep_schedule: Option<LockstepSchedule>,
}

/// Mutable per-frame state that carries across iterations.
struct FrameState {
    recv_buf: [u8; 512],
    pkt_count: u64,
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
    max_step_dt: f64,
}

impl LockstepSchedule {
    fn from_config(lockstep: &LockstepConfig, sim_dt: f64) -> Self {
        Self {
            send_dt: 1.0 / lockstep.send_rate_hz,
            receive_dt: 1.0 / lockstep.receive_rate_hz,
            max_step_dt: lockstep.max_step_dt.unwrap_or(sim_dt),
        }
    }
}

pub struct SimLoopArgs<'a> {
    pub cfg: &'a SimulationConfig,
    pub http_port: u16,
    pub ws_port: u16,
    pub debug: bool,
}

struct StepperFrameSnapshot {
    values: Option<indexmap::IndexMap<String, f64>>,
}

impl StepperFrameSnapshot {
    fn new(stepper: &impl InteractiveStepper, names: &[String]) -> Result<Self> {
        Ok(Self {
            values: stepper.values_for(names)?,
        })
    }

    fn get(&self, stepper: &impl InteractiveStepper, name: &str) -> Result<Option<f64>> {
        if let Some(value) = self
            .values
            .as_ref()
            .and_then(|values| values.get(name).copied())
        {
            return Ok(Some(value));
        }
        stepper.get(name).map_err(Into::into)
    }
}

enum FrameControl {
    Continue,
    Break,
}

/// Run the interactive simulation app. Blocks the calling thread. In standalone
/// mode (no `[schema]`/`[receive]`/`[send]` in config) the UDP socket and
/// codecs are not created and no external-interface coupling happens.
pub fn run_sim_loop<S>(stepper: &mut S, args: SimLoopArgs<'_>) -> Result<()>
where
    S: InteractiveStepper,
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
            "  Lockstep send: {:.3} Hz, receive barrier: {:.3} Hz, max step dt: {} s",
            1.0 / schedule.send_dt,
            1.0 / schedule.receive_dt,
            schedule.max_step_dt
        ));
    } else if matches!(mode, SimPacingMode::Lockstep) && steps_per_packet > 1 {
        status_line(&format!(
            "  Lockstep steps/packet: {steps_per_packet} ({} s simulated per packet)",
            cfg.sim.dt * steps_per_packet as f64
        ));
    }
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
        mode,
        steps_per_packet,
        lockstep_schedule,
    };
    let trace = open_trace_logger(cfg)?;
    let mut state = FrameState {
        recv_buf: [0u8; 512],
        pkt_count: 0,
        frame_num: 0,
        last_poll: Instant::now(),
        trace,
        lockstep_schedule_initialized: false,
        next_lockstep_send_time: 0.0,
        next_lockstep_control_time: 0.0,
    };
    while let FrameControl::Continue =
        ctx.run_one_frame(&mut state, stepper, &mut engine, &mut input_runtime)?
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
    let udp_cfg = resolve_udp(cfg).context("FB config present but no [transport.udp] section")?;
    eprintln!("  UDP listen: {}", udp_cfg.listen);
    eprintln!("  UDP send:   {}", udp_cfg.send);
    eprintln!("  Expecting {recv_expected}-byte receive packets");
    let udp = UdpTransport::bind(udp_cfg)?;
    Ok(Some(FbTransport {
        udp,
        pack,
        unpack,
        recv_expected,
    }))
}

impl FrameCtx<'_> {
    fn run_one_frame(
        &self,
        state: &mut FrameState,
        stepper: &mut impl InteractiveStepper,
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

        if matches!(self.mode, SimPacingMode::Lockstep)
            && let Some(schedule) = self.lockstep_schedule
        {
            return self.run_scheduled_lockstep_frame(
                schedule,
                state,
                stepper,
                engine,
                input_runtime,
                viewer_input,
            );
        }

        // Pacing-specific receive + step gate.
        match self.mode {
            SimPacingMode::AsFastAsPossible | SimPacingMode::Realtime => {
                self.drain_udp(state, stepper, engine)?;
            }
            SimPacingMode::Lockstep => {
                let transport_packet = self.wait_for_command(state, stepper, engine)?;
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
        let poll_dt = if matches!(self.mode, SimPacingMode::Lockstep | SimPacingMode::Realtime) {
            self.dt * steps_this_frame as f64
        } else {
            let elapsed = state.last_poll.elapsed().as_secs_f64();
            state.last_poll = Instant::now();
            elapsed
        };
        input_runtime.poll_with_keyboard_events(engine, poll_dt, viewer_input.keys);
        if let FrameControl::Break = self.handle_signals(engine, stepper)? {
            return Ok(FrameControl::Break);
        }
        self.apply_stepper_inputs(state, stepper, engine, input_runtime)?;
        for _ in 0..steps_this_frame {
            step_substeps(stepper, self.dt)?;
        }

        self.emit_payloads(state, stepper, engine, input_runtime)?;

        self.emit_status(state, stepper);

        // Realtime pacing is an explicit mode. Lockstep is paced by input
        // arrival, and as-fast-as-possible intentionally never sleeps here.
        if matches!(self.mode, SimPacingMode::Realtime) && self.realtime.load(Ordering::Relaxed) {
            let elapsed = frame_start.elapsed();
            let target = Duration::from_secs_f64(self.dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }
        state.frame_num += steps_this_frame as u64;
        Ok(FrameControl::Continue)
    }

    fn run_scheduled_lockstep_frame(
        &self,
        schedule: LockstepSchedule,
        state: &mut FrameState,
        stepper: &mut impl InteractiveStepper,
        engine: &mut InputEngine,
        input_runtime: &mut Devices,
        viewer_input: ViewerInputDrain,
    ) -> Result<FrameControl> {
        // Slack for comparing accumulated send/control times against each other:
        // the schedule advances `next_lockstep_send_time` by `send_dt` each send,
        // so exact `==` would be defeated by float rounding. 1 ns is far below any
        // realistic sim dt yet large enough to absorb that accumulation drift.
        const EPS: f64 = 1e-9;

        let now = stepper.time();
        if !state.lockstep_schedule_initialized {
            state.lockstep_schedule_initialized = true;
            state.next_lockstep_control_time = now + schedule.receive_dt;
            state.next_lockstep_send_time = now;
        }

        let control_time = state.next_lockstep_control_time;
        let poll_dt = (control_time - now).max(0.0);
        input_runtime.poll_with_keyboard_events(engine, poll_dt, viewer_input.keys);
        if let FrameControl::Break = self.handle_signals(engine, stepper)? {
            return Ok(FrameControl::Break);
        }
        self.apply_stepper_inputs(state, stepper, engine, input_runtime)?;

        while state.next_lockstep_send_time <= control_time + EPS {
            let target = state.next_lockstep_send_time;
            let step_dt = target - stepper.time();
            if step_dt > EPS {
                step_with_max_dt(stepper, step_dt, schedule.max_step_dt)?;
            }
            self.emit_payloads(state, stepper, engine, input_runtime)?;
            // In this scheduled path `frame_num` counts *sends* (one per emitted
            // payload), whereas the packet-paced path counts internal solver
            // steps. The viewer only needs a monotonic frame counter, so the
            // differing units are intentional.
            state.frame_num += 1;
            self.emit_status(state, stepper);
            state.next_lockstep_send_time += schedule.send_dt;
        }

        let remaining_dt = control_time - stepper.time();
        if remaining_dt > EPS {
            step_with_max_dt(stepper, remaining_dt, schedule.max_step_dt)?;
        }

        let transport_packet = self.wait_for_command(state, stepper, engine)?;
        if transport_packet {
            state.next_lockstep_control_time += schedule.receive_dt;
        }
        Ok(FrameControl::Continue)
    }

    /// Apply configured local/runtime signal routes into model inputs before stepping.
    fn apply_stepper_inputs(
        &self,
        state: &FrameState,
        stepper: &mut impl InteractiveStepper,
        engine: &mut InputEngine,
        input_runtime: &Devices,
    ) -> Result<()> {
        let wall_ms = wall_ms_since_unix_epoch()?;
        let stepper_time = stepper.time();
        let stepper_get = |name: &str| stepper.get(name).map_err(Into::into);
        let rt = RuntimeContext {
            frame_num: state.frame_num,
            wall_ms,
            input_connected: input_runtime.is_connected(),
            input_mode: input_runtime.mode(),
            input_message: engine.last_message(),
            stepper_time,
            stepper_get: &stepper_get,
        };
        for (name, val) in self.mapper.build_stepper_inputs(engine, &rt)? {
            stepper
                .set_input(&name, val)
                .with_context(|| format!("set stepper input '{name}'"))?;
        }
        Ok(())
    }

    /// Build payloads + send FB + push viewer JSON.
    /// Shared by all pacing paths.
    fn emit_payloads(
        &self,
        state: &mut FrameState,
        stepper: &mut impl InteractiveStepper,
        engine: &mut InputEngine,
        input_runtime: &Devices,
    ) -> Result<()> {
        let wall_ms = wall_ms_since_unix_epoch()?;
        let (send_frame, json) = {
            let stepper_time = stepper.time();
            let snapshot = StepperFrameSnapshot::new(stepper, self.mapper.stepper_lookup_names())?;
            let stepper_get = |name: &str| snapshot.get(stepper, name);
            let rt = RuntimeContext {
                frame_num: state.frame_num,
                wall_ms,
                input_connected: input_runtime.is_connected(),
                input_mode: input_runtime.mode(),
                input_message: engine.last_message(),
                stepper_time,
                stepper_get: &stepper_get,
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
            fb.udp.send(&fb.pack.pack(&frame));
        }
        let _ = self.state_tx.send(json);
        Ok(())
    }

    fn emit_status(&self, state: &FrameState, stepper: &impl InteractiveStepper) {
        // Status line (~1 Hz). In lockstep the period is approximate because
        // frame rate depends on external input pacing.
        let status_period = (1.0_f64 / self.dt).max(1.0) as u64;
        if state.frame_num.is_multiple_of(status_period) {
            eprint!(
                "\r[sim] t={:.1}s frame={} pkts={}            ",
                stepper.time(),
                state.frame_num,
                state.pkt_count
            );
        }
    }

    /// Lockstep receive: consume one transport packet, apply to stepper/locals.
    /// Returns `true` if a packet was consumed, `false` on timeout.
    fn wait_for_command(
        &self,
        state: &mut FrameState,
        stepper: &mut impl InteractiveStepper,
        engine: &mut InputEngine,
    ) -> Result<bool> {
        let Some(fb) = self.fb else {
            return Ok(false);
        };
        let Some(n) = fb.udp.recv_blocking(&mut state.recv_buf) else {
            return Ok(false);
        };
        state.pkt_count += 1;
        apply_fb_datagram(fb, &state.recv_buf[..n], stepper, engine)?;
        Ok(true)
    }

    fn handle_signals(
        &self,
        engine: &mut InputEngine,
        stepper: &mut impl InteractiveStepper,
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
                stepper,
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
        stepper: &mut impl InteractiveStepper,
        engine: &mut InputEngine,
    ) -> Result<()> {
        let Some(fb) = self.fb else {
            return Ok(());
        };
        let expected = fb.recv_expected;
        let mut error = None;
        fb.udp.drain(&mut state.recv_buf, |datagram| {
            state.pkt_count += 1;
            if datagram.len() != expected || error.is_some() {
                return;
            }
            error = apply_fb_datagram(fb, datagram, stepper, engine).err();
        });
        if let Some(error) = error {
            return Err(error);
        }
        Ok(())
    }
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
    stepper: &mut impl InteractiveStepper,
    engine: &mut InputEngine,
) -> Result<()> {
    if datagram.len() != fb.recv_expected {
        return Ok(());
    }
    let values = fb.unpack.unpack(datagram);
    apply_received(&values, stepper, engine)
}

/// Apply a received SignalFrame to the stepper or locals based on the key
/// prefix. Keys like `"stepper:omega_m1"` are applied to the stepper; keys
/// like `"local:armed"` go to the engine's locals; bare names default to
/// the stepper for convenience.
fn apply_received(
    values: &rumoca_codec::SignalFrame,
    stepper: &mut impl InteractiveStepper,
    engine: &mut InputEngine,
) -> Result<()> {
    for (key, val) in values.iter() {
        if let Some(rest) = key.strip_prefix("stepper:") {
            stepper
                .set_input(rest, val)
                .with_context(|| format!("set received stepper input '{rest}'"))?;
        } else if let Some(rest) = key.strip_prefix("local:") {
            engine.set_local(rest, val);
        } else {
            stepper
                .set_input(key, val)
                .with_context(|| format!("set received stepper input '{key}'"))?;
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
    stepper: &mut S,
    runtime: ResetRuntime<'_>,
) -> Result<()>
where
    S: InteractiveStepper,
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
    if reset_cfg.rebuild_stepper {
        let reset_time = stepper.time();
        stepper
            .reset(reset_time)
            .context("reset: stepper reset failed")?;
        eprintln!("[reset] stepper reset");
    }
    Ok(())
}

// ── Step helper ────────────────────────────────────────────────────────────

fn step_substeps(stepper: &mut impl InteractiveStepper, dt: f64) -> Result<()> {
    let max_step_dt = stepper.max_runner_step_dt().unwrap_or(dt);
    step_with_max_dt(stepper, dt, max_step_dt)
}

fn step_with_max_dt(
    stepper: &mut impl InteractiveStepper,
    dt: f64,
    max_step_dt: f64,
) -> Result<()> {
    let target = stepper.time() + dt;
    let step_dt = target - stepper.time();
    if step_dt <= 0.0 {
        return Ok(());
    }
    let max_sub_dt = stepper
        .max_runner_step_dt()
        .map(|stepper_dt| stepper_dt.min(max_step_dt))
        .unwrap_or(max_step_dt)
        .min(step_dt);
    let n_steps = ((step_dt / max_sub_dt).ceil() as usize).max(1);
    let sub_dt = step_dt / n_steps as f64;
    for i in 0..n_steps {
        if let Err(e) = stepper.step(sub_dt) {
            eprintln!(
                "\r[sim] step {}/{n_steps} failed (sub_dt={sub_dt:.4}): {e}",
                i + 1,
            );
            return Err(anyhow::anyhow!(
                "simulation step {}/{n_steps} failed at t={:.9}: {e}",
                i + 1,
                stepper.time()
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
