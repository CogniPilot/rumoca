//! Lockstep simulation loop driven entirely by the TOML config.
//!
//! Per-frame orchestration (transports live in sibling crates):
//!   1. poll input engine (config-driven gamepad/keyboard)
//!   2. drain incoming UDP, apply unpacked values to stepper / locals
//!   3. step physics
//!   4. build outgoing `SignalFrame` via signal mapper
//!   5. pack + send UDP
//!   6. build viewer JSON via signal mapper
//!   7. push to WebSocket
//!   8. realtime pacing

use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result};
use rumoca_codec_flatbuffers::bfbs::SchemaSet;
use rumoca_codec_flatbuffers::codec::{PackCodec, UnpackCodec};
use rumoca_solver_diffsol::{SimStepper, StepperOptions};
use rumoca_transport_udp::{UdpConfig, UdpTransport};
use rumoca_transport_websocket::run_broadcast_server;

use rumoca_session::config::{ResetConfig, SimMode, SimulationConfig};
use rumoca_input::{InputEngine, RuntimeContext, SignalMapper};

const MAX_SUB_DT: f64 = 0.002;

// ── Autopilot subprocess ───────────────────────────────────────────────────

struct AutopilotProcess {
    child: Option<Child>,
    command: String,
}

impl AutopilotProcess {
    fn new(command: &str) -> Self {
        Self {
            child: None,
            command: command.to_string(),
        }
    }

    fn start(&mut self) -> Result<()> {
        self.stop();
        eprintln!("[autopilot] starting: {}", self.command);
        let mut cmd = Command::new(&self.command);
        cmd.stdin(Stdio::null());
        // `RUMOCA_AUTOPILOT_LOG=1` lets the child's stdout/stderr through
        // to this terminal — handy for debugging Cerebri boot issues.
        if std::env::var("RUMOCA_AUTOPILOT_LOG").is_ok() {
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
            .with_context(|| format!("Failed to start autopilot: {}", self.command))?;
        eprintln!("[autopilot] pid {}", child.id());
        self.child = Some(child);
        Ok(())
    }

    fn stop(&mut self) {
        let Some(mut child) = self.child.take() else {
            return;
        };
        let pid = child.id();
        eprintln!("[autopilot] killing pid {pid}");
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
        eprintln!("[autopilot] pid {pid} did not exit within 500ms; abandoning");
    }
}

impl Drop for AutopilotProcess {
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
// Activated by `RUMOCA_TRACE_LOG=/path/to/trace.csv` when the config has a
// `[debug_log]` section. Fields come from `debug_log.capture`, evaluated
// each frame. Writes through a BufWriter and flushes on Drop. Designed
// for offline plotting — load into a notebook with pandas.read_csv.

struct TraceLogger {
    writer: BufWriter<File>,
    fields: Vec<String>,
    path: PathBuf,
}

impl TraceLogger {
    fn open(path: PathBuf, fields: Vec<String>) -> Result<Self> {
        let file = File::create(&path)
            .with_context(|| format!("Open trace log {}", path.display()))?;
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

    fn record(&mut self, engine: &InputEngine, rt: &RuntimeContext<'_>) {
        let mut first = true;
        for name in &self.fields {
            if !first {
                let _ = self.writer.write_all(b",");
            }
            first = false;
            let v = resolve_trace_field(name, engine, rt);
            let _ = write!(self.writer, "{v}");
        }
        let _ = self.writer.write_all(b"\n");
    }
}

fn open_trace_logger(cfg: &SimulationConfig) -> Result<Option<TraceLogger>> {
    let Some(dbg) = cfg.debug_log.as_ref() else {
        return Ok(None);
    };
    // Default: drop `rumoca_trace.csv` in the cwd so you always have a log
    // to share with no setup. Override with RUMOCA_TRACE_LOG=/path/other.csv.
    let path = std::env::var("RUMOCA_TRACE_LOG")
        .unwrap_or_else(|_| "rumoca_trace.csv".to_string());
    let logger = TraceLogger::open(PathBuf::from(path), dbg.capture.clone())?;
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
/// values log as `nan` rather than failing the row.
fn resolve_trace_field(name: &str, engine: &InputEngine, rt: &RuntimeContext<'_>) -> f64 {
    if let Some(rest) = name.strip_prefix("stepper:") {
        if rest == "time" {
            return rt.stepper_time;
        }
        return (rt.stepper_get)(rest).unwrap_or(f64::NAN);
    }
    if let Some(rest) = name.strip_prefix("local:") {
        return engine.get(rest).unwrap_or(f64::NAN);
    }
    if let Some(rest) = name.strip_prefix("runtime:") {
        return match rest {
            "frame_num" => rt.frame_num as f64,
            "wall_ms" => rt.wall_ms,
            "input_connected" => f64::from(u8::from(rt.input_connected)),
            "stepper_time" => rt.stepper_time,
            _ => f64::NAN,
        };
    }
    f64::NAN
}

// ── UDP config resolution (bridge legacy [udp] + new [transport.udp]) ──────

fn resolve_udp(cfg: &SimulationConfig) -> Option<&UdpConfig> {
    cfg.transport
        .as_ref()
        .and_then(|t| t.udp.as_ref())
        .or(cfg.udp.as_ref())
}

// ── Main loop ──────────────────────────────────────────────────────────────

/// Bundle of per-frame FB transport state. Present only when `[schema]` +
/// `[receive]` + `[send]` are configured (autopilot coupling). Absent in
/// standalone mode (e.g. rover demo).
struct FbTransport {
    udp: UdpTransport,
    pack: PackCodec,
    unpack: UnpackCodec,
    recv_expected: usize,
}

/// Immutable per-frame context: FB transport (if any), mapper, and channels.
struct FrameCtx<'a> {
    cfg: &'a SimulationConfig,
    fb: Option<&'a FbTransport>,
    mapper: &'a SignalMapper,
    state_tx: &'a mpsc::Sender<String>,
    realtime: &'a Arc<AtomicBool>,
    quit: &'a Arc<AtomicBool>,
    autopilot: &'a Arc<Mutex<Option<AutopilotProcess>>>,
    model_source: &'a str,
    model_name: &'a str,
    debug: bool,
    dt: f64,
    mode: SimMode,
}

/// Mutable per-frame state that carries across iterations.
struct FrameState {
    recv_buf: [u8; 512],
    pkt_count: u64,
    frame_num: u64,
    last_poll: Instant,
    trace: Option<TraceLogger>,
}

enum FrameControl {
    Continue,
    Break,
}

/// Run the lockstep simulation app. Blocks the calling thread. In standalone
/// mode (no `[schema]`/`[receive]`/`[send]` in config) the UDP socket and
/// FB codecs are not created and no autopilot coupling happens.
pub(crate) fn run_sim_loop(
    cfg: &SimulationConfig,
    schema_set: Option<&SchemaSet>,
    stepper: &mut SimStepper,
    model_source: &str,
    model_name: &str,
    ws_port: u16,
    debug: bool,
) -> Result<()> {
    // ── Signal handler FIRST, before any other thread or child spawns. ───
    // signal_hook masks the target signals on threads spawned after it, so
    // installing it ahead of the input engine / WS thread / autopilot child
    // ensures SIGINT/SIGTERM funnel to our dedicated signal thread — not a
    // gilrs worker, not zephyr's pgrp.
    let autopilot: Arc<Mutex<Option<AutopilotProcess>>> = Arc::new(Mutex::new(None));
    spawn_sigint_handler(Arc::clone(&autopilot));

    let fb = setup_fb_transport(cfg, schema_set)?;

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
    let mapper = SignalMapper::new(signals_cfg, &cfg.locals).context("Compile signal mapper")?;

    // ── Autopilot + WS thread ─────────────────────────────────────────────
    start_autopilot_into(cfg, &autopilot)?;
    let (state_tx, state_rx) = mpsc::channel::<String>();
    let realtime = Arc::new(AtomicBool::new(cfg.sim.realtime));
    let quit = Arc::new(AtomicBool::new(false));
    let realtime_ws = Arc::clone(&realtime);
    let quit_ws = Arc::clone(&quit);
    thread::spawn(move || run_broadcast_server(ws_port, state_rx, realtime_ws, quit_ws));

    let mode = SimMode::resolve(cfg.sim.mode, cfg.has_fb());
    eprintln!(
        "  Pacing: {}",
        match mode {
            SimMode::Lockstep => "lockstep (autopilot-paced)",
            SimMode::FreeRun => "free-run (wall-clock paced)",
        }
    );
    eprintln!("\nReady. Simulation running.");

    // ── Loop ──────────────────────────────────────────────────────────────
    let ctx = FrameCtx {
        cfg,
        fb: fb.as_ref(),
        mapper: &mapper,
        state_tx: &state_tx,
        realtime: &realtime,
        quit: &quit,
        autopilot: &autopilot,
        model_source,
        model_name,
        debug,
        dt: cfg.sim.dt,
        mode,
    };
    let trace = open_trace_logger(cfg)?;
    let mut state = FrameState {
        recv_buf: [0u8; 512],
        pkt_count: 0,
        frame_num: 0,
        last_poll: Instant::now(),
        trace,
    };
    while let FrameControl::Continue =
        ctx.run_one_frame(&mut state, stepper, &mut engine)?
    {}

    // Explicit stop: the signal-handler thread still holds an Arc clone of
    // `autopilot`, so Drop would not fire on normal exit and zephyr would
    // orphan. Kill the child here, deterministically.
    if let Ok(mut ap) = autopilot.lock()
        && let Some(proc) = ap.as_mut()
    {
        proc.stop();
    }
    Ok(())
}

fn setup_fb_transport(
    cfg: &SimulationConfig,
    schema_set: Option<&SchemaSet>,
) -> Result<Option<FbTransport>> {
    if !cfg.has_fb() {
        eprintln!("  Mode: standalone (no UDP/codec)");
        return Ok(None);
    }
    let schema_set = schema_set.context("FB config present but schema_set not loaded")?;
    let send_cfg = cfg.send.as_ref().unwrap(); // validated by has_fb
    let recv_cfg = cfg.receive.as_ref().unwrap();
    let pack = PackCodec::compile(schema_set, send_cfg).context("Build pack codec")?;
    let unpack = UnpackCodec::compile(schema_set, recv_cfg).context("Build unpack codec")?;
    let recv_expected = unpack.expected_size();
    let udp_cfg = resolve_udp(cfg)
        .context("FB config present but no [transport.udp] or [udp] section")?;
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
        stepper: &mut SimStepper,
        engine: &mut InputEngine,
    ) -> Result<FrameControl> {
        let frame_start = Instant::now();

        // Poll input + handle engine-emitted signals (shared).
        let poll_dt = state.last_poll.elapsed().as_secs_f64();
        state.last_poll = Instant::now();
        engine.poll(poll_dt);
        if let FrameControl::Break = self.handle_signals(engine, stepper)? {
            return Ok(FrameControl::Break);
        }

        // Mode-specific receive + step gate.
        //   free_run: drain non-blocking, always step
        //   lockstep: block for one packet; if timeout, skip step entirely
        match self.mode {
            SimMode::FreeRun => {
                self.drain_udp(state, stepper, engine);
            }
            SimMode::Lockstep => {
                if !self.wait_for_command(state, stepper, engine) {
                    // No command arrived within socket timeout — try again,
                    // physics stays paused (lockstep semantics).
                    return Ok(FrameControl::Continue);
                }
            }
        }
        step_substeps(stepper, self.dt);

        self.emit_payloads(state, stepper, engine)?;

        // Status line (~1 Hz) — in lockstep the period is approximate since
        // frame rate depends on autopilot pacing.
        let status_period = (1.0_f64 / self.dt).max(1.0) as u64;
        if state.frame_num.is_multiple_of(status_period) {
            eprint!(
                "\r[sim] t={:.1}s frame={} pkts={}            ",
                stepper.time(),
                state.frame_num,
                state.pkt_count
            );
        }

        // Realtime pacing: free-run only. Lockstep is paced by the autopilot,
        // so wall-clock sleep would starve physics of commands.
        if matches!(self.mode, SimMode::FreeRun) && self.realtime.load(Ordering::Relaxed) {
            let elapsed = frame_start.elapsed();
            let target = Duration::from_secs_f64(self.dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }
        state.frame_num += 1;
        Ok(FrameControl::Continue)
    }

    /// Build payloads + apply stepper_inputs + send FB + push viewer JSON.
    /// Shared by both free-run and lockstep paths.
    fn emit_payloads(
        &self,
        state: &mut FrameState,
        stepper: &mut SimStepper,
        engine: &mut InputEngine,
    ) -> Result<()> {
        let wall_ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as f64;
        let (stepper_inputs, send_frame, json) = {
            let stepper_time = stepper.time();
            let stepper_get = |name: &str| stepper.get(name);
            let rt = RuntimeContext {
                frame_num: state.frame_num,
                wall_ms,
                input_connected: engine.is_connected(),
                input_mode: engine.mode(),
                stepper_time,
                stepper_get: &stepper_get,
            };
            let stepper_inputs = self.mapper.build_stepper_inputs(engine, &rt);
            let send_frame = self.fb.map(|_| self.mapper.build_send(engine, &rt));
            let json = self.mapper.build_viewer_json(engine, &rt);
            if let Some(trace) = state.trace.as_mut() {
                trace.record(engine, &rt);
            }
            (stepper_inputs, send_frame, json)
        };
        for (name, val) in stepper_inputs {
            let _ = stepper.set_input(&name, val);
        }
        if let (Some(fb), Some(frame)) = (self.fb, send_frame) {
            fb.udp.send(&fb.pack.pack(&frame));
        }
        let _ = self.state_tx.send(json);
        Ok(())
    }

    /// Lockstep receive: block for one packet, apply to stepper/locals.
    /// Returns `true` if a packet was consumed, `false` on timeout.
    fn wait_for_command(
        &self,
        state: &mut FrameState,
        stepper: &mut SimStepper,
        engine: &mut InputEngine,
    ) -> bool {
        let Some(fb) = self.fb else {
            // No FB transport configured — caller shouldn't use lockstep
            // mode in standalone; treat as "packet arrived" so we step.
            return true;
        };
        let Some(n) = fb.udp.recv_blocking(&mut state.recv_buf) else {
            return false;
        };
        state.pkt_count += 1;
        if n == fb.recv_expected {
            let values = fb.unpack.unpack(&state.recv_buf[..n]);
            apply_received(&values, stepper, engine);
        }
        true
    }

    fn handle_signals(
        &self,
        engine: &mut InputEngine,
        stepper: &mut SimStepper,
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
                self.model_source,
                self.model_name,
                self.autopilot,
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
        stepper: &mut SimStepper,
        engine: &mut InputEngine,
    ) {
        let Some(fb) = self.fb else {
            return;
        };
        let expected = fb.recv_expected;
        fb.udp.drain(&mut state.recv_buf, |datagram| {
            state.pkt_count += 1;
            if datagram.len() == expected {
                let values = fb.unpack.unpack(datagram);
                apply_received(&values, stepper, engine);
            }
        });
    }
}

fn start_autopilot_into(
    cfg: &SimulationConfig,
    autopilot: &Arc<Mutex<Option<AutopilotProcess>>>,
) -> Result<()> {
    if let Some(ap_cfg) = &cfg.autopilot {
        let mut ap = AutopilotProcess::new(&ap_cfg.command);
        ap.start()?;
        *autopilot.lock().unwrap() = Some(ap);
    }
    Ok(())
}

/// Set up a robust shutdown path for SIGINT/SIGTERM:
/// - 1st signal: clean shutdown (kill autopilot with timeout, disable raw
///   mode, exit 0)
/// - 2nd signal: hard exit 130 (skip cleanup)
///
/// Uses `signal_hook::iterator::Signals` — a blocking iterator that wakes
/// exactly when a signal arrives. No polling, no race windows.
fn spawn_sigint_handler(autopilot: Arc<Mutex<Option<AutopilotProcess>>>) {
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
                spawn_cleanup_thread(Arc::clone(&autopilot));
            } else {
                eprintln!("[sim] force quit");
                crossterm::terminal::disable_raw_mode().ok();
                std::process::exit(130);
            }
        }
    });
}

fn spawn_cleanup_thread(autopilot: Arc<Mutex<Option<AutopilotProcess>>>) {
    thread::spawn(move || {
        if let Ok(mut ap) = autopilot.lock()
            && let Some(proc) = ap.as_mut()
        {
            proc.stop();
        }
        crossterm::terminal::disable_raw_mode().ok();
        std::process::exit(0);
    });
}

// ── Helpers ────────────────────────────────────────────────────────────────

/// Apply a received SignalFrame to the stepper or locals based on the key
/// prefix. Keys like `"stepper:omega_m1"` are applied to the stepper; keys
/// like `"local:armed"` go to the engine's locals; bare names default to
/// the stepper for convenience.
fn apply_received(
    values: &rumoca_codec::SignalFrame,
    stepper: &mut SimStepper,
    engine: &mut InputEngine,
) {
    for (key, val) in values.iter() {
        if let Some(rest) = key.strip_prefix("stepper:") {
            let _ = stepper.set_input(rest, val);
        } else if let Some(rest) = key.strip_prefix("local:") {
            engine.set_local(rest, val);
        } else {
            let _ = stepper.set_input(key, val);
        }
    }
}

fn handle_reset(
    reset_cfg: &ResetConfig,
    engine: &mut InputEngine,
    stepper: &mut SimStepper,
    model_source: &str,
    model_name: &str,
    ap_handle: &Arc<Mutex<Option<AutopilotProcess>>>,
) -> Result<()> {
    eprintln!("\n[reset] triggered");
    if reset_cfg.reset_locals {
        engine.reset();
    }
    if reset_cfg.restart_autopilot
        && let Ok(mut ap) = ap_handle.lock()
        && let Some(proc) = ap.as_mut()
        && let Err(e) = proc.start()
    {
        eprintln!("[reset] autopilot restart failed: {e}");
    }
    if reset_cfg.rebuild_stepper {
        let mut session = rumoca_session::compile::Session::default();
        session
            .add_document(&format!("{model_name}.mo"), model_source)
            .map_err(|e| anyhow::anyhow!("reset: parse failed: {e}"))?;
        let result = session
            .compile_model(model_name)
            .context("reset: compilation failed")?;
        let new_stepper = SimStepper::new(
            &result.dae,
            StepperOptions {
                rtol: 1e-3,
                atol: 1e-3,
                ..Default::default()
            },
        )
        .context("reset: stepper creation failed")?;
        *stepper = new_stepper;
        eprintln!("[reset] stepper rebuilt");
    }
    Ok(())
}

// ── Step helper ────────────────────────────────────────────────────────────

fn step_substeps(stepper: &mut SimStepper, dt: f64) {
    let target = stepper.time() + dt;
    let step_dt = target - stepper.time();
    if step_dt <= 0.0 {
        return;
    }
    let n_steps = ((step_dt / MAX_SUB_DT).ceil() as usize).max(1);
    let sub_dt = step_dt / n_steps as f64;
    for i in 0..n_steps {
        if let Err(e) = stepper.step(sub_dt) {
            eprintln!(
                "\r[sim] step {}/{n_steps} failed (sub_dt={sub_dt:.4}): {e}",
                i + 1,
            );
        }
    }
}

// WebSocket server lives in rumoca-transport-websocket.
// HTTP viewer server lives in rumoca-viz-web.
