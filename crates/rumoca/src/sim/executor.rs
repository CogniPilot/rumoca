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
        let child = Command::new(&self.command)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .with_context(|| format!("Failed to start autopilot: {}", self.command))?;
        eprintln!("[autopilot] pid {}", child.id());
        self.child = Some(child);
        Ok(())
    }

    fn stop(&mut self) {
        if let Some(mut child) = self.child.take() {
            let pid = child.id();
            eprintln!("[autopilot] killing pid {pid}");
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

impl Drop for AutopilotProcess {
    fn drop(&mut self) {
        self.stop();
    }
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

    // ── Autopilot + SIGINT + WS thread ────────────────────────────────────
    let autopilot = spawn_autopilot(cfg)?;
    spawn_sigint_handler(Arc::clone(&autopilot));
    let (state_tx, state_rx) = mpsc::channel::<String>();
    let realtime = Arc::new(AtomicBool::new(cfg.sim.realtime));
    let realtime_ws = Arc::clone(&realtime);
    thread::spawn(move || run_broadcast_server(ws_port, state_rx, realtime_ws));

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
        autopilot: &autopilot,
        model_source,
        model_name,
        debug,
        dt: cfg.sim.dt,
        mode,
    };
    let mut state = FrameState {
        recv_buf: [0u8; 512],
        pkt_count: 0,
        frame_num: 0,
        last_poll: Instant::now(),
    };
    while let FrameControl::Continue =
        ctx.run_one_frame(&mut state, stepper, &mut engine)?
    {}
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

fn spawn_autopilot(cfg: &SimulationConfig) -> Result<Arc<Mutex<Option<AutopilotProcess>>>> {
    let autopilot: Arc<Mutex<Option<AutopilotProcess>>> = Arc::new(Mutex::new(None));
    if let Some(ap_cfg) = &cfg.autopilot {
        let mut ap = AutopilotProcess::new(&ap_cfg.command);
        ap.start()?;
        *autopilot.lock().unwrap() = Some(ap);
    }
    Ok(autopilot)
}

fn spawn_sigint_handler(autopilot: Arc<Mutex<Option<AutopilotProcess>>>) {
    let sigint = Arc::new(AtomicBool::new(false));
    if let Err(e) = signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&sigint)) {
        eprintln!("Warning: could not register SIGINT handler: {e}");
    }
    thread::spawn(move || {
        while !sigint.load(Ordering::Relaxed) {
            thread::sleep(Duration::from_millis(100));
        }
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
