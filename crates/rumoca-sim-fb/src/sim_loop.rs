//! Lockstep simulation loop driven entirely by the TOML config.
//!
//! This module owns the transport plumbing (UDP + WebSocket + HTTP) and the
//! per-frame orchestration:
//!   1. poll input engine  (config-driven gamepad/keyboard)
//!   2. drain incoming UDP, apply unpacked values to stepper / locals
//!   3. step physics
//!   4. build outgoing `SignalFrame` via signal mapper
//!   5. pack + send UDP
//!   6. build viewer JSON via signal mapper
//!   7. push to WebSocket
//!   8. realtime pacing
//!
//! No vehicle-specific code here — that all lives in `quadrotor.toml` now.

use std::net::{TcpListener, UdpSocket};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result};
use rumoca_codec_flatbuffers::bfbs::SchemaSet;
use rumoca_codec_flatbuffers::codec::{PackCodec, UnpackCodec};
use rumoca_solver_diffsol::{SimStepper, StepperOptions};
use tungstenite::{Message, accept};

use crate::config::{ResetConfig, SimFbConfig, UdpConfig};
use crate::input_engine::InputEngine;
use crate::signal_mapper::{RuntimeContext, SignalMapper};

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

fn resolve_udp(cfg: &SimFbConfig) -> Option<&UdpConfig> {
    cfg.transport
        .as_ref()
        .and_then(|t| t.udp.as_ref())
        .or(cfg.udp.as_ref())
}

// ── Main loop ──────────────────────────────────────────────────────────────

/// Run the lockstep simulation app. Blocks the calling thread.
#[allow(clippy::too_many_lines)]
pub fn run_sim_loop(
    cfg: &SimFbConfig,
    schema_set: &SchemaSet,
    stepper: &mut SimStepper,
    model_source: &str,
    model_name: &str,
    ws_port: u16,
    debug: bool,
) -> Result<()> {
    let dt = cfg.sim.dt;

    // ── Codecs ────────────────────────────────────────────────────────────
    let pack_codec = PackCodec::compile(schema_set, &cfg.send).context("Build pack codec")?;
    let unpack_codec =
        UnpackCodec::compile(schema_set, &cfg.receive).context("Build unpack codec")?;
    let recv_expected = unpack_codec.expected_size();

    // ── UDP ───────────────────────────────────────────────────────────────
    let udp_cfg = resolve_udp(cfg)
        .context("No UDP config: expected [transport.udp] or legacy [udp] section")?;
    let socket =
        UdpSocket::bind(&udp_cfg.listen).with_context(|| format!("Bind UDP {}", udp_cfg.listen))?;
    socket.set_read_timeout(Some(Duration::from_millis(100)))?;
    eprintln!("  UDP listen: {}", udp_cfg.listen);
    eprintln!("  UDP send:   {}", udp_cfg.send);
    eprintln!("  Expecting {recv_expected}-byte receive packets");

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

    // ── Autopilot subprocess ──────────────────────────────────────────────
    let autopilot: Arc<Mutex<Option<AutopilotProcess>>> = Arc::new(Mutex::new(None));
    if let Some(ap_cfg) = &cfg.autopilot {
        let mut ap = AutopilotProcess::new(&ap_cfg.command);
        ap.start()?;
        *autopilot.lock().unwrap() = Some(ap);
    }

    // ── SIGINT handler ────────────────────────────────────────────────────
    let ap_cleanup = Arc::clone(&autopilot);
    let sigint = Arc::new(AtomicBool::new(false));
    if let Err(e) = signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&sigint)) {
        eprintln!("Warning: could not register SIGINT handler: {e}");
    }
    thread::spawn(move || {
        while !sigint.load(Ordering::Relaxed) {
            thread::sleep(Duration::from_millis(100));
        }
        if let Ok(mut ap) = ap_cleanup.lock()
            && let Some(proc) = ap.as_mut()
        {
            proc.stop();
        }
        crossterm::terminal::disable_raw_mode().ok();
        std::process::exit(0);
    });

    // ── WebSocket viewer thread ───────────────────────────────────────────
    let (state_tx, state_rx) = mpsc::channel::<String>();
    let realtime = Arc::new(AtomicBool::new(cfg.sim.realtime));
    let realtime_ws = Arc::clone(&realtime);
    thread::spawn(move || run_ws_server(ws_port, state_rx, realtime_ws));

    eprintln!("\nReady. Simulation running.");

    // ── Loop state ────────────────────────────────────────────────────────
    let mut recv_buf = [0u8; 512];
    let mut pkt_count = 0u64;
    let mut frame_num = 0u64;
    let mut last_poll = Instant::now();
    let ap_handle = Arc::clone(&autopilot);

    loop {
        let frame_start = Instant::now();

        // 0. Poll input engine with wall-clock dt.
        let poll_dt = last_poll.elapsed().as_secs_f64();
        last_poll = Instant::now();
        engine.poll(poll_dt);

        // 0a. Handle signals emitted by the engine.
        if engine.take_signal("quit") {
            eprintln!("\n[sim] quit requested");
            break;
        }
        if let Some(reset_cfg) = cfg.reset.as_ref()
            && engine.take_signal(&reset_cfg.on_signal)
        {
            handle_reset(
                reset_cfg,
                &mut engine,
                stepper,
                model_source,
                model_name,
                &ap_handle,
            )?;
        }
        if debug
            && let Some(dbg) = cfg.debug_log.as_ref()
            && engine.take_signal(&dbg.trigger_signal)
        {
            // TODO(phase 4b): ring-buffer-backed debug log dump.
            eprintln!("[debug] log trigger — ring buffer not yet implemented");
        }

        // 1. Drain UDP (non-blocking), unpack, route to stepper or locals.
        socket.set_nonblocking(true).ok();
        while let Ok((n, _)) = socket.recv_from(&mut recv_buf) {
            pkt_count += 1;
            if n == recv_expected {
                let values = unpack_codec.unpack(&recv_buf[..n]);
                apply_received(&values, stepper, &mut engine);
            }
        }
        socket.set_nonblocking(false).ok();

        // 2. Step physics in MAX_SUB_DT-sized chunks.
        let target_clock = stepper.time() + dt;
        let step_dt = target_clock - stepper.time();
        if step_dt > 0.0 {
            let n_steps = ((step_dt / MAX_SUB_DT).ceil() as usize).max(1);
            let sub_dt = step_dt / n_steps as f64;
            for i in 0..n_steps {
                if let Err(e) = stepper.step(sub_dt) {
                    eprintln!(
                        "\r[sim] step {}/{} failed (sub_dt={sub_dt:.4}): {e}",
                        i + 1,
                        n_steps,
                    );
                }
            }
        }

        // 3. Assemble runtime context once; used by both send + viewer.
        let wall_ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as f64;
        let stepper_time = stepper.time();
        let stepper_get = |name: &str| stepper.get(name);
        let rt = RuntimeContext {
            frame_num,
            wall_ms,
            input_connected: engine.is_connected(),
            input_mode: engine.mode(),
            stepper_time,
            stepper_get: &stepper_get,
        };

        // 4. Send outgoing frame to autopilot.
        let send_frame = mapper.build_send(&engine, &rt);
        let buf = pack_codec.pack(&send_frame);
        let _ = socket.send_to(&buf, &udp_cfg.send);

        // 5. Push viewer state to WebSocket thread.
        let json = mapper.build_viewer_json(&engine, &rt);
        let _ = state_tx.send(json);

        // 6. Throttled status line (about once per second at any dt).
        let status_period = (1.0_f64 / dt).max(1.0) as u64;
        if frame_num.is_multiple_of(status_period) {
            eprint!(
                "\r[sim] t={:.1}s frame={frame_num} pkts={pkt_count}            ",
                stepper.time()
            );
        }

        // 7. Realtime pacing (togglable from the browser).
        if realtime.load(Ordering::Relaxed) {
            let elapsed = frame_start.elapsed();
            let target = Duration::from_secs_f64(dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }

        frame_num += 1;
    }

    Ok(())
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

// ── WebSocket server ──────────────────────────────────────────────────────

fn run_ws_server(port: u16, state_rx: mpsc::Receiver<String>, realtime: Arc<AtomicBool>) {
    let listener = match TcpListener::bind(format!("0.0.0.0:{port}")) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to bind WS port {port}: {e}");
            return;
        }
    };
    eprintln!("  WebSocket: ws://0.0.0.0:{port}");

    for stream in listener.incoming() {
        let Ok(stream) = stream else { continue };
        eprintln!("[WS] viewer connected");
        let mut ws = match accept(stream) {
            Ok(ws) => ws,
            Err(e) => {
                eprintln!("[WS] handshake error: {e}");
                continue;
            }
        };
        ws.get_ref().set_nonblocking(true).ok();
        ws.get_ref()
            .set_write_timeout(Some(Duration::from_millis(100)))
            .ok();

        let mut alive = true;
        while alive {
            loop {
                match ws.read() {
                    Ok(Message::Text(text)) => {
                        if let Ok(cmd) = serde_json::from_str::<serde_json::Value>(&text)
                            && let Some(rt) = cmd.get("realtime").and_then(|v| v.as_bool())
                        {
                            realtime.store(rt, Ordering::Relaxed);
                            eprintln!("\r[WS] realtime: {rt}                    \r");
                        }
                    }
                    Ok(Message::Close(_)) => {
                        alive = false;
                        break;
                    }
                    Err(tungstenite::Error::Io(ref e))
                        if e.kind() == std::io::ErrorKind::WouldBlock =>
                    {
                        break;
                    }
                    Err(_) => {
                        alive = false;
                        break;
                    }
                    _ => {}
                }
            }
            if !alive {
                break;
            }

            let mut latest: Option<String> = None;
            while let Ok(json) = state_rx.try_recv() {
                latest = Some(json);
            }
            if let Some(json) = latest
                && ws.send(Message::Text(json.into())).is_err()
            {
                break;
            }
            thread::sleep(Duration::from_millis(16)); // ~60fps
        }
        eprintln!("[WS] viewer disconnected");
    }
}
