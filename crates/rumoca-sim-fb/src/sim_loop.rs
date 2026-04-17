//! Lockstep simulation loop with UDP FlatBuffer I/O for the current app/demo.
//!
//! The reusable named-signal contract lives in `rumoca-codec`, while the
//! FlatBuffer schema/codecs live in `rumoca-codec-flatbuffers`. This module owns the
//! concrete quadrotor/controller/viewer application loop on top of those
//! lower layers.

use std::net::{TcpListener, UdpSocket};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{Context, Result};
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use gilrs::{Axis, Button, Gilrs};
use rumoca_codec::SignalFrame;
use rumoca_codec_flatbuffers::bfbs::SchemaSet;
use rumoca_codec_flatbuffers::codec::{PackCodec, UnpackCodec};
use rumoca_solver_diffsol::{SimStepper, StepperOptions};
use tungstenite::{Message, accept};

use crate::config::SimFbConfig;

const MAX_SUB_DT: f64 = 0.002;
const RC_CENTER: i32 = 1500;
const RC_MIN: i32 = 1000;
const RC_MAX: i32 = 2000;

// ── Autopilot process management ─────────────────────────────────────

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
        eprintln!("[autopilot] Starting: {}", self.command);
        let child = Command::new(&self.command)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .with_context(|| format!("Failed to start autopilot: {}", self.command))?;
        eprintln!("[autopilot] PID: {}", child.id());
        self.child = Some(child);
        Ok(())
    }

    fn stop(&mut self) {
        if let Some(mut child) = self.child.take() {
            let pid = child.id();
            eprintln!("[autopilot] Killing PID {}", pid);
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

// ── Input handling (keyboard + gamepad) ──────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq)]
enum InputMode {
    Gamepad,
    Keyboard,
}

struct InputState {
    gilrs: Gilrs,
    rc: [i32; 16],
    armed: bool,
    arm_prev: bool,
    arm_last_toggle: Instant,
    throttle: f64,
    last_poll: Instant,
    mode: InputMode,
    kb_roll: f64,
    kb_pitch: f64,
    kb_yaw: f64,
    kb_throttle_input: f64,
    reset_requested: bool,
    log_requested: bool,
}

impl InputState {
    fn new() -> Self {
        let gilrs = Gilrs::new().expect("Failed to initialize gilrs");
        for (_id, gamepad) in gilrs.gamepads() {
            eprintln!("Gamepad found: {} ({})", gamepad.name(), gamepad.os_name());
        }

        let gp_count = gilrs.gamepads().count();
        eprintln!("[input] Detected {} gamepad(s)", gp_count);
        let mode = if gp_count > 0 {
            eprintln!("[input] Using gamepad");
            InputMode::Gamepad
        } else {
            eprintln!("[input] Using keyboard");
            eprintln!("  Up/Down    — throttle");
            eprintln!("  Left/Right — yaw");
            eprintln!("  W/S        — pitch");
            eprintln!("  A/D        — roll");
            eprintln!("  Space      — arm/disarm");
            eprintln!("  R          — reset");
            eprintln!("  L          — save debug log");
            eprintln!("  Q          — quit");
            match crossterm::terminal::enable_raw_mode() {
                Ok(()) => eprintln!("[input] Raw mode enabled"),
                Err(e) => eprintln!(
                    "[input] WARNING: raw mode failed: {} — keyboard input may not work",
                    e
                ),
            }
            InputMode::Keyboard
        };

        let mut rc = [RC_CENTER; 16];
        rc[2] = RC_MIN;
        rc[4] = RC_MIN;
        Self {
            gilrs,
            rc,
            armed: false,
            arm_prev: false,
            arm_last_toggle: Instant::now() - Duration::from_secs(10),
            throttle: 0.0,
            last_poll: Instant::now(),
            mode,
            kb_roll: 0.0,
            kb_pitch: 0.0,
            kb_yaw: 0.0,
            kb_throttle_input: 0.0,
            reset_requested: false,
            log_requested: false,
        }
    }

    fn poll(&mut self) {
        match self.mode {
            InputMode::Gamepad => self.poll_gamepad(),
            InputMode::Keyboard => self.poll_keyboard(),
        }
    }

    /// One-time check to verify keyboard input works
    fn verify_input(&self) {
        eprintln!("[input] Mode: {:?}", self.mode);
        eprintln!(
            "[input] Raw mode active: {}",
            crossterm::terminal::is_raw_mode_enabled().unwrap_or(false)
        );
    }

    fn poll_gamepad(&mut self) {
        while self.gilrs.next_event().is_some() {}
        let Some((_id, gamepad)) = self.gilrs.gamepads().next() else {
            return;
        };

        self.rc[0] = axis_to_rc(gamepad.value(Axis::RightStickX), RC_CENTER, 500);
        self.rc[1] = axis_to_rc(gamepad.value(Axis::RightStickY), RC_CENTER, -500);
        self.rc[3] = axis_to_rc(gamepad.value(Axis::LeftStickX), RC_CENTER, 500);

        let dt = self.last_poll.elapsed().as_secs_f64();
        self.last_poll = Instant::now();
        let stick_y = gamepad.value(Axis::LeftStickY) as f64;
        let input = if stick_y.abs() < 0.1 { 0.0 } else { stick_y };
        self.throttle = (self.throttle + input * 0.7 * dt).clamp(0.0, 1.0);
        self.rc[2] = (RC_MIN as f64 + self.throttle * (RC_MAX - RC_MIN) as f64).round() as i32;

        let arm_btn = gamepad.is_pressed(Button::Start);
        if arm_btn
            && !self.arm_prev
            && self.arm_last_toggle.elapsed() > Duration::from_millis(500)
            && self.rc[2] <= 1050
        {
            self.armed = !self.armed;
            self.arm_last_toggle = Instant::now();
            eprintln!(
                "\r[gamepad] {}",
                if self.armed { "ARMED" } else { "DISARMED" }
            );
        }
        self.arm_prev = arm_btn;
        self.rc[4] = if self.armed { RC_MAX } else { RC_MIN };

        if gamepad.is_pressed(Button::South) {
            self.reset_requested = true;
        }
        if gamepad.is_pressed(Button::North) {
            self.log_requested = true; // only acts if debug mode is on
        }
    }

    #[allow(clippy::excessive_nesting)]
    fn poll_keyboard(&mut self) {
        let dt = self.last_poll.elapsed().as_secs_f64();
        self.last_poll = Instant::now();

        let decay = 0.85_f64.powf(dt / 0.016);
        self.kb_roll *= decay;
        self.kb_pitch *= decay;
        self.kb_yaw *= decay;
        self.kb_throttle_input *= decay;

        while event::poll(Duration::ZERO).unwrap_or(false) {
            let evt = event::read();
            match &evt {
                Ok(Event::Key(ke)) => {
                    // Skip release events
                    if ke.kind == crossterm::event::KeyEventKind::Release {
                        continue;
                    }
                    let ctrl = ke.modifiers.contains(KeyModifiers::CONTROL);
                    match ke.code {
                        KeyCode::Char('c') if ctrl => {
                            crossterm::terminal::disable_raw_mode().ok();
                            std::process::exit(0);
                        }
                        KeyCode::Char('q') => {
                            crossterm::terminal::disable_raw_mode().ok();
                            std::process::exit(0);
                        }
                        KeyCode::Up => self.kb_throttle_input = 1.0,
                        KeyCode::Down => self.kb_throttle_input = -1.0,
                        KeyCode::Left => self.kb_yaw = -0.6,
                        KeyCode::Right => self.kb_yaw = 0.6,
                        KeyCode::Char('w') => self.kb_pitch = -0.6,
                        KeyCode::Char('s') => self.kb_pitch = 0.6,
                        KeyCode::Char('a') => self.kb_roll = -0.6,
                        KeyCode::Char('d') => self.kb_roll = 0.6,
                        KeyCode::Char(' ')
                            if self.arm_last_toggle.elapsed() > Duration::from_millis(500) =>
                        {
                            if !self.armed && self.rc[2] > 1050 {
                                eprint!(
                                    "\r[keyboard] Cannot arm: lower throttle first!                 \r"
                                );
                            } else {
                                self.armed = !self.armed;
                                self.arm_last_toggle = Instant::now();
                                eprint!(
                                    "\r[keyboard] {}                                        \r",
                                    if self.armed { "ARMED" } else { "DISARMED" }
                                );
                            }
                        }
                        KeyCode::Char('r') => {
                            self.reset_requested = true;
                            eprint!("\r[keyboard] RESET requested                              \r");
                        }
                        KeyCode::Char('l') => {
                            self.log_requested = true;
                            eprint!("\r[keyboard] LOG requested                                \r");
                        }
                        _ => {}
                    }
                }
                Ok(_) => {} // mouse, resize, etc
                Err(e) => {
                    eprint!(
                        "\r[keyboard] read error: {}                                    \r",
                        e
                    );
                }
            }
        }

        self.throttle = (self.throttle + self.kb_throttle_input * 0.7 * dt).clamp(0.0, 1.0);
        self.rc[2] = (RC_MIN as f64 + self.throttle * (RC_MAX - RC_MIN) as f64).round() as i32;
        self.rc[0] = (RC_CENTER as f64 + self.kb_roll * 500.0)
            .round()
            .clamp(RC_MIN as f64, RC_MAX as f64) as i32;
        self.rc[1] = (RC_CENTER as f64 + self.kb_pitch * 500.0)
            .round()
            .clamp(RC_MIN as f64, RC_MAX as f64) as i32;
        self.rc[3] = (RC_CENTER as f64 + self.kb_yaw * 500.0)
            .round()
            .clamp(RC_MIN as f64, RC_MAX as f64) as i32;
        self.rc[4] = if self.armed { RC_MAX } else { RC_MIN };
    }

    fn is_connected(&self) -> bool {
        match self.mode {
            InputMode::Gamepad => self.gilrs.gamepads().count() > 0,
            InputMode::Keyboard => true,
        }
    }
}

impl Drop for InputState {
    fn drop(&mut self) {
        if self.mode == InputMode::Keyboard {
            crossterm::terminal::disable_raw_mode().ok();
        }
    }
}

fn axis_to_rc(value: f32, center: i32, half_range: i32) -> i32 {
    ((center as f32) + value * (half_range as f32))
        .round()
        .clamp(RC_MIN as f32, RC_MAX as f32) as i32
}

// ── Sensor values for pack codec ─────────────────────────────────────

fn build_send_values(stepper: &SimStepper, input: &InputState) -> SignalFrame {
    let mut v = SignalFrame::with_capacity(24);
    let get = |name: &str| stepper.get(name).unwrap_or(0.0);

    v.insert("gyro_x", get("gyro_x"));
    v.insert("gyro_y", get("gyro_y"));
    v.insert("gyro_z", get("gyro_z"));
    v.insert("accel_x", get("accel_x"));
    v.insert("accel_y", get("accel_y"));
    v.insert("accel_z", get("accel_z"));

    for i in 0..16 {
        v.insert(format!("rc_{i}"), input.rc[i] as f64);
    }

    let connected = input.is_connected();
    v.insert("rc_link_quality", if connected { 255.0 } else { 0.0 });
    v.insert("rc_valid", if connected { 1.0 } else { 0.0 });
    v.insert("imu_valid", 1.0);
    v
}

// ── State JSON for viz WebSocket ─────────────────────────────────────

fn build_state_json(
    stepper: &SimStepper,
    armed: bool,
    motor_rpms: &[f64; 4],
    input: &InputState,
    frame_num: u64,
) -> String {
    let get = |name: &str| stepper.get(name).unwrap_or(0.0);
    // Include wall-clock timestamp for latency measurement in browser
    let wall_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as f64;
    serde_json::json!({
        "t": stepper.time(),
        "frame": frame_num,
        "wall_ms": wall_ms,
        "px": get("px"), "py": get("py"), "pz": get("pz"),
        "vx": get("vx"), "vy": get("vy"), "vz": get("vz"),
        "q0": stepper.get("q0").unwrap_or(1.0),
        "q1": get("q1"), "q2": get("q2"), "q3": get("q3"),
        "omega_m1": motor_rpms[0], "omega_m2": motor_rpms[1],
        "omega_m3": motor_rpms[2], "omega_m4": motor_rpms[3],
        "T": get("T"),
        "accel_x": get("accel_x"), "accel_y": get("accel_y"), "accel_z": get("accel_z"),
        "gyro_x": get("gyro_x"), "gyro_y": get("gyro_y"), "gyro_z": get("gyro_z"),
        "armed": armed,
        "rc_throttle": input.rc[2],
        "input_mode": if input.mode == InputMode::Gamepad { "gamepad" } else { "keyboard" },
    })
    .to_string()
}

// ── Frame log for debug download ─────────────────────────────────────

const FRAME_LOG_SIZE: usize = 500;

struct FrameLogEntry {
    frame: u64,
    t: f64,
    armed: bool,
    rc: [i32; 5],
    motor_rpms: [f64; 4],
    px: f64,
    py: f64,
    pz: f64,
    accel: [f64; 3],
    gyro: [f64; 3],
}

fn download_debug_log(
    stepper: &SimStepper,
    frame_log: &[FrameLogEntry],
    model_name: &str,
    armed: bool,
    input: &InputState,
) {
    let mut lines = Vec::new();
    lines.push("=== Rumoca SIL Debug Log ===".to_string());
    lines.push(format!("Time: {:?}", std::time::SystemTime::now()));
    lines.push(format!("Model: {}", model_name));
    lines.push(format!("Armed: {}", armed));
    lines.push(format!("Input mode: {:?}", input.mode));
    lines.push(format!("Frames logged: {}", frame_log.len()));
    lines.push(String::new());

    lines.push("=== CURRENT STATE ===".to_string());
    let get = |name: &str| stepper.get(name).unwrap_or(0.0);
    lines.push(format!(
        "t={:.4} px={:.4} py={:.4} pz={:.4}",
        stepper.time(),
        get("px"),
        get("py"),
        get("pz")
    ));
    lines.push(format!(
        "vx={:.4} vy={:.4} vz={:.4}",
        get("vx"),
        get("vy"),
        get("vz")
    ));
    lines.push(format!(
        "q0={:.6} q1={:.6} q2={:.6} q3={:.6}",
        stepper.get("q0").unwrap_or(1.0),
        get("q1"),
        get("q2"),
        get("q3")
    ));
    lines.push(format!(
        "accel=[{:.3},{:.3},{:.3}]",
        get("accel_x"),
        get("accel_y"),
        get("accel_z")
    ));
    lines.push(format!(
        "gyro=[{:.4},{:.4},{:.4}]",
        get("gyro_x"),
        get("gyro_y"),
        get("gyro_z")
    ));
    lines.push(format!("Input names: {:?}", stepper.input_names()));
    lines.push(format!("Variable names: {:?}", stepper.variable_names()));
    lines.push(String::new());

    lines.push(format!("=== LAST {} FRAMES ===", frame_log.len()));
    for f in frame_log {
        lines.push(format!(
            "#{} t={:.4} rc=[{},{},{},{},{}] armed={} motors=[{:.1},{:.1},{:.1},{:.1}] pos=[{:.3},{:.3},{:.3}] accel=[{:.3},{:.3},{:.3}] gyro=[{:.4},{:.4},{:.4}]",
            f.frame, f.t,
            f.rc[0], f.rc[1], f.rc[2], f.rc[3], f.rc[4],
            f.armed,
            f.motor_rpms[0], f.motor_rpms[1], f.motor_rpms[2], f.motor_rpms[3],
            f.px, f.py, f.pz,
            f.accel[0], f.accel[1], f.accel[2],
            f.gyro[0], f.gyro[1], f.gyro[2],
        ));
    }

    let filename = format!(
        "sil_debug_{}.txt",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis()
    );
    match std::fs::write(&filename, lines.join("\n")) {
        Ok(()) => eprintln!(
            "\n[debug] Log saved: {}                                          \n",
            filename
        ),
        Err(e) => eprintln!(
            "\n[debug] Failed to save log: {}                                 \n",
            e
        ),
    }
}

// ── Main simulation loop ─────────────────────────────────────────────

/// Run the current lockstep simulation app loop.
///
/// This function blocks the calling thread. It:
/// 1. Starts the controller process (if configured)
/// 2. Starts the WebSocket viewer thread
/// 3. Runs the main loop: recv outputs → step physics → send inputs
#[allow(clippy::too_many_lines, clippy::excessive_nesting)]
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

    // Build codecs
    let pack_codec = PackCodec::compile(schema_set, &cfg.send).context("Build pack codec")?;
    let unpack_codec =
        UnpackCodec::compile(schema_set, &cfg.receive).context("Build unpack codec")?;

    // UDP socket
    let udp_cfg = cfg.udp.as_ref().context("No [udp] section in config")?;
    let socket =
        UdpSocket::bind(&udp_cfg.listen).with_context(|| format!("Bind UDP {}", udp_cfg.listen))?;
    socket.set_read_timeout(Some(Duration::from_millis(100)))?;
    eprintln!("  UDP listen: {}", udp_cfg.listen);
    eprintln!("  UDP send:   {}", udp_cfg.send);

    // Autopilot process
    let autopilot: Arc<Mutex<Option<AutopilotProcess>>> = Arc::new(Mutex::new(None));
    if let Some(ap_cfg) = &cfg.autopilot {
        let mut ap = AutopilotProcess::new(&ap_cfg.command);
        ap.start()?;
        *autopilot.lock().unwrap() = Some(ap);
    }

    // SIGINT handler
    let ap_cleanup = Arc::clone(&autopilot);
    let sigint = Arc::new(AtomicBool::new(false));
    if let Err(e) = signal_hook::flag::register(signal_hook::consts::SIGINT, Arc::clone(&sigint)) {
        eprintln!("Warning: could not register SIGINT handler: {}", e);
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

    // Channel for viz state updates (sim loop → WS thread)
    let (state_tx, state_rx) = mpsc::channel::<String>();

    // Shared realtime toggle (browser can flip this)
    let realtime = Arc::new(AtomicBool::new(cfg.sim.realtime));
    let realtime_ws = Arc::clone(&realtime);

    // WebSocket viz thread
    thread::spawn(move || {
        let ws_listener = match TcpListener::bind(format!("0.0.0.0:{}", ws_port)) {
            Ok(l) => l,
            Err(e) => {
                eprintln!("Failed to bind WS port {}: {}", ws_port, e);
                return;
            }
        };
        eprintln!("  WebSocket: ws://0.0.0.0:{}", ws_port);

        for stream in ws_listener.incoming() {
            let Ok(stream) = stream else { continue };
            eprintln!("[WS] Viewer connected");
            let mut ws = match accept(stream) {
                Ok(ws) => ws,
                Err(e) => {
                    eprintln!("[WS] Handshake error: {}", e);
                    continue;
                }
            };
            ws.get_ref().set_nonblocking(true).ok();
            ws.get_ref()
                .set_write_timeout(Some(Duration::from_millis(100)))
                .ok();

            let mut alive = true;
            while alive {
                // Drain incoming WS messages (detect close, handle commands)
                loop {
                    match ws.read() {
                        Ok(Message::Text(text)) => {
                            // Handle commands from browser
                            if let Ok(cmd) = serde_json::from_str::<serde_json::Value>(&text)
                                && let Some(rt) = cmd.get("realtime").and_then(|v| v.as_bool())
                            {
                                realtime_ws.store(rt, Ordering::Relaxed);
                                eprintln!("\r[WS] Realtime: {}                    \r", rt);
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

                // Drain channel, keep only the latest state
                let mut latest = None;
                while let Ok(json) = state_rx.try_recv() {
                    latest = Some(json);
                }
                if let Some(json) = latest
                    && ws.send(Message::Text(json.into())).is_err()
                {
                    break;
                }

                // Throttle to ~60fps (sleep AFTER send)
                thread::sleep(Duration::from_millis(16));
            }
            eprintln!("[WS] Viewer disconnected");
        }
    });

    // Input state
    let mut input = InputState::new();
    let mut recv_buf = [0u8; 512];
    let mut armed = false;
    let mut pkt_count = 0u64;
    let mut frame_num = 0u64;
    let mut motor_rpms = [0.0f64; 4];
    let recv_expected = unpack_codec.expected_size();
    let ap_handle = Arc::clone(&autopilot);
    let mut frame_log: Vec<FrameLogEntry> = Vec::with_capacity(FRAME_LOG_SIZE);

    eprintln!("  Expecting {recv_expected}-byte receive packets");
    input.verify_input();
    eprintln!("\nReady. Simulation running.");
    eprintln!(
        "  Controls: Up/Down=throttle, Left/Right=yaw, W/S=pitch, A/D=roll, Space=arm, R=reset, L=log, Q=quit\n"
    );

    loop {
        let _frame_start = Instant::now();
        input.poll();

        // Download debug log (only in debug mode)
        if debug && input.log_requested {
            input.log_requested = false;
            download_debug_log(stepper, &frame_log, model_name, armed, &input);
        }

        // Reset
        if input.reset_requested {
            input.reset_requested = false;
            motor_rpms = [0.0; 4];
            armed = false;
            input.armed = false;
            input.throttle = 0.0;
            input.rc[2] = RC_MIN;
            input.rc[4] = RC_MIN;

            // Restart autopilot
            if let Ok(mut ap) = ap_handle.lock()
                && let Some(proc) = ap.as_mut()
                && let Err(e) = proc.start()
            {
                eprintln!("[autopilot] Restart failed: {}", e);
            }

            // Reset stepper
            let mut session = rumoca_session::compile::Session::default();
            if let Err(e) = session.add_document(&format!("{model_name}.mo"), model_source) {
                eprintln!("[reset] Parse failed: {}", e);
            } else {
                match session.compile_model(model_name) {
                    Ok(result) => {
                        match SimStepper::new(
                            &result.dae,
                            StepperOptions {
                                rtol: 1e-3,
                                atol: 1e-3,
                                ..Default::default()
                            },
                        ) {
                            Ok(new_stepper) => {
                                *stepper = new_stepper;
                                eprintln!("[reset] Simulation reset");
                            }
                            Err(e) => eprintln!("[reset] Stepper creation failed: {}", e),
                        }
                    }
                    Err(e) => eprintln!("[reset] Compilation failed: {}", e),
                }
            }
        }

        // ── Sim loop (matches working rumoca_sil): drain → step → send ──

        // 1. Drain all queued motor packets (non-blocking), keep latest
        socket.set_nonblocking(true).ok();
        while let Ok((n, _)) = socket.recv_from(&mut recv_buf) {
            pkt_count += 1;
            if n == recv_expected {
                let values = unpack_codec.unpack(&recv_buf[..n]);
                if !values.is_empty() {
                    motor_rpms = [
                        values.get("omega_m1").copied().unwrap_or(0.0),
                        values.get("omega_m2").copied().unwrap_or(0.0),
                        values.get("omega_m3").copied().unwrap_or(0.0),
                        values.get("omega_m4").copied().unwrap_or(0.0),
                    ];
                    armed = values.get("armed").copied().unwrap_or(0.0) != 0.0;
                }
            }
        }
        socket.set_nonblocking(false).ok();

        // 2. Apply motors and step physics
        for (i, &rpm) in motor_rpms.iter().enumerate() {
            let name = format!("omega_m{}", i + 1);
            let _ = stepper.set_input(&name, rpm);
        }
        let target_clock = stepper.time() + dt;
        let step_dt = target_clock - stepper.time();
        if step_dt > 0.0 {
            let n_steps = ((step_dt / MAX_SUB_DT).ceil() as usize).max(1);
            let sub_dt = step_dt / n_steps as f64;
            for i in 0..n_steps {
                if let Err(e) = stepper.step(sub_dt) {
                    eprintln!(
                        "\r[sim] Step {}/{} failed (sub_dt={:.4}s): {}",
                        i + 1,
                        n_steps,
                        sub_dt,
                        e
                    );
                }
            }
        }

        // 3. Send sensor data to autopilot
        let send_vals = build_send_values(stepper, &input);
        let buf = pack_codec.pack(&send_vals);
        let _ = socket.send_to(&buf, &udp_cfg.send);

        // Frame log
        let get = |name: &str| stepper.get(name).unwrap_or(0.0);
        let entry = FrameLogEntry {
            frame: frame_num,
            t: stepper.time(),
            armed,
            rc: [
                input.rc[0],
                input.rc[1],
                input.rc[2],
                input.rc[3],
                input.rc[4],
            ],
            motor_rpms,
            px: get("px"),
            py: get("py"),
            pz: get("pz"),
            accel: [get("accel_x"), get("accel_y"), get("accel_z")],
            gyro: [get("gyro_x"), get("gyro_y"), get("gyro_z")],
        };
        if frame_log.len() >= FRAME_LOG_SIZE {
            frame_log.remove(0);
        }
        frame_log.push(entry);
        frame_num += 1;

        // Status logging (once per second)
        if frame_num.is_multiple_of(250) {
            eprint!(
                "\r[sim] t={:.1}s alt={:.2}m motors=[{:.0},{:.0},{:.0},{:.0}] armed={} rc=[{},{},{},{},arm={}] pkts={}    ",
                stepper.time(),
                -(stepper.get("pz").unwrap_or(0.0)),
                motor_rpms[0],
                motor_rpms[1],
                motor_rpms[2],
                motor_rpms[3],
                armed,
                input.rc[0],
                input.rc[1],
                input.rc[2],
                input.rc[3],
                input.rc[4],
                pkt_count,
            );
        }

        // Send state to viz
        let json = build_state_json(stepper, armed, &motor_rpms, &input, frame_num);
        let _ = state_tx.send(json);

        // Realtime pacing (togglable from browser)
        if realtime.load(Ordering::Relaxed) {
            let elapsed = _frame_start.elapsed();
            let target = Duration::from_secs_f64(dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }
    }
}
