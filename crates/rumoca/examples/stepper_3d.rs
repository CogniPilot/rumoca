#![allow(clippy::excessive_nesting, clippy::too_many_lines)]
//! Interactive 3D quadrotor demo using the real-time stepper.
//!
//! Runs a WebSocket server that:
//! - Serves an HTML page with a three.js quadrotor visualization
//! - Streams simulation state at 50 Hz
//! - Receives keyboard inputs (WASD + arrows) as RC stick commands
//!
//! Usage:
//!   cargo run --example stepper_3d -p rumoca
//!   Then open http://localhost:8080 in a browser.

use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use rumoca_sim::{SimStepper, StepperOptions};
use tungstenite::{Message, accept};

const DT: f64 = 0.02; // 50 Hz
const HTTP_PORT: u16 = 8080;
const WS_PORT: u16 = 8081;

const MODEL_SOURCE: &str = include_str!("../../../examples/QuadrotorAttitude.mo");
const HOVER_THRUST: f64 = 2.0 * 9.80665; // mass * g
const MAX_ANGLE: f64 = 0.5; // ~28 degrees max tilt
const MAX_YAW_RATE: f64 = 2.0; // rad/s
const THREE_JS: &str = include_str!("../../rumoca-sim/web/three.min.js");

const HTML_PAGE: &str = r##"<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Quadrotor Interactive Stepper</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body { background: #111; color: #eee; font-family: monospace; overflow: hidden; }
  #container { width: 100vw; height: 100vh; }
  #hud {
    position: fixed; top: 10px; left: 10px; z-index: 10;
    background: rgba(0,0,0,0.7); padding: 10px 14px; border-radius: 6px;
    font-size: 13px; line-height: 1.6;
  }
  #hud .label { color: #888; }
  #hud .val { color: #0f0; font-weight: bold; }
  #controls {
    position: fixed; bottom: 10px; left: 10px; z-index: 10;
    background: rgba(0,0,0,0.7); padding: 10px 14px; border-radius: 6px;
    font-size: 12px; line-height: 1.5;
  }
  #status {
    position: fixed; top: 10px; right: 10px; z-index: 10;
    background: rgba(0,0,0,0.7); padding: 6px 12px; border-radius: 6px;
    font-size: 12px;
  }
  .connected { color: #0f0; }
  .disconnected { color: #f00; }
</style>
</head>
<body>
<div id="container"></div>
<div id="hud">
  <span class="label">t:</span> <span class="val" id="v-t">0.00</span>s<br>
  <span class="label">pos:</span> <span class="val" id="v-pos">0, 0, 0</span><br>
  <span class="label">alt:</span> <span class="val" id="v-alt">0.0</span>m<br>
  <span class="label">vel:</span> <span class="val" id="v-vel">0.0</span> m/s<br>
  <span class="label">roll:</span> <span class="val" id="v-roll">0.0</span>°
  <span class="label">pitch:</span> <span class="val" id="v-pitch">0.0</span>°<br>
  <span class="label">RC:</span> <span class="val" id="v-rc">0, 0, 0, 0</span>
</div>
<div id="controls">
  <b>Controls:</b><br>
  W/S &mdash; pitch fwd/back<br>
  A/D &mdash; roll left/right<br>
  &uarr;/&darr; &mdash; throttle up/down<br>
  &larr;/&rarr; &mdash; yaw left/right<br>
  Space &mdash; zero sticks<br>
  R &mdash; reset simulation
</div>
<div id="status"><span class="disconnected" id="ws-status">Connecting...</span></div>

<script>
__THREE_JS__
</script>
<script>
const rc = { throttle: 0, pitch: 0, roll: 0, yaw: 0 };
const stickRate = 0.05;
const keys = {};

document.addEventListener("keydown", (e) => { keys[e.code] = true; e.preventDefault(); });
document.addEventListener("keyup", (e) => { keys[e.code] = false; e.preventDefault(); });

function updateSticks() {
  if (keys["KeyR"]) {
    keys["KeyR"] = false;
    rc.throttle = 0; rc.pitch = 0; rc.roll = 0; rc.yaw = 0;
    if (ws && ws.readyState === WebSocket.OPEN) ws.send(JSON.stringify({reset: true}));
    // Clear trail
    if (state.trail) { state.trailCount = 0; state.trail.geometry.setDrawRange(0, 0); }
    return;
  }
  if (keys["Space"]) { rc.throttle = 0; rc.pitch = 0; rc.roll = 0; rc.yaw = 0; }
  if (keys["ArrowUp"])    rc.throttle = Math.min(1, rc.throttle + stickRate);
  if (keys["ArrowDown"])  rc.throttle = Math.max(-1, rc.throttle - stickRate);
  if (keys["KeyW"])       rc.pitch = Math.min(1, rc.pitch + stickRate);
  if (keys["KeyS"])       rc.pitch = Math.max(-1, rc.pitch - stickRate);
  if (keys["KeyA"])       rc.roll = Math.max(-1, rc.roll - stickRate);
  if (keys["KeyD"])       rc.roll = Math.min(1, rc.roll + stickRate);
  if (keys["ArrowLeft"])  rc.yaw = Math.max(-1, rc.yaw - stickRate);
  if (keys["ArrowRight"]) rc.yaw = Math.min(1, rc.yaw + stickRate);
  if (!keys["ArrowUp"] && !keys["ArrowDown"]) rc.throttle *= 0.95;
  if (!keys["KeyW"] && !keys["KeyS"]) rc.pitch *= 0.95;
  if (!keys["KeyA"] && !keys["KeyD"]) rc.roll *= 0.95;
  if (!keys["ArrowLeft"] && !keys["ArrowRight"]) rc.yaw *= 0.95;
}

const container = document.getElementById("container");
const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.1, 500);
camera.position.set(2, 3, 5);
camera.lookAt(0, 0, 0);
const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.shadowMap.enabled = true;
container.appendChild(renderer.domElement);

window.addEventListener("resize", () => {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
});

let camAngle = 0.8, camElev = 0.5, camDist = 4;
let camTarget = new THREE.Vector3(0, 1, 0);
container.addEventListener("wheel", (e) => {
  camDist = Math.max(1, Math.min(20, camDist + e.deltaY * 0.005));
});
let dragging = false, lastMX = 0, lastMY = 0;
container.addEventListener("mousedown", (e) => { dragging = true; lastMX = e.clientX; lastMY = e.clientY; });
container.addEventListener("mouseup", () => { dragging = false; });
container.addEventListener("mousemove", (e) => {
  if (!dragging) return;
  camAngle -= (e.clientX - lastMX) * 0.005;
  camElev = Math.max(-1.2, Math.min(1.5, camElev + (e.clientY - lastMY) * 0.005));
  lastMX = e.clientX; lastMY = e.clientY;
});

const state = { scene, frameCount: 0 };
const api = {
  THREE, state, canvas: renderer.domElement,
  enableDefaultViewerRuntime: () => {},
  getValue: () => 0, getTime: () => 0, sampleIndex: 0,
};
const ctx = {};

__QUADROTOR_INIT_JS__

if (ctx.onInit) ctx.onInit(api);

let ws = null;
let latestState = null;

function connectWS() {
  ws = new WebSocket("ws://localhost:__WS_PORT__");
  ws.onopen = () => {
    document.getElementById("ws-status").textContent = "Connected";
    document.getElementById("ws-status").className = "connected";
  };
  ws.onclose = () => {
    document.getElementById("ws-status").textContent = "Disconnected";
    document.getElementById("ws-status").className = "disconnected";
    setTimeout(connectWS, 1000);
  };
  ws.onmessage = (e) => { latestState = JSON.parse(e.data); };
}
connectWS();

function animate() {
  requestAnimationFrame(animate);
  updateSticks();

  if (ws && ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(rc));
  }

  if (latestState && state.quad) {
    const s = latestState;
    const px = s.px || 0, py = s.py || 0, pz = s.pz || 0;
    const q0 = s.q0 || 1, q1 = s.q1 || 0, q2 = s.q2 || 0, q3 = s.q3 || 0;
    // T may be eliminated by the solver; estimate from stick position
    const T = s.T || (1 + rc.throttle) * 19.6;

    const tx = py, ty = -pz, tz = px;
    state.quad.position.set(tx, ty, tz);

    const quat = new THREE.Quaternion(-q2, -q3, -q1, q0);
    quat.normalize();
    state.quad.setRotationFromQuaternion(quat);

    const spin = Math.max(T, 0) * 0.6;
    if (state.propGroups) {
      state.propGroups.forEach((p) => {
        p.group.rotation.y += (p.cw ? 1 : -1) * (0.3 + spin * 0.04);
      });
    }

    if (state.trail) {
      const idx = (state.trailCount || 0) % 800;
      state.trailPos[idx * 3] = tx;
      state.trailPos[idx * 3 + 1] = ty;
      state.trailPos[idx * 3 + 2] = tz;
      state.trailCount = (state.trailCount || 0) + 1;
      state.trail.geometry.attributes.position.needsUpdate = true;
      state.trail.geometry.setDrawRange(0, Math.min(state.trailCount, 800));
    }

    if (state.shadow) {
      state.shadow.position.set(tx, 0.003, tz);
      const alt = Math.max(ty, 0.01);
      const sc = Math.max(0.4, 1.2 - alt * 0.04);
      state.shadow.scale.set(sc, sc, 1);
      state.shadow.material.opacity = Math.max(0.03, 0.25 - alt * 0.015);
    }

    camTarget.lerp(new THREE.Vector3(tx, ty, tz), 0.05);

    document.getElementById("v-t").textContent = (s.t || 0).toFixed(2);
    document.getElementById("v-pos").textContent =
      `${px.toFixed(1)}, ${py.toFixed(1)}, ${pz.toFixed(1)}`;
    document.getElementById("v-alt").textContent = (-pz).toFixed(2);
    const speed = Math.sqrt((s.vx||0)**2 + (s.vy||0)**2 + (s.vz||0)**2);
    document.getElementById("v-vel").textContent = speed.toFixed(2);
    // Compute Euler angles from quaternion (roll/pitch may be eliminated by solver)
    const rl = Math.atan2(2*(q0*q1 + q2*q3), 1 - 2*(q1*q1 + q2*q2));
    const pt = Math.asin(Math.max(-1, Math.min(1, 2*(q0*q2 - q3*q1))));
    document.getElementById("v-roll").textContent = (rl * 180/Math.PI).toFixed(1);
    document.getElementById("v-pitch").textContent = (pt * 180/Math.PI).toFixed(1);
    document.getElementById("v-rc").textContent =
      `T:${rc.throttle.toFixed(2)} P:${rc.pitch.toFixed(2)} R:${rc.roll.toFixed(2)} Y:${rc.yaw.toFixed(2)}`;
  }

  camera.position.set(
    camTarget.x + camDist * Math.sin(camAngle) * Math.cos(camElev),
    camTarget.y + camDist * Math.sin(camElev),
    camTarget.z + camDist * Math.cos(camAngle) * Math.cos(camElev)
  );
  camera.lookAt(camTarget);
  renderer.render(scene, camera);
}
animate();
</script>
</body>
</html>"##;

/// RC input from the browser
#[derive(Default)]
struct RcInput {
    throttle: f64,
    pitch: f64,
    roll: f64,
    yaw: f64,
    reset: bool,
}

fn serve_http(listener: TcpListener, html: String) {
    for stream in listener.incoming() {
        let Ok(mut stream) = stream else { continue };
        let mut reader = BufReader::new(stream.try_clone().unwrap());
        let mut request_line = String::new();
        let _ = reader.read_line(&mut request_line);
        loop {
            let mut line = String::new();
            let _ = reader.read_line(&mut line);
            if line.trim().is_empty() {
                break;
            }
        }
        let response = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
            html.len(),
            html
        );
        let _ = stream.write_all(response.as_bytes());
    }
}

fn main() -> anyhow::Result<()> {
    eprintln!("Compiling QuadrotorAttitude model...");
    let compiler = rumoca::Compiler::new().model("QuadrotorAttitude");
    let result = compiler.compile_str(MODEL_SOURCE, "QuadrotorAttitude.mo")?;

    eprintln!("Creating stepper...");
    let mut stepper = SimStepper::new(&result.dae, StepperOptions::default())?;
    eprintln!("Inputs: {:?}", stepper.input_names());
    eprintln!(
        "Variables ({} total): {:?}",
        stepper.variable_names().len(),
        &stepper.variable_names()[..stepper.variable_names().len().min(10)]
    );

    // Load quadrotor viewer JS at runtime
    let quadrotor_js = {
        let env_path = std::env::var("RUMOCA_QUADROTOR_VIEWER_JS").ok();
        let paths = [
            env_path.as_deref(),
            Some(".rumoca/viewer3d/QuadrotorAttitude/timeseries_2.js"),
        ];
        paths
            .iter()
            .flatten()
            .find_map(|p| std::fs::read_to_string(p).ok())
            .unwrap_or_else(|| {
                eprintln!("Warning: No quadrotor viewer JS found");
                String::new()
            })
    };

    let html = HTML_PAGE
        .replace("__THREE_JS__", THREE_JS)
        .replace("__QUADROTOR_INIT_JS__", &quadrotor_js)
        .replace("__WS_PORT__", &WS_PORT.to_string());

    // Start HTTP server on a background thread
    let http_listener = TcpListener::bind(format!("0.0.0.0:{HTTP_PORT}"))?;
    eprintln!("HTTP server: http://localhost:{HTTP_PORT}");
    let html_clone = html;
    thread::spawn(move || serve_http(http_listener, html_clone));

    // Channels for communicating with the WebSocket thread
    let (input_tx, input_rx) = mpsc::channel::<RcInput>();
    let (state_tx, state_rx) = mpsc::channel::<String>();

    // WebSocket thread — handles browser connection, forwards inputs/state via channels
    thread::spawn(move || {
        let ws_listener = TcpListener::bind(format!("0.0.0.0:{WS_PORT}")).unwrap();
        eprintln!("WebSocket server: ws://localhost:{WS_PORT}");
        eprintln!("\nOpen http://localhost:{HTTP_PORT} in your browser!");
        eprintln!("Controls: WASD = pitch/roll, Arrows = throttle/yaw, Space = zero\n");

        for stream in ws_listener.incoming() {
            let Ok(stream) = stream else { continue };
            eprintln!("Client connected");
            let mut ws = match accept(stream) {
                Ok(ws) => ws,
                Err(e) => {
                    eprintln!("WS accept error: {e}");
                    continue;
                }
            };
            ws.get_ref().set_nonblocking(true).ok();

            loop {
                // Read inputs from browser
                loop {
                    match ws.read() {
                        Ok(Message::Text(text)) => {
                            if let Ok(v) = serde_json::from_str::<serde_json::Value>(text.as_ref())
                            {
                                let is_reset = v["reset"].as_bool().unwrap_or(false);
                                let _ = input_tx.send(RcInput {
                                    throttle: v["throttle"].as_f64().unwrap_or(0.0),
                                    pitch: v["pitch"].as_f64().unwrap_or(0.0),
                                    roll: v["roll"].as_f64().unwrap_or(0.0),
                                    yaw: v["yaw"].as_f64().unwrap_or(0.0),
                                    reset: is_reset,
                                });
                            }
                        }
                        Ok(Message::Close(_)) => {
                            eprintln!("Client disconnected");
                            break;
                        }
                        Err(tungstenite::Error::Io(ref e))
                            if e.kind() == std::io::ErrorKind::WouldBlock =>
                        {
                            break;
                        }
                        Err(_) => {
                            break;
                        }
                        _ => {}
                    }
                }

                // Send latest state to browser
                if let Ok(state_json) = state_rx.try_recv() {
                    // Drain to latest
                    let mut latest = state_json;
                    while let Ok(newer) = state_rx.try_recv() {
                        latest = newer;
                    }
                    ws.get_ref().set_nonblocking(false).ok();
                    if ws.send(Message::Text(latest.into())).is_err() {
                        break;
                    }
                    ws.get_ref().set_nonblocking(true).ok();
                }

                thread::sleep(Duration::from_millis(5));
            }
        }
    });

    // Main simulation loop (single-threaded, owns the stepper)
    let mut rc = RcInput::default();
    let mut frame_count = 0u64;
    loop {
        let frame_start = Instant::now();

        // Drain latest RC input
        while let Ok(new_rc) = input_rx.try_recv() {
            rc = new_rc;
        }

        // Handle reset
        if rc.reset {
            eprintln!("[sim] Resetting simulation...");
            match SimStepper::new(&result.dae, StepperOptions::default()) {
                Ok(new_stepper) => {
                    stepper = new_stepper;
                    eprintln!("[sim] Reset complete");
                }
                Err(e) => eprintln!("[sim] Reset failed: {e}"),
            }
            rc = RcInput::default();
            continue;
        }

        // Map RC sticks to physical commands
        // Throttle: 0 stick = hover, +1 = 2x hover thrust, -1 = zero thrust
        let thrust = HOVER_THRUST * (1.0 + rc.throttle).max(0.0);
        let roll_cmd = rc.roll * MAX_ANGLE;
        let pitch_cmd = rc.pitch * MAX_ANGLE;
        let yaw_cmd = rc.yaw * MAX_YAW_RATE;

        let _ = stepper.set_input("cmd_thrust", thrust);
        let _ = stepper.set_input("cmd_roll", roll_cmd);
        let _ = stepper.set_input("cmd_pitch", pitch_cmd);
        let _ = stepper.set_input("cmd_yaw", yaw_cmd);

        if let Err(e) = stepper.step(DT) {
            eprintln!("Step error: {e}");
        }

        frame_count += 1;
        if frame_count.is_multiple_of(50) {
            eprintln!(
                "[sim] t={:.1} alt={:.2}m roll={:.1}° pitch={:.1}° T={:.1}N  stick=[T:{:.2} R:{:.2} P:{:.2} Y:{:.2}]",
                stepper.time(),
                -stepper.get("pz").unwrap_or(0.0),
                stepper.get("roll").unwrap_or(0.0).to_degrees(),
                stepper.get("pitch").unwrap_or(0.0).to_degrees(),
                stepper.get("T").unwrap_or(0.0),
                rc.throttle,
                rc.roll,
                rc.pitch,
                rc.yaw,
            );
        }

        // Build state JSON
        let state_json = serde_json::json!({
            "t": stepper.time(),
            "px": stepper.get("px").unwrap_or(0.0),
            "py": stepper.get("py").unwrap_or(0.0),
            "pz": stepper.get("pz").unwrap_or(0.0),
            "vx": stepper.get("vx").unwrap_or(0.0),
            "vy": stepper.get("vy").unwrap_or(0.0),
            "vz": stepper.get("vz").unwrap_or(0.0),
            "q0": stepper.get("q0").unwrap_or(1.0),
            "q1": stepper.get("q1").unwrap_or(0.0),
            "q2": stepper.get("q2").unwrap_or(0.0),
            "q3": stepper.get("q3").unwrap_or(0.0),
            "T": stepper.get("T").unwrap_or(0.0),
            "roll": stepper.get("roll").unwrap_or(0.0),
            "pitch": stepper.get("pitch").unwrap_or(0.0),
        });
        let _ = state_tx.send(state_json.to_string());

        // Wait for remainder of frame
        let elapsed = frame_start.elapsed();
        let target = Duration::from_secs_f64(DT);
        if elapsed < target {
            thread::sleep(target - elapsed);
        }
    }
}
