//! Quadrotor SIL (Software-in-the-Loop) plant simulator.
//!
//! Receives motor commands from Cerebri via UDP (48-byte flatbuffer),
//! steps 6-DOF quadrotor physics, sends back sensor data as a
//! 164-byte flight_snapshot flatbuffer, and streams visualization
//! to a Three.js browser viewer.
//!
//! Usage:
//!   cargo run --example quadrotor_sil -p rumoca
//!   Then open http://localhost:8080 in a browser.
//!
//! UDP ports (configurable via env vars):
//!   SIL_UDP_LISTEN=0.0.0.0:4243   — listen for motor_output from Cerebri
//!   SIL_UDP_SEND=192.0.2.1:4242   — send flight_snapshot to Cerebri
//!   SIL_DT=0.004                   — simulation timestep in seconds

use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, UdpSocket};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use rumoca_sim::{SimStepper, StepperOptions};
use tungstenite::{Message, accept};

const HTTP_PORT: u16 = 8080;
const WS_PORT: u16 = 8081;
const MAX_SUB_DT: f64 = 0.002;

const MODEL_SOURCE: &str = include_str!("../../../examples/quadrotor_sil/QuadrotorSIL.mo");
const THREE_JS: &str = include_str!("../../rumoca-sim/web/three.min.js");

// Physical constants matching the Modelica model
const MASS: f64 = 2.0;
const G: f64 = 9.80665;
const CT: f64 = 8.5e-6;

/// Max motor angular velocity [rad/s] for normalised→rad/s conversion.
/// motors[i] in [0,1] maps to [0, OMEGA_MAX].
const OMEGA_MAX: f64 = 1100.0;

fn hover_omega() -> f64 {
    (MASS * G / (4.0 * CT)).sqrt()
}

// ===========================================================================
// Cerebri flatbuffer protocol (Rust port of topic_flatbuffer.c)
// ===========================================================================

mod cerebri_fb {
    // --- Motor output (Cerebri → SIL): 48 bytes ---
    pub(super) const MOTOR_OUTPUT_SIZE: usize = 48;
    const MOTOR_VTABLE_OFFSET: usize = 4;
    const MOTOR_VTABLE_SIZE: u16 = 12;
    const MOTOR_TABLE_OFFSET: usize = MOTOR_VTABLE_OFFSET + MOTOR_VTABLE_SIZE as usize; // 16
    const MOTOR_OBJECT_SIZE: u16 = 32;
    const MOTOR_FIELD_MOTORS: u16 = 4;
    const MOTOR_FIELD_RAW: u16 = 20;
    const MOTOR_FIELD_ARMED: u16 = 28;
    const MOTOR_FIELD_TEST_MODE: u16 = 29;

    // --- Flight snapshot (SIL → Cerebri): 164 bytes ---
    #[allow(dead_code)]
    pub(super) const FLIGHT_SNAPSHOT_SIZE: usize = 164;
    const FLIGHT_VTABLE_OFFSET: usize = 4;
    const FLIGHT_VTABLE_SIZE: u16 = 16;
    const FLIGHT_TABLE_OFFSET: usize = FLIGHT_VTABLE_OFFSET + FLIGHT_VTABLE_SIZE as usize; // 20
    const FLIGHT_OBJECT_SIZE: u16 = 144;
    const FLIGHT_FIELD_GYRO: u16 = 4;
    const FLIGHT_FIELD_ACCEL: u16 = 16;
    const FLIGHT_FIELD_RC: u16 = 28;
    const FLIGHT_FIELD_STATUS: u16 = 96;
    const FLIGHT_FIELD_RATE_DESIRED: u16 = 120;
    const FLIGHT_FIELD_RATE_CMD: u16 = 132;

    // Status field offsets within the status struct region
    const STATUS_RC_STAMP_MS: usize = 0;
    const STATUS_THROTTLE_US: usize = 8;
    const STATUS_RC_LINK_QUALITY: usize = 12;
    const STATUS_ARMED: usize = 13;
    const STATUS_RC_VALID: usize = 14;
    #[allow(dead_code)]
    const STATUS_RC_STALE: usize = 15;
    const STATUS_IMU_OK: usize = 16;
    #[allow(dead_code)]
    const STATUS_ARM_SWITCH: usize = 17;

    // --- Data structs ---

    #[derive(Debug, Clone, Default)]
    pub(super) struct MotorOutput {
        pub motors: [f32; 4], // normalised 0..1
        pub raw: [u16; 4],    // PWM microseconds
        pub armed: bool,
        pub test_mode: bool,
    }

    #[derive(Debug, Clone, Default)]
    pub(super) struct FlightSnapshot {
        pub gyro_rad_s: [f32; 3],
        pub accel_m_s2: [f32; 3],
        pub rc_us: [i32; 16],
        pub rc_stamp_ms: i64,
        pub throttle_us: i32,
        pub rc_link_quality: u8,
        pub armed: bool,
        pub rc_valid: bool,
        pub imu_ok: bool,
        pub rate_desired: [f32; 3], // roll, pitch, yaw
        pub rate_cmd: [f32; 3],
    }

    // --- Little-endian helpers ---

    fn get_le16(buf: &[u8]) -> u16 {
        u16::from_le_bytes([buf[0], buf[1]])
    }
    fn get_le32(buf: &[u8]) -> u32 {
        u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]])
    }
    fn get_float_le(buf: &[u8]) -> f32 {
        f32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]])
    }

    fn put_le16(buf: &mut [u8], v: u16) {
        buf[..2].copy_from_slice(&v.to_le_bytes());
    }
    fn put_le32(buf: &mut [u8], v: u32) {
        buf[..4].copy_from_slice(&v.to_le_bytes());
    }
    fn put_le64(buf: &mut [u8], v: u64) {
        buf[..8].copy_from_slice(&v.to_le_bytes());
    }
    fn put_float_le(buf: &mut [u8], v: f32) {
        buf[..4].copy_from_slice(&v.to_le_bytes());
    }

    // --- Unpack motor_output (48 bytes → MotorOutput) ---

    pub(super) fn unpack_motor_output(buf: &[u8]) -> Option<MotorOutput> {
        if buf.len() != MOTOR_OUTPUT_SIZE {
            return None;
        }
        // Validate layout
        let root_offset = get_le32(&buf[0..4]);
        if root_offset != MOTOR_TABLE_OFFSET as u32 {
            return None;
        }
        let table = MOTOR_TABLE_OFFSET;
        let vtable_distance = get_le32(&buf[table..table + 4]);
        if vtable_distance != (MOTOR_TABLE_OFFSET - MOTOR_VTABLE_OFFSET) as u32 {
            return None;
        }
        let vt = MOTOR_VTABLE_OFFSET;
        if get_le16(&buf[vt..]) != MOTOR_VTABLE_SIZE
            || get_le16(&buf[vt + 2..]) != MOTOR_OBJECT_SIZE
            || get_le16(&buf[vt + 4..]) != MOTOR_FIELD_MOTORS
            || get_le16(&buf[vt + 6..]) != MOTOR_FIELD_RAW
            || get_le16(&buf[vt + 8..]) != MOTOR_FIELD_ARMED
            || get_le16(&buf[vt + 10..]) != MOTOR_FIELD_TEST_MODE
        {
            return None;
        }

        let mut out = MotorOutput::default();
        let m = table + MOTOR_FIELD_MOTORS as usize;
        for i in 0..4 {
            out.motors[i] = get_float_le(&buf[m + i * 4..]);
        }
        let r = table + MOTOR_FIELD_RAW as usize;
        for i in 0..4 {
            out.raw[i] = get_le16(&buf[r + i * 2..]);
        }
        out.armed = buf[table + MOTOR_FIELD_ARMED as usize] != 0;
        out.test_mode = buf[table + MOTOR_FIELD_TEST_MODE as usize] != 0;
        Some(out)
    }

    // --- Pack flight_snapshot (FlightSnapshot → 164 bytes) ---

    pub(super) fn pack_flight_snapshot(snap: &FlightSnapshot) -> [u8; FLIGHT_SNAPSHOT_SIZE] {
        let mut buf = [0u8; FLIGHT_SNAPSHOT_SIZE];

        // Root offset
        put_le32(&mut buf[0..4], FLIGHT_TABLE_OFFSET as u32);

        // Vtable
        let vt = FLIGHT_VTABLE_OFFSET;
        put_le16(&mut buf[vt..], FLIGHT_VTABLE_SIZE);
        put_le16(&mut buf[vt + 2..], FLIGHT_OBJECT_SIZE);
        put_le16(&mut buf[vt + 4..], FLIGHT_FIELD_GYRO);
        put_le16(&mut buf[vt + 6..], FLIGHT_FIELD_ACCEL);
        put_le16(&mut buf[vt + 8..], FLIGHT_FIELD_RC);
        put_le16(&mut buf[vt + 10..], FLIGHT_FIELD_STATUS);
        put_le16(&mut buf[vt + 12..], FLIGHT_FIELD_RATE_DESIRED);
        put_le16(&mut buf[vt + 14..], FLIGHT_FIELD_RATE_CMD);

        let t = FLIGHT_TABLE_OFFSET;
        // Vtable distance
        put_le32(
            &mut buf[t..t + 4],
            (FLIGHT_TABLE_OFFSET - FLIGHT_VTABLE_OFFSET) as u32,
        );

        // Gyro (3 floats)
        let g = t + FLIGHT_FIELD_GYRO as usize;
        for i in 0..3 {
            put_float_le(&mut buf[g + i * 4..], snap.gyro_rad_s[i]);
        }

        // Accel (3 floats)
        let a = t + FLIGHT_FIELD_ACCEL as usize;
        for i in 0..3 {
            put_float_le(&mut buf[a + i * 4..], snap.accel_m_s2[i]);
        }

        // RC channels (16 x int32)
        let rc = t + FLIGHT_FIELD_RC as usize;
        for i in 0..16 {
            put_le32(&mut buf[rc + i * 4..], snap.rc_us[i] as u32);
        }

        // Status
        let s = t + FLIGHT_FIELD_STATUS as usize;
        put_le64(&mut buf[s + STATUS_RC_STAMP_MS..], snap.rc_stamp_ms as u64);
        put_le32(&mut buf[s + STATUS_THROTTLE_US..], snap.throttle_us as u32);
        buf[s + STATUS_RC_LINK_QUALITY] = snap.rc_link_quality;
        buf[s + STATUS_ARMED] = snap.armed as u8;
        buf[s + STATUS_RC_VALID] = snap.rc_valid as u8;
        buf[s + STATUS_IMU_OK] = snap.imu_ok as u8;

        // Rate desired (3 floats: roll, pitch, yaw)
        let rd = t + FLIGHT_FIELD_RATE_DESIRED as usize;
        for i in 0..3 {
            put_float_le(&mut buf[rd + i * 4..], snap.rate_desired[i]);
        }

        // Rate cmd (3 floats)
        let rc2 = t + FLIGHT_FIELD_RATE_CMD as usize;
        for i in 0..3 {
            put_float_le(&mut buf[rc2 + i * 4..], snap.rate_cmd[i]);
        }

        buf
    }
}

// ===========================================================================
// SIL API
// ===========================================================================

#[derive(Debug, Clone, Copy)]
pub struct SensorOutput {
    pub clock_sec: f64,
    pub accel: [f64; 3],
    pub gyro: [f64; 3],
    pub mag: [f64; 3],
    pub position_ned: [f64; 3],
    pub velocity_ned: [f64; 3],
    pub quaternion: [f64; 4],
}

pub struct QuadrotorSil {
    stepper: SimStepper,
    model_source: String,
}

impl QuadrotorSil {
    pub fn new() -> anyhow::Result<Self> {
        let source = MODEL_SOURCE.to_string();
        let compiler = rumoca::Compiler::new().model("QuadrotorSIL");
        let result = compiler.compile_str(&source, "QuadrotorSIL.mo")?;
        let stepper = SimStepper::new(
            &result.dae,
            StepperOptions {
                rtol: 1e-4,
                atol: 1e-4,
                ..Default::default()
            },
        )?;
        Ok(Self {
            stepper,
            model_source: source,
        })
    }

    /// Step physics with motor angular velocities [rad/s] to target clock.
    pub fn receive_motors(
        &mut self,
        motor_rpms: [f64; 4],
        clock_sec: f64,
    ) -> anyhow::Result<SensorOutput> {
        let _ = self.stepper.set_input("omega_m1", motor_rpms[0]);
        let _ = self.stepper.set_input("omega_m2", motor_rpms[1]);
        let _ = self.stepper.set_input("omega_m3", motor_rpms[2]);
        let _ = self.stepper.set_input("omega_m4", motor_rpms[3]);

        let current_time = self.stepper.time();
        let dt = clock_sec - current_time;
        if dt > 0.0 {
            let n_steps = ((dt / MAX_SUB_DT).ceil() as usize).max(1);
            let sub_dt = dt / n_steps as f64;
            for _ in 0..n_steps {
                self.stepper.step(sub_dt)?;
            }
        }
        Ok(self.read_sensors())
    }

    pub fn read_sensors(&self) -> SensorOutput {
        let get = |name: &str| self.stepper.get(name).unwrap_or(0.0);
        SensorOutput {
            clock_sec: self.stepper.time(),
            accel: [get("accel_x"), get("accel_y"), get("accel_z")],
            gyro: [get("gyro_x"), get("gyro_y"), get("gyro_z")],
            mag: [get("mag_x"), get("mag_y"), get("mag_z")],
            position_ned: [get("px"), get("py"), get("pz")],
            velocity_ned: [get("vx"), get("vy"), get("vz")],
            quaternion: [
                self.stepper.get("q0").unwrap_or(1.0),
                get("q1"),
                get("q2"),
                get("q3"),
            ],
        }
    }

    pub fn state_json(&self) -> String {
        let get = |name: &str| self.stepper.get(name).unwrap_or(0.0);
        serde_json::json!({
            "t": self.stepper.time(),
            "px": get("px"), "py": get("py"), "pz": get("pz"),
            "vx": get("vx"), "vy": get("vy"), "vz": get("vz"),
            "q0": self.stepper.get("q0").unwrap_or(1.0),
            "q1": get("q1"), "q2": get("q2"), "q3": get("q3"),
            "R11": get("R11"), "R12": get("R12"), "R13": get("R13"),
            "R21": get("R21"), "R22": get("R22"), "R23": get("R23"),
            "R31": get("R31"), "R32": get("R32"), "R33": get("R33"),
            "omega_m1": get("omega_m1"), "omega_m2": get("omega_m2"),
            "omega_m3": get("omega_m3"), "omega_m4": get("omega_m4"),
            "T": get("T"),
            "accel_x": get("accel_x"), "accel_y": get("accel_y"), "accel_z": get("accel_z"),
            "gyro_x": get("gyro_x"), "gyro_y": get("gyro_y"), "gyro_z": get("gyro_z"),
            "mag_x": get("mag_x"), "mag_y": get("mag_y"), "mag_z": get("mag_z"),
        })
        .to_string()
    }

    pub fn reset(&mut self) -> anyhow::Result<()> {
        let compiler = rumoca::Compiler::new().model("QuadrotorSIL");
        let result = compiler.compile_str(&self.model_source, "QuadrotorSIL.mo")?;
        self.stepper = SimStepper::new(
            &result.dae,
            StepperOptions {
                rtol: 1e-4,
                atol: 1e-4,
                ..Default::default()
            },
        )?;
        Ok(())
    }

    pub fn time(&self) -> f64 {
        self.stepper.time()
    }

    /// Convert SensorOutput to a cerebri flight_snapshot flatbuffer.
    fn sensor_to_snapshot(
        &self,
        sensors: &SensorOutput,
        armed: bool,
    ) -> cerebri_fb::FlightSnapshot {
        let time_ms = (sensors.clock_sec * 1000.0) as i64;
        cerebri_fb::FlightSnapshot {
            gyro_rad_s: [
                sensors.gyro[0] as f32,
                sensors.gyro[1] as f32,
                sensors.gyro[2] as f32,
            ],
            accel_m_s2: [
                sensors.accel[0] as f32,
                sensors.accel[1] as f32,
                sensors.accel[2] as f32,
            ],
            rc_us: [0; 16],
            rc_stamp_ms: time_ms,
            throttle_us: 0,
            rc_link_quality: 255,
            armed,
            rc_valid: false,
            imu_ok: true,
            rate_desired: [
                sensors.gyro[0] as f32,
                sensors.gyro[1] as f32,
                sensors.gyro[2] as f32,
            ],
            rate_cmd: [
                sensors.gyro[0] as f32,
                sensors.gyro[1] as f32,
                sensors.gyro[2] as f32,
            ],
        }
    }
}

// ===========================================================================
// HTML / Three.js viewer
// ===========================================================================

const HTML_PAGE: &str = r##"<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Quadrotor SIL Simulator</title>
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
  #hud .sensor { color: #0cf; }
  #status {
    position: fixed; top: 10px; right: 10px; z-index: 10;
    background: rgba(0,0,0,0.7); padding: 6px 12px; border-radius: 6px;
    font-size: 12px;
  }
  .connected { color: #0f0; }
  .disconnected { color: #f00; }
  #mode {
    position: fixed; bottom: 10px; left: 10px; z-index: 10;
    background: rgba(0,0,0,0.7); padding: 8px 14px; border-radius: 6px;
    font-size: 12px; line-height: 1.5;
  }
</style>
</head>
<body>
<div id="container"></div>
<div id="hud">
  <b>SIL Plant Simulator</b><br>
  <span class="label">t:</span> <span class="val" id="v-t">0.00</span>s<br>
  <span class="label">pos NED:</span> <span class="val" id="v-pos">0, 0, 0</span><br>
  <span class="label">alt:</span> <span class="val" id="v-alt">0.0</span>m<br>
  <span class="label">vel:</span> <span class="val" id="v-vel">0.0</span> m/s<br>
  <span class="label">roll:</span> <span class="val" id="v-roll">0.0</span>&deg;
  <span class="label">pitch:</span> <span class="val" id="v-pitch">0.0</span>&deg;<br>
  <hr style="border-color:#333; margin:4px 0">
  <span class="label">motors:</span> <span class="sensor" id="v-motors">0, 0, 0, 0</span> rad/s<br>
  <span class="label">thrust:</span> <span class="sensor" id="v-thrust">0.0</span> N<br>
  <span class="label">accel:</span> <span class="sensor" id="v-accel">0, 0, 0</span><br>
  <span class="label">gyro:</span> <span class="sensor" id="v-gyro">0, 0, 0</span><br>
  <span class="label">mag:</span> <span class="sensor" id="v-mag">0, 0, 0</span>
</div>
<div id="mode" id="v-mode">
  <b>Mode:</b> <span id="v-mode-text">Self-test (hover)</span>
</div>
<div id="status"><span class="disconnected" id="ws-status">Connecting...</span></div>

<script>
__THREE_JS__
</script>
<script>
function nedToThree(px, py, pz) { return [py, -pz, px]; }

const container = document.getElementById("container");
const scene = new THREE.Scene();
scene.background = new THREE.Color(0x0a0f14);
const camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.1, 500);
camera.position.set(2, 3, 5);
camera.lookAt(0, 0, 0);
const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
container.appendChild(renderer.domElement);
window.addEventListener("resize", () => {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
});

let camAngle = 0.8, camElev = 0.5, camDist = 4;
let camTarget = new THREE.Vector3(0, 1, 0);
container.addEventListener("wheel", (e) => { camDist = Math.max(1, Math.min(20, camDist + e.deltaY * 0.005)); });
let dragging = false, lastMX = 0, lastMY = 0;
container.addEventListener("mousedown", (e) => { dragging = true; lastMX = e.clientX; lastMY = e.clientY; });
container.addEventListener("mouseup", () => { dragging = false; });
container.addEventListener("mousemove", (e) => {
  if (!dragging) return;
  camAngle -= (e.clientX - lastMX) * 0.005;
  camElev = Math.max(-1.2, Math.min(1.5, camElev + (e.clientY - lastMY) * 0.005));
  lastMX = e.clientX; lastMY = e.clientY;
});

const key = new THREE.DirectionalLight(0xffffff, 1.2); key.position.set(5, 10, 5); scene.add(key);
const fill = new THREE.DirectionalLight(0x8ec8f0, 0.4); fill.position.set(-4, 6, -3); scene.add(fill);
scene.add(new THREE.AmbientLight(0x404860, 0.6));
scene.add(new THREE.GridHelper(40, 40, 0x1a3a4a, 0x151515));
const floor = new THREE.Mesh(new THREE.PlaneGeometry(40, 40), new THREE.MeshStandardMaterial({ color: 0x1a1a1a, roughness: 0.9 }));
floor.rotation.x = -Math.PI / 2; floor.position.y = -0.01; scene.add(floor);

const quad = new THREE.Group(); quad.name = "quadrotor";
const armLen = 0.25, armRadius = 0.012, bodyR = 0.06, bodyH = 0.035;
const motorR = 0.025, motorH = 0.03, propR = 0.13, legH = 0.06, legRadius = 0.006;
const bodyMat = new THREE.MeshStandardMaterial({ color: 0x2a2a2a, roughness: 0.3, metalness: 0.7 });
const armMat = new THREE.MeshStandardMaterial({ color: 0x333333, roughness: 0.4, metalness: 0.6 });
const motorMat = new THREE.MeshStandardMaterial({ color: 0x555555, roughness: 0.3, metalness: 0.8 });
const propMatCW = new THREE.MeshStandardMaterial({ color: 0x00ccff, roughness: 0.4, metalness: 0.3, transparent: true, opacity: 0.6 });
const propMatCCW = new THREE.MeshStandardMaterial({ color: 0xff6633, roughness: 0.4, metalness: 0.3, transparent: true, opacity: 0.6 });
const legMat = new THREE.MeshStandardMaterial({ color: 0x444444, roughness: 0.5, metalness: 0.5 });
quad.add(new THREE.Mesh(new THREE.CylinderGeometry(bodyR, bodyR, bodyH, 16), bodyMat));
const top = new THREE.Mesh(new THREE.CylinderGeometry(bodyR*1.1, bodyR*1.1, 0.006, 16), new THREE.MeshStandardMaterial({ color: 0x1a1a1a, roughness: 0.2, metalness: 0.9 }));
top.position.y = bodyH/2 + 0.003; quad.add(top);

const motorAngles = [Math.PI/4, 3*Math.PI/4, 5*Math.PI/4, 7*Math.PI/4];
const propMats = [propMatCW, propMatCCW, propMatCW, propMatCCW];
const propMeshes = [];
motorAngles.forEach((angle, i) => {
  const mx = armLen*Math.cos(angle), mz = armLen*Math.sin(angle);
  const arm = new THREE.Mesh(new THREE.CylinderGeometry(armRadius, armRadius, armLen, 8), armMat);
  arm.quaternion.setFromAxisAngle(new THREE.Vector3(0,1,0), angle); arm.rotateZ(Math.PI/2);
  arm.position.set(mx/2, 0, mz/2); quad.add(arm);
  const motor = new THREE.Mesh(new THREE.CylinderGeometry(motorR, motorR*0.8, motorH, 12), motorMat);
  motor.position.set(mx, motorH/2, mz); quad.add(motor);
  const prop = new THREE.Mesh(new THREE.CylinderGeometry(propR, propR, 0.004, 24), propMats[i]);
  prop.position.set(mx, motorH+0.005, mz); quad.add(prop); propMeshes.push(prop);
  const leg = new THREE.Mesh(new THREE.CylinderGeometry(legRadius, legRadius*0.7, legH, 6), legMat);
  leg.position.set(mx, -bodyH/2-legH/2, mz); quad.add(leg);
  const foot = new THREE.Mesh(new THREE.SphereGeometry(legRadius*1.5, 6, 4), legMat);
  foot.position.set(mx, -bodyH/2-legH, mz); quad.add(foot);
});
const ledF = new THREE.Mesh(new THREE.SphereGeometry(0.01, 8, 6), new THREE.MeshStandardMaterial({ color: 0x00ff44, emissive: 0x00ff44, emissiveIntensity: 2 }));
ledF.position.set(0, 0, -bodyR-0.01); quad.add(ledF);
const ledB = new THREE.Mesh(new THREE.SphereGeometry(0.01, 8, 6), new THREE.MeshStandardMaterial({ color: 0xff0000, emissive: 0xff0000, emissiveIntensity: 2 }));
ledB.position.set(0, 0, bodyR+0.01); quad.add(ledB);
scene.add(quad);

const shadow = new THREE.Mesh(new THREE.CircleGeometry(0.15, 16), new THREE.MeshBasicMaterial({ color: 0x000000, transparent: true, opacity: 0.2 }));
shadow.rotation.x = -Math.PI/2; shadow.position.y = 0.003; scene.add(shadow);
const maxTrailPts = 800;
const trailGeo = new THREE.BufferGeometry();
const trailPos = new Float32Array(maxTrailPts * 3);
trailGeo.setAttribute("position", new THREE.BufferAttribute(trailPos, 3));
trailGeo.setDrawRange(0, 0);
const trail = new THREE.Line(trailGeo, new THREE.LineBasicMaterial({ color: 0x00ccff, transparent: true, opacity: 0.7 }));
scene.add(trail); let trailCount = 0;

let ws = null, latestState = null;
function connectWS() {
  ws = new WebSocket("ws://localhost:__WS_PORT__");
  ws.onopen = () => { document.getElementById("ws-status").textContent = "Connected"; document.getElementById("ws-status").className = "connected"; };
  ws.onclose = () => { document.getElementById("ws-status").textContent = "Disconnected"; document.getElementById("ws-status").className = "disconnected"; setTimeout(connectWS, 1000); };
  ws.onmessage = (e) => { latestState = JSON.parse(e.data); };
}
connectWS();

function animate() {
  requestAnimationFrame(animate);
  if (latestState) {
    const s = latestState;
    const px = s.px||0, py = s.py||0, pz = s.pz||0;
    const [tx, ty, tz] = nedToThree(px, py, pz);
    quad.position.set(tx, ty, tz);
    const R11=s.R11||1,R12=s.R12||0,R13=s.R13||0,R21=s.R21||0,R22=s.R22||1,R23=s.R23||0,R31=s.R31||0,R32=s.R32||0,R33=s.R33||1;
    const m=quad.matrix, e=m.elements;
    e[0]=R22;e[1]=-R32;e[2]=R12;e[3]=0; e[4]=-R23;e[5]=R33;e[6]=-R13;e[7]=0;
    e[8]=R21;e[9]=-R31;e[10]=R11;e[11]=0; e[12]=tx;e[13]=ty;e[14]=tz;e[15]=1;
    quad.matrixAutoUpdate=false; quad.matrixWorldNeedsUpdate=true;
    const omegas=[s.omega_m1||0,s.omega_m2||0,s.omega_m3||0,s.omega_m4||0];
    propMeshes.forEach((p,i)=>{const dir=(i%2===0)?1:-1;p.rotation.y+=dir*Math.min(omegas[i]/500,2.0)*0.5;});
    shadow.position.set(tx,0.003,tz);
    const alt=Math.max(ty,0.01),sc=Math.max(0.4,1.2-alt*0.04);
    shadow.scale.set(sc,sc,1); shadow.material.opacity=Math.max(0.03,0.25-alt*0.015);
    const idx2=trailCount%maxTrailPts;
    trailPos[idx2*3]=tx;trailPos[idx2*3+1]=ty;trailPos[idx2*3+2]=tz;trailCount++;
    trail.geometry.attributes.position.needsUpdate=true;trail.geometry.setDrawRange(0,Math.min(trailCount,maxTrailPts));
    camTarget.lerp(new THREE.Vector3(tx,ty,tz),0.05);
    document.getElementById("v-t").textContent=(s.t||0).toFixed(2);
    document.getElementById("v-pos").textContent=`${px.toFixed(2)}, ${py.toFixed(2)}, ${pz.toFixed(2)}`;
    document.getElementById("v-alt").textContent=(-pz).toFixed(2);
    document.getElementById("v-vel").textContent=Math.sqrt((s.vx||0)**2+(s.vy||0)**2+(s.vz||0)**2).toFixed(2);
    const q0=s.q0||1,q1=s.q1||0,q2=s.q2||0,q3=s.q3||0;
    document.getElementById("v-roll").textContent=(Math.atan2(2*(q0*q1+q2*q3),1-2*(q1*q1+q2*q2))*180/Math.PI).toFixed(1);
    document.getElementById("v-pitch").textContent=(Math.asin(Math.max(-1,Math.min(1,2*(q0*q2-q3*q1))))*180/Math.PI).toFixed(1);
    document.getElementById("v-motors").textContent=omegas.map(o=>o.toFixed(0)).join(", ");
    document.getElementById("v-thrust").textContent=(s.T||0).toFixed(2);
    document.getElementById("v-accel").textContent=`${(s.accel_x||0).toFixed(2)}, ${(s.accel_y||0).toFixed(2)}, ${(s.accel_z||0).toFixed(2)}`;
    document.getElementById("v-gyro").textContent=`${(s.gyro_x||0).toFixed(2)}, ${(s.gyro_y||0).toFixed(2)}, ${(s.gyro_z||0).toFixed(2)}`;
    document.getElementById("v-mag").textContent=`${(s.mag_x||0).toFixed(3)}, ${(s.mag_y||0).toFixed(3)}, ${(s.mag_z||0).toFixed(3)}`;
    if (s.mode) document.getElementById("v-mode-text").textContent = s.mode;
  }
  camera.position.set(camTarget.x+camDist*Math.sin(camAngle)*Math.cos(camElev),camTarget.y+camDist*Math.sin(camElev),camTarget.z+camDist*Math.cos(camAngle)*Math.cos(camElev));
  camera.lookAt(camTarget); renderer.render(scene, camera);
}
animate();
</script>
</body>
</html>"##;

// ===========================================================================
// HTTP server
// ===========================================================================

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

// ===========================================================================
// Main
// ===========================================================================

fn main() -> anyhow::Result<()> {
    // Configuration from environment
    let udp_listen = std::env::var("SIL_UDP_LISTEN").unwrap_or_default();
    let udp_send = std::env::var("SIL_UDP_SEND").unwrap_or_default();
    let dt: f64 = std::env::var("SIL_DT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.004); // 250 Hz default

    let udp_mode = !udp_listen.is_empty() && !udp_send.is_empty();

    eprintln!("Compiling QuadrotorSIL model...");
    let mut sil = QuadrotorSil::new()?;
    eprintln!("Inputs: {:?}", sil.stepper.input_names());

    let omega_hover = hover_omega();
    eprintln!(
        "Hover omega: {:.1} rad/s ({:.0} RPM)",
        omega_hover,
        omega_hover * 60.0 / (2.0 * std::f64::consts::PI)
    );

    if udp_mode {
        eprintln!(
            "UDP mode: listen={} send={} dt={}s",
            udp_listen, udp_send, dt
        );
    } else {
        eprintln!("Self-test mode (no UDP). Set SIL_UDP_LISTEN and SIL_UDP_SEND to enable.");
        eprintln!("  Example: SIL_UDP_LISTEN=0.0.0.0:4243 SIL_UDP_SEND=192.0.2.1:4242");
    }

    // Prepare HTML
    let html = HTML_PAGE
        .replace("__THREE_JS__", THREE_JS)
        .replace("__WS_PORT__", &WS_PORT.to_string());

    // Start HTTP server
    let http_listener = TcpListener::bind(format!("0.0.0.0:{HTTP_PORT}"))?;
    eprintln!("HTTP server: http://localhost:{HTTP_PORT}");
    thread::spawn(move || serve_http(http_listener, html));

    // WebSocket thread for viz
    let (state_tx, state_rx) = mpsc::channel::<String>();
    thread::spawn(move || {
        let ws_listener = TcpListener::bind(format!("0.0.0.0:{WS_PORT}")).unwrap();
        eprintln!("WebSocket: ws://localhost:{WS_PORT}");
        eprintln!("\nOpen http://localhost:{HTTP_PORT} in your browser!\n");
        for stream in ws_listener.incoming() {
            let Ok(stream) = stream else { continue };
            eprintln!("Viewer connected");
            let mut ws = match accept(stream) {
                Ok(ws) => ws,
                Err(e) => {
                    eprintln!("WS error: {e}");
                    continue;
                }
            };
            ws.get_ref().set_nonblocking(true).ok();
            loop {
                loop {
                    match ws.read() {
                        Ok(Message::Close(_)) => break,
                        Err(tungstenite::Error::Io(ref e))
                            if e.kind() == std::io::ErrorKind::WouldBlock =>
                        {
                            break;
                        }
                        Err(_) => break,
                        _ => {}
                    }
                }
                if let Ok(json) = state_rx.try_recv() {
                    let mut latest = json;
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

    if udp_mode {
        // ===== UDP mode: receive motor_output, step, send flight_snapshot =====
        let socket = UdpSocket::bind(&udp_listen)?;
        socket.set_read_timeout(Some(Duration::from_millis(100)))?;
        eprintln!("Listening for motor_output on {udp_listen}");
        eprintln!("Sending flight_snapshot to {udp_send}");

        let mut recv_buf = [0u8; 256];
        let mut armed;
        let mut pkt_count = 0u64;

        loop {
            let frame_start = Instant::now();

            // Try to receive a motor_output packet
            match socket.recv_from(&mut recv_buf) {
                Ok((n, _src)) => {
                    if let Some(motor_out) = cerebri_fb::unpack_motor_output(&recv_buf[..n]) {
                        armed = motor_out.armed;

                        // Convert normalised motors [0..1] to rad/s
                        let motor_rpms = [
                            motor_out.motors[0] as f64 * OMEGA_MAX,
                            motor_out.motors[1] as f64 * OMEGA_MAX,
                            motor_out.motors[2] as f64 * OMEGA_MAX,
                            motor_out.motors[3] as f64 * OMEGA_MAX,
                        ];

                        let target_clock = sil.time() + dt;
                        match sil.receive_motors(motor_rpms, target_clock) {
                            Ok(sensors) => {
                                // Pack and send flight_snapshot
                                let snap = sil.sensor_to_snapshot(&sensors, armed);
                                let buf = cerebri_fb::pack_flight_snapshot(&snap);
                                let _ = socket.send_to(&buf, &udp_send);

                                pkt_count += 1;
                                if pkt_count % 250 == 0 {
                                    eprintln!(
                                        "[udp] t={:.1}s alt={:.2}m motors=[{:.2},{:.2},{:.2},{:.2}] armed={}",
                                        sensors.clock_sec,
                                        -sensors.position_ned[2],
                                        motor_out.motors[0],
                                        motor_out.motors[1],
                                        motor_out.motors[2],
                                        motor_out.motors[3],
                                        armed,
                                    );
                                }
                            }
                            Err(e) => eprintln!("[udp] Step error: {e}"),
                        }

                        // Stream to viz
                        let mut json = sil.state_json();
                        // Inject mode into JSON
                        json.pop(); // remove trailing }
                        json.push_str(&format!(
                            r#","mode":"UDP ({})"}}""#,
                            if armed { "ARMED" } else { "disarmed" }
                        ));
                        let _ = state_tx.send(json);
                    } else if n > 0 {
                        eprintln!(
                            "[udp] Invalid packet ({n} bytes), expected {}",
                            cerebri_fb::MOTOR_OUTPUT_SIZE
                        );
                    }
                }
                Err(ref e)
                    if e.kind() == std::io::ErrorKind::WouldBlock
                        || e.kind() == std::io::ErrorKind::TimedOut => {}
                Err(e) => eprintln!("[udp] recv error: {e}"),
            }

            let elapsed = frame_start.elapsed();
            let target = Duration::from_secs_f64(dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }
    } else {
        // ===== Self-test mode: hover =====
        eprintln!("Running self-test: hover at 1m altitude\n");
        let hover_rpms = [omega_hover; 4];
        let mut frame_count = 0u64;

        loop {
            let frame_start = Instant::now();
            let target_clock = sil.time() + dt;
            match sil.receive_motors(hover_rpms, target_clock) {
                Ok(sensors) => {
                    frame_count += 1;
                    if frame_count % 250 == 0 {
                        eprintln!(
                            "[sil] t={:.1}s alt={:.3}m accel_z={:.2} gyro=[{:.3},{:.3},{:.3}]",
                            sensors.clock_sec,
                            -sensors.position_ned[2],
                            sensors.accel[2],
                            sensors.gyro[0],
                            sensors.gyro[1],
                            sensors.gyro[2],
                        );
                    }
                }
                Err(e) => eprintln!("[sil] Step error: {e}"),
            }

            let mut json = sil.state_json();
            json.pop();
            json.push_str(r#","mode":"Self-test (hover)"}"#);
            let _ = state_tx.send(json);

            let elapsed = frame_start.elapsed();
            let target = Duration::from_secs_f64(dt);
            if elapsed < target {
                thread::sleep(target - elapsed);
            }
        }
    }
}
