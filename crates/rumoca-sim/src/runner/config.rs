//! TOML configuration model for the interactive simulation runner.
//!
//! Aggregates input, codec, transport, and pacing sections outside the
//! compiler/session layer. Lockstep is one pacing mode, not the runner's
//! architecture.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use rumoca_codec::config::{
    MessageConfig as FlatbufferMessageConfig, SchemaConfig as FlatbufferSchemaConfig,
};
use rumoca_input::config::{DeriveSpec, InputConfig, LocalDef, SignalsConfig};
use rumoca_solver::SimPacingMode;
use rumoca_transport_udp::UdpConfig;
use rumoca_transport_zenoh::ZenohConfig;

// ── Top-level ──────────────────────────────────────────────────────────────

/// Authoritative marker that a TOML file is a Rumoca task file.
///
/// The filename convention (`rumoca-scenario.toml` /
/// `rumoca-scenario.<profile>.toml`) is the discovery hook; this `[rumoca]`
/// section is the authoritative declaration. Keeping a `version` field gives a
/// clean path for future schema evolution.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RumocaMarker {
    /// Task-file schema version (currently `"1"`).
    pub version: String,
    /// Optional declared task kind (e.g. `"simulate"`, `"codegen"`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub task: Option<String>,
}

/// Whether a path's filename matches the Rumoca task-file convention:
/// `rumoca-scenario.toml` (default) or `rumoca-scenario.<profile>.toml`.
///
/// This is the editor/discovery hook only — the authoritative check is the
/// presence of the `[rumoca]` marker inside the file (see [`RumocaMarker`]).
#[must_use]
pub fn is_rumoca_task_filename(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(str::to_ascii_lowercase)
        .is_some_and(|name| {
            name == "rumoca-scenario.toml"
                || (name.starts_with("rumoca-scenario.") && name.ends_with(".toml"))
        })
}

#[derive(Debug, Deserialize)]
pub struct SimulationConfig {
    /// Required marker section declaring this a Rumoca task file.
    #[serde(default)]
    pub rumoca: Option<RumocaMarker>,

    pub sim: SimConfig,
    #[serde(default)]
    pub lockstep: Option<LockstepConfig>,

    /// Additional Modelica package roots loaded before the requested model.
    /// Paths are resolved relative to the config file by the CLI.
    #[serde(default)]
    pub source_roots: Vec<String>,

    /// FlatBuffer schema files. Required only when coupling to an external
    /// interface over UDP; standalone demos (rover etc.) omit this.
    #[serde(default)]
    pub schema: Option<FlatbufferSchemaConfig>,
    /// Incoming FB message routing. Omit for standalone mode.
    #[serde(default)]
    pub receive: Option<FlatbufferMessageConfig>,
    /// Outgoing FB message routing. Omit for standalone mode.
    #[serde(default)]
    pub send: Option<FlatbufferMessageConfig>,

    #[serde(default)]
    pub external_interface: Option<ExternalInterfaceConfig>,
    /// Complete top-level Modelica model. If a controller is needed, compose
    /// it with the plant in Modelica and point this section at that wrapper.
    #[serde(default, alias = "physics")]
    pub model: Option<ModelConfig>,
    #[serde(default)]
    controller: Option<toml::Value>,
    #[serde(default)]
    pub transport: Option<TransportConfig>,
    /// Message-name to Zenoh key mapping. Message names may be `send`,
    /// `receive`, a FlatBuffer root type, or the root type's snake_case leaf.
    #[serde(default)]
    pub publish: HashMap<String, String>,
    /// Message-name to Zenoh key mapping for inbound command messages.
    #[serde(default)]
    pub subscribe: HashMap<String, String>,
    #[serde(default)]
    pub locals: HashMap<String, LocalDef>,
    #[serde(default)]
    pub derive: HashMap<String, DeriveSpec>,
    #[serde(default)]
    pub input: Option<InputConfig>,
    #[serde(default)]
    pub signals: Option<SignalsConfig>,
    #[serde(default)]
    pub debug_log: Option<DebugLogConfig>,
    #[serde(default)]
    pub reset: Option<ResetConfig>,
    #[serde(default)]
    pub viewer: Option<ViewerConfig>,
}

impl SimulationConfig {
    pub fn http_port(&self) -> u16 {
        self.transport
            .as_ref()
            .and_then(|transport| transport.http.as_ref())
            .map(|http| http.port)
            .unwrap_or(8080)
    }

    pub fn websocket_port(&self) -> u16 {
        self.transport
            .as_ref()
            .and_then(|transport| transport.websocket.as_ref())
            .map(|websocket| websocket.port)
            .unwrap_or(8081)
    }
}

// ── Sim + external interface (orchestration-level) ─────────────────────────

#[derive(Debug, Deserialize)]
pub struct ExternalInterfaceConfig {
    pub command: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SimConfig {
    #[serde(default = "default_dt")]
    pub dt: f64,
    #[serde(default = "default_t_end")]
    pub t_end: f64,
    #[serde(default)]
    pub atol: Option<f64>,
    #[serde(default)]
    pub rtol: Option<f64>,
    #[serde(default)]
    pub solver: Option<String>,
    #[serde(default)]
    pub output: Option<String>,
    #[serde(default, rename = "test")]
    pub _test: bool,
    /// Outer-loop pacing mode.
    ///
    /// - `as_fast_as_possible`: drain available input and advance without wall-clock sleep.
    /// - `realtime`: drain available input and pace steps to wall-clock `dt`.
    /// - `lockstep`: wait for each inbound external-interface packet before stepping.
    #[serde(default)]
    pub mode: Option<SimPacingMode>,
    /// Number of `dt` runner steps to advance after each received lockstep
    /// packet. Values greater than 1 are useful for a slower external
    /// controller driving a faster plant integration rate.
    #[serde(default = "default_steps_per_packet")]
    pub steps_per_packet: usize,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LockstepConfig {
    /// Outbound sample stream rate. With the current runner this is the single
    /// configured `[send]` FlatBuffer message, e.g. fake mocap.
    pub send_rate_hz: f64,
    /// Inbound control barrier rate. The runner waits for one received packet
    /// before advancing beyond each interval.
    pub receive_rate_hz: f64,
    /// Maximum plant integration step used while advancing between scheduled
    /// send/control boundaries. Defaults to `[sim].dt`.
    #[serde(default)]
    pub max_advance_dt: Option<f64>,
}

fn default_dt() -> f64 {
    0.004
}
fn default_t_end() -> f64 {
    1.0
}
fn default_steps_per_packet() -> usize {
    1
}

// ── Model ──────────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct ModelConfig {
    pub file: String,
    pub name: String,
}

// ── Transports ─────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct TransportConfig {
    #[serde(default)]
    pub udp: Option<UdpConfig>,
    #[serde(default)]
    pub zenoh: Option<ZenohConfig>,
    #[serde(default, rename = "websocket")]
    pub websocket: Option<WebSocketConfig>,
    #[serde(default)]
    pub http: Option<HttpConfig>,
}

#[derive(Debug, Deserialize)]
pub struct WebSocketConfig {
    #[serde(rename = "port")]
    pub port: u16,
}

#[derive(Debug, Deserialize)]
pub struct HttpConfig {
    #[serde(rename = "port")]
    pub port: u16,
    #[serde(default)]
    pub scene: Option<String>,
    /// Directory served at `/assets/...`, resolved relative to the config file.
    /// Defaults to the scene script's parent directory when unset.
    #[serde(default)]
    pub asset_dir: Option<String>,
}

// ── Viewer presentation ───────────────────────────────────────────────────

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct ViewerConfig {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub prefer_external: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub status_title: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub show_armed: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub controls: Option<ViewerControlsConfig>,
    /// Named kinematic frames driven by viewer signals ([[viewer.frame]]).
    #[serde(default, rename = "frame", skip_serializing_if = "Vec::is_empty")]
    pub frames: Vec<ViewerFrameConfig>,
    /// Cameras mounted on frames ([[viewer.camera]]); the C key cycles
    /// between the scene-controlled camera and these.
    #[serde(default, rename = "camera", skip_serializing_if = "Vec::is_empty")]
    pub cameras: Vec<ViewerCameraConfig>,
    /// Opt-in heads-up overlay ([viewer.hud]); absent means no HUD.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hud: Option<ViewerHudConfig>,
}

/// A kinematic frame driven by viewer signals, expressed in model FLU
/// coordinates (x forward, y left, z up). The viewer owns the single
/// FLU-to-renderer conversion; scenes receive the resolved matrices.
#[derive(Debug, Deserialize, Serialize)]
pub struct ViewerFrameConfig {
    pub name: String,
    /// Position coordinates: signal names or numeric constants. Two entries
    /// mean planar `[x, y]` with `z = 0`.
    pub position: Vec<SignalOrConst>,
    /// Scalar-first body-to-world quaternion signal names `[q0, q1, q2, q3]`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub quaternion: Option<[String; 4]>,
    /// Planar yaw signal (radians about model +z). Mutually exclusive with
    /// `quaternion`; omitting both leaves the frame unrotated.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub heading: Option<String>,
}

/// A signal name or a numeric constant.
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SignalOrConst {
    Const(f64),
    Signal(String),
}

/// A camera rigidly mounted on a frame ([[viewer.camera]]). All vectors are
/// in the frame's model (FLU) coordinates.
#[derive(Debug, Deserialize, Serialize)]
pub struct ViewerCameraConfig {
    pub name: String,
    /// Frame the camera is mounted on; must name a [[viewer.frame]].
    pub frame: String,
    /// Mount offset (default the frame origin).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mount: Option<[f64; 3]>,
    /// View direction (default `[1, 0, 0]`, along frame forward).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub look: Option<[f64; 3]>,
    /// Camera up (default `[0, 0, 1]`, frame up).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub up: Option<[f64; 3]>,
}

/// Opt-in HUD overlay. The only built-in mode is "flight" (attitude ladder,
/// altitude/speed readouts, stick echo). Models that are not aircraft simply
/// omit this table.
#[derive(Debug, Deserialize, Serialize)]
pub struct ViewerHudConfig {
    pub mode: String,
    /// Frame supplying roll/pitch; must name a [[viewer.frame]].
    pub frame: String,
    /// Altitude readout signal (row hidden when absent).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub altitude: Option<String>,
    /// Velocity component signals for the speed readout (hidden when absent).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub speed: Option<Vec<String>>,
    /// Stick echo signals (row hidden when absent).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sticks: Option<ViewerHudSticks>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ViewerHudSticks {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub roll: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pitch: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub yaw: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub throttle: Option<String>,
}

impl ViewerFrameConfig {
    /// Shape-check one frame and collect its unrouted signal references.
    fn validate(
        &self,
        signal_routed: &dyn Fn(&str) -> bool,
        missing: &mut Vec<String>,
    ) -> anyhow::Result<()> {
        if self.position.is_empty() || self.position.len() > 3 {
            anyhow::bail!(
                "[[viewer.frame]] '{}' position needs 1-3 entries, got {}",
                self.name,
                self.position.len()
            );
        }
        if self.quaternion.is_some() && self.heading.is_some() {
            anyhow::bail!(
                "[[viewer.frame]] '{}' sets both quaternion and heading; pick one",
                self.name
            );
        }
        let position_signals = self.position.iter().filter_map(|coord| match coord {
            SignalOrConst::Signal(name) => Some(name),
            SignalOrConst::Const(_) => None,
        });
        missing.extend(
            position_signals
                .chain(self.quaternion.iter().flatten())
                .chain(self.heading.iter())
                .filter(|name| !signal_routed(name))
                .cloned(),
        );
        Ok(())
    }
}

impl ViewerHudConfig {
    /// Check the HUD mode/frame and collect unrouted signal references.
    fn validate(
        &self,
        frame_names: &[&str],
        signal_routed: &dyn Fn(&str) -> bool,
        missing: &mut Vec<String>,
    ) -> anyhow::Result<()> {
        if self.mode != "flight" {
            anyhow::bail!(
                "[viewer.hud] mode '{}' is not supported; the built-in mode is 'flight'",
                self.mode
            );
        }
        if !frame_names.contains(&self.frame.as_str()) {
            anyhow::bail!(
                "[viewer.hud] references unknown frame '{}'; declare it as [[viewer.frame]]",
                self.frame
            );
        }
        let stick_signals = self.sticks.iter().flat_map(|sticks| {
            [&sticks.roll, &sticks.pitch, &sticks.yaw, &sticks.throttle]
                .into_iter()
                .flatten()
        });
        missing.extend(
            self.altitude
                .iter()
                .chain(self.speed.iter().flatten())
                .chain(stick_signals)
                .filter(|name| !signal_routed(name))
                .cloned(),
        );
        Ok(())
    }
}

#[derive(Debug, Default, Deserialize, Serialize)]
pub struct ViewerControlsConfig {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub keyboard: Vec<ViewerControlHelpItem>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub gamepad: Vec<ViewerControlHelpItem>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ViewerControlHelpItem {
    pub keys: String,
    pub action: String,
}

// ── Debug log ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct DebugLogConfig {
    #[serde(rename = "ring_size")]
    pub _ring_size: usize,
    pub trigger_signal: String,
    pub capture: Vec<String>,
    /// Output CSV path for the trace log (default `rumoca_trace.csv`).
    #[serde(default = "default_trace_log_path")]
    pub path: String,
}

fn default_trace_log_path() -> String {
    "rumoca_trace.csv".to_string()
}

// ── Reset ──────────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct ResetConfig {
    pub on_signal: String,
    #[serde(default)]
    pub reset_locals: bool,
    #[serde(default)]
    pub reset_session: bool,
    #[serde(default)]
    pub restart_external_interface: bool,
}

// ── Loader ─────────────────────────────────────────────────────────────────

impl SimulationConfig {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let text = std::fs::read_to_string(path)?;
        let config: SimulationConfig = toml::from_str(&text)?;
        if config.rumoca.is_none() {
            anyhow::bail!(
                "'{}' is not a Rumoca task file: missing the required `[rumoca]` marker section. \
                 Add:\n\n[rumoca]\nversion = \"1\"\n\nand name task files `rumoca-scenario.toml` or \
                 `rumoca-scenario.<profile>.toml` (e.g. `rumoca-scenario.f16.toml`).",
                path.display()
            );
        }
        config.validate()?;
        Ok(config)
    }

    /// True when a `[transport.zenoh]` section is configured.
    fn has_zenoh_transport(&self) -> bool {
        self.transport
            .as_ref()
            .is_some_and(|transport| transport.zenoh.is_some())
    }

    /// The three FB sections must all be present (external coupling) or all
    /// absent (standalone). Any mix is a user error.
    fn validate(&self) -> anyhow::Result<()> {
        if self.sim.dt <= 0.0 || !self.sim.dt.is_finite() {
            anyhow::bail!("[sim] dt must be a positive finite number");
        }
        if self.sim.t_end <= 0.0 || !self.sim.t_end.is_finite() {
            anyhow::bail!("[sim] t_end must be a positive finite number");
        }
        validate_positive_finite_option("[sim] atol", self.sim.atol)?;
        validate_positive_finite_option("[sim] rtol", self.sim.rtol)?;
        if self.sim.steps_per_packet == 0 {
            anyhow::bail!("[sim] steps_per_packet must be at least 1");
        }
        if let Some(lockstep) = &self.lockstep {
            if lockstep.send_rate_hz <= 0.0 || !lockstep.send_rate_hz.is_finite() {
                anyhow::bail!("[lockstep] send_rate_hz must be a positive finite number");
            }
            if lockstep.receive_rate_hz <= 0.0 || !lockstep.receive_rate_hz.is_finite() {
                anyhow::bail!("[lockstep] receive_rate_hz must be a positive finite number");
            }
            if let Some(max_advance_dt) = lockstep.max_advance_dt
                && (max_advance_dt <= 0.0 || !max_advance_dt.is_finite())
            {
                anyhow::bail!("[lockstep] max_advance_dt must be a positive finite number");
            }
        }
        let present = [
            ("schema", self.schema.is_some()),
            ("receive", self.receive.is_some()),
            ("send", self.send.is_some()),
        ];
        let count = present.iter().filter(|(_, b)| *b).count();
        if count != 0 && count != 3 {
            let have = present
                .iter()
                .filter(|(_, b)| *b)
                .map(|(n, _)| *n)
                .collect::<Vec<_>>()
                .join(", ");
            let missing = present
                .iter()
                .filter(|(_, b)| !*b)
                .map(|(n, _)| *n)
                .collect::<Vec<_>>()
                .join(", ");
            anyhow::bail!(
                "FB config is partial: have [{have}], missing [{missing}]. \
                 Provide all three ([schema], [receive], [send]) to enable \
                 external-interface coupling, or omit all three for standalone mode."
            );
        }
        if !self.publish.is_empty() && !self.has_zenoh_transport() {
            anyhow::bail!("[publish] requires a [transport.zenoh] section");
        }
        if !self.subscribe.is_empty() && !self.has_zenoh_transport() {
            anyhow::bail!("[subscribe] requires a [transport.zenoh] section");
        }
        self.validate_zenoh_message_maps()?;
        if self.controller.is_some() {
            anyhow::bail!(
                "[controller] is no longer part of simulation config. Compose the controller \
                 and vehicle in a Modelica model, then point [model] at that top-level class."
            );
        }
        self.validate_viewer_presentation()?;
        Ok(())
    }

    fn validate_zenoh_message_maps(&self) -> anyhow::Result<()> {
        if self.publish.is_empty() && self.subscribe.is_empty() {
            return Ok(());
        }
        if !self.has_fb() {
            anyhow::bail!(
                "[publish] and [subscribe] require complete FlatBuffer coupling: provide \
                 [schema], [send], and [receive]"
            );
        }
        let send = self.send.as_ref().expect("has_fb checked [send]");
        let receive = self.receive.as_ref().expect("has_fb checked [receive]");
        validate_zenoh_map_keys(
            "publish",
            &self.publish,
            &[
                message_aliases("send", &send.root_type),
                message_aliases("receive", &receive.root_type),
            ],
        )?;
        validate_zenoh_map_keys(
            "subscribe",
            &self.subscribe,
            &[message_aliases("receive", &receive.root_type)],
        )
    }

    /// Cross-check [[viewer.frame]], [[viewer.camera]], and [viewer.hud]:
    /// frame names are unique, orientation is at most one parametrization,
    /// camera/HUD frame references resolve, and every referenced signal is
    /// routed under [signals.viewer].
    fn validate_viewer_presentation(&self) -> anyhow::Result<()> {
        let Some(viewer) = self.viewer.as_ref() else {
            return Ok(());
        };
        let signal_routed = |name: &str| {
            self.signals
                .as_ref()
                .is_some_and(|signals| signals.viewer.contains_key(name))
        };
        let mut missing: Vec<String> = Vec::new();
        let mut frame_names: Vec<&str> = Vec::new();
        for frame in &viewer.frames {
            if frame_names.contains(&frame.name.as_str()) {
                anyhow::bail!("[[viewer.frame]] name '{}' is declared twice", frame.name);
            }
            frame_names.push(frame.name.as_str());
            frame.validate(&signal_routed, &mut missing)?;
        }
        let mut camera_names: Vec<&str> = Vec::new();
        for camera in &viewer.cameras {
            if camera_names.contains(&camera.name.as_str()) {
                anyhow::bail!("[[viewer.camera]] name '{}' is declared twice", camera.name);
            }
            camera_names.push(camera.name.as_str());
            if !frame_names.contains(&camera.frame.as_str()) {
                anyhow::bail!(
                    "[[viewer.camera]] '{}' references unknown frame '{}'; declare it as [[viewer.frame]]",
                    camera.name,
                    camera.frame
                );
            }
        }
        if let Some(hud) = &viewer.hud {
            hud.validate(&frame_names, &signal_routed, &mut missing)?;
        }
        if missing.is_empty() {
            return Ok(());
        }
        missing.sort();
        missing.dedup();
        anyhow::bail!(
            "[viewer] frame/hud references signal(s) not routed under [signals.viewer]: {}",
            missing.join(", ")
        );
    }

    /// True when configured for external coupling (all FB sections present).
    pub fn has_fb(&self) -> bool {
        self.schema.is_some() && self.receive.is_some() && self.send.is_some()
    }

    /// Effective interactive pacing mode. Explicit TOML wins; otherwise an
    /// externally-coupled runner defaults to lockstep and standalone defaults
    /// to realtime.
    pub fn effective_pacing_mode(&self) -> SimPacingMode {
        SimPacingMode::resolve_interactive(self.sim.mode, self.has_fb())
    }

    /// True when the config requests the interactive runner instead of a
    /// batch compile/simulate/report run.
    pub fn is_interactive_runner(&self) -> bool {
        self.has_fb()
            || self.external_interface.is_some()
            || self.transport.is_some()
            || self.input.is_some()
            || self.signals.is_some()
            || !self.locals.is_empty()
            || !self.derive.is_empty()
            || self.debug_log.is_some()
            || self.reset.is_some()
    }
}

fn validate_positive_finite_option(field: &str, value: Option<f64>) -> anyhow::Result<()> {
    if let Some(value) = value
        && (value <= 0.0 || !value.is_finite())
    {
        anyhow::bail!("{field} must be a positive finite number");
    }
    Ok(())
}

fn validate_zenoh_map_keys(
    section: &str,
    map: &HashMap<String, String>,
    valid_groups: &[Vec<String>],
) -> anyhow::Result<()> {
    let valid = valid_groups
        .iter()
        .flatten()
        .map(String::as_str)
        .collect::<Vec<_>>();
    let invalid = map
        .keys()
        .filter(|key| !valid.iter().any(|candidate| candidate == &key.as_str()))
        .map(String::as_str)
        .collect::<Vec<_>>();
    if invalid.is_empty() {
        return Ok(());
    }
    anyhow::bail!(
        "[{section}] contains unknown message name(s): {}; expected one of: {}",
        invalid.join(", "),
        valid.join(", ")
    );
}

fn message_aliases(alias: &str, root_type: &str) -> Vec<String> {
    vec![
        alias.to_string(),
        root_type.to_string(),
        snake_case_type_leaf(root_type),
    ]
}

fn snake_case_type_leaf(root_type: &str) -> String {
    let leaf = match root_type.rfind('.') {
        Some(dot) => &root_type[dot + 1..],
        None => root_type,
    };
    let mut out = String::with_capacity(leaf.len());
    for (i, ch) in leaf.chars().enumerate() {
        if ch.is_uppercase() {
            if i != 0 {
                out.push('_');
            }
            out.extend(ch.to_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn workspace_root() -> PathBuf {
        // CARGO_MANIFEST_DIR is crates/rumoca; workspace root is two up.
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf()
    }

    #[test]
    fn parses_rover_rum_standalone_mode() {
        let path = workspace_root()
            .join("examples")
            .join("interactive")
            .join("rover")
            .join("rumoca-scenario.toml");
        let cfg = SimulationConfig::load(&path).expect("rover rumoca-scenario.toml must parse");
        assert!(!cfg.has_fb());
        assert_eq!(cfg.effective_pacing_mode(), SimPacingMode::Realtime);
        let sig = cfg.signals.as_ref().expect("[signals]");
        assert_eq!(sig.model_inputs.len(), 2, "steering + throttle");
        let viewer = cfg.viewer.as_ref().expect("[viewer]");
        assert_eq!(viewer.status_title.as_deref(), Some("Rover"));
        assert_eq!(viewer.show_armed, Some(false));
        let chassis = viewer
            .frames
            .iter()
            .find(|frame| frame.name == "chassis")
            .expect("rover declares the chassis frame");
        assert!(chassis.heading.is_some(), "planar rover uses heading");
        assert!(
            viewer
                .cameras
                .iter()
                .any(|camera| camera.frame == "chassis"),
            "onboard camera mounts on the chassis frame"
        );
        cfg.validate().expect("rover viewer config validates");
        let controls = viewer.controls.as_ref().expect("[viewer.controls]");
        assert!(
            controls
                .keyboard
                .iter()
                .any(|item| item.action == "Steering"),
            "rover viewer help should describe steering controls"
        );
    }

    #[test]
    fn standalone_defaults_to_realtime_pacing() {
        let text = r#"
[sim]
dt = 0.01
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        assert!(!cfg.has_fb());
        assert_eq!(cfg.effective_pacing_mode(), SimPacingMode::Realtime);
        assert!(!cfg.is_interactive_runner());
    }

    #[test]
    fn simple_model_config_is_batch_simulation_config() {
        let text = r#"
[model]
file = "Ball.mo"
name = "Ball"

[sim]
t_end = 10.0
dt = 0.01
atol = 1e-7
rtol = 1e-6
output = "Ball_results.html"
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        assert!(!cfg.is_interactive_runner());
        assert_eq!(cfg.sim.t_end, 10.0);
        assert_eq!(cfg.sim.atol, Some(1.0e-7));
        assert_eq!(cfg.sim.rtol, Some(1.0e-6));
        assert_eq!(cfg.sim.output.as_deref(), Some("Ball_results.html"));
    }

    #[test]
    fn parses_all_pacing_modes() {
        for (raw, expected) in [
            ("as_fast_as_possible", SimPacingMode::AsFastAsPossible),
            ("realtime", SimPacingMode::Realtime),
            ("lockstep", SimPacingMode::Lockstep),
        ] {
            let text = format!(
                r#"
[sim]
dt = 0.01
mode = "{raw}"
"#
            );
            let cfg = toml::from_str::<SimulationConfig>(&text).unwrap();
            assert_eq!(cfg.effective_pacing_mode(), expected);
        }
    }

    #[test]
    fn parses_lockstep_steps_per_packet() {
        let text = r#"
[sim]
dt = 0.01
mode = "lockstep"
steps_per_packet = 4
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        assert_eq!(cfg.sim.steps_per_packet, 4);
        cfg.validate()
            .expect("positive steps_per_packet should validate");
    }

    #[test]
    fn rejects_zero_steps_per_packet() {
        let text = r#"
[sim]
dt = 0.01
steps_per_packet = 0
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        let err = cfg.validate().unwrap_err();
        assert!(
            err.to_string().contains("steps_per_packet"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn parses_multirate_lockstep_config() {
        let text = r#"
[sim]
dt = 0.002
mode = "lockstep"

[lockstep]
send_rate_hz = 240
receive_rate_hz = 50
max_advance_dt = 0.002
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        let lockstep = cfg.lockstep.as_ref().expect("lockstep config");
        assert_eq!(lockstep.send_rate_hz, 240.0);
        assert_eq!(lockstep.receive_rate_hz, 50.0);
        assert_eq!(lockstep.max_advance_dt, Some(0.002));
        cfg.validate().expect("multirate lockstep validates");
    }

    #[test]
    fn rejects_invalid_multirate_lockstep_rates() {
        let text = r#"
[sim]
dt = 0.002

[lockstep]
send_rate_hz = 0
receive_rate_hz = 50
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        let err = cfg.validate().unwrap_err();
        assert!(
            err.to_string().contains("send_rate_hz"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn resolves_configured_viewer_ports() {
        let text = r#"
[sim]

[transport.http]
port = 8090

[transport.websocket]
port = 8091
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        assert_eq!(cfg.http_port(), 8090);
        assert_eq!(cfg.websocket_port(), 8091);
    }

    #[test]
    fn validates_viewer_frame_and_camera_references() {
        let text = r#"
[sim]
dt = 0.01

[[viewer.frame]]
name = "chassis"
position = ["x", "y"]
heading = "theta"

[[viewer.camera]]
name = "onboard"
frame = "chassis"
mount = [0.95, 0.0, 0.24]

[signals.viewer]
x = "model:x"
y = "model:y"
theta = "model:theta"
"#;
        let cfg = toml::from_str::<SimulationConfig>(text).unwrap();
        cfg.validate()
            .expect("frame signals routed and camera frame resolves");
    }

    #[test]
    fn rejects_viewer_frame_with_unrouted_signal() {
        let text = r#"
[sim]
dt = 0.01

[[viewer.frame]]
name = "chassis"
position = ["x", "y_typo"]
heading = "theta"

[signals.viewer]
x = "model:x"
y = "model:y"
theta = "model:theta"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(
            err.to_string().contains("y_typo") && err.to_string().contains("[signals.viewer]"),
            "got: {err}"
        );
    }

    #[test]
    fn rejects_camera_on_unknown_frame() {
        let text = r#"
[sim]
dt = 0.01

[[viewer.camera]]
name = "onboard"
frame = "missing"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(
            err.to_string().contains("missing") && err.to_string().contains("[[viewer.frame]]"),
            "got: {err}"
        );
    }

    #[test]
    fn rejects_frame_with_both_orientations() {
        let text = r#"
[sim]
dt = 0.01

[[viewer.frame]]
name = "body"
position = [0.0, 0.0, 0.0]
heading = "theta"
quaternion = ["q0", "q1", "q2", "q3"]

[signals.viewer]
theta = "model:theta"
q0 = "model:q0"
q1 = "model:q1"
q2 = "model:q2"
q3 = "model:q3"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(err.to_string().contains("pick one"), "got: {err}");
    }

    #[test]
    fn rejects_hud_with_unknown_mode_or_frame() {
        let text = r#"
[sim]
dt = 0.01

[[viewer.frame]]
name = "body"
position = [0.0, 0.0, 0.0]

[viewer.hud]
mode = "flight"
frame = "wing"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(err.to_string().contains("wing"), "got: {err}");
    }

    #[test]
    fn rejects_obsolete_realtime_boolean() {
        let text = r#"
[sim]
dt = 0.01
realtime = true
"#;
        let err = toml::from_str::<SimulationConfig>(text).unwrap_err();
        assert!(
            err.to_string().contains("unknown field `realtime`"),
            "got: {err}"
        );
    }

    #[test]
    fn rejects_partial_fb_config() {
        let text = r#"
[sim]
dt = 0.01

[schema]
bfbs = []
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(err.to_string().contains("partial"), "got: {err}");
    }

    #[test]
    fn rejects_zenoh_publish_without_fb_config() {
        let text = r#"
[sim]
dt = 0.01

[transport.zenoh]

[publish]
send = "demo/send"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(
            err.to_string().contains("FlatBuffer coupling"),
            "got: {err}"
        );
    }

    #[test]
    fn rejects_zenoh_unknown_message_name() {
        let text = r#"
[sim]
dt = 0.01

[schema]
bfbs = []

[send]
root_type = "cerebri2.topic.SensorPacket"
route = {}

[receive]
root_type = "cerebri2.topic.CommandPacket"
route = {}

[transport.zenoh]

[publish]
sensor_typo = "demo/send"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(
            err.to_string().contains("sensor_typo") && err.to_string().contains("[publish]"),
            "got: {err}"
        );
    }

    #[test]
    fn parses_quadrotor_rum() {
        let path = workspace_root()
            .join("examples")
            .join("interactive")
            .join("quadrotor")
            .join("rumoca-scenario.acro.toml");
        let cfg = SimulationConfig::load(&path).expect("rumoca-scenario.acro.toml must parse");
        assert!(!cfg.has_fb());
        assert_eq!(cfg.effective_pacing_mode(), SimPacingMode::Realtime);
        assert_eq!(
            cfg.source_roots,
            vec!["../../../target/cmm/CMM-v0.0.2".to_string()]
        );
        assert_eq!(cfg.model.as_ref().unwrap().name, "QuadrotorAcro");
        assert!(cfg.locals.contains_key("armed"));
    }

    #[test]
    fn rejects_runtime_controller_composition() {
        let text = r#"
[sim]
dt = 0.01

[model]
file = "Vehicle.mo"
name = "Vehicle"

[controller]
file = "Controller.mo"
name = "Controller"
"#;
        let err = toml::from_str::<SimulationConfig>(text)
            .unwrap()
            .validate()
            .unwrap_err();
        assert!(err.to_string().contains("[controller]"), "got: {err}");
    }
}
