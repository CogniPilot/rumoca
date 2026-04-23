//! Shared TOML configuration model for Rumoca simulations.
//!
//! Consumed by the CLI, by bindings (wasm/python), and by the sim-fb
//! lockstep runtime. Aggregates input + signal sections from
//! `rumoca-input` and transport from `rumoca-transport-*`.

use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

use rumoca_codec_flatbuffers::config::{
    MessageConfig as FlatbufferMessageConfig, SchemaConfig as FlatbufferSchemaConfig,
};
pub use rumoca_input::config::{
    DeriveSpec, GamepadAxis, GamepadButton, GamepadConfig, InputConfig, Integrator, KeyBinding,
    KeyDecay, KeyboardConfig, LocalDef, SignalSpec, SignalsConfig,
};
pub use rumoca_transport_udp::UdpConfig;

// ── Top-level ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct SimulationConfig {
    pub sim: SimConfig,

    /// Legacy flat [udp] section. Prefer [transport.udp] in new configs.
    #[serde(default)]
    pub udp: Option<UdpConfig>,

    /// FlatBuffer schema files. Required only when coupling to an external
    /// autopilot over UDP; standalone demos (rover etc.) omit this.
    #[serde(default)]
    pub schema: Option<FlatbufferSchemaConfig>,
    /// Incoming FB message routing. Omit for standalone mode.
    #[serde(default)]
    pub receive: Option<FlatbufferMessageConfig>,
    /// Outgoing FB message routing. Omit for standalone mode.
    #[serde(default)]
    pub send: Option<FlatbufferMessageConfig>,

    #[serde(default)]
    pub autopilot: Option<AutopilotConfig>,

    /// Plant / physics model. Required.
    #[serde(default)]
    pub physics: Option<ModelConfig>,
    /// Optional in-process Modelica controller. When present, rumoca
    /// synthesizes a composition wrapper at load time: physics and
    /// controller instantiated together and wired via `controller.actuate`
    /// and `controller.sense`. Mutually exclusive with `autopilot` at the
    /// config-semantics level: both technically load, but pick one.
    #[serde(default)]
    pub controller: Option<ControllerConfig>,
    #[serde(default)]
    pub transport: Option<TransportConfig>,
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
}

// ── Sim + autopilot (orchestration-level) ──────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct AutopilotConfig {
    pub command: String,
}

#[derive(Debug, Deserialize)]
pub struct SimConfig {
    #[serde(default = "default_dt")]
    pub dt: f64,
    #[serde(default = "default_true")]
    pub realtime: bool,
    #[serde(default)]
    pub test: bool,
    /// Outer-loop pacing mode.
    ///
    /// - `lockstep`: block on each inbound autopilot packet; one physics
    ///   step per packet. Deterministic; the autopilot paces the sim.
    ///   Default when autopilot coupling is configured.
    /// - `free_run`: non-blocking drain every `dt`; wall-clock paced.
    ///   The only option that makes sense for standalone mode.
    ///   Default when no autopilot / FB sections are configured.
    #[serde(default)]
    pub mode: Option<SimMode>,
}

/// Outer-loop pacing mode. See [`SimConfig::mode`].
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum SimMode {
    Lockstep,
    FreeRun,
}

impl SimMode {
    /// Resolve the effective mode: explicit config takes priority,
    /// otherwise defaults to lockstep iff autopilot coupling is present.
    #[must_use]
    pub fn resolve(explicit: Option<SimMode>, has_fb: bool) -> SimMode {
        explicit.unwrap_or(if has_fb {
            SimMode::Lockstep
        } else {
            SimMode::FreeRun
        })
    }
}

fn default_dt() -> f64 {
    0.004
}
fn default_true() -> bool {
    true
}

// ── Model ──────────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct ModelConfig {
    pub file: String,
    pub name: String,
}

/// In-process Modelica controller paired with the physics model.
///
/// At load time rumoca generates a wrapper model that instantiates
/// `physics` and `controller`, forwards top-level inputs (controller
/// inputs NOT in `sense`) through to the controller, wires `actuate`
/// (controller output → physics input) and `sense` (physics output →
/// controller input), and exposes physics variables via hierarchical
/// `physics.<name>` stepper paths.
#[derive(Debug, Deserialize)]
pub struct ControllerConfig {
    pub file: String,
    pub name: String,
    /// Controller output -> physics input. Keys are controller-side,
    /// values are physics-side (left arrow, from caller's perspective).
    #[serde(default)]
    pub actuate: HashMap<String, String>,
    /// Physics output -> controller input. Keys are physics-side, values
    /// are controller-side.
    #[serde(default)]
    pub sense: HashMap<String, String>,
}

// ── Transports ─────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct TransportConfig {
    #[serde(default)]
    pub udp: Option<UdpConfig>,
    #[serde(default)]
    pub websocket: Option<WebSocketConfig>,
    #[serde(default)]
    pub http: Option<HttpConfig>,
}

#[derive(Debug, Deserialize)]
pub struct WebSocketConfig {
    pub port: u16,
}

#[derive(Debug, Deserialize)]
pub struct HttpConfig {
    pub port: u16,
    #[serde(default)]
    pub scene: Option<String>,
}

// ── Debug log ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct DebugLogConfig {
    pub ring_size: usize,
    pub trigger_signal: String,
    pub capture: Vec<String>,
}

// ── Reset ──────────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct ResetConfig {
    pub on_signal: String,
    #[serde(default)]
    pub reset_locals: bool,
    #[serde(default)]
    pub rebuild_stepper: bool,
    #[serde(default)]
    pub restart_autopilot: bool,
}

// ── Loader ─────────────────────────────────────────────────────────────────

impl SimulationConfig {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let text = std::fs::read_to_string(path)?;
        let config: SimulationConfig = toml::from_str(&text)?;
        config.validate()?;
        Ok(config)
    }

    /// The three FB sections must all be present (autopilot coupling) or all
    /// absent (standalone). Any mix is a user error.
    fn validate(&self) -> anyhow::Result<()> {
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
                 autopilot coupling, or omit all three for standalone mode."
            );
        }
        Ok(())
    }

    /// True when configured for autopilot coupling (all FB sections present).
    pub fn has_fb(&self) -> bool {
        self.schema.is_some() && self.receive.is_some() && self.send.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn workspace_root() -> PathBuf {
        // CARGO_MANIFEST_DIR is crates/rumoca-session; workspace root is two up.
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf()
    }

    #[test]
    fn parses_rover_toml_standalone_mode() {
        let path = workspace_root()
            .join("examples")
            .join("rover_sil")
            .join("rover.toml");
        let cfg = SimulationConfig::load(&path).expect("rover.toml must parse");
        assert!(!cfg.has_fb());
        let sig = cfg.signals.as_ref().expect("[signals]");
        assert_eq!(sig.stepper_inputs.len(), 2, "forward_cmd + turn_cmd");
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
    fn parses_quadrotor_toml() {
        let path = workspace_root()
            .join("examples")
            .join("quadrotor_sil")
            .join("quadrotor.toml");
        let cfg = SimulationConfig::load(&path).expect("quadrotor.toml must parse");
        assert!(cfg.has_fb());
        assert_eq!(cfg.locals["rc"].len, Some(16));
    }
}
