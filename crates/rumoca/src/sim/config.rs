//! TOML configuration model for the `rumoca lockstep` runtime.
//!
//! Aggregates lockstep-only input, codec, and transport sections outside the
//! compiler/session layer.

use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

use rumoca_codec_flatbuffers::config::{
    MessageConfig as FlatbufferMessageConfig, SchemaConfig as FlatbufferSchemaConfig,
};
use rumoca_input::config::{DeriveSpec, InputConfig, LocalDef, SignalsConfig};
use rumoca_transport_udp::UdpConfig;

// ── Top-level ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub(crate) struct SimulationConfig {
    pub(crate) sim: SimConfig,

    /// Legacy flat `[udp]` section. Prefer `[transport.udp]` in new configs.
    #[serde(default)]
    pub(crate) udp: Option<UdpConfig>,

    /// FlatBuffer schema files. Required only when coupling to an external
    /// autopilot over UDP; standalone demos (rover etc.) omit this.
    #[serde(default)]
    pub(crate) schema: Option<FlatbufferSchemaConfig>,
    /// Incoming FB message routing. Omit for standalone mode.
    #[serde(default)]
    pub(crate) receive: Option<FlatbufferMessageConfig>,
    /// Outgoing FB message routing. Omit for standalone mode.
    #[serde(default)]
    pub(crate) send: Option<FlatbufferMessageConfig>,

    #[serde(default)]
    pub(crate) autopilot: Option<AutopilotConfig>,

    /// Plant / physics model. Required.
    #[serde(default)]
    pub(crate) physics: Option<ModelConfig>,
    /// Optional in-process Modelica controller. When present, rumoca
    /// synthesizes a composition wrapper at load time: physics and
    /// controller instantiated together and wired via `controller.actuate`
    /// and `controller.sense`. Mutually exclusive with `autopilot` at the
    /// config-semantics level: both technically load, but pick one.
    #[serde(default)]
    pub(crate) controller: Option<ControllerConfig>,
    #[serde(default)]
    pub(crate) transport: Option<TransportConfig>,
    #[serde(default)]
    pub(crate) locals: HashMap<String, LocalDef>,
    #[serde(default)]
    pub(crate) derive: HashMap<String, DeriveSpec>,
    #[serde(default)]
    pub(crate) input: Option<InputConfig>,
    #[serde(default)]
    pub(crate) signals: Option<SignalsConfig>,
    #[serde(default)]
    pub(crate) debug_log: Option<DebugLogConfig>,
    #[serde(default)]
    pub(crate) reset: Option<ResetConfig>,
}

// ── Sim + autopilot (orchestration-level) ──────────────────────────────────

#[derive(Debug, Deserialize)]
pub(crate) struct AutopilotConfig {
    pub(crate) command: String,
}

#[derive(Debug, Deserialize)]
pub(crate) struct SimConfig {
    #[serde(default = "default_dt")]
    pub(crate) dt: f64,
    #[serde(default = "default_true")]
    pub(crate) realtime: bool,
    #[serde(default, rename = "test")]
    pub(crate) _test: bool,
    /// Outer-loop pacing mode.
    ///
    /// - `lockstep`: block on each inbound autopilot packet; one physics
    ///   step per packet. Deterministic; the autopilot paces the sim.
    ///   Default when autopilot coupling is configured.
    /// - `free_run`: non-blocking drain every `dt`; wall-clock paced.
    ///   The only option that makes sense for standalone mode.
    ///   Default when no autopilot / FB sections are configured.
    #[serde(default)]
    pub(crate) mode: Option<SimMode>,
}

/// Outer-loop pacing mode. See [`SimConfig::mode`].
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum SimMode {
    Lockstep,
    FreeRun,
}

impl SimMode {
    /// Resolve the effective mode: explicit config takes priority,
    /// otherwise defaults to lockstep iff autopilot coupling is present.
    #[must_use]
    pub(crate) fn resolve(explicit: Option<SimMode>, has_fb: bool) -> SimMode {
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
pub(crate) struct ModelConfig {
    pub(crate) file: String,
    pub(crate) name: String,
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
pub(crate) struct ControllerConfig {
    pub(crate) file: String,
    pub(crate) name: String,
    /// Controller output -> physics input. Keys are controller-side,
    /// values are physics-side (left arrow, from caller's perspective).
    #[serde(default)]
    pub(crate) actuate: HashMap<String, String>,
    /// Physics output -> controller input. Keys are physics-side, values
    /// are controller-side.
    #[serde(default)]
    pub(crate) sense: HashMap<String, String>,
}

// ── Transports ─────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub(crate) struct TransportConfig {
    #[serde(default)]
    pub(crate) udp: Option<UdpConfig>,
    #[serde(default, rename = "websocket")]
    pub(crate) _websocket: Option<WebSocketConfig>,
    #[serde(default)]
    pub(crate) http: Option<HttpConfig>,
}

#[derive(Debug, Deserialize)]
pub(crate) struct WebSocketConfig {
    #[serde(rename = "port")]
    pub(crate) _port: u16,
}

#[derive(Debug, Deserialize)]
pub(crate) struct HttpConfig {
    #[serde(rename = "port")]
    pub(crate) _port: u16,
    #[serde(default)]
    pub(crate) scene: Option<String>,
}

// ── Debug log ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub(crate) struct DebugLogConfig {
    #[serde(rename = "ring_size")]
    pub(crate) _ring_size: usize,
    pub(crate) trigger_signal: String,
    pub(crate) capture: Vec<String>,
}

// ── Reset ──────────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub(crate) struct ResetConfig {
    pub(crate) on_signal: String,
    #[serde(default)]
    pub(crate) reset_locals: bool,
    #[serde(default)]
    pub(crate) rebuild_stepper: bool,
    #[serde(default)]
    pub(crate) restart_autopilot: bool,
}

// ── Loader ─────────────────────────────────────────────────────────────────

impl SimulationConfig {
    pub(crate) fn load(path: &Path) -> anyhow::Result<Self> {
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
    pub(crate) fn has_fb(&self) -> bool {
        self.schema.is_some() && self.receive.is_some() && self.send.is_some()
    }
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
            .join("quadrotor_cerebri.toml");
        let cfg = SimulationConfig::load(&path).expect("quadrotor_cerebri.toml must parse");
        assert!(cfg.has_fb());
        assert_eq!(cfg.locals["rc"].len, Some(16));
    }
}
