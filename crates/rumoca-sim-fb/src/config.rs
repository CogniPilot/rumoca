use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

use rumoca_codec_flatbuffers::config::{
    MessageConfig as FlatbufferMessageConfig, SchemaConfig as FlatbufferSchemaConfig,
};

// ── Top-level ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct SimFbConfig {
    pub sim: SimConfig,

    // Legacy flat [udp] section — kept for backward compatibility with
    // sil_config.toml. New configs should use [transport.udp] instead.
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

    // New sections introduced by the generic-lockstep schema.
    // All optional so legacy configs keep loading.
    #[serde(default)]
    pub model: Option<ModelConfig>,
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

// ── Sim + autopilot + UDP (existing) ───────────────────────────────────────

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
}

fn default_dt() -> f64 {
    0.004
}
fn default_true() -> bool {
    true
}

#[derive(Debug, Deserialize)]
pub struct UdpConfig {
    pub listen: String,
    pub send: String,
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

// ── Locals ─────────────────────────────────────────────────────────────────

/// A named piece of persistent state. `kind` selects the storage:
///   - "bool"  → boolean flag
///   - "float" → f64
///   - "array" → homogeneous vector (requires `element` + `len`)
#[derive(Debug, Deserialize)]
pub struct LocalDef {
    #[serde(rename = "type")]
    pub kind: String,
    #[serde(default)]
    pub default: Option<toml::Value>,
    #[serde(default)]
    pub element: Option<String>,
    #[serde(default)]
    pub len: Option<usize>,
}

// ── Derive specs ───────────────────────────────────────────────────────────

/// Per-frame computation from one local to another.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum DeriveSpec {
    /// Linear map: `scale * from + offset`, optionally clamped.
    Linear {
        from: String,
        scale: f64,
        #[serde(default)]
        offset: f64,
        #[serde(default)]
        clamp: Option<[f64; 2]>,
    },
    /// Branch on a bool.
    Conditional {
        from: String,
        when_true: toml::Value,
        when_false: toml::Value,
    },
}

// ── Input engine ───────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct InputConfig {
    /// "gamepad" | "keyboard" | "auto"
    #[serde(default = "default_input_mode")]
    pub mode: String,
    #[serde(default)]
    pub gamepad: Option<GamepadConfig>,
    #[serde(default)]
    pub keyboard: Option<KeyboardConfig>,
}

fn default_input_mode() -> String {
    "auto".to_string()
}

#[derive(Debug, Deserialize)]
pub struct GamepadConfig {
    #[serde(default)]
    pub axes: HashMap<String, GamepadAxis>,
    #[serde(default)]
    pub integrators: HashMap<String, Integrator>,
    #[serde(default)]
    pub buttons: HashMap<String, GamepadButton>,
}

#[derive(Debug, Deserialize)]
pub struct GamepadAxis {
    pub source: String,
    pub write: String,
    #[serde(default = "default_unit_scale")]
    pub scale: f64,
    #[serde(default)]
    pub invert: bool,
}

fn default_unit_scale() -> f64 {
    1.0
}

/// Integrator: `state += source * rate * dt`, clamped. `deadband` zeroes
/// the input when `|source| < deadband` (useful for sticks near center).
#[derive(Debug, Deserialize)]
pub struct Integrator {
    pub source: String,
    pub write: String,
    #[serde(default)]
    pub deadband: f64,
    pub rate: f64,
    pub clamp: [f64; 2],
}

#[derive(Debug, Deserialize)]
pub struct GamepadButton {
    pub source: String,
    /// "toggle" | "signal"
    pub action: String,
    #[serde(default)]
    pub state: Option<String>,
    #[serde(default)]
    pub signal: Option<String>,
    #[serde(default)]
    pub debounce_ms: Option<u64>,
    #[serde(default)]
    pub precondition: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct KeyboardConfig {
    #[serde(default)]
    pub decay: Option<KeyDecay>,
    #[serde(default)]
    pub keys: HashMap<String, KeyBinding>,
    #[serde(default)]
    pub integrators: HashMap<String, Integrator>,
}

/// Each frame, multiply `targets` by `factor^(dt/ref_dt)`.
#[derive(Debug, Deserialize)]
pub struct KeyDecay {
    pub targets: Vec<String>,
    pub factor: f64,
    pub ref_dt: f64,
}

#[derive(Debug, Deserialize)]
pub struct KeyBinding {
    /// "set" | "toggle" | "signal"
    pub action: String,
    #[serde(default)]
    pub target: Option<String>,
    #[serde(default)]
    pub state: Option<String>,
    #[serde(default)]
    pub value: Option<f64>,
    #[serde(default)]
    pub signal: Option<String>,
    #[serde(default)]
    pub debounce_ms: Option<u64>,
    #[serde(default)]
    pub precondition: Option<String>,
}

// ── Signal assembly ────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct SignalsConfig {
    #[serde(default)]
    pub send: HashMap<String, SignalSpec>,
    #[serde(default)]
    pub viewer: HashMap<String, SignalSpec>,
    /// Signals applied directly to the stepper each frame. Used in
    /// standalone mode (no autopilot) to drive model inputs from local
    /// state (e.g. gamepad → stepper input).
    #[serde(default)]
    pub stepper_inputs: HashMap<String, SignalSpec>,
}

/// How a single signal value is produced.
/// Order matters for untagged deserialization — most specific first.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum SignalSpec {
    /// `"stepper:name"` / `"local:name"` / `"runtime:name"` / `"local:rc.2"`
    Ref(String),
    /// `{ from = "...", when_true = X, when_false = Y }`
    Conditional {
        from: String,
        when_true: toml::Value,
        when_false: toml::Value,
    },
    /// `{ from = "...", default = X }`
    WithDefault { from: String, default: f64 },
    /// `{ const = X }`
    Const {
        #[serde(rename = "const")]
        value: toml::Value,
    },
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

impl SimFbConfig {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let text = std::fs::read_to_string(path)?;
        let config: SimFbConfig = toml::from_str(&text)?;
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

    /// Path to the workspace root, derived from CARGO_MANIFEST_DIR.
    fn workspace_root() -> PathBuf {
        // CARGO_MANIFEST_DIR is crates/rumoca-sim-fb, workspace is two dirs up.
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf()
    }

    #[test]
    fn parses_legacy_sil_config() {
        // Existing demo config outside the repo — only assert if present.
        let path = workspace_root()
            .parent()
            .unwrap()
            .join("sim_fb_demo")
            .join("sil_config.toml");
        if !path.exists() {
            eprintln!("skip: {} not found", path.display());
            return;
        }
        let cfg = SimFbConfig::load(&path).expect("legacy config must parse");
        assert!(cfg.udp.is_some(), "legacy [udp] should populate");
        assert!(cfg.transport.is_none(), "no [transport] in legacy");
        assert!(cfg.locals.is_empty(), "no [locals] in legacy");
    }

    #[test]
    fn parses_rover_toml_standalone_mode() {
        let path = workspace_root()
            .join("examples")
            .join("rover_sil")
            .join("rover.toml");
        let cfg = SimFbConfig::load(&path).expect("rover.toml must parse");
        assert!(!cfg.has_fb(), "rover config is standalone");
        assert!(cfg.schema.is_none());
        assert!(cfg.receive.is_none());
        assert!(cfg.send.is_none());
        assert!(cfg.autopilot.is_none());
        let sig = cfg.signals.as_ref().expect("[signals]");
        assert!(sig.send.is_empty(), "no send frame for standalone");
        assert_eq!(sig.stepper_inputs.len(), 2, "forward_cmd + turn_cmd");
        assert!(cfg.locals.contains_key("forward_cmd"));
        assert!(cfg.locals.contains_key("turn_cmd"));
    }

    #[test]
    fn rejects_partial_fb_config() {
        let text = r#"
[sim]
dt = 0.01

[schema]
bfbs = []
"#;
        let err = toml::from_str::<SimFbConfig>(text)
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
        let cfg = SimFbConfig::load(&path).expect("quadrotor.toml must parse");

        // Spot-check every new section landed in the right place.
        let model = cfg.model.as_ref().expect("[model]");
        assert_eq!(model.name, "QuadrotorSIL");

        let transport = cfg.transport.as_ref().expect("[transport]");
        let udp = transport.udp.as_ref().expect("[transport.udp]");
        assert_eq!(udp.listen, "0.0.0.0:4244");
        assert_eq!(transport.websocket.as_ref().unwrap().port, 8081);
        assert_eq!(transport.http.as_ref().unwrap().port, 8080);

        assert!(cfg.locals.contains_key("armed"));
        assert_eq!(cfg.locals["armed"].kind, "bool");
        assert_eq!(cfg.locals["rc"].kind, "array");
        assert_eq!(cfg.locals["rc"].len, Some(16));

        // 5 derive rules: rc.0..4
        assert_eq!(cfg.derive.len(), 5);
        assert!(cfg.derive.contains_key("rc.2"));

        let input = cfg.input.as_ref().expect("[input]");
        assert_eq!(input.mode, "auto");
        let gp = input.gamepad.as_ref().unwrap();
        assert_eq!(gp.axes.len(), 3);
        assert_eq!(gp.buttons.len(), 3);
        let arm_btn = &gp.buttons["arm"];
        assert_eq!(arm_btn.action, "toggle");
        assert_eq!(arm_btn.precondition.as_deref(), Some("rc.2 <= 1050"));

        let kb = input.keyboard.as_ref().unwrap();
        assert!(kb.decay.is_some());
        assert!(kb.keys.len() >= 12);

        let signals = cfg.signals.as_ref().expect("[signals]");
        assert!(signals.send.len() >= 20);
        assert!(signals.viewer.len() >= 20);

        let dbg = cfg.debug_log.as_ref().expect("[debug_log]");
        assert_eq!(dbg.ring_size, 500);

        let rst = cfg.reset.as_ref().expect("[reset]");
        assert_eq!(rst.on_signal, "reset");
        assert!(rst.reset_locals);

        // Existing FB sections parse via the `to`/`key` aliases.
        let recv = cfg.receive.as_ref().expect("[receive]");
        assert_eq!(recv.route.len(), 6);
        assert_eq!(
            recv.route["motors.m0"].name(),
            "stepper:omega_m1",
            "receive route name should be full ref string"
        );
        assert_eq!(recv.route["motors.m0"].scale(), 1100.0);
    }
}
