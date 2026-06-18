//! TOML config types for the input engine + signal mapper.

use serde::Deserialize;
use std::collections::HashMap;

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
    #[serde(default)]
    pub reject_message: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct KeyboardConfig {
    #[serde(default)]
    pub keys: HashMap<String, KeyBinding>,
    #[serde(default)]
    pub integrators: HashMap<String, Integrator>,
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
    #[serde(default)]
    pub reject_message: Option<String>,
}

// ── Signal assembly ────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
pub struct SignalsConfig {
    #[serde(default)]
    pub send: HashMap<String, SignalSpec>,
    #[serde(default)]
    pub viewer: HashMap<String, SignalSpec>,
    /// Signals applied directly to the stepper each frame. Used in
    /// standalone mode (no external interface) to drive model inputs from local
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
