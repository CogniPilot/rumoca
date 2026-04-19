//! Config re-exports.
//!
//! The canonical `SimulationConfig` lives in `rumoca_session::config` now.
//! This module keeps legacy aliases for existing call sites during the
//! sim-fb dissolution.

pub use rumoca_session::config::{
    AutopilotConfig, DebugLogConfig, HttpConfig, ModelConfig, ResetConfig, SimConfig,
    SimulationConfig, TransportConfig, UdpConfig, WebSocketConfig,
};

// Transitional: sim-fb's sim_loop.rs still imports `SimFbConfig` as an alias.
pub type SimFbConfig = SimulationConfig;

// Input/signal config types also re-exported for convenience.
pub use rumoca_input::config::{
    DeriveSpec, GamepadAxis, GamepadButton, GamepadConfig, InputConfig, Integrator, KeyBinding,
    KeyDecay, KeyboardConfig, LocalDef, SignalSpec, SignalsConfig,
};
