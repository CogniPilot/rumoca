//! Input engine + signal mapping for Rumoca lockstep simulations.
//!
//! This crate owns everything that operates on the shared local store:
//! gamepad / keyboard polling, derive rules, action dispatch (debounce +
//! precondition), and the signal mapper that builds outgoing SignalFrames
//! and viewer JSON.
//!
//! On `wasm32` targets only the config types are exposed — the engine +
//! signal mapper depend on native-only crates (`gilrs`, `crossterm`). A
//! future `rumoca-input-browser` will provide the wasm device path.

pub mod config;

#[cfg(not(target_arch = "wasm32"))]
pub mod engine;
#[cfg(not(target_arch = "wasm32"))]
pub mod signal_mapper;

#[cfg(not(target_arch = "wasm32"))]
pub use engine::compile;
#[cfg(not(target_arch = "wasm32"))]
pub use engine::{
    ButtonAction, CompiledDecay, CompiledDerive, CompiledGamepadAxis, CompiledGamepadButton,
    CompiledInput, CompiledIntegrator, CompiledKey, DeriveRule, InputEngine, InputMode,
    IntegratorSource, KeyAction, LocalValue, Path, Precondition, PreconditionOp,
};
#[cfg(not(target_arch = "wasm32"))]
pub use signal_mapper::{RuntimeContext, SignalMapper};
