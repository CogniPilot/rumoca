//! Input engine + signal mapping for Rumoca lockstep simulations.
//!
//! This crate owns everything that operates on the shared local store:
//! gamepad / keyboard polling, derive rules, action dispatch (debounce +
//! precondition), and the signal mapper that builds outgoing SignalFrames
//! and viewer JSON.

pub mod config;
pub mod engine;
pub mod signal_mapper;

pub use engine::compile;
pub use engine::{
    ButtonAction, CompiledDecay, CompiledDerive, CompiledGamepadAxis, CompiledGamepadButton,
    CompiledInput, CompiledIntegrator, CompiledKey, DeriveRule, InputEngine, InputMode,
    IntegratorSource, KeyAction, LocalValue, Path, Precondition, PreconditionOp,
};
pub use signal_mapper::{RuntimeContext, SignalMapper};
