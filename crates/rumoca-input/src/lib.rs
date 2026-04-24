//! Input engine + signal mapping for Rumoca lockstep simulations.
//!
//! This crate owns everything abstract that operates on the shared local
//! store: input identifiers, derive rules, action dispatch (debounce +
//! precondition), and the signal mapper that builds outgoing SignalFrames
//! and viewer JSON.
//!
//! Concrete device crates own native polling dependencies (`gilrs`,
//! `crossterm`) and feed abstract snapshots/events into this crate.

pub mod config;
pub mod device;

pub mod engine;
pub mod signal_mapper;

pub use device::{
    GamepadAxis, GamepadButton, KeyCode, KeyModifiers, parse_gamepad_axis, parse_gamepad_button,
    parse_key,
};
pub use engine::compile;
pub use engine::{
    ButtonAction, CompiledDecay, CompiledDerive, CompiledGamepadAxis, CompiledGamepadButton,
    CompiledInput, CompiledIntegrator, CompiledKey, DeriveRule, GamepadSnapshot, InputEngine,
    InputMode, IntegratorSource, KeyAction, KeyboardEvent, LocalValue, Path, Precondition,
    PreconditionOp,
};
pub use signal_mapper::{RuntimeContext, SignalMapper};
