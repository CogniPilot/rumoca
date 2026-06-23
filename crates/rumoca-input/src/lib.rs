//! Input engine + signal mapping facade for Rumoca simulations.
//!
//! - Owns the input vocabulary (GamepadAxis, GamepadButton, KeyCode, etc.),
//!   device snapshots (GamepadSnapshot, KeyboardEvent), and InputMode.
//! - Owns the `InputEngine` (config-driven local store, debounce,
//!   preconditions, derive rules) and `SignalMapper` (outgoing SignalFrames
//!   + viewer JSON).
//!
//! Concrete device adapters (`rumoca-input-gamepad`, `rumoca-input-keyboard`)
//! depend on this crate for the shared vocabulary. The `Devices` runtime
//! factory that composes the adapters lives in `rumoca-sim`.

pub mod config;
pub mod device;
pub mod engine;
pub mod signal_mapper;

pub use device::{
    GamepadAxis, GamepadButton, GamepadSnapshot, InputMode, KeyCode, KeyModifiers, KeyboardEvent,
    parse_gamepad_axis, parse_gamepad_button, parse_key,
};
pub use engine::compile;
pub use engine::{
    ButtonAction, CompiledDerive, CompiledGamepadAxis, CompiledGamepadButton, CompiledInput,
    CompiledIntegrator, CompiledKey, CompiledKeyboardDecay, DeriveRule, InputEngine,
    IntegratorSource, KeyAction, LocalValue, Path, Precondition, PreconditionOp,
};
pub use signal_mapper::{RuntimeContext, SignalMapper};
