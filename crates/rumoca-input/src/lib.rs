//! Input engine + signal mapping facade for Rumoca lockstep simulations.
//!
//! Front door for input handling:
//!
//! - Re-exports the shared vocabulary types from `rumoca-input-types`.
//! - Owns the `InputEngine` (config-driven local store, debounce,
//!   preconditions, derive rules) and `SignalMapper` (outgoing SignalFrames
//!   + viewer JSON).
//! - Provides a `Devices` factory that picks gamepad or keyboard at
//!   runtime and drives them against the engine. Concrete device polling
//!   lives in `rumoca-input-gamepad` (gilrs) and `rumoca-input-keyboard`
//!   (crossterm); this crate depends on them and dispatches.
//!
//! Sim and other consumers depend only on this crate.

pub mod config;
pub mod device;
pub mod devices;

pub mod engine;
pub mod signal_mapper;

pub use devices::Devices;

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
