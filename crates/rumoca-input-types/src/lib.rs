//! Shared input types for Rumoca lockstep simulations.
//!
//! Defines the gamepad/keyboard vocabulary (`GamepadAxis`, `GamepadButton`,
//! `KeyCode`, `KeyModifiers`) and the events/snapshots that flow between
//! concrete device adapters (`rumoca-input-gamepad`, `rumoca-input-keyboard`)
//! and the abstract input engine (`rumoca-input`). Lives in its own crate
//! so the impl crates and consumer crates share the type vocabulary
//! without depending on each other.

use std::collections::HashMap;

pub mod device;

pub use device::{
    GamepadAxis, GamepadButton, KeyCode, KeyModifiers, parse_gamepad_axis, parse_gamepad_button,
    parse_key,
};

/// Which physical input the runtime is currently driven by.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputMode {
    Gamepad,
    Keyboard,
}

/// A snapshot of gamepad state at one poll. Produced by the concrete
/// gamepad adapter and consumed by the input engine.
#[derive(Debug, Clone, Default)]
pub struct GamepadSnapshot {
    pub axis_values: HashMap<GamepadAxis, f64>,
    pub button_pressed: HashMap<GamepadButton, bool>,
}

impl GamepadSnapshot {
    pub fn new(
        axis_values: HashMap<GamepadAxis, f64>,
        button_pressed: HashMap<GamepadButton, bool>,
    ) -> Self {
        Self {
            axis_values,
            button_pressed,
        }
    }
}

/// A single keyboard press event. Produced by the concrete keyboard
/// adapter and consumed by the input engine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KeyboardEvent {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

impl KeyboardEvent {
    pub fn new(code: KeyCode, modifiers: KeyModifiers) -> Self {
        Self { code, modifiers }
    }
}
