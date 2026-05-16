//! Device vocabulary types — re-exported from `rumoca-input-types` so this
//! crate can stay the user-facing facade while concrete adapters depend on
//! the shared types crate directly.

pub use rumoca_input_types::{
    GamepadAxis, GamepadButton, KeyCode, KeyModifiers, parse_gamepad_axis, parse_gamepad_button,
    parse_key,
};
