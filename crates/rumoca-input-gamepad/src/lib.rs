//! Gilrs-backed gamepad device for `rumoca-input`.
//!
//! Owns the gilrs dependency and the string→enum parsing for TOML
//! `source = "..."` names. The engine state machine still lives in
//! `rumoca-input` — this crate is intentionally thin until a future
//! `InputDevice` trait refactor moves polling here.

pub use gilrs::{Axis, Button, Gilrs};

use anyhow::{Result, bail};

/// Parse a TOML axis name into a `gilrs::Axis`.
pub fn parse_axis(s: &str) -> Result<Axis> {
    Ok(match s {
        "LeftStickX" => Axis::LeftStickX,
        "LeftStickY" => Axis::LeftStickY,
        "RightStickX" => Axis::RightStickX,
        "RightStickY" => Axis::RightStickY,
        "LeftZ" => Axis::LeftZ,
        "RightZ" => Axis::RightZ,
        "DPadX" => Axis::DPadX,
        "DPadY" => Axis::DPadY,
        _ => bail!("unknown gamepad axis: '{s}'"),
    })
}

/// Parse a TOML button name into a `gilrs::Button`.
pub fn parse_button(s: &str) -> Result<Button> {
    Ok(match s {
        "South" => Button::South,
        "East" => Button::East,
        "North" => Button::North,
        "West" => Button::West,
        "LeftTrigger" => Button::LeftTrigger,
        "LeftTrigger2" => Button::LeftTrigger2,
        "RightTrigger" => Button::RightTrigger,
        "RightTrigger2" => Button::RightTrigger2,
        "Select" => Button::Select,
        "Start" => Button::Start,
        "Mode" => Button::Mode,
        "LeftThumb" => Button::LeftThumb,
        "RightThumb" => Button::RightThumb,
        "DPadUp" => Button::DPadUp,
        "DPadDown" => Button::DPadDown,
        "DPadLeft" => Button::DPadLeft,
        "DPadRight" => Button::DPadRight,
        _ => bail!("unknown gamepad button: '{s}'"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_known_axes() {
        assert_eq!(parse_axis("RightStickX").unwrap(), Axis::RightStickX);
        assert!(parse_axis("nope").is_err());
    }

    #[test]
    fn parses_known_buttons() {
        assert_eq!(parse_button("Start").unwrap(), Button::Start);
        assert!(parse_button("nope").is_err());
    }
}
