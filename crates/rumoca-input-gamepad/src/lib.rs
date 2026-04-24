//! Gilrs-backed gamepad device adapter for `rumoca-input`.

use std::collections::HashMap;

use anyhow::{Result, anyhow};
pub use gilrs::Gilrs;
use rumoca_input::{GamepadAxis, GamepadButton, GamepadSnapshot};

pub struct GamepadDevice {
    gilrs: Gilrs,
}

impl GamepadDevice {
    pub fn new() -> Result<Self> {
        let gilrs = Gilrs::new().map_err(|error| anyhow!("gilrs init failed: {error:?}"))?;
        Ok(Self { gilrs })
    }

    pub fn is_connected(&self) -> bool {
        self.gilrs.gamepads().count() > 0
    }

    pub fn announce_gamepads(&self) {
        for (_id, gamepad) in self.gilrs.gamepads() {
            eprintln!(
                "[input] gamepad: {} ({})",
                gamepad.name(),
                gamepad.os_name()
            );
        }
    }

    pub fn snapshot(&mut self) -> Option<GamepadSnapshot> {
        while self.gilrs.next_event().is_some() {}
        let (_, gamepad) = self.gilrs.gamepads().next()?;
        let axis_values = GamepadAxis::ALL
            .into_iter()
            .map(|axis| (axis, f64::from(gamepad.value(to_gilrs_axis(axis)))))
            .collect::<HashMap<_, _>>();
        let button_pressed = GamepadButton::ALL
            .into_iter()
            .map(|button| (button, gamepad.is_pressed(to_gilrs_button(button))))
            .collect::<HashMap<_, _>>();
        Some(GamepadSnapshot::new(axis_values, button_pressed))
    }
}

fn to_gilrs_axis(axis: GamepadAxis) -> gilrs::Axis {
    match axis {
        GamepadAxis::LeftStickX => gilrs::Axis::LeftStickX,
        GamepadAxis::LeftStickY => gilrs::Axis::LeftStickY,
        GamepadAxis::RightStickX => gilrs::Axis::RightStickX,
        GamepadAxis::RightStickY => gilrs::Axis::RightStickY,
        GamepadAxis::LeftZ => gilrs::Axis::LeftZ,
        GamepadAxis::RightZ => gilrs::Axis::RightZ,
        GamepadAxis::DPadX => gilrs::Axis::DPadX,
        GamepadAxis::DPadY => gilrs::Axis::DPadY,
    }
}

fn to_gilrs_button(button: GamepadButton) -> gilrs::Button {
    match button {
        GamepadButton::South => gilrs::Button::South,
        GamepadButton::East => gilrs::Button::East,
        GamepadButton::North => gilrs::Button::North,
        GamepadButton::West => gilrs::Button::West,
        GamepadButton::LeftTrigger => gilrs::Button::LeftTrigger,
        GamepadButton::LeftTrigger2 => gilrs::Button::LeftTrigger2,
        GamepadButton::RightTrigger => gilrs::Button::RightTrigger,
        GamepadButton::RightTrigger2 => gilrs::Button::RightTrigger2,
        GamepadButton::Select => gilrs::Button::Select,
        GamepadButton::Start => gilrs::Button::Start,
        GamepadButton::Mode => gilrs::Button::Mode,
        GamepadButton::LeftThumb => gilrs::Button::LeftThumb,
        GamepadButton::RightThumb => gilrs::Button::RightThumb,
        GamepadButton::DPadUp => gilrs::Button::DPadUp,
        GamepadButton::DPadDown => gilrs::Button::DPadDown,
        GamepadButton::DPadLeft => gilrs::Button::DPadLeft,
        GamepadButton::DPadRight => gilrs::Button::DPadRight,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_abstract_names_to_gilrs() {
        assert_eq!(
            to_gilrs_axis(GamepadAxis::RightStickX),
            gilrs::Axis::RightStickX
        );
        assert_eq!(to_gilrs_button(GamepadButton::Start), gilrs::Button::Start);
    }
}
