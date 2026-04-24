use anyhow::{Result, bail};
use std::ops::{BitOr, BitOrAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GamepadAxis {
    LeftStickX,
    LeftStickY,
    RightStickX,
    RightStickY,
    LeftZ,
    RightZ,
    DPadX,
    DPadY,
}

impl GamepadAxis {
    pub const ALL: [Self; 8] = [
        Self::LeftStickX,
        Self::LeftStickY,
        Self::RightStickX,
        Self::RightStickY,
        Self::LeftZ,
        Self::RightZ,
        Self::DPadX,
        Self::DPadY,
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GamepadButton {
    South,
    East,
    North,
    West,
    LeftTrigger,
    LeftTrigger2,
    RightTrigger,
    RightTrigger2,
    Select,
    Start,
    Mode,
    LeftThumb,
    RightThumb,
    DPadUp,
    DPadDown,
    DPadLeft,
    DPadRight,
}

impl GamepadButton {
    pub const ALL: [Self; 17] = [
        Self::South,
        Self::East,
        Self::North,
        Self::West,
        Self::LeftTrigger,
        Self::LeftTrigger2,
        Self::RightTrigger,
        Self::RightTrigger2,
        Self::Select,
        Self::Start,
        Self::Mode,
        Self::LeftThumb,
        Self::RightThumb,
        Self::DPadUp,
        Self::DPadDown,
        Self::DPadLeft,
        Self::DPadRight,
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyCode {
    Char(char),
    Up,
    Down,
    Left,
    Right,
    Enter,
    Tab,
    Esc,
    Backspace,
    Delete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct KeyModifiers(u8);

impl KeyModifiers {
    pub const NONE: Self = Self(0);
    pub const SHIFT: Self = Self(1 << 0);
    pub const CONTROL: Self = Self(1 << 1);
    pub const ALT: Self = Self(1 << 2);

    pub fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }
}

impl BitOr for KeyModifiers {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitOrAssign for KeyModifiers {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

pub fn parse_gamepad_axis(s: &str) -> Result<GamepadAxis> {
    Ok(match s {
        "LeftStickX" => GamepadAxis::LeftStickX,
        "LeftStickY" => GamepadAxis::LeftStickY,
        "RightStickX" => GamepadAxis::RightStickX,
        "RightStickY" => GamepadAxis::RightStickY,
        "LeftZ" => GamepadAxis::LeftZ,
        "RightZ" => GamepadAxis::RightZ,
        "DPadX" => GamepadAxis::DPadX,
        "DPadY" => GamepadAxis::DPadY,
        _ => bail!("unknown gamepad axis: '{s}'"),
    })
}

pub fn parse_gamepad_button(s: &str) -> Result<GamepadButton> {
    Ok(match s {
        "South" => GamepadButton::South,
        "East" => GamepadButton::East,
        "North" => GamepadButton::North,
        "West" => GamepadButton::West,
        "LeftTrigger" => GamepadButton::LeftTrigger,
        "LeftTrigger2" => GamepadButton::LeftTrigger2,
        "RightTrigger" => GamepadButton::RightTrigger,
        "RightTrigger2" => GamepadButton::RightTrigger2,
        "Select" => GamepadButton::Select,
        "Start" => GamepadButton::Start,
        "Mode" => GamepadButton::Mode,
        "LeftThumb" => GamepadButton::LeftThumb,
        "RightThumb" => GamepadButton::RightThumb,
        "DPadUp" => GamepadButton::DPadUp,
        "DPadDown" => GamepadButton::DPadDown,
        "DPadLeft" => GamepadButton::DPadLeft,
        "DPadRight" => GamepadButton::DPadRight,
        _ => bail!("unknown gamepad button: '{s}'"),
    })
}

pub fn parse_key(s: &str) -> Result<(KeyCode, KeyModifiers)> {
    let mut modifiers = KeyModifiers::NONE;
    let mut body = s;
    loop {
        if let Some(rest) = body.strip_prefix("Ctrl+") {
            modifiers |= KeyModifiers::CONTROL;
            body = rest;
        } else if let Some(rest) = body.strip_prefix("Alt+") {
            modifiers |= KeyModifiers::ALT;
            body = rest;
        } else if let Some(rest) = body.strip_prefix("Shift+") {
            modifiers |= KeyModifiers::SHIFT;
            body = rest;
        } else {
            break;
        }
    }
    let code = match body {
        "ArrowUp" | "Up" => KeyCode::Up,
        "ArrowDown" | "Down" => KeyCode::Down,
        "ArrowLeft" | "Left" => KeyCode::Left,
        "ArrowRight" | "Right" => KeyCode::Right,
        "Space" | " " => KeyCode::Char(' '),
        "Enter" => KeyCode::Enter,
        "Tab" => KeyCode::Tab,
        "Esc" | "Escape" => KeyCode::Esc,
        "Backspace" => KeyCode::Backspace,
        "Delete" => KeyCode::Delete,
        other if other.chars().count() == 1 => KeyCode::Char(other.chars().next().unwrap()),
        _ => bail!("unknown key: '{s}'"),
    };
    Ok((code, modifiers))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_gamepad_names() {
        assert_eq!(
            parse_gamepad_axis("RightStickX").unwrap(),
            GamepadAxis::RightStickX
        );
        assert_eq!(parse_gamepad_button("Start").unwrap(), GamepadButton::Start);
        assert!(parse_gamepad_axis("nope").is_err());
        assert!(parse_gamepad_button("nope").is_err());
    }

    #[test]
    fn parses_key_modifiers() {
        let (code, mods) = parse_key("Ctrl+c").unwrap();
        assert_eq!(code, KeyCode::Char('c'));
        assert!(mods.contains(KeyModifiers::CONTROL));
    }
}
