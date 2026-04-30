//! Runtime input-device factory.
//!
//! Picks gamepad or keyboard based on a config-mode string ("gamepad",
//! "keyboard", or "auto") and drives the chosen device against an
//! `InputEngine`. Wraps the concrete adapter crates so consumers depend
//! only on `rumoca-input`.

use anyhow::Result;

use crate::engine::{InputEngine, InputMode};
use rumoca_input_gamepad::GamepadDevice;

/// Re-export so signal handlers and other emergency-exit paths can
/// restore the terminal without holding a `Devices` instance.
pub use rumoca_input_keyboard::disable_raw_mode as disable_terminal_raw_mode;

/// Bundle of the active input device(s) for a lockstep run.
pub struct Devices {
    mode: InputMode,
    gamepad: Option<GamepadDevice>,
    keyboard_raw_mode: bool,
}

impl Devices {
    /// Build the device runtime from an `input.mode` config string.
    ///
    /// - `"gamepad"`: require a connected gamepad; error otherwise.
    /// - `"keyboard"`: enable terminal raw mode and read keyboard events.
    /// - `"auto"`: prefer gamepad; fall back to keyboard if none connected.
    pub fn new(requested: &str) -> Result<Self> {
        match requested {
            "gamepad" => {
                let gamepad = GamepadDevice::new()?;
                gamepad.announce_gamepads();
                Ok(Self {
                    mode: InputMode::Gamepad,
                    gamepad: Some(gamepad),
                    keyboard_raw_mode: false,
                })
            }
            "keyboard" => Self::keyboard(),
            "auto" => match GamepadDevice::new() {
                Ok(gamepad) if gamepad.is_connected() => {
                    gamepad.announce_gamepads();
                    Ok(Self {
                        mode: InputMode::Gamepad,
                        gamepad: Some(gamepad),
                        keyboard_raw_mode: false,
                    })
                }
                _ => {
                    eprintln!("[input] No gamepad detected, falling back to keyboard.");
                    Self::keyboard()
                }
            },
            other => anyhow::bail!("unknown input.mode: '{other}'"),
        }
    }

    fn keyboard() -> Result<Self> {
        let keyboard_raw_mode = rumoca_input_keyboard::enable_raw_mode();
        if keyboard_raw_mode {
            eprintln!("[input] Raw mode enabled");
        }
        Ok(Self {
            mode: InputMode::Keyboard,
            gamepad: None,
            keyboard_raw_mode,
        })
    }

    pub fn mode(&self) -> InputMode {
        self.mode
    }

    pub fn is_connected(&self) -> bool {
        match self.mode {
            InputMode::Gamepad => self
                .gamepad
                .as_ref()
                .is_some_and(GamepadDevice::is_connected),
            InputMode::Keyboard => true,
        }
    }

    /// Poll the active device once and feed the resulting snapshot/events
    /// into the engine.
    pub fn poll(&mut self, engine: &mut InputEngine, dt: f64) {
        match self.mode {
            InputMode::Gamepad => {
                if let Some(snapshot) = self.gamepad.as_mut().and_then(GamepadDevice::snapshot) {
                    engine.poll_gamepad_snapshot(&snapshot, dt);
                } else {
                    engine.poll_idle();
                }
            }
            InputMode::Keyboard => {
                let events = rumoca_input_keyboard::drain_events();
                engine.poll_keyboard_events(&events, dt);
            }
        }
    }

    /// Disable terminal raw mode if it was enabled. Call before exit so
    /// the user's terminal is left in a usable state. Safe to call
    /// multiple times.
    pub fn restore_terminal(&mut self) {
        if self.keyboard_raw_mode {
            rumoca_input_keyboard::disable_raw_mode();
            self.keyboard_raw_mode = false;
        }
    }
}

impl Drop for Devices {
    fn drop(&mut self) {
        self.restore_terminal();
    }
}
