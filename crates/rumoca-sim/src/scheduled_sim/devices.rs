//! Runtime input-device factory.
//!
//! Composes the concrete adapter crates (`rumoca-input-gamepad`,
//! `rumoca-input-keyboard`) with the abstract `InputEngine` from
//! `rumoca-input`. Lives here so `rumoca-input` stays adapter-free.

use anyhow::Result;
use rumoca_input::{InputEngine, InputMode, KeyboardEvent};
#[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
use rumoca_input_gamepad::GamepadDevice;

/// Re-export so signal handlers and other emergency-exit paths can
/// restore the terminal without holding a `Devices` instance.
pub use rumoca_input_keyboard::disable_raw_mode as disable_terminal_raw_mode;

/// Bundle of the active input device(s) for a scheduled simulation run.
pub struct Devices {
    mode: InputMode,
    #[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
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
            "gamepad" => Self::gamepad(),
            "keyboard" => Self::keyboard(),
            "auto" => Self::auto(),
            other => anyhow::bail!("unknown input.mode: '{other}'"),
        }
    }

    #[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
    fn gamepad() -> Result<Self> {
        let gamepad = GamepadDevice::new()?;
        gamepad.announce_gamepads();
        Ok(Self {
            mode: InputMode::Gamepad,
            gamepad: Some(gamepad),
            keyboard_raw_mode: false,
        })
    }

    #[cfg(any(target_env = "musl", not(feature = "input-gamepad")))]
    fn gamepad() -> Result<Self> {
        anyhow::bail!("input.mode='gamepad' is not available in this build")
    }

    #[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
    fn auto() -> Result<Self> {
        match GamepadDevice::new() {
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
        }
    }

    #[cfg(any(target_env = "musl", not(feature = "input-gamepad")))]
    fn auto() -> Result<Self> {
        eprintln!(
            "[input] Gamepad input is not available in this build, falling back to keyboard."
        );
        Self::keyboard()
    }

    fn keyboard() -> Result<Self> {
        let keyboard_raw_mode = rumoca_input_keyboard::enable_raw_mode();
        if keyboard_raw_mode {
            eprintln!("[input] Raw mode enabled");
        }
        Ok(Self {
            mode: InputMode::Keyboard,
            #[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
            gamepad: None,
            keyboard_raw_mode,
        })
    }

    pub fn mode(&self) -> InputMode {
        self.mode
    }

    pub fn is_connected(&self) -> bool {
        match self.mode {
            InputMode::Gamepad => self.is_gamepad_connected(),
            InputMode::Keyboard => true,
        }
    }

    #[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
    fn is_gamepad_connected(&self) -> bool {
        self.gamepad
            .as_ref()
            .is_some_and(GamepadDevice::is_connected)
    }

    #[cfg(any(target_env = "musl", not(feature = "input-gamepad")))]
    fn is_gamepad_connected(&self) -> bool {
        false
    }

    /// Poll the active device once and feed the resulting snapshot/events
    /// into the engine.
    pub fn poll(&mut self, engine: &mut InputEngine, dt: f64) {
        self.poll_with_keyboard_events(engine, dt, Vec::new());
    }

    /// Poll the active device once, merging browser/viewer key events into the
    /// same input-engine tick.
    pub fn poll_with_keyboard_events(
        &mut self,
        engine: &mut InputEngine,
        dt: f64,
        extra_keyboard_events: Vec<KeyboardEvent>,
    ) {
        match self.mode {
            InputMode::Gamepad => {
                self.poll_gamepad_or_idle(engine, dt);
                if !extra_keyboard_events.is_empty() {
                    engine.poll_keyboard_events(&extra_keyboard_events, dt);
                }
            }
            InputMode::Keyboard => {
                let mut events = rumoca_input_keyboard::drain_events();
                events.extend(extra_keyboard_events);
                engine.poll_keyboard_events(&events, dt);
            }
        }
    }

    #[cfg(all(feature = "input-gamepad", not(target_env = "musl")))]
    fn poll_gamepad_or_idle(&mut self, engine: &mut InputEngine, dt: f64) {
        if let Some(snapshot) = self.gamepad.as_mut().and_then(GamepadDevice::snapshot) {
            engine.poll_gamepad_snapshot(&snapshot, dt);
        } else {
            engine.poll_idle();
        }
    }

    #[cfg(any(target_env = "musl", not(feature = "input-gamepad")))]
    fn poll_gamepad_or_idle(&mut self, engine: &mut InputEngine, _dt: f64) {
        engine.poll_idle();
    }

    /// Disable terminal raw mode if it was enabled. Safe to call multiple times.
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
