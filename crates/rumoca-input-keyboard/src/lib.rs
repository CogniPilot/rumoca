//! Crossterm-backed keyboard device for `rumoca-input`.
//!
//! Owns the crossterm dependency, TOML key-name parsing, and the
//! non-blocking event drain. The engine state machine still lives in
//! `rumoca-input` — this crate is intentionally thin until a future
//! `InputDevice` trait refactor moves polling here.

pub use crossterm::event::{KeyCode, KeyEventKind, KeyModifiers};

use anyhow::{Result, bail};

/// Parse a TOML key name into `(KeyCode, KeyModifiers)`.
///
/// Accepts:
/// - Named keys: `ArrowUp/Down/Left/Right`, `Up/Down/Left/Right`, `Space`,
///   `Enter`, `Tab`, `Esc/Escape`, `Backspace`, `Delete`
/// - A single character: `"w"`, `"s"`, `"a"`
/// - Modifier prefixes: `Ctrl+`, `Alt+`, `Shift+` (composable)
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

/// Drain all pending key press/repeat events. Release events are filtered
/// out; Press and Repeat are treated the same.
pub fn drain_events() -> Vec<(KeyCode, KeyModifiers)> {
    use crossterm::event::{Event, poll, read};
    let mut out = Vec::new();
    while poll(std::time::Duration::ZERO).unwrap_or(false) {
        let Ok(evt) = read() else { continue };
        let Event::Key(ke) = evt else { continue };
        if ke.kind == KeyEventKind::Release {
            continue;
        }
        out.push((ke.code, ke.modifiers));
    }
    out
}

/// Enable crossterm raw mode. Returns `true` on success. Callers should
/// pair with `disable_raw_mode()` on drop.
pub fn enable_raw_mode() -> bool {
    match crossterm::terminal::enable_raw_mode() {
        Ok(()) => true,
        Err(e) => {
            eprintln!("[input] Raw mode failed: {e} — keyboard may not work");
            false
        }
    }
}

pub fn disable_raw_mode() {
    let _ = crossterm::terminal::disable_raw_mode();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_chars_and_arrows() {
        let (code, mods) = parse_key("w").unwrap();
        assert_eq!(code, KeyCode::Char('w'));
        assert_eq!(mods, KeyModifiers::NONE);
        let (code, _) = parse_key("ArrowUp").unwrap();
        assert_eq!(code, KeyCode::Up);
    }

    #[test]
    fn parses_modifier_prefixes() {
        let (code, mods) = parse_key("Ctrl+c").unwrap();
        assert_eq!(code, KeyCode::Char('c'));
        assert!(mods.contains(KeyModifiers::CONTROL));
    }

    #[test]
    fn rejects_unknown() {
        assert!(parse_key("Nonsense").is_err());
    }
}
