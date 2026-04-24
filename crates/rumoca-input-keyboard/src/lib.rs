//! Crossterm-backed keyboard device adapter for `rumoca-input`.

use crossterm::event::{KeyCode as CrosstermKeyCode, KeyEventKind};
use rumoca_input::{KeyCode, KeyModifiers, KeyboardEvent};

/// Drain all pending key press/repeat events. Release events are filtered
/// out; Press and Repeat are treated the same.
pub fn drain_events() -> Vec<KeyboardEvent> {
    use crossterm::event::{Event, poll, read};
    let mut out = Vec::new();
    while poll(std::time::Duration::ZERO).unwrap_or(false) {
        let Ok(evt) = read() else { continue };
        let Event::Key(ke) = evt else { continue };
        if ke.kind == KeyEventKind::Release {
            continue;
        }
        if let Some(code) = map_code(ke.code) {
            out.push(KeyboardEvent::new(code, map_modifiers(ke.modifiers)));
        }
    }
    out
}

fn map_code(code: CrosstermKeyCode) -> Option<KeyCode> {
    Some(match code {
        CrosstermKeyCode::Char(value) => KeyCode::Char(value),
        CrosstermKeyCode::Up => KeyCode::Up,
        CrosstermKeyCode::Down => KeyCode::Down,
        CrosstermKeyCode::Left => KeyCode::Left,
        CrosstermKeyCode::Right => KeyCode::Right,
        CrosstermKeyCode::Enter => KeyCode::Enter,
        CrosstermKeyCode::Tab => KeyCode::Tab,
        CrosstermKeyCode::Esc => KeyCode::Esc,
        CrosstermKeyCode::Backspace => KeyCode::Backspace,
        CrosstermKeyCode::Delete => KeyCode::Delete,
        _ => return None,
    })
}

fn map_modifiers(modifiers: crossterm::event::KeyModifiers) -> KeyModifiers {
    let mut mapped = KeyModifiers::NONE;
    if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
        mapped |= KeyModifiers::SHIFT;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
        mapped |= KeyModifiers::CONTROL;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::ALT) {
        mapped |= KeyModifiers::ALT;
    }
    mapped
}

/// Enable crossterm raw mode. Returns `true` on success. Callers should
/// pair with `disable_raw_mode()` on drop.
pub fn enable_raw_mode() -> bool {
    match crossterm::terminal::enable_raw_mode() {
        Ok(()) => true,
        Err(e) => {
            eprintln!("[input] Raw mode failed: {e} - keyboard may not work");
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
    fn maps_crossterm_events_to_abstract_events() {
        assert_eq!(
            map_code(CrosstermKeyCode::Char('w')),
            Some(KeyCode::Char('w'))
        );
        assert_eq!(map_code(CrosstermKeyCode::Up), Some(KeyCode::Up));
        let mods = map_modifiers(crossterm::event::KeyModifiers::CONTROL);
        assert!(mods.contains(KeyModifiers::CONTROL));
    }
}
