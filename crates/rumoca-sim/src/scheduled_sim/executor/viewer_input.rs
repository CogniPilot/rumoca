use super::*;

#[derive(Default)]
pub(super) struct ViewerInputDrain {
    pub(super) keys: Vec<KeyboardEvent>,
    pub(super) labels: Vec<String>,
    pub(super) realtime: Option<bool>,
    pub(super) quit: bool,
}

pub(super) fn drain_viewer_input(
    rx: &mpsc::Receiver<ViewerControlCommand>,
    first_packet_timeout: Option<Duration>,
    debug: bool,
) -> ViewerInputDrain {
    let mut drained = ViewerInputDrain::default();
    let mut events = Vec::new();
    if let Some(timeout) = first_packet_timeout
        && let Ok(command) = rx.recv_timeout(timeout)
    {
        drain_viewer_command(command, &mut drained, &mut events);
    }
    while let Ok(command) = rx.try_recv() {
        drain_viewer_command(command, &mut drained, &mut events);
    }
    if debug && !events.is_empty() {
        eprintln!(
            "\r[input] viewer keys: {}                    ",
            drained.labels.join(", ")
        );
    }
    drained.keys = events;
    drained
}

fn drain_viewer_command(
    command: ViewerControlCommand,
    drained: &mut ViewerInputDrain,
    events: &mut Vec<KeyboardEvent>,
) {
    match command {
        ViewerControlCommand::Quit => drained.quit = true,
        ViewerControlCommand::Realtime(enabled) => drained.realtime = Some(enabled),
        ViewerControlCommand::Key(key) => {
            let event = browser_key_to_event(&key);
            let suffix = if key.pressed() { "" } else { " up" };
            drained.labels.push(format!("{}{suffix}", key.code()));
            events.push(event);
        }
    }
}

pub(super) fn browser_key_to_event(key: &ViewerKeyCommand) -> KeyboardEvent {
    let code = match key.code() {
        ViewerKeyCode::Up => KeyCode::Up,
        ViewerKeyCode::Down => KeyCode::Down,
        ViewerKeyCode::Left => KeyCode::Left,
        ViewerKeyCode::Right => KeyCode::Right,
        ViewerKeyCode::Enter => KeyCode::Enter,
        ViewerKeyCode::Tab => KeyCode::Tab,
        ViewerKeyCode::Escape => KeyCode::Esc,
        ViewerKeyCode::Backspace => KeyCode::Backspace,
        ViewerKeyCode::Delete => KeyCode::Delete,
        ViewerKeyCode::Character(character) => KeyCode::Char(character.get()),
    };
    let mut modifiers = KeyModifiers::NONE;
    if key.shift() {
        modifiers |= KeyModifiers::SHIFT;
    }
    if key.ctrl() {
        modifiers |= KeyModifiers::CONTROL;
    }
    if key.alt() {
        modifiers |= KeyModifiers::ALT;
    }
    if key.pressed() {
        KeyboardEvent::holdable_press(code, modifiers)
    } else {
        KeyboardEvent::released(code, modifiers)
    }
}
