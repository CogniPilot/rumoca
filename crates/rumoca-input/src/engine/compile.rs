//! Compile TOML config into efficient runtime structures.
//!
//! This module parses device names, resolves local paths, and parses
//! precondition strings. Validation errors surface here at load time
//! (not at the first frame) so typos in config are caught early.

use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use rumoca_input_keyboard::{KeyCode, KeyModifiers};
use rumoca_input_gamepad::{Axis, Button};

use crate::config::{
    DeriveSpec, GamepadAxis, GamepadButton, GamepadConfig, InputConfig, Integrator, KeyBinding,
    KeyDecay, KeyboardConfig, LocalDef,
};

// ── Compiled runtime structures ────────────────────────────────────────────

/// A location within a local: `name` or `name[index]`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub name: String,
    pub index: Option<usize>,
}

impl Path {
    pub fn parse(s: &str) -> Self {
        // Strip leading "local:" prefix if present (tolerant).
        let s = s.strip_prefix("local:").unwrap_or(s);
        if let Some((lhs, rhs)) = s.rsplit_once('.')
            && let Ok(i) = rhs.parse::<usize>()
        {
            return Self {
                name: lhs.to_string(),
                index: Some(i),
            };
        }
        Self {
            name: s.to_string(),
            index: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreconditionOp {
    Lt,
    Le,
    Eq,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone)]
pub struct Precondition {
    pub var: Path,
    pub op: PreconditionOp,
    pub value: f64,
}

impl Precondition {
    /// Parse a minimal expression: `path OP number`.
    /// Supported operators: `<`, `<=`, `==`, `!=`, `>=`, `>`.
    pub fn parse(s: &str) -> Result<Self> {
        // Try longest operators first to avoid `<=` matching `<`.
        for (token, op) in [
            ("<=", PreconditionOp::Le),
            (">=", PreconditionOp::Ge),
            ("==", PreconditionOp::Eq),
            ("!=", PreconditionOp::Ne),
            ("<", PreconditionOp::Lt),
            (">", PreconditionOp::Gt),
        ] {
            if let Some((lhs, rhs)) = s.split_once(token) {
                let var = Path::parse(lhs.trim());
                let value = rhs
                    .trim()
                    .parse::<f64>()
                    .map_err(|e| anyhow!("precondition '{}': bad value: {}", s, e))?;
                return Ok(Self { var, op, value });
            }
        }
        Err(anyhow!(
            "precondition '{}': expected form 'path OP value'",
            s
        ))
    }

    pub fn eval(&self, lhs: f64) -> bool {
        match self.op {
            PreconditionOp::Lt => lhs < self.value,
            PreconditionOp::Le => lhs <= self.value,
            PreconditionOp::Eq => (lhs - self.value).abs() < f64::EPSILON,
            PreconditionOp::Ne => (lhs - self.value).abs() >= f64::EPSILON,
            PreconditionOp::Ge => lhs >= self.value,
            PreconditionOp::Gt => lhs > self.value,
        }
    }
}

// ── Source type: either a gamepad axis, a keyboard key, or a local ref ────

#[derive(Debug, Clone)]
pub enum IntegratorSource {
    GamepadAxis(Axis),
    Local(Path),
}

impl IntegratorSource {
    fn parse(s: &str) -> Result<Self> {
        if let Some(rest) = s.strip_prefix("local:") {
            Ok(Self::Local(Path::parse(rest)))
        } else {
            Ok(Self::GamepadAxis(parse_gamepad_axis(s)?))
        }
    }
}

// ── Compiled primitive structs ─────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct CompiledGamepadAxis {
    pub source: Axis,
    pub write: Path,
    pub scale: f64,
    pub invert: bool,
}

#[derive(Debug, Clone)]
pub struct CompiledIntegrator {
    pub source: IntegratorSource,
    pub write: Path,
    pub deadband: f64,
    pub rate: f64,
    pub clamp: [f64; 2],
}

#[derive(Debug, Clone)]
pub enum ButtonAction {
    /// Toggle a boolean local on rising edge.
    Toggle { state: Path },
    /// Emit a named signal on rising edge.
    Signal { name: String },
}

#[derive(Debug, Clone)]
pub struct CompiledGamepadButton {
    pub id: String,
    pub source: Button,
    pub action: ButtonAction,
    pub debounce_ms: u64,
    pub precondition: Option<Precondition>,
}

#[derive(Debug, Clone)]
pub enum KeyAction {
    Set { target: Path, value: f64 },
    Toggle { state: Path },
    Signal { name: String },
}

#[derive(Debug, Clone)]
pub struct CompiledKey {
    pub id: String,
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
    pub action: KeyAction,
    pub debounce_ms: u64,
    pub precondition: Option<Precondition>,
}

#[derive(Debug, Clone)]
pub struct CompiledDecay {
    pub targets: Vec<Path>,
    pub factor: f64,
    pub ref_dt: f64,
}

#[derive(Debug, Clone)]
pub enum DeriveRule {
    Linear {
        from: Path,
        scale: f64,
        offset: f64,
        clamp: Option<[f64; 2]>,
    },
    Conditional {
        from: Path,
        when_true: f64,
        when_false: f64,
    },
}

#[derive(Debug, Clone)]
pub struct CompiledDerive {
    pub target: Path,
    pub rule: DeriveRule,
}

// ── Top-level compiled config ──────────────────────────────────────────────

#[derive(Debug, Default)]
pub struct CompiledInput {
    pub gamepad_axes: Vec<CompiledGamepadAxis>,
    pub gamepad_integrators: Vec<CompiledIntegrator>,
    pub gamepad_buttons: Vec<CompiledGamepadButton>,
    pub keyboard_decay: Option<CompiledDecay>,
    pub keyboard_keys: Vec<CompiledKey>,
    pub keyboard_integrators: Vec<CompiledIntegrator>,
    pub derive: Vec<CompiledDerive>,
}

// ── Device name parsers ────────────────────────────────────────────────────

pub fn parse_gamepad_axis(s: &str) -> Result<Axis> {
    Ok(match s {
        "LeftStickX" => Axis::LeftStickX,
        "LeftStickY" => Axis::LeftStickY,
        "RightStickX" => Axis::RightStickX,
        "RightStickY" => Axis::RightStickY,
        "LeftZ" => Axis::LeftZ,
        "RightZ" => Axis::RightZ,
        "DPadX" => Axis::DPadX,
        "DPadY" => Axis::DPadY,
        _ => bail!("unknown gamepad axis: '{}'", s),
    })
}

pub fn parse_gamepad_button(s: &str) -> Result<Button> {
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
        _ => bail!("unknown gamepad button: '{}'", s),
    })
}

/// Parse a key name into `(KeyCode, KeyModifiers)`. Accepts:
///   - Single character: "w", "s", " ", "a"
///   - Named keys: "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Space", "Enter", "Tab", "Esc"
///   - Modifier prefixes: "Ctrl+c", "Alt+F1", "Shift+Tab"
pub fn parse_key(s: &str) -> Result<(KeyCode, KeyModifiers)> {
    let mut modifiers = KeyModifiers::NONE;
    let mut body = s;
    // Parse modifiers
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
        _ => bail!("unknown key: '{}'", s),
    };
    Ok((code, modifiers))
}

// ── Compile functions ──────────────────────────────────────────────────────

/// Compile an `InputConfig` plus the `[derive]` map into runtime structures.
/// Locals defs are passed in so we can cross-check referenced names.
pub fn compile(
    input: &InputConfig,
    derive: &HashMap<String, DeriveSpec>,
    locals: &HashMap<String, LocalDef>,
) -> Result<CompiledInput> {
    let mut out = CompiledInput::default();

    if let Some(gp) = input.gamepad.as_ref() {
        compile_gamepad(gp, locals, &mut out)?;
    }
    if let Some(kb) = input.keyboard.as_ref() {
        compile_keyboard(kb, locals, &mut out)?;
    }

    for (target, spec) in derive {
        out.derive.push(compile_derive(target, spec, locals)?);
    }

    Ok(out)
}

fn compile_gamepad(
    gp: &GamepadConfig,
    locals: &HashMap<String, LocalDef>,
    out: &mut CompiledInput,
) -> Result<()> {
    for (name, axis) in &gp.axes {
        out.gamepad_axes.push(compile_gp_axis(name, axis, locals)?);
    }
    for (name, integ) in &gp.integrators {
        out.gamepad_integrators
            .push(compile_integrator(name, integ, locals, /*keyboard=*/ false)?);
    }
    for (name, btn) in &gp.buttons {
        out.gamepad_buttons.push(compile_gp_button(name, btn, locals)?);
    }
    Ok(())
}

fn compile_keyboard(
    kb: &KeyboardConfig,
    locals: &HashMap<String, LocalDef>,
    out: &mut CompiledInput,
) -> Result<()> {
    if let Some(decay) = kb.decay.as_ref() {
        out.keyboard_decay = Some(compile_decay(decay, locals)?);
    }
    for (name, key) in &kb.keys {
        out.keyboard_keys.push(compile_key(name, key, locals)?);
    }
    for (name, integ) in &kb.integrators {
        out.keyboard_integrators
            .push(compile_integrator(name, integ, locals, /*keyboard=*/ true)?);
    }
    Ok(())
}

fn compile_gp_axis(
    name: &str,
    axis: &GamepadAxis,
    locals: &HashMap<String, LocalDef>,
) -> Result<CompiledGamepadAxis> {
    let src = parse_gamepad_axis(&axis.source)
        .map_err(|e| anyhow!("axis '{}': {}", name, e))?;
    let write = Path::parse(&axis.write);
    validate_local_ref(&write, locals, &format!("axis '{}' write", name))?;
    Ok(CompiledGamepadAxis {
        source: src,
        write,
        scale: axis.scale,
        invert: axis.invert,
    })
}

fn compile_integrator(
    name: &str,
    integ: &Integrator,
    locals: &HashMap<String, LocalDef>,
    keyboard: bool,
) -> Result<CompiledIntegrator> {
    let source = IntegratorSource::parse(&integ.source).map_err(|e| {
        anyhow!(
            "{} integrator '{}': {}",
            if keyboard { "keyboard" } else { "gamepad" },
            name,
            e
        )
    })?;
    let write = Path::parse(&integ.write);
    validate_local_ref(&write, locals, &format!("integrator '{}' write", name))?;
    if let IntegratorSource::Local(ref p) = source {
        validate_local_ref(p, locals, &format!("integrator '{}' source", name))?;
    }
    Ok(CompiledIntegrator {
        source,
        write,
        deadband: integ.deadband,
        rate: integ.rate,
        clamp: integ.clamp,
    })
}

fn compile_gp_button(
    id: &str,
    btn: &GamepadButton,
    locals: &HashMap<String, LocalDef>,
) -> Result<CompiledGamepadButton> {
    let source =
        parse_gamepad_button(&btn.source).map_err(|e| anyhow!("button '{}': {}", id, e))?;
    let action = compile_button_action(id, &btn.action, btn.state.as_deref(), btn.signal.as_deref(), locals)?;
    let precondition = btn
        .precondition
        .as_deref()
        .map(Precondition::parse)
        .transpose()
        .map_err(|e| anyhow!("button '{}' precondition: {}", id, e))?;
    Ok(CompiledGamepadButton {
        id: id.to_string(),
        source,
        action,
        debounce_ms: btn.debounce_ms.unwrap_or(0),
        precondition,
    })
}

fn compile_button_action(
    id: &str,
    action: &str,
    state: Option<&str>,
    signal: Option<&str>,
    locals: &HashMap<String, LocalDef>,
) -> Result<ButtonAction> {
    match action {
        "toggle" => {
            let state = state
                .ok_or_else(|| anyhow!("button '{}' action=toggle requires `state`", id))?;
            let path = Path::parse(state);
            validate_local_ref(&path, locals, &format!("button '{}' state", id))?;
            Ok(ButtonAction::Toggle { state: path })
        }
        "signal" => {
            let name = signal
                .ok_or_else(|| anyhow!("button '{}' action=signal requires `signal`", id))?;
            Ok(ButtonAction::Signal {
                name: name.to_string(),
            })
        }
        other => bail!("button '{}' unknown action '{}'", id, other),
    }
}

fn compile_key(
    id: &str,
    key: &KeyBinding,
    locals: &HashMap<String, LocalDef>,
) -> Result<CompiledKey> {
    let (code, modifiers) = parse_key(id).map_err(|e| anyhow!("key '{}': {}", id, e))?;
    let action = match key.action.as_str() {
        "set" => {
            let target = key
                .target
                .as_deref()
                .ok_or_else(|| anyhow!("key '{}' action=set requires `target`", id))?;
            let target = Path::parse(target);
            validate_local_ref(&target, locals, &format!("key '{}' target", id))?;
            let value = key
                .value
                .ok_or_else(|| anyhow!("key '{}' action=set requires `value`", id))?;
            KeyAction::Set { target, value }
        }
        "toggle" => {
            let state = key
                .state
                .as_deref()
                .ok_or_else(|| anyhow!("key '{}' action=toggle requires `state`", id))?;
            let state = Path::parse(state);
            validate_local_ref(&state, locals, &format!("key '{}' state", id))?;
            KeyAction::Toggle { state }
        }
        "signal" => {
            let name = key
                .signal
                .as_deref()
                .ok_or_else(|| anyhow!("key '{}' action=signal requires `signal`", id))?;
            KeyAction::Signal {
                name: name.to_string(),
            }
        }
        other => bail!("key '{}' unknown action '{}'", id, other),
    };
    let precondition = key
        .precondition
        .as_deref()
        .map(Precondition::parse)
        .transpose()
        .map_err(|e| anyhow!("key '{}' precondition: {}", id, e))?;
    Ok(CompiledKey {
        id: id.to_string(),
        code,
        modifiers,
        action,
        debounce_ms: key.debounce_ms.unwrap_or(0),
        precondition,
    })
}

fn compile_decay(decay: &KeyDecay, locals: &HashMap<String, LocalDef>) -> Result<CompiledDecay> {
    let targets = decay
        .targets
        .iter()
        .map(|t| {
            let path = Path::parse(t);
            validate_local_ref(&path, locals, &format!("decay target '{}'", t))?;
            Ok(path)
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(CompiledDecay {
        targets,
        factor: decay.factor,
        ref_dt: decay.ref_dt,
    })
}

fn compile_derive(
    target: &str,
    spec: &DeriveSpec,
    locals: &HashMap<String, LocalDef>,
) -> Result<CompiledDerive> {
    let target_path = Path::parse(target);
    validate_local_ref(&target_path, locals, &format!("derive target '{}'", target))?;
    let rule = match spec {
        DeriveSpec::Linear {
            from,
            scale,
            offset,
            clamp,
        } => {
            let from_path = Path::parse(from);
            validate_local_ref(&from_path, locals, &format!("derive '{}' from", target))?;
            DeriveRule::Linear {
                from: from_path,
                scale: *scale,
                offset: *offset,
                clamp: *clamp,
            }
        }
        DeriveSpec::Conditional {
            from,
            when_true,
            when_false,
        } => {
            let from_path = Path::parse(from);
            validate_local_ref(&from_path, locals, &format!("derive '{}' from", target))?;
            let wt = toml_to_f64(when_true)
                .ok_or_else(|| anyhow!("derive '{}' when_true: expected number", target))?;
            let wf = toml_to_f64(when_false)
                .ok_or_else(|| anyhow!("derive '{}' when_false: expected number", target))?;
            DeriveRule::Conditional {
                from: from_path,
                when_true: wt,
                when_false: wf,
            }
        }
    };
    Ok(CompiledDerive {
        target: target_path,
        rule,
    })
}

// ── Cross-reference validation ─────────────────────────────────────────────

fn validate_local_ref(path: &Path, locals: &HashMap<String, LocalDef>, ctx: &str) -> Result<()> {
    let Some(def) = locals.get(&path.name) else {
        bail!("{}: local '{}' is not declared in [locals]", ctx, path.name);
    };
    // If indexed, the local must be an array with sufficient length.
    if let Some(idx) = path.index {
        if def.kind != "array" {
            bail!(
                "{}: local '{}' is not an array but was indexed as '{}[{}]'",
                ctx,
                path.name,
                path.name,
                idx
            );
        }
        if let Some(len) = def.len
            && idx >= len
        {
            bail!(
                "{}: index {} out of range (len={}) for local '{}'",
                ctx,
                idx,
                len,
                path.name
            );
        }
    }
    Ok(())
}

fn toml_to_f64(v: &toml::Value) -> Option<f64> {
    match v {
        toml::Value::Integer(i) => Some(*i as f64),
        toml::Value::Float(f) => Some(*f),
        toml::Value::Boolean(b) => Some(if *b { 1.0 } else { 0.0 }),
        _ => None,
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_parse() {
        assert_eq!(
            Path::parse("armed"),
            Path {
                name: "armed".into(),
                index: None
            }
        );
        assert_eq!(
            Path::parse("rc.2"),
            Path {
                name: "rc".into(),
                index: Some(2)
            }
        );
        assert_eq!(
            Path::parse("local:rc.0"),
            Path {
                name: "rc".into(),
                index: Some(0)
            }
        );
        // Trailing non-integer after dot stays part of name.
        assert_eq!(
            Path::parse("foo.bar"),
            Path {
                name: "foo.bar".into(),
                index: None
            }
        );
    }

    #[test]
    fn precondition_all_ops() {
        let cases = [
            ("x <= 5", 5.0, true),
            ("x <= 5", 6.0, false),
            ("x < 5", 5.0, false),
            ("x < 5", 4.9, true),
            ("x >= 5", 5.0, true),
            ("x > 5", 5.0, false),
            ("x == 3", 3.0, true),
            ("x != 3", 3.0, false),
        ];
        for (src, lhs, expected) in cases {
            let p = Precondition::parse(src).unwrap_or_else(|e| panic!("{src}: {e}"));
            assert_eq!(p.eval(lhs), expected, "{src} vs {lhs}");
        }
    }

    #[test]
    fn precondition_indexed() {
        let p = Precondition::parse("rc.2 <= 1050").unwrap();
        assert_eq!(p.var.name, "rc");
        assert_eq!(p.var.index, Some(2));
        assert!(p.eval(1050.0));
        assert!(!p.eval(1051.0));
    }

    #[test]
    fn precondition_bad_form() {
        assert!(Precondition::parse("garbage").is_err());
        assert!(Precondition::parse("x <=").is_err());
    }

    #[test]
    fn parse_gamepad_axis_known() {
        assert_eq!(parse_gamepad_axis("RightStickX").unwrap(), Axis::RightStickX);
        assert!(parse_gamepad_axis("NotAnAxis").is_err());
    }

    #[test]
    fn parse_key_with_modifier() {
        let (code, mods) = parse_key("Ctrl+c").unwrap();
        assert_eq!(code, KeyCode::Char('c'));
        assert!(mods.contains(KeyModifiers::CONTROL));
    }

    #[test]
    fn compile_full_quadrotor_config() {
        let root =
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("examples")
                .join("quadrotor_sil")
                .join("quadrotor.toml");
        #[derive(serde::Deserialize)]
        struct Subset {
            #[serde(default)]
            locals: HashMap<String, LocalDef>,
            #[serde(default)]
            derive: HashMap<String, DeriveSpec>,
            input: crate::config::InputConfig,
        }
        let text = std::fs::read_to_string(&root).expect("read toml");
        let cfg: Subset = toml::from_str(&text).expect("parse");
        let compiled = compile(&cfg.input, &cfg.derive, &cfg.locals).expect("compile");
        // Quadrotor config expectations.
        assert_eq!(compiled.gamepad_axes.len(), 3);
        assert_eq!(compiled.gamepad_integrators.len(), 1);
        assert_eq!(compiled.gamepad_buttons.len(), 3);
        assert!(compiled.keyboard_decay.is_some());
        assert_eq!(compiled.keyboard_keys.len(), 12);
        assert_eq!(compiled.keyboard_integrators.len(), 1);
        assert_eq!(compiled.derive.len(), 5);
    }

    #[test]
    fn compile_rejects_undeclared_local() {
        use crate::config::{GamepadAxis, InputConfig};
        let mut axes = HashMap::new();
        axes.insert(
            "roll".to_string(),
            GamepadAxis {
                source: "RightStickX".to_string(),
                write: "not_declared".to_string(),
                scale: 1.0,
                invert: false,
            },
        );
        let gp = GamepadConfig {
            axes,
            integrators: HashMap::new(),
            buttons: HashMap::new(),
        };
        let input = InputConfig {
            mode: "gamepad".into(),
            gamepad: Some(gp),
            keyboard: None,
        };
        let err = compile(&input, &HashMap::new(), &HashMap::new()).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("not declared"), "got: {msg}");
    }
}
