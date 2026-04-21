//! Config-driven input engine.
//!
//! Owns gamepad (via `gilrs`) and keyboard (via `crossterm`) polling.
//! Maintains declared locals, evaluates preconditions, applies derive
//! rules. The `sim_loop` queries the engine each frame for values and
//! signals — it has no quadrotor-specific knowledge.

pub mod compile;

use std::collections::{HashMap, HashSet};
use std::time::Instant;

use anyhow::{Result, anyhow, bail};
use rumoca_input_gamepad::Gilrs;

use crate::config::{DeriveSpec, InputConfig, LocalDef};

pub use compile::{
    ButtonAction, CompiledDecay, CompiledDerive, CompiledGamepadAxis, CompiledGamepadButton,
    CompiledInput, CompiledIntegrator, CompiledKey, DeriveRule, IntegratorSource, KeyAction,
    Path, Precondition, PreconditionOp,
};

// ── Runtime values ─────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum LocalValue {
    Bool(bool),
    Float(f64),
    IntArray(Vec<i32>),
    FloatArray(Vec<f64>),
}

impl LocalValue {
    pub fn as_f64_at(&self, index: Option<usize>) -> Option<f64> {
        match (self, index) {
            (Self::Bool(b), None) => Some(if *b { 1.0 } else { 0.0 }),
            (Self::Float(f), None) => Some(*f),
            (Self::IntArray(v), Some(i)) => v.get(i).copied().map(f64::from),
            (Self::FloatArray(v), Some(i)) => v.get(i).copied(),
            _ => None,
        }
    }

    pub fn as_bool_at(&self, index: Option<usize>) -> Option<bool> {
        match (self, index) {
            (Self::Bool(b), None) => Some(*b),
            (Self::Float(f), None) => Some(*f != 0.0),
            (Self::IntArray(v), Some(i)) => v.get(i).copied().map(|x| x != 0),
            (Self::FloatArray(v), Some(i)) => v.get(i).copied().map(|x| x != 0.0),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputMode {
    Gamepad,
    Keyboard,
}

/// A snapshot of gamepad state at one poll. Constructed by `snapshot_gamepad`
/// (device I/O) and consumed by `process_gamepad` (pure logic) — splitting
/// these makes the state machine testable without real hardware.
#[derive(Debug, Clone, Default)]
struct GamepadSnapshot {
    axis_values: HashMap<rumoca_input_gamepad::Axis, f64>,
    button_pressed: HashMap<rumoca_input_gamepad::Button, bool>,
}

// ── The engine ─────────────────────────────────────────────────────────────

pub struct InputEngine {
    mode: InputMode,
    locals: HashMap<String, LocalValue>,
    defaults: HashMap<String, LocalValue>,
    compiled: CompiledInput,

    gilrs: Option<Gilrs>,
    keyboard_raw_mode: bool,

    /// Previous pressed/unpressed state of each bound button/key, keyed by binding id.
    /// Used for rising-edge detection. Populated by device polling (phase 2c/2d).
    #[allow(dead_code)]
    edge_prev: HashMap<String, bool>,

    /// Last time a given state was toggled (for debounce), keyed by the state name.
    /// Populated by device polling (phase 2c/2d).
    #[allow(dead_code)]
    debounce: HashMap<String, Instant>,

    pending_signals: HashSet<String>,
    last_poll: Instant,
}

impl InputEngine {
    pub fn new(
        input_cfg: &InputConfig,
        locals_cfg: &HashMap<String, LocalDef>,
        derive_cfg: &HashMap<String, DeriveSpec>,
    ) -> Result<Self> {
        let defaults = initialize_locals(locals_cfg)?;
        let compiled = compile::compile(input_cfg, derive_cfg, locals_cfg)?;
        let (mode, gilrs, keyboard_raw_mode) = init_mode(input_cfg.mode.as_str())?;
        let locals = defaults.clone();

        Ok(Self {
            mode,
            locals,
            defaults,
            compiled,
            gilrs,
            keyboard_raw_mode,
            edge_prev: HashMap::new(),
            debounce: HashMap::new(),
            pending_signals: HashSet::new(),
            last_poll: Instant::now(),
        })
    }

    pub fn mode(&self) -> InputMode {
        self.mode
    }

    pub fn is_connected(&self) -> bool {
        match self.mode {
            InputMode::Gamepad => self.gilrs.as_ref().is_some_and(|g| g.gamepads().count() > 0),
            InputMode::Keyboard => true,
        }
    }

    /// Reset all locals to their defaults and drain any pending signals.
    pub fn reset(&mut self) {
        self.locals = self.defaults.clone();
        self.pending_signals.clear();
        self.debounce.clear();
        // edge_prev intentionally kept: we don't want a held button to
        // re-trigger after reset.
    }

    /// Step one frame: poll the active device, then apply derive rules.
    pub fn poll(&mut self, dt: f64) {
        match self.mode {
            InputMode::Gamepad => self.poll_gamepad(dt),
            InputMode::Keyboard => self.poll_keyboard(dt),
        }
        self.apply_derive();
        self.last_poll = Instant::now();
    }

    /// Read a value by path. Supports dotted indexing (`rc.2`) and the
    /// optional `local:` prefix.
    pub fn get(&self, path: &str) -> Option<f64> {
        let p = Path::parse(path);
        self.locals.get(&p.name)?.as_f64_at(p.index)
    }

    /// Read a boolean value by path.
    pub fn get_bool(&self, path: &str) -> Option<bool> {
        let p = Path::parse(path);
        self.locals.get(&p.name)?.as_bool_at(p.index)
    }

    /// Consume a pending signal if present.
    pub fn take_signal(&mut self, name: &str) -> bool {
        self.pending_signals.remove(name)
    }

    /// Write a scalar value to a local by path. Used by the receive path
    /// when the incoming codec routes a field to `local:<name>`. No-op if
    /// the target doesn't exist or the type/index doesn't fit.
    pub fn set_local(&mut self, path: &str, value: f64) {
        let p = Path::parse(path);
        self.write_local(&p, value);
    }

    // ── Gamepad polling ───────────────────────────────────────────────────

    fn poll_gamepad(&mut self, dt: f64) {
        let Some(snap) = self.snapshot_gamepad() else {
            return;
        };
        self.process_gamepad(&snap, dt);
    }

    /// Drain events and read current axis/button state from the first gamepad.
    /// Returns None if no gamepad is connected.
    fn snapshot_gamepad(&mut self) -> Option<GamepadSnapshot> {
        {
            let gilrs = self.gilrs.as_mut()?;
            while gilrs.next_event().is_some() {}
        }
        let gilrs = self.gilrs.as_ref()?;
        let (_, gp) = gilrs.gamepads().next()?;

        let mut axis_values: HashMap<rumoca_input_gamepad::Axis, f64> = HashMap::new();
        for a in &self.compiled.gamepad_axes {
            axis_values
                .entry(a.source)
                .or_insert_with(|| f64::from(gp.value(a.source)));
        }
        for i in &self.compiled.gamepad_integrators {
            if let IntegratorSource::GamepadAxis(ax) = i.source {
                axis_values
                    .entry(ax)
                    .or_insert_with(|| f64::from(gp.value(ax)));
            }
        }

        let button_pressed: HashMap<rumoca_input_gamepad::Button, bool> = self
            .compiled
            .gamepad_buttons
            .iter()
            .map(|b| (b.source, gp.is_pressed(b.source)))
            .collect();

        Some(GamepadSnapshot {
            axis_values,
            button_pressed,
        })
    }

    fn process_gamepad(&mut self, snap: &GamepadSnapshot, dt: f64) {
        // Axes: source axis value -> write local (with scale + optional invert).
        let axes = self.compiled.gamepad_axes.clone();
        for axis in &axes {
            let raw = snap.axis_values.get(&axis.source).copied().unwrap_or(0.0);
            let v = if axis.invert { -raw } else { raw } * axis.scale;
            self.write_local(&axis.write, v);
        }

        // Integrators: integrate deadbanded source into target local at `rate` * dt.
        let integrators = self.compiled.gamepad_integrators.clone();
        for integ in &integrators {
            let src = match &integ.source {
                IntegratorSource::GamepadAxis(ax) => {
                    snap.axis_values.get(ax).copied().unwrap_or(0.0)
                }
                IntegratorSource::Local(p) => self.read_path(p).unwrap_or(0.0),
            };
            self.step_integrator(integ, src, dt);
        }

        // Buttons: rising-edge detection + debounce + precondition.
        let buttons = self.compiled.gamepad_buttons.clone();
        for btn in &buttons {
            let pressed = snap
                .button_pressed
                .get(&btn.source)
                .copied()
                .unwrap_or(false);
            let prev = self.edge_prev.get(&btn.id).copied().unwrap_or(false);
            self.edge_prev.insert(btn.id.clone(), pressed);
            if !pressed || prev {
                continue; // only fire on rising edge
            }
            self.fire_button(&btn.action, btn.debounce_ms, btn.precondition.as_ref());
        }
    }

    // ── Keyboard polling ──────────────────────────────────────────────────

    fn poll_keyboard(&mut self, dt: f64) {
        let events = Self::drain_keyboard_events();
        self.process_keyboard(&events, dt);
    }

    /// Drain all pending keyboard events via crossterm. Release events are
    /// filtered out; Press and Repeat are treated the same.
    fn drain_keyboard_events() -> Vec<(rumoca_input_keyboard::KeyCode, rumoca_input_keyboard::KeyModifiers)> {
        rumoca_input_keyboard::drain_events()
    }

    fn process_keyboard(
        &mut self,
        events: &[(rumoca_input_keyboard::KeyCode, rumoca_input_keyboard::KeyModifiers)],
        dt: f64,
    ) {
        // 1. Decay configured targets (applied every frame regardless of events).
        if let Some(decay) = self.compiled.keyboard_decay.clone() {
            let factor = decay.factor.powf(dt / decay.ref_dt);
            for target in &decay.targets {
                let current = self.read_path(target).unwrap_or(0.0);
                self.write_local(target, current * factor);
            }
        }

        // 2a. Ctrl+C → emit "quit" signal. Raw mode disables the tty's ISIG
        //     translation, so the OS never delivers SIGINT — we have to
        //     recognize the keystroke ourselves or the user is stuck.
        for (code, mods) in events {
            if *code == rumoca_input_keyboard::KeyCode::Char('c')
                && mods.contains(rumoca_input_keyboard::KeyModifiers::CONTROL)
            {
                eprintln!("\r[input] Ctrl+C → quit                    \r");
                self.pending_signals.insert("quit".to_string());
            }
        }

        // 2b. Dispatch key events: match against bindings, fire action.
        let keys = self.compiled.keyboard_keys.clone();
        for (code, mods) in events {
            for key in keys
                .iter()
                .filter(|k| k.code == *code && k.modifiers == *mods)
            {
                self.fire_key(key);
            }
        }

        // 3. Run keyboard integrators (source is typically a local that decays).
        let integrators = self.compiled.keyboard_integrators.clone();
        for integ in &integrators {
            let src = match &integ.source {
                IntegratorSource::GamepadAxis(_) => 0.0, // invalid in keyboard mode
                IntegratorSource::Local(p) => self.read_path(p).unwrap_or(0.0),
            };
            self.step_integrator(integ, src, dt);
        }
    }

    fn fire_key(&mut self, key: &CompiledKey) {
        match &key.action {
            KeyAction::Set { target, value } => {
                // Set: precondition (if any), no debounce.
                if key
                    .precondition
                    .as_ref()
                    .is_some_and(|pre| !self.passes_precondition(pre))
                {
                    return;
                }
                self.write_local(target, *value);
            }
            KeyAction::Toggle { state } => {
                self.fire_button(
                    &ButtonAction::Toggle {
                        state: state.clone(),
                    },
                    key.debounce_ms,
                    key.precondition.as_ref(),
                );
            }
            KeyAction::Signal { name } => {
                self.fire_button(
                    &ButtonAction::Signal { name: name.clone() },
                    key.debounce_ms,
                    key.precondition.as_ref(),
                );
            }
        }
    }

    // ── Shared helpers (used by gamepad now, keyboard next) ───────────────

    fn read_path(&self, p: &Path) -> Option<f64> {
        self.locals.get(&p.name)?.as_f64_at(p.index)
    }

    fn step_integrator(&mut self, integ: &CompiledIntegrator, src: f64, dt: f64) {
        let deadbanded = if src.abs() < integ.deadband { 0.0 } else { src };
        let current = self.read_path(&integ.write).unwrap_or(0.0);
        let new = (current + deadbanded * integ.rate * dt).clamp(integ.clamp[0], integ.clamp[1]);
        self.write_local(&integ.write, new);
    }

    /// Shared entry point for button/key actions. Handles debounce +
    /// precondition + dispatching to the right side effect.
    fn fire_button(
        &mut self,
        action: &ButtonAction,
        debounce_ms: u64,
        precondition: Option<&Precondition>,
    ) {
        if let Some(pre) = precondition {
            let lhs = self.read_path(&pre.var).unwrap_or(0.0);
            if !pre.eval(lhs) {
                return;
            }
        }
        let debounce_key = match action {
            ButtonAction::Toggle { state } => format!("toggle:{}", state.name),
            ButtonAction::Signal { name } => format!("signal:{name}"),
        };
        if let Some(last) = self.debounce.get(&debounce_key)
            && (last.elapsed().as_millis() as u64) < debounce_ms
        {
            return;
        }
        self.debounce.insert(debounce_key, Instant::now());
        match action {
            ButtonAction::Toggle { state } => {
                let current = self.get_bool_at(state).unwrap_or(false);
                self.write_local(state, if current { 0.0 } else { 1.0 });
            }
            ButtonAction::Signal { name } => {
                self.pending_signals.insert(name.clone());
            }
        }
    }

    fn get_bool_at(&self, p: &Path) -> Option<bool> {
        self.locals.get(&p.name)?.as_bool_at(p.index)
    }

    fn passes_precondition(&self, pre: &Precondition) -> bool {
        let lhs = self.read_path(&pre.var).unwrap_or(0.0);
        pre.eval(lhs)
    }

    // ── Derive application ────────────────────────────────────────────────

    fn apply_derive(&mut self) {
        // Snapshot rules once so the borrow on self.compiled doesn't block writes to self.locals.
        let rules: Vec<CompiledDerive> = self.compiled.derive.clone();
        for rule in &rules {
            if let Some(value) = self.eval_derive(&rule.rule) {
                self.write_local(&rule.target, value);
            }
        }
    }

    fn eval_derive(&self, rule: &DeriveRule) -> Option<f64> {
        match rule {
            DeriveRule::Linear {
                from,
                scale,
                offset,
                clamp,
            } => {
                let src = self
                    .locals
                    .get(&from.name)
                    .and_then(|v| v.as_f64_at(from.index))?;
                let mut out = src * *scale + *offset;
                if let Some([lo, hi]) = clamp {
                    out = out.clamp(*lo, *hi);
                }
                Some(out)
            }
            DeriveRule::Conditional {
                from,
                when_true,
                when_false,
            } => {
                let b = self
                    .locals
                    .get(&from.name)
                    .and_then(|v| v.as_bool_at(from.index))?;
                Some(if b { *when_true } else { *when_false })
            }
        }
    }

    /// Write a float value into a local. Supports indexed writes into int/float arrays
    /// (integer arrays round to nearest i32). No-op if the target doesn't exist.
    pub(crate) fn write_local(&mut self, target: &Path, value: f64) {
        let Some(slot) = self.locals.get_mut(&target.name) else {
            return;
        };
        match (slot, target.index) {
            (LocalValue::Float(f), None) => *f = value,
            (LocalValue::Bool(b), None) => *b = value != 0.0,
            (LocalValue::IntArray(v), Some(i)) => {
                if let Some(dst) = v.get_mut(i) {
                    *dst = value.round() as i32;
                }
            }
            (LocalValue::FloatArray(v), Some(i)) => {
                if let Some(dst) = v.get_mut(i) {
                    *dst = value;
                }
            }
            _ => {} // mismatched index/type — already caught at compile time
        }
    }
}

impl Drop for InputEngine {
    fn drop(&mut self) {
        if self.keyboard_raw_mode {
            rumoca_input_keyboard::disable_raw_mode();
        }
    }
}

// ── Test helpers (shared across test modules) ─────────────────────────────

#[cfg(test)]
impl InputEngine {
    /// Build an engine from already-compiled pieces without touching any
    /// input device. Test-only convenience; sibling test modules rely on
    /// this to exercise the state machine without hardware.
    pub(crate) fn from_parts_for_test(
        defaults: HashMap<String, LocalValue>,
        compiled: CompiledInput,
    ) -> Self {
        Self {
            mode: InputMode::Keyboard,
            locals: defaults.clone(),
            defaults,
            compiled,
            gilrs: None,
            keyboard_raw_mode: false,
            edge_prev: HashMap::new(),
            debounce: HashMap::new(),
            pending_signals: HashSet::new(),
            last_poll: Instant::now(),
        }
    }

    pub(crate) fn apply_derive_for_test(&mut self) {
        self.apply_derive();
    }
}

// ── Initialization helpers ─────────────────────────────────────────────────

fn init_mode(requested: &str) -> Result<(InputMode, Option<Gilrs>, bool)> {
    match requested {
        "gamepad" => {
            let gilrs = Gilrs::new().map_err(|e| anyhow!("gilrs init failed: {:?}", e))?;
            announce_gamepads(&gilrs);
            Ok((InputMode::Gamepad, Some(gilrs), false))
        }
        "keyboard" => {
            let raw = enable_keyboard_raw_mode();
            Ok((InputMode::Keyboard, None, raw))
        }
        "auto" => {
            let gilrs = Gilrs::new().ok();
            let has_gp = gilrs.as_ref().is_some_and(|g| g.gamepads().count() > 0);
            if has_gp {
                if let Some(ref g) = gilrs {
                    announce_gamepads(g);
                }
                Ok((InputMode::Gamepad, gilrs, false))
            } else {
                eprintln!("[input] No gamepad detected, falling back to keyboard.");
                let raw = enable_keyboard_raw_mode();
                Ok((InputMode::Keyboard, None, raw))
            }
        }
        other => bail!("unknown input.mode: '{}'", other),
    }
}

fn announce_gamepads(gilrs: &Gilrs) {
    for (_id, gp) in gilrs.gamepads() {
        eprintln!("[input] gamepad: {} ({})", gp.name(), gp.os_name());
    }
}

fn enable_keyboard_raw_mode() -> bool {
    let ok = rumoca_input_keyboard::enable_raw_mode();
    if ok {
        eprintln!("[input] Raw mode enabled");
    }
    ok
}

pub(crate) fn initialize_locals(
    defs: &HashMap<String, LocalDef>,
) -> Result<HashMap<String, LocalValue>> {
    let mut out = HashMap::with_capacity(defs.len());
    for (name, def) in defs {
        let value = match def.kind.as_str() {
            "bool" => {
                let b = def
                    .default
                    .as_ref()
                    .map(|v| v.as_bool().ok_or_else(|| anyhow!("local '{}': bad bool default", name)))
                    .transpose()?
                    .unwrap_or(false);
                LocalValue::Bool(b)
            }
            "float" => {
                let f = def
                    .default
                    .as_ref()
                    .map(|v| toml_to_f64(v).ok_or_else(|| anyhow!("local '{}': bad float default", name)))
                    .transpose()?
                    .unwrap_or(0.0);
                LocalValue::Float(f)
            }
            "array" => build_array_local(name, def)?,
            other => bail!("local '{}': unknown type '{}'", name, other),
        };
        out.insert(name.clone(), value);
    }
    Ok(out)
}

fn build_array_local(name: &str, def: &LocalDef) -> Result<LocalValue> {
    let len = def
        .len
        .ok_or_else(|| anyhow!("local '{}': array requires `len`", name))?;
    let element = def
        .element
        .as_deref()
        .ok_or_else(|| anyhow!("local '{}': array requires `element`", name))?;
    match element {
        "int" => {
            let default_i = match def.default.as_ref() {
                Some(v) => toml_to_i64(v)
                    .ok_or_else(|| anyhow!("local '{}': bad int default", name))?,
                None => 0,
            };
            Ok(LocalValue::IntArray(vec![default_i as i32; len]))
        }
        "float" => {
            let default_f = match def.default.as_ref() {
                Some(v) => toml_to_f64(v)
                    .ok_or_else(|| anyhow!("local '{}': bad float default", name))?,
                None => 0.0,
            };
            Ok(LocalValue::FloatArray(vec![default_f; len]))
        }
        other => bail!("local '{}': unknown array element '{}'", name, other),
    }
}

fn toml_to_f64(v: &toml::Value) -> Option<f64> {
    match v {
        toml::Value::Integer(i) => Some(*i as f64),
        toml::Value::Float(f) => Some(*f),
        toml::Value::Boolean(b) => Some(if *b { 1.0 } else { 0.0 }),
        _ => None,
    }
}

fn toml_to_i64(v: &toml::Value) -> Option<i64> {
    match v {
        toml::Value::Integer(i) => Some(*i),
        toml::Value::Float(f) => Some(*f as i64),
        toml::Value::Boolean(b) => Some(if *b { 1 } else { 0 }),
        _ => None,
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{DeriveSpec, InputConfig, LocalDef, SignalsConfig};
    use serde::Deserialize;
    use std::path::PathBuf;

    /// Minimal subset of the full TOML config needed to exercise the input
    /// engine in tests — avoids depending on the orchestration-layer config.
    #[derive(Deserialize, Default)]
    struct InputSections {
        #[serde(default)]
        locals: HashMap<String, LocalDef>,
        #[serde(default)]
        derive: HashMap<String, DeriveSpec>,
        input: Option<InputConfig>,
        #[allow(dead_code)]
        #[serde(default)]
        signals: Option<SignalsConfig>,
    }

    fn load_quadrotor() -> InputSections {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let root = manifest.parent().unwrap().parent().unwrap();
        let path = root
            .join("examples")
            .join("quadrotor_sil")
            .join("quadrotor.toml");
        let text = std::fs::read_to_string(&path).expect("read toml");
        toml::from_str(&text).expect("parse input sections")
    }

    /// Build an engine bypassing device init (which needs gilrs/tty).
    fn build_for_test(cfg: &InputSections) -> InputEngine {
        let input_cfg = cfg.input.as_ref().unwrap();
        let defaults = initialize_locals(&cfg.locals).expect("init locals");
        let compiled = compile::compile(input_cfg, &cfg.derive, &cfg.locals).expect("compile");
        InputEngine {
            mode: InputMode::Keyboard, // doesn't matter; we're not polling
            locals: defaults.clone(),
            defaults,
            compiled,
            gilrs: None,
            keyboard_raw_mode: false,
            edge_prev: HashMap::new(),
            debounce: HashMap::new(),
            pending_signals: HashSet::new(),
            last_poll: Instant::now(),
        }
    }

    #[test]
    fn locals_initialize_with_defaults() {
        let cfg = load_quadrotor();
        let eng = build_for_test(&cfg);
        assert_eq!(eng.get_bool("armed"), Some(false));
        assert_eq!(eng.get("throttle"), Some(0.0));
        // rc is an int array of len 16, default 1500
        assert_eq!(eng.get("rc.0"), Some(1500.0));
        assert_eq!(eng.get("rc.15"), Some(1500.0));
        assert_eq!(eng.get("rc.16"), None); // out of range
    }

    #[test]
    fn derive_produces_rc_channels_from_locals() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // Initial state: roll_cmd=0, pitch_cmd=0, yaw_cmd=0, throttle=0, armed=false.
        // Applying derive should populate rc.0..rc.4 accordingly.
        eng.apply_derive();
        // roll_cmd=0 -> rc.0 = 0*500 + 1500 = 1500
        assert_eq!(eng.get("rc.0"), Some(1500.0));
        // pitch_cmd=0 -> rc.1 = 0*-500 + 1500 = 1500
        assert_eq!(eng.get("rc.1"), Some(1500.0));
        // throttle=0 -> rc.2 = 0*1000 + 1000 = 1000
        assert_eq!(eng.get("rc.2"), Some(1000.0));
        // yaw_cmd=0 -> rc.3 = 1500
        assert_eq!(eng.get("rc.3"), Some(1500.0));
        // armed=false -> rc.4 = when_false = 1000
        assert_eq!(eng.get("rc.4"), Some(1000.0));
    }

    #[test]
    fn derive_conditional_on_armed() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // Set armed = true directly.
        eng.write_local(&Path::parse("armed"), 1.0);
        eng.apply_derive();
        assert_eq!(eng.get("rc.4"), Some(2000.0));
    }

    #[test]
    fn derive_linear_with_clamp() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // roll_cmd at extreme should clamp at rc.0 bounds [1000, 2000].
        eng.write_local(&Path::parse("roll_cmd"), 10.0); // 10*500 + 1500 = 6500 → clamp to 2000
        eng.apply_derive();
        assert_eq!(eng.get("rc.0"), Some(2000.0));
        eng.write_local(&Path::parse("roll_cmd"), -10.0); // -5000+1500 → clamp to 1000
        eng.apply_derive();
        assert_eq!(eng.get("rc.0"), Some(1000.0));
    }

    #[test]
    fn reset_restores_defaults() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        eng.write_local(&Path::parse("throttle"), 0.7);
        eng.write_local(&Path::parse("armed"), 1.0);
        assert_eq!(eng.get("throttle"), Some(0.7));
        eng.reset();
        assert_eq!(eng.get("throttle"), Some(0.0));
        assert_eq!(eng.get_bool("armed"), Some(false));
    }

    #[test]
    fn signal_take_is_idempotent() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        eng.pending_signals.insert("reset".to_string());
        assert!(eng.take_signal("reset"));
        assert!(!eng.take_signal("reset")); // already consumed
    }

    // ── Gamepad polling (phase 2c) ────────────────────────────────────────

    fn snap(
        axes: &[(rumoca_input_gamepad::Axis, f64)],
        buttons: &[(rumoca_input_gamepad::Button, bool)],
    ) -> GamepadSnapshot {
        GamepadSnapshot {
            axis_values: axes.iter().copied().collect(),
            button_pressed: buttons.iter().copied().collect(),
        }
    }

    #[test]
    fn gamepad_axis_writes_local() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // Quadrotor config: RightStickX -> roll_cmd (scale=1.0, no invert).
        let s = snap(&[(rumoca_input_gamepad::Axis::RightStickX, 0.75)], &[]);
        eng.process_gamepad(&s, 0.01);
        assert_eq!(eng.get("roll_cmd"), Some(0.75));
    }

    #[test]
    fn gamepad_integrator_accumulates_with_deadband_and_clamp() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // LeftStickY integrator: deadband 0.1, rate 0.7, clamp [0, 1] -> throttle.
        // Below deadband: no change.
        let below = snap(&[(rumoca_input_gamepad::Axis::LeftStickY, 0.05)], &[]);
        eng.process_gamepad(&below, 1.0);
        assert_eq!(eng.get("throttle"), Some(0.0));

        // Push stick full forward: 1.0 * 0.7 * dt=0.5 = 0.35 accumulated
        let full = snap(&[(rumoca_input_gamepad::Axis::LeftStickY, 1.0)], &[]);
        eng.process_gamepad(&full, 0.5);
        assert!((eng.get("throttle").unwrap() - 0.35).abs() < 1e-9);

        // Big dt pushes beyond clamp upper bound 1.0.
        eng.process_gamepad(&full, 10.0);
        assert_eq!(eng.get("throttle"), Some(1.0));
    }

    #[test]
    fn gamepad_button_toggle_on_rising_edge_only() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // arm button: Start, debounce 500ms, precondition "rc.2 <= 1050".
        // rc.2 starts at 1000 after derive runs (throttle=0 → rc.2 = 1000).
        eng.apply_derive();
        assert_eq!(eng.get("rc.2"), Some(1000.0));

        // Press and release — should toggle armed exactly once.
        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::Start, true)]), 0.01);
        assert_eq!(eng.get_bool("armed"), Some(true));
        // Hold: no retrigger.
        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::Start, true)]), 0.01);
        assert_eq!(eng.get_bool("armed"), Some(true));
        // Release + press: WOULD toggle again, but debounce blocks (500ms not elapsed).
        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::Start, false)]), 0.01);
        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::Start, true)]), 0.01);
        assert_eq!(
            eng.get_bool("armed"),
            Some(true),
            "debounce must prevent re-toggle within 500ms"
        );
    }

    #[test]
    fn gamepad_arm_precondition_blocks_when_throttle_high() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // Push throttle high so rc.2 goes above 1050.
        eng.write_local(&Path::parse("throttle"), 0.5); // rc.2 = 0.5*1000+1000 = 1500
        eng.apply_derive();
        assert_eq!(eng.get("rc.2"), Some(1500.0));

        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::Start, true)]), 0.01);
        assert_eq!(
            eng.get_bool("armed"),
            Some(false),
            "precondition rc.2 <= 1050 should block arm toggle"
        );
    }

    #[test]
    fn gamepad_signal_button_emits_on_rising_edge() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // reset button: South -> signal "reset"
        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::South, true)]), 0.01);
        assert!(eng.take_signal("reset"));
        // Already consumed.
        assert!(!eng.take_signal("reset"));
        // Held button: no new emission.
        eng.process_gamepad(&snap(&[], &[(rumoca_input_gamepad::Button::South, true)]), 0.01);
        assert!(!eng.take_signal("reset"));
    }

    // ── Keyboard polling (phase 2d) ───────────────────────────────────────

    fn key(c: char) -> (rumoca_input_keyboard::KeyCode, rumoca_input_keyboard::KeyModifiers) {
        (
            rumoca_input_keyboard::KeyCode::Char(c),
            rumoca_input_keyboard::KeyModifiers::NONE,
        )
    }
    fn arrow(
        code: rumoca_input_keyboard::KeyCode,
    ) -> (rumoca_input_keyboard::KeyCode, rumoca_input_keyboard::KeyModifiers) {
        (code, rumoca_input_keyboard::KeyModifiers::NONE)
    }

    #[test]
    fn keyboard_set_action_writes_target() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // 'w' -> set pitch_cmd = -0.6
        eng.process_keyboard(&[key('w')], 0.01);
        assert_eq!(eng.get("pitch_cmd"), Some(-0.6));
        // 's' -> set pitch_cmd = 0.6 (overwrites)
        eng.process_keyboard(&[key('s')], 0.01);
        assert_eq!(eng.get("pitch_cmd"), Some(0.6));
    }

    #[test]
    fn keyboard_decay_reduces_targets_each_frame() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // Set pitch_cmd directly and verify it decays over time.
        eng.write_local(&Path::parse("pitch_cmd"), 1.0);
        // Decay: factor=0.85, ref_dt=0.016 -> per-frame factor at dt=0.016 is 0.85.
        eng.process_keyboard(&[], 0.016);
        let v1 = eng.get("pitch_cmd").unwrap();
        assert!((v1 - 0.85).abs() < 1e-9, "expected 0.85, got {v1}");
        eng.process_keyboard(&[], 0.016);
        let v2 = eng.get("pitch_cmd").unwrap();
        assert!((v2 - 0.85 * 0.85).abs() < 1e-9, "expected 0.7225, got {v2}");
    }

    #[test]
    fn keyboard_toggle_respects_precondition_and_debounce() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        eng.apply_derive(); // rc.2 = 1000 at zero throttle, precondition "rc.2 <= 1050" holds
        let space = key(' ');

        // Press Space -> toggle armed true
        eng.process_keyboard(&[space], 0.01);
        assert_eq!(eng.get_bool("armed"), Some(true));
        // Press again within 500ms debounce -> blocked
        eng.process_keyboard(&[space], 0.01);
        assert_eq!(eng.get_bool("armed"), Some(true));

        // Raise throttle so rc.2 > 1050 (precondition fails)
        eng.write_local(&Path::parse("throttle"), 0.5);
        eng.apply_derive();
        assert_eq!(eng.get("rc.2"), Some(1500.0));
        // Clear debounce so we'd otherwise be allowed.
        eng.debounce.clear();
        eng.process_keyboard(&[space], 0.01);
        assert_eq!(
            eng.get_bool("armed"),
            Some(true),
            "precondition should block disarm when throttle high"
        );
    }

    #[test]
    fn keyboard_signal_action_emits() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // 'r' -> signal "reset"
        eng.process_keyboard(&[key('r')], 0.01);
        assert!(eng.take_signal("reset"));
        // 'q' -> signal "quit"
        eng.process_keyboard(&[key('q')], 0.01);
        assert!(eng.take_signal("quit"));
    }

    #[test]
    fn keyboard_integrator_accumulates_from_local_source() {
        let cfg = load_quadrotor();
        let mut eng = build_for_test(&cfg);
        // Arrow Up sets throttle_input = 1.0 (with decay).
        // Keyboard integrator reads local:throttle_input, rate 0.7, clamp [0, 1] -> throttle.
        eng.process_keyboard(&[arrow(rumoca_input_keyboard::KeyCode::Up)], 0.5);
        // After this poll:
        //   decay runs first (throttle_input was 0, still 0)
        //   event fires: throttle_input = 1.0
        //   integrator reads throttle_input = 1.0, adds 1.0 * 0.7 * 0.5 = 0.35 to throttle
        assert!(
            (eng.get("throttle").unwrap() - 0.35).abs() < 1e-9,
            "expected 0.35, got {:?}",
            eng.get("throttle")
        );
    }
}
