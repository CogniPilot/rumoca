//! Config-driven signal assembly.
//!
//! Builds two artifacts each frame from `[signals.send]` and
//! `[signals.viewer]`:
//!   - a `SignalFrame` for the outgoing FB codec (all values are f64)
//!   - a JSON object for the browser viewer (preserves int/bool/float)
//!
//! Sources resolvable in signal specs:
//!   - `stepper:time`                       → `stepper_time` argument
//!   - `stepper:<name>`                     → `stepper_get(name)`
//!   - `local:<name>` / `local:<name>.<idx>`→ input engine locals
//!   - `runtime:frame_num` / `wall_ms`      → `RuntimeContext`
//!   - `runtime:input_connected` (bool)     → `RuntimeContext`
//!   - `runtime:input_mode` (string)        → `RuntimeContext`

use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use rumoca_codec::SignalFrame;
use serde_json::{Map as JsonMap, Value as JsonValue};

use crate::config::{LocalDef, SignalSpec, SignalsConfig};
use crate::input_engine::{InputEngine, InputMode, Path};

// ── Public types ───────────────────────────────────────────────────────────

/// Runtime-only values that the simulator advertises to the signal mapper
/// (and that users can reference as `runtime:...`).
pub struct RuntimeContext<'a> {
    pub frame_num: u64,
    pub wall_ms: f64,
    pub input_connected: bool,
    pub input_mode: InputMode,
    pub stepper_time: f64,
    /// Read a named stepper variable. Return `None` if unknown.
    pub stepper_get: &'a dyn Fn(&str) -> Option<f64>,
}

#[derive(Debug)]
pub struct SignalMapper {
    /// Order-preserving (matches config order for deterministic JSON output).
    send: Vec<(String, CompiledSpec)>,
    viewer: Vec<(String, CompiledSpec)>,
}

// ── Compiled source representation ─────────────────────────────────────────

#[derive(Debug, Clone)]
enum ValueSource {
    StepperTime,
    StepperVar(String),
    LocalFloat(Path),
    LocalInt(Path),
    LocalBool(Path),
    RuntimeFrameNum,
    RuntimeWallMs,
    RuntimeInputConnected,
    RuntimeInputMode,
}

#[derive(Debug, Clone)]
enum CompiledSpec {
    Direct(ValueSource),
    WithDefault {
        source: ValueSource,
        default: JsonValue,
    },
    Conditional {
        source: ValueSource, // must resolve to a bool
        when_true: JsonValue,
        when_false: JsonValue,
    },
    Const(JsonValue),
}

// ── Compile ────────────────────────────────────────────────────────────────

impl SignalMapper {
    pub fn new(cfg: &SignalsConfig, locals: &HashMap<String, LocalDef>) -> Result<Self> {
        let send = compile_section(&cfg.send, locals, "signals.send")?;
        let viewer = compile_section(&cfg.viewer, locals, "signals.viewer")?;
        Ok(Self { send, viewer })
    }

    /// Build the outgoing `SignalFrame` (f64-valued) for the codec.
    pub fn build_send(
        &self,
        engine: &InputEngine,
        rt: &RuntimeContext<'_>,
    ) -> SignalFrame {
        let mut frame = SignalFrame::with_capacity(self.send.len());
        for (key, spec) in &self.send {
            let v = eval(spec, engine, rt);
            frame.insert(key.clone(), json_to_f64(&v));
        }
        frame
    }

    /// Build the browser-viewer JSON object.
    pub fn build_viewer_json(&self, engine: &InputEngine, rt: &RuntimeContext<'_>) -> String {
        let mut obj = JsonMap::with_capacity(self.viewer.len());
        for (key, spec) in &self.viewer {
            obj.insert(key.clone(), eval(spec, engine, rt));
        }
        JsonValue::Object(obj).to_string()
    }
}

fn compile_section(
    section: &HashMap<String, SignalSpec>,
    locals: &HashMap<String, LocalDef>,
    ctx: &str,
) -> Result<Vec<(String, CompiledSpec)>> {
    let mut out: Vec<(String, CompiledSpec)> = section
        .iter()
        .map(|(k, v)| compile_entry(k, v, locals, ctx).map(|c| (k.clone(), c)))
        .collect::<Result<_>>()?;
    // HashMap iteration order isn't deterministic; sort for stability.
    out.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(out)
}

fn compile_entry(
    key: &str,
    spec: &SignalSpec,
    locals: &HashMap<String, LocalDef>,
    ctx: &str,
) -> Result<CompiledSpec> {
    match spec {
        SignalSpec::Ref(r) => Ok(CompiledSpec::Direct(parse_source(r, locals, ctx, key)?)),
        SignalSpec::WithDefault { from, default } => Ok(CompiledSpec::WithDefault {
            source: parse_source(from, locals, ctx, key)?,
            default: JsonValue::from(*default),
        }),
        SignalSpec::Conditional {
            from,
            when_true,
            when_false,
        } => {
            let source = parse_source(from, locals, ctx, key)?;
            if !source_is_bool_compatible(&source) {
                bail!(
                    "{}.{}: conditional source '{}' must be a bool-compatible local or runtime flag",
                    ctx,
                    key,
                    from
                );
            }
            Ok(CompiledSpec::Conditional {
                source,
                when_true: toml_to_json(when_true)
                    .ok_or_else(|| anyhow!("{}.{}: when_true must be number/bool", ctx, key))?,
                when_false: toml_to_json(when_false)
                    .ok_or_else(|| anyhow!("{}.{}: when_false must be number/bool", ctx, key))?,
            })
        }
        SignalSpec::Const { value } => {
            let jv = toml_to_json(value)
                .ok_or_else(|| anyhow!("{}.{}: const must be number/bool", ctx, key))?;
            Ok(CompiledSpec::Const(jv))
        }
    }
}

fn parse_source(
    s: &str,
    locals: &HashMap<String, LocalDef>,
    ctx: &str,
    key: &str,
) -> Result<ValueSource> {
    if let Some(rest) = s.strip_prefix("stepper:") {
        if rest == "time" {
            return Ok(ValueSource::StepperTime);
        }
        return Ok(ValueSource::StepperVar(rest.to_string()));
    }
    if let Some(rest) = s.strip_prefix("local:") {
        let path = Path::parse(rest);
        let def = locals.get(&path.name).ok_or_else(|| {
            anyhow!(
                "{}.{}: local '{}' is not declared in [locals]",
                ctx,
                key,
                path.name
            )
        })?;
        let source = match (def.kind.as_str(), path.index.is_some()) {
            ("bool", false) => ValueSource::LocalBool(path),
            ("float", false) => ValueSource::LocalFloat(path),
            ("array", true) => match def.element.as_deref() {
                Some("int") => ValueSource::LocalInt(path),
                Some("float") => ValueSource::LocalFloat(path),
                other => bail!(
                    "{}.{}: local '{}' has unknown array element '{:?}'",
                    ctx,
                    key,
                    path.name,
                    other
                ),
            },
            (_, true) => bail!(
                "{}.{}: local '{}' is {} but was indexed",
                ctx,
                key,
                path.name,
                def.kind
            ),
            (_, false) => bail!(
                "{}.{}: local '{}' is an array — reference with an index (e.g. '{}.0')",
                ctx,
                key,
                path.name,
                path.name
            ),
        };
        return Ok(source);
    }
    if let Some(rest) = s.strip_prefix("runtime:") {
        return Ok(match rest {
            "frame_num" => ValueSource::RuntimeFrameNum,
            "wall_ms" => ValueSource::RuntimeWallMs,
            "input_connected" => ValueSource::RuntimeInputConnected,
            "input_mode" => ValueSource::RuntimeInputMode,
            other => bail!("{}.{}: unknown runtime field '{}'", ctx, key, other),
        });
    }
    bail!(
        "{}.{}: reference '{}' must use a `stepper:`, `local:`, or `runtime:` prefix",
        ctx,
        key,
        s
    )
}

fn source_is_bool_compatible(source: &ValueSource) -> bool {
    matches!(
        source,
        ValueSource::LocalBool(_) | ValueSource::RuntimeInputConnected
    )
}

fn toml_to_json(v: &toml::Value) -> Option<JsonValue> {
    match v {
        toml::Value::Integer(i) => Some(JsonValue::from(*i)),
        toml::Value::Float(f) => Some(JsonValue::from(*f)),
        toml::Value::Boolean(b) => Some(JsonValue::from(*b)),
        toml::Value::String(s) => Some(JsonValue::from(s.clone())),
        _ => None,
    }
}

// ── Evaluation ─────────────────────────────────────────────────────────────

fn eval(spec: &CompiledSpec, engine: &InputEngine, rt: &RuntimeContext<'_>) -> JsonValue {
    match spec {
        CompiledSpec::Const(v) => v.clone(),
        // Direct: fall back to 0.0 for missing sources (matches the
        // behavior of the pre-generification quadrotor adapter).
        CompiledSpec::Direct(source) => {
            resolve_source(source, engine, rt).unwrap_or(JsonValue::from(0.0))
        }
        CompiledSpec::WithDefault { source, default } => {
            resolve_source(source, engine, rt).unwrap_or_else(|| default.clone())
        }
        CompiledSpec::Conditional {
            source,
            when_true,
            when_false,
        } => {
            let b = match source {
                ValueSource::LocalBool(p) => engine.get_bool(&fmt_path(p)).unwrap_or(false),
                ValueSource::RuntimeInputConnected => rt.input_connected,
                _ => false,
            };
            if b { when_true.clone() } else { when_false.clone() }
        }
    }
}

fn resolve_source(
    source: &ValueSource,
    engine: &InputEngine,
    rt: &RuntimeContext<'_>,
) -> Option<JsonValue> {
    match source {
        ValueSource::StepperTime => Some(JsonValue::from(rt.stepper_time)),
        // Report None when the stepper doesn't know the variable, so
        // `WithDefault` can substitute — `Direct` handles None at the top
        // layer by falling back to 0.0 for parity with the old adapter.
        ValueSource::StepperVar(name) => (rt.stepper_get)(name).map(JsonValue::from),
        ValueSource::LocalFloat(p) => engine.get(&fmt_path(p)).map(JsonValue::from),
        ValueSource::LocalInt(p) => engine.get(&fmt_path(p)).map(|f| JsonValue::from(f as i64)),
        ValueSource::LocalBool(p) => engine.get_bool(&fmt_path(p)).map(JsonValue::from),
        ValueSource::RuntimeFrameNum => Some(JsonValue::from(rt.frame_num)),
        ValueSource::RuntimeWallMs => Some(JsonValue::from(rt.wall_ms)),
        ValueSource::RuntimeInputConnected => Some(JsonValue::from(rt.input_connected)),
        ValueSource::RuntimeInputMode => Some(JsonValue::from(match rt.input_mode {
            InputMode::Gamepad => "gamepad",
            InputMode::Keyboard => "keyboard",
        })),
    }
}

fn fmt_path(p: &Path) -> String {
    match p.index {
        Some(i) => format!("{}.{}", p.name, i),
        None => p.name.clone(),
    }
}

fn json_to_f64(v: &JsonValue) -> f64 {
    match v {
        JsonValue::Number(n) => n.as_f64().unwrap_or(0.0),
        JsonValue::Bool(b) => {
            if *b {
                1.0
            } else {
                0.0
            }
        }
        _ => 0.0,
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::SimFbConfig;
    use crate::input_engine::InputEngine;
    use std::collections::HashMap;
    use std::path::PathBuf;

    fn load_cfg() -> SimFbConfig {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let root = manifest.parent().unwrap().parent().unwrap();
        SimFbConfig::load(
            &root
                .join("examples")
                .join("quadrotor_sil")
                .join("quadrotor.toml"),
        )
        .expect("parse")
    }

    fn build_engine(cfg: &SimFbConfig) -> InputEngine {
        let defaults =
            crate::input_engine::initialize_locals(&cfg.locals).expect("init locals");
        let compiled = crate::input_engine::compile::compile(
            cfg.input.as_ref().unwrap(),
            &cfg.derive,
            &cfg.locals,
        )
        .unwrap();
        InputEngine::from_parts_for_test(defaults, compiled)
    }

    fn stepper_get_fn(
        map: &HashMap<String, f64>,
    ) -> impl Fn(&str) -> Option<f64> + '_ {
        move |name: &str| map.get(name).copied()
    }

    fn make_rt<'a>(
        time: f64,
        stepper_get: &'a dyn Fn(&str) -> Option<f64>,
    ) -> RuntimeContext<'a> {
        RuntimeContext {
            frame_num: 42,
            wall_ms: 123_456.0,
            input_connected: true,
            input_mode: InputMode::Gamepad,
            stepper_time: time,
            stepper_get,
        }
    }

    #[test]
    fn send_frame_uses_stepper_and_local_refs() {
        let cfg = load_cfg();
        let mut engine = build_engine(&cfg);
        engine.apply_derive_for_test(); // initialize rc.0..rc.4

        let stepper_vars: HashMap<String, f64> = [
            ("gyro_x", 0.1),
            ("gyro_y", 0.2),
            ("gyro_z", 0.3),
            ("accel_x", 1.0),
            ("accel_y", 2.0),
            ("accel_z", -9.81),
        ]
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
        let get = stepper_get_fn(&stepper_vars);
        let rt = make_rt(0.5, &get);

        let mapper = SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).expect("compile");
        let frame = mapper.build_send(&engine, &rt);

        assert_eq!(frame.get("gyro_x"), Some(&0.1));
        assert_eq!(frame.get("accel_z"), Some(&-9.81));
        // rc_2 local:rc.2 = 1000 (throttle=0)
        assert_eq!(frame.get("rc_2"), Some(&1000.0));
        // const: imu_valid = 1.0
        assert_eq!(frame.get("imu_valid"), Some(&1.0));
        // conditional: rc_valid when runtime:input_connected=true -> 1
        assert_eq!(frame.get("rc_valid"), Some(&1.0));
        assert_eq!(frame.get("rc_link_quality"), Some(&255.0));
    }

    #[test]
    fn send_conditional_honors_false_branch() {
        let cfg = load_cfg();
        let mut engine = build_engine(&cfg);
        engine.apply_derive_for_test();
        let stepper_vars: HashMap<String, f64> = HashMap::new();
        let get = stepper_get_fn(&stepper_vars);
        let mut rt = make_rt(0.0, &get);
        rt.input_connected = false;

        let mapper = SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).unwrap();
        let frame = mapper.build_send(&engine, &rt);
        assert_eq!(frame.get("rc_valid"), Some(&0.0));
        assert_eq!(frame.get("rc_link_quality"), Some(&0.0));
    }

    #[test]
    fn viewer_json_contains_expected_keys_and_types() {
        let cfg = load_cfg();
        let mut engine = build_engine(&cfg);
        engine.apply_derive_for_test();

        let mut stepper_vars: HashMap<String, f64> = HashMap::new();
        stepper_vars.insert("q0".into(), 0.987);
        stepper_vars.insert("px".into(), 1.5);
        let get = stepper_get_fn(&stepper_vars);
        let rt = make_rt(1.25, &get);

        let mapper = SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).unwrap();
        let text = mapper.build_viewer_json(&engine, &rt);
        let v: JsonValue = serde_json::from_str(&text).expect("valid json");
        assert_eq!(v["t"], 1.25);
        assert_eq!(v["frame"], 42);
        assert_eq!(v["q0"], 0.987);
        assert_eq!(v["armed"], false); // local:armed
        // rc_throttle = local:rc.2 = 1000 (int array element)
        assert_eq!(v["rc_throttle"], 1000);
        assert_eq!(v["input_mode"], "gamepad");
    }

    #[test]
    fn viewer_with_default_uses_default_when_source_missing() {
        // q0 has { from = "stepper:q0", default = 1.0 }. With no q0 in the
        // stepper map, the default should kick in.
        let cfg = load_cfg();
        let mut engine = build_engine(&cfg);
        engine.apply_derive_for_test();

        let stepper_vars: HashMap<String, f64> = HashMap::new();
        let get = stepper_get_fn(&stepper_vars);
        let rt = make_rt(0.0, &get);

        let mapper = SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).unwrap();
        let text = mapper.build_viewer_json(&engine, &rt);
        let v: JsonValue = serde_json::from_str(&text).unwrap();
        // stepper_get returns None for "q0", default is 1.0 — but WithDefault
        // is only invoked when the source itself returns None. Our StepperVar
        // always returns Some(0.0) when missing, so the default path needs
        // stepper_get to return None, which it does here (not in the map).
        // Verify: q0 comes back as 1.0 (default).
        assert_eq!(v["q0"], 1.0);
    }

    #[test]
    fn rejects_undeclared_local_ref() {
        use crate::config::{SignalSpec, SignalsConfig};
        let mut send = HashMap::new();
        send.insert("x".into(), SignalSpec::Ref("local:not_declared".into()));
        let sig = SignalsConfig {
            send,
            viewer: HashMap::new(),
        };
        let err = SignalMapper::new(&sig, &HashMap::new()).unwrap_err();
        assert!(err.to_string().contains("not declared"), "got: {err}");
    }

    #[test]
    fn rejects_unknown_runtime_field() {
        use crate::config::{SignalSpec, SignalsConfig};
        let mut viewer = HashMap::new();
        viewer.insert("x".into(), SignalSpec::Ref("runtime:not_a_field".into()));
        let sig = SignalsConfig {
            send: HashMap::new(),
            viewer,
        };
        let err = SignalMapper::new(&sig, &HashMap::new()).unwrap_err();
        assert!(err.to_string().contains("unknown runtime"), "got: {err}");
    }

    #[test]
    fn rejects_missing_prefix() {
        use crate::config::{SignalSpec, SignalsConfig};
        let mut send = HashMap::new();
        send.insert("x".into(), SignalSpec::Ref("no_prefix_here".into()));
        let sig = SignalsConfig {
            send,
            viewer: HashMap::new(),
        };
        let err = SignalMapper::new(&sig, &HashMap::new()).unwrap_err();
        assert!(err.to_string().contains("prefix"), "got: {err}");
    }
}
