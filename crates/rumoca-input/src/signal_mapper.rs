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
//!   - `runtime:input_message` (string)     → `RuntimeContext`

use std::collections::{BTreeSet, HashMap};

use anyhow::{Result, anyhow, bail};
use rumoca_signal_frame::SignalFrame;
use serde_json::{Map as JsonMap, Value as JsonValue};

use crate::config::{LocalDef, SignalSpec, SignalsConfig};
use crate::engine::{InputEngine, InputMode, Path};

// ── Public types ───────────────────────────────────────────────────────────

/// Runtime-only values that the simulator advertises to the signal mapper
/// (and that users can reference as `runtime:...`).
pub struct RuntimeContext<'a> {
    pub frame_num: u64,
    pub wall_ms: f64,
    pub input_connected: bool,
    pub input_mode: InputMode,
    pub input_message: Option<&'a str>,
    pub stepper_time: f64,
    /// Read a named stepper variable. Return `Ok(None)` if unknown.
    pub stepper_get: &'a dyn Fn(&str) -> Result<Option<f64>>,
}

#[derive(Debug)]
pub struct SignalMapper {
    /// Order-preserving (matches config order for deterministic JSON output).
    send: Vec<(String, CompiledSpec)>,
    viewer: Vec<(String, CompiledSpec)>,
    stepper_inputs: Vec<(String, CompiledSpec)>,
    stepper_lookup_names: Vec<String>,
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
    RuntimeInputMessage,
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
        let stepper_inputs =
            compile_section(&cfg.stepper_inputs, locals, "signals.stepper_inputs")?;
        let stepper_lookup_names = collect_stepper_lookup_names([&send, &viewer, &stepper_inputs]);
        Ok(Self {
            send,
            viewer,
            stepper_inputs,
            stepper_lookup_names,
        })
    }

    /// Build the outgoing `SignalFrame` (f64-valued) for the codec.
    pub fn build_send(&self, engine: &InputEngine, rt: &RuntimeContext<'_>) -> Result<SignalFrame> {
        let mut frame = SignalFrame::with_capacity(self.send.len());
        for (key, spec) in &self.send {
            let v = eval(key, spec, engine, rt)?;
            frame.insert(key.clone(), json_to_f64(key, &v)?);
        }
        Ok(frame)
    }

    /// Build the browser-viewer JSON object.
    pub fn build_viewer_json(
        &self,
        engine: &InputEngine,
        rt: &RuntimeContext<'_>,
    ) -> Result<String> {
        let mut obj = JsonMap::with_capacity(self.viewer.len());
        for (key, spec) in &self.viewer {
            obj.insert(key.clone(), eval(key, spec, engine, rt)?);
        }
        Ok(JsonValue::Object(obj).to_string())
    }

    /// Resolve `[signals.stepper_inputs]` into `(stepper_input_name, value)`
    /// pairs. The sim loop calls `stepper.set_input` with each pair. Used in
    /// standalone mode (no external interface) to drive model inputs from locals.
    pub fn build_stepper_inputs(
        &self,
        engine: &InputEngine,
        rt: &RuntimeContext<'_>,
    ) -> Result<Vec<(String, f64)>> {
        self.stepper_inputs
            .iter()
            .map(|(key, spec)| {
                let value = eval(key, spec, engine, rt)?;
                Ok((key.clone(), json_to_f64(key, &value)?))
            })
            .collect()
    }

    pub fn stepper_lookup_names(&self) -> &[String] {
        &self.stepper_lookup_names
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
            "input_message" => ValueSource::RuntimeInputMessage,
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

fn collect_stepper_lookup_names<'a>(
    sections: impl IntoIterator<Item = &'a Vec<(String, CompiledSpec)>>,
) -> Vec<String> {
    let mut names = BTreeSet::new();
    for section in sections {
        for (_, spec) in section {
            collect_spec_stepper_names(spec, &mut names);
        }
    }
    names.into_iter().collect()
}

fn collect_spec_stepper_names(spec: &CompiledSpec, names: &mut BTreeSet<String>) {
    match spec {
        CompiledSpec::Direct(source) => collect_source_stepper_name(source, names),
        CompiledSpec::WithDefault { source, .. } => collect_source_stepper_name(source, names),
        CompiledSpec::Conditional { source, .. } => collect_source_stepper_name(source, names),
        CompiledSpec::Const(_) => {}
    }
}

fn collect_source_stepper_name(source: &ValueSource, names: &mut BTreeSet<String>) {
    if let ValueSource::StepperVar(name) = source {
        names.insert(name.clone());
    }
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

fn eval(
    key: &str,
    spec: &CompiledSpec,
    engine: &InputEngine,
    rt: &RuntimeContext<'_>,
) -> Result<JsonValue> {
    match spec {
        CompiledSpec::Const(v) => Ok(v.clone()),
        CompiledSpec::Direct(source) => resolve_source(source, engine, rt)?
            .ok_or_else(|| anyhow!("signal '{key}' source did not resolve")),
        CompiledSpec::WithDefault { source, default } => {
            Ok(resolve_source(source, engine, rt)?.unwrap_or_else(|| default.clone()))
        }
        CompiledSpec::Conditional {
            source,
            when_true,
            when_false,
        } => {
            let b = match source {
                ValueSource::LocalBool(p) => engine
                    .get_bool(&fmt_path(p))
                    .ok_or_else(|| anyhow!("conditional signal '{key}' source did not resolve"))?,
                ValueSource::RuntimeInputConnected => rt.input_connected,
                _ => false,
            };
            if b {
                Ok(when_true.clone())
            } else {
                Ok(when_false.clone())
            }
        }
    }
}

fn resolve_source(
    source: &ValueSource,
    engine: &InputEngine,
    rt: &RuntimeContext<'_>,
) -> Result<Option<JsonValue>> {
    match source {
        ValueSource::StepperTime => Ok(Some(JsonValue::from(rt.stepper_time))),
        ValueSource::StepperVar(name) => Ok((rt.stepper_get)(name)?.map(JsonValue::from)),
        ValueSource::LocalFloat(p) => Ok(engine.get(&fmt_path(p)).map(JsonValue::from)),
        ValueSource::LocalInt(p) => Ok(engine.get(&fmt_path(p)).map(|f| JsonValue::from(f as i64))),
        ValueSource::LocalBool(p) => Ok(engine.get_bool(&fmt_path(p)).map(JsonValue::from)),
        ValueSource::RuntimeFrameNum => Ok(Some(JsonValue::from(rt.frame_num))),
        ValueSource::RuntimeWallMs => Ok(Some(JsonValue::from(rt.wall_ms))),
        ValueSource::RuntimeInputConnected => Ok(Some(JsonValue::from(rt.input_connected))),
        ValueSource::RuntimeInputMode => Ok(Some(JsonValue::from(match rt.input_mode {
            InputMode::Gamepad => "gamepad",
            InputMode::Keyboard => "keyboard",
        }))),
        ValueSource::RuntimeInputMessage => Ok(rt.input_message.map(JsonValue::from)),
    }
}

fn fmt_path(p: &Path) -> String {
    match p.index {
        Some(i) => format!("{}.{}", p.name, i),
        None => p.name.clone(),
    }
}

fn json_to_f64(key: &str, v: &JsonValue) -> Result<f64> {
    match v {
        JsonValue::Number(n) => n
            .as_f64()
            .ok_or_else(|| anyhow!("signal '{key}' number is not representable as f64")),
        JsonValue::Bool(true) => Ok(1.0),
        JsonValue::Bool(false) => Ok(0.0),
        _ => Err(anyhow!("signal '{key}' value is not numeric or boolean")),
    }
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{DeriveSpec, InputConfig, LocalDef};
    use crate::engine::InputEngine;
    use serde::Deserialize;
    use std::collections::HashMap;

    /// Minimal subset of the full TOML config needed for signal-mapper tests.
    #[derive(Deserialize)]
    struct Bundle {
        #[serde(default)]
        locals: HashMap<String, LocalDef>,
        #[serde(default)]
        derive: HashMap<String, DeriveSpec>,
        input: Option<InputConfig>,
        signals: Option<SignalsConfig>,
    }

    fn load_cfg() -> Bundle {
        toml::from_str(TEST_SIGNAL_CONFIG).expect("parse")
    }

    const TEST_SIGNAL_CONFIG: &str = r#"
[input]
mode = "auto"

[locals.armed]
default = false
type = "bool"

[locals.rc]
default = 1500
element = "int"
len = 16
type = "array"

[derive."rc.2"]
clamp = [1000.0, 2000.0]
from = "throttle"
offset = 1000.0
scale = 1000.0

[locals.throttle]
default = 0.0
type = "float"

[signals.send]
accel_z = "stepper:accel[3]"
gyro_x = "stepper:gyro[1]"
imu_valid = { const = 1.0 }
rc_2 = "local:rc.2"
rc_link_quality = { from = "runtime:input_connected", when_false = 0.0, when_true = 255.0 }
rc_valid = { from = "runtime:input_connected", when_false = 0.0, when_true = 1.0 }

[signals.viewer]
armed = "local:armed"
frame = "runtime:frame_num"
input_mode = "runtime:input_mode"
q0 = { from = "stepper:quat[1]", default = 1.0 }
rc_throttle = "local:rc.2"
t = "stepper:time"
"#;

    fn build_engine(cfg: &Bundle) -> InputEngine {
        let defaults = crate::engine::initialize_locals(&cfg.locals).expect("init locals");
        let compiled =
            crate::engine::compile::compile(cfg.input.as_ref().unwrap(), &cfg.derive, &cfg.locals)
                .unwrap();
        InputEngine::from_parts_for_test(defaults, compiled)
    }

    fn stepper_get_fn(map: &HashMap<String, f64>) -> impl Fn(&str) -> Result<Option<f64>> + '_ {
        move |name: &str| Ok(map.get(name).copied())
    }

    fn make_rt<'a>(
        time: f64,
        stepper_get: &'a dyn Fn(&str) -> Result<Option<f64>>,
    ) -> RuntimeContext<'a> {
        RuntimeContext {
            frame_num: 42,
            wall_ms: 123_456.0,
            input_connected: true,
            input_mode: InputMode::Gamepad,
            input_message: None,
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
            ("gyro[1]", 0.1),
            ("gyro[2]", 0.2),
            ("gyro[3]", 0.3),
            ("accel[1]", 1.0),
            ("accel[2]", 2.0),
            ("accel[3]", -9.81),
        ]
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
        let get = stepper_get_fn(&stepper_vars);
        let rt = make_rt(0.5, &get);

        let mapper =
            SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).expect("compile");
        let frame = mapper.build_send(&engine, &rt).expect("send frame");

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
        let stepper_vars: HashMap<String, f64> = [
            ("gyro[1]", 0.1),
            ("gyro[2]", 0.2),
            ("gyro[3]", 0.3),
            ("accel[1]", 1.0),
            ("accel[2]", 2.0),
            ("accel[3]", -9.81),
        ]
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
        let get = stepper_get_fn(&stepper_vars);
        let mut rt = make_rt(0.0, &get);
        rt.input_connected = false;

        let mapper = SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).unwrap();
        let frame = mapper.build_send(&engine, &rt).expect("send frame");
        assert_eq!(frame.get("rc_valid"), Some(&0.0));
        assert_eq!(frame.get("rc_link_quality"), Some(&0.0));
    }

    #[test]
    fn viewer_json_contains_expected_keys_and_types() {
        let cfg = load_cfg();
        let mut engine = build_engine(&cfg);
        engine.apply_derive_for_test();

        let mut stepper_vars: HashMap<String, f64> = HashMap::new();
        stepper_vars.insert("quat[1]".into(), 0.987);
        stepper_vars.insert("position[1]".into(), 1.5);
        let get = stepper_get_fn(&stepper_vars);
        let rt = make_rt(1.25, &get);

        let mapper = SignalMapper::new(cfg.signals.as_ref().unwrap(), &cfg.locals).unwrap();
        let text = mapper.build_viewer_json(&engine, &rt).expect("viewer json");
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
        let text = mapper.build_viewer_json(&engine, &rt).expect("viewer json");
        let v: JsonValue = serde_json::from_str(&text).unwrap();
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
            stepper_inputs: HashMap::new(),
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
            stepper_inputs: HashMap::new(),
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
            stepper_inputs: HashMap::new(),
        };
        let err = SignalMapper::new(&sig, &HashMap::new()).unwrap_err();
        assert!(err.to_string().contains("prefix"), "got: {err}");
    }
}
