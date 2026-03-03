use super::builtin_table::{
    eval_external_table_function, resolve_function_closure, resolve_user_function,
};
use super::distribution_clock::{eval_clock_special_function, eval_distribution_function};
use super::*;
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, OnceLock};

fn eval_boolean_vector_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let values = || {
        args.first()
            .map(|e| eval_array_like_f64_values(e, env))
            .unwrap_or_default()
    };
    match short_name {
        "anyTrue" => Some(T::from_bool(values().iter().any(|v| *v != 0.0))),
        "andTrue" => {
            let vals = values();
            Some(T::from_bool(
                !vals.is_empty() && vals.iter().all(|v| *v != 0.0),
            ))
        }
        "firstTrueIndex" => {
            let idx = values()
                .iter()
                .position(|v| *v != 0.0)
                .map(|i| i + 1)
                .unwrap_or(0);
            Some(T::from_f64(idx as f64))
        }
        _ => None,
    }
}

fn eval_unit_conversion_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        "to_degC" => Some(eval_expr::<T>(&args[0], env) + T::from_f64(-273.15)),
        "from_degC" => Some(eval_expr::<T>(&args[0], env) + T::from_f64(273.15)),
        "to_deg" => Some(eval_expr::<T>(&args[0], env) * T::from_f64(180.0 / std::f64::consts::PI)),
        "from_deg" => {
            Some(eval_expr::<T>(&args[0], env) * T::from_f64(std::f64::consts::PI / 180.0))
        }
        _ => None,
    }
}

fn eval_stream_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        // MLS stream operators; runtime currently uses direct argument passthrough.
        // This avoids NaN cascades until full connector-flow-dependent semantics
        // are implemented in the simulator.
        "actualStream" | "inStream" => Some(
            args.first()
                .map(|arg| eval_expr::<T>(arg, env))
                .unwrap_or_else(T::zero),
        ),
        _ => None,
    }
}

fn eval_state_accessor_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let field = match short_name {
        "temperature" => "T",
        "pressure" => "p",
        "density" => "d",
        "specificEnthalpy" => "h",
        "specificInternalEnergy" => "u",
        "specificEntropy" => "s",
        _ => return None,
    };
    eval_state_accessor_from_expr(args.first()?, field, env)
}

fn eval_state_accessor_from_expr<T: SimFloat>(
    expr: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    match expr {
        Expression::VarRef { name, subscripts } => {
            eval_state_accessor_from_var_ref(name, subscripts, field, env)
        }
        Expression::FieldAccess {
            base,
            field: member,
        } => {
            let base_path = eval_state_accessor_field_path(base, env)?;
            let state_name = VarName::new(format!("{base_path}.{member}"));
            eval_state_accessor_from_var_ref(&state_name, &[], field, env)
        }
        Expression::FunctionCall { name, args, .. } => {
            eval_state_accessor_from_set_state(name.as_str(), args, field, env)
        }
        _ => None,
    }
}

fn eval_state_accessor_field_path<T: SimFloat>(
    expr: &Expression,
    env: &VarEnv<T>,
) -> Option<String> {
    match expr {
        Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                return Some(name.to_string());
            }
            let mut idx_parts = Vec::with_capacity(subscripts.len());
            for sub in subscripts {
                let idx = match sub {
                    Subscript::Index(i) => *i,
                    Subscript::Expr(e) => eval_expr::<T>(e, env).real().round() as i64,
                    Subscript::Colon => return None,
                };
                idx_parts.push(idx.to_string());
            }
            Some(format!("{}[{}]", name.as_str(), idx_parts.join(",")))
        }
        Expression::FieldAccess { base, field } => {
            let prefix = eval_state_accessor_field_path(base, env)?;
            Some(format!("{prefix}.{field}"))
        }
        _ => None,
    }
}

fn eval_state_accessor_from_var_ref<T: SimFloat>(
    name: &VarName,
    subscripts: &[Subscript],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let mut candidates = Vec::new();
    if subscripts.is_empty() {
        let base = name.as_str();
        candidates.push(base.to_string());
        // Scalar evaluator fallback for arrays of state records.
        candidates.push(format!("{base}[1]"));
    } else {
        let mut idx_parts = Vec::with_capacity(subscripts.len());
        for sub in subscripts {
            let idx = match sub {
                Subscript::Index(i) => *i,
                Subscript::Expr(expr) => eval_expr::<T>(expr, env).real().round() as i64,
                Subscript::Colon => return None,
            };
            idx_parts.push(idx.to_string());
        }
        candidates.push(format!("{}[{}]", name.as_str(), idx_parts.join(",")));
    }

    for base in candidates {
        let key = format!("{base}.{field}");
        if let Some(value) = env.vars.get(&key).copied() {
            return Some(value);
        }
    }
    None
}

fn eval_state_accessor_from_set_state<T: SimFloat>(
    name: &str,
    args: &[Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let short_name = name.rsplit('.').next().unwrap_or(name);
    match short_name {
        // setState_pTX(p, T, X)
        "setState_pTX" | "setState_pT" => match field {
            "p" => args.first().map(|e| eval_expr::<T>(e, env)),
            "T" => args.get(1).map(|e| eval_expr::<T>(e, env)),
            _ => None,
        },
        // setState_dTX(d, T, X)
        "setState_dTX" => match field {
            "d" => args.first().map(|e| eval_expr::<T>(e, env)),
            "T" => args.get(1).map(|e| eval_expr::<T>(e, env)),
            _ => None,
        },
        // setState_phX(p, h, X)
        "setState_phX" | "setState_ph" => match field {
            "p" => args.first().map(|e| eval_expr::<T>(e, env)),
            "h" => args.get(1).map(|e| eval_expr::<T>(e, env)),
            "T" => eval_state_accessor_via_user_helper(
                name,
                args,
                env,
                &["temperature_phX", "temperature_ph"],
            ),
            _ => None,
        },
        // setState_psX(p, s, X)
        "setState_psX" | "setState_ps" => match field {
            "p" => args.first().map(|e| eval_expr::<T>(e, env)),
            "s" => args.get(1).map(|e| eval_expr::<T>(e, env)),
            "T" => eval_state_accessor_via_user_helper(
                name,
                args,
                env,
                &["temperature_psX", "temperature_ps"],
            ),
            _ => None,
        },
        // setSmoothState(x, state_a, state_b, x_small)
        "setSmoothState" => {
            let x = args.first().map(|e| eval_expr::<T>(e, env).real())?;
            let a = args
                .get(1)
                .and_then(|e| eval_state_accessor_from_expr(e, field, env))?;
            let b = args
                .get(2)
                .and_then(|e| eval_state_accessor_from_expr(e, field, env))?;
            let x_small = args
                .get(3)
                .map(|e| eval_expr::<T>(e, env).real().abs())
                .unwrap_or(0.0);
            if x_small <= f64::EPSILON {
                return Some(if x >= 0.0 { a } else { b });
            }
            if x >= x_small {
                return Some(a);
            }
            if x <= -x_small {
                return Some(b);
            }
            let alpha = (x / x_small + 1.0) * 0.5;
            Some(b + T::from_f64(alpha) * (a - b))
        }
        _ => None,
    }
}

fn eval_state_accessor_via_user_helper<T: SimFloat>(
    constructor_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
    helper_suffixes: &[&str],
) -> Option<T> {
    let (prefix, _) = constructor_name.rsplit_once('.')?;
    for suffix in helper_suffixes {
        let helper = VarName::new(format!("{prefix}.{suffix}"));
        if let Some(v) = eval_user_function_call(&helper, args, env) {
            return Some(v);
        }
    }
    None
}

#[derive(Default)]
struct ImpureRandomRegistry {
    next_id: i64,
    auto_seed_counter: u64,
    streams: HashMap<i64, u64>,
}

static IMPURE_RANDOM_REGISTRY: OnceLock<Mutex<ImpureRandomRegistry>> = OnceLock::new();

fn impure_random_registry() -> &'static Mutex<ImpureRandomRegistry> {
    IMPURE_RANDOM_REGISTRY.get_or_init(|| Mutex::new(ImpureRandomRegistry::default()))
}

fn scramble_seed(mut x: u64) -> u64 {
    // splitmix64 finalizer
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58476D1CE4E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D049BB133111EB);
    x ^ (x >> 31)
}

fn xorshift64star_next(state: &mut u64) -> u64 {
    let mut x = *state;
    if x == 0 {
        x = 0x9E3779B97F4A7C15;
    }
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    *state = x;
    x.wrapping_mul(0x2545F4914F6CDD1D)
}

fn eval_integer_arg<T: SimFloat>(args: &[Expression], idx: usize, env: &VarEnv<T>) -> i64 {
    args.get(idx)
        .map(|expr| eval_expr::<T>(expr, env).real().round() as i64)
        .unwrap_or(0)
}

fn clamp_i64_to_positive_u31(v: u64) -> i64 {
    let raw = (v & 0x7FFF_FFFF) as i64;
    if raw == 0 { 1 } else { raw }
}

fn automatic_local_seed_from_expr<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> i64 {
    if let Some(Expression::Literal(Literal::String(path))) = args.first() {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        path.hash(&mut hasher);
        return clamp_i64_to_positive_u31(hasher.finish());
    }
    // Fallback for non-literal paths in numeric-only runtime.
    let approx = args
        .first()
        .map(|expr| eval_expr::<T>(expr, env).real().to_bits())
        .unwrap_or(0);
    clamp_i64_to_positive_u31(scramble_seed(approx))
}

fn automatic_global_seed() -> i64 {
    let mut registry = impure_random_registry()
        .lock()
        .expect("impure random registry poisoned");
    registry.auto_seed_counter = registry.auto_seed_counter.wrapping_add(1);
    clamp_i64_to_positive_u31(scramble_seed(registry.auto_seed_counter))
}

fn initialize_impure_random(seed: i64) -> i64 {
    let mut registry = impure_random_registry()
        .lock()
        .expect("impure random registry poisoned");
    registry.next_id = registry.next_id.saturating_add(1);
    let id = registry.next_id.max(1);
    let mixed = scramble_seed((seed as u64) ^ (id as u64).wrapping_mul(0x9E3779B97F4A7C15));
    registry.streams.insert(id, mixed.max(1));
    id
}

fn impure_random_value(id: i64) -> f64 {
    let mut registry = impure_random_registry()
        .lock()
        .expect("impure random registry poisoned");
    let state = registry
        .streams
        .entry(id)
        .or_insert_with(|| scramble_seed((id as u64).wrapping_mul(0xD1B54A32D192ED03)).max(1));
    let sample = xorshift64star_next(state);
    // Uniform in (0, 1], matching MSL contract for impureRandom.
    let unit = (((sample >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))).max(f64::EPSILON);
    unit.min(1.0)
}

fn eval_misc_intrinsic_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        // Assert/terminate are side-effect statements. The numeric evaluator only
        // needs a stable scalar fallback when they appear in lowered call form.
        "assert" => {
            let cond = args
                .first()
                .map(|expr| eval_expr::<T>(expr, env))
                .unwrap_or_else(T::one);
            if cond.to_bool() {
                Some(T::zero())
            } else {
                Some(T::nan())
            }
        }
        "terminate" => Some(T::zero()),
        // String-valued utility intrinsics are accepted as simulation helpers.
        // The runtime evaluator is scalar-numeric, so these return conservative
        // numeric placeholders when true string evaluation is unavailable.
        "String" => Some(
            args.first()
                .map(|expr| eval_expr::<T>(expr, env))
                .unwrap_or_else(T::zero),
        ),
        "cardinality" => Some(T::zero()),
        "array" => Some(
            args.first()
                .map(|expr| eval_expr::<T>(expr, env))
                .unwrap_or_else(T::zero),
        ),
        "getInstanceName" | "loadResource" => Some(T::zero()),
        "isValidTable" => Some(T::one()),
        "isEmpty" => {
            if let Some(Expression::Literal(Literal::String(s))) = args.first() {
                Some(T::from_bool(s.trim().is_empty()))
            } else {
                Some(T::zero())
            }
        }
        "automaticGlobalSeed" => Some(T::from_f64(automatic_global_seed() as f64)),
        "automaticLocalSeed" => Some(T::from_f64(automatic_local_seed_from_expr(args, env) as f64)),
        "initializeImpureRandom" => {
            let seed = eval_integer_arg(args, 0, env);
            Some(T::from_f64(initialize_impure_random(seed) as f64))
        }
        "impureRandom" => {
            let id = eval_integer_arg(args, 0, env);
            Some(T::from_f64(impure_random_value(id)))
        }
        "impureRandomInteger" => {
            let id = eval_integer_arg(args, 0, env);
            let imin = eval_integer_arg(args, 1, env);
            let imax = eval_integer_arg(args, 2, env);
            let (lo, hi) = if imin <= imax {
                (imin, imax)
            } else {
                (imax, imin)
            };
            let span = (hi - lo + 1).max(1) as f64;
            let y = lo as f64 + (impure_random_value(id) * span).floor();
            Some(T::from_f64(y.clamp(lo as f64, hi as f64)))
        }
        "length" => {
            if let Some(Expression::Literal(Literal::String(s))) = args.first() {
                Some(T::from_f64(s.chars().count() as f64))
            } else {
                Some(T::zero())
            }
        }
        "find" | "findLast" => {
            let (
                Some(Expression::Literal(Literal::String(haystack))),
                Some(Expression::Literal(Literal::String(needle))),
            ) = (args.first(), args.get(1))
            else {
                return Some(T::zero());
            };
            let idx = if short_name == "find" {
                haystack.find(needle)
            } else {
                haystack.rfind(needle)
            };
            Some(T::from_f64(
                idx.map(|i| i.saturating_add(1) as f64).unwrap_or(0.0),
            ))
        }
        "substring" => Some(T::zero()),
        _ => None,
    }
}

fn eval_qualified_special_function<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
        | "Modelica.Math.Random.Generators.Xorshift64star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift128plus.initialState" => {
            let local_seed = eval_integer_arg(args, 0, env);
            let global_seed = eval_integer_arg(args, 1, env);
            let n_state = eval_integer_arg(args, 2, env).max(1);
            let mixed =
                scramble_seed((local_seed as u64) ^ ((global_seed as u64) << 1) ^ (n_state as u64));
            Some(T::from_f64(clamp_i64_to_positive_u31(mixed) as f64))
        }
        "Modelica.Math.Random.Generators.Xorshift64star.random"
        | "Modelica.Math.Random.Generators.Xorshift128plus.random" => {
            let state_tag = args
                .first()
                .map(|expr| eval_expr::<T>(expr, env).real().to_bits())
                .unwrap_or(1);
            let id = clamp_i64_to_positive_u31(scramble_seed(state_tag));
            Some(T::from_f64(impure_random_value(id)))
        }
        _ => None,
    }
}

/// Returns true if a short function name is handled by runtime special-function
/// evaluators instead of user-function bodies.
pub fn is_runtime_special_function_short_name(short_name: &str) -> bool {
    matches!(
        short_name,
        "ExternalCombiTimeTable"
            | "ExternalCombiTable1D"
            | "getTimeTableTmax"
            | "getTimeTableTmin"
            | "getNextTimeEvent"
            | "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getTable1DAbscissaUmax"
            | "getTable1DAbscissaUmin"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue"
            | "anyTrue"
            | "andTrue"
            | "firstTrueIndex"
            | "distribution"
            | "Clock"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
            | "hold"
            | "noClock"
            | "previous"
            | "interval"
            | "firstTick"
            | "actualStream"
            | "inStream"
            | "temperature"
            | "pressure"
            | "density"
            | "specificEnthalpy"
            | "specificInternalEnergy"
            | "specificEntropy"
            | "to_degC"
            | "from_degC"
            | "to_deg"
            | "from_deg"
            | "assert"
            | "terminate"
            | "String"
            | "cardinality"
            | "array"
            | "getInstanceName"
            | "loadResource"
            | "isValidTable"
            | "isEmpty"
            | "automaticGlobalSeed"
            | "automaticLocalSeed"
            | "initializeImpureRandom"
            | "impureRandom"
            | "impureRandomInteger"
            | "length"
            | "find"
            | "findLast"
            | "substring"
    )
}

fn is_runtime_special_function_qualified_name(name: &str) -> bool {
    matches!(
        name,
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
            | "Modelica.Math.Random.Generators.Xorshift64star.initialState"
            | "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
            | "Modelica.Math.Random.Generators.Xorshift64star.random"
            | "Modelica.Math.Random.Generators.Xorshift128plus.random"
    )
}

/// Returns true if the function name (qualified or short) is handled by
/// runtime special-function evaluators.
pub fn is_runtime_special_function_name(name: &str) -> bool {
    let short_name = name.rsplit('.').next().unwrap_or(name);
    is_runtime_special_function_short_name(short_name)
        || is_runtime_special_function_qualified_name(name)
}

fn eval_special_function_call<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    if let Some(v) = eval_qualified_special_function(name, args, env) {
        return Some(v);
    }
    let short_name = name.rsplit('.').next().unwrap_or(name);
    if let Some(v) = eval_misc_intrinsic_function(short_name, args, env) {
        return Some(v);
    }
    if let Some(v) = eval_external_table_function(short_name, args, env) {
        return Some(v);
    }
    if let Some(v) = eval_boolean_vector_function(short_name, args, env) {
        return Some(v);
    }
    if short_name == "distribution"
        && let Some(v) = eval_distribution_function(args, env)
    {
        return Some(v);
    }
    if let Some(v) = eval_clock_special_function(short_name, args, env) {
        return Some(v);
    }
    if let Some(v) = eval_stream_special_function(short_name, args, env) {
        return Some(v);
    }
    if let Some(v) = eval_state_accessor_special_function(short_name, args, env) {
        return Some(v);
    }
    eval_unit_conversion_function(short_name, args, env)
}

#[derive(Clone)]
struct OutputProjection {
    output_name: String,
    indices: Vec<i64>,
}

fn parse_projection_suffix(suffix: &str) -> Option<OutputProjection> {
    if suffix.is_empty() {
        return None;
    }
    let (output_name, indices) = if let Some(open) = suffix.find('[') {
        if !suffix.ends_with(']') || open == 0 {
            return None;
        }
        let name = suffix[..open].to_string();
        let inner = &suffix[open + 1..suffix.len() - 1];
        let idx = inner
            .split(',')
            .map(str::trim)
            .map(|token| token.parse::<i64>().ok())
            .collect::<Option<Vec<_>>>()?;
        (name, idx)
    } else {
        (suffix.to_string(), Vec::new())
    };
    Some(OutputProjection {
        output_name,
        indices,
    })
}

fn resolve_user_function_target<T: SimFloat>(
    requested_name: &str,
    env: &VarEnv<T>,
) -> Option<(VarName, Option<OutputProjection>)> {
    if resolve_user_function(requested_name, env).is_some() {
        return Some((VarName::new(requested_name), None));
    }

    let mut split_positions: Vec<usize> =
        requested_name.match_indices('.').map(|(i, _)| i).collect();
    split_positions.reverse();
    for split_idx in split_positions {
        let base_name = &requested_name[..split_idx];
        let suffix = &requested_name[split_idx + 1..];
        let Some(projection) = parse_projection_suffix(suffix) else {
            continue;
        };
        if resolve_user_function(base_name, env).is_some() {
            return Some((VarName::new(base_name), Some(projection)));
        }
    }
    None
}

/// Resolve a function call target and return its output names in declaration order.
///
/// This is used by algorithm statement evaluation for multi-output assignments.
pub fn resolve_function_call_outputs_pub<T: SimFloat>(
    name: &VarName,
    env: &VarEnv<T>,
) -> Option<(VarName, Vec<String>)> {
    let (resolved_name, projection) = resolve_user_function_target(name.as_str(), env)?;
    if projection.is_some() {
        return None;
    }
    let func = resolve_user_function(resolved_name.as_str(), env)?;
    let output_names = func
        .outputs
        .iter()
        .map(|param| param.name.clone())
        .collect();
    Some((resolved_name, output_names))
}

struct RecursionDepthGuard;

impl Drop for RecursionDepthGuard {
    fn drop(&mut self) {
        FUNC_RECURSION_DEPTH.with(|cell| cell.set(cell.get().saturating_sub(1)));
    }
}

fn try_enter_function_recursion() -> Option<RecursionDepthGuard> {
    let depth = FUNC_RECURSION_DEPTH.with(|cell| {
        let depth = cell.get();
        cell.set(depth + 1);
        depth
    });
    if depth >= MAX_FUNC_RECURSION {
        FUNC_RECURSION_DEPTH.with(|cell| cell.set(cell.get().saturating_sub(1)));
        return None;
    }
    Some(RecursionDepthGuard)
}

fn build_local_function_env<T: SimFloat>(env: &VarEnv<T>) -> VarEnv<T> {
    let mut local_env = VarEnv::<T>::new();
    local_env.functions = env.functions.clone();
    local_env.dims = env.dims.clone();
    local_env.start_exprs = env.start_exprs.clone();
    local_env.is_initial = env.is_initial;
    local_env
        .vars
        .extend(env.vars.iter().map(|(k, v)| (k.clone(), *v)));
    local_env.function_closures = env.function_closures.clone();
    local_env
}

fn seed_function_scope_dims<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    inputs: &[FunctionParam],
    outputs: &[FunctionParam],
    locals: &[FunctionParam],
) {
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    for param in inputs.iter().chain(outputs.iter()).chain(locals.iter()) {
        if param.dims.is_empty() || param.dims.iter().any(|dim| *dim <= 0) {
            continue;
        }
        dims.insert(param.name.clone(), param.dims.clone());
    }
}

fn bind_user_function_inputs<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    function_name: &str,
    inputs: &[FunctionParam],
    args: &[Expression],
    caller_env: &VarEnv<T>,
) {
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_idx = 0usize;
    for param in inputs {
        let arg_expr = named_args.get(param.name.as_str()).copied().or_else(|| {
            let next = positional_args.get(positional_idx).copied();
            if next.is_some() {
                positional_idx += 1;
            }
            next
        });

        if let Some(arg_expr) = arg_expr {
            local_env.set(&param.name, eval_expr::<T>(arg_expr, caller_env));
            maybe_bind_function_input_alias(local_env, function_name, param, arg_expr, caller_env);
            if let Some(arg_path) = eval_field_access_path(arg_expr, caller_env) {
                copy_projected_input_fields(local_env, &param.name, &arg_path, caller_env);
            }
            copy_array_input_entries(local_env, &param.name, arg_expr, caller_env);
            continue;
        }

        let val = param
            .default
            .as_ref()
            .map(|default_expr| eval_expr::<T>(default_expr, local_env))
            .unwrap_or(T::zero());
        local_env.set(&param.name, val);
        if let Some(default_expr) = &param.default {
            maybe_bind_function_input_alias(
                local_env,
                function_name,
                param,
                default_expr,
                caller_env,
            );
        }
    }
}

fn copy_array_literal_vector_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    elements: &[Expression],
    caller_env: &VarEnv<T>,
) -> bool {
    if elements.is_empty() {
        return false;
    }

    let mut first = None;
    let projection_field = projected_component_field_in_current_call();
    for (idx, element) in elements.iter().enumerate() {
        let value = eval_expr::<T>(element, caller_env);
        if first.is_none() {
            first = Some(value);
        }
        local_env.set(&format!("{param_name}[{}]", idx + 1), value);
        if let Some(field) = projection_field {
            local_env.set(&format!("{param_name}.{field}[{}]", idx + 1), value);
            local_env.set(&format!("{param_name}[{}].{field}", idx + 1), value);
        }
    }
    if let Some(value) = first {
        local_env.set(param_name, value);
        if let Some(field) = projection_field {
            local_env.set(&format!("{param_name}.{field}"), value);
        }
    }
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    let shape = vec![elements.len() as i64];
    dims.insert(param_name.to_string(), shape.clone());
    if let Some(field) = projection_field {
        dims.insert(format!("{param_name}.{field}"), shape);
    }
    true
}

fn copy_array_literal_matrix_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    rows: &[Expression],
    caller_env: &VarEnv<T>,
) -> bool {
    if rows.is_empty() {
        return false;
    }

    let mut first = None;
    let mut max_cols = 0usize;
    let projection_field = projected_component_field_in_current_call();
    for (row_idx, row_expr) in rows.iter().enumerate() {
        let row_values: Vec<&Expression> = match row_expr {
            Expression::Array { elements, .. } => elements.iter().collect(),
            _ => vec![row_expr],
        };
        max_cols = max_cols.max(row_values.len());
        for (col_idx, value_expr) in row_values.iter().enumerate() {
            let value = eval_expr::<T>(value_expr, caller_env);
            if first.is_none() {
                first = Some(value);
            }
            local_env.set(
                &format!("{param_name}[{},{}]", row_idx + 1, col_idx + 1),
                value,
            );
            if let Some(field) = projection_field {
                local_env.set(
                    &format!("{param_name}.{field}[{},{}]", row_idx + 1, col_idx + 1),
                    value,
                );
                local_env.set(
                    &format!("{param_name}[{},{}].{field}", row_idx + 1, col_idx + 1),
                    value,
                );
            }
        }
    }

    if max_cols == 0 {
        return false;
    }
    if let Some(value) = first {
        local_env.set(param_name, value);
        if let Some(field) = projection_field {
            local_env.set(&format!("{param_name}.{field}"), value);
        }
    }
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    let shape = vec![rows.len() as i64, max_cols as i64];
    dims.insert(param_name.to_string(), shape.clone());
    if let Some(field) = projection_field {
        dims.insert(format!("{param_name}.{field}"), shape);
    }
    true
}

fn projected_component_field_in_current_call() -> Option<&'static str> {
    let caller = current_function_call_name()?;
    if caller.ends_with(".re") {
        Some("re")
    } else if caller.ends_with(".im") {
        Some("im")
    } else {
        None
    }
}

fn copy_array_literal_input_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> bool {
    let Expression::Array {
        elements,
        is_matrix,
    } = arg_expr
    else {
        return false;
    };

    if *is_matrix {
        copy_array_literal_matrix_entries(local_env, param_name, elements, caller_env)
    } else {
        copy_array_literal_vector_entries(local_env, param_name, elements, caller_env)
    }
}

fn is_pre_like_call_name(name: &VarName) -> bool {
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    short.eq_ignore_ascii_case("pre") || short.eq_ignore_ascii_case("previous")
}

fn pre_like_array_source_name<T: SimFloat>(
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Option<String> {
    let pre_arg = match arg_expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
        } => args.first(),
        Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
        } if is_pre_like_call_name(name) => args.first(),
        _ => None,
    }?;
    match pre_arg {
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            Some(name.as_str().to_string())
        }
        _ => eval_field_access_path(pre_arg, caller_env),
    }
}

fn lookup_pre_with_normalization<T: SimFloat>(name: &str, env: &VarEnv<T>) -> Option<T> {
    if let Some(value) = lookup_pre_value(name) {
        return Some(T::from_f64(value));
    }
    if let Some(normalized) = normalize_var_name::<T>(name, env)
        && let Some(value) = lookup_pre_value(normalized.as_str())
    {
        return Some(T::from_f64(value));
    }
    if let Some(base_name) = unity_subscript_base_name(name)
        && let Some(value) = lookup_pre_value(base_name.as_str())
    {
        return Some(T::from_f64(value));
    }
    None
}

fn copy_array_input_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) {
    if copy_array_literal_input_entries(local_env, param_name, arg_expr, caller_env) {
        return;
    }

    let trace_array_bind =
        std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok();
    let pre_source_name = pre_like_array_source_name(arg_expr, caller_env);
    let use_pre_values = pre_source_name.is_some();
    let source_name = pre_source_name.or_else(|| match arg_expr {
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            Some(name.as_str().to_string())
        }
        _ => eval_field_access_path(arg_expr, caller_env),
    });
    let Some(source_name) = source_name else {
        return;
    };

    if let Some(value) = if use_pre_values {
        lookup_pre_with_normalization::<T>(&source_name, caller_env)
    } else {
        caller_env.vars.get(&source_name).copied()
    } {
        local_env.set(param_name, value);
    }

    let source_index_prefix = format!("{source_name}[");
    let mut copied_entries = 0usize;
    for (key, value) in &caller_env.vars {
        if let Some(index_suffix) = key.strip_prefix(&source_name)
            && index_suffix.starts_with('[')
        {
            let source_index_key = format!("{source_name}{index_suffix}");
            let copied_value = if use_pre_values {
                lookup_pre_with_normalization::<T>(&source_index_key, caller_env).unwrap_or(*value)
            } else {
                *value
            };
            local_env.set(&format!("{param_name}{index_suffix}"), copied_value);
            copied_entries += 1;
            continue;
        }
        if let Some(index_suffix) = key.strip_prefix(&source_index_prefix) {
            let source_index_key = format!("{source_index_prefix}{index_suffix}");
            let copied_value = if use_pre_values {
                lookup_pre_with_normalization::<T>(&source_index_key, caller_env).unwrap_or(*value)
            } else {
                *value
            };
            local_env.set(&format!("{param_name}[{index_suffix}"), copied_value);
            copied_entries += 1;
        }
    }

    if let Some(dims) = caller_env.dims.get(&source_name) {
        std::sync::Arc::make_mut(&mut local_env.dims).insert(param_name.to_string(), dims.clone());
    }

    if trace_array_bind && source_name.contains("timeTable.table") {
        let t11 = caller_env
            .vars
            .get(&format!("{}[1,1]", source_name))
            .copied();
        let t21 = caller_env
            .vars
            .get(&format!("{}[2,1]", source_name))
            .copied();
        let t22 = caller_env
            .vars
            .get(&format!("{}[2,2]", source_name))
            .copied();
        eprintln!(
            "[sim-trace] function array-arg bind source='{}' param='{}' copied_entries={} dims={:?} base_present={} sample_entries=[1,1]={:?} [2,1]={:?} [2,2]={:?}",
            source_name,
            param_name,
            copied_entries,
            caller_env.dims.get(&source_name),
            caller_env.vars.contains_key(&source_name),
            t11,
            t21,
            t22
        );
    }
}

fn initialize_user_function_scope_values<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    outputs: &[FunctionParam],
    locals: &[FunctionParam],
) {
    for param in outputs.iter().chain(locals.iter()) {
        let val = param
            .default
            .as_ref()
            .map(|d| eval_expr::<T>(d, local_env))
            .unwrap_or(T::zero());
        local_env.set(&param.name, val);
    }
}

fn projected_output_name(projection: &OutputProjection) -> String {
    if projection.indices.is_empty() {
        return projection.output_name.clone();
    }
    let joined = projection
        .indices
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    format!("{}[{joined}]", projection.output_name)
}

struct FunctionCallSummary {
    args_len: usize,
    inputs_len: usize,
    outputs_len: usize,
    locals_len: usize,
    body_len: usize,
    is_external: bool,
}

fn trace_function_call_summary(
    trace_call: bool,
    requested_name: &VarName,
    resolved_name: &VarName,
    summary: &FunctionCallSummary,
) {
    if !trace_call {
        return;
    }
    eprintln!(
        "[sim-introspect] function-call name='{}' resolved='{}' args={} inputs={} outputs={} locals={} body={} external={}",
        requested_name.as_str(),
        resolved_name.as_str(),
        summary.args_len,
        summary.inputs_len,
        summary.outputs_len,
        summary.locals_len,
        summary.body_len,
        summary.is_external
    );
}

fn trace_function_call_body_once(
    trace_call: bool,
    resolved_name: &VarName,
    body: &[rumoca_ir_flat::Statement],
) {
    if !trace_call {
        return;
    }
    static PRINT_FUNCTION_BODY_ONCE: std::sync::Once = std::sync::Once::new();
    PRINT_FUNCTION_BODY_ONCE.call_once(|| {
        eprintln!(
            "[sim-introspect] function-call first traced body for '{}': {:?}",
            resolved_name.as_str(),
            body
        );
    });
}

fn trace_function_call_inputs<T: SimFloat>(
    trace_call: bool,
    local_env: &VarEnv<T>,
    inputs: &[FunctionParam],
) {
    if !trace_call || std::env::var("RUMOCA_SIM_TRACE_FUNCTION_INPUTS").is_err() {
        return;
    }
    for input in inputs {
        let name = input.name.as_str();
        let dim = local_env.dims.get(name).cloned();
        let first = local_env.vars.get(&format!("{name}[1]")).copied();
        let second = local_env.vars.get(&format!("{name}[2]")).copied();
        let first_re = local_env.vars.get(&format!("{name}[1].re")).copied();
        let second_re = local_env.vars.get(&format!("{name}[2].re")).copied();
        let first_im = local_env.vars.get(&format!("{name}[1].im")).copied();
        let second_im = local_env.vars.get(&format!("{name}[2].im")).copied();
        eprintln!(
            "[sim-introspect] function-call input {} dims={:?} {}[1]={:?} {}[2]={:?} {}[1].re={:?} {}[2].re={:?} {}[1].im={:?} {}[2].im={:?}",
            name,
            dim,
            name,
            first,
            name,
            second,
            name,
            first_re,
            name,
            second_re,
            name,
            first_im,
            name,
            second_im
        );
    }
}

fn trace_function_call_outputs<T: SimFloat>(
    trace_call: bool,
    local_env: &VarEnv<T>,
    outputs: &[FunctionParam],
) {
    if !trace_call {
        return;
    }
    for output in outputs {
        let base = output.name.as_str();
        eprintln!(
            "[sim-introspect] function-call output {} = {} | {}.re = {} | {}.im = {}",
            base,
            local_env.get(base).real(),
            base,
            local_env.get(&format!("{base}.re")).real(),
            base,
            local_env.get(&format!("{base}.im")).real()
        );
    }
}

fn maybe_trace_interpolation_coefficients_state<T: SimFloat>(
    resolved_name: &VarName,
    local_env: &VarEnv<T>,
    body: &[rumoca_ir_flat::Statement],
) {
    if (std::env::var("RUMOCA_SIM_TRACE").is_err()
        && std::env::var("RUMOCA_SIM_INTROSPECT").is_err())
        || !resolved_name
            .as_str()
            .ends_with("Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients")
    {
        return;
    }
    static PRINT_GETINTERP_BODY: std::sync::Once = std::sync::Once::new();
    PRINT_GETINTERP_BODY.call_once(|| {
        eprintln!("[sim-trace] getInterpolationCoefficients body={:?}", body);
    });
    eprintln!(
        "[sim-trace] getInterpolationCoefficients state: startTimeScaled={} shiftTimeScaled={} timeScaled={} last_in={} next={} nrow={} tp={} dt={} a={} b={} nextEventScaled={}",
        local_env.get("startTimeScaled").real(),
        local_env.get("shiftTimeScaled").real(),
        local_env.get("timeScaled").real(),
        local_env.get("last").real(),
        local_env.get("next").real(),
        local_env.get("nrow").real(),
        local_env.get("tp").real(),
        local_env.get("dt").real(),
        local_env.get("a").real(),
        local_env.get("b").real(),
        local_env.get("nextEventScaled").real()
    );
}

fn resolve_projection_value<T: SimFloat>(
    local_env: &VarEnv<T>,
    projection: &OutputProjection,
) -> T {
    let projected_name = projected_output_name(projection);
    if let Some(value) = local_env.vars.get(&projected_name).copied() {
        return value;
    }
    if let Some((base_output, _)) = projection.output_name.split_once('.')
        && let Some(value) = local_env.vars.get(base_output).copied()
    {
        // Scalar evaluator executes user functions in projection context
        // (`*.re` / `*.im` caller names). If field materialization is absent
        // in the local env, use the base output bound in that context.
        return value;
    }
    T::zero()
}

fn eval_user_function_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let (resolved_name, projection) = resolve_user_function_target(name.as_str(), env)?;
    let func = resolve_user_function(resolved_name.as_str(), env)?;

    let Some(_recursion_depth_guard) = try_enter_function_recursion() else {
        return Some(T::nan());
    };

    // External/empty stubs cannot be evaluated here; allow special handlers
    // (e.g. BooleanVectors/Clock wrappers) to resolve after this path.
    if func.external.is_some() || func.body.is_empty() {
        return None;
    }

    // Clone function data to release borrow on env.functions
    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    let trace_call = function_trace_match_enabled(name.as_str(), resolved_name.as_str());
    let summary = FunctionCallSummary {
        args_len: args.len(),
        inputs_len: inputs.len(),
        outputs_len: outputs.len(),
        locals_len: locals.len(),
        body_len: body.len(),
        is_external: func.external.is_some(),
    };
    trace_function_call_summary(trace_call, name, &resolved_name, &summary);
    trace_function_call_body_once(trace_call, &resolved_name, &body);

    let mut local_env = build_local_function_env(env);
    seed_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    // Bind arguments/defaults and execute the body under the call-stack context
    // so projected function calls (`*.re` / `*.im`) propagate correctly.
    with_function_call_stack(name.as_str(), || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env);
        trace_function_call_inputs(trace_call, &local_env, &inputs);
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals);
        crate::statement::eval_statements(&body, &mut local_env);
    });
    trace_function_call_outputs(trace_call, &local_env, &outputs);
    maybe_trace_interpolation_coefficients_state(&resolved_name, &local_env, &body);

    if let Some(ref projection) = projection {
        return Some(resolve_projection_value(&local_env, projection));
    }

    Some(
        outputs
            .first()
            .map_or_else(T::zero, |out| local_env.get(&out.name)),
    )
}

fn function_trace_match_enabled(name: &str, resolved_name: &str) -> bool {
    let Ok(raw) = std::env::var("RUMOCA_SIM_TRACE_FUNCTION_MATCH") else {
        return false;
    };
    raw.split(',')
        .map(str::trim)
        .filter(|token| !token.is_empty())
        .any(|token| name.contains(token) || resolved_name.contains(token))
}

fn copy_projected_input_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    arg_path: &str,
    env: &VarEnv<T>,
) {
    let src_prefix = format!("{arg_path}.");
    let dst_prefix = format!("{param_name}.");
    for (field_name, field_value) in projected_input_fields(env, &src_prefix, &dst_prefix) {
        local_env.set(&field_name, field_value);
    }
}

fn projected_input_fields<T: SimFloat>(
    env: &VarEnv<T>,
    src_prefix: &str,
    dst_prefix: &str,
) -> Vec<(String, T)> {
    let mut projected = Vec::new();
    for (key, value) in &env.vars {
        if let Some(suffix) = key.strip_prefix(src_prefix) {
            projected.push((format!("{dst_prefix}{suffix}"), *value));
        }
    }
    projected
}

fn maybe_bind_function_input_alias<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    current_function_name: &str,
    param: &FunctionParam,
    arg_expr: &Expression,
    env: &VarEnv<T>,
) {
    // Only function-typed inputs can be invoked as callable values.
    if !param.type_name.to_ascii_lowercase().contains("function") {
        return;
    }
    let Some(closure) = function_closure_from_arg(arg_expr, env) else {
        return;
    };
    local_env
        .function_closures
        .insert(param.name.clone(), closure.clone());
    local_env
        .function_closures
        .insert(format!("{current_function_name}.{}", param.name), closure);
}

fn function_closure_from_arg<T: SimFloat>(
    arg_expr: &Expression,
    env: &VarEnv<T>,
) -> Option<FunctionClosure> {
    match arg_expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } if !is_constructor && resolve_user_function(name.as_str(), env).is_some() => {
            Some(FunctionClosure {
                target_name: name.clone(),
                bound_args: args.clone(),
            })
        }
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            resolve_function_closure(name.as_str(), env).cloned()
        }
        _ => None,
    }
}

fn eval_function_closure_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let closure = resolve_function_closure(name.as_str(), env)?;
    let target = resolve_user_function(closure.target_name.as_str(), env)?;
    let mut merged_args = Vec::with_capacity(args.len().saturating_add(closure.bound_args.len()));
    // Modelica partial function application leaves at least one open argument
    // (e.g., `f(u)`), so runtime invocation supplies those first.
    merged_args.extend(args.iter().cloned());
    merged_args.extend(closure.bound_args.iter().cloned());
    if merged_args.len() > target.inputs.len() {
        return None;
    }
    eval_user_function_call(&closure.target_name, &merged_args, env)
}

fn eval_constructor_call<T: SimFloat>(name: &VarName, args: &[Expression], env: &VarEnv<T>) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        if let Some(caller) = current_function_call_name()
            && caller.ends_with(".im")
        {
            // Constructors like Complex(1) imply zero imaginary part.
            return T::zero();
        }
        return eval_expr::<T>(&args[0], env);
    }

    // When constructor calls survive into scalar evaluation, prefer component
    // selection by caller suffix if available (e.g., generated `*.im` helpers).
    if let Some(caller) = current_function_call_name() {
        if caller.ends_with(".im") {
            return eval_expr::<T>(&args[1], env);
        }
        if caller.ends_with(".re") {
            return eval_expr::<T>(&args[0], env);
        }
    }

    let _ = name;
    eval_expr::<T>(&args[0], env)
}

pub(super) fn eval_function_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    env: &VarEnv<T>,
) -> T {
    if is_constructor {
        return eval_constructor_call(name, args, env);
    }
    // Try qualified builtin resolution:
    // "Modelica.Math.sin" → "sin" → BuiltinFunction::Sin
    let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if let Some(builtin) = BuiltinFunction::from_name(short_name) {
        return eval_builtin(builtin, args, env);
    }
    if let Some(builtin) = BuiltinFunction::from_name(&short_name.to_ascii_lowercase()) {
        return eval_builtin(builtin, args, env);
    }

    if let Some(result) = eval_function_closure_call(name, args, env) {
        return result;
    }

    // Preserve user-function lookup precedence for qualified alias cases.
    if let Some(result) = eval_user_function_call(name, args, env) {
        return result;
    }
    if let Some(result) = eval_special_function_call(name.as_str(), args, env) {
        return result;
    }
    trace_unresolved_user_function(name.as_str(), env);

    warn_once!(
        WARNED_USER_FUNCTIONS,
        "User-defined function '{}' not supported in simulation evaluator, \
         returning NaN. Results may be incorrect.",
        name.as_str()
    );
    T::nan()
}

fn trace_unresolved_user_function<T: SimFloat>(name: &str, env: &VarEnv<T>) {
    if std::env::var("RUMOCA_SIM_TRACE").is_err() && std::env::var("RUMOCA_SIM_INTROSPECT").is_err()
    {
        return;
    }
    let short = name.rsplit('.').next().unwrap_or(name);
    let direct_hit = env.functions.contains_key(name);
    let short_hits: Vec<String> = env
        .functions
        .keys()
        .filter(|candidate| {
            candidate
                .rsplit('.')
                .next()
                .is_some_and(|leaf| leaf == short)
        })
        .take(16)
        .cloned()
        .collect();
    eprintln!(
        "[sim-trace] unresolved user function: name='{}' direct_hit={} total_functions={} short='{}' short_hits={:?}",
        name,
        direct_hit,
        env.functions.len(),
        short,
        short_hits
    );
}

/// Public wrapper for `eval_builtin`, used by the statement evaluator.
pub fn eval_builtin_pub<T: SimFloat>(
    function: BuiltinFunction,
    args: &[Expression],
    env: &VarEnv<T>,
) -> T {
    eval_builtin(function, args, env)
}

/// Public wrapper for `eval_function_call`, used by the statement evaluator.
pub fn eval_function_call_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> T {
    eval_function_call(name, args, false, env)
}

pub(super) fn eval_if<T: SimFloat>(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    env: &VarEnv<T>,
) -> T {
    for (cond, then_expr) in branches {
        if eval_expr::<T>(cond, env).to_bool() {
            return eval_expr::<T>(then_expr, env);
        }
    }
    eval_expr::<T>(else_branch, env)
}

/// Evaluate a condition expression as a smooth zero-crossing function for root finding (f64-only).
pub fn eval_condition_as_root(expr: &Expression, env: &VarEnv<f64>) -> f64 {
    match expr {
        Expression::Binary { op, lhs, rhs } => {
            let l = eval_expr::<f64>(lhs, env);
            let r = eval_expr::<f64>(rhs, env);
            match op {
                OpBinary::Lt(_) | OpBinary::Le(_) => l - r,
                OpBinary::Gt(_) | OpBinary::Ge(_) => r - l,
                _ => {
                    let val = eval_expr::<f64>(expr, env);
                    if val.to_bool() { -1.0 } else { 1.0 }
                }
            }
        }
        _ => {
            let val = eval_expr::<f64>(expr, env);
            if val.to_bool() { -1.0 } else { 1.0 }
        }
    }
}
