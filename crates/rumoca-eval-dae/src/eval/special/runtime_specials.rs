use super::*;
use std::sync::Mutex;

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
        .map(|expr| eval_expr_or_default::<T>(expr, env).real().round() as i64)
        .unwrap_or(0)
}

fn clamp_i64_to_positive_u31(v: u64) -> i64 {
    let raw = (v & 0x7FFF_FFFF) as i64;
    if raw == 0 { 1 } else { raw }
}

pub fn modelica_strings_hash_string(value: &str) -> i32 {
    let mut hash = 0xAAAA_AAAA_u32;
    for (idx, byte) in value.bytes().enumerate() {
        if idx & 1 == 0 {
            let mixed = (hash << 7) ^ u32::from(byte).wrapping_mul(hash >> 3);
            hash ^= mixed;
        } else {
            let mixed = (hash << 11).wrapping_add(u32::from(byte) ^ (hash >> 5));
            hash ^= !mixed;
        }
    }
    i32::from_ne_bytes(hash.to_ne_bytes())
}

fn automatic_local_seed_from_expr<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> i64 {
    if let Some(Expression::Literal {
        value: Literal::String(path),
        span: rumoca_core::Span::DUMMY,
    }) = args.first()
    {
        return i64::from(modelica_strings_hash_string(path));
    }
    // Fallback for non-literal paths in numeric-only runtime.
    let approx = args
        .first()
        .map(|expr| eval_expr_or_default::<T>(expr, env).real().to_bits())
        .unwrap_or(0);
    clamp_i64_to_positive_u31(scramble_seed(approx))
}

fn automatic_global_seed_in(registry: &Mutex<ImpureRandomRegistry>) -> i64 {
    let mut registry = registry
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    registry.auto_seed_counter = registry.auto_seed_counter.wrapping_add(1);
    deterministic_automatic_global_seed(registry.auto_seed_counter)
}

pub fn deterministic_automatic_global_seed(counter: u64) -> i64 {
    clamp_i64_to_positive_u31(scramble_seed(counter))
}

fn initialize_impure_random_in(registry: &Mutex<ImpureRandomRegistry>, seed: i64) -> i64 {
    let mut registry = registry
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    registry.next_id = registry.next_id.saturating_add(1);
    let id = registry.next_id.max(1);
    let mixed = scramble_seed((seed as u64) ^ (id as u64).wrapping_mul(0x9E3779B97F4A7C15));
    registry.streams.insert(id, mixed.max(1));
    id
}

fn impure_random_value_in(registry: &Mutex<ImpureRandomRegistry>, id: i64) -> f64 {
    let mut registry = registry
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
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
                .map(|expr| eval_expr_or_default::<T>(expr, env))
                .unwrap_or_else(T::one);
            if cond.to_bool() {
                Some(T::zero())
            } else {
                Some(T::nan())
            }
        }
        "terminate" => Some(T::zero()),
        "cardinality" => Some(T::zero()),
        "array" => Some(
            args.first()
                .map(|expr| eval_expr_or_default::<T>(expr, env))
                .unwrap_or_else(T::zero),
        ),
        "automaticGlobalSeed" => Some(T::from_f64(automatic_global_seed_in(
            &env.runtime.impure_random,
        ) as f64)),
        "automaticLocalSeed" => Some(T::from_f64(automatic_local_seed_from_expr(args, env) as f64)),
        "initializeImpureRandom" => {
            let seed = eval_integer_arg(args, 0, env);
            Some(T::from_f64(
                initialize_impure_random_in(&env.runtime.impure_random, seed) as f64,
            ))
        }
        "impureRandom" => {
            let id = eval_integer_arg(args, 0, env);
            Some(T::from_f64(impure_random_value_in(
                &env.runtime.impure_random,
                id,
            )))
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
            let y =
                lo as f64 + (impure_random_value_in(&env.runtime.impure_random, id) * span).floor();
            Some(T::from_f64(y.clamp(lo as f64, hi as f64)))
        }
        _ => eval_string_misc_intrinsic_function(short_name, args, env),
    }
}

fn eval_string_misc_intrinsic_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        // String-valued utility intrinsics are accepted as simulation helpers.
        // The runtime evaluator is scalar-numeric, so these return conservative
        // numeric placeholders when true string evaluation is unavailable.
        "String" => Some(
            args.first()
                .map(|expr| eval_expr_or_default::<T>(expr, env))
                .unwrap_or_else(T::zero),
        ),
        "getInstanceName" | "loadResource" | "fullPathName" | "readLine" | "scanBoolean"
        | "scanDelimiter" | "scanIdentifier" | "scanInteger" | "scanNoToken" | "scanReal"
        | "scanString" | "scanToken" | "skipWhiteSpace" | "substring" => Some(T::zero()),
        "isValidTable" => Some(T::one()),
        "isEmpty" => eval_literal_string_arg(args, |s| T::from_bool(s.trim().is_empty())),
        "length" => eval_literal_string_arg(args, |s| T::from_f64(s.chars().count() as f64)),
        "find" | "findLast" => eval_string_find_intrinsic(short_name, args),
        _ => None,
    }
}

fn eval_literal_string_arg<T: SimFloat>(
    args: &[Expression],
    eval: impl FnOnce(&str) -> T,
) -> Option<T> {
    if let Some(Expression::Literal {
        value: Literal::String(s),
        span: rumoca_core::Span::DUMMY,
    }) = args.first()
    {
        Some(eval(s))
    } else {
        Some(T::zero())
    }
}

fn eval_string_find_intrinsic<T: SimFloat>(short_name: &str, args: &[Expression]) -> Option<T> {
    let (
        Some(Expression::Literal {
            value: Literal::String(haystack),
            span: rumoca_core::Span::DUMMY,
        }),
        Some(Expression::Literal {
            value: Literal::String(needle),
            span: rumoca_core::Span::DUMMY,
        }),
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
        idx.map_or(0.0, |index| index.saturating_add(1) as f64),
    ))
}

fn eval_qualified_special_function<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    if let Some(value) = eval_qualified_math_special_function(name, args, env) {
        return Some(value);
    }
    if let Some(value) = eval_qualified_distribution_function(name, args, env) {
        return Some(value);
    }
    match name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
        | "Modelica.Math.Random.Generators.Xorshift64star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
        | "Modelica.Math.Random.Generators.Xorshift1024star.initialState" => {
            let local_seed = eval_integer_arg(args, 0, env);
            let global_seed = eval_integer_arg(args, 1, env);
            let n_state = eval_integer_arg(args, 2, env).max(1);
            let mixed =
                scramble_seed((local_seed as u64) ^ ((global_seed as u64) << 1) ^ (n_state as u64));
            Some(T::from_f64(clamp_i64_to_positive_u31(mixed) as f64))
        }
        "Modelica.Math.Random.Generators.Xorshift64star.random"
        | "Modelica.Math.Random.Generators.Xorshift128plus.random"
        | "Modelica.Math.Random.Generators.Xorshift1024star.random" => {
            let state_tag = args
                .first()
                .map(|expr| eval_expr_or_default::<T>(expr, env).real().to_bits())
                .unwrap_or(1);
            let id = clamp_i64_to_positive_u31(scramble_seed(state_tag));
            Some(T::from_f64(impure_random_value_in(
                &env.runtime.impure_random,
                id,
            )))
        }
        _ => None,
    }
}

pub(super) fn runtime_special_output_names(name: &str) -> Option<&'static [&'static str]> {
    match name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
        | "Modelica.Math.Random.Generators.Xorshift64star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
        | "Modelica.Math.Random.Generators.Xorshift1024star.initialState" => Some(&["state"]),
        "Modelica.Math.Random.Generators.Xorshift64star.random"
        | "Modelica.Math.Random.Generators.Xorshift128plus.random"
        | "Modelica.Math.Random.Generators.Xorshift1024star.random" => {
            Some(&["result", "stateOut"])
        }
        _ => None,
    }
}

pub(super) fn resolve_runtime_special_target(
    requested_name: &str,
) -> Option<(VarName, Option<OutputSelection>)> {
    if runtime_special_output_names(requested_name).is_some() {
        return Some((VarName::new(requested_name), None));
    }

    rumoca_core::find_map_top_level_splits_rev(requested_name, |base_name, suffix| {
        let selection = parse_selection_suffix(suffix)?;
        if runtime_special_output_names(base_name).is_some() {
            Some((VarName::new(base_name), Some(selection)))
        } else {
            None
        }
    })
}

fn random_state_len<T: SimFloat>(
    base_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<usize> {
    match base_name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star" => {
            Some(eval_integer_arg(args, 2, env).max(1) as usize)
        }
        "Modelica.Math.Random.Generators.Xorshift64star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift64star.random" => Some(2),
        "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
        | "Modelica.Math.Random.Generators.Xorshift128plus.random" => Some(4),
        "Modelica.Math.Random.Generators.Xorshift1024star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift1024star.random" => Some(33),
        _ => None,
    }
}

fn unit_from_u64(sample: u64) -> f64 {
    (((sample >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))).clamp(f64::EPSILON, 1.0)
}

fn initial_state_values<T: SimFloat>(
    base_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let len = random_state_len(base_name, args, env)?;
    let local_seed = eval_integer_arg(args, 0, env);
    let global_seed = eval_integer_arg(args, 1, env);
    let mut state = scramble_seed(
        (local_seed as u64)
            ^ ((global_seed as u64) << 1)
            ^ (len as u64).wrapping_mul(0x9E3779B97F4A7C15),
    )
    .max(1);
    let mut out = Vec::with_capacity(len);
    for idx in 0..len {
        state = scramble_seed(state ^ (idx as u64 + 1).wrapping_mul(0xD1B54A32D192ED03)).max(1);
        out.push(T::from_f64(clamp_i64_to_positive_u31(state) as f64));
    }
    Some(out)
}

fn random_result_and_state<T: SimFloat>(
    base_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<(T, Vec<T>)> {
    let len = random_state_len(base_name, args, env)?;
    let seed_values = args
        .first()
        .map(|expr| eval_array_like_f64_values(expr, env))
        .filter(|values| !values.is_empty())
        .unwrap_or_else(|| vec![1.0]);
    let mut state = seed_values
        .iter()
        .fold(0u64, |acc, value| acc ^ scramble_seed(value.to_bits()))
        ^ scramble_seed(
            base_name
                .bytes()
                .fold(0u64, |acc, b| acc.wrapping_mul(16777619) ^ b as u64),
        );
    if state == 0 {
        state = 0x9E3779B97F4A7C15;
    }

    let mut out = Vec::with_capacity(len);
    for idx in 0..len {
        state = scramble_seed(state ^ (idx as u64 + 1).wrapping_mul(0x94D049BB133111EB)).max(1);
        out.push(T::from_f64(clamp_i64_to_positive_u31(state) as f64));
    }
    let mut sample_state = state.max(1);
    let result = T::from_f64(unit_from_u64(xorshift64star_next(&mut sample_state)));
    Some((result, out))
}

fn select_special_output<T: SimFloat>(values: &[T], selection: &OutputSelection) -> T {
    let idx = selection.indices.first().copied().unwrap_or(1).max(1) as usize - 1;
    values
        .get(idx)
        .copied()
        .unwrap_or_else(|| values.first().copied().unwrap_or_else(T::zero))
}

fn eval_selected_runtime_special_function<T: SimFloat>(
    base_name: &str,
    selection: &OutputSelection,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match selection.output_name.as_str() {
        "result" => random_result_and_state(base_name, args, env).map(|(result, _)| result),
        "stateOut" => {
            let (_, state) = random_result_and_state(base_name, args, env)?;
            Some(select_special_output(&state, selection))
        }
        "state" => {
            let state = initial_state_values(base_name, args, env)?;
            Some(select_special_output(&state, selection))
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
            | "specificHeatCapacityCp"
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
            | "fullPathName"
            | "loadResource"
            | "readLine"
            | "scanBoolean"
            | "scanDelimiter"
            | "scanIdentifier"
            | "scanInteger"
            | "scanNoToken"
            | "scanReal"
            | "scanString"
            | "scanToken"
            | "skipWhiteSpace"
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
            | "writeRealMatrix"
    )
}

fn is_runtime_special_function_qualified_name(name: &str) -> bool {
    if is_qualified_distribution_function_name(name) {
        return true;
    }
    if is_qualified_math_special_function_name(name) {
        return true;
    }
    matches!(
        name,
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
            | "Modelica.Math.Random.Generators.Xorshift64star.initialState"
            | "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
            | "Modelica.Math.Random.Generators.Xorshift1024star.initialState"
            | "Modelica.Math.Random.Generators.Xorshift64star.random"
            | "Modelica.Math.Random.Generators.Xorshift128plus.random"
            | "Modelica.Math.Random.Generators.Xorshift1024star.random"
    )
}

/// Returns true if the function name (qualified or short) is handled by
/// runtime special-function evaluators.
pub fn is_runtime_special_function_name(name: &str) -> bool {
    let short_name = rumoca_core::top_level_last_segment(name);
    is_runtime_special_function_short_name(short_name)
        || is_runtime_special_function_qualified_name(name)
        || resolve_runtime_special_target(name).is_some()
}

pub(super) fn eval_special_function_call<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    if let Some((resolved_name, Some(selection))) = resolve_runtime_special_target(name) {
        return eval_selected_runtime_special_function(
            resolved_name.as_str(),
            &selection,
            args,
            env,
        );
    }
    if let Some(v) = eval_qualified_special_function(name, args, env) {
        return Some(v);
    }
    let short_name = rumoca_core::top_level_last_segment(name);
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
