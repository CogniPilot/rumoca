use super::super::table_eval::resolve_modelica_resource_path;
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

fn eval_integer_arg<T: SimFloat>(
    args: &[Expression],
    idx: usize,
    env: &VarEnv<T>,
) -> Result<i64, EvalError> {
    let expr = args.get(idx).ok_or(EvalError::UnsupportedExpression {
        kind: "integer argument",
    })?;
    Ok(eval_expr::<T>(expr, env)?.real().round() as i64)
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

fn automatic_local_seed_from_expr<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<i64, EvalError> {
    let arg = args.first().ok_or(EvalError::UnsupportedExpression {
        kind: "automaticLocalSeed path",
    })?;
    let path = eval_string_expr(arg, env)?;
    Ok(i64::from(modelica_strings_hash_string(&path)))
}

fn eval_string_expr<T: SimFloat>(expr: &Expression, env: &VarEnv<T>) -> Result<String, EvalError> {
    match expr {
        Expression::Literal {
            value: Literal::String(value),
            ..
        } => Ok(value.clone()),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let Some(start) = env.start_exprs.get(name.as_str()) else {
                return Err(with_expression_span_if_available(
                    EvalError::MissingBinding {
                        name: name.as_str().to_string(),
                    },
                    expr,
                ));
            };
            eval_string_expr(start, env)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_expr::<T>(condition, env)?.to_bool() {
                    return eval_string_expr(value, env);
                }
            }
            eval_string_expr(else_branch, env)
        }
        Expression::FunctionCall {
            name, args, span, ..
        } => eval_string_function_call(name.as_str(), args, env)
            .map_err(|err| err.with_span_if_missing(*span)),
        _ => Err(with_expression_span_if_available(
            EvalError::UnsupportedExpression {
                kind: "string expression",
            },
            expr,
        )),
    }
}

fn eval_string_function_call<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<String, EvalError> {
    match name {
        "getInstanceName" => Err(EvalError::UnsupportedExpression {
            kind: "getInstanceName must be lowered to a string literal before DAE evaluation",
        }),
        "loadResource" | "Modelica.Utilities.Files.loadResource" => {
            let raw = args
                .first()
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "loadResource path",
                })
                .and_then(|arg| eval_string_expr(arg, env))?;
            Ok(resolve_modelica_resource_path(&raw)
                .map(|path| path.to_string_lossy().into_owned())
                .unwrap_or(raw))
        }
        _ => {
            if args.iter().any(|arg| {
                matches!(
                    arg,
                    Expression::Literal {
                        value: Literal::String(_),
                        ..
                    }
                )
            }) {
                return Err(EvalError::UnsupportedExpression {
                    kind: "string function call",
                });
            }
            Err(EvalError::MissingFunction {
                name: name.to_string(),
            })
        }
    }
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
) -> Result<Option<T>, EvalError> {
    match short_name {
        "assert" => {
            let cond = match args.first() {
                Some(expr) => eval_expr::<T>(expr, env)?,
                None => T::one(),
            };
            if cond.to_bool() {
                Ok(Some(T::zero()))
            } else {
                Err(EvalError::UnsupportedExpression { kind: "assert" })
            }
        }
        "terminate" => Err(EvalError::UnsupportedExpression {
            kind: "terminate statement in scalar expression",
        }),
        "cardinality" => Err(EvalError::UnsupportedExpression {
            kind: "unlowered cardinality operator",
        }),
        "array" => Err(EvalError::UnsupportedExpression {
            kind: "array constructor in scalar expression",
        }),
        "automaticGlobalSeed" => Ok(Some(T::from_f64(automatic_global_seed_in(
            &env.runtime.impure_random,
        ) as f64))),
        "automaticLocalSeed" => Ok(Some(T::from_f64(
            automatic_local_seed_from_expr(args, env)? as f64,
        ))),
        "initializeImpureRandom" => {
            let seed = eval_integer_arg(args, 0, env)?;
            Ok(Some(T::from_f64(
                initialize_impure_random_in(&env.runtime.impure_random, seed) as f64,
            )))
        }
        "impureRandom" => {
            let id = eval_integer_arg(args, 0, env)?;
            Ok(Some(T::from_f64(impure_random_value_in(
                &env.runtime.impure_random,
                id,
            ))))
        }
        "impureRandomInteger" => {
            let id = eval_integer_arg(args, 0, env)?;
            let imin = eval_integer_arg(args, 1, env)?;
            let imax = eval_integer_arg(args, 2, env)?;
            let (lo, hi) = if imin <= imax {
                (imin, imax)
            } else {
                (imax, imin)
            };
            let span = (hi - lo + 1).max(1) as f64;
            let y =
                lo as f64 + (impure_random_value_in(&env.runtime.impure_random, id) * span).floor();
            Ok(Some(T::from_f64(y.clamp(lo as f64, hi as f64))))
        }
        _ => eval_string_misc_intrinsic_function(short_name, args, env),
    }
}

fn eval_string_misc_intrinsic_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match rumoca_core::modelica_string_intrinsic_short_name(short_name) {
        Some(rumoca_core::ModelicaStringIntrinsic::RequiresLowering) => {
            return Err(EvalError::UnsupportedExpression {
                kind: "string/file intrinsic",
            });
        }
        Some(rumoca_core::ModelicaStringIntrinsic::IsEmpty) => {
            let value = eval_required_string_arg(args, env, 0, "string argument")?;
            return Ok(Some(T::from_bool(value.trim().is_empty())));
        }
        Some(rumoca_core::ModelicaStringIntrinsic::HashString) => {
            let value = eval_required_string_arg(args, env, 0, "string argument")?;
            return Ok(Some(T::from_f64(
                modelica_strings_hash_string(&value) as f64
            )));
        }
        Some(rumoca_core::ModelicaStringIntrinsic::Length) => {
            let value = eval_required_string_arg(args, env, 0, "string argument")?;
            return Ok(Some(T::from_f64(value.chars().count() as f64)));
        }
        Some(intrinsic @ rumoca_core::ModelicaStringIntrinsic::Find)
        | Some(intrinsic @ rumoca_core::ModelicaStringIntrinsic::FindLast) => {
            return eval_string_find_intrinsic(
                intrinsic == rumoca_core::ModelicaStringIntrinsic::FindLast,
                args,
                env,
            );
        }
        None => {}
    }

    match short_name {
        "String" => {
            let arg = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "String argument",
            })?;
            eval_expr::<T>(arg, env).map(Some)
        }
        "isValidTable" => Ok(Some(T::one())),
        _ => Ok(None),
    }
}

fn eval_required_string_arg<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
    idx: usize,
    kind: &'static str,
) -> Result<String, EvalError> {
    let expr = args
        .get(idx)
        .ok_or(EvalError::UnsupportedExpression { kind })?;
    eval_string_expr(expr, env)
}

fn eval_string_find_intrinsic<T: SimFloat>(
    find_last: bool,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let haystack = eval_string_find_string_arg(
        &named_args,
        &positional_args,
        "string",
        0,
        env,
        "string search arguments",
    )?;
    let needle = eval_string_find_string_arg(
        &named_args,
        &positional_args,
        "searchString",
        1,
        env,
        "string search arguments",
    )?;
    let case_sensitive =
        eval_string_find_bool_arg(&named_args, &positional_args, "caseSensitive", 3, env)?
            .unwrap_or(true);
    let default_start = if find_last {
        haystack.chars().count().max(1) as i64
    } else {
        1
    };
    let start_index =
        eval_string_find_integer_arg(&named_args, &positional_args, "startIndex", 2, env)?
            .unwrap_or(default_start);
    let idx = modelica_string_find(&haystack, &needle, start_index, find_last, case_sensitive);
    Ok(Some(T::from_f64(idx as f64)))
}

fn eval_string_find_string_arg<T: SimFloat>(
    named_args: &std::collections::HashMap<&str, &Expression>,
    positional_args: &[&Expression],
    name: &str,
    positional_idx: usize,
    env: &VarEnv<T>,
    kind: &'static str,
) -> Result<String, EvalError> {
    let expr = named_args
        .get(name)
        .copied()
        .or_else(|| positional_args.get(positional_idx).copied())
        .ok_or(EvalError::UnsupportedExpression { kind })?;
    eval_string_expr(expr, env)
}

fn eval_string_find_integer_arg<T: SimFloat>(
    named_args: &std::collections::HashMap<&str, &Expression>,
    positional_args: &[&Expression],
    name: &str,
    positional_idx: usize,
    env: &VarEnv<T>,
) -> Result<Option<i64>, EvalError> {
    let Some(expr) = named_args
        .get(name)
        .copied()
        .or_else(|| positional_args.get(positional_idx).copied())
    else {
        return Ok(None);
    };
    Ok(Some(eval_expr::<T>(expr, env)?.real().round() as i64))
}

fn eval_string_find_bool_arg<T: SimFloat>(
    named_args: &std::collections::HashMap<&str, &Expression>,
    positional_args: &[&Expression],
    name: &str,
    positional_idx: usize,
    env: &VarEnv<T>,
) -> Result<Option<bool>, EvalError> {
    let Some(expr) = named_args
        .get(name)
        .copied()
        .or_else(|| positional_args.get(positional_idx).copied())
    else {
        return Ok(None);
    };
    Ok(Some(eval_expr::<T>(expr, env)?.to_bool()))
}

fn modelica_string_find(
    haystack: &str,
    needle: &str,
    start_index: i64,
    find_last: bool,
    case_sensitive: bool,
) -> usize {
    if needle.is_empty() || start_index < 1 {
        return 0;
    }
    let source = if case_sensitive {
        haystack.to_string()
    } else {
        haystack.to_lowercase()
    };
    let target = if case_sensitive {
        needle.to_string()
    } else {
        needle.to_lowercase()
    };
    if find_last {
        find_last_modelica_index(&source, &target, start_index)
    } else {
        find_first_modelica_index(&source, &target, start_index)
    }
}

fn find_first_modelica_index(source: &str, target: &str, start_index: i64) -> usize {
    let start = (start_index - 1) as usize;
    source
        .char_indices()
        .nth(start)
        .and_then(|(byte_start, _)| {
            source[byte_start..]
                .find(target)
                .map(|offset| source[..byte_start + offset].chars().count() + 1)
        })
        .unwrap_or(0)
}

fn find_last_modelica_index(source: &str, target: &str, start_index: i64) -> usize {
    let char_count = source.chars().count();
    if char_count == 0 {
        return 0;
    }
    let end_char = start_index.min(char_count as i64).max(1) as usize;
    source
        .char_indices()
        .nth(end_char)
        .map(|(byte_end, _)| &source[..byte_end])
        .unwrap_or(source)
        .rfind(target)
        .map(|byte_index| source[..byte_index].chars().count() + 1)
        .unwrap_or(0)
}

fn eval_qualified_special_function<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    if let Some(value) = eval_qualified_math_special_function(name, args, env)? {
        return Ok(Some(value));
    }
    if let Some(value) = eval_qualified_distribution_function(name, args, env)? {
        return Ok(Some(value));
    }
    match name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star"
        | "Modelica.Math.Random.Generators.Xorshift64star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
        | "Modelica.Math.Random.Generators.Xorshift1024star.initialState" => {
            let local_seed = eval_integer_arg(args, 0, env)?;
            let global_seed = eval_integer_arg(args, 1, env)?;
            let Some(n_state) = random_state_len(name, args, env)? else {
                return Ok(None);
            };
            let n_state = n_state as i64;
            let mixed =
                scramble_seed((local_seed as u64) ^ ((global_seed as u64) << 1) ^ (n_state as u64));
            Ok(Some(T::from_f64(clamp_i64_to_positive_u31(mixed) as f64)))
        }
        "Modelica.Math.Random.Generators.Xorshift64star.random"
        | "Modelica.Math.Random.Generators.Xorshift128plus.random"
        | "Modelica.Math.Random.Generators.Xorshift1024star.random" => {
            let state_tag = match args.first() {
                Some(expr) => eval_expr::<T>(expr, env)?.real().to_bits(),
                None => 1,
            };
            let id = clamp_i64_to_positive_u31(scramble_seed(state_tag));
            Ok(Some(T::from_f64(impure_random_value_in(
                &env.runtime.impure_random,
                id,
            ))))
        }
        _ => Ok(None),
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

pub(in crate::eval) fn resolve_runtime_special_target(
    requested_name: &str,
) -> Option<(VarName, Option<OutputSelection>)> {
    if runtime_special_output_names(requested_name).is_some() {
        return Some((VarName::new(requested_name), None));
    }

    rumoca_core::find_map_top_level_splits_rev(requested_name, |base_name, suffix| {
        let outputs = runtime_special_output_names(base_name)?;
        if !outputs.contains(&suffix) {
            return None;
        }
        Some((
            VarName::new(base_name),
            Some(OutputSelection::new(suffix, &[])),
        ))
    })
}

fn random_state_len<T: SimFloat>(
    base_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<usize>, EvalError> {
    match base_name {
        "Modelica.Math.Random.Utilities.initialStateWithXorshift64star" => {
            Ok(Some(eval_integer_arg(args, 2, env)?.max(1) as usize))
        }
        "Modelica.Math.Random.Generators.Xorshift64star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift64star.random" => Ok(Some(2)),
        "Modelica.Math.Random.Generators.Xorshift128plus.initialState"
        | "Modelica.Math.Random.Generators.Xorshift128plus.random" => Ok(Some(4)),
        "Modelica.Math.Random.Generators.Xorshift1024star.initialState"
        | "Modelica.Math.Random.Generators.Xorshift1024star.random" => Ok(Some(33)),
        _ => Ok(None),
    }
}

fn unit_from_u64(sample: u64) -> f64 {
    (((sample >> 11) as f64) * (1.0 / ((1u64 << 53) as f64))).clamp(f64::EPSILON, 1.0)
}

fn initial_state_values<T: SimFloat>(
    base_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let Some(len) = random_state_len(base_name, args, env)? else {
        return Ok(None);
    };
    let local_seed = eval_integer_arg(args, 0, env)?;
    let global_seed = eval_integer_arg(args, 1, env)?;
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
    Ok(Some(out))
}

fn random_result_and_state<T: SimFloat>(
    base_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<(T, Vec<T>)>, EvalError> {
    let Some(len) = random_state_len(base_name, args, env)? else {
        return Ok(None);
    };
    let seed_values = match args.first() {
        Some(expr) => match eval_array_like_f64_values(expr, env) {
            Ok(values) if !values.is_empty() => values,
            Ok(_) => vec![1.0],
            Err(err) => return Err(err),
        },
        None => vec![1.0],
    };
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
    Ok(Some((result, out)))
}

fn select_special_output<T: SimFloat>(
    values: &[T],
    selection: &OutputSelection,
) -> Result<T, EvalError> {
    let Some(index) = selection.indices.first().copied() else {
        return Err(EvalError::ShapeMismatch {
            context: "runtime special output selection",
            expected: 1,
            actual: 0,
        });
    };
    let idx = usize::try_from(index)
        .ok()
        .and_then(|index| index.checked_sub(1))
        .ok_or(EvalError::InvalidShape {
            context: "runtime special output selection",
            reason: format!("output index must be positive, got {index}"),
        })?;
    values.get(idx).copied().ok_or(EvalError::ShapeMismatch {
        context: "runtime special output selection",
        expected: idx + 1,
        actual: values.len(),
    })
}

pub(super) fn eval_selected_runtime_special_function<T: SimFloat>(
    base_name: &str,
    selection: &OutputSelection,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match selection.output_name.as_str() {
        "result" => Ok(random_result_and_state(base_name, args, env)?.map(|(result, _)| result)),
        "stateOut" => {
            let Some((_, state)) = random_result_and_state(base_name, args, env)? else {
                return Ok(None);
            };
            select_special_output(&state, selection).map(Some)
        }
        "state" => {
            let Some(state) = initial_state_values(base_name, args, env)? else {
                return Ok(None);
            };
            select_special_output(&state, selection).map(Some)
        }
        _ => Ok(None),
    }
}

pub(in crate::eval) fn eval_selected_runtime_special_array_output<T: SimFloat>(
    base_name: &str,
    selection: &OutputSelection,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    if !selection.indices.is_empty() {
        return eval_selected_runtime_special_function(base_name, selection, args, env)
            .map(|value| value.map(|value| vec![value]));
    }
    match selection.output_name.as_str() {
        "stateOut" => Ok(random_result_and_state(base_name, args, env)?.map(|(_, state)| state)),
        "state" => initial_state_values(base_name, args, env),
        "result" => eval_selected_runtime_special_function(base_name, selection, args, env)
            .map(|value| value.map(|value| vec![value])),
        _ => Ok(None),
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
            | "allTrue"
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
            | "isValidTable"
            | "isEmpty"
            | "automaticGlobalSeed"
            | "automaticLocalSeed"
            | "initializeImpureRandom"
            | "impureRandom"
            | "impureRandomInteger"
            | "writeRealMatrix"
    ) || rumoca_core::modelica_string_intrinsic_short_name(short_name).is_some()
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
pub fn is_runtime_special_function_name(name: &rumoca_core::VarName) -> bool {
    is_runtime_special_function_short_name(name.last_segment())
        || is_runtime_special_function_qualified_name(name.as_str())
        || resolve_runtime_special_target(name.as_str()).is_some()
}

pub(super) fn eval_special_function_call<T: SimFloat>(
    name: &rumoca_core::VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    if let Some((resolved_name, Some(selection))) = resolve_runtime_special_target(name.as_str()) {
        return eval_selected_runtime_special_function(
            resolved_name.as_str(),
            &selection,
            args,
            env,
        );
    }
    if let Some(v) = eval_qualified_special_function(name.as_str(), args, env)? {
        return Ok(Some(v));
    }
    let short_name = name.last_segment();
    if let Some(v) = eval_misc_intrinsic_function(short_name, args, env)? {
        return Ok(Some(v));
    }
    if let Some(v) = eval_external_table_function(short_name, args, env)? {
        return Ok(Some(v));
    }
    if let Some(v) = eval_boolean_vector_function(short_name, args, env)? {
        return Ok(Some(v));
    }
    if short_name == "distribution"
        && let Some(v) = eval_distribution_function(args, env)?
    {
        return Ok(Some(v));
    }
    if let Some(v) = eval_clock_special_function(short_name, args, env)? {
        return Ok(Some(v));
    }
    if let Some(v) = eval_stream_special_function(short_name, args, env)? {
        return Ok(Some(v));
    }
    if let Some(v) = eval_state_accessor_special_function(short_name, args, env)? {
        return Ok(Some(v));
    }
    eval_unit_conversion_function(short_name, args, env)
}
