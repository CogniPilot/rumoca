use super::*;

/// Compatibility wrapper that clears cached `pre()` values in the process
/// default runtime.
///
/// New production callers should prefer [`clear_pre_values_in_env_runtime`] or
/// [`EvalRuntimeState::clear_pre_values`] so evaluator state stays scoped to a
/// request/session.
pub fn clear_pre_values() {
    default_runtime_state().clear_pre_values();
}

/// Clear cached `pre()` values in an explicit request-local runtime.
pub fn clear_pre_values_in_env_runtime<T: SimFloat>(env: &VarEnv<T>) {
    env.runtime.clear_pre_values();
}

/// Compatibility wrapper that clears all evaluator state in the process
/// default runtime.
///
/// New production callers should prefer [`clear_runtime_state_in_env_runtime`]
/// or [`EvalRuntimeState::clear_runtime_state`] so evaluator state stays scoped
/// to a request/session.
pub fn clear_runtime_state() {
    default_runtime_state().clear_runtime_state();
}

/// Reset all evaluator state in an explicit request-local runtime.
pub fn clear_runtime_state_in_env_runtime<T: SimFloat>(env: &VarEnv<T>) {
    env.runtime.clear_runtime_state();
}

/// Compatibility wrapper that snapshots `pre()` values from the process
/// default runtime. New production callers should prefer
/// [`snapshot_pre_values_from_env`] or [`snapshot_pre_values_from_runtime`].
pub fn snapshot_pre_values() -> IndexMap<String, f64> {
    snapshot_pre_values_in(default_runtime_state())
}

pub fn snapshot_pre_values_from_runtime(runtime: &EvalRuntimeState) -> IndexMap<String, f64> {
    snapshot_pre_values_in(runtime)
}

pub fn snapshot_pre_values_from_env<T: SimFloat>(env: &VarEnv<T>) -> IndexMap<String, f64> {
    snapshot_pre_values_in(&env.runtime)
}

pub(crate) fn snapshot_pre_values_in(runtime: &EvalRuntimeState) -> IndexMap<String, f64> {
    runtime
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .clone()
}

/// Compatibility wrapper that restores `pre()` values into the process default
/// runtime. New production callers should prefer
/// [`restore_pre_values_in_env_runtime`] or [`restore_pre_values_in_runtime`].
pub fn restore_pre_values(values: IndexMap<String, f64>) {
    *default_runtime_state()
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner()) = values;
}

pub fn restore_pre_values_in_runtime(runtime: &EvalRuntimeState, values: IndexMap<String, f64>) {
    *runtime
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner()) = values;
}

pub fn restore_pre_values_in_env_runtime<T: SimFloat>(
    env: &VarEnv<T>,
    values: IndexMap<String, f64>,
) {
    restore_pre_values_in_runtime(&env.runtime, values);
}

/// Seed `pre()` values into both the request-local runtime carried by `env` and
/// the process default runtime.
///
/// This remains for compatibility with older tests/callers that read `pre()`
/// through global wrappers. New production callers should prefer
/// [`seed_pre_values_in_env_runtime`].
pub fn seed_pre_values_from_env<T: SimFloat>(env: &VarEnv<T>) {
    seed_pre_values_in_runtime(&env.runtime, env);
    seed_pre_values_in_runtime(default_runtime_state(), env);
}

pub fn seed_pre_values_in_env_runtime<T: SimFloat>(env: &VarEnv<T>) {
    seed_pre_values_in_runtime(&env.runtime, env);
}

fn seed_pre_values_in_runtime<T: SimFloat>(runtime: &EvalRuntimeState, env: &VarEnv<T>) {
    let mut map = runtime
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let same_layout = map.len() == env.vars.len()
        && map
            .keys()
            .zip(env.vars.keys())
            .all(|(cached, current)| cached == current);
    if same_layout {
        for ((_, cached), (_, current)) in map.iter_mut().zip(env.vars.iter()) {
            *cached = current.real();
        }
        return;
    }

    map.clear();
    for (name, value) in &env.vars {
        map.insert(name.clone(), value.real());
    }
}

pub(crate) fn lookup_pre_value(name: &str) -> Option<f64> {
    lookup_pre_value_in(default_runtime_state(), name)
}

pub(crate) fn lookup_pre_value_in(runtime: &EvalRuntimeState, name: &str) -> Option<f64> {
    lookup_pre_value_in_runtime(runtime, name)
}

fn lookup_pre_value_in_runtime(runtime: &EvalRuntimeState, name: &str) -> Option<f64> {
    runtime
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .get(name)
        .copied()
}

/// Return the current cached `pre()` value for a scalar variable name.
///
/// Compatibility wrapper over the process default runtime. New production
/// callers should prefer [`get_pre_value_from_env`].
pub fn get_pre_value(name: &str) -> Option<f64> {
    lookup_pre_value(name)
}

pub fn get_pre_value_from_env<T: SimFloat>(env: &VarEnv<T>, name: &str) -> Option<f64> {
    lookup_pre_value_in(&env.runtime, name)
}

/// Override a single cached `pre()` value in the process default runtime.
///
/// New production callers should prefer [`set_pre_value_in_env`] or
/// [`set_pre_value_in_runtime`].
pub fn set_pre_value(name: &str, value: f64) {
    default_runtime_state()
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .insert(name.to_string(), value);
}

pub fn set_pre_value_in_runtime(runtime: &EvalRuntimeState, name: &str, value: f64) {
    runtime
        .pre_values
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .insert(name.to_string(), value);
}

pub fn set_pre_value_in_env<T: SimFloat>(env: &VarEnv<T>, name: &str, value: f64) {
    set_pre_value_in_runtime(&env.runtime, name, value);
}

pub fn build_env(dae: &Dae, y: &[f64], p: &[f64], t: f64) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    populate_env(dae, y, p, t, &mut env)?;
    Ok(env)
}

pub fn build_env_with_runtime(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    populate_env(dae, y, p, t, &mut env)?;
    Ok(env)
}

fn populate_env(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    env: &mut VarEnv<f64>,
) -> Result<(), EvalError> {
    check_runtime_vector_len("y", y.len(), solver_vector_len(dae))?;
    check_runtime_vector_len("p", p.len(), parameter_vector_len(dae))?;
    try_populate_env_values(dae, y, p, t, env)
}

pub fn build_runtime_parameter_tail_env(
    dae: &Dae,
    p: &[f64],
    t: f64,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    populate_runtime_parameter_tail_env(dae, p, t, &mut env)?;
    Ok(env)
}

pub fn build_runtime_parameter_tail_env_with_runtime(
    dae: &Dae,
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    populate_runtime_parameter_tail_env(dae, p, t, &mut env)?;
    Ok(env)
}

pub fn build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
    dae: &Dae,
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    check_runtime_vector_len("p", p.len(), declared_parameter_vector_len(dae))?;
    try_populate_runtime_parameter_tail_env_values_with_policy(
        dae,
        p,
        t,
        &mut env,
        ParameterSlotPolicy::Declared,
    )?;
    Ok(env)
}

fn populate_runtime_parameter_tail_env(
    dae: &Dae,
    p: &[f64],
    t: f64,
    env: &mut VarEnv<f64>,
) -> Result<(), EvalError> {
    try_populate_runtime_parameter_tail_env_values(dae, p, t, env)?;
    check_parameter_tail_bindings(dae, p.len(), env)
}

fn check_parameter_tail_bindings(
    dae: &Dae,
    actual_len: usize,
    env: &VarEnv<f64>,
) -> Result<(), EvalError> {
    let mut expected_len = 0usize;
    for (name, var) in &dae.variables.parameters {
        if !parameter_has_numeric_slot(var, env) {
            continue;
        }
        expected_len =
            expected_len
                .checked_add(var.size())
                .ok_or(EvalError::ShortRuntimeVector {
                    vector: "p",
                    expected: usize::MAX,
                    actual: actual_len,
                })?;
        if actual_len < expected_len && !var_binding_present(env, name.as_str(), var) {
            return Err(EvalError::ShortRuntimeVector {
                vector: "p",
                expected: expected_len,
                actual: actual_len,
            });
        }
    }
    Ok(())
}

fn var_binding_present(env: &VarEnv<f64>, name: &str, var: &dae::Variable) -> bool {
    let size = var.size();
    if size == 0 {
        return true;
    }
    if var.dims.is_empty() {
        return env.vars.contains_key(name);
    }
    (0..size).all(|index| {
        dae::flat_index_to_subscripts(&var.dims, index)
            .map(|subscripts| {
                env.vars
                    .contains_key(&dae::format_subscript_key(name, &subscripts))
            })
            .unwrap_or(false)
    })
}

pub fn refresh_env_solver_and_parameter_values(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<(), EvalError> {
    check_runtime_vector_len("y", y.len(), solver_vector_len(dae))?;
    check_runtime_vector_len("p", p.len(), parameter_vector_len(dae))?;
    refresh_env_solver_and_parameter_values_unchecked(env, dae, y, p, t);
    Ok(())
}

fn check_runtime_vector_len(
    vector: &'static str,
    actual: usize,
    expected: usize,
) -> Result<(), EvalError> {
    if actual < expected {
        return Err(EvalError::ShortRuntimeVector {
            vector,
            expected,
            actual,
        });
    }
    Ok(())
}

fn solver_vector_len(dae: &Dae) -> usize {
    dae.variables
        .states
        .values()
        .chain(dae.variables.algebraics.values())
        .chain(dae.variables.outputs.values())
        .map(dae::Variable::size)
        .sum()
}

fn parameter_vector_len(dae: &Dae) -> usize {
    let mut env = VarEnv::new();
    configure_env_metadata(&mut env, dae);
    dae.variables
        .parameters
        .values()
        .filter(|var| parameter_has_numeric_slot(var, &env))
        .map(dae::Variable::size)
        .sum()
}

fn declared_parameter_vector_len(dae: &Dae) -> usize {
    dae.variables
        .parameters
        .values()
        .map(dae::Variable::size)
        .sum()
}
