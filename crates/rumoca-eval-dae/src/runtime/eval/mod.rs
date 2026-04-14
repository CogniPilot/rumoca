//! Generic expression evaluator for dae::Expression trees.

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::runtime::sim_float::SimFloat;
use indexmap::IndexMap;
use rumoca_ir_dae as dae;

type Dae = dae::Dae;
type BuiltinFunction = dae::BuiltinFunction;
type Expression = dae::Expression;
type Function = dae::Function;
type FunctionParam = dae::FunctionParam;
type Literal = dae::Literal;
type OpBinary = rumoca_ir_core::OpBinary;
type Subscript = dae::Subscript;
type VarName = dae::VarName;

const MAX_FUNC_RECURSION: usize = 64;

mod external_table;
use external_table::{ExternalTableSpec, lookup_external_table, register_external_table};
mod pre_seed;
use pre_seed::try_seed_var_from_pre_store;
mod array_helpers;
mod builtin_table;
mod clock_eval;
mod distribution_clock;
use array_helpers::{
    array_values_from_env_name, array_values_from_env_name_generic, encoded_slice_field_values,
    eval_field_access_array_values, eval_unary_builtin_array_values, infer_dims_from_values,
};
use builtin_table::{eval_builtin_product, eval_builtin_sum};
#[cfg(any(feature = "cranelift", test))]
pub(crate) use builtin_table::{
    eval_table_bound_value, eval_table_lookup_slope_value, eval_table_lookup_value,
    eval_time_table_next_event_value,
};
pub use clock_eval::infer_clock_timing_seconds;
use clock_eval::{
    clock_tick_value, eval_builtin_sample, eval_time_seconds, infer_clock_timing_from_call,
    infer_clock_timing_from_expr,
};

macro_rules! warn_once {
    ($flag:expr, $($arg:tt)*) => {
        if !$flag.swap(true, Ordering::Relaxed) {
            eprintln!("WARNING: {}", format!($($arg)*));
        }
    };
}

mod special;
pub use special::{
    eval_builtin_pub, eval_condition_as_root, eval_function_call_pub, eval_function_call_pub_dae,
    eval_projected_function_output_pub, eval_projected_function_output_pub_dae,
    is_runtime_special_function_name, is_runtime_special_function_short_name,
    resolve_function_call_outputs_pub, resolve_function_call_outputs_pub_dae,
};
use special::{eval_function_call, eval_if};
mod eval_expr_impl;
pub use eval_expr_impl::eval_expr;
use eval_expr_impl::*;

pub const INIT_HOMOTOPY_LAMBDA_KEY: &str = "__rumoca_init_homotopy_lambda";

static WARNED_ARRAY_BUILTINS: AtomicBool = AtomicBool::new(false);
static WARNED_USER_FUNCTIONS: AtomicBool = AtomicBool::new(false);
static WARNED_TABLE_EXTRAPOLATION: AtomicBool = AtomicBool::new(false);
static WARNED_TABLE_INVALID_BOUNDS: AtomicBool = AtomicBool::new(false);

thread_local! {
    static FUNC_RECURSION_DEPTH: Cell<usize> = const { Cell::new(0) };
    static FUNC_CALL_STACK: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    static PRE_VALUES: RefCell<IndexMap<String, f64>> = RefCell::new(IndexMap::new());
}
#[derive(Debug, Clone)]
pub struct FunctionClosure {
    pub target_name: dae::VarName,
    pub bound_args: Vec<dae::Expression>,
}

#[derive(Debug, Clone)]
pub struct VarEnv<T: SimFloat = f64> {
    pub vars: IndexMap<String, T>,
    pub functions: Arc<IndexMap<String, dae::Function>>,
    pub dims: Arc<IndexMap<String, Vec<i64>>>,
    pub start_exprs: Arc<IndexMap<String, dae::Expression>>,
    pub clock_intervals: Arc<IndexMap<String, f64>>,
    pub enum_literal_ordinals: Arc<IndexMap<String, i64>>,
    pub function_closures: IndexMap<String, FunctionClosure>,
    pub is_initial: bool,
}

impl<T: SimFloat> Default for VarEnv<T> {
    fn default() -> Self {
        Self {
            vars: IndexMap::new(),
            functions: Arc::new(IndexMap::new()),
            dims: Arc::new(IndexMap::new()),
            start_exprs: Arc::new(IndexMap::new()),
            clock_intervals: Arc::new(IndexMap::new()),
            enum_literal_ordinals: Arc::new(IndexMap::new()),
            function_closures: IndexMap::new(),
            is_initial: false,
        }
    }
}

impl<T: SimFloat> VarEnv<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, name: &str) -> T {
        self.vars.get(name).copied().unwrap_or(T::zero())
    }

    pub fn set(&mut self, name: &str, value: T) {
        self.vars.insert(name.to_string(), value);
    }
}

/// Evaluate a lowered boolean condition with Modelica when-vector semantics.
///
/// MLS §8.3.5 permits vectorized when-conditions; Rumoca lowers
/// `when {c1, c2, ...}` into `Array` / `Tuple` guards that are active when any
/// listed condition is true.
pub fn eval_condition_truth<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> bool {
    match expr {
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| eval_condition_truth(element, env)),
        _ => eval_expr::<T>(expr, env).to_bool(),
    }
}

pub fn clear_pre_values() {
    PRE_VALUES.with(|values| values.borrow_mut().clear());
    distribution_clock::clear_clock_special_states();
}

/// Internal runtime environment key used to evaluate implicit `Clock()`
/// conditions inside guarded when-equations.
pub const IMPLICIT_CLOCK_ACTIVE_ENV_KEY: &str = "__rumoca_implicit_clock_active";

pub fn snapshot_pre_values() -> IndexMap<String, f64> {
    PRE_VALUES.with(|values| values.borrow().clone())
}

pub fn restore_pre_values(values: IndexMap<String, f64>) {
    PRE_VALUES.with(|store| {
        *store.borrow_mut() = values;
    });
}

pub fn seed_pre_values_from_env<T: SimFloat>(env: &VarEnv<T>) {
    PRE_VALUES.with(|values| {
        let mut map = values.borrow_mut();
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
    });
}

fn lookup_pre_value(name: &str) -> Option<f64> {
    PRE_VALUES.with(|values| values.borrow().get(name).copied())
}

pub(super) fn previous_start_or_default<T: SimFloat>(arg: &dae::Expression, env: &VarEnv<T>) -> T {
    let dae::Expression::VarRef { name, subscripts } = arg else {
        return T::zero();
    };

    let key = if subscripts.is_empty() {
        name.as_str().to_string()
    } else {
        let indices = eval_subscript_indices(subscripts, env);
        format!("{}[{}]", name.as_str(), indices.join(","))
    };

    if let Some(start) = env.start_exprs.get(key.as_str()) {
        return eval_expr::<T>(start, env);
    }
    if let Some(normalized) = normalize_var_name::<T>(&key, env)
        && let Some(start) = env.start_exprs.get(normalized.as_str())
    {
        return eval_expr::<T>(start, env);
    }
    if let Some(base_name) = unity_subscript_base_name(&key)
        && let Some(start) = env.start_exprs.get(base_name.as_str())
    {
        return eval_expr::<T>(start, env);
    }
    T::zero()
}

/// Return the current cached `pre()` value for a scalar variable name.
///
/// This is primarily used by the simulation runtime for event/discrete update
/// handling between solver iterations.
pub fn get_pre_value(name: &str) -> Option<f64> {
    lookup_pre_value(name)
}

/// Override a single cached `pre()` value.
pub fn set_pre_value(name: &str, value: f64) {
    PRE_VALUES.with(|values| {
        values.borrow_mut().insert(name.to_string(), value);
    });
}

fn lowered_pre_parameter_target(name: &str) -> Option<&str> {
    name.strip_prefix("__pre__.")
}

fn try_seed_lowered_pre_parameter_from_store(
    env: &mut VarEnv<f64>,
    name: &str,
    var: &rumoca_ir_dae::Variable,
) -> bool {
    // MLS §3.7.5 / pre-lowering: lowered `pre(x)` parameters must reflect the
    // event left-limit store, not the static runtime parameter vector.
    let Some(target_name) = lowered_pre_parameter_target(name) else {
        return false;
    };

    let size = var.size();
    if size <= 1 {
        let Some(value) = get_pre_value(target_name) else {
            return false;
        };
        if var.dims.is_empty() {
            env.set(name, value);
        } else {
            set_array_entries(env, name, &var.dims, &[value]);
        }
        return true;
    }

    let mut values = Vec::with_capacity(size);
    let mut found_any = false;
    for flat_idx in 0..size {
        let key = flat_index_to_subscripts(flat_idx, &var.dims)
            .map(|subs| format_multi_subscript_key(target_name, &subs))
            .unwrap_or_else(|| format!("{target_name}[{}]", flat_idx + 1));
        if let Some(value) = get_pre_value(&key) {
            values.push(value);
            found_any = true;
        } else {
            values.push(f64::NAN);
        }
    }

    let fallback =
        get_pre_value(target_name).or_else(|| values.iter().copied().find(|v| v.is_finite()));
    if !found_any && fallback.is_none() {
        return false;
    }

    let fill = fallback.unwrap_or(0.0);
    for value in &mut values {
        if !value.is_finite() {
            *value = fill;
        }
    }
    set_array_entries(env, name, &var.dims, &values);
    true
}

/// Map a variable (possibly array) into the environment from a value slice.
///
/// Scalar variables get a single entry. Array variables with `dims=[n]` get
/// entries for `name[1]` through `name[n]` (1-based Modelica indexing) as well
/// as `name` mapped to the first element (for aggregate references).
pub fn map_var_to_env<T: SimFloat>(
    env: &mut VarEnv<T>,
    name: &str,
    var: &rumoca_ir_dae::Variable,
    y: &[T],
    idx: &mut usize,
) {
    let sz = var.size();
    if sz == 0 {
        // MLS Chapter 10 dynamic arrays may materialize only in the env. A
        // zero-sized declaration must not consume a flattened solver/parameter
        // slot, or every later runtime binding shifts out of alignment.
        return;
    }
    if sz <= 1 {
        if *idx < y.len() {
            if var.dims.is_empty() {
                env.set(name, y[*idx]);
            } else {
                set_array_entries(env, name, &var.dims, &[y[*idx]]);
            }
        }
        *idx += 1;
    } else {
        let start = *idx;
        let mut vals = Vec::with_capacity(sz);
        for i in 0..sz {
            if start + i < y.len() {
                vals.push(y[start + i]);
            }
        }
        set_array_entries(env, name, &var.dims, &vals);
        *idx += sz;
    }
}

fn flat_index_to_subscripts(flat_idx: usize, dims: &[i64]) -> Option<Vec<usize>> {
    if dims.is_empty() {
        return None;
    }
    let mut dims_usize = Vec::with_capacity(dims.len());
    for &d in dims {
        let du = usize::try_from(d).ok()?;
        if du == 0 {
            return None;
        }
        dims_usize.push(du);
    }

    let mut idx = flat_idx;
    let mut subs_rev = Vec::with_capacity(dims_usize.len());
    for &dim in dims_usize.iter().rev() {
        subs_rev.push((idx % dim) + 1);
        idx /= dim;
    }
    if idx != 0 {
        return None;
    }
    subs_rev.reverse();
    Some(subs_rev)
}

fn format_multi_subscript_key(name: &str, subs: &[usize]) -> String {
    let mut key = String::from(name);
    key.push('[');
    for (i, s) in subs.iter().enumerate() {
        if i > 0 {
            key.push(',');
        }
        key.push_str(&s.to_string());
    }
    key.push(']');
    key
}

/// Map flattened array values into scalar and subscripted entries.
///
/// Writes:
/// - `name` = first element
/// - `name[i]` for flat 1-based index
/// - `name[i,j,...]` when dimensions are available
pub fn set_array_entries<T: SimFloat>(env: &mut VarEnv<T>, name: &str, dims: &[i64], values: &[T]) {
    let Some(&first) = values.first() else { return };
    env.set(name, first);
    for (i, &v) in values.iter().enumerate() {
        env.set(&format!("{name}[{}]", i + 1), v);
        if let Some(subs) = flat_index_to_subscripts(i, dims)
            && subs.len() > 1
        {
            env.set(&format_multi_subscript_key(name, &subs), v);
        }
    }
}

/// Well-known Modelica standard library constants (MLS Appendix A).
///
/// These are injected into the environment as fallbacks when not already
/// provided by the DAE constant declarations.
pub const MODELICA_CONSTANTS: &[(&str, f64)] = &[
    ("Modelica.Constants.pi", std::f64::consts::PI),
    ("Modelica.Constants.e", std::f64::consts::E),
    ("Modelica.Constants.g_n", 9.80665),
    ("Modelica.Constants.small", 1e-60),
    ("Modelica.Constants.eps", f64::EPSILON),
    ("Modelica.Constants.inf", f64::INFINITY),
    ("Modelica.Constants.sigma", 5.670374419e-8),
    ("Modelica.Constants.R", 8.314462618),
    ("Modelica.Constants.N_A", 6.02214076e23),
    ("Modelica.Constants.k", 1.380649e-23),
    ("Modelica.Constants.q", 1.602176634e-19),
    ("Modelica.Constants.h", 6.62607015e-34),
    ("Modelica.Constants.c", 299792458.0),
    ("Modelica.Constants.F", 96485.33212),
    ("Modelica.Constants.mu_0", 1.25663706212e-6),
    ("Modelica.Constants.epsilon_0", 8.8541878128e-12),
    ("Modelica.Constants.T_zero", -273.15),
];

pub const MODELICA_COMPLEX_CONSTANTS: &[(&str, f64)] = &[
    // Modelica.ComplexMath.j = Complex(0, 1)
    ("Modelica.ComplexMath.j.re", 0.0),
    ("Modelica.ComplexMath.j.im", 1.0),
    // Imported alias inside function scopes (`import Modelica.ComplexMath.j;`)
    ("j.re", 0.0),
    ("j.im", 1.0),
];

/// Build a variable environment from the DAE and current state vector (f64-only).
///
/// The combined state vector `y` is `[x; z; y_out]` where x = states,
/// z = algebraics, y_out = outputs. `p` contains parameter values.
pub fn build_env(dae: &Dae, y: &[f64], p: &[f64], t: f64) -> VarEnv<f64> {
    let mut env = VarEnv::new();
    env.set("time", t);

    map_solver_vectors_into_env(&mut env, dae, y);
    populate_runtime_parameter_tail(&mut env, dae, p);

    env
}

/// Build only the parameter/input/discrete runtime tail for a DAE env.
///
/// This excludes solver-vector slots (`states`, `algebraics`, `outputs`) and
/// is intended for callers that only need runtime tail bindings such as input
/// or discrete start values.
pub fn build_runtime_parameter_tail_env(dae: &Dae, p: &[f64], t: f64) -> VarEnv<f64> {
    let mut env = VarEnv::new();
    env.set("time", t);
    populate_runtime_parameter_tail(&mut env, dae, p);
    env
}

fn populate_runtime_parameter_tail(env: &mut VarEnv<f64>, dae: &Dae, p: &[f64]) {
    map_parameter_vector_into_env(env, dae, p);
    configure_env_metadata(env, dae);
    bind_missing_parameter_values(env, dae);
    inject_modelica_constants(env);
    bind_constants_and_inputs(env, dae);
    seed_discrete_values(env, dae);
}

/// Refresh solver- and parameter-backed runtime slots in an existing env.
///
/// This preserves already-settled discrete/runtime tail values while updating
/// the current solver state, parameters, and time.
pub fn refresh_env_solver_and_parameter_values(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
) {
    env.set("time", t);
    map_solver_vectors_into_env(env, dae, y);
    map_parameter_vector_into_env(env, dae, p);
}

fn map_solver_vectors_into_env(env: &mut VarEnv<f64>, dae: &Dae, y: &[f64]) {
    let mut idx = 0;
    for (name, var) in &dae.states {
        map_var_to_env(env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae.algebraics {
        map_var_to_env(env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae.outputs {
        map_var_to_env(env, name.as_str(), var, y, &mut idx);
    }
}

fn map_parameter_vector_into_env(env: &mut VarEnv<f64>, dae: &Dae, p: &[f64]) {
    let mut pidx = 0;
    for (name, var) in &dae.parameters {
        map_var_to_env(env, name.as_str(), var, p, &mut pidx);
        let _ = try_seed_lowered_pre_parameter_from_store(env, name.as_str(), var);
    }
}

fn configure_env_metadata(env: &mut VarEnv<f64>, dae: &Dae) {
    if !dae.functions.is_empty() {
        let func_map: IndexMap<String, dae::Function> = dae
            .functions
            .iter()
            .map(|(name, func)| (name.as_str().to_string(), func.clone()))
            .collect();
        env.functions = Arc::new(func_map);
    }
    env.dims = Arc::new(collect_var_dims(dae));
    env.start_exprs = Arc::new(collect_var_starts(dae));
    env.clock_intervals = Arc::new(dae.clock_intervals.clone());
    env.enum_literal_ordinals = Arc::new(dae.enum_literal_ordinals.clone());
}

fn inject_modelica_constants(env: &mut VarEnv<f64>) {
    for &(fqn, value) in MODELICA_CONSTANTS {
        if !env.vars.contains_key(fqn) {
            env.set(fqn, value);
        }
    }
    for &(fqn, value) in MODELICA_COMPLEX_CONSTANTS {
        if !env.vars.contains_key(fqn) {
            env.set(fqn, value);
        }
    }
}

fn bind_start_value(env: &mut VarEnv<f64>, name: &str, var: &rumoca_ir_dae::Variable) {
    let Some(start) = var.start.as_ref() else {
        return;
    };
    let size = var.size();
    if size <= 1 {
        let value = eval_expr::<f64>(start, env);
        if var.dims.is_empty() {
            env.set(name, value);
        } else {
            set_array_entries(env, name, &var.dims, &[value]);
        }
        return;
    }

    let raw_values: Vec<f64> = eval_array_values::<f64>(start, env);
    let values = if raw_values.len() == size {
        raw_values
    } else if raw_values.is_empty() {
        vec![0.0; size]
    } else if raw_values.len() == 1 {
        vec![raw_values[0]; size]
    } else {
        let last = *raw_values.last().unwrap_or(&0.0);
        let mut expanded = Vec::with_capacity(size);
        for i in 0..size {
            expanded.push(raw_values.get(i).copied().unwrap_or(last));
        }
        expanded
    };
    if !values.is_empty() {
        set_array_entries(env, name, &var.dims, &values);
    }
}

fn bind_constants_and_inputs(env: &mut VarEnv<f64>, dae: &Dae) {
    for _ in 0..2 {
        for (name, var) in &dae.constants {
            bind_start_value(env, name.as_str(), var);
        }
    }
    for _ in 0..2 {
        for (name, var) in &dae.inputs {
            bind_start_value(env, name.as_str(), var);
        }
    }
}

fn bind_missing_parameter_values(env: &mut VarEnv<f64>, dae: &Dae) {
    let max_passes = dae.parameters.len().clamp(1, 8);
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, var) in &dae.parameters {
            if env.vars.contains_key(name.as_str()) {
                continue;
            }
            let before_len = env.vars.len();
            bind_start_value(env, name.as_str(), var);
            changed |= env.vars.len() != before_len || env.vars.contains_key(name.as_str());
        }
        if !changed {
            break;
        }
    }
}

fn seed_discrete_values(env: &mut VarEnv<f64>, dae: &Dae) {
    let mut pre_seeded: HashSet<String> = HashSet::new();
    for (name, var) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
        if env.vars.contains_key(name.as_str()) {
            continue;
        }
        if try_seed_var_from_pre_store(env, name.as_str(), var) {
            pre_seeded.insert(name.as_str().to_string());
        }
    }

    for _ in 0..2 {
        for (name, var) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
            if pre_seeded.contains(name.as_str()) {
                continue;
            }
            bind_start_value(env, name.as_str(), var);
        }
    }

    for (name, var) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
        if env.vars.contains_key(name.as_str()) {
            continue;
        }
        let size = var.size();
        if size <= 1 {
            env.set(name.as_str(), 0.0);
            continue;
        }
        let zeros = vec![0.0; size];
        set_array_entries(env, name.as_str(), &var.dims, zeros.as_slice());
    }
}

/// Collect variable dimensions from all variable categories in the DAE.
pub fn collect_var_dims(dae: &Dae) -> IndexMap<String, Vec<i64>> {
    let mut map = IndexMap::new();
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .chain(dae.parameters.iter())
        .chain(dae.constants.iter())
        .chain(dae.inputs.iter())
        .chain(dae.discrete_reals.iter())
        .chain(dae.discrete_valued.iter())
    {
        if !var.dims.is_empty() {
            map.insert(name.as_str().to_string(), var.dims.clone());
        }
    }
    map
}

/// Collect start expressions from all variable categories in the DAE.
pub fn collect_var_starts(dae: &Dae) -> IndexMap<String, dae::Expression> {
    let mut map = IndexMap::new();
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .chain(dae.parameters.iter())
        .chain(dae.constants.iter())
        .chain(dae.inputs.iter())
        .chain(dae.discrete_reals.iter())
        .chain(dae.discrete_valued.iter())
    {
        if let Some(start) = &var.start {
            map.insert(name.as_str().to_string(), start.clone());
        }
    }
    map
}

/// Collect lowered user function bodies for runtime function calls.
pub fn collect_user_functions(dae: &Dae) -> IndexMap<String, dae::Function> {
    dae.functions
        .iter()
        .map(|(name, func)| (name.as_str().to_string(), func.clone()))
        .collect()
}

/// Lift an f64 environment to a generic SimFloat environment.
///
/// All values are converted with `T::from_f64()`, so dual parts start at 0.
/// dae::Function definitions are shared via Arc (zero-copy).
pub fn lift_env<T: SimFloat>(env: &VarEnv<f64>) -> VarEnv<T> {
    let mut result = VarEnv::new();
    for (name, &val) in &env.vars {
        result.vars.insert(name.clone(), T::from_f64(val));
    }
    result.functions = env.functions.clone();
    result.dims = env.dims.clone();
    result.start_exprs = env.start_exprs.clone();
    result.clock_intervals = env.clock_intervals.clone();
    result.enum_literal_ordinals = env.enum_literal_ordinals.clone();
    result.is_initial = env.is_initial;
    result
}

/// Evaluate a constant expression (for start values, parameter defaults).
pub fn eval_const_expr(expr: &dae::Expression) -> f64 {
    eval_expr::<f64>(expr, &VarEnv::new())
}

/// Evaluate a DAE expression by converting it to flat IR first.
pub fn eval_expr_dae<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> T {
    eval_expr::<T>(expr, env)
}

/// Evaluate a constant DAE expression.
pub fn eval_const_expr_dae(expr: &dae::Expression) -> f64 {
    eval_expr_dae::<f64>(expr, &VarEnv::new())
}

/// Evaluate a DAE expression as a flattened array of scalar values.
pub fn eval_array_values_dae<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> Vec<T> {
    eval_array_values::<T>(expr, env)
}

/// Infer a periodic clock timing from a DAE expression.
pub fn infer_clock_timing_seconds_dae(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<(f64, f64)> {
    infer_clock_timing_seconds(expr, env)
}

/// Evaluate a DAE root condition expression into a signed root value.
pub fn eval_condition_as_root_dae(expr: &dae::Expression, env: &VarEnv<f64>) -> f64 {
    eval_condition_as_root(expr, env)
}

/// Evaluate an expression as a flattened array of scalar values.
///
/// Nested array literals are flattened recursively; scalar expressions produce
/// a single-element vector.
pub fn eval_array_values<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> Vec<T> {
    let mut out = Vec::new();
    collect_array_values(expr, env, &mut out);
    out
}

fn collect_array_values<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>, out: &mut Vec<T>) {
    match expr {
        dae::Expression::Array {
            elements,
            is_matrix,
        } => collect_array_literal_values(elements, *is_matrix, env, out),
        dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_array_values(element, env, out);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => collect_if_values(branches, else_branch, env, out),
        dae::Expression::Range { start, step, end } => {
            collect_range_values(start, step, end, env, out);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => collect_array_comprehension_values(expr, indices, filter.as_deref(), env, out),
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                out.extend(values);
            } else {
                out.push(eval_expr::<T>(expr, env));
            }
        }
        dae::Expression::FieldAccess { base, field } => {
            if let Some(values) = eval_field_access_array_values(base, field, env) {
                out.extend(values);
            } else {
                out.push(eval_expr::<T>(expr, env));
            }
        }
        _ => out.push(eval_expr::<T>(expr, env)),
    }
}

fn collect_array_literal_values<T: SimFloat>(
    elements: &[dae::Expression],
    is_matrix: bool,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    if can_interleave_matrix_columns(elements, is_matrix)
        && interleave_matrix_columns(elements, env, out)
    {
        return;
    }
    for element in elements {
        collect_array_values(element, env, out);
    }
}

fn can_interleave_matrix_columns(elements: &[dae::Expression], is_matrix: bool) -> bool {
    is_matrix
        && !elements.is_empty()
        && !elements
            .iter()
            .all(|element| matches!(element, dae::Expression::Array { .. }))
}

fn interleave_matrix_columns<T: SimFloat>(
    elements: &[dae::Expression],
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) -> bool {
    let columns: Vec<Vec<T>> = elements
        .iter()
        .map(|element| eval_array_like_values::<T>(element, env))
        .collect();
    let row_count = columns.iter().map(Vec::len).max().unwrap_or(0);
    if row_count == 0 {
        return false;
    }

    for row in 0..row_count {
        for column in &columns {
            out.push(interleaved_column_value(column, row));
        }
    }
    true
}

fn interleaved_column_value<T: SimFloat>(column: &[T], row: usize) -> T {
    if column.is_empty() {
        return T::zero();
    }
    if row < column.len() {
        return column[row];
    }
    if column.len() == 1 {
        return column[0];
    }
    *column.last().unwrap_or(&T::zero())
}

fn collect_if_values<T: SimFloat>(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    for (cond, then_expr) in branches {
        if eval_condition_truth(cond, env) {
            collect_array_values(then_expr, env, out);
            return;
        }
    }
    collect_array_values(else_branch, env, out);
}

fn collect_range_values<T: SimFloat>(
    start: &dae::Expression,
    step: &Option<Box<dae::Expression>>,
    end: &dae::Expression,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    let start_v = eval_expr::<T>(start, env).real();
    let end_v = eval_expr::<T>(end, env).real();
    let step_v = step
        .as_ref()
        .map(|step_expr| eval_expr::<T>(step_expr, env).real())
        .unwrap_or_else(|| if end_v >= start_v { 1.0 } else { -1.0 });
    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return;
    }
    extend_range_values(start_v, end_v, step_v, out);
}

fn extend_range_values<T: SimFloat>(start_v: f64, end_v: f64, step_v: f64, out: &mut Vec<T>) {
    let limit = 100_000usize;
    let tol = step_v.abs() * 1e-9 + 1e-12;
    let mut value = start_v;
    for _ in 0..limit {
        let past_end =
            (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
        if past_end {
            return;
        }
        out.push(T::from_f64(value));
        value += step_v;
    }
}

fn collect_array_comprehension_values<T: SimFloat>(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    expand_array_comprehension(0, expr, indices, filter, env, out);
}

fn expand_array_comprehension<T: SimFloat>(
    level: usize,
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    env: &VarEnv<T>,
    out: &mut Vec<T>,
) {
    if level >= indices.len() {
        if filter.is_none_or(|f| eval_expr::<T>(f, env).to_bool()) {
            collect_array_values(expr, env, out);
        }
        return;
    }

    let index = &indices[level];
    for value in eval_array_values::<T>(&index.range, env) {
        let mut local_env = env.clone();
        local_env.set(index.name.as_str(), value);
        expand_array_comprehension(level + 1, expr, indices, filter, &local_env, out);
    }
}

fn reshape_flat_matrix(flat_values: &[f64], rows: usize, cols: usize) -> Vec<Vec<f64>> {
    let mut matrix = Vec::with_capacity(rows);
    for r in 0..rows {
        let start = r.saturating_mul(cols).min(flat_values.len());
        let end = start.saturating_add(cols).min(flat_values.len());
        let mut row = flat_values[start..end].to_vec();
        row.resize(cols, 0.0);
        matrix.push(row);
    }
    matrix
}

fn eval_array_like_values<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> Vec<T> {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if let Some(values) = encoded_slice_field_values(name.as_str(), env) {
                return values;
            }
            if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                return values;
            }
            vec![eval_expr::<T>(expr, env)]
        }
        dae::Expression::FieldAccess { base, field } => {
            if let Some(values) = eval_field_access_array_values(base, field, env) {
                return values;
            }
            vec![eval_expr::<T>(expr, env)]
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Cat,
            args,
        } => eval_cat_values(args, env),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Linspace,
            args,
        } => eval_linspace_values(args, env),
        dae::Expression::BuiltinCall { function, args } if args.len() == 1 => {
            let values = eval_array_like_values::<T>(&args[0], env);
            if values.len() > 1
                && let Some(mapped) = eval_unary_builtin_array_values(*function, values)
            {
                return mapped;
            }
            vec![eval_expr::<T>(expr, env)]
        }
        dae::Expression::Array { .. }
        | dae::Expression::Tuple { .. }
        | dae::Expression::Range { .. }
        | dae::Expression::If { .. }
        | dae::Expression::ArrayComprehension { .. } => eval_array_values::<T>(expr, env),
        _ => vec![eval_expr::<T>(expr, env)],
    }
}

fn eval_cat_values<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> Vec<T> {
    // cat(dim, A, B, ...)
    if args.len() <= 1 {
        return Vec::new();
    }

    let mut out = Vec::new();
    for arg in args.iter().skip(1) {
        out.extend(eval_array_like_values(arg, env));
    }
    out
}

fn eval_linspace_values<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> Vec<T> {
    if args.len() != 3 {
        return Vec::new();
    }
    let start = eval_expr::<T>(&args[0], env).real();
    let end = eval_expr::<T>(&args[1], env).real();
    let n = eval_expr::<T>(&args[2], env).real().round() as i64;
    if n < 2 {
        return Vec::new();
    }
    let n_usize = n as usize;
    let step = (end - start) / ((n_usize - 1) as f64);
    let mut out: Vec<T> = (0..n_usize)
        .map(|i| T::from_f64(start + step * i as f64))
        .collect();
    if let Some(last) = out.last_mut() {
        *last = T::from_f64(end);
    }
    out
}

fn eval_array_like_f64_values<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> Vec<f64> {
    eval_array_like_values(expr, env)
        .into_iter()
        .map(|v| v.real())
        .collect()
}

fn eval_cat_f64_values<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> Vec<f64> {
    eval_cat_values(args, env)
        .into_iter()
        .map(|v| v.real())
        .collect()
}

fn eval_columns_arg<T: SimFloat>(expr: Option<&dae::Expression>, env: &VarEnv<T>) -> Vec<usize> {
    let Some(expr) = expr else { return Vec::new() };
    eval_array_like_f64_values(expr, env)
        .into_iter()
        .map(|v| v.round() as i64)
        .filter(|v| *v > 0)
        .map(|v| v as usize)
        .collect()
}

fn eval_table_matrix_arg<T: SimFloat>(
    expr: &dae::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<f64>>> {
    match expr {
        dae::Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return Some(Vec::new());
            }

            if elements
                .iter()
                .all(|e| matches!(e, dae::Expression::Array { .. }))
            {
                let mut rows = Vec::with_capacity(elements.len());
                for row_expr in elements {
                    let row_vals = eval_array_values::<T>(row_expr, env)
                        .iter()
                        .map(|v| v.real())
                        .collect::<Vec<_>>();
                    rows.push(row_vals);
                }
                return Some(rows);
            }

            let values = eval_array_values::<T>(expr, env);
            if values.is_empty() {
                return Some(Vec::new());
            }
            Some(vec![values.iter().map(|v| v.real()).collect()])
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            let flat_values = array_values_from_env_name(name.as_str(), env)?;
            if flat_values.is_empty() {
                return Some(Vec::new());
            }
            let raw_dims = env.dims.get(name.as_str()).cloned().unwrap_or_default();
            let inferred = infer_dims_from_values(&raw_dims, flat_values.len());
            if inferred.len() >= 2 {
                let rows = inferred[0].max(1);
                let cols = inferred[1].max(1);
                let matrix = reshape_flat_matrix(&flat_values, rows, cols);
                return Some(matrix);
            }
            Some(vec![flat_values])
        }
        _ => None,
    }
}

fn map_selected_table_column(
    columns: &[usize],
    requested_output_col: usize,
    data_col_count: usize,
) -> usize {
    if data_col_count == 0 {
        return 0;
    }
    if columns.is_empty() {
        // No explicit mapping: first data column after abscissa is output 1.
        return requested_output_col
            .saturating_add(1)
            .min(data_col_count.saturating_sub(1));
    }
    let mapped = columns
        .get(requested_output_col)
        .copied()
        .or_else(|| columns.last().copied())
        .unwrap_or(1);
    mapped
        .saturating_sub(1)
        .min(data_col_count.saturating_sub(1))
}

fn table_x_bounds(spec: &ExternalTableSpec) -> Option<(f64, f64)> {
    let first = spec.data.first()?.first().copied()?;
    let last = spec.data.last()?.first().copied()?;
    Some((first, last))
}

fn apply_extrapolation_policy(
    mut x: f64,
    x_min: f64,
    x_max: f64,
    extrapolation: i64,
) -> (f64, bool, bool) {
    if !x_min.is_finite() || !x_max.is_finite() || x_min > x_max {
        warn_once!(
            WARNED_TABLE_INVALID_BOUNDS,
            "Invalid table bounds [{x_min}, {x_max}] during lookup; keeping input value."
        );
        return (x, false, true);
    }
    if x_min == x_max {
        return (x_min, false, false);
    }
    if x >= x_min && x <= x_max {
        return (x, false, true);
    }
    match extrapolation {
        // HoldLastPoint
        1 => {
            x = x.clamp(x_min, x_max);
            (x, true, false)
        }
        // LastTwoPoints (linear extrapolation) - preserve out-of-range x
        2 => (x, true, true),
        // Periodic
        3 => {
            let span = x_max - x_min;
            if span > 0.0 {
                let mut wrapped = (x - x_min) % span;
                if wrapped < 0.0 {
                    wrapped += span;
                }
                x = x_min + wrapped;
            } else {
                x = x_min;
            }
            (x, false, true)
        }
        // NoExtrapolation: clamp to avoid NaN poisoning, warn once.
        4 => {
            warn_once!(
                WARNED_TABLE_EXTRAPOLATION,
                "NoExtrapolation requested for table lookup; clamping to table bounds."
            );
            x = x.clamp(x_min, x_max);
            (x, true, false)
        }
        _ => (x.clamp(x_min, x_max), true, false),
    }
}

fn eval_table_1d_lookup<T: SimFloat>(
    spec: &ExternalTableSpec,
    requested_output_col: usize,
    x: T,
) -> T {
    if spec.data.is_empty() {
        return T::zero();
    }
    let data_col_count = spec.data.first().map(|r| r.len()).unwrap_or(0);
    if data_col_count < 2 {
        return T::zero();
    }

    let output_col = map_selected_table_column(&spec.columns, requested_output_col, data_col_count);
    let (x_min, x_max) = match table_x_bounds(spec) {
        Some(bounds) => bounds,
        None => return T::zero(),
    };

    let (x_real, out_of_range, preserve_dual_x) =
        apply_extrapolation_policy(x.real(), x_min, x_max, spec.extrapolation);
    let x_eval = if preserve_dual_x {
        // Keep AD slope d(x_eval)/d(x) = 1 while shifting real part as needed (e.g. periodic wrap).
        x + T::from_f64(x_real - x.real())
    } else {
        // Clamped/held extrapolation should not propagate slope outside table range.
        T::from_f64(x_real)
    };
    if spec.data.len() == 1 {
        return T::from_f64(spec.data[0][output_col]);
    }

    // Choose interpolation interval robustly at/near boundaries:
    // - x <= first abscissa: use first segment
    // - x >= last abscissa: use last segment (enables linear extrapolation semantics)
    // - otherwise: first segment where x < x_{k+1}
    let last_idx = spec.data.len() - 1;
    let k = if x_real <= spec.data[0][0] {
        0usize
    } else if x_real >= spec.data[last_idx][0] {
        last_idx.saturating_sub(1)
    } else {
        let mut idx = 0usize;
        while idx + 1 < spec.data.len() && x_real >= spec.data[idx + 1][0] {
            idx += 1;
        }
        idx.min(last_idx.saturating_sub(1))
    };

    let x0 = spec.data[k][0];
    let x1 = spec.data[k + 1][0];
    let y0 = spec.data[k][output_col];
    let y1 = spec.data[k + 1][output_col];

    // Constant segments per Modelica.Blocks.Types.Smoothness.ConstantSegments.
    if spec.smoothness == 3 && !out_of_range {
        if x_real >= x_max {
            return T::from_f64(spec.data[last_idx][output_col]);
        }
        return T::from_f64(y0);
    }

    if (x1 - x0).abs() <= f64::EPSILON {
        return T::from_f64(y0);
    }

    let alpha = (x_eval - T::from_f64(x0)) / T::from_f64(x1 - x0);
    T::from_f64(y0) + alpha * T::from_f64(y1 - y0)
}

fn eval_table_constructor<T: SimFloat>(
    args: &[dae::Expression],
    env: &VarEnv<T>,
    is_time_table: bool,
) -> Option<T> {
    let table_arg_idx = 2usize;
    let columns_arg_idx = if is_time_table { 4 } else { 3 };
    let smoothness_idx = if is_time_table { 5 } else { 4 };
    let extrapolation_idx = if is_time_table { 6 } else { 5 };

    let table_matrix = eval_table_matrix_arg(args.get(table_arg_idx)?, env)?;
    if table_matrix.is_empty() {
        return Some(T::from_f64(0.0));
    }

    let columns = eval_columns_arg(args.get(columns_arg_idx), env);
    let smoothness = args
        .get(smoothness_idx)
        .map(|e| eval_expr::<T>(e, env).real().round() as i64)
        .unwrap_or(1);
    let extrapolation = args
        .get(extrapolation_idx)
        .map(|e| eval_expr::<T>(e, env).real().round() as i64)
        .unwrap_or(1);

    let spec = ExternalTableSpec {
        data: table_matrix,
        columns,
        smoothness,
        extrapolation,
    };
    let id = register_external_table(spec);
    Some(T::from_f64(id as f64))
}

#[cfg(test)]
mod tests;
