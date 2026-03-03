//! Generic expression evaluator for flat::Expression trees.

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::sim_float::SimFloat;
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

type Dae = dae::Dae;
type BuiltinFunction = flat::BuiltinFunction;
type Expression = flat::Expression;
type Function = flat::Function;
type FunctionParam = flat::FunctionParam;
type Literal = flat::Literal;
type OpBinary = flat::OpBinary;
type Subscript = flat::Subscript;
type VarName = flat::VarName;

const MAX_FUNC_RECURSION: usize = 64;

mod external_table;
use external_table::{ExternalTableSpec, lookup_external_table, register_external_table};
mod pre_seed;
use pre_seed::try_seed_var_from_pre_store;
mod array_helpers;
mod builtin_table;
mod distribution_clock;
use array_helpers::{
    array_values_from_env_name, array_values_from_env_name_generic, encoded_slice_field_values,
    eval_field_access_array_values, eval_unary_builtin_array_values, infer_dims_from_values,
};
use builtin_table::{eval_builtin_product, eval_builtin_sum};

macro_rules! warn_once {
    ($flag:expr, $($arg:tt)*) => {
        if !$flag.swap(true, Ordering::Relaxed) {
            eprintln!("WARNING: {}", format!($($arg)*));
        }
    };
}

mod special;
pub use special::{
    eval_builtin_pub, eval_condition_as_root, eval_function_call_pub,
    is_runtime_special_function_name, is_runtime_special_function_short_name,
    resolve_function_call_outputs_pub,
};
use special::{eval_function_call, eval_if};

static WARNED_ARRAY_BUILTINS: AtomicBool = AtomicBool::new(false);
static WARNED_USER_FUNCTIONS: AtomicBool = AtomicBool::new(false);
static WARNED_TABLE_EXTRAPOLATION: AtomicBool = AtomicBool::new(false);
static WARNED_TABLE_INVALID_BOUNDS: AtomicBool = AtomicBool::new(false);

thread_local! {
    static FUNC_RECURSION_DEPTH: Cell<usize> = const { Cell::new(0) };
    static FUNC_CALL_STACK: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    static PRE_VALUES: RefCell<HashMap<String, f64>> = RefCell::new(HashMap::new());
}
#[derive(Debug, Clone)]
pub struct FunctionClosure {
    pub target_name: flat::VarName,
    pub bound_args: Vec<flat::Expression>,
}

#[derive(Debug, Clone)]
pub struct VarEnv<T: SimFloat = f64> {
    pub vars: IndexMap<String, T>,
    pub functions: Arc<IndexMap<String, flat::Function>>,
    pub dims: Arc<IndexMap<String, Vec<i64>>>,
    pub start_exprs: Arc<IndexMap<String, flat::Expression>>,
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

pub fn clear_pre_values() {
    PRE_VALUES.with(|values| values.borrow_mut().clear());
    distribution_clock::clear_clock_special_states();
}

/// Internal runtime environment key used to evaluate implicit `Clock()`
/// conditions inside guarded when-equations.
pub const IMPLICIT_CLOCK_ACTIVE_ENV_KEY: &str = "__rumoca_implicit_clock_active";

pub fn snapshot_pre_values() -> HashMap<String, f64> {
    PRE_VALUES.with(|values| values.borrow().clone())
}

pub fn restore_pre_values(values: HashMap<String, f64>) {
    PRE_VALUES.with(|store| {
        *store.borrow_mut() = values;
    });
}

pub fn seed_pre_values_from_env<T: SimFloat>(env: &VarEnv<T>) {
    PRE_VALUES.with(|values| {
        let mut map = values.borrow_mut();
        map.clear();
        for (name, value) in &env.vars {
            map.insert(name.clone(), value.real());
        }
    });
}

fn lookup_pre_value(name: &str) -> Option<f64> {
    PRE_VALUES.with(|values| values.borrow().get(name).copied())
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

    let mut idx = 0;
    for (name, var) in &dae.states {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae.algebraics {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae.outputs {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }

    // Map parameters from p (also handle arrays)
    let mut pidx = 0;
    for (name, var) in &dae.parameters {
        map_var_to_env(&mut env, name.as_str(), var, p, &mut pidx);
    }

    // Share user-defined function table for function body evaluation.
    // This must be available before evaluating constant/input start expressions.
    if !dae.functions.is_empty() {
        let func_map: IndexMap<String, flat::Function> = dae
            .functions
            .iter()
            .map(|(name, func)| (name.as_str().to_string(), func.clone()))
            .collect();
        env.functions = Arc::new(func_map);
    }

    // Populate dimension info for Size builtin
    env.dims = Arc::new(collect_var_dims(dae));
    env.start_exprs = Arc::new(collect_var_starts(dae));
    env.enum_literal_ordinals = Arc::new(dae.enum_literal_ordinals.clone());

    // Inject well-known Modelica constants as fallbacks.
    // Only set if not already provided by the DAE constant declarations.
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

    let bind_start_value = |env: &mut VarEnv<f64>, name: &str, var: &rumoca_ir_dae::Variable| {
        let Some(start) = var.start.as_ref() else {
            return;
        };
        let sz = var.size();
        if sz <= 1 {
            env.set(name, eval_expr::<f64>(start, env));
            return;
        }

        // Array-valued starts can come from literals, references, conditionals,
        // or other expressions; always evaluate through the array evaluator.
        let raw_vals: Vec<f64> = eval_array_values::<f64>(start, env);
        let vals = if raw_vals.len() == sz {
            raw_vals
        } else if raw_vals.is_empty() {
            vec![0.0; sz]
        } else if raw_vals.len() == 1 {
            vec![raw_vals[0]; sz]
        } else {
            let last = *raw_vals.last().unwrap_or(&0.0);
            let mut expanded = Vec::with_capacity(sz);
            for i in 0..sz {
                expanded.push(raw_vals.get(i).copied().unwrap_or(last));
            }
            expanded
        };
        if !vals.is_empty() {
            set_array_entries(env, name, &var.dims, &vals);
        }
    };

    // Constants may reference earlier/later constants; do a second pass.
    for _ in 0..2 {
        for (name, var) in &dae.constants {
            bind_start_value(&mut env, name.as_str(), var);
        }
    }

    // Inputs: bind default start expressions in the same context.
    for _ in 0..2 {
        for (name, var) in &dae.inputs {
            bind_start_value(&mut env, name.as_str(), var);
        }
    }

    // Discrete variables are not part of the continuous solver vector in the
    // current runtime. First restore their current values from the pre-store
    // (event-to-event memory), then fall back to start values when unavailable.
    let mut pre_seeded_discrete: HashSet<String> = HashSet::new();
    for (name, var) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
        if env.vars.contains_key(name.as_str()) {
            continue;
        }
        if try_seed_var_from_pre_store(&mut env, name.as_str(), var) {
            pre_seeded_discrete.insert(name.as_str().to_string());
        }
    }

    for _ in 0..2 {
        for (name, var) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
            if pre_seeded_discrete.contains(name.as_str()) {
                continue;
            }
            bind_start_value(&mut env, name.as_str(), var);
        }
    }

    env
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
pub fn collect_var_starts(dae: &Dae) -> IndexMap<String, flat::Expression> {
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

/// Lift an f64 environment to a generic SimFloat environment.
///
/// All values are converted with `T::from_f64()`, so dual parts start at 0.
/// flat::Function definitions are shared via Arc (zero-copy).
pub fn lift_env<T: SimFloat>(env: &VarEnv<f64>) -> VarEnv<T> {
    let mut result = VarEnv::new();
    for (name, &val) in &env.vars {
        result.vars.insert(name.clone(), T::from_f64(val));
    }
    result.functions = env.functions.clone();
    result.dims = env.dims.clone();
    result.start_exprs = env.start_exprs.clone();
    result.enum_literal_ordinals = env.enum_literal_ordinals.clone();
    result.is_initial = env.is_initial;
    result
}

/// Evaluate a constant expression (for start values, parameter defaults).
pub fn eval_const_expr(expr: &flat::Expression) -> f64 {
    eval_expr::<f64>(expr, &VarEnv::new())
}

/// Evaluate an expression as a flattened array of scalar values.
///
/// Nested array literals are flattened recursively; scalar expressions produce
/// a single-element vector.
pub fn eval_array_values<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> Vec<T> {
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

    fn collect<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>, out: &mut Vec<T>) {
        match expr {
            flat::Expression::Array { elements, .. } => {
                for e in elements {
                    collect(e, env, out);
                }
            }
            flat::Expression::Tuple { elements } => {
                for e in elements {
                    collect(e, env, out);
                }
            }
            flat::Expression::Range { start, step, end } => {
                let start_v = eval_expr::<T>(start, env).real();
                let end_v = eval_expr::<T>(end, env).real();
                let step_v = step
                    .as_ref()
                    .map(|s| eval_expr::<T>(s, env).real())
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
            flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
                if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                    out.extend(values);
                } else {
                    out.push(eval_expr::<T>(expr, env));
                }
            }
            flat::Expression::FieldAccess { base, field } => {
                if let Some(values) = eval_field_access_array_values(base, field, env) {
                    out.extend(values);
                } else {
                    out.push(eval_expr::<T>(expr, env));
                }
            }
            _ => out.push(eval_expr::<T>(expr, env)),
        }
    }

    let mut out = Vec::new();
    collect(expr, env, &mut out);
    out
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

fn eval_array_like_values<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> Vec<T> {
    match expr {
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if let Some(values) = encoded_slice_field_values(name.as_str(), env) {
                return values;
            }
            if let Some(values) = array_values_from_env_name_generic(name.as_str(), env) {
                return values;
            }
            vec![eval_expr::<T>(expr, env)]
        }
        flat::Expression::FieldAccess { base, field } => {
            if let Some(values) = eval_field_access_array_values(base, field, env) {
                return values;
            }
            vec![eval_expr::<T>(expr, env)]
        }
        flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Cat,
            args,
        } => eval_cat_values(args, env),
        flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Linspace,
            args,
        } => eval_linspace_values(args, env),
        flat::Expression::BuiltinCall { function, args } if args.len() == 1 => {
            let values = eval_array_like_values::<T>(&args[0], env);
            if values.len() > 1
                && let Some(mapped) = eval_unary_builtin_array_values(*function, values)
            {
                return mapped;
            }
            vec![eval_expr::<T>(expr, env)]
        }
        flat::Expression::Array { .. }
        | flat::Expression::Tuple { .. }
        | flat::Expression::Range { .. } => eval_array_values::<T>(expr, env),
        _ => vec![eval_expr::<T>(expr, env)],
    }
}

fn eval_cat_values<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> Vec<T> {
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

fn eval_linspace_values<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> Vec<T> {
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

fn eval_array_like_f64_values<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> Vec<f64> {
    eval_array_like_values(expr, env)
        .into_iter()
        .map(|v| v.real())
        .collect()
}

fn eval_cat_f64_values<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> Vec<f64> {
    eval_cat_values(args, env)
        .into_iter()
        .map(|v| v.real())
        .collect()
}

fn eval_columns_arg<T: SimFloat>(expr: Option<&flat::Expression>, env: &VarEnv<T>) -> Vec<usize> {
    let Some(expr) = expr else { return Vec::new() };
    eval_array_like_f64_values(expr, env)
        .into_iter()
        .map(|v| v.round() as i64)
        .filter(|v| *v > 0)
        .map(|v| v as usize)
        .collect()
}

fn eval_table_matrix_arg<T: SimFloat>(
    expr: &flat::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<Vec<f64>>> {
    match expr {
        flat::Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return Some(Vec::new());
            }

            if elements
                .iter()
                .all(|e| matches!(e, flat::Expression::Array { .. }))
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
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
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
    args: &[flat::Expression],
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

/// Evaluate a flat::Expression to a value of type T.
pub fn eval_expr<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> T {
    match expr {
        flat::Expression::Literal(lit) => eval_literal::<T>(lit),
        flat::Expression::VarRef { name, subscripts } => eval_var_ref::<T>(name, subscripts, env),
        flat::Expression::Binary { op, lhs, rhs } => eval_binary::<T>(op, lhs, rhs, env),
        flat::Expression::Unary { op, rhs } => eval_unary::<T>(op, rhs, env),
        flat::Expression::BuiltinCall { function, args } => eval_builtin::<T>(*function, args, env),
        flat::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => eval_function_call::<T>(name, args, *is_constructor, env),
        flat::Expression::If {
            branches,
            else_branch,
        } => eval_if::<T>(branches, else_branch, env),
        flat::Expression::Array { elements, .. } => {
            if let Some(first) = elements.first() {
                eval_expr::<T>(first, env)
            } else {
                T::zero()
            }
        }
        flat::Expression::Index { base, subscripts } => eval_index_expr::<T>(base, subscripts, env),
        flat::Expression::FieldAccess { base, field } => eval_field_access::<T>(base, field, env),
        flat::Expression::Empty => T::zero(),
        flat::Expression::Range { .. }
        | flat::Expression::Tuple { .. }
        | flat::Expression::ArrayComprehension { .. } => T::zero(),
    }
}

fn eval_index_expr<T: SimFloat>(
    base: &flat::Expression,
    subscripts: &[flat::Subscript],
    env: &VarEnv<T>,
) -> T {
    let Some(indices) = eval_index_subscripts(subscripts, env) else {
        return T::zero();
    };

    if let Some(path) = eval_field_access_path(base, env)
        && let Some(value) = eval_index_from_env_path(&path, &indices, env)
    {
        return value;
    }

    eval_index_from_nested_expr(base, &indices, env).unwrap_or_else(T::zero)
}

fn eval_index_subscripts<T: SimFloat>(
    subscripts: &[flat::Subscript],
    env: &VarEnv<T>,
) -> Option<Vec<usize>> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let raw = match subscript {
            flat::Subscript::Index(i) => *i as f64,
            flat::Subscript::Expr(expr) => eval_expr::<T>(expr, env).real().round(),
            flat::Subscript::Colon => return None,
        };
        if !raw.is_finite() || raw < 1.0 {
            return None;
        }
        indices.push(raw as usize);
    }
    Some(indices)
}

fn eval_index_from_env_path<T: SimFloat>(
    base_path: &str,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return env.vars.get(base_path).copied();
    }

    let joined = indices
        .iter()
        .map(|idx| idx.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let direct_key = format!("{base_path}[{joined}]");
    if let Some(value) = env.vars.get(&direct_key).copied() {
        return Some(value);
    }

    let dims = env.dims.get(base_path)?;
    if dims.len() != indices.len() {
        return None;
    }

    let mut flat_index = 0usize;
    for (dim, index) in dims.iter().zip(indices.iter()) {
        let dim_usize = usize::try_from(*dim).ok()?;
        if dim_usize == 0 || *index > dim_usize {
            return None;
        }
        flat_index = flat_index.saturating_mul(dim_usize);
        flat_index = flat_index.saturating_add(index.saturating_sub(1));
    }
    let flat_key = format!("{base_path}[{}]", flat_index + 1);
    env.vars.get(&flat_key).copied()
}

fn eval_index_from_nested_expr<T: SimFloat>(
    expr: &flat::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return Some(eval_expr::<T>(expr, env));
    }

    match expr {
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            let idx0 = indices[0].checked_sub(1)?;
            let element = elements.get(idx0)?;
            eval_index_from_nested_expr(element, &indices[1..], env)
        }
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            eval_index_from_env_path(name.as_str(), indices, env)
        }
        _ if indices.len() == 1 => {
            let values = eval_array_like_values::<T>(expr, env);
            values.get(indices[0].checked_sub(1)?).copied()
        }
        _ => None,
    }
}

fn with_function_call_stack<R>(name: &str, f: impl FnOnce() -> R) -> R {
    FUNC_CALL_STACK.with(|stack| stack.borrow_mut().push(name.to_string()));
    let out = f();
    FUNC_CALL_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        let _ = stack.pop();
    });
    out
}

fn current_function_call_name() -> Option<String> {
    FUNC_CALL_STACK.with(|stack| stack.borrow().last().cloned())
}

fn eval_subscript_indices<T: SimFloat>(
    subscripts: &[flat::Subscript],
    env: &VarEnv<T>,
) -> Vec<String> {
    subscripts
        .iter()
        .map(|sub| match sub {
            flat::Subscript::Index(i) => i.to_string(),
            flat::Subscript::Expr(expr) => eval_expr::<T>(expr, env).real().round().to_string(),
            flat::Subscript::Colon => ":".to_string(),
        })
        .collect()
}

fn eval_field_access_path<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> Option<String> {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                Some(name.as_str().to_string())
            } else {
                let idx = eval_subscript_indices(subscripts, env);
                Some(format!("{}[{}]", name.as_str(), idx.join(",")))
            }
        }
        flat::Expression::FieldAccess { base, field } => {
            let prefix = eval_field_access_path(base, env)?;
            Some(format!("{prefix}.{field}"))
        }
        _ => None,
    }
}

fn eval_field_access_constructor<T: SimFloat>(
    base_name: &flat::VarName,
    args: &[flat::Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let field_idx = match field {
        // Modelica.Complex and most scalar record constructors use positional
        // constructor arguments in declared field order.
        "re" => 0,
        "im" => 1,
        _ => return None,
    };
    let arg = args.get(field_idx)?;
    let _ = base_name;
    Some(eval_expr::<T>(arg, env))
}

const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

fn decode_named_constructor_arg(expr: &flat::Expression) -> Option<(&str, &flat::Expression)> {
    let flat::Expression::FunctionCall {
        name,
        args,
        is_constructor: _,
    } = expr
    else {
        return None;
    };
    let named = name.as_str().strip_prefix(NAMED_CONSTRUCTOR_ARG_PREFIX)?;
    let value = args.first()?;
    Some((named, value))
}

fn split_named_and_positional_call_args(
    args: &[flat::Expression],
) -> (HashMap<&str, &flat::Expression>, Vec<&flat::Expression>) {
    let mut named_args: HashMap<&str, &flat::Expression> = HashMap::new();
    let mut positional_args: Vec<&flat::Expression> = Vec::new();
    for arg in args {
        if let Some((name, value_expr)) = decode_named_constructor_arg(arg) {
            named_args.insert(name, value_expr);
        } else {
            positional_args.push(arg);
        }
    }
    (named_args, positional_args)
}

fn bind_constructor_inputs<T: SimFloat>(
    constructor: &flat::Function,
    args: &[flat::Expression],
    env: &VarEnv<T>,
) -> (VarEnv<T>, Vec<T>) {
    let mut local_env = env.clone();
    let mut input_values = Vec::with_capacity(constructor.inputs.len());
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_idx = 0usize;
    for input in &constructor.inputs {
        let value = if let Some(arg_expr) = named_args.get(input.name.as_str()) {
            eval_expr::<T>(arg_expr, &local_env)
        } else if let Some(arg_expr) = positional_args.get(positional_idx) {
            positional_idx += 1;
            eval_expr::<T>(arg_expr, &local_env)
        } else if let Some(default_expr) = &input.default {
            eval_expr::<T>(default_expr, &local_env)
        } else if let Some(existing) = local_env.vars.get(&input.name).copied() {
            existing
        } else {
            T::zero()
        };
        local_env.set(&input.name, value);
        input_values.push(value);
    }
    (local_env, input_values)
}

fn eval_field_access_constructor_by_signature<T: SimFloat>(
    base_name: &flat::VarName,
    args: &[flat::Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let constructor = env.functions.get(base_name.as_str())?;
    let (local_env, input_values) = bind_constructor_inputs(constructor, args, env);

    if let Some((idx, _)) = constructor
        .inputs
        .iter()
        .enumerate()
        .find(|(_, input)| input.name == field)
    {
        return input_values.get(idx).copied();
    }

    if let Some(output) = constructor
        .outputs
        .iter()
        .find(|output| output.name == field)
    {
        if let Some(default_expr) = &output.default {
            return Some(eval_expr::<T>(default_expr, &local_env));
        }
        if let Some(value) = local_env.vars.get(&output.name).copied() {
            return Some(value);
        }
    }

    None
}

fn eval_field_access<T: SimFloat>(base: &flat::Expression, field: &str, env: &VarEnv<T>) -> T {
    if let Some(path) = eval_field_access_path(base, env) {
        let key = format!("{path}.{field}");
        if let Some(value) = env.vars.get(&key).copied() {
            return value;
        }
    }

    if let flat::Expression::FunctionCall {
        name,
        args,
        is_constructor,
    } = base
    {
        if *is_constructor
            && let Some(value) = eval_field_access_constructor(name, args, field, env)
        {
            return value;
        }
        if *is_constructor
            && let Some(value) = eval_field_access_constructor_by_signature(name, args, field, env)
        {
            return value;
        }

        let projected = flat::VarName::new(format!("{}.{}", name.as_str(), field));
        let value = eval_function_call::<T>(&projected, args, false, env);
        if value.real().is_finite() {
            return value;
        }
    }

    T::zero()
}

fn eval_literal<T: SimFloat>(lit: &flat::Literal) -> T {
    match lit {
        flat::Literal::Real(v) => T::from_f64(*v),
        flat::Literal::Integer(v) => T::from_f64(*v as f64),
        flat::Literal::Boolean(v) => T::from_bool(*v),
        flat::Literal::String(_) => T::zero(),
    }
}

fn eval_var_ref<T: SimFloat>(
    name: &flat::VarName,
    subscripts: &[flat::Subscript],
    env: &VarEnv<T>,
) -> T {
    if subscripts.is_empty() {
        return eval_var_ref_no_subscripts(name.as_str(), env);
    }
    {
        let indices: Vec<String> = subscripts
            .iter()
            .map(|s| match s {
                flat::Subscript::Index(i) => format!("{}", i),
                flat::Subscript::Expr(expr) => {
                    let v = eval_expr::<T>(expr, env);
                    format!("{}", v.real() as i64)
                }
                flat::Subscript::Colon => ":".to_string(),
            })
            .collect();
        let indexed_name = format!("{}[{}]", name.as_str(), indices.join(","));
        let val = env.vars.get(&indexed_name).copied();
        val.unwrap_or_else(|| env.get(name.as_str()))
    }
}

/// Look up a variable with no explicit subscripts.
/// Handles names with embedded subscript expressions like `x[(2-1)]`.
fn eval_var_ref_no_subscripts<T: SimFloat>(raw: &str, env: &VarEnv<T>) -> T {
    if let Some(&v) = env.vars.get(raw) {
        return v;
    }
    if let Some(caller) = current_function_call_name() {
        let projection_field = if caller.ends_with(".re") {
            Some("re")
        } else if caller.ends_with(".im") {
            Some("im")
        } else {
            None
        };
        if let Some(field) = projection_field {
            let projected_key = format!("{raw}.{field}");
            if let Some(&v) = env.vars.get(projected_key.as_str()) {
                return v;
            }
        }
    }
    // If name contains brackets with expressions, try normalizing.
    if raw.contains('[') {
        if let Some(v) =
            normalize_var_name::<T>(raw, env).and_then(|n| env.vars.get(n.as_str()).copied())
        {
            return v;
        }
        if let Some(base_name) = unity_subscript_base_name(raw)
            && let Some(&v) = env.vars.get(base_name.as_str())
        {
            return v;
        }
    }
    if let Some(ordinal) = lookup_enum_literal_ordinal(raw, &env.enum_literal_ordinals) {
        return T::from_f64(ordinal as f64);
    }
    T::zero()
}

fn lookup_enum_literal_ordinal(raw: &str, ordinals: &IndexMap<String, i64>) -> Option<i64> {
    if let Some(&ordinal) = ordinals.get(raw) {
        return Some(ordinal);
    }
    let (prefix, literal) = raw.rsplit_once('.')?;
    if let Some(unquoted) = strip_quoted_identifier(literal) {
        let alt = format!("{prefix}.{unquoted}");
        return ordinals.get(&alt).copied();
    }
    let alt = format!("{prefix}.'{literal}'");
    ordinals.get(&alt).copied()
}

fn strip_quoted_identifier(segment: &str) -> Option<&str> {
    if segment.len() >= 2 && segment.starts_with('\'') && segment.ends_with('\'') {
        Some(&segment[1..segment.len() - 1])
    } else {
        None
    }
}

fn unity_subscript_base_name(name: &str) -> Option<String> {
    let mut base = String::with_capacity(name.len());
    let mut depth = 0usize;
    let mut current = String::new();
    let mut saw_subscript = false;

    for ch in name.chars() {
        match ch {
            '[' => {
                depth += 1;
                if depth == 1 {
                    current.clear();
                    saw_subscript = true;
                } else {
                    current.push(ch);
                }
            }
            ']' => {
                if depth == 1 {
                    let trimmed = current.trim();
                    validate_unity_subscript_text(trimmed)?;
                    current.clear();
                } else if depth > 1 {
                    current.push(ch);
                }
                depth = depth.saturating_sub(1);
            }
            _ if depth == 0 => base.push(ch),
            _ => current.push(ch),
        }
    }

    (saw_subscript && depth == 0).then_some(base)
}

fn is_unity_subscript_text(text: &str) -> bool {
    text == "1"
        || text
            .parse::<f64>()
            .ok()
            .is_some_and(|v| v.is_finite() && v == 1.0)
}

fn validate_unity_subscript_text(text: &str) -> Option<()> {
    is_unity_subscript_text(text).then_some(())
}

/// Normalize a variable name by evaluating constant subscript expressions.
///
/// For example: `"x[(2 - 1)]"` → `"x[1]"`, `"a[(3 + 1)].b"` → `"a[4].b"`
fn normalize_var_name<T: SimFloat>(name: &str, env: &VarEnv<T>) -> Option<String> {
    let mut result = String::with_capacity(name.len());
    let mut chars = name.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch != '[' {
            result.push(ch);
            continue;
        }
        let subscript_str = collect_bracketed(&mut chars);
        let val = subscript_str
            .trim()
            .parse::<i64>()
            .map(|v| v as f64)
            .unwrap_or_else(|_| eval_simple_int_expr(&subscript_str, env));
        result.push('[');
        result.push_str(&(val as i64).to_string());
        result.push(']');
    }

    if result != name { Some(result) } else { None }
}

/// Collect characters between `[` and matching `]`, handling nesting.
fn collect_bracketed(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut depth = 1;
    let mut s = String::new();
    for c in chars.by_ref() {
        match c {
            '[' => depth += 1,
            ']' if depth == 1 => break,
            ']' => depth -= 1,
            _ => {}
        }
        s.push(c);
    }
    s
}

/// Evaluate a simple integer expression from a subscript string.
/// Handles: integer literals, parenthesized expressions, +, -, *, and variable references.
fn eval_simple_int_expr<T: SimFloat>(s: &str, env: &VarEnv<T>) -> f64 {
    let s = s.trim();
    if let Ok(v) = s.parse::<i64>() {
        return v as f64;
    }
    if s.chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
    {
        return env.get(s).real();
    }
    if s.starts_with('(') && s.ends_with(')') {
        return eval_simple_int_expr(&s[1..s.len() - 1], env);
    }
    // Try binary ops: scan right-to-left for +/- then * (respecting parens)
    if let Some(v) = try_split_binop(s, b"+-", env) {
        return v;
    }
    if let Some(v) = try_split_binop(s, b"*", env) {
        return v;
    }
    0.0
}

/// Try splitting `s` at a binary operator (rightmost, outside parens).
fn try_split_binop<T: SimFloat>(s: &str, ops: &[u8], env: &VarEnv<T>) -> Option<f64> {
    let bytes = s.as_bytes();
    let mut depth = 0i32;
    for i in (1..bytes.len()).rev() {
        match bytes[i] {
            b')' => depth += 1,
            b'(' => depth -= 1,
            op if depth == 0 && ops.contains(&op) => {
                let left = s[..i].trim();
                if left.is_empty() {
                    continue;
                }
                let l = eval_simple_int_expr(left, env);
                let r = eval_simple_int_expr(&s[i + 1..], env);
                return Some(match op {
                    b'+' => l + r,
                    b'-' => l - r,
                    b'*' => l * r,
                    _ => 0.0,
                });
            }
            _ => {}
        }
    }
    None
}

fn eval_vector_values<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> Option<Vec<T>> {
    match expr {
        flat::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            let dims = env.dims.get(name.as_str())?;
            if dims.len() != 1 || dims[0] <= 1 {
                return None;
            }
            array_values_from_env_name_generic(name.as_str(), env).filter(|values| values.len() > 1)
        }
        flat::Expression::Array { is_matrix, .. } if !*is_matrix => {
            let values = eval_array_values(expr, env);
            (values.len() > 1).then_some(values)
        }
        _ => None,
    }
}

fn eval_vector_dot_product<T: SimFloat>(
    lhs: &flat::Expression,
    rhs: &flat::Expression,
    env: &VarEnv<T>,
) -> Option<T> {
    let lhs_values = eval_vector_values(lhs, env)?;
    let rhs_values = eval_vector_values(rhs, env)?;
    if lhs_values.len() != rhs_values.len() || lhs_values.is_empty() {
        return None;
    }

    Some(
        lhs_values
            .iter()
            .zip(rhs_values.iter())
            .fold(T::zero(), |acc, (l, r)| acc + (*l * *r)),
    )
}

fn eval_binary<T: SimFloat>(
    op: &flat::OpBinary,
    lhs: &flat::Expression,
    rhs: &flat::Expression,
    env: &VarEnv<T>,
) -> T {
    if matches!(op, flat::OpBinary::Mul(_))
        && let Some(dot) = eval_vector_dot_product(lhs, rhs, env)
    {
        return dot;
    }

    let l = eval_expr::<T>(lhs, env);
    let r = eval_expr::<T>(rhs, env);
    match op {
        flat::OpBinary::Add(_) | flat::OpBinary::AddElem(_) => l + r,
        flat::OpBinary::Sub(_) | flat::OpBinary::SubElem(_) => l - r,
        flat::OpBinary::Mul(_) | flat::OpBinary::MulElem(_) => l * r,
        flat::OpBinary::Div(_) | flat::OpBinary::DivElem(_) => {
            if r.real() == 0.0 {
                // 0/0 = 0 (simulation convention, avoids NaN propagation);
                // nonzero/0 = infinity (IEEE 754 convention).
                if l.real() == 0.0 {
                    T::zero()
                } else {
                    T::infinity()
                }
            } else {
                l / r
            }
        }
        flat::OpBinary::Exp(_) | flat::OpBinary::ExpElem(_) => l.powf(r),
        flat::OpBinary::And(_) => T::from_bool(l.to_bool() && r.to_bool()),
        flat::OpBinary::Or(_) => T::from_bool(l.to_bool() || r.to_bool()),
        flat::OpBinary::Lt(_) => T::from_bool(l.lt(r)),
        flat::OpBinary::Le(_) => T::from_bool(l.le(r)),
        flat::OpBinary::Gt(_) => T::from_bool(l.gt(r)),
        flat::OpBinary::Ge(_) => T::from_bool(l.ge(r)),
        flat::OpBinary::Eq(_) => T::from_bool(l.eq_approx(r)),
        flat::OpBinary::Neq(_) => T::from_bool(!l.eq_approx(r)),
        flat::OpBinary::Empty | flat::OpBinary::Assign(_) => T::zero(),
    }
}

fn eval_unary<T: SimFloat>(op: &flat::OpUnary, rhs: &flat::Expression, env: &VarEnv<T>) -> T {
    let r = eval_expr::<T>(rhs, env);
    match op {
        flat::OpUnary::Minus(_) | flat::OpUnary::DotMinus(_) => -r,
        flat::OpUnary::Plus(_) | flat::OpUnary::DotPlus(_) => r,
        flat::OpUnary::Not(_) => T::from_bool(!r.to_bool()),
        flat::OpUnary::Empty => r,
    }
}

#[derive(Debug, Clone, Copy)]
struct ClockTiming {
    period: f64,
    phase: f64,
}

fn eval_time_seconds<T: SimFloat>(env: &VarEnv<T>) -> f64 {
    env.get("time").real()
}

fn is_clock_tick(time: f64, period: f64, phase: f64) -> bool {
    if !time.is_finite() || !period.is_finite() || !phase.is_finite() || period <= 0.0 {
        return false;
    }

    let shifted = time - phase;
    let tol = 1e-9 * period.max(1.0);
    if shifted < -tol {
        return false;
    }

    let k = (shifted / period).round();
    let nearest = k * period;
    (shifted - nearest).abs() <= tol
}

fn clock_tick_value<T: SimFloat>(env: &VarEnv<T>, timing: ClockTiming) -> T {
    T::from_bool(is_clock_tick(
        eval_time_seconds(env),
        timing.period,
        timing.phase,
    ))
}

fn valid_positive_period(period: f64) -> Option<f64> {
    (period.is_finite() && period > 0.0).then_some(period)
}

fn eval_positive_factor<T: SimFloat>(
    arg: Option<&flat::Expression>,
    env: &VarEnv<T>,
) -> Option<f64> {
    let raw = arg.map(|expr| eval_expr::<T>(expr, env).real())?;
    let rounded = raw.round();
    (rounded.is_finite() && rounded > 0.0).then_some(rounded)
}

fn infer_clock_timing_from_expr<T: SimFloat>(
    expr: &flat::Expression,
    env: &VarEnv<T>,
) -> Option<ClockTiming> {
    let flat::Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    infer_clock_timing_from_call(short_name, args, env)
}

fn infer_clock_counter_form<T: SimFloat>(expr: &flat::Expression, env: &VarEnv<T>) -> Option<f64> {
    let flat::Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short_name != "Clock" || args.len() != 1 {
        return None;
    }

    let raw = eval_expr::<T>(&args[0], env).real();
    let rounded = raw.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    if !rounded.is_finite() || rounded <= 0.0 || (raw - rounded).abs() > tol {
        return None;
    }
    Some(rounded)
}

fn infer_clock_timing_from_call<T: SimFloat>(
    short_name: &str,
    args: &[flat::Expression],
    env: &VarEnv<T>,
) -> Option<ClockTiming> {
    match short_name {
        "Clock" => {
            let first = args.first()?;
            if let Some(base) = infer_clock_timing_from_expr(first, env) {
                return Some(base);
            }

            if args.len() >= 2 {
                let count = eval_expr::<T>(first, env).real();
                let resolution = eval_expr::<T>(&args[1], env).real();
                if resolution.is_finite() && resolution > 0.0 {
                    return valid_positive_period(count / resolution)
                        .map(|period| ClockTiming { period, phase: 0.0 });
                }
            }

            let period = eval_expr::<T>(first, env).real();
            valid_positive_period(period).map(|period| ClockTiming { period, phase: 0.0 })
        }
        "subSample" => {
            // MLS exact-clock construction pattern used by PeriodicExactClock:
            // c = subSample(Clock(factor), resolutionFactor)
            // corresponds to period = factor / resolutionFactor.
            if let Some(counter) = args
                .first()
                .and_then(|expr| infer_clock_counter_form(expr, env))
            {
                let resolution = eval_positive_factor(args.get(1), env).unwrap_or(1.0);
                return valid_positive_period(counter / resolution)
                    .map(|period| ClockTiming { period, phase: 0.0 });
            }

            let base = infer_clock_timing_from_expr(args.first()?, env)?;
            let factor = eval_positive_factor(args.get(1), env).unwrap_or(1.0);
            valid_positive_period(base.period * factor).map(|period| ClockTiming {
                period,
                phase: base.phase,
            })
        }
        "superSample" => {
            let base = infer_clock_timing_from_expr(args.first()?, env)?;
            let factor = eval_positive_factor(args.get(1), env).unwrap_or(1.0);
            valid_positive_period(base.period / factor).map(|period| ClockTiming {
                period,
                phase: base.phase,
            })
        }
        "shiftSample" | "backSample" => {
            let base = infer_clock_timing_from_expr(args.first()?, env)?;
            let shift = eval_expr::<T>(args.get(1).unwrap_or(args.first()?), env).real();
            let offset = if args.len() >= 3 {
                let resolution = eval_expr::<T>(&args[2], env).real();
                if resolution.is_finite() && resolution != 0.0 {
                    shift / resolution
                } else {
                    shift * base.period
                }
            } else {
                shift * base.period
            };
            let phase = if short_name == "shiftSample" {
                base.phase + offset
            } else {
                base.phase - offset
            };
            valid_positive_period(base.period).map(|period| ClockTiming { period, phase })
        }
        _ => None,
    }
}

fn eval_builtin_sample<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> T {
    match args {
        [] => T::zero(),
        [value] => eval_expr::<T>(value, env),
        [value, clock, ..] if infer_clock_timing_from_expr(clock, env).is_some() => {
            eval_expr::<T>(value, env)
        }
        [start, interval, ..] => {
            let start_t = eval_expr::<T>(start, env).real();
            let period = eval_expr::<T>(interval, env).real();
            let Some(period) = valid_positive_period(period) else {
                return T::zero();
            };
            let timing = ClockTiming {
                period,
                phase: start_t,
            };
            clock_tick_value(env, timing)
        }
    }
}

pub fn infer_clock_timing_seconds(
    expr: &flat::Expression,
    env: &VarEnv<f64>,
) -> Option<(f64, f64)> {
    infer_clock_timing_from_expr(expr, env).map(|timing| (timing.period, timing.phase))
}

fn eval_builtin_pre<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> T {
    let Some(arg0) = args.first() else {
        return T::zero();
    };

    if let flat::Expression::VarRef { name, subscripts } = arg0 {
        let key = if subscripts.is_empty() {
            name.as_str().to_string()
        } else {
            let indices = eval_subscript_indices(subscripts, env);
            format!("{}[{}]", name.as_str(), indices.join(","))
        };

        if let Some(value) = lookup_pre_value(&key) {
            return T::from_f64(value);
        }
        if let Some(normalized) = normalize_var_name::<T>(&key, env)
            && let Some(value) = lookup_pre_value(normalized.as_str())
        {
            return T::from_f64(value);
        }
        if let Some(base_name) = unity_subscript_base_name(&key)
            && let Some(value) = lookup_pre_value(base_name.as_str())
        {
            return T::from_f64(value);
        }
    }

    eval_expr::<T>(arg0, env)
}

fn eval_builtin<T: SimFloat>(
    function: flat::BuiltinFunction,
    args: &[flat::Expression],
    env: &VarEnv<T>,
) -> T {
    let arg = |i: usize| -> T {
        args.get(i)
            .map(|a| eval_expr::<T>(a, env))
            .unwrap_or(T::zero())
    };

    match function {
        flat::BuiltinFunction::Der => {
            if let Some(flat::Expression::VarRef { name, .. }) = args.first() {
                let der_name = format!("der({})", name.as_str());
                env.get(&der_name)
            } else {
                T::zero()
            }
        }
        flat::BuiltinFunction::Pre => eval_builtin_pre(args, env),

        // Math functions
        flat::BuiltinFunction::Abs => arg(0).abs(),
        flat::BuiltinFunction::Sign => arg(0).sign(),
        flat::BuiltinFunction::Sqrt => arg(0).sqrt(),
        flat::BuiltinFunction::Floor | flat::BuiltinFunction::Integer => arg(0).floor(),
        flat::BuiltinFunction::Ceil => arg(0).ceil(),
        flat::BuiltinFunction::Min => eval_builtin_min(args, env),
        flat::BuiltinFunction::Max => eval_builtin_max(args, env),
        flat::BuiltinFunction::Div => eval_div_mod_rem(arg(0), arg(1), DivKind::Div),
        flat::BuiltinFunction::Mod => eval_div_mod_rem(arg(0), arg(1), DivKind::Mod),
        flat::BuiltinFunction::Rem => eval_div_mod_rem(arg(0), arg(1), DivKind::Rem),
        flat::BuiltinFunction::SemiLinear => {
            let x = arg(0);
            if x.real() >= 0.0 {
                arg(1) * x
            } else {
                arg(2) * x
            }
        }

        // Trig / hyperbolic / exp
        _ => eval_builtin_math_and_event(function, args, env),
    }
}

fn eval_builtin_min<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        return reduce_array_argument(&args[0], env, |acc, v| acc.min(v), T::zero());
    }
    let mut it = args.iter().map(|expr| eval_expr::<T>(expr, env));
    let first = it.next().unwrap_or_else(T::zero);
    it.fold(first, |acc, v| acc.min(v))
}

fn eval_builtin_max<T: SimFloat>(args: &[flat::Expression], env: &VarEnv<T>) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        return reduce_array_argument(&args[0], env, |acc, v| acc.max(v), T::zero());
    }
    let mut it = args.iter().map(|expr| eval_expr::<T>(expr, env));
    let first = it.next().unwrap_or_else(T::zero);
    it.fold(first, |acc, v| acc.max(v))
}

fn reduce_array_argument<T: SimFloat, F: FnMut(T, T) -> T>(
    arg: &flat::Expression,
    env: &VarEnv<T>,
    reduce: F,
    default: T,
) -> T {
    let mut values = eval_array_like_values(arg, env).into_iter();
    let Some(first) = values.next() else {
        return default;
    };
    values.fold(first, reduce)
}

enum DivKind {
    Div,
    Mod,
    Rem,
}

fn eval_div_mod_rem<T: SimFloat>(x: T, divisor: T, kind: DivKind) -> T {
    if divisor.real() == 0.0 {
        return T::zero();
    }
    match kind {
        DivKind::Div => (x / divisor).trunc(),
        DivKind::Mod | DivKind::Rem => x.modulo(divisor),
    }
}

fn eval_builtin_math_and_event<T: SimFloat>(
    function: flat::BuiltinFunction,
    args: &[flat::Expression],
    env: &VarEnv<T>,
) -> T {
    let arg = |i: usize| -> T {
        args.get(i)
            .map(|a| eval_expr::<T>(a, env))
            .unwrap_or(T::zero())
    };

    match function {
        // Trigonometric
        flat::BuiltinFunction::Sin => arg(0).sin(),
        flat::BuiltinFunction::Cos => arg(0).cos(),
        flat::BuiltinFunction::Tan => arg(0).tan(),
        flat::BuiltinFunction::Asin => arg(0).asin(),
        flat::BuiltinFunction::Acos => arg(0).acos(),
        flat::BuiltinFunction::Atan => arg(0).atan(),
        flat::BuiltinFunction::Atan2 => arg(0).atan2(arg(1)),
        // Hyperbolic
        flat::BuiltinFunction::Sinh => arg(0).sinh(),
        flat::BuiltinFunction::Cosh => arg(0).cosh(),
        flat::BuiltinFunction::Tanh => arg(0).tanh(),
        // Exponential/logarithmic
        flat::BuiltinFunction::Exp => arg(0).exp(),
        flat::BuiltinFunction::Log => arg(0).ln(),
        flat::BuiltinFunction::Log10 => arg(0).log10(),

        // Event-related / pass-through
        flat::BuiltinFunction::Edge => {
            let current = arg(0).to_bool();
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env).to_bool();
            if current && !previous {
                T::one()
            } else {
                T::zero()
            }
        }
        flat::BuiltinFunction::Change => {
            let current = arg(0).to_bool();
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env).to_bool();
            if current != previous {
                T::one()
            } else {
                T::zero()
            }
        }
        flat::BuiltinFunction::Initial => {
            if env.is_initial {
                T::one()
            } else {
                T::zero()
            }
        }
        flat::BuiltinFunction::Terminal => T::zero(),
        flat::BuiltinFunction::Sample => eval_builtin_sample(args, env),
        flat::BuiltinFunction::Reinit => arg(1),
        flat::BuiltinFunction::NoEvent | flat::BuiltinFunction::Delay => arg(0),
        flat::BuiltinFunction::Smooth => arg(1),
        flat::BuiltinFunction::Homotopy => arg(0),

        // Reduction operators
        flat::BuiltinFunction::Sum => eval_builtin_sum(args, env),
        flat::BuiltinFunction::Product => eval_builtin_product(args, env),

        // Size builtin: size(A, dim) → dimension of A along dim
        flat::BuiltinFunction::Size => {
            if let Some(flat::Expression::VarRef { name, .. }) = args.first()
                && let Some(d) = env.dims.get(name.as_str())
            {
                let dim_idx = if args.len() > 1 {
                    arg(1).real() as usize
                } else {
                    1
                };
                let val = d.get(dim_idx.saturating_sub(1)).copied().unwrap_or(1);
                return T::from_f64(val as f64);
            }
            T::one()
        }

        // Array functions with scalar fallbacks
        flat::BuiltinFunction::Zeros => T::zero(),
        flat::BuiltinFunction::Ones => T::one(),
        flat::BuiltinFunction::Fill
        | flat::BuiltinFunction::Scalar
        | flat::BuiltinFunction::Vector => arg(0),
        flat::BuiltinFunction::Linspace => eval_linspace_values(args, env)
            .first()
            .copied()
            .unwrap_or_else(T::zero),
        flat::BuiltinFunction::Identity => T::one(),
        flat::BuiltinFunction::Cat => eval_cat_f64_values(args, env)
            .first()
            .copied()
            .map(T::from_f64)
            .unwrap_or_else(T::zero),

        // Unsupported array functions
        _ => {
            warn_once!(
                WARNED_ARRAY_BUILTINS,
                "Array builtin function {:?} not supported in scalar simulation evaluator, \
                 returning NaN. Results may be incorrect.",
                function
            );
            T::nan()
        }
    }
}

#[cfg(test)]
mod tests;
