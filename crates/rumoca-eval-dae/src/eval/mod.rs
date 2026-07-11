//! Generic expression evaluator for rumoca_core::Expression trees.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Mutex, OnceLock};

use crate::sim_float::SimFloat;
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rustc_hash::FxHashMap;

type Dae = dae::Dae;
type BuiltinFunction = rumoca_core::BuiltinFunction;
type Expression = rumoca_core::Expression;
type Function = rumoca_core::Function;
type FunctionParam = rumoca_core::FunctionParam;
type Literal = rumoca_core::Literal;
type OpBinary = rumoca_core::OpBinary;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;

const MAX_FUNC_RECURSION: usize = 64;

fn complex_field_selection_from_path(path: &str) -> Option<&'static str> {
    if rumoca_core::top_level_path_ends_with(path, "re") {
        Some("re")
    } else if rumoca_core::top_level_path_ends_with(path, "im") {
        Some("im")
    } else {
        None
    }
}

fn pre_store_array_fill_default() -> f64 {
    0.0
}

pub(super) fn expression_source_span(expr: &Expression) -> Option<rumoca_core::Span> {
    expr.span().filter(|span| !span.is_dummy())
}

pub(super) fn with_expression_span_if_available(err: EvalError, expr: &Expression) -> EvalError {
    match expression_source_span(expr) {
        Some(span) => err.with_span_if_missing(span),
        None => err,
    }
}

mod external_table;
use external_table::{
    ExternalTableRegistry, ExternalTableSpec, external_table_data_for_values,
    external_table_data_for_values_in, lookup_external_table_in, lookup_external_table_in_registry,
    register_external_table_in,
};
mod pre_seed;
use pre_seed::try_seed_var_from_pre_store;
mod array_helpers;
mod builtin_table;
mod clock_eval;
mod distribution_clock;
use array_helpers::{
    array_values_from_env_name, array_values_from_env_name_generic, encoded_slice_field_values,
    eval_unary_builtin_array_values, flattened_field_access_name, function_call_field_path,
    infer_dims_from_values, try_eval_field_access_array_values,
};
// Public for `rumoca-jit-dae` — the JIT emits calls into these runtime
// helpers when generating machine code for table-lookup expressions.
pub use builtin_table::{
    eval_table_bound_value_in, eval_table_bound_value_opt_in, eval_table_lookup_slope_value_in,
    eval_table_lookup_slope_value_opt_in, eval_table_lookup_value_in,
    eval_table_lookup_value_opt_in, eval_time_table_next_event_value_in,
    eval_time_table_next_event_value_opt_in,
};
pub use clock_eval::infer_clock_timing_seconds;
use clock_eval::{
    clock_tick_value, eval_builtin_sample, eval_time_seconds, infer_clock_timing_from_call,
    infer_clock_timing_from_expr,
};
use table_eval::{
    eval_table_1d_lookup, eval_table_1d_lookup_with_runtime, eval_table_constructor,
    eval_table_matrix_arg, table_x_bounds,
};

macro_rules! warn_once {
    ($flag:expr, $($arg:tt)*) => {
        if !$flag.swap(true, Ordering::Relaxed) {
            tracing::warn!($($arg)*);
        }
    };
}

mod array_eval;
mod builtin_runtime;
mod table_eval;

mod special;
use special::{
    copy_record_function_output_fields, eval_function_call, function_closure_from_arg,
    resolve_user_function_target,
};
pub use special::{
    MaterializedOutputs, RecordOutputFields, deterministic_automatic_global_seed,
    eval_builtin_pub, eval_condition_as_root,
    eval_function_call_pub, eval_function_call_pub_dae, eval_selected_function_output_pub,
    eval_selected_function_output_pub_dae, eval_user_function_array_output_pub,
    eval_user_function_output_array_path_pub, eval_user_function_output_path_pub,
    eval_user_function_outputs_pub, eval_user_function_record_output_pub,
    is_runtime_special_function_name, is_runtime_special_function_short_name,
    modelica_strings_hash_string, resolve_function_call_outputs_pub,
    resolve_function_call_outputs_pub_dae, resolve_user_function_output_dims_pub,
};
mod eval_expr_impl;
use array_eval::{
    declared_dims, eval_array_like_f64_values, eval_array_like_values, eval_binary_array_values,
    eval_columns_arg, eval_cross_values, eval_index_array_values, eval_linspace_values,
    eval_matrix_index, eval_matrix_literal_rows, eval_matrix_vector_product,
    eval_outer_product_values, eval_skew_values, eval_vector_matrix_product,
    eval_symmetric_values, eval_transpose_values, reshape_flat_matrix, try_eval_cat_values,
    try_infer_runtime_expr_dims,
};
pub use array_eval::{eval_array_values, eval_matrix_values, eval_shaped_array_values};

pub(crate) fn infer_runtime_expr_dims<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    try_infer_runtime_expr_dims(expr, env)
}

pub(crate) fn eval_field_access_path<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
    try_eval_field_access_path(expr, env)
}
use eval_expr_impl::*;
pub use eval_expr_impl::{EvalError, eval_expr};
mod runtime_env;
pub use runtime_env::{
    build_env, build_env_with_runtime, build_runtime_parameter_tail_env,
    build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    build_runtime_parameter_tail_env_with_runtime, clear_pre_values,
    clear_pre_values_in_env_runtime, clear_runtime_state, clear_runtime_state_in_env_runtime,
    get_pre_value, get_pre_value_from_env, refresh_env_solver_and_parameter_values,
    restore_pre_values, restore_pre_values_in_env_runtime, restore_pre_values_in_runtime,
    seed_pre_values_from_env, seed_pre_values_in_env_runtime, set_pre_value, set_pre_value_in_env,
    set_pre_value_in_runtime, snapshot_pre_values, snapshot_pre_values_from_env,
    snapshot_pre_values_from_runtime,
};
pub(crate) use runtime_env::{lookup_pre_value, lookup_pre_value_in, snapshot_pre_values_in};

pub const INIT_HOMOTOPY_LAMBDA_KEY: &str = "__rumoca_init_homotopy_lambda";

#[derive(Debug)]
pub struct EvalRuntimeState {
    pre_values: Mutex<IndexMap<String, f64>>,
    env_key_cache: Mutex<HashMap<EnvKeyCacheKey, Arc<[String]>>>,
    function_recursion_depth: AtomicUsize,
    function_call_stack: Mutex<Vec<String>>,
    clock_special_states: Mutex<HashMap<String, distribution_clock::ShiftSignalState>>,
    external_tables: Mutex<ExternalTableRegistry>,
    impure_random: Mutex<special::ImpureRandomRegistry>,
    warned_array_builtins: AtomicBool,
    warned_user_functions: AtomicBool,
    warned_table_extrapolation: AtomicBool,
    warned_table_invalid_bounds: AtomicBool,
}

impl Default for EvalRuntimeState {
    fn default() -> Self {
        Self {
            pre_values: Mutex::new(IndexMap::new()),
            env_key_cache: Mutex::new(HashMap::new()),
            function_recursion_depth: AtomicUsize::new(0),
            function_call_stack: Mutex::new(Vec::new()),
            clock_special_states: Mutex::new(HashMap::new()),
            external_tables: Mutex::new(ExternalTableRegistry::default()),
            impure_random: Mutex::new(special::ImpureRandomRegistry::default()),
            warned_array_builtins: AtomicBool::new(false),
            warned_user_functions: AtomicBool::new(false),
            warned_table_extrapolation: AtomicBool::new(false),
            warned_table_invalid_bounds: AtomicBool::new(false),
        }
    }
}

impl EvalRuntimeState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear_pre_values(&self) {
        self.pre_values
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clear();
    }

    pub fn clear_runtime_state(&self) {
        self.clear_pre_values();
        *self
            .impure_random
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner()) =
            special::ImpureRandomRegistry::default();
        self.function_recursion_depth.store(0, Ordering::Relaxed);
        self.function_call_stack
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clear();
        self.clock_special_states
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clear();
        self.env_key_cache
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clear();
        self.warned_array_builtins.store(false, Ordering::Relaxed);
        self.warned_user_functions.store(false, Ordering::Relaxed);
        self.warned_table_extrapolation
            .store(false, Ordering::Relaxed);
        self.warned_table_invalid_bounds
            .store(false, Ordering::Relaxed);
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum EnvKeyCacheKey {
    Indexed {
        name: String,
        count: usize,
    },
    IndexedField {
        base: String,
        field: String,
        count: usize,
    },
    FieldIndexed {
        base: String,
        field: String,
        count: usize,
    },
}

fn cached_env_keys(
    runtime: &EvalRuntimeState,
    cache_key: EnvKeyCacheKey,
    build: impl FnOnce() -> Vec<String>,
) -> Arc<[String]> {
    let mut cache = runtime
        .env_key_cache
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    if let Some(keys) = cache.get(&cache_key) {
        return keys.clone();
    }
    let keys: Arc<[String]> = build().into();
    cache.insert(cache_key, keys.clone());
    keys
}

fn cached_indexed_keys(runtime: &EvalRuntimeState, name: &str, count: usize) -> Arc<[String]> {
    cached_env_keys(
        runtime,
        EnvKeyCacheKey::Indexed {
            name: name.to_string(),
            count,
        },
        || {
            (1..=count)
                .map(|idx| dae::format_subscript_key(name, &[idx]))
                .collect()
        },
    )
}

fn cached_indexed_field_keys(
    runtime: &EvalRuntimeState,
    base: &str,
    field: &str,
    count: usize,
) -> Arc<[String]> {
    cached_env_keys(
        runtime,
        EnvKeyCacheKey::IndexedField {
            base: base.to_string(),
            field: field.to_string(),
            count,
        },
        || {
            (1..=count)
                .map(|idx| format!("{}.{}", dae::format_subscript_key(base, &[idx]), field))
                .collect()
        },
    )
}

fn cached_field_indexed_keys(
    runtime: &EvalRuntimeState,
    base: &str,
    field: &str,
    count: usize,
) -> Arc<[String]> {
    cached_env_keys(
        runtime,
        EnvKeyCacheKey::FieldIndexed {
            base: base.to_string(),
            field: field.to_string(),
            count,
        },
        || {
            (1..=count)
                .map(|idx| dae::format_subscript_key(&format!("{base}.{field}"), &[idx]))
                .collect()
        },
    )
}

static DEFAULT_RUNTIME_STATE: OnceLock<Arc<EvalRuntimeState>> = OnceLock::new();

fn default_runtime_state() -> &'static Arc<EvalRuntimeState> {
    DEFAULT_RUNTIME_STATE.get_or_init(|| Arc::new(EvalRuntimeState::default()))
}

#[derive(Debug, Clone)]
pub struct FunctionClosure {
    pub target_name: rumoca_core::VarName,
    pub bound_args: Vec<rumoca_core::Expression>,
}

#[derive(Debug)]
pub struct VarScope<T> {
    frame: Arc<VarScopeFrame<T>>,
}

impl<T> Clone for VarScope<T> {
    fn clone(&self) -> Self {
        Self {
            frame: self.frame.clone(),
        }
    }
}

#[derive(Debug, Clone)]
struct VarScopeFrame<T> {
    local: IndexMap<String, T>,
    parent: Option<VarScope<T>>,
}

pub struct VarScopeIter<'a, T> {
    entries: std::vec::IntoIter<(&'a String, &'a T)>,
}

impl<'a, T> Iterator for VarScopeIter<'a, T> {
    type Item = (&'a String, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.entries.next()
    }
}

impl<T> Default for VarScope<T> {
    fn default() -> Self {
        Self {
            frame: Arc::new(VarScopeFrame {
                local: IndexMap::new(),
                parent: None,
            }),
        }
    }
}

impl<T> VarScope<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn child_of(parent: &Self) -> Self {
        Self {
            frame: Arc::new(VarScopeFrame {
                local: IndexMap::new(),
                parent: Some(parent.clone()),
            }),
        }
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.frame.local.get(name).or_else(|| {
            self.frame
                .parent
                .as_ref()
                .and_then(|parent| parent.get(name))
        })
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.get(name).is_some()
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut T> {
        Arc::get_mut(&mut self.frame)?.local.get_mut(name)
    }

    pub fn len(&self) -> usize {
        self.ordered_entries().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> VarScopeIter<'_, T> {
        VarScopeIter {
            entries: self.ordered_entries().into_iter(),
        }
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.iter().map(|(name, _)| name)
    }

    pub fn local_iter(&self) -> indexmap::map::Iter<'_, String, T> {
        self.frame.local.iter()
    }

    fn ordered_entries(&self) -> Vec<(&String, &T)> {
        if self.frame.parent.is_none() {
            return self.frame.local.iter().collect();
        }
        let mut entries = Vec::new();
        let mut positions = FxHashMap::default();
        self.collect_ordered_entries(&mut entries, &mut positions);
        entries
    }

    fn collect_ordered_entries<'a>(
        &'a self,
        entries: &mut Vec<(&'a String, &'a T)>,
        positions: &mut FxHashMap<&'a str, usize>,
    ) {
        if let Some(parent) = &self.frame.parent {
            parent.collect_ordered_entries(entries, positions);
        }
        for (name, value) in &self.frame.local {
            if let Some(index) = positions.get(name.as_str()).copied() {
                entries[index] = (name, value);
            } else {
                positions.insert(name.as_str(), entries.len());
                entries.push((name, value));
            }
        }
    }
}

impl<T: Clone> VarScope<T> {
    pub fn insert(&mut self, name: String, value: T) -> Option<T> {
        Arc::make_mut(&mut self.frame).local.insert(name, value)
    }

    pub fn remove(&mut self, name: &str) -> Option<T> {
        Arc::make_mut(&mut self.frame).local.shift_remove(name)
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = (String, T)>,
    {
        Arc::make_mut(&mut self.frame).local.extend(iter);
    }
}

impl<'a, T> IntoIterator for &'a VarScope<T> {
    type Item = (&'a String, &'a T);
    type IntoIter = VarScopeIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionOutputCacheKey {
    function_id: rumoca_core::VarNameId,
    args: Vec<Expression>,
    output_name: String,
    is_initial: bool,
}

impl FunctionOutputCacheKey {
    pub(crate) fn new(
        function: &VarName,
        args: &[Expression],
        output_name: impl Into<String>,
        is_initial: bool,
    ) -> Self {
        Self {
            function_id: function.id(),
            args: args.to_vec(),
            output_name: output_name.into(),
            is_initial,
        }
    }

    fn matches(&self, rhs: &Self) -> bool {
        self.function_id == rhs.function_id
            && self.output_name == rhs.output_name
            && self.is_initial == rhs.is_initial
            && self.args.len() == rhs.args.len()
            && self
                .args
                .iter()
                .zip(rhs.args.iter())
                .all(|(lhs, rhs)| lhs.semantically_eq_ignoring_spans(rhs))
    }
}

#[derive(Debug, Clone)]
struct FunctionOutputCacheEntry<T: SimFloat> {
    key: FunctionOutputCacheKey,
    value: T,
}

#[derive(Debug)]
pub struct VarEnv<T: SimFloat = f64> {
    pub vars: VarScope<T>,
    pub runtime: Arc<EvalRuntimeState>,
    pub functions: Arc<IndexMap<String, rumoca_core::Function>>,
    pub dims: Arc<IndexMap<String, Vec<i64>>>,
    pub start_exprs: Arc<IndexMap<String, rumoca_core::Expression>>,
    pub clock_intervals: Arc<IndexMap<String, f64>>,
    pub enum_literal_ordinals: Arc<IndexMap<String, i64>>,
    pub function_closures: IndexMap<String, FunctionClosure>,
    pub is_initial: bool,
    function_output_cache: RefCell<Vec<FunctionOutputCacheEntry<T>>>,
}

impl<T: SimFloat> Clone for VarEnv<T> {
    fn clone(&self) -> Self {
        Self {
            vars: self.vars.clone(),
            runtime: self.runtime.clone(),
            functions: self.functions.clone(),
            dims: self.dims.clone(),
            start_exprs: self.start_exprs.clone(),
            clock_intervals: self.clock_intervals.clone(),
            enum_literal_ordinals: self.enum_literal_ordinals.clone(),
            function_closures: self.function_closures.clone(),
            is_initial: self.is_initial,
            function_output_cache: RefCell::new(Vec::new()),
        }
    }
}

impl<T: SimFloat> Default for VarEnv<T> {
    fn default() -> Self {
        Self {
            vars: VarScope::new(),
            runtime: Arc::new(EvalRuntimeState::new()),
            functions: Arc::new(IndexMap::new()),
            dims: Arc::new(IndexMap::new()),
            start_exprs: Arc::new(IndexMap::new()),
            clock_intervals: Arc::new(IndexMap::new()),
            enum_literal_ordinals: Arc::new(IndexMap::new()),
            function_closures: IndexMap::new(),
            is_initial: false,
            function_output_cache: RefCell::new(Vec::new()),
        }
    }
}

impl<T: SimFloat> VarEnv<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_isolated_runtime(mut self) -> Self {
        self.runtime = Arc::new(EvalRuntimeState::new());
        self
    }

    pub fn with_default_runtime(mut self) -> Self {
        self.runtime = default_runtime_state().clone();
        self
    }

    pub fn get_optional(&self, name: &str) -> Option<T> {
        self.vars.get(name).copied()
    }

    pub fn require(&self, name: &str) -> Result<T, EvalError> {
        self.get_optional(name)
            .ok_or_else(|| EvalError::MissingBinding {
                name: name.to_string(),
            })
    }

    pub fn set(&mut self, name: &str, value: T) {
        set_env_key(self, name, value);
    }

    pub(crate) fn remove(&mut self, name: &str) -> Option<T> {
        self.clear_function_output_cache();
        self.vars.remove(name)
    }

    pub(crate) fn get_function_output_cache(&self, key: &FunctionOutputCacheKey) -> Option<T> {
        self.function_output_cache
            .borrow()
            .iter()
            .find(|entry| entry.key.matches(key))
            .map(|entry| entry.value)
    }

    pub(crate) fn insert_function_output_cache(&self, key: FunctionOutputCacheKey, value: T) {
        let mut cache = self.function_output_cache.borrow_mut();
        if let Some(entry) = cache.iter_mut().find(|entry| entry.key.matches(&key)) {
            entry.value = value;
            return;
        }
        cache.push(FunctionOutputCacheEntry { key, value });
    }

    pub(crate) fn clear_function_output_cache(&self) {
        let mut cache = self.function_output_cache.borrow_mut();
        if !cache.is_empty() {
            cache.clear();
        }
    }
}

fn set_env_key<T: SimFloat>(env: &mut VarEnv<T>, key: &str, value: T) {
    env.clear_function_output_cache();
    if let Some(slot) = env.vars.get_mut(key) {
        *slot = value;
    } else {
        env.vars.insert(key.to_string(), value);
    }
}

/// Evaluate a lowered boolean condition with Modelica when-vector semantics.
///
/// MLS §8.3.5 permits vectorized when-conditions; Rumoca lowers
/// `when {c1, c2, ...}` into `Array` / `Tuple` guards that are active when any
/// listed condition is true.
#[cfg(test)]
pub fn eval_condition_truth<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> bool {
    match expr {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| eval_condition_truth(element, env)),
        _ => eval_expr::<T>(expr, env)
            .expect("test condition should evaluate")
            .to_bool(),
    }
}

pub fn try_eval_condition_truth<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    match expr {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                if try_eval_condition_truth(element, env)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        _ => Ok(eval_expr::<T>(expr, env)?.to_bool()),
    }
}

pub(crate) fn eval_statement_value<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    eval_expr(expr, env)
}

/// Internal runtime environment key used to evaluate implicit `Clock()`
/// conditions inside guarded when-equations.
pub const IMPLICIT_CLOCK_ACTIVE_ENV_KEY: &str = "__rumoca_implicit_clock_active";

pub(super) fn previous_start_or_default<T: SimFloat>(
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg
    else {
        return Ok(T::zero());
    };

    if let Some(start) = env.start_exprs.get(name.as_str()) {
        if !subscripts.is_empty() {
            let span = expression_source_span(arg)
                .or_else(|| expression_source_span(start))
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "previous start indexed value missing source span",
                })?;
            let indexed_start = rumoca_core::Expression::Index {
                base: Box::new(start.clone()),
                subscripts: subscripts.to_vec(),
                span,
            };
            return eval_expr::<T>(&indexed_start, env);
        }
        return eval_expr::<T>(start, env);
    }
    Ok(T::zero())
}

fn try_seed_lowered_pre_parameter_from_store(
    env: &mut VarEnv<f64>,
    name: &str,
    var: &rumoca_ir_dae::Variable,
) -> bool {
    // MLS §3.7.5 / pre-lowering: lowered `pre(x)` parameters must reflect the
    // event left-limit store, not the static runtime parameter vector.
    let Some(target_name) = rumoca_core::pre_slot_base(name) else {
        return false;
    };

    let size = var.size();
    if size <= 1 {
        let Some(value) = lookup_pre_value_in(&env.runtime, target_name) else {
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
        let key = dae::scalar_name_text_for_flat_index(target_name, &var.dims, flat_idx);
        if let Some(value) = lookup_pre_value_in(&env.runtime, &key) {
            values.push(Some(value));
            found_any = true;
        } else {
            values.push(None);
        }
    }

    let fallback = lookup_pre_value_in(&env.runtime, target_name)
        .or_else(|| values.iter().copied().flatten().next());
    if !found_any && fallback.is_none() {
        return false;
    }

    let fill = fallback.unwrap_or_else(pre_store_array_fill_default);
    let values = values
        .into_iter()
        .map(|value| value.unwrap_or(fill))
        .collect::<Vec<_>>();
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

/// Map flattened array values into scalar and subscripted entries.
///
/// Writes:
/// - `name` = first element
/// - `name[i]` for flat 1-based index
/// - `name[i,j,...]` when dimensions are available
pub fn set_array_entries<T: SimFloat>(env: &mut VarEnv<T>, name: &str, dims: &[i64], values: &[T]) {
    let Some(&first) = values.first() else { return };
    env.set(name, first);
    let runtime = env.runtime.clone();
    let flat_keys = cached_indexed_keys(&runtime, name, values.len());
    for (i, &v) in values.iter().enumerate() {
        set_env_key(env, flat_keys[i].as_str(), v);
        if let Some(subs) = dae::flat_index_to_subscripts(dims, i)
            && subs.len() > 1
        {
            env.set(&dae::format_subscript_key(name, &subs), v);
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

pub(super) fn try_populate_env_values(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    env: &mut VarEnv<f64>,
) -> Result<(), EvalError> {
    env.set("time", t);

    map_solver_vectors_into_env(env, dae, y);
    try_populate_runtime_parameter_tail(env, dae, p, ParameterSlotPolicy::Numeric)
}

pub fn build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
    dae: &Dae,
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    try_populate_partial_runtime_parameter_tail_env_values(
        dae,
        p,
        t,
        &mut env,
        ParameterSlotPolicy::Declared,
    )?;
    Ok(env)
}

fn try_populate_partial_runtime_parameter_tail_env_values(
    dae: &Dae,
    p: &[f64],
    t: f64,
    env: &mut VarEnv<f64>,
    parameter_slot_policy: ParameterSlotPolicy,
) -> Result<(), EvalError> {
    env.set("time", t);
    configure_env_metadata(env, dae);
    map_parameter_vector_into_env(env, dae, p, parameter_slot_policy);
    inject_modelica_constants(env);
    bind_constants(env, dae)
}

pub(super) fn try_populate_runtime_parameter_tail_env_values(
    dae: &Dae,
    p: &[f64],
    t: f64,
    env: &mut VarEnv<f64>,
) -> Result<(), EvalError> {
    try_populate_runtime_parameter_tail_env_values_with_policy(
        dae,
        p,
        t,
        env,
        ParameterSlotPolicy::Numeric,
    )
}

pub(super) fn try_populate_runtime_parameter_tail_env_values_with_policy(
    dae: &Dae,
    p: &[f64],
    t: f64,
    env: &mut VarEnv<f64>,
    parameter_slot_policy: ParameterSlotPolicy,
) -> Result<(), EvalError> {
    env.set("time", t);
    try_populate_runtime_parameter_tail(env, dae, p, parameter_slot_policy)
}

fn try_populate_runtime_parameter_tail(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    p: &[f64],
    parameter_slot_policy: ParameterSlotPolicy,
) -> Result<(), EvalError> {
    configure_env_metadata(env, dae);
    map_parameter_vector_into_env(env, dae, p, parameter_slot_policy);
    inject_modelica_constants(env);
    bind_constants(env, dae)?;
    bind_missing_parameter_values(env, dae)?;
    bind_inputs(env, dae)?;
    seed_discrete_values(env, dae)
}

/// Refresh solver- and parameter-backed runtime slots in an existing env.
///
/// This preserves already-settled discrete/runtime tail values while updating
/// the current solver state, parameters, and time.
pub(super) fn refresh_env_solver_and_parameter_values_unchecked(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
) {
    env.set("time", t);
    map_solver_vectors_into_env(env, dae, y);
    map_parameter_vector_into_env(env, dae, p, ParameterSlotPolicy::Numeric);
}

fn map_solver_vectors_into_env(env: &mut VarEnv<f64>, dae: &Dae, y: &[f64]) {
    let mut idx = 0;
    for (name, var) in &dae.variables.states {
        map_var_to_env(env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae.variables.algebraics {
        map_var_to_env(env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae.variables.outputs {
        map_var_to_env(env, name.as_str(), var, y, &mut idx);
    }
}

#[derive(Clone, Copy)]
pub(super) enum ParameterSlotPolicy {
    Numeric,
    Declared,
}

fn map_parameter_vector_into_env(
    env: &mut VarEnv<f64>,
    dae: &Dae,
    p: &[f64],
    policy: ParameterSlotPolicy,
) {
    let mut pidx = 0;
    for (name, var) in &dae.variables.parameters {
        if !parameter_has_numeric_slot(var, env) {
            if matches!(policy, ParameterSlotPolicy::Declared) {
                pidx += var.size();
            }
            continue;
        }
        map_var_to_env(env, name.as_str(), var, p, &mut pidx);
        let _ = try_seed_lowered_pre_parameter_from_store(env, name.as_str(), var);
    }
}

pub(crate) fn parameter_has_numeric_slot(var: &rumoca_ir_dae::Variable, env: &VarEnv<f64>) -> bool {
    var.start
        .as_ref()
        .is_none_or(|start| !start_expr_is_nonnumeric(start, env))
}

fn configure_env_metadata(env: &mut VarEnv<f64>, dae: &Dae) {
    if !dae.symbols.functions.is_empty() {
        let func_map: IndexMap<String, rumoca_core::Function> = dae
            .symbols
            .functions
            .iter()
            .map(|(name, func)| (name.as_str().to_string(), func.clone()))
            .collect();
        env.functions = Arc::new(func_map);
    }
    env.dims = Arc::new(collect_var_dims(dae));
    env.start_exprs = Arc::new(collect_var_starts(dae));
    env.clock_intervals = Arc::new(dae.clocks.intervals.clone());
    env.enum_literal_ordinals = Arc::new(dae.symbols.enum_literal_ordinals.clone());
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

fn bind_start_value(
    env: &mut VarEnv<f64>,
    name: &str,
    var: &rumoca_ir_dae::Variable,
) -> Result<(), EvalError> {
    let Some(start) = var.start.as_ref() else {
        return Ok(());
    };
    if start_expr_is_nonnumeric(start, env) {
        return Ok(());
    }
    let size = var.size();
    if size == 0 && !var.dims.is_empty() {
        // MLS Chapter 10: arrays with an unknown dimension can still have a
        // parameter binding that materializes at evaluation time. Preserve the
        // aggregate values in the environment instead of collapsing the binding
        // to one scalar placeholder.
        let values = if var.dims.len() >= 2 {
            match eval_matrix_values(start, env) {
                Ok(Some(matrix)) => matrix.into_iter().flatten().collect(),
                Ok(None) => eval_array_values::<f64>(start, env)?,
                Err(err) => return Err(err),
            }
        } else {
            eval_array_values::<f64>(start, env)?
        };
        if !values.is_empty() {
            set_array_entries(env, name, &var.dims, &values);
        }
        return Ok(());
    }
    if size <= 1 && var.dims.is_empty() {
        let value = eval_expr::<f64>(start, env)?;
        env.set(name, value);
        return Ok(());
    }
    if size <= 1 {
        let values = eval_shaped_array_values::<f64>(start, env, 1)?;
        set_array_entries(env, name, &var.dims, &values);
        return Ok(());
    }

    let values = match eval_shaped_array_values::<f64>(start, env, size) {
        Ok(values) => values,
        Err(EvalError::ShapeMismatch { actual: 1, .. })
            if can_broadcast_start_value(start, env) =>
        {
            vec![eval_expr::<f64>(start, env)?; size]
        }
        Err(err) => return Err(err),
    };
    set_array_entries(env, name, &var.dims, &values);
    Ok(())
}

pub fn start_expr_is_nonnumeric(expr: &rumoca_core::Expression, env: &VarEnv<f64>) -> bool {
    start_expr_is_nonnumeric_inner(expr, env, &mut HashSet::new())
}

fn start_expr_is_nonnumeric_inner(
    expr: &rumoca_core::Expression,
    env: &VarEnv<f64>,
    visited: &mut HashSet<String>,
) -> bool {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(_),
            ..
        } => true,
        rumoca_core::Expression::Literal { .. } => false,
        rumoca_core::Expression::Unary { rhs, .. } => {
            start_expr_is_nonnumeric_inner(rhs, env, visited)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            start_expr_is_nonnumeric_inner(lhs, env, visited)
                || start_expr_is_nonnumeric_inner(rhs, env, visited)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if *function == rumoca_core::BuiltinFunction::Size =>
        {
            args.get(1)
                .is_some_and(|arg| start_expr_is_nonnumeric_inner(arg, env, visited))
        }
        rumoca_core::Expression::BuiltinCall { args, .. } => args
            .iter()
            .any(|arg| start_expr_is_nonnumeric_inner(arg, env, visited)),
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            !is_runtime_special_function_name(name.var_name())
                && args
                    .iter()
                    .any(|arg| start_expr_is_nonnumeric_inner(arg, env, visited))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                start_expr_is_nonnumeric_inner(condition, env, visited)
                    || start_expr_is_nonnumeric_inner(value, env, visited)
            }) || start_expr_is_nonnumeric_inner(else_branch, env, visited)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| start_expr_is_nonnumeric_inner(element, env, visited)),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            start_expr_is_nonnumeric_inner(expr, env, visited)
                || indices
                    .iter()
                    .any(|index| start_expr_is_nonnumeric_inner(&index.range, env, visited))
                || filter
                    .as_ref()
                    .is_some_and(|filter| start_expr_is_nonnumeric_inner(filter, env, visited))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            start_expr_is_nonnumeric_inner(base, env, visited)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        start_expr_is_nonnumeric_inner(expr, env, visited)
                    }
                    rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
                        false
                    }
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            start_expr_is_nonnumeric_inner(base, env, visited)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            start_expr_is_nonnumeric_inner(start, env, visited)
                || step
                    .as_ref()
                    .is_some_and(|step| start_expr_is_nonnumeric_inner(step, env, visited))
                || start_expr_is_nonnumeric_inner(end, env, visited)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let key = name.as_str();
            if env.enum_literal_ordinals.contains_key(key) || !visited.insert(key.to_string()) {
                return false;
            }
            env.start_exprs
                .get(key)
                .is_some_and(|start| start_expr_is_nonnumeric_inner(start, env, visited))
        }
        rumoca_core::Expression::VarRef { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}

pub fn can_broadcast_start_value(expr: &rumoca_core::Expression, env: &VarEnv<f64>) -> bool {
    match expr {
        rumoca_core::Expression::Literal { .. } => true,
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && env.enum_literal_ordinals.contains_key(name.as_str()) => true,
        rumoca_core::Expression::Unary { rhs, .. } => can_broadcast_start_value(rhs, env),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .all(|(_, value)| can_broadcast_start_value(value, env))
                && can_broadcast_start_value(else_branch, env)
        }
        _ => false,
    }
}

fn bind_constants(env: &mut VarEnv<f64>, dae: &Dae) -> Result<(), EvalError> {
    let vars = dae.variables.constants.iter().collect::<Vec<_>>();
    bind_start_values_until_stable(env, &vars, false, vars.len().clamp(1, 8))
}

fn bind_inputs(env: &mut VarEnv<f64>, dae: &Dae) -> Result<(), EvalError> {
    let vars = dae.variables.inputs.iter().collect::<Vec<_>>();
    bind_start_values_until_stable(env, &vars, false, vars.len().clamp(1, 8))
}

fn bind_missing_parameter_values(env: &mut VarEnv<f64>, dae: &Dae) -> Result<(), EvalError> {
    let vars = dae.variables.parameters.iter().collect::<Vec<_>>();
    bind_start_values_until_stable(env, &vars, true, vars.len().clamp(1, 32))
}

fn bind_start_values_until_stable(
    env: &mut VarEnv<f64>,
    vars: &[(&VarName, &rumoca_ir_dae::Variable)],
    skip_existing: bool,
    max_passes: usize,
) -> Result<(), EvalError> {
    let mut pending_missing = None;
    for _ in 0..max_passes {
        let mut changed = false;
        pending_missing = None;
        for (name, var) in vars {
            if skip_existing && env.vars.contains_key(name.as_str()) {
                continue;
            }
            let before_len = env.vars.len();
            match bind_start_value(env, name.as_str(), var) {
                Ok(()) => {}
                Err(err) if err.missing_binding_name().is_some() => {
                    pending_missing.get_or_insert(err);
                    continue;
                }
                Err(err) => {
                    let span = var
                        .start
                        .as_ref()
                        .and_then(|expr| expr.span())
                        .unwrap_or(var.source_span);
                    return Err(err.with_span_if_missing(span));
                }
            }
            changed |= env.vars.len() != before_len || env.vars.contains_key(name.as_str());
        }
        if !changed {
            break;
        }
    }
    match pending_missing {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

fn seed_discrete_values(env: &mut VarEnv<f64>, dae: &Dae) -> Result<(), EvalError> {
    let mut pre_seeded: HashSet<String> = HashSet::new();
    for (name, var) in dae
        .variables
        .discrete_reals
        .iter()
        .chain(dae.variables.discrete_valued.iter())
    {
        if env.vars.contains_key(name.as_str()) {
            continue;
        }
        if try_seed_var_from_pre_store(env, name.as_str(), var) {
            pre_seeded.insert(name.as_str().to_string());
        }
    }

    let vars = dae
        .variables
        .discrete_reals
        .iter()
        .chain(dae.variables.discrete_valued.iter())
        .filter(|(name, _)| !pre_seeded.contains(name.as_str()))
        .collect::<Vec<_>>();
    seed_default_discrete_values(env, &vars);
    bind_start_values_until_stable(env, &vars, false, vars.len().clamp(1, 8))?;

    for (name, var) in dae
        .variables
        .discrete_reals
        .iter()
        .chain(dae.variables.discrete_valued.iter())
    {
        if env.vars.contains_key(name.as_str()) {
            continue;
        }
        set_default_var_value(env, name.as_str(), var);
    }
    Ok(())
}

fn seed_default_discrete_values(
    env: &mut VarEnv<f64>,
    vars: &[(&VarName, &rumoca_ir_dae::Variable)],
) {
    for (name, var) in vars {
        if var.start.is_some() || env.vars.contains_key(name.as_str()) {
            continue;
        }
        set_default_var_value(env, name.as_str(), var);
    }
}

fn set_default_var_value(env: &mut VarEnv<f64>, name: &str, var: &rumoca_ir_dae::Variable) {
    let size = var.size();
    if var.dims.is_empty() {
        env.set(name, 0.0);
    } else {
        let zeros = vec![0.0; size];
        set_array_entries(env, name, &var.dims, zeros.as_slice());
    }
}

/// Collect variable dimensions from all variable categories in the DAE.
pub fn collect_var_dims(dae: &Dae) -> IndexMap<String, Vec<i64>> {
    let mut map = IndexMap::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
        .chain(dae.variables.discrete_reals.iter())
        .chain(dae.variables.discrete_valued.iter())
    {
        if !var.dims.is_empty() {
            map.insert(name.as_str().to_string(), var.dims.clone());
        }
    }
    map
}

/// Collect start expressions from all variable categories in the DAE.
pub fn collect_var_starts(dae: &Dae) -> IndexMap<String, rumoca_core::Expression> {
    let mut map = IndexMap::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
        .chain(dae.variables.discrete_reals.iter())
        .chain(dae.variables.discrete_valued.iter())
    {
        if let Some(start) = &var.start {
            map.insert(name.as_str().to_string(), start.clone());
        }
    }
    map
}

/// Collect lowered user function bodies for runtime function calls.
pub fn collect_user_functions(dae: &Dae) -> IndexMap<String, rumoca_core::Function> {
    dae.symbols
        .functions
        .iter()
        .map(|(name, func)| (name.as_str().to_string(), func.clone()))
        .collect()
}

/// Lift an f64 environment to a generic SimFloat environment.
///
/// All values are converted with `T::from_f64()`, so dual parts start at 0.
/// rumoca_core::Function definitions are shared via Arc (zero-copy).
pub fn lift_env<T: SimFloat>(env: &VarEnv<f64>) -> VarEnv<T> {
    let mut result = VarEnv::new();
    for (name, &val) in &env.vars {
        result.vars.insert(name.clone(), T::from_f64(val));
    }
    result.runtime = env.runtime.clone();
    result.functions = env.functions.clone();
    result.dims = env.dims.clone();
    result.start_exprs = env.start_exprs.clone();
    result.clock_intervals = env.clock_intervals.clone();
    result.enum_literal_ordinals = env.enum_literal_ordinals.clone();
    result.is_initial = env.is_initial;
    result
}

/// Evaluate a constant expression (for start values, parameter defaults).
pub fn eval_const_expr(expr: &rumoca_core::Expression) -> Result<f64, EvalError> {
    eval_expr::<f64>(expr, &VarEnv::new())
}

pub fn external_table_data_for_parameter_values(
    values: &[f64],
) -> Vec<rumoca_core::ExternalTableData> {
    external_table_data_for_values(values)
}

pub fn external_table_data_for_parameter_values_in<T: SimFloat>(
    env: &VarEnv<T>,
    values: &[f64],
) -> Vec<rumoca_core::ExternalTableData> {
    external_table_data_for_values_in(&env.runtime.external_tables, values)
}

#[cfg(test)]
mod tests;
