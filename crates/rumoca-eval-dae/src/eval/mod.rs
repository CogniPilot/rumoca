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
    eval_field_access_array_values, eval_unary_builtin_array_values, flattened_field_access_name,
    infer_dims_from_values, try_eval_field_access_array_values,
};
use builtin_table::{eval_builtin_product, eval_builtin_sum};
// Public for `rumoca-jit-dae` — the JIT emits calls into these runtime
// helpers when generating machine code for table-lookup expressions.
pub use builtin_table::{
    eval_table_bound_value_in, eval_table_lookup_slope_value_in, eval_table_lookup_value_in,
    eval_time_table_next_event_value_in, try_eval_table_bound_value_in,
    try_eval_table_lookup_slope_value_in, try_eval_table_lookup_value_in,
    try_eval_time_table_next_event_value_in,
};
pub use clock_eval::infer_clock_timing_seconds;
use clock_eval::{
    clock_tick_value, eval_builtin_sample, eval_time_seconds, infer_clock_timing_from_call,
    infer_clock_timing_from_expr,
};
use table_eval::{
    eval_external_table_data_matrix, eval_table_1d_lookup, eval_table_1d_lookup_with_runtime,
    eval_table_constructor, table_x_bounds,
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
pub use table_eval::resolve_modelica_resource_path;

mod special;
pub use special::{
    deterministic_automatic_global_seed, eval_builtin_pub, eval_condition_as_root,
    eval_function_call_pub, eval_function_call_pub_dae, eval_selected_function_output_pub,
    eval_selected_function_output_pub_dae, eval_user_function_array_output_pub,
    eval_user_function_output_path_pub, is_runtime_special_function_name,
    is_runtime_special_function_short_name, modelica_strings_hash_string,
    resolve_function_call_outputs_pub, resolve_function_call_outputs_pub_dae,
};
use special::{eval_function_call, eval_if, function_closure_from_arg};
mod eval_expr_impl;
use array_eval::{
    declared_dims_or_scalar, eval_array_like_f64_values, eval_array_like_values,
    eval_binary_array_values, eval_cat_f64_values, eval_columns_arg, eval_cross_values,
    eval_linspace_values, eval_matrix_index, eval_matrix_literal_rows, eval_outer_product_values,
    eval_skew_values, eval_symmetric_values, eval_transpose_values, infer_runtime_expr_dims,
    reshape_flat_matrix, try_eval_array_like_values,
};
pub use array_eval::{eval_array_values, eval_matrix_values, eval_shaped_array_values};
pub(crate) use eval_expr_impl::eval_expr_or_default;
use eval_expr_impl::*;
pub use eval_expr_impl::{EvalError, eval_expr};
mod runtime_env;
pub use runtime_env::{
    clear_pre_values, clear_pre_values_in_env_runtime, clear_runtime_state,
    clear_runtime_state_in_env_runtime, get_pre_value, get_pre_value_from_env, restore_pre_values,
    restore_pre_values_in_env_runtime, restore_pre_values_in_runtime, seed_pre_values_from_env,
    seed_pre_values_in_env_runtime, set_pre_value, set_pre_value_in_env, set_pre_value_in_runtime,
    snapshot_pre_values, snapshot_pre_values_from_env, snapshot_pre_values_from_runtime,
    try_build_env, try_build_env_with_runtime, try_build_runtime_parameter_tail_env,
    try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    try_build_runtime_parameter_tail_env_with_runtime, try_refresh_env_solver_and_parameter_values,
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
    StartExprPrefix {
        map_addr: usize,
        count: usize,
        prefix: String,
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
        || (1..=count).map(|idx| format!("{name}[{idx}]")).collect(),
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
                .map(|idx| format!("{base}[{idx}].{field}"))
                .collect()
        },
    )
}

pub(crate) fn prefixed_start_expr_keys<T: SimFloat>(
    env: &VarEnv<T>,
    prefix: &str,
) -> Arc<[String]> {
    cached_env_keys(
        &env.runtime,
        EnvKeyCacheKey::StartExprPrefix {
            map_addr: Arc::as_ptr(&env.start_exprs) as usize,
            count: env.start_exprs.len(),
            prefix: prefix.to_string(),
        },
        || {
            env.start_exprs
                .keys()
                .filter(|key| key.starts_with(prefix))
                .cloned()
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
                .map(|idx| format!("{base}.{field}[{idx}]"))
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
    local_prefix_cache: RefCell<HashMap<String, Arc<[String]>>>,
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
                local_prefix_cache: RefCell::new(HashMap::new()),
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
                local_prefix_cache: RefCell::new(HashMap::new()),
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

    pub fn prefixed_entries<'a>(&'a self, prefix: &str) -> Vec<(&'a String, &'a T)> {
        let mut entries = Vec::new();
        let mut seen = HashSet::new();
        self.collect_prefixed_entries(prefix, &mut entries, &mut seen);
        entries.reverse();
        entries
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

    fn collect_prefixed_entries<'a>(
        &'a self,
        prefix: &str,
        entries: &mut Vec<(&'a String, &'a T)>,
        seen: &mut HashSet<&'a str>,
    ) {
        for name in self.frame.local_prefixed_keys(prefix).iter() {
            if let Some((stored_name, value)) = self.frame.local.get_key_value(name.as_str())
                && seen.insert(stored_name.as_str())
            {
                entries.push((stored_name, value));
            }
        }
        if let Some(parent) = &self.frame.parent {
            parent.collect_prefixed_entries(prefix, entries, seen);
        }
    }
}

impl<T> VarScopeFrame<T> {
    fn local_prefixed_keys(&self, prefix: &str) -> Arc<[String]> {
        if let Some(keys) = self.local_prefix_cache.borrow().get(prefix) {
            return keys.clone();
        }
        let keys: Arc<[String]> = self
            .local
            .keys()
            .rev()
            .filter(|name| name.starts_with(prefix))
            .cloned()
            .collect::<Vec<_>>()
            .into();
        self.local_prefix_cache
            .borrow_mut()
            .insert(prefix.to_string(), keys.clone());
        keys
    }

    fn clear_local_prefix_cache(&self) {
        self.local_prefix_cache.borrow_mut().clear();
    }
}

impl<T: Clone> VarScope<T> {
    pub fn insert(&mut self, name: String, value: T) -> Option<T> {
        let frame = Arc::make_mut(&mut self.frame);
        frame.clear_local_prefix_cache();
        frame.local.insert(name, value)
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = (String, T)>,
    {
        let frame = Arc::make_mut(&mut self.frame);
        frame.clear_local_prefix_cache();
        frame.local.extend(iter);
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
    pub start_expr_refs: Arc<Vec<String>>,
    pub start_alias_sources: Arc<IndexMap<String, Vec<String>>>,
    pub fixed_attrs: Arc<IndexMap<String, Option<bool>>>,
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
            start_expr_refs: self.start_expr_refs.clone(),
            start_alias_sources: self.start_alias_sources.clone(),
            fixed_attrs: self.fixed_attrs.clone(),
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
            start_expr_refs: Arc::new(Vec::new()),
            start_alias_sources: Arc::new(IndexMap::new()),
            fixed_attrs: Arc::new(IndexMap::new()),
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

    pub fn get(&self, name: &str) -> T {
        self.vars.get(name).copied().unwrap_or(T::zero())
    }

    pub fn set(&mut self, name: &str, value: T) {
        set_env_key(self, name, value);
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
pub fn eval_condition_truth<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> bool {
    match expr {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| eval_condition_truth(element, env)),
        _ => eval_expr_or_default::<T>(expr, env).to_bool(),
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
    match eval_expr(expr, env) {
        Ok(value) => Ok(value),
        Err(_) if is_constructor_selection_value(expr) => Ok(eval_expr_or_default(expr, env)),
        Err(err) => Err(err),
    }
}

fn is_constructor_selection_value(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: true,
            ..
        } if name.as_str() == "Complex"
    )
}

/// Internal runtime environment key used to evaluate implicit `Clock()`
/// conditions inside guarded when-equations.
pub const IMPLICIT_CLOCK_ACTIVE_ENV_KEY: &str = "__rumoca_implicit_clock_active";

pub(super) fn previous_start_or_default<T: SimFloat>(
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> T {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg
    else {
        return T::zero();
    };

    let key = if subscripts.is_empty() {
        name.as_str().to_string()
    } else {
        let indices = eval_subscript_indices(subscripts, env);
        format!("{}[{}]", name.as_str(), indices.join(","))
    };

    if let Some(start) = env.start_exprs.get(key.as_str()) {
        return eval_expr_or_default::<T>(start, env);
    }
    if let Some(normalized) = normalize_var_name::<T>(&key, env)
        && let Some(start) = env.start_exprs.get(normalized.as_str())
    {
        return eval_expr_or_default::<T>(start, env);
    }
    if let Some(base_name) = unity_subscript_base_name(&key)
        && let Some(start) = env.start_exprs.get(base_name.as_str())
    {
        return eval_expr_or_default::<T>(start, env);
    }
    T::zero()
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
            values.push(value);
            found_any = true;
        } else {
            values.push(f64::NAN);
        }
    }

    let fallback = lookup_pre_value_in(&env.runtime, target_name)
        .or_else(|| values.iter().copied().find(|v| v.is_finite()));
    if !found_any && fallback.is_none() {
        return false;
    }

    let fill = fallback.unwrap_or_else(pre_store_array_fill_default);
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

/// Build a variable environment from the DAE and current state vector (f64-only).
///
/// The combined state vector `y` is `[x; z; y_out]` where x = states,
/// z = algebraics, y_out = outputs. `p` contains parameter values.
pub fn build_env(dae: &Dae, y: &[f64], p: &[f64], t: f64) -> VarEnv<f64> {
    let mut env = VarEnv::new();
    populate_env(dae, y, p, t, &mut env);
    env
}

pub fn build_env_with_runtime(
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> VarEnv<f64> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    populate_env(dae, y, p, t, &mut env);
    env
}

fn populate_env(dae: &Dae, y: &[f64], p: &[f64], t: f64, env: &mut VarEnv<f64>) {
    try_populate_env_values(dae, y, p, t, env).expect("DAE environment population failed");
}

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

/// Build only the parameter/input/discrete runtime tail for a DAE env.
///
/// This excludes solver-vector slots (`states`, `algebraics`, `outputs`) and
/// is intended for callers that only need runtime tail bindings such as input
/// or discrete start values.
pub fn build_runtime_parameter_tail_env(dae: &Dae, p: &[f64], t: f64) -> VarEnv<f64> {
    let mut env = VarEnv::new();
    populate_runtime_parameter_tail_env(dae, p, t, &mut env);
    env
}

pub fn build_runtime_parameter_tail_env_with_runtime(
    dae: &Dae,
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> VarEnv<f64> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    populate_runtime_parameter_tail_env(dae, p, t, &mut env);
    env
}

pub fn try_build_partial_runtime_parameter_tail_env_with_runtime(
    dae: &Dae,
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    try_populate_runtime_parameter_tail_env_values(dae, p, t, &mut env)?;
    Ok(env)
}

pub fn try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
    dae: &Dae,
    p: &[f64],
    t: f64,
    runtime: Arc<EvalRuntimeState>,
) -> Result<VarEnv<f64>, EvalError> {
    let mut env = VarEnv::new();
    env.runtime = runtime;
    try_populate_runtime_parameter_tail_env_values_with_policy(
        dae,
        p,
        t,
        &mut env,
        ParameterSlotPolicy::Declared,
    )?;
    Ok(env)
}

fn populate_runtime_parameter_tail_env(dae: &Dae, p: &[f64], t: f64, env: &mut VarEnv<f64>) {
    try_populate_runtime_parameter_tail_env_values(dae, p, t, env)
        .expect("DAE runtime parameter-tail environment population failed");
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
    bind_runtime_start_aliases(env);
    bind_inputs(env, dae)?;
    seed_discrete_values(env, dae)
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
    if is_known_string_path(var.name.as_str()) {
        return false;
    }
    var.start.as_ref().is_none_or(|start| {
        if start_expr_is_nonnumeric(start, env) {
            return false;
        }
        let start_dims = infer_runtime_expr_dims(start, env);
        if start_dims.len() == var.dims.len() && start_dims.contains(&0) {
            return false;
        }
        true
    })
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
    env.start_expr_refs = Arc::new(collect_var_start_refs(env.start_exprs.values()));
    env.start_alias_sources = Arc::new(collect_start_alias_sources(&env.start_expr_refs));
    env.fixed_attrs = Arc::new(collect_var_fixed_attrs(dae));
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
    if is_known_string_path(name) {
        return Ok(());
    }
    let Some(start) = var.start.as_ref() else {
        if let Some(value) = numbered_volume_modifier_alias(name, env) {
            env.set(name, value);
        }
        return Ok(());
    };
    let binding_dims =
        runtime_binding_dims(name, var, start, env).unwrap_or_else(|| var.dims.clone());
    if binding_dims != var.dims {
        Arc::make_mut(&mut env.dims).insert(name.to_string(), binding_dims.clone());
    }
    let size = binding_size(var, &binding_dims);
    if size == 0 && !binding_dims.is_empty() {
        if has_explicit_zero_dim(&binding_dims) {
            return Ok(());
        }
        // MLS Chapter 10: arrays with an unknown dimension can still have a
        // parameter binding that materializes at evaluation time. Preserve the
        // aggregate values in the environment instead of collapsing the binding
        // to one scalar placeholder.
        let values = if var.dims.len() >= 2 {
            eval_matrix_values(start, env)
                .map(|matrix| matrix.into_iter().flatten().collect())
                .unwrap_or_else(|| eval_array_values::<f64>(start, env))
        } else {
            eval_array_values::<f64>(start, env)
        };
        if !values.is_empty() {
            set_array_entries(env, name, &binding_dims, &values);
        }
        return Ok(());
    }
    if start_expr_is_nonnumeric(start, env) {
        return Ok(());
    }
    if size <= 1 && var.dims.is_empty() {
        let value = match projected_record_array_field_value(name, start, env)
            .map_or_else(|| eval_expr::<f64>(start, env), Ok)
        {
            Ok(value) => value,
            Err(EvalError::UnsupportedExpression { .. })
                if start_may_materialize_array(start, env) =>
            {
                let values = eval_array_values::<f64>(start, env);
                if values.len() == 1 {
                    values[0]
                } else {
                    return Err(EvalError::ShapeMismatch {
                        context: "scalar start value",
                        expected: 1,
                        actual: values.len(),
                    });
                }
            }
            Err(err) => return Err(err),
        };
        env.set(name, value);
        return Ok(());
    }
    if size <= 1 {
        let values = match eval_shaped_array_values::<f64>(start, env, 1) {
            Ok(values) => values,
            Err(EvalError::ShapeMismatch { .. }) => {
                let values = eval_array_values::<f64>(start, env);
                if values.len() == 1 {
                    values
                } else {
                    component_array_literal_member_start_values(name, start, env, 1)
                        .or_else(|| component_array_row_start_values(name, start, env, 1))
                        .ok_or(EvalError::ShapeMismatch {
                            context: "shaped array value",
                            expected: 1,
                            actual: values.len(),
                        })?
                }
            }
            Err(EvalError::UnsupportedExpression { .. })
                if start_may_materialize_array(start, env) =>
            {
                let values = eval_array_values::<f64>(start, env);
                if values.len() == 1 {
                    values
                } else {
                    return Err(EvalError::ShapeMismatch {
                        context: "shaped array value",
                        expected: 1,
                        actual: values.len(),
                    });
                }
            }
            Err(err) => return Err(err),
        };
        set_array_entries(env, name, &binding_dims, &values);
        return Ok(());
    }

    let values = match eval_shaped_array_values::<f64>(start, env, size) {
        Ok(values) => values,
        Err(EvalError::ShapeMismatch { actual: 1, .. }) => {
            if let Some(values) =
                component_array_literal_member_start_values(name, start, env, size)
                    .or_else(|| component_array_row_start_values(name, start, env, size))
            {
                values
            } else {
                let values = eval_array_values::<f64>(start, env);
                if values.len() == size {
                    values
                } else {
                    vec![eval_expr::<f64>(start, env)?; size]
                }
            }
        }
        Err(EvalError::ShapeMismatch { .. }) => {
            component_array_literal_member_start_values(name, start, env, size)
                .or_else(|| component_array_row_start_values(name, start, env, size))
                .ok_or_else(|| {
                    let actual = eval_array_values::<f64>(start, env).len();
                    trace_runtime_shape_mismatch(name, var, start, size, actual);
                    EvalError::ShapeMismatch {
                        context: "shaped array value",
                        expected: size,
                        actual,
                    }
                })?
        }
        Err(err) => return Err(err),
    };
    set_array_entries(env, name, &binding_dims, &values);
    Ok(())
}

fn trace_runtime_shape_mismatch(
    name: &str,
    var: &rumoca_ir_dae::Variable,
    start: &rumoca_core::Expression,
    expected: usize,
    actual: usize,
) {
    if std::env::var_os("RUMOCA_TRACE_RUNTIME_SHAPE_MISMATCH").is_none() {
        return;
    }
    eprintln!(
        "runtime shape mismatch for {name}: expected={expected} actual={actual} var_dims={:?} start={start:#?}",
        var.dims
    );
}

fn binding_size(var: &rumoca_ir_dae::Variable, dims: &[i64]) -> usize {
    if dims.is_empty() {
        return var.size();
    }
    dims.iter()
        .map(|dim| usize::try_from(*dim).ok().unwrap_or(0))
        .product()
}

fn has_explicit_zero_dim(dims: &[i64]) -> bool {
    dims.contains(&0)
}

fn runtime_binding_dims(
    name: &str,
    var: &rumoca_ir_dae::Variable,
    start: &rumoca_core::Expression,
    env: &VarEnv<f64>,
) -> Option<Vec<i64>> {
    let start_dims = infer_runtime_expr_dims(start, env);
    if start_dims.len() == var.dims.len() && start_dims.contains(&0) {
        let runtime_dims = start_dims
            .iter()
            .map(|dim| i64::try_from(*dim).ok())
            .collect::<Option<Vec<_>>>()?;
        if runtime_dims != var.dims {
            return Some(runtime_dims);
        }
    }
    if var.dims.len() != 1 {
        return None;
    }
    let (base, _) = name.rsplit_once('.')?;
    let n_name = format!("{base}.n");
    let start_len = eval_array_values::<f64>(start, env).len();
    if let Some(raw) = env
        .vars
        .get(n_name.as_str())
        .copied()
        .map(|value| value.real())
    {
        if raw.is_finite() && raw >= 0.0 && raw.fract() == 0.0 {
            let dim = raw as i64;
            if usize::try_from(dim).ok()? == start_len && dim != var.dims[0] {
                return Some(vec![dim]);
            }
        }
    }
    let static_size = var.size();
    if static_size > 0
        && (component_array_literal_member_start_values(name, start, env, static_size).is_some()
            || component_array_row_start_values(name, start, env, static_size).is_some())
    {
        return None;
    }
    if start_may_materialize_array(start, env) && start_len > 0 && start_len != var.dims[0] as usize
    {
        return i64::try_from(start_len).ok().map(|len| vec![len]);
    }
    None
}

fn start_may_materialize_array(expr: &rumoca_core::Expression, env: &VarEnv<f64>) -> bool {
    match expr {
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => true,
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            subscripts.is_empty()
                && env
                    .dims
                    .get(name.as_str())
                    .is_some_and(|dims| !dims.is_empty())
        }
        rumoca_core::Expression::FieldAccess { base, .. } => start_may_materialize_array(base, env),
        rumoca_core::Expression::Unary { rhs, .. } => start_may_materialize_array(rhs, env),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            start_may_materialize_array(lhs, env) || start_may_materialize_array(rhs, env)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .any(|(_, value)| start_may_materialize_array(value, env))
                || start_may_materialize_array(else_branch, env)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            args.iter().any(|arg| start_may_materialize_array(arg, env))
        }
        rumoca_core::Expression::Index { base, .. } => start_may_materialize_array(base, env),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}

fn component_array_literal_member_start_values(
    target_name: &str,
    start: &rumoca_core::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    let rumoca_core::Expression::Array { elements, .. } = start else {
        return None;
    };
    let row_idx = last_component_array_index(target_name)?;
    let element = elements.get(row_idx.checked_sub(1)?)?;
    eval_shaped_array_values::<f64>(element, env, expected_len)
        .ok()
        .or_else(|| component_array_row_start_values(target_name, element, env, expected_len))
}

fn projected_record_array_field_value(
    target_name: &str,
    start: &rumoca_core::Expression,
    env: &VarEnv<f64>,
) -> Option<f64> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = start else {
        return None;
    };
    let rumoca_core::Expression::VarRef {
        name: source_base,
        subscripts,
        ..
    } = base.as_ref()
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let component_idx = last_component_array_index(target_name)?;
    let (source_owner, record_name) = source_base.as_str().rsplit_once('.')?;
    let (parent_owner, _) = source_owner.rsplit_once('.')?;
    let parent_record_key = format!("{parent_owner}.{record_name}[{component_idx}].{field}");
    eval_projected_start_value(parent_record_key.as_str(), env, 0).or_else(|| {
        let sibling_element_key = format!("{source_owner}.per[{component_idx}].{field}");
        eval_projected_start_value(sibling_element_key.as_str(), env, 0)
    })
}

fn eval_projected_start_value(name: &str, env: &VarEnv<f64>, depth: usize) -> Option<f64> {
    if let Some(value) = env.vars.get(name).copied() {
        return Some(value);
    }
    if depth >= 16 {
        return None;
    }
    let start = env.start_exprs.get(name)?;
    match start {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => eval_projected_start_value(name.as_str(), env, depth + 1),
        _ => eval_expr::<f64>(start, env).ok(),
    }
}

fn numbered_volume_modifier_alias(name: &str, env: &VarEnv<f64>) -> Option<f64> {
    if let Some(rest) = name.strip_suffix(".m_flow_nominal") {
        let (owner, side) = numbered_volume_owner_and_side(rest)?;
        let source = format!("{owner}.m{side}_flow_nominal");
        return env.vars.get(source.as_str()).copied();
    }
    let rest = name.strip_suffix(".V")?;
    let (owner, side) = numbered_volume_owner_and_side(rest)?;
    let m_flow = env
        .vars
        .get(format!("{owner}.m{side}_flow_nominal").as_str())?;
    let tau = env.vars.get(format!("{owner}.tau{side}").as_str())?;
    let rho = env
        .vars
        .get(format!("{owner}.rho{side}_nominal").as_str())?;
    Some(*m_flow * *tau / *rho)
}

fn numbered_volume_owner_and_side(rest: &str) -> Option<(&str, &str)> {
    let (owner, component) = rest.rsplit_once('.')?;
    let side = component.strip_prefix("vol")?;
    if side.is_empty() || !side.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }
    Some((owner, side))
}

fn component_array_row_start_values(
    target_name: &str,
    start: &rumoca_core::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    let rumoca_core::Expression::VarRef {
        name: source_name,
        subscripts,
        ..
    } = start
    else {
        return None;
    };
    let source_dims = env.dims.get(source_name.as_str())?;
    if source_dims.len() != 2 {
        return None;
    }
    let rows = usize::try_from(source_dims[0])
        .ok()
        .filter(|rows| *rows > 0)?;
    let cols = usize::try_from(source_dims[1])
        .ok()
        .filter(|cols| *cols > 0)?;
    if cols != expected_len {
        return None;
    }
    let row_idx = if subscripts.is_empty() {
        last_component_array_index(target_name)?
    } else {
        matrix_row_subscript_index(subscripts, env)?
    };
    if row_idx == 0 || row_idx > rows {
        return None;
    }
    let source_expr = rumoca_core::Expression::VarRef {
        name: source_name.clone(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    };
    let values = eval_array_values::<f64>(&source_expr, env);
    if values.len() != rows.checked_mul(cols)? {
        return None;
    }
    let start_idx = (row_idx - 1).checked_mul(cols)?;
    Some(values[start_idx..start_idx + cols].to_vec())
}

fn matrix_row_subscript_index(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<f64>,
) -> Option<usize> {
    if subscripts.len() != 1 {
        return None;
    }
    match &subscripts[0] {
        rumoca_core::Subscript::Index { value, .. } => {
            usize::try_from(*value).ok().filter(|value| *value > 0)
        }
        rumoca_core::Subscript::Expr { expr, .. } => {
            let raw = eval_expr::<f64>(expr, env).ok()?.real();
            if !raw.is_finite() || raw < 1.0 || raw.fract() != 0.0 {
                return None;
            }
            Some(raw as usize)
        }
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn last_component_array_index(name: &str) -> Option<usize> {
    name.match_indices('[')
        .filter_map(|(start, _)| {
            let rest = &name[start + 1..];
            let end = rest.find(']')?;
            rest[..end].parse::<usize>().ok()
        })
        .last()
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
            if is_known_string_function(name.as_str()) {
                return true;
            }
            !is_runtime_special_function_name(name.as_str())
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
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            is_known_string_field(field)
                || start_expr_is_nonnumeric_inner(base, env, visited)
                || field_access_start_is_nonnumeric(base, field, env, visited)
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
        } => {
            let key = name.as_str();
            if is_known_string_path(key) {
                return true;
            }
            if !subscripts.is_empty() {
                return false;
            }
            if env.enum_literal_ordinals.contains_key(key) || !visited.insert(key.to_string()) {
                return false;
            }
            env.start_exprs
                .get(key)
                .is_some_and(|start| start_expr_is_nonnumeric_inner(start, env, visited))
        }
        rumoca_core::Expression::Empty { .. } => false,
    }
}

fn field_access_start_is_nonnumeric(
    base: &rumoca_core::Expression,
    field: &str,
    env: &VarEnv<f64>,
    visited: &mut HashSet<String>,
) -> bool {
    let Some(path) = start_expr_field_access_path(base, field, env) else {
        return false;
    };
    if !visited.insert(path.clone()) {
        return false;
    }
    env.start_exprs
        .get(path.as_str())
        .is_some_and(|start| start_expr_is_nonnumeric_inner(start, env, visited))
}

fn start_expr_field_access_path(
    base: &rumoca_core::Expression,
    field: &str,
    env: &VarEnv<f64>,
) -> Option<String> {
    let prefix = start_expr_path(base, env)?;
    Some(format!("{prefix}.{field}"))
}

fn start_expr_path(expr: &rumoca_core::Expression, env: &VarEnv<f64>) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                Some(name.as_str().to_string())
            } else {
                let idx = eval_subscript_indices(subscripts, env);
                Some(format!("{}[{}]", name.as_str(), idx.join(",")))
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let prefix = start_expr_path(base, env)?;
            let idx = eval_subscript_indices(subscripts, env);
            Some(format!("{prefix}[{}]", idx.join(",")))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            start_expr_field_access_path(base, field, env)
        }
        _ => None,
    }
}

fn is_known_string_field(field: &str) -> bool {
    matches!(
        field,
        "buildingsRootFileLocation"
            | "epName"
            | "epwName"
            | "fmuName"
            | "idfName"
            | "idfVersion"
            | "inpNames"
            | "inpUnits"
            | "jsonKeysValues"
            | "jsonName"
            | "modelicaInstanceName"
            | "modelicaName"
            | "modelicaNameBuilding"
            | "modelicaNameThermalZone"
            | "outNames"
            | "outUnits"
            | "parOutNames"
            | "parOutUnits"
            | "spawnExe"
            | "weaName"
            | "zoneName"
    )
}

fn is_known_string_path(path: &str) -> bool {
    let terminal = path
        .rsplit('.')
        .next()
        .unwrap_or(path)
        .split('[')
        .next()
        .unwrap_or(path);
    is_known_string_field(terminal)
}

fn is_known_string_function(name: &str) -> bool {
    matches!(name, "getInstanceName") || name.ends_with(".getInstanceName")
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
    let mut pending_missing_owner = None;
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
                    if let Some(missing) = err.missing_binding_name()
                        && bind_enclosing_start_alias(env, missing, &mut HashSet::new()).is_some()
                    {
                        changed = true;
                        continue;
                    }
                    if let Some(missing) = err.missing_binding_name()
                        && seed_missing_indexed_parameter_default(env, vars, missing)
                    {
                        changed = true;
                        continue;
                    }
                    if err.missing_binding_name().is_some_and(|missing| {
                        seed_fixed_false_missing_default(env, vars, missing)
                            || missing_binding_is_fixed_false(vars, missing)
                    }) {
                        set_default_var_value(env, name.as_str(), var);
                        changed = true;
                        continue;
                    }
                    pending_missing.get_or_insert(err);
                    pending_missing_owner.get_or_insert_with(|| name.as_str().to_string());
                    continue;
                }
                Err(err) => {
                    trace_runtime_start_error(name.as_str(), var, &err);
                    let span = var
                        .start
                        .as_ref()
                        .and_then(|expr| expr.span())
                        .unwrap_or(var.source_span);
                    return Err(err.with_fallback_span(span));
                }
            }
            changed |= env.vars.len() != before_len || env.vars.contains_key(name.as_str());
        }
        if !changed {
            break;
        }
    }
    match pending_missing {
        Some(err) => {
            trace_pending_runtime_start_missing(env, pending_missing_owner.as_deref(), &err);
            Err(err)
        }
        None => Ok(()),
    }
}

fn seed_missing_indexed_parameter_default(
    env: &mut VarEnv<f64>,
    vars: &[(&VarName, &rumoca_ir_dae::Variable)],
    missing: &str,
) -> bool {
    let Some(base) = indexed_missing_base_name(missing) else {
        return false;
    };
    if env.vars.contains_key(missing) {
        return true;
    }
    let Some((_, var)) = vars.iter().find(|(name, _)| name.as_str() == base) else {
        return false;
    };
    if var.start.is_some()
        && bind_start_value(env, base, var).is_ok()
        && env.vars.contains_key(missing)
    {
        return env.vars.contains_key(missing);
    }
    set_default_var_value(env, base, var);
    env.vars.contains_key(missing)
}

fn indexed_missing_base_name(missing: &str) -> Option<&str> {
    let (base, index) = missing.rsplit_once('[')?;
    (!base.is_empty() && index.ends_with(']')).then_some(base)
}

fn missing_binding_is_fixed_false(
    vars: &[(&VarName, &rumoca_ir_dae::Variable)],
    missing: &str,
) -> bool {
    vars.iter()
        .any(|(name, var)| name.as_str() == missing && var.fixed == Some(false))
}

fn seed_fixed_false_missing_default(
    env: &mut VarEnv<f64>,
    vars: &[(&VarName, &rumoca_ir_dae::Variable)],
    missing: &str,
) -> bool {
    if env.vars.contains_key(missing) {
        return true;
    }
    if let Some((_, var)) = vars
        .iter()
        .find(|(name, var)| name.as_str() == missing && var.fixed == Some(false))
    {
        set_default_var_value(env, missing, var);
        return true;
    }
    if !env
        .fixed_attrs
        .get(missing)
        .is_some_and(|fixed| *fixed == Some(false))
    {
        return false;
    }
    if let Some(dims) = env.dims.get(missing).cloned() {
        let size = dims
            .iter()
            .try_fold(1usize, |acc, dim| {
                usize::try_from(*dim)
                    .ok()
                    .filter(|value| *value > 0)
                    .and_then(|value| acc.checked_mul(value))
            })
            .unwrap_or(0);
        set_array_entries(env, missing, &dims, vec![0.0; size].as_slice());
    } else {
        env.set(missing, 0.0);
    }
    true
}

fn bind_enclosing_start_alias(
    env: &mut VarEnv<f64>,
    missing: &str,
    visited: &mut HashSet<String>,
) -> Option<f64> {
    if let Some(value) = env.vars.get(missing).copied() {
        return Some(value);
    }
    let (owner, field) = missing.rsplit_once('.')?;
    let mut cursor = owner;
    while let Some((parent, _)) = cursor.rsplit_once('.') {
        let source = format!("{parent}.{field}");
        if let Some(value) = bind_start_key(env, source.as_str(), visited) {
            env.set(missing, value);
            return Some(value);
        }
        cursor = parent;
    }
    None
}

pub fn bind_runtime_start_aliases(env: &mut VarEnv<f64>) {
    ensure_runtime_start_alias_sources(env);
    for _ in 0..8 {
        let source_index = env.start_alias_sources.clone();
        if source_index.is_empty() {
            break;
        }
        let mut updates = Vec::new();
        for (source, targets) in source_index.iter() {
            let Some(value) = env.vars.get(source.as_str()).copied() else {
                continue;
            };
            for target in targets {
                if env.vars.get(target.as_str()).copied() == Some(value) {
                    continue;
                }
                updates.push((target.clone(), value));
            }
        }
        if updates.is_empty() {
            break;
        }
        for (target, value) in updates {
            env.set(&target, value);
        }
    }
}

pub fn bind_runtime_start_aliases_from_sources(
    env: &mut VarEnv<f64>,
    sources: impl IntoIterator<Item = String>,
) {
    ensure_runtime_start_alias_sources(env);
    let source_index = env.start_alias_sources.clone();
    if source_index.is_empty() {
        return;
    }

    let mut pending = sources.into_iter().collect::<Vec<_>>();
    let mut visited = HashSet::new();
    while let Some(source) = pending.pop() {
        if !visited.insert(source.clone()) {
            continue;
        }
        let Some(value) = env.vars.get(source.as_str()).copied() else {
            continue;
        };
        let Some(targets) = source_index.get(source.as_str()) else {
            continue;
        };
        for target in targets {
            if env.vars.get(target.as_str()).copied() == Some(value) {
                continue;
            }
            env.set(target, value);
            pending.push(target.clone());
        }
    }
}

fn ensure_runtime_start_alias_sources(env: &mut VarEnv<f64>) {
    if env.start_expr_refs.is_empty() && !env.start_exprs.is_empty() {
        env.start_expr_refs = Arc::new(collect_var_start_refs(env.start_exprs.values()));
        env.start_alias_sources = Arc::new(collect_start_alias_sources(&env.start_expr_refs));
    }
}

fn collect_start_expr_var_refs(expr: &rumoca_core::Expression, refs: &mut Vec<String>) {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                refs.push(name.as_str().to_string());
            }
            for subscript in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
                    collect_start_expr_var_refs(expr, refs);
                }
            }
        }
        rumoca_core::Expression::Unary { rhs, .. } => collect_start_expr_var_refs(rhs, refs),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_start_expr_var_refs(lhs, refs);
            collect_start_expr_var_refs(rhs, refs);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            for arg in args {
                collect_start_expr_var_refs(arg, refs);
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_start_expr_var_refs(condition, refs);
                collect_start_expr_var_refs(value, refs);
            }
            collect_start_expr_var_refs(else_branch, refs);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_start_expr_var_refs(expr, refs);
            for index in indices {
                collect_start_expr_var_refs(&index.range, refs);
            }
            if let Some(filter) = filter {
                collect_start_expr_var_refs(filter, refs);
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            if let Some(path) = static_start_expr_path(expr) {
                refs.push(path);
            }
            collect_start_expr_var_refs(base, refs);
            for subscript in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
                    collect_start_expr_var_refs(expr, refs);
                }
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            if let Some(path) = static_start_expr_path(expr) {
                refs.push(path);
            }
            collect_start_expr_var_refs(base, refs);
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_start_expr_var_refs(start, refs);
            if let Some(step) = step {
                collect_start_expr_var_refs(step, refs);
            }
            collect_start_expr_var_refs(end, refs);
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn static_start_expr_path(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Some(name.as_str().to_string());
            }
            Some(format!(
                "{}[{}]",
                name.as_str(),
                static_subscript_indices(subscripts)?
            ))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => Some(format!(
            "{}[{}]",
            static_start_expr_path(base)?,
            static_subscript_indices(subscripts)?
        )),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{field}", static_start_expr_path(base)?))
        }
        _ => None,
    }
}

fn static_subscript_indices(subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => indices.push(value.to_string()),
            rumoca_core::Subscript::Expr { .. } | rumoca_core::Subscript::Colon { .. } => {
                return None;
            }
        }
    }
    Some(indices.join(","))
}

fn bind_start_key(env: &mut VarEnv<f64>, name: &str, visited: &mut HashSet<String>) -> Option<f64> {
    if let Some(value) = env.vars.get(name).copied() {
        return Some(value);
    }
    if is_known_string_path(name) || !visited.insert(name.to_string()) {
        return None;
    }
    let start = env.start_exprs.get(name)?.clone();
    if start_expr_is_nonnumeric(&start, env) {
        return None;
    }
    for _ in 0..8 {
        match eval_expr::<f64>(&start, env) {
            Ok(value) => {
                env.set(name, value);
                return Some(value);
            }
            Err(err) => {
                let missing = err.missing_binding_name()?;
                if bind_start_key(env, missing, visited).is_some()
                    || bind_enclosing_start_alias(env, missing, visited).is_some()
                {
                    continue;
                }
                return None;
            }
        }
    }
    None
}

fn trace_pending_runtime_start_missing(env: &VarEnv<f64>, owner: Option<&str>, err: &EvalError) {
    if std::env::var_os("RUMOCA_TRACE_RUNTIME_START_ERROR").is_none() {
        return;
    }
    eprintln!("runtime start pending missing owner={owner:?}: err={err:?}");
    if let Some(missing) = err.missing_binding_name()
        && let Some(suffix) = missing.rsplit_once('.').map(|(_, suffix)| suffix)
    {
        let matches = env
            .vars
            .keys()
            .chain(env.start_exprs.keys())
            .filter(|key| key.ends_with(suffix))
            .take(24)
            .cloned()
            .collect::<Vec<_>>();
        eprintln!("runtime start pending suffix matches for {suffix}: {matches:?}");
        if let Some((owner, field)) = missing.rsplit_once('.') {
            let mut cursor = owner;
            for _ in 0..8 {
                for source in [
                    "PreDroWat",
                    "PreDroAir",
                    "dp1_nominal",
                    "dp2_nominal",
                    field,
                ] {
                    let key = format!("{cursor}.{source}");
                    let in_vars = env.vars.contains_key(key.as_str());
                    let in_starts = env.start_exprs.contains_key(key.as_str());
                    if in_vars || in_starts {
                        eprintln!(
                            "runtime start pending candidate key={key:?} vars={in_vars} starts={in_starts}"
                        );
                    }
                }
                let Some((parent, _)) = cursor.rsplit_once('.') else {
                    break;
                };
                cursor = parent;
            }
        }
    }
}

fn trace_runtime_start_error(name: &str, var: &rumoca_ir_dae::Variable, err: &EvalError) {
    if std::env::var_os("RUMOCA_TRACE_RUNTIME_START_ERROR").is_none() {
        return;
    }
    eprintln!(
        "runtime start error for {name}: err={err:?} dims={:?} start={:#?}",
        var.dims, var.start
    );
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

/// Collect unique static variable references used by start expressions.
pub fn collect_var_start_refs<'a>(
    starts: impl IntoIterator<Item = &'a rumoca_core::Expression>,
) -> Vec<String> {
    let mut refs = Vec::new();
    let mut seen = HashSet::new();
    for start in starts {
        let before = refs.len();
        collect_start_expr_var_refs(start, &mut refs);
        for value in refs.drain(before..).collect::<Vec<_>>() {
            if seen.insert(value.clone()) {
                refs.push(value);
            }
        }
    }
    refs
}

pub fn collect_start_alias_sources(refs: &[String]) -> IndexMap<String, Vec<String>> {
    let mut map = IndexMap::<String, Vec<String>>::new();
    for target in refs {
        for source in enclosing_start_alias_sources(target) {
            map.entry(source).or_default().push(target.clone());
        }
    }
    map
}

fn enclosing_start_alias_sources(target: &str) -> Vec<String> {
    let Some((owner, field)) = target.rsplit_once('.') else {
        return Vec::new();
    };
    let mut sources = Vec::new();
    let mut cursor = owner;
    while let Some((parent, _)) = cursor.rsplit_once('.') {
        sources.push(format!("{parent}.{field}"));
        cursor = parent;
    }
    sources
}

/// Collect fixed attributes from all variable categories in the DAE.
pub fn collect_var_fixed_attrs(dae: &Dae) -> IndexMap<String, Option<bool>> {
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
        map.insert(name.as_str().to_string(), var.fixed);
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
    result.start_expr_refs = env.start_expr_refs.clone();
    result.start_alias_sources = env.start_alias_sources.clone();
    result.fixed_attrs = env.fixed_attrs.clone();
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
