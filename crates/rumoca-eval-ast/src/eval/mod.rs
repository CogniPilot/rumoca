//! Compile-time constant evaluation for typecheck-time AST expressions.
//!
//! The typecheck phase needs early evaluation for:
//! - structural parameters and dimensions (MLS §10, §18)
//! - enum/integer/boolean conditions in guarded expressions
//! - shape inference before flattening produces Expression forms

use rumoca_core::{
    IntegerBinaryOperator, eval_integer_binary as eval_common_integer_binary,
    eval_integer_div_builtin, find_last_top_level_dot, has_top_level_dot, top_level_last_segment,
};
use rumoca_ir_ast::{
    ClassDef, ClassType, Expression, Statement, StatementBlock, Subscript, TerminalType,
};
use rumoca_ir_core::{Causality, OpBinary, OpUnary};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;

pub use dimension_inference::{
    infer_dimensions_from_binding, infer_dimensions_from_binding_with_scope,
};

/// Epsilon for compile-time real equality checks.
const REAL_COMPARISON_EPSILON: f64 = 1e-15;

/// Scope-aware lookup: resolve `scope.name` down to `name`.
///
/// For `scope=A.B.C`, this checks: `A.B.C.name`, `A.B.name`, `A.name`, `name`.
fn lookup_by_scope<'a, T>(name: &str, scope: &str, map: &'a FxHashMap<String, T>) -> Option<&'a T> {
    if scope.is_empty() {
        return map.get(name);
    }

    let mut scope_end = scope.len();
    let mut qualified = String::with_capacity(scope.len() + 1 + name.len());
    loop {
        let current_scope = &scope[..scope_end];
        qualified.clear();
        qualified.push_str(current_scope);
        qualified.push('.');
        qualified.push_str(name);

        if let Some(val) = map.get(qualified.as_str()) {
            return Some(val);
        }

        let Some(pos) = find_last_top_level_dot(current_scope) else {
            break;
        };
        scope_end = pos;
    }

    map.get(name)
}

/// General lookup for constant/scalar evaluation.
///
/// Unlike `lookup_structural_with_scope`, this permits guarded leaf fallback
/// so package constants referenced through aliases still resolve.
fn lookup_with_scope<'a, T: PartialEq>(
    name: &str,
    scope: &str,
    map: &'a FxHashMap<String, T>,
    suffix_index: Option<&SuffixIndex>,
) -> Option<&'a T> {
    if let Some(val) = lookup_by_scope(name, scope, map) {
        return Some(val);
    }

    // Suffix fallback for package constants (MLS §7.3).
    // For bare names like "nX", look for any entry ending in ".nX".
    // For dotted names like "Medium.nX", prefer full dotted suffix
    // ".Medium.nX". If no full-dotted match exists, allow a guarded leaf
    // fallback only when the leaf suffix resolves to exactly one key.
    if has_top_level_dot(name) {
        match lookup_by_suffix_state(name, map, suffix_index) {
            SuffixLookup::Found(val) => return Some(val),
            SuffixLookup::Ambiguous => return None,
            SuffixLookup::Missing => {}
        }
        return lookup_by_suffix_unique_key(top_level_last_segment(name), map, suffix_index);
    }

    lookup_by_suffix(name, map, suffix_index)
}

/// Structural lookup used by shape inference and strict dimension resolution.
///
/// Dotted names intentionally avoid leaf fallback to prevent cross-scope
/// accidental matches (for example `Medium.nX` resolving to unrelated `*.nX`).
fn lookup_structural_with_scope<'a, T: PartialEq>(
    name: &str,
    scope: &str,
    map: &'a FxHashMap<String, T>,
    suffix_index: Option<&SuffixIndex>,
) -> Option<&'a T> {
    if let Some(value) = lookup_by_scope(name, scope, map) {
        return Some(value);
    }

    if has_top_level_dot(name) {
        return match lookup_by_suffix_state(name, map, suffix_index) {
            SuffixLookup::Found(value) => Some(value),
            SuffixLookup::Missing | SuffixLookup::Ambiguous => None,
        };
    }

    lookup_by_suffix(name, map, suffix_index)
}

fn find_unique_value<'a, T: PartialEq>(
    candidates: &[usize],
    map: &'a FxHashMap<String, T>,
    suffix_index: &SuffixIndex,
) -> SuffixLookup<'a, T> {
    let mut found: Option<&T> = None;
    for candidate_idx in candidates {
        let key = suffix_index.key(*candidate_idx);
        if let Some(val) = map.get(key) {
            if found.is_some_and(|prev| prev != val) {
                return SuffixLookup::Ambiguous;
            }
            found = Some(val);
        }
    }
    found.map_or(SuffixLookup::Missing, SuffixLookup::Found)
}

enum SuffixLookup<'a, T> {
    Found(&'a T),
    Missing,
    Ambiguous,
}

fn lookup_by_suffix_state<'a, T: PartialEq>(
    name: &str,
    map: &'a FxHashMap<String, T>,
    suffix_index: Option<&SuffixIndex>,
) -> SuffixLookup<'a, T> {
    if has_top_level_dot(name) {
        if let Some(index) = suffix_index {
            let Some(candidates) = index.keys_by_dotted_suffix.get(name) else {
                return SuffixLookup::Missing;
            };
            return find_unique_value(candidates, map, index);
        }

        let mut found: Option<&T> = None;
        for (key, val) in map {
            if !has_top_level_suffix_match(key, name) {
                continue;
            }
            if found.is_some_and(|prev| prev != val) {
                return SuffixLookup::Ambiguous;
            }
            found = Some(val);
        }
        return found.map_or(SuffixLookup::Missing, SuffixLookup::Found);
    }

    if let Some(index) = suffix_index {
        // O(1) lookup using pre-built suffix index
        let Some(candidates) = index.keys_by_suffix.get(name) else {
            return SuffixLookup::Missing;
        };
        find_unique_value(candidates, map, index)
    } else {
        // Fallback: linear scan
        let mut found: Option<&T> = None;
        for (key, val) in map {
            if !has_top_level_suffix_match(key, name) {
                continue;
            }
            if found.is_some_and(|prev| prev != val) {
                return SuffixLookup::Ambiguous; // Ambiguous: different values
            }
            found = Some(val);
        }
        found.map_or(SuffixLookup::Missing, SuffixLookup::Found)
    }
}

fn lookup_by_suffix<'a, T: PartialEq>(
    name: &str,
    map: &'a FxHashMap<String, T>,
    suffix_index: Option<&SuffixIndex>,
) -> Option<&'a T> {
    match lookup_by_suffix_state(name, map, suffix_index) {
        SuffixLookup::Found(val) => Some(val),
        SuffixLookup::Missing | SuffixLookup::Ambiguous => None,
    }
}

/// Dotted lookup leaf fallback that requires a unique target-map key.
fn lookup_by_suffix_unique_key<'a, T>(
    name: &str,
    map: &'a FxHashMap<String, T>,
    suffix_index: Option<&SuffixIndex>,
) -> Option<&'a T> {
    if let Some(index) = suffix_index {
        let candidates = index.keys_by_suffix.get(name)?;
        let mut matched_idx: Option<usize> = None;
        for candidate_idx in candidates {
            let key = index.key(*candidate_idx);
            if !map.contains_key(key) {
                continue;
            }
            if matched_idx.replace(*candidate_idx).is_some() {
                return None;
            }
        }
        return matched_idx.and_then(|idx| map.get(index.key(idx)));
    }

    let mut matched_key: Option<&str> = None;
    for key in map.keys() {
        if !has_top_level_suffix_match(key, name) {
            continue;
        }
        if matched_key.replace(key.as_str()).is_some() {
            return None;
        }
    }
    matched_key.and_then(|key| map.get(key))
}

fn has_top_level_suffix_match(key: &str, suffix: &str) -> bool {
    if key.len() <= suffix.len() || !key.ends_with(suffix) {
        return false;
    }

    let boundary_idx = key.len() - suffix.len() - 1;
    matches!(key.as_bytes().get(boundary_idx), Some(b'.')) && is_top_level_dot_at(key, boundary_idx)
}

fn is_top_level_dot_at(path: &str, dot_index: usize) -> bool {
    let mut bracket_depth = 0usize;
    for (idx, byte) in path.bytes().enumerate().take(dot_index + 1) {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if idx == dot_index => return bracket_depth == 0,
            _ => {}
        }
    }
    false
}

pub struct TypeCheckEvalContext {
    pub integers: FxHashMap<String, i64>,
    pub reals: FxHashMap<String, f64>,
    pub booleans: FxHashMap<String, bool>,
    pub enums: FxHashMap<String, String>,
    pub dimensions: FxHashMap<String, Vec<usize>>,
    /// Function definitions for compile-time evaluation (MLS §12.4).
    pub functions: Arc<FxHashMap<String, ClassDef>>,
    pub func_eval_depth: usize,
    pub enum_sizes: FxHashMap<String, usize>,
    pub enum_ordinals: FxHashMap<String, i64>,
    suffix_index: Option<SuffixIndex>,
}

struct SuffixIndex {
    keys: Vec<String>,
    keys_by_suffix: FxHashMap<String, Vec<usize>>,
    keys_by_dotted_suffix: FxHashMap<String, Vec<usize>>,
}

impl SuffixIndex {
    fn key(&self, idx: usize) -> &str {
        self.keys[idx].as_str()
    }

    fn insert_key(&mut self, key: String) {
        if self.keys.iter().any(|existing| existing == &key) {
            return;
        }
        let key_idx = self.keys.len();
        index_key_suffixes(
            &key,
            key_idx,
            &mut self.keys_by_suffix,
            &mut self.keys_by_dotted_suffix,
        );
        self.keys.push(key);
    }
}

fn index_key_suffixes(
    key: &str,
    key_idx: usize,
    keys_by_suffix: &mut FxHashMap<String, Vec<usize>>,
    keys_by_dotted_suffix: &mut FxHashMap<String, Vec<usize>>,
) {
    let mut bracket_depth = 0usize;
    let mut top_level_dots: Vec<usize> = Vec::new();
    for (idx, byte) in key.bytes().enumerate() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => top_level_dots.push(idx),
            _ => {}
        }
    }

    let Some(&last_dot) = top_level_dots.last() else {
        return;
    };

    keys_by_suffix
        .entry(key[last_dot + 1..].to_string())
        .or_default()
        .push(key_idx);

    for dot_idx in top_level_dots
        .iter()
        .take(top_level_dots.len().saturating_sub(1))
    {
        keys_by_dotted_suffix
            .entry(key[*dot_idx + 1..].to_string())
            .or_default()
            .push(key_idx);
    }
}

impl Default for TypeCheckEvalContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeCheckEvalContext {
    pub fn new() -> Self {
        Self {
            integers: FxHashMap::default(),
            reals: FxHashMap::default(),
            booleans: FxHashMap::default(),
            enums: FxHashMap::default(),
            dimensions: FxHashMap::default(),
            functions: Arc::new(FxHashMap::default()),
            func_eval_depth: 0,
            enum_sizes: FxHashMap::default(),
            enum_ordinals: FxHashMap::default(),
            suffix_index: None,
        }
    }

    pub fn build_suffix_index(&mut self) {
        let mut keys_by_suffix: FxHashMap<String, Vec<usize>> = FxHashMap::default();
        let mut keys_by_dotted_suffix: FxHashMap<String, Vec<usize>> = FxHashMap::default();
        let mut seen: FxHashSet<String> = FxHashSet::default();
        let mut keys: Vec<String> = Vec::new();

        // Collect suffixes from all value maps and function names
        for key in self
            .integers
            .keys()
            .chain(self.reals.keys())
            .chain(self.booleans.keys())
            .chain(self.enums.keys())
            .chain(self.dimensions.keys())
            .chain(self.functions.keys())
            .chain(self.enum_ordinals.keys())
            .chain(self.enum_sizes.keys())
        {
            let owned = key.clone();
            if seen.insert(owned.clone()) {
                keys.push(owned);
            }
        }

        for (key_idx, key) in keys.iter().enumerate() {
            index_key_suffixes(
                key,
                key_idx,
                &mut keys_by_suffix,
                &mut keys_by_dotted_suffix,
            );
        }

        self.suffix_index = Some(SuffixIndex {
            keys,
            keys_by_suffix,
            keys_by_dotted_suffix,
        });
    }

    pub fn add_integer(&mut self, name: impl Into<String>, value: i64) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.integers.insert(name, value);
    }

    pub fn add_integer_if_absent(&mut self, name: impl Into<String>, value: i64) {
        let name = name.into();
        if !self.integers.contains_key(&name) {
            self.add_integer(name, value);
        }
    }

    pub fn add_real(&mut self, name: impl Into<String>, value: f64) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.reals.insert(name, value);
    }

    pub fn add_real_if_absent(&mut self, name: impl Into<String>, value: f64) {
        let name = name.into();
        if !self.reals.contains_key(&name) {
            self.add_real(name, value);
        }
    }

    pub fn add_boolean(&mut self, name: impl Into<String>, value: bool) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.booleans.insert(name, value);
    }

    pub fn add_boolean_if_absent(&mut self, name: impl Into<String>, value: bool) {
        let name = name.into();
        if !self.booleans.contains_key(&name) {
            self.add_boolean(name, value);
        }
    }

    pub fn add_enum(&mut self, name: impl Into<String>, value: impl Into<String>) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.enums.insert(name, value.into());
    }

    pub fn add_dimensions(&mut self, name: impl Into<String>, dims: Vec<usize>) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.dimensions.insert(name, dims);
    }

    pub fn add_enum_size(&mut self, name: impl Into<String>, size: usize) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.enum_sizes.insert(name, size);
    }

    pub fn add_enum_size_if_absent(&mut self, name: impl Into<String>, size: usize) {
        let name = name.into();
        if !self.enum_sizes.contains_key(&name) {
            self.add_enum_size(name, size);
        }
    }

    pub fn add_enum_ordinal(&mut self, name: impl Into<String>, ordinal: i64) {
        let name = name.into();
        self.add_suffix_index_key(name.as_str());
        self.enum_ordinals.insert(name, ordinal);
    }

    pub fn add_enum_ordinal_if_absent(&mut self, name: impl Into<String>, ordinal: i64) {
        let name = name.into();
        if !self.enum_ordinals.contains_key(&name) {
            self.add_enum_ordinal(name, ordinal);
        }
    }

    fn add_suffix_index_key(&mut self, name: &str) {
        if let Some(index) = &mut self.suffix_index {
            index.insert_key(name.to_string());
        }
    }

    pub fn get_integer(&self, name: &str) -> Option<i64> {
        self.integers.get(name).copied()
    }

    pub fn get_dimensions(&self, name: &str) -> Option<&Vec<usize>> {
        self.dimensions.get(name)
    }
}

/// Scope-aware evaluation for integer builtins and pure functions.
fn eval_integer_func_with_scope(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<i64> {
    match func_name {
        "integer" if args.len() == 1 => eval_real_with_scope(&args[0], ctx, scope)
            .map(|r| r.floor() as i64)
            .or_else(|| eval_integer_with_scope(&args[0], ctx, scope)),
        "size" if args.len() == 2 => {
            let dim_idx = eval_integer_with_scope(&args[1], ctx, scope)? as usize;
            if dim_idx < 1 {
                return None;
            }
            // Try named array lookup first
            if let Some(array_name) = extract_component_path(&args[0])
                && let Some(dims) = lookup_dims_with_scope(&array_name, ctx, scope)
                && dim_idx <= dims.len()
            {
                return Some(dims[dim_idx - 1] as i64);
            }
            // Fallback: infer dimensions from expression (handles fill(), zeros(), array literals)
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            (dim_idx <= dims.len()).then(|| dims[dim_idx - 1] as i64)
        }
        "abs" if args.len() == 1 => eval_integer_with_scope(&args[0], ctx, scope).map(|v| v.abs()),
        "max" if args.len() == 2 => {
            let a = eval_integer_with_scope(&args[0], ctx, scope)?;
            let b = eval_integer_with_scope(&args[1], ctx, scope)?;
            Some(a.max(b))
        }
        "max" if args.len() == 1 => {
            // max(array) reduction form - returns maximum element
            eval_integer_array_with_scope(&args[0], ctx, scope)
                .and_then(|vals| vals.into_iter().max())
        }
        "min" if args.len() == 2 => {
            let a = eval_integer_with_scope(&args[0], ctx, scope)?;
            let b = eval_integer_with_scope(&args[1], ctx, scope)?;
            Some(a.min(b))
        }
        "min" if args.len() == 1 => {
            // min(array) reduction form - returns minimum element
            eval_integer_array_with_scope(&args[0], ctx, scope)
                .and_then(|vals| vals.into_iter().min())
        }
        "div" if args.len() == 2 => {
            let a = eval_integer_with_scope(&args[0], ctx, scope)?;
            let b = eval_integer_with_scope(&args[1], ctx, scope)?;
            eval_integer_div_builtin(a, b)
        }
        "mod" if args.len() == 2 => {
            let a = eval_integer_with_scope(&args[0], ctx, scope)?;
            let b = eval_integer_with_scope(&args[1], ctx, scope)?;
            (b != 0).then(|| a % b)
        }
        "floor" if args.len() == 1 => {
            eval_real_with_scope(&args[0], ctx, scope).map(|r| r.floor() as i64)
        }
        "ceil" if args.len() == 1 => {
            eval_real_with_scope(&args[0], ctx, scope).map(|r| r.ceil() as i64)
        }
        "sum" if args.len() == 1 => {
            eval_integer_array_with_scope(&args[0], ctx, scope).map(|vals| vals.iter().sum())
        }
        "product" if args.len() == 1 => {
            eval_integer_array_with_scope(&args[0], ctx, scope).map(|vals| vals.iter().product())
        }
        "rem" if args.len() == 2 => {
            let a = eval_integer_with_scope(&args[0], ctx, scope)?;
            let b = eval_integer_with_scope(&args[1], ctx, scope)?;
            (b != 0).then(|| a % b)
        }
        "sign" if args.len() == 1 => {
            eval_integer_with_scope(&args[0], ctx, scope).map(|v| v.signum())
        }
        "ndims" if args.len() == 1 => {
            if let Some(array_name) = extract_component_path(&args[0])
                && let Some(dims) = lookup_dims_with_scope(&array_name, ctx, scope)
            {
                return Some(dims.len() as i64);
            }
            // Fallback: infer dimensions from expression
            infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)
                .map(|dims| dims.len() as i64)
        }
        // Fallback: try interpreting user-defined pure functions (MLS §12.4)
        _ => eval_user_func_integer(func_name, args, ctx, scope),
    }
}

const MAX_FUNC_EVAL_DEPTH: usize = 10;

fn lookup_function<'a>(func_name: &str, ctx: &'a TypeCheckEvalContext) -> Option<&'a ClassDef> {
    if let Some(def) = ctx.functions.get(func_name) {
        return Some(def);
    }
    // Use suffix index for function name lookup if available
    if let Some(index) = &ctx.suffix_index {
        if let Some(keys) = index.keys_by_suffix.get(func_name)
            && let Some(def) = keys
                .iter()
                .find_map(|key_idx| ctx.functions.get(index.key(*key_idx)))
        {
            return Some(def);
        }

        if has_top_level_dot(func_name)
            && let Some(keys) = index.keys_by_dotted_suffix.get(func_name)
            && let Some(def) = keys
                .iter()
                .find_map(|key_idx| ctx.functions.get(index.key(*key_idx)))
        {
            return Some(def);
        }

        // For multi-segment relative names like "A.B.Func", try the last segment
        let last_seg = top_level_last_segment(func_name);
        if let Some(keys) = index.keys_by_suffix.get(last_seg) {
            // If only one function with this name exists, use it
            // (handles import aliases like Cv.from_deg → Conversions.from_deg)
            if let Some(key_idx) = unique_function_key_idx(keys, index, &ctx.functions)
                && let Some(def) = ctx.functions.get(index.key(key_idx))
            {
                return Some(def);
            }
        }
    }
    // Fallback: linear scan
    ctx.functions
        .iter()
        .find(|(key, _)| has_top_level_suffix_match(key, func_name))
        .map(|(_, def)| def)
}

fn unique_function_key_idx(
    keys: &[usize],
    index: &SuffixIndex,
    functions: &FxHashMap<String, ClassDef>,
) -> Option<usize> {
    let mut candidates = keys
        .iter()
        .copied()
        .filter(|key_idx| functions.contains_key(index.key(*key_idx)));
    let first = candidates.next()?;
    candidates.next().is_none().then_some(first)
}

fn find_func_output_name(func_def: &ClassDef) -> Option<String> {
    func_def
        .components
        .iter()
        .find(|(_, comp)| matches!(comp.causality, Causality::Output(_)))
        .map(|(name, _)| name.clone())
}

fn integral_real_to_i64(value: f64) -> Option<i64> {
    let rounded = value.round();
    ((value - rounded).abs() < REAL_COMPARISON_EPSILON).then_some(rounded as i64)
}

fn local_has_scalar(local: &TypeCheckEvalContext, name: &str) -> bool {
    local.integers.contains_key(name)
        || local.reals.contains_key(name)
        || local.booleans.contains_key(name)
}

fn bind_local_scalar_value(
    local: &mut TypeCheckEvalContext,
    name: &str,
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) {
    if let Some(v) = eval_integer_with_scope(expr, ctx, scope) {
        local.integers.insert(name.to_string(), v);
        local.reals.insert(name.to_string(), v as f64);
        return;
    }
    if let Some(v) = eval_real_with_scope(expr, ctx, scope) {
        local.reals.insert(name.to_string(), v);
        if let Some(i) = integral_real_to_i64(v) {
            local.integers.insert(name.to_string(), i);
        }
        return;
    }
    if let Some(v) = eval_boolean_with_scope(expr, ctx, scope) {
        local.booleans.insert(name.to_string(), v);
    }
}

/// Build a local evaluation context for interpreting a function call (MLS §12.4).
///
/// Maps formal input parameters to actual argument values. Falls back to
/// default values when arguments are not provided.
fn build_func_eval_context(
    func_def: &ClassDef,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<TypeCheckEvalContext> {
    let mut local = TypeCheckEvalContext::new();
    local.functions = Arc::clone(&ctx.functions);
    local.func_eval_depth = ctx.func_eval_depth + 1;
    if local.func_eval_depth > MAX_FUNC_EVAL_DEPTH {
        return None;
    }
    let inputs: Vec<_> = func_def
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.causality, Causality::Input(_)))
        .collect();
    // Pass 1: match positional (non-named) arguments
    let mut positional_idx = 0;
    for arg in args {
        if matches!(arg, Expression::NamedArgument { .. }) {
            continue; // Named args handled in pass 2
        }
        if positional_idx < inputs.len() {
            let (param_name, _) = &inputs[positional_idx];
            bind_local_scalar_value(&mut local, param_name, arg, ctx, scope);
        }
        positional_idx += 1;
    }
    // Pass 2: match named arguments by name
    for arg in args {
        if let Expression::NamedArgument { name, value } = arg
            && let Some((param_name, _)) = inputs
                .iter()
                .find(|(n, _)| n.as_str() == name.text.as_ref())
        {
            bind_local_scalar_value(&mut local, param_name, value, ctx, scope);
        }
    }
    // Pass 3: fill remaining inputs with defaults
    for (param_name, param_comp) in &inputs {
        if local_has_scalar(&local, param_name) {
            continue;
        }
        if let Some(binding) = &param_comp.binding {
            bind_local_scalar_value(&mut local, param_name, binding, ctx, scope);
        }
        if !local_has_scalar(&local, param_name) && !matches!(param_comp.start, Expression::Empty) {
            bind_local_scalar_value(&mut local, param_name, &param_comp.start, ctx, scope);
        }
    }
    Some(local)
}

/// Try to evaluate a user-defined pure function returning a scalar integer (MLS §12.4).
///
/// Looks up the function definition, builds a local context with input values,
/// interprets the algorithm section, and returns the output variable's value.
fn eval_user_func_integer(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<i64> {
    if ctx.func_eval_depth >= MAX_FUNC_EVAL_DEPTH {
        return None;
    }
    let func_def = lookup_function(func_name, ctx)?;
    if func_def.class_type != ClassType::Function {
        return None;
    }
    let mut local_ctx = build_func_eval_context(func_def, args, ctx, scope)?;
    let output_name = find_func_output_name(func_def)?;
    for algo in &func_def.algorithms {
        if matches!(
            interpret_stmts(algo, &mut local_ctx)?,
            FunctionStmtFlow::Return
        ) {
            break;
        }
    }
    local_ctx.integers.get(&output_name).copied().or_else(|| {
        local_ctx
            .reals
            .get(&output_name)
            .and_then(|v| integral_real_to_i64(*v))
    })
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum FunctionStmtFlow {
    Continue,
    Break,
    Return,
}

/// Interpret a sequence of algorithm statements (MLS §11.1).
fn interpret_stmts(
    stmts: &[Statement],
    ctx: &mut TypeCheckEvalContext,
) -> Option<FunctionStmtFlow> {
    for stmt in stmts {
        let flow = interpret_stmt(stmt, ctx)?;
        if flow != FunctionStmtFlow::Continue {
            return Some(flow);
        }
    }
    Some(FunctionStmtFlow::Continue)
}

/// Interpret a single algorithm statement for compile-time function evaluation.
///
/// Handles assignment and if-elseif-else branching. Returns None if the
/// statement cannot be interpreted (unsupported construct or evaluation failure).
fn interpret_stmt(stmt: &Statement, ctx: &mut TypeCheckEvalContext) -> Option<FunctionStmtFlow> {
    match stmt {
        Statement::Assignment { comp, value } => {
            let var_name = comp.to_string();
            if let Some(val) = eval_integer_with_scope(value, ctx, "") {
                ctx.integers.insert(var_name.clone(), val);
                ctx.reals.insert(var_name, val as f64);
                return Some(FunctionStmtFlow::Continue);
            }
            if let Some(val) = eval_real_with_scope(value, ctx, "") {
                ctx.reals.insert(var_name.clone(), val);
                if let Some(i) = integral_real_to_i64(val) {
                    ctx.integers.insert(var_name, i);
                }
                return Some(FunctionStmtFlow::Continue);
            }
            if let Some(val) = eval_boolean_with_scope(value, ctx, "") {
                ctx.booleans.insert(var_name, val);
            }
            Some(FunctionStmtFlow::Continue)
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => interpret_if_stmt(cond_blocks, else_block.as_deref(), ctx),
        Statement::For { indices, equations } => interpret_for_stmt(indices, equations, ctx),
        Statement::While(block) => interpret_while_stmt(block, ctx),
        Statement::Break { .. } => Some(FunctionStmtFlow::Break),
        Statement::Return { .. } => Some(FunctionStmtFlow::Return),
        Statement::Empty => Some(FunctionStmtFlow::Continue),
        _ => Some(FunctionStmtFlow::Continue), // Skip unsupported statements
    }
}

/// Interpret an if-elseif-else statement (MLS §11.2.6).
fn interpret_if_stmt(
    cond_blocks: &[StatementBlock],
    else_block: Option<&[Statement]>,
    ctx: &mut TypeCheckEvalContext,
) -> Option<FunctionStmtFlow> {
    for block in cond_blocks {
        match eval_boolean_with_scope(&block.cond, ctx, "") {
            Some(true) => return interpret_stmts(&block.stmts, ctx),
            Some(false) => continue,
            None => return None,
        }
    }
    if let Some(else_stmts) = else_block {
        interpret_stmts(else_stmts, ctx)
    } else {
        Some(FunctionStmtFlow::Continue)
    }
}

/// Interpret a for-loop statement (MLS §11.2.4).
fn interpret_for_stmt(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[Statement],
    ctx: &mut TypeCheckEvalContext,
) -> Option<FunctionStmtFlow> {
    if indices.len() != 1 {
        return Some(FunctionStmtFlow::Continue); // Only single-index for loops
    }
    let idx = &indices[0];
    let var_name = idx.ident.text.to_string();
    let (start, end) = eval_for_range(&idx.range, ctx)?;
    for i in start..=end {
        ctx.integers.insert(var_name.clone(), i);
        match interpret_stmts(equations, ctx)? {
            FunctionStmtFlow::Continue => {}
            FunctionStmtFlow::Break => {
                ctx.integers.remove(&var_name);
                return Some(FunctionStmtFlow::Continue);
            }
            FunctionStmtFlow::Return => {
                ctx.integers.remove(&var_name);
                return Some(FunctionStmtFlow::Return);
            }
        }
    }
    ctx.integers.remove(&var_name);
    Some(FunctionStmtFlow::Continue)
}

/// Interpret a while-loop statement (MLS §11.2.5).
fn interpret_while_stmt(
    block: &StatementBlock,
    ctx: &mut TypeCheckEvalContext,
) -> Option<FunctionStmtFlow> {
    const MAX_WHILE_ITERATIONS: usize = 100_000;
    for _ in 0..MAX_WHILE_ITERATIONS {
        match eval_boolean_with_scope(&block.cond, ctx, "") {
            Some(true) => match interpret_stmts(&block.stmts, ctx)? {
                FunctionStmtFlow::Continue => {}
                FunctionStmtFlow::Break => return Some(FunctionStmtFlow::Continue),
                FunctionStmtFlow::Return => return Some(FunctionStmtFlow::Return),
            },
            Some(false) => return Some(FunctionStmtFlow::Continue),
            None => return None,
        }
    }
    None
}

/// Evaluate a for-loop range expression to (start, end) bounds.
fn eval_for_range(range: &Expression, ctx: &TypeCheckEvalContext) -> Option<(i64, i64)> {
    if let Expression::Range { start, end, .. } = range {
        let s = eval_integer_with_scope(start, ctx, "")?;
        let e = eval_integer_with_scope(end, ctx, "")?;
        Some((s, e))
    } else {
        None
    }
}

/// Look up array dimensions with scope-aware progressive lookup.
///
/// For `array_name` = "a" and `scope` = "tf.inner", tries:
/// 1. "tf.inner.a"
/// 2. "tf.a"
/// 3. "a"
fn lookup_dims_with_scope<'a>(
    array_name: &str,
    ctx: &'a TypeCheckEvalContext,
    scope: &str,
) -> Option<&'a Vec<usize>> {
    lookup_with_scope(
        array_name,
        scope,
        &ctx.dimensions,
        ctx.suffix_index.as_ref(),
    )
}

/// Flatten a matrix row element into integer values.
fn flatten_matrix_row(
    elem: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
    result: &mut Vec<i64>,
) -> Option<()> {
    if let Expression::Array { elements: row, .. } = elem {
        for sub in row {
            result.push(eval_integer_with_scope(sub, ctx, scope)?);
        }
    } else {
        result.push(eval_integer_with_scope(elem, ctx, scope)?);
    }
    Some(())
}

/// Try to evaluate an array expression to a Vec of integers (for sum/product/max/min).
///
/// Handles both flat arrays `{1, 2, 3}` and matrix syntax `[a; b; c]` where
/// each element may be a single-element row array.
fn eval_integer_array_with_scope(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<i64>> {
    match expr {
        Expression::Array {
            elements,
            is_matrix,
        } if *is_matrix => {
            // Matrix syntax [a; b; c] - flatten row arrays to scalar elements
            let mut result = Vec::new();
            for elem in elements {
                flatten_matrix_row(elem, ctx, scope, &mut result)?;
            }
            Some(result)
        }
        Expression::Array { elements, .. } => {
            // Flat array {a, b, c}
            elements
                .iter()
                .map(|e| eval_integer_with_scope(e, ctx, scope))
                .collect()
        }
        Expression::Parenthesized { inner } => eval_integer_array_with_scope(inner, ctx, scope),
        _ => None,
    }
}

/// Evaluate integer binary operations.
fn eval_integer_binary(op: &OpBinary, l: i64, r: i64) -> Option<i64> {
    let operator = match op {
        OpBinary::Add(_) | OpBinary::AddElem(_) => IntegerBinaryOperator::Add,
        OpBinary::Sub(_) | OpBinary::SubElem(_) => IntegerBinaryOperator::Sub,
        OpBinary::Mul(_) | OpBinary::MulElem(_) => IntegerBinaryOperator::Mul,
        OpBinary::Div(_) | OpBinary::DivElem(_) => IntegerBinaryOperator::Div,
        OpBinary::Exp(_) | OpBinary::ExpElem(_) => IntegerBinaryOperator::Exp,
        _ => return None,
    };
    eval_common_integer_binary(operator, l, r)
}

/// Try to evaluate an AST expression to an integer.
pub fn eval_integer(expr: &Expression, ctx: &TypeCheckEvalContext) -> Option<i64> {
    eval_integer_with_scope(expr, ctx, "")
}

/// Try to evaluate an AST expression to a real.
pub fn eval_real(expr: &Expression, ctx: &TypeCheckEvalContext) -> Option<f64> {
    eval_real_with_scope(expr, ctx, "")
}

/// Try to evaluate an AST expression to a real with scope-aware lookup.
pub fn eval_real_with_scope(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<f64> {
    match expr {
        Expression::Terminal {
            terminal_type,
            token,
        } => match terminal_type {
            TerminalType::UnsignedReal => token.text.parse().ok(),
            TerminalType::UnsignedInteger => token.text.parse::<i64>().ok().map(|i| i as f64),
            _ => None,
        },

        Expression::ComponentReference(cr) => {
            if cr.parts.is_empty() {
                return None;
            }
            let ref_path = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");

            lookup_with_scope(&ref_path, scope, &ctx.reals, ctx.suffix_index.as_ref())
                .copied()
                .or_else(|| {
                    lookup_with_scope(&ref_path, scope, &ctx.integers, ctx.suffix_index.as_ref())
                        .map(|&i| i as f64)
                })
        }

        Expression::Unary { op, rhs } => {
            let val = eval_real_with_scope(rhs, ctx, scope)?;
            match op {
                OpUnary::Plus(_) | OpUnary::DotPlus(_) | OpUnary::Empty => Some(val),
                OpUnary::Minus(_) | OpUnary::DotMinus(_) => Some(-val),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            let l = eval_real_with_scope(lhs, ctx, scope)?;
            let r = eval_real_with_scope(rhs, ctx, scope)?;
            match op {
                OpBinary::Add(_) | OpBinary::AddElem(_) => Some(l + r),
                OpBinary::Sub(_) | OpBinary::SubElem(_) => Some(l - r),
                OpBinary::Mul(_) | OpBinary::MulElem(_) => Some(l * r),
                OpBinary::Div(_) | OpBinary::DivElem(_) => {
                    if r != 0.0 {
                        Some(l / r)
                    } else {
                        None
                    }
                }
                OpBinary::Exp(_) | OpBinary::ExpElem(_) => Some(l.powf(r)),
                _ => None,
            }
        }

        Expression::Parenthesized { inner } => eval_real_with_scope(inner, ctx, scope),

        Expression::FunctionCall { comp, args } => {
            let func_name = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            eval_real_func_with_scope(&func_name, args, ctx, scope)
        }

        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                match eval_boolean_with_scope(cond, ctx, scope) {
                    Some(true) => return eval_real_with_scope(then_expr, ctx, scope),
                    Some(false) => continue,
                    None => return None,
                }
            }
            eval_real_with_scope(else_branch, ctx, scope)
        }

        _ => None,
    }
}

/// Scope-aware evaluation of real-valued function calls.
fn eval_real_func_with_scope(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<f64> {
    match func_name {
        "abs" if args.len() == 1 => eval_real_with_scope(&args[0], ctx, scope).map(|v| v.abs()),
        "sqrt" if args.len() == 1 => eval_real_with_scope(&args[0], ctx, scope).map(|v| v.sqrt()),
        "floor" if args.len() == 1 => eval_real_with_scope(&args[0], ctx, scope).map(|v| v.floor()),
        "ceil" if args.len() == 1 => eval_real_with_scope(&args[0], ctx, scope).map(|v| v.ceil()),
        "max" if args.len() == 2 => {
            let a = eval_real_with_scope(&args[0], ctx, scope)?;
            let b = eval_real_with_scope(&args[1], ctx, scope)?;
            Some(a.max(b))
        }
        "min" if args.len() == 2 => {
            let a = eval_real_with_scope(&args[0], ctx, scope)?;
            let b = eval_real_with_scope(&args[1], ctx, scope)?;
            Some(a.min(b))
        }
        _ => None,
    }
}

/// Look up a boolean value with scope-aware progressive lookup.
fn lookup_boolean_with_scope(
    ref_path: &str,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<bool> {
    lookup_with_scope(ref_path, scope, &ctx.booleans, ctx.suffix_index.as_ref()).copied()
}

/// Evaluate a numeric comparison (integer then real) with scope-aware lookup.
fn eval_numeric_comparison_with_scope(
    lhs: &Expression,
    rhs: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
    int_cmp: fn(i64, i64) -> bool,
    real_cmp: fn(f64, f64) -> bool,
) -> Option<bool> {
    if let (Some(l), Some(r)) = (
        eval_integer_with_scope(lhs, ctx, scope),
        eval_integer_with_scope(rhs, ctx, scope),
    ) {
        Some(int_cmp(l, r))
    } else if let (Some(l), Some(r)) = (
        eval_real_with_scope(lhs, ctx, scope),
        eval_real_with_scope(rhs, ctx, scope),
    ) {
        Some(real_cmp(l, r))
    } else {
        None
    }
}

/// Try to evaluate a boolean expression with scope-aware lookup.
pub fn eval_boolean_with_scope(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<bool> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token,
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        Expression::Terminal { .. } => None,

        Expression::ComponentReference(cr) if !cr.parts.is_empty() => {
            let ref_path = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            lookup_boolean_with_scope(&ref_path, ctx, scope)
        }
        Expression::ComponentReference(_) => None,

        Expression::Unary {
            op: OpUnary::Not(_),
            rhs,
        } => eval_boolean_with_scope(rhs, ctx, scope).map(|b| !b),
        Expression::Unary { .. } => None,

        Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::And(_) => {
                let l = eval_boolean_with_scope(lhs, ctx, scope)?;
                let r = eval_boolean_with_scope(rhs, ctx, scope)?;
                Some(l && r)
            }
            OpBinary::Or(_) => {
                let l = eval_boolean_with_scope(lhs, ctx, scope)?;
                let r = eval_boolean_with_scope(rhs, ctx, scope)?;
                Some(l || r)
            }
            OpBinary::Eq(_) => eval_numeric_comparison_with_scope(
                lhs,
                rhs,
                ctx,
                scope,
                |l, r| l == r,
                |l, r| (l - r).abs() < REAL_COMPARISON_EPSILON,
            )
            .or_else(|| eval_enum_comparison(lhs, rhs, ctx, scope)),
            OpBinary::Neq(_) => eval_numeric_comparison_with_scope(
                lhs,
                rhs,
                ctx,
                scope,
                |l, r| l != r,
                |l, r| (l - r).abs() >= REAL_COMPARISON_EPSILON,
            )
            .or_else(|| eval_enum_comparison(lhs, rhs, ctx, scope).map(|v| !v)),
            OpBinary::Lt(_) => {
                eval_numeric_comparison_with_scope(lhs, rhs, ctx, scope, |l, r| l < r, |l, r| l < r)
            }
            OpBinary::Le(_) => eval_numeric_comparison_with_scope(
                lhs,
                rhs,
                ctx,
                scope,
                |l, r| l <= r,
                |l, r| l <= r,
            ),
            OpBinary::Gt(_) => {
                eval_numeric_comparison_with_scope(lhs, rhs, ctx, scope, |l, r| l > r, |l, r| l > r)
            }
            OpBinary::Ge(_) => eval_numeric_comparison_with_scope(
                lhs,
                rhs,
                ctx,
                scope,
                |l, r| l >= r,
                |l, r| l >= r,
            ),
            _ => None,
        },

        Expression::Parenthesized { inner } => eval_boolean_with_scope(inner, ctx, scope),

        _ => None,
    }
}

/// Try to evaluate an AST expression to a boolean.
pub fn eval_boolean(expr: &Expression, ctx: &TypeCheckEvalContext) -> Option<bool> {
    eval_boolean_with_scope(expr, ctx, "")
}

/// Extract a component path from an expression (for size() calls).
fn extract_component_path(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ComponentReference(cr) => {
            if cr.parts.is_empty() {
                return None;
            }
            Some(cr.to_string())
        }
        Expression::Parenthesized { inner } => extract_component_path(inner),
        _ => None,
    }
}

/// Extract an enumeration value from an expression.
///
/// Enumeration literals are represented as ComponentReferences with qualified paths,
/// e.g., `Modelica.Blocks.Types.AnalogFilter.CriticalDamping`. This extracts the
/// full path as a string for comparison.
pub fn extract_enum_value(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ComponentReference(cr) if cr.parts.len() >= 2 => {
            // Enum literals have at least 2 parts (TypeName.LiteralName)
            // and no subscripts on any part
            let all_simple = cr.parts.iter().all(|p| p.subs.is_none());
            if all_simple {
                Some(
                    cr.parts
                        .iter()
                        .map(|p| p.ident.text.as_ref())
                        .collect::<Vec<_>>()
                        .join("."),
                )
            } else {
                None
            }
        }
        Expression::Parenthesized { inner } => extract_enum_value(inner),
        _ => None,
    }
}

/// Look up an enumeration value with scope-aware progressive lookup.
///
/// For `ref_path` = "filterType" and `scope` = "CDF", tries:
/// 1. "CDF.filterType"
/// 2. "filterType"
fn lookup_enum_with_scope<'a>(
    ref_path: &str,
    ctx: &'a TypeCheckEvalContext,
    scope: &str,
) -> Option<&'a str> {
    lookup_with_scope(ref_path, scope, &ctx.enums, ctx.suffix_index.as_ref()).map(|s| s.as_str())
}

/// Compare two enumeration values using suffix matching.
///
/// Enum values may be stored with different qualification levels:
/// - "CriticalDamping" vs "AnalogFilter.CriticalDamping" vs "Modelica.Blocks.Types.AnalogFilter.CriticalDamping"
///
/// Suffix matching ensures these all compare as equal.
fn enum_values_equal(a: &str, b: &str) -> bool {
    rumoca_core::enum_values_equal(a, b)
}

/// Try to evaluate an enumeration comparison expression.
///
/// Handles patterns like:
/// - `filterType == Modelica.Blocks.Types.FilterType.LowPass`
/// - `analogFilter == AnalogFilter.CriticalDamping`
fn eval_enum_comparison(
    lhs: &Expression,
    rhs: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<bool> {
    // Get enum value for LHS: either direct literal or looked up from variable
    let lhs_val = get_enum_expr_value(lhs, ctx, scope)?;
    let rhs_val = get_enum_expr_value(rhs, ctx, scope)?;
    Some(enum_values_equal(&lhs_val, &rhs_val))
}

/// Get the enumeration value for an expression (either a literal or a variable reference).
fn get_enum_expr_value(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<String> {
    match expr {
        Expression::ComponentReference(cr) if !cr.parts.is_empty() => {
            let path = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");

            // If it looks like a qualified enum literal (has dots and no subscripts)
            if cr.parts.len() >= 2 && cr.parts.iter().all(|p| p.subs.is_none()) {
                // First try as a variable reference that holds an enum value
                if let Some(val) = lookup_enum_with_scope(&path, ctx, scope) {
                    return Some(val.to_string());
                }
                // Otherwise treat as a direct enum literal
                return Some(path);
            }

            // Single-part reference: look up as a variable
            lookup_enum_with_scope(&path, ctx, scope).map(|s| s.to_string())
        }
        Expression::Parenthesized { inner } => get_enum_expr_value(inner, ctx, scope),
        _ => None,
    }
}

/// Try to evaluate a subscript to a dimension value.
pub fn eval_dimension(sub: &Subscript, ctx: &TypeCheckEvalContext) -> Option<usize> {
    match sub {
        Subscript::Expression(expr) => eval_integer(expr, ctx)
            .and_then(|i| if i >= 0 { Some(i as usize) } else { None })
            .or_else(|| eval_enum_dimension(expr, ctx)),
        Subscript::Range { .. } => None, // Colon dimensions need inference
        Subscript::Empty => None,
    }
}

/// Try to evaluate a subscript to a dimension value with scope-aware lookup.
///
/// This handles the case where dimension expressions reference parameters
/// that need to be looked up in the component's scope. For example, if
/// evaluating `n` for component `a.b.c`, this will try:
/// 1. `a.b.n` (scope prefix + ref)
/// 2. `a.n` (parent scope)
/// 3. `n` (root scope)
///
/// Also handles enumeration types used as dimensions (MLS §10.5):
/// if the expression is a type reference to an enumeration, the dimension
/// size is the number of enumeration literals.
pub fn eval_dimension_with_scope(
    sub: &Subscript,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<usize> {
    match sub {
        Subscript::Expression(expr) => eval_integer_with_scope(expr, ctx, scope)
            .and_then(|i| if i >= 0 { Some(i as usize) } else { None })
            .or_else(|| eval_enum_dimension_with_scope(expr, ctx, scope)),
        Subscript::Range { .. } => None, // Colon dimensions need inference
        Subscript::Empty => None,
    }
}

/// Try to resolve a dimension expression as an enumeration type (MLS §10.5).
///
/// When an enumeration type is used as a dimension (e.g., `Real x[Logic]`),
/// the size of that dimension is the number of enumeration literals.
fn eval_enum_dimension(expr: &Expression, ctx: &TypeCheckEvalContext) -> Option<usize> {
    if let Expression::ComponentReference(cr) = expr {
        let ref_path = cr
            .parts
            .iter()
            .map(|p| p.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".");
        ctx.enum_sizes.get(&ref_path).copied()
    } else {
        None
    }
}

/// Try to resolve a dimension expression as an enumeration type with scope-aware lookup.
fn eval_enum_dimension_with_scope(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<usize> {
    if let Expression::ComponentReference(cr) = expr {
        let ref_path = cr
            .parts
            .iter()
            .map(|p| p.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".");
        lookup_with_scope(&ref_path, scope, &ctx.enum_sizes, ctx.suffix_index.as_ref()).copied()
    } else {
        None
    }
}

/// Try to evaluate an expression to an integer with scope-aware lookup.
///
/// For component references, tries progressively shorter scope prefixes.
pub fn eval_integer_with_scope(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<i64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => token.text.parse().ok(),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
        } => {
            let f: f64 = token.text.parse().ok()?;
            (f.fract() == 0.0).then_some(f as i64)
        }
        Expression::Terminal { .. } => None,

        Expression::ComponentReference(cr) if !cr.parts.is_empty() => {
            let ref_path = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");

            // Try integer lookup first, then enum ordinal lookup
            lookup_with_scope(&ref_path, scope, &ctx.integers, ctx.suffix_index.as_ref())
                .copied()
                .or_else(|| {
                    lookup_with_scope(
                        &ref_path,
                        scope,
                        &ctx.enum_ordinals,
                        ctx.suffix_index.as_ref(),
                    )
                    .copied()
                })
        }
        Expression::ComponentReference(_) => None,

        Expression::Unary { op, rhs } => {
            let val = eval_integer_with_scope(rhs, ctx, scope)?;
            match op {
                OpUnary::Not(_) => None,
                OpUnary::Plus(_) | OpUnary::DotPlus(_) | OpUnary::Empty => Some(val),
                OpUnary::Minus(_) | OpUnary::DotMinus(_) => Some(-val),
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            let l = eval_integer_with_scope(lhs, ctx, scope)?;
            let r = eval_integer_with_scope(rhs, ctx, scope)?;
            eval_integer_binary(op, l, r)
        }

        Expression::Parenthesized { inner } => eval_integer_with_scope(inner, ctx, scope),

        Expression::FunctionCall { comp, args } => {
            let func_name = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            eval_integer_func_with_scope(&func_name, args, ctx, scope)
        }

        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                match eval_boolean_with_scope(cond, ctx, scope) {
                    Some(true) => return eval_integer_with_scope(then_expr, ctx, scope),
                    Some(false) => continue,
                    None => return None,
                }
            }
            eval_integer_with_scope(else_branch, ctx, scope)
        }

        _ => None,
    }
}

/// Infer dimensions from an array literal expression.
fn infer_array_dims(
    elements: &[Expression],
    is_matrix: bool,
    ctx: &TypeCheckEvalContext,
) -> Option<Vec<usize>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }
    if is_matrix {
        let num_rows = elements.len();
        // Parser encodes single-row matrix literals like `[1, 2, 3]` as
        // `Array { is_matrix: true, elements: [1,2,3] }` (not nested rows).
        // Treat those as 2D `[1, n]`, not 1D `[n]`.
        return match elements.first() {
            Some(Expression::Array { elements: row, .. }) => Some(vec![num_rows, row.len()]),
            _ => Some(vec![1, num_rows]),
        };
    }
    // Check for nested arrays
    if let Some(inner) = elements
        .first()
        .and_then(|f| infer_dimensions_from_binding(f, ctx))
    {
        let mut dims = vec![elements.len()];
        dims.extend(inner);
        return Some(dims);
    }
    Some(vec![elements.len()])
}

/// Infer dimensions for `cat(dim, A, B, ...)` concatenation.
fn infer_cat_dims_with_scope(
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    let cat_dim = eval_integer_with_scope(&args[0], ctx, scope)? as usize;
    if cat_dim < 1 {
        return None;
    }
    let cat_idx = cat_dim - 1;
    let mut result_dims: Option<Vec<usize>> = None;
    for arg in &args[1..] {
        let arg_dims = infer_dimensions_from_binding_with_scope(arg, ctx, scope)?;
        match &mut result_dims {
            None => result_dims = Some(arg_dims),
            Some(dims) => {
                if arg_dims.len() != dims.len() || cat_idx >= dims.len() {
                    return None;
                }
                dims[cat_idx] += arg_dims[cat_idx];
            }
        }
    }
    result_dims
}

/// Scope-aware dimension inference from array-constructing function calls.
fn infer_dims_from_func_with_scope(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    match func_name {
        "zeros" | "ones" => args
            .iter()
            .map(|a| eval_integer_with_scope(a, ctx, scope).map(|i| i as usize))
            .collect(),
        "fill" if args.len() >= 2 => args[1..]
            .iter()
            .map(|a| eval_integer_with_scope(a, ctx, scope).map(|i| i as usize))
            .collect(),
        "identity" if args.len() == 1 => {
            eval_integer_with_scope(&args[0], ctx, scope).map(|n| vec![n as usize, n as usize])
        }
        "cat" if args.len() >= 2 => infer_cat_dims_with_scope(args, ctx, scope),
        // transpose(A) → swap dimensions
        "transpose" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            if dims.len() == 2 {
                Some(vec![dims[1], dims[0]])
            } else {
                None
            }
        }
        // diagonal(v) → [n,n] from [n]
        "diagonal" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            if dims.len() == 1 {
                Some(vec![dims[0], dims[0]])
            } else {
                None
            }
        }
        // symmetric(A) → same dims as A
        "symmetric" if args.len() == 1 => {
            infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)
        }
        // linspace(a, b, n) → [n]
        "linspace" if args.len() == 3 => {
            eval_integer_with_scope(&args[2], ctx, scope).map(|n| vec![n as usize])
        }
        // scalar(A) → [] (scalar)
        "scalar" if args.len() == 1 => Some(vec![]),
        // vector(A) → [product(dims)]
        "vector" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            let total: usize = dims.iter().product();
            Some(vec![total])
        }
        // matrix(A) → [n,m] reshape to 2D
        "matrix" if args.len() == 1 => {
            let dims = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope)?;
            match dims.len() {
                0 => Some(vec![1, 1]),
                1 => Some(vec![dims[0], 1]),
                2 => Some(dims),
                _ => None,
            }
        }
        // cross(a, b) → [3] (cross product is always 3D)
        "cross" if args.len() == 2 => Some(vec![3]),
        // skew(v) → [3,3] from [3]
        "skew" if args.len() == 1 => Some(vec![3, 3]),
        // array(args...) → [len(args)] if all scalars, or [len(args), inner...] if arrays
        "array" if !args.is_empty() => {
            if let Some(inner) = infer_dimensions_from_binding_with_scope(&args[0], ctx, scope) {
                let mut dims = vec![args.len()];
                dims.extend(inner);
                Some(dims)
            } else {
                Some(vec![args.len()])
            }
        }
        // Fallback: infer dimensions from user-defined function output type (MLS §12.4)
        _ => infer_dims_from_user_func(func_name, args, ctx, scope),
    }
}

/// Infer output array dimensions from a user-defined function call (MLS §12.4).
///
/// Looks up the function definition, finds the output variable's dimension
/// expressions, substitutes actual argument values, and evaluates them.
fn infer_dims_from_user_func(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    if ctx.func_eval_depth >= MAX_FUNC_EVAL_DEPTH {
        return None;
    }
    let func_def = lookup_function(func_name, ctx)?;
    if func_def.class_type != ClassType::Function {
        return None;
    }
    let local_ctx = build_func_eval_context(func_def, args, ctx, scope)?;
    let (_, output) = func_def
        .components
        .iter()
        .find(|(_, comp)| matches!(comp.causality, Causality::Output(_)))?;
    // Scalar output (no dimension expressions)
    if output.shape_expr.is_empty() {
        // MLS §12.4.6: scalar functions applied element-wise to arrays.
        // If any actual argument has array dims, the result inherits those dims.
        return Some(find_broadcast_dims(args, ctx, scope));
    }
    // Evaluate each dimension expression in the local context
    output
        .shape_expr
        .iter()
        .map(|sub| match sub {
            Subscript::Expression(expr) => {
                eval_integer_with_scope(expr, &local_ctx, "").map(|v| v as usize)
            }
            _ => None,
        })
        .collect()
}

/// Find the largest array dimensions among actual arguments (MLS §12.4.6).
///
/// When a scalar function is called with array arguments, the result has
/// the shape of the largest argument (element-wise broadcast).
fn find_broadcast_dims(args: &[Expression], ctx: &TypeCheckEvalContext, scope: &str) -> Vec<usize> {
    let mut best: Vec<usize> = vec![];
    for arg in args {
        // Skip named arguments, use the value inside
        let expr = if let Expression::NamedArgument { value, .. } = arg {
            value.as_ref()
        } else {
            arg
        };
        if let Some(dims) = infer_dimensions_from_binding_with_scope(expr, ctx, scope)
            && dims.len() > best.len()
        {
            best = dims;
        }
    }
    best
}

/// Compute range length from start, step, end.
fn compute_range_len(start: i64, step: i64, end: i64) -> usize {
    if step == 0 {
        return 0;
    }
    if step > 0 {
        if end >= start {
            ((end - start) / step + 1) as usize
        } else {
            0
        }
    } else if start >= end {
        ((start - end) / (-step) + 1) as usize
    } else {
        0
    }
}

/// Compute range length for real-valued ranges.
///
/// MLS range expressions (`start:step:end`) enumerate values while stepping
/// toward the end value; the number of elements is therefore determined by the
/// reachable step count, not by integer-only arithmetic.
fn compute_range_len_real(start: f64, step: f64, end: f64) -> usize {
    const STEP_EPS: f64 = 1e-12;
    if step.abs() <= STEP_EPS {
        return 0;
    }

    let delta = end - start;
    if (step > 0.0 && delta < -STEP_EPS) || (step < 0.0 && delta > STEP_EPS) {
        return 0;
    }

    let n = delta / step;
    if !n.is_finite() {
        return 0;
    }

    // Tolerate minor floating-point roundoff near integer boundaries.
    let eps = (n.abs() * 1e-12).max(1e-12);
    let len = (n + eps).floor() + 1.0;
    if len.is_finite() && len > 0.0 {
        len as usize
    } else {
        0
    }
}

fn infer_range_len_numeric(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<usize> {
    let int_start = eval_integer_with_scope(start, ctx, scope);
    let int_end = eval_integer_with_scope(end, ctx, scope);
    let int_step = step
        .map(|x| eval_integer_with_scope(x, ctx, scope))
        .unwrap_or(Some(1));
    if let (Some(s), Some(e), Some(st)) = (int_start, int_end, int_step)
        && st != 0
    {
        return Some(compute_range_len(s, st, e));
    }

    let s = eval_real_with_scope(start, ctx, scope)?;
    let e = eval_real_with_scope(end, ctx, scope)?;
    let st = step
        .map(|x| eval_real_with_scope(x, ctx, scope))
        .unwrap_or(Some(1.0))?;
    Some(compute_range_len_real(s, st, e))
}
mod dimension_inference;

mod eval_lookup_impl;

#[cfg(test)]
mod function_eval_tests;

mod late_inference;
pub use late_inference::*;
