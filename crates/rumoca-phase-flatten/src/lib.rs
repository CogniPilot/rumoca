//! Flatten phase for the Rumoca compiler.
//!
//! This crate implements the flattening pass that converts an ast::InstancedTree to a flat::Model.
//! It produces a flat equation system with globally unique variable names (MLS §5.6).
//!
//! # Overview
//!
//! Flattening is responsible for:
//! - Converting hierarchical instance tree to flat variables
//! - Generating globally unique variable names (e.g., "body.position.x")
//! - Expanding connection equations per MLS §9
//! - Converting equations to residual form (0 = residual)
//! - Preserving algorithm sections per SPEC_0020
//!
//! # Tracing (SPEC_0024)
//!
//! Enable the `tracing` feature for detailed diagnostic output:
//! ```bash
//! RUST_LOG=rumoca_phase_flatten=debug cargo test test_model -- --nocapture
//! ```
//!
//! # Example
//!
//! ```ignore
//! use rumoca_phase_flatten::flatten;
//!
//! let instanced: ast::InstancedTree = instantiate(typed, "MyModel")?;
//! let flat: flat::Model = flatten(instanced)?;
//! ```

mod algorithms;
mod array_comprehension;
mod ast_lower;
mod boolean_eval;
mod connections;
#[cfg(test)]
mod context_suffix_tests;
mod enum_literals;
mod equations;
mod errors;
mod flat_eval;
mod function_lowering;
mod functions;
mod outer_refs;
mod path_utils;
mod pipeline;
mod postprocess;
pub mod qualify;
mod variables;
mod vcg;
mod when_equations;

use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

use indexmap::IndexMap;
use rumoca_core::Span;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

pub use errors::{FlattenError, FlattenResult};

type ClassDef = ast::ClassDef;
type ClassInstanceData = ast::ClassInstanceData;
type ClassTree = ast::ClassTree;
type InstanceOverlay = ast::InstanceOverlay;
type InstanceStatement = ast::InstanceStatement;
type OpBinary = rumoca_ir_core::OpBinary;
type OpUnary = rumoca_ir_core::OpUnary;
type QualifiedName = ast::QualifiedName;
type Algorithm = flat::Algorithm;
type Expression = flat::Expression;
type Function = flat::Function;
type Literal = flat::Literal;
type Model = flat::Model;
type Subscript = flat::Subscript;
type VarName = flat::VarName;

pub(crate) type ResolveDefMap = indexmap::IndexMap<rumoca_core::DefId, String>;

use flat_eval::{
    ParamEvalContext, build_eval_context, eval_user_func_real, infer_array_dimensions,
    infer_array_dimensions_full_with_conds, looks_like_enum_literal_path,
    try_eval_flat_expr_boolean, try_eval_flat_expr_enum, try_eval_flat_expr_real,
    try_eval_integer_with_context, try_infer_better_dims,
};
use pipeline::*;
use postprocess::*;

/// Options controlling flatten strictness.
#[derive(Debug, Clone, Copy)]
pub struct FlattenOptions {
    /// Whether to enforce connection type/dimension validation.
    pub strict_connection_validation: bool,
}

impl Default for FlattenOptions {
    fn default() -> Self {
        Self {
            strict_connection_validation: true,
        }
    }
}

/// Aggregate timing for a flatten subpass across all flattened models.
#[derive(Debug, Clone, Copy, Default)]
pub struct FlattenPhaseTimingStat {
    /// Number of times this subpass executed.
    pub calls: u64,
    /// Total wall-clock time spent in this subpass.
    pub total_nanos: u64,
}

impl FlattenPhaseTimingStat {
    pub fn total_seconds(self) -> f64 {
        self.total_nanos as f64 / 1_000_000_000.0
    }
}

/// Snapshot of flatten subpass timing accumulators.
#[derive(Debug, Clone, Copy, Default)]
pub struct FlattenPhaseTimingSnapshot {
    pub connections: FlattenPhaseTimingStat,
    pub eval_fallback: FlattenPhaseTimingStat,
}

static CONNECTIONS_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static CONNECTIONS_CALLS: AtomicU64 = AtomicU64::new(0);
static EVAL_FALLBACK_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static EVAL_FALLBACK_CALLS: AtomicU64 = AtomicU64::new(0);

fn duration_to_nanos(duration: Duration) -> u64 {
    let nanos = duration.as_nanos();
    if nanos > u128::from(u64::MAX) {
        u64::MAX
    } else {
        nanos as u64
    }
}

fn timing_stat(total_nanos: &AtomicU64, calls: &AtomicU64) -> FlattenPhaseTimingStat {
    FlattenPhaseTimingStat {
        calls: calls.load(Ordering::Relaxed),
        total_nanos: total_nanos.load(Ordering::Relaxed),
    }
}

pub(crate) fn record_connections_timing(duration: Duration) {
    let nanos = duration_to_nanos(duration);
    CONNECTIONS_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
    CONNECTIONS_CALLS.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn record_eval_fallback_timing(duration: Duration) {
    let nanos = duration_to_nanos(duration);
    EVAL_FALLBACK_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
    EVAL_FALLBACK_CALLS.fetch_add(1, Ordering::Relaxed);
}

#[inline]
pub(crate) fn maybe_start_timer() -> Option<Instant> {
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[inline]
pub(crate) fn maybe_record_connections_timing(start: Option<Instant>) {
    if let Some(start) = start {
        record_connections_timing(start.elapsed());
    }
}

#[inline]
pub(crate) fn maybe_record_eval_fallback_timing(start: Option<Instant>) {
    if let Some(start) = start {
        record_eval_fallback_timing(start.elapsed());
    }
}

/// Reset global flatten subpass timing accumulators.
pub fn reset_flatten_phase_timing_stats() {
    CONNECTIONS_TOTAL_NANOS.store(0, Ordering::Relaxed);
    CONNECTIONS_CALLS.store(0, Ordering::Relaxed);
    EVAL_FALLBACK_TOTAL_NANOS.store(0, Ordering::Relaxed);
    EVAL_FALLBACK_CALLS.store(0, Ordering::Relaxed);
}

/// Snapshot global flatten subpass timing accumulators.
pub fn flatten_phase_timing_stats() -> FlattenPhaseTimingSnapshot {
    FlattenPhaseTimingSnapshot {
        connections: timing_stat(&CONNECTIONS_TOTAL_NANOS, &CONNECTIONS_CALLS),
        eval_fallback: timing_stat(&EVAL_FALLBACK_TOTAL_NANOS, &EVAL_FALLBACK_CALLS),
    }
}

/// Check if a qualified name belongs to a disabled conditional component (MLS §4.8).
fn is_in_disabled_component(
    qn: &ast::QualifiedName,
    disabled_components: &std::collections::HashSet<String>,
) -> bool {
    let parts: Vec<&str> = qn.parts.iter().map(|(name, _)| name.as_str()).collect();
    for disabled in disabled_components {
        let disabled_parts = crate::path_utils::parse_path_with_indices(disabled);
        if parts.len() >= disabled_parts.len()
            && parts[..disabled_parts.len()] == disabled_parts[..]
        {
            return true;
        }
    }
    false
}

/// Choose the better (more complete) of two optional dimension vectors.
fn best_dims(a: Option<&Vec<i64>>, b: Option<&Vec<i64>>) -> Option<Vec<i64>> {
    match (a, b) {
        (Some(av), Some(bv)) => Some(if bv.len() > av.len() {
            bv.clone()
        } else {
            av.clone()
        }),
        (Some(av), None) => Some(av.clone()),
        (None, Some(bv)) => Some(bv.clone()),
        (None, None) => None,
    }
}

/// Flatten an ast::InstancedTree into a flat::Model.
///
/// This is the main entry point for the flatten phase.
///
/// # Arguments
///
/// * `instanced` - The instantiated tree from the instantiate phase
///
/// # Returns
///
/// A `flat::Model` with globally unique variable names and flat equations.
pub fn flatten(instanced: ast::InstancedTree) -> Result<flat::Model, FlattenError> {
    flatten_ref_with_options(
        instanced.inner(),
        instanced.overlay(),
        "",
        FlattenOptions::default(),
    )
}

/// Flatten a model from tree and overlay references.
///
/// This is more efficient for batch compilation as it doesn't require
/// ownership of the tree or overlay.
///
/// # Arguments
///
/// * `tree` - Reference to the class tree
/// * `overlay` - Reference to the instance overlay
///
/// # Returns
///
/// A `flat::Model` with globally unique variable names and flat equations.
pub fn flatten_ref(
    tree: &ast::ClassTree,
    overlay: &ast::InstanceOverlay,
    model_name: &str,
) -> Result<flat::Model, FlattenError> {
    flatten_ref_with_options(tree, overlay, model_name, FlattenOptions::default())
}

/// Flatten a model from references with configurable strictness.
pub fn flatten_ref_with_options(
    tree: &ast::ClassTree,
    overlay: &ast::InstanceOverlay,
    model_name: &str,
    options: FlattenOptions,
) -> Result<flat::Model, FlattenError> {
    let mut ctx = Context::new();
    let mut flat = flat::Model::new();
    let global_imports = collect_global_imports(overlay);
    let component_override_map = build_component_override_map(overlay, tree);

    initialize_flat_metadata(&mut flat, overlay);
    process_component_instances_for_flatten(
        &mut ctx,
        &mut flat,
        overlay,
        &component_override_map,
        tree,
        &global_imports,
    )?;
    let flatten_graph =
        prepare_context_for_equation_flattening(&mut ctx, &mut flat, overlay, tree, model_name);
    process_class_instances_for_flatten(
        &mut ctx,
        &mut flat,
        overlay,
        &component_override_map,
        tree,
    )?;
    finalize_flat_model(
        &mut ctx,
        &mut flat,
        overlay,
        tree,
        model_name,
        options,
        &flatten_graph,
    )?;

    Ok(flat)
}

fn seed_flat_functions_from_context(ctx: &Context, flat: &mut flat::Model) {
    for func in ctx.functions.values() {
        flat.functions
            .entry(func.name.clone())
            .or_insert_with(|| func.clone());
    }
}

fn collect_enum_literal_ordinals(tree: &ast::ClassTree) -> IndexMap<String, i64> {
    let mut ordinals = IndexMap::new();
    for (enum_name, literals) in [
        (
            "StateSelect",
            &["never", "avoid", "default", "prefer", "always"][..],
        ),
        ("AssertionLevel", &["warning", "error"][..]),
    ] {
        for (idx, literal) in literals.iter().enumerate() {
            ordinals.insert(format!("{enum_name}.{literal}"), (idx + 1) as i64);
        }
    }
    for (def_id, qualified_name) in &tree.def_map {
        let Some(class_def) = tree.get_class_by_def_id(*def_id) else {
            continue;
        };
        if class_def.enum_literals.is_empty() {
            continue;
        }
        let short_name = qualified_name.rsplit('.').next().unwrap_or(qualified_name);
        for (idx, literal) in class_def.enum_literals.iter().enumerate() {
            let ordinal = (idx + 1) as i64;
            let ident = literal.ident.text.clone();
            ordinals.insert(format!("{qualified_name}.{ident}"), ordinal);
            ordinals
                .entry(format!("{short_name}.{ident}"))
                .or_insert(ordinal);
        }
    }
    ordinals
}

/// if-equation conditions can be evaluated at compile time.
fn compute_cardinality_counts(ctx: &mut Context, overlay: &ast::InstanceOverlay) {
    for (_def_id, class_data) in &overlay.classes {
        for conn in &class_data.connections {
            if connections::connection_involves_disabled(conn, &overlay.disabled_components) {
                continue;
            }
            let a_path = conn.a.to_flat_string();
            let b_path = conn.b.to_flat_string();
            *ctx.cardinality_counts.entry(a_path).or_insert(0) += 1;
            *ctx.cardinality_counts.entry(b_path).or_insert(0) += 1;
        }
    }
}

/// Pre-collect functions from equations and component bindings for constant evaluation.
///
/// This scans all equations and component bindings in the overlay for function calls
/// and looks up their definitions from the ast::ClassTree. This is needed for evaluating
/// constant expressions like for-loop ranges and parameter bindings that may contain
/// function calls.
fn pre_collect_functions(ctx: &mut Context, overlay: &ast::InstanceOverlay, tree: &ast::ClassTree) {
    use std::collections::HashSet;

    let mut function_names: HashSet<String> = HashSet::new();

    // Scan all class instances for function calls in equations
    for (_def_id, class_data) in &overlay.classes {
        for eq in &class_data.equations {
            collect_function_calls_from_equation(&eq.equation, &mut function_names, tree);
        }
    }

    // Scan all component instances for function calls in bindings
    // This is needed for parameters like `mBasic = numberOfSymmetricBaseSystems(m)`
    for (_def_id, instance_data) in &overlay.components {
        if let Some(binding) = &instance_data.binding {
            collect_function_calls_from_expression(binding, &mut function_names, tree);
        }
        // Also scan start, min, max expressions which may contain function calls
        if let Some(start) = &instance_data.start {
            collect_function_calls_from_expression(start, &mut function_names, tree);
        }
        if let Some(min) = &instance_data.min {
            collect_function_calls_from_expression(min, &mut function_names, tree);
        }
        if let Some(max) = &instance_data.max {
            collect_function_calls_from_expression(max, &mut function_names, tree);
        }
        // Scan dims_expr for function calls (e.g., Real buf[realFFTsamplePoints(...)])
        // These are array dimension expressions that may contain user-defined functions
        for sub in &instance_data.dims_expr {
            if let ast::Subscript::Expression(expr) = sub {
                collect_function_calls_from_expression(expr, &mut function_names, tree);
            }
        }
    }

    // Look up each function and add to context
    // We add both the fully qualified name and the short name (last part)
    // because flat::Expression::FunctionCall uses the textual name from the AST
    // which may be short (imported) or fully qualified
    for func_name in function_names {
        add_function_to_context(&func_name, ctx, tree);
    }
}

/// Add a function to the context if it exists and isn't already present.
fn add_function_to_context(func_name: &str, ctx: &mut Context, tree: &ast::ClassTree) {
    let Some(func) = functions::lookup_function(tree, func_name) else {
        return;
    };
    let qualified_name = func.name.to_string();

    // Add canonical qualified key used by flattened function table.
    if !ctx.functions.contains_key(&qualified_name) {
        ctx.functions.insert(qualified_name.clone(), func.clone());
    }
    // Preserve textual alias key for compile-time eval expressions.
    if !ctx.functions.contains_key(func_name) {
        ctx.functions.insert(func_name.to_string(), func.clone());
    }
    // Also add short-name aliases (from both textual and canonical paths).
    add_function_short_name(func_name, &func, ctx);
    add_function_short_name(&qualified_name, &func, ctx);
}

/// Add function with short name if applicable.
fn add_function_short_name(func_name: &str, func: &flat::Function, ctx: &mut Context) {
    let short_name = crate::path_utils::top_level_last_segment(func_name);
    if short_name != func_name && !ctx.functions.contains_key(short_name) {
        ctx.functions.insert(short_name.to_string(), func.clone());
    }
}

/// Extract a path from an expression, if it's a simple variable reference.
/// Returns None if the expression contains subscripts or is not a simple path.
fn extract_simple_path(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            // Check for subscripts
            let has_subscripts = cr
                .parts
                .iter()
                .any(|p| p.subs.as_ref().is_some_and(|s| !s.is_empty()));
            if has_subscripts || cr.parts.is_empty() {
                return None;
            }
            Some(
                cr.parts
                    .iter()
                    .map(|p| p.ident.text.as_ref())
                    .collect::<Vec<_>>()
                    .join("."),
            )
        }
        ast::Expression::FieldAccess { base, field } => {
            // Recursively extract base path and append field
            let base_path = extract_simple_path(base)?;
            Some(format!("{}.{}", base_path, field))
        }
        _ => None,
    }
}

/// Extract record aliases from the instance overlay (MLS §7.2.3).
///
/// When a component has a VarRef binding (like `battery2.cellData = cellData2`),
/// this creates an alias that allows resolving field access like
/// `battery2.cellData.nRC` to `cellData2.nRC`.
///
/// This must be called before build_parameter_lookup because record aliases
/// are NOT in the flat::Model (records are expanded into fields).
///
/// IMPORTANT: Alias targets must be fully qualified. When the binding is from a
/// modification (e.g., `cellData=stackData.cellData`), the target path is relative
/// to the scope where the modification was written, which is the grandparent scope
/// of the component (MLS §7.2.4).
fn extract_record_aliases(ctx: &mut Context, overlay: &ast::InstanceOverlay) {
    for (_def_id, instance_data) in &overlay.components {
        // Check if this component has a binding that's a simple path
        let Some(binding) = &instance_data.binding else {
            continue;
        };

        // Extract the path from the binding (supports both ComponentReference and FieldAccess)
        let Some(alias_target_unqualified) = extract_simple_path(binding) else {
            continue;
        };

        let alias_source = instance_data.qualified_name.to_flat_string();

        // Qualify the alias target based on whether it's from a modification binding.
        // For modification bindings, the target is relative to the grandparent scope
        // (where the modification was written). For declaration bindings, it's relative
        // to the parent scope (sibling variables).
        let alias_target = if instance_data.binding_from_modification {
            // Modification bindings: qualify from modifier lexical scope.
            let source_scope = variables::modification_binding_prefix_pub(instance_data);
            if source_scope.is_empty() {
                alias_target_unqualified
            } else {
                format!(
                    "{}.{}",
                    source_scope.to_flat_string(),
                    alias_target_unqualified
                )
            }
        } else {
            // Declaration bindings: qualify with parent prefix (sibling scope)
            let parent = variables::parent_prefix_pub(&instance_data.qualified_name);
            if parent.is_empty() {
                alias_target_unqualified
            } else {
                format!("{}.{}", parent.to_flat_string(), alias_target_unqualified)
            }
        };

        // Add the alias if it's not self-referential
        if alias_source != alias_target {
            ctx.record_aliases.insert(alias_source, alias_target);
        }
    }

    // Compute transitive closure of aliases (MLS §7.2.3)
    // If A -> B and B -> C, we should have A -> C for efficient resolution.
    compute_transitive_alias_closure(&mut ctx.record_aliases);
}

/// Compute transitive closure of record aliases (MLS §7.2.3).
///
/// If we have aliases A -> B and B -> C, we want to resolve A directly to C
/// for more efficient lookups. This also handles chains like:
/// - stack.cell.cellData -> stack.stackData.cellData
/// - stack.stackData.cellData -> stackData.cellData
/// - stackData.cellData -> cellDataOriginal
///
/// Result: stack.cell.cellData -> cellDataOriginal
///
/// Also synthesizes missing intermediate aliases. If we have:
/// - stack.cell.cell.cellData -> stack.cell.stackData.cellData
/// - stack.stackData -> stackData
///
/// Then we infer: stack.cell.stackData -> stack.stackData
fn compute_transitive_alias_closure(aliases: &mut rustc_hash::FxHashMap<String, String>) {
    synthesize_intermediate_aliases(aliases);
    compute_closure_iterations(aliases, 20);
}

/// Find parent path alias for a prefix (e.g., "stack.cell.stackData" -> "stack.stackData").
fn find_parent_alias(
    prefix_parts: &[&str],
    aliases: &rustc_hash::FxHashMap<String, String>,
) -> Option<String> {
    if prefix_parts.len() < 3 {
        return None;
    }
    let component_name = prefix_parts.last()?;
    for skip in 1..prefix_parts.len() - 1 {
        let parent: Vec<&str> = prefix_parts
            .iter()
            .take(prefix_parts.len() - 1 - skip)
            .chain(std::iter::once(component_name))
            .copied()
            .collect();
        let parent_path = parent.join(".");
        if aliases.contains_key(&parent_path) {
            return Some(parent_path);
        }
    }
    None
}

/// Try to find a synthetic alias for a single prefix.
fn find_synthetic_alias(
    prefix: &str,
    aliases: &rustc_hash::FxHashMap<String, String>,
) -> Option<String> {
    let prefix_parts = crate::path_utils::parse_path_with_indices(prefix);
    find_parent_alias(&prefix_parts, aliases)
}

/// Synthesize missing intermediate aliases for unresolvable prefixes.
fn synthesize_intermediate_aliases(aliases: &mut rustc_hash::FxHashMap<String, String>) {
    let mut synthetic: Vec<(String, String)> = Vec::new();
    for target in aliases.values() {
        let parts = crate::path_utils::parse_path_with_indices(target);
        for i in (2..parts.len()).rev() {
            let prefix = parts[..i].join(".");
            let already_exists =
                aliases.contains_key(&prefix) || synthetic.iter().any(|(s, _)| s == &prefix);
            if already_exists {
                continue;
            }
            if let Some(parent_path) = find_synthetic_alias(&prefix, aliases) {
                synthetic.push((prefix, parent_path));
                break;
            }
        }
    }
    aliases.extend(synthetic);
}

/// Try to resolve a path through prefix-based alias resolution.
fn resolve_through_prefix(
    current: &str,
    source: &str,
    aliases: &rustc_hash::FxHashMap<String, String>,
) -> Option<String> {
    let parts = crate::path_utils::parse_path_with_indices(current);
    for i in (1..parts.len()).rev() {
        let prefix = parts[..i].join(".");
        if let Some(alias_target) = aliases.get(&prefix) {
            let suffix = parts[i..].join(".");
            let resolved = format!("{}.{}", alias_target, suffix);
            if resolved != current && resolved != source {
                return Some(resolved);
            }
        }
    }
    None
}

/// Resolve an alias target through the chain of aliases.
fn resolve_alias_chain(
    target: &str,
    source: &str,
    aliases: &rustc_hash::FxHashMap<String, String>,
) -> Option<String> {
    let mut current = target.to_string();
    for _ in 0..10 {
        // Try exact match
        if let Some(next) = aliases.get(&current)
            && next != &current
            && next != source
        {
            current = next.clone();
            continue;
        }
        // Try prefix resolution
        if let Some(resolved) = resolve_through_prefix(&current, source, aliases) {
            current = resolved;
            continue;
        }
        break;
    }
    (current != target && current != source).then_some(current)
}

/// Compute transitive closure through iterative resolution.
fn compute_closure_iterations(
    aliases: &mut rustc_hash::FxHashMap<String, String>,
    max_iter: usize,
) {
    for _ in 0..max_iter {
        let updates: Vec<(String, String)> = aliases
            .iter()
            .filter_map(|(source, target)| {
                resolve_alias_chain(target, source, aliases).map(|new| (source.clone(), new))
            })
            .collect();
        if updates.is_empty() {
            break;
        }
        aliases.extend(updates);
    }
}

/// Resolve constant values from the ast::ClassTree into the evaluation context.
///
/// Looks up constants like `Modelica.Constants.eps` by navigating the ast::ClassTree:
/// the package path (e.g., "Modelica.Constants") is looked up as a class, then
/// each component with a literal binding is extracted and added to the eval context.
///
/// This replaces hardcoded constant values with values from the actual parsed library.
fn resolve_constants_from_tree(
    tree: &ast::ClassTree,
    eval_ctx: &mut rumoca_eval_flat::constant::EvalContext,
) {
    // Well-known constant packages to resolve.
    // ModelicaServices.Machine must come first since Modelica.Constants.eps
    // references ModelicaServices.Machine.eps.
    const CONSTANT_PACKAGES: &[&str] = &["ModelicaServices.Machine", "Modelica.Constants"];

    for &pkg_name in CONSTANT_PACKAGES {
        let Some(class_def) = tree.get_class_by_qualified_name(pkg_name) else {
            continue;
        };
        for (comp_name, component) in &class_def.components {
            let qualified = format!("{}.{}", pkg_name, comp_name);
            if eval_ctx.get(&qualified).is_some() {
                continue;
            }
            let Some(binding) = &component.binding else {
                continue;
            };
            let flat_binding = qualify_expression(binding, &ast::QualifiedName::new());
            let Ok(val) = rumoca_eval_flat::constant::eval_expr(&flat_binding, eval_ctx) else {
                continue;
            };
            eval_ctx.add_parameter(qualified, val);
        }
    }
}

fn inject_referenced_qualified_class_constants(
    tree: &ClassTree,
    model_name: &str,
    flat: &Model,
    ctx: &mut Context,
) {
    const WELL_KNOWN_CONSTANT_PACKAGES: &[&str] =
        &["ModelicaServices.Machine", "Modelica.Constants"];
    const MAX_PASSES: usize = 4;
    let live_vars: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for _ in 0..MAX_PASSES {
        let prev = context_constant_footprint(ctx);
        let mut scopes = HashSet::new();
        collect_referenced_class_scopes(flat, &live_vars, &mut scopes);
        collect_context_constant_class_scopes(ctx, &live_vars, &mut scopes);
        for package in WELL_KNOWN_CONSTANT_PACKAGES {
            scopes.insert((*package).to_string());
        }

        for scope in scopes {
            let resolved = resolve_referenced_scope_class(tree, &scope, model_name);
            let Some((resolved_scope, class_def)) = resolved else {
                continue;
            };
            inject_class_import_enum_aliases(tree, &scope, class_def, ctx);
            extract_constants_from_class_with_prefix(&scope, class_def, ctx);
            let resolve_context = class_def
                .def_id
                .and_then(|id| tree.def_map.get(&id).cloned())
                .unwrap_or(resolved_scope);
            for ext in &class_def.extends {
                apply_extends_constants_for_scope(tree, &scope, ext, &resolve_context, ctx);
            }
        }

        if context_constant_footprint(ctx) == prev {
            break;
        }
    }
}

fn resolve_referenced_scope_class<'a>(
    tree: &'a ClassTree,
    scope: &str,
    model_name: &str,
) -> Option<(String, &'a ClassDef)> {
    if let Some(class_def) = tree.get_class_by_qualified_name(scope) {
        return Some((scope.to_string(), class_def));
    }

    let (class_def, resolved_name) = resolve_class_in_scope(tree, scope, model_name);
    let class_def = class_def?;
    Some((
        resolved_name.unwrap_or_else(|| scope.to_string()),
        class_def,
    ))
}

fn collect_context_constant_class_scopes(
    ctx: &Context,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    for value in ctx.constant_values.values() {
        collect_expression_class_scopes(value, live_vars, scopes);
    }
    for value in ctx.enum_parameter_values.values() {
        maybe_add_referenced_class_scope(value, live_vars, scopes);
    }
}

fn inject_class_import_enum_aliases(
    tree: &ClassTree,
    scope: &str,
    class_def: &ClassDef,
    ctx: &mut Context,
) {
    for import in &class_def.imports {
        let ast::Import::Renamed { alias, path, .. } = import else {
            continue;
        };
        let target_path = path.to_string();
        let Some(enum_class) = tree.get_class_by_qualified_name(&target_path) else {
            continue;
        };
        if enum_class.enum_literals.is_empty() {
            continue;
        }
        for literal in &enum_class.enum_literals {
            let lit = literal.ident.text.as_ref();
            let key = format!("{scope}.{}.{}", alias.text, lit);
            let value = format!("{target_path}.{lit}");
            ctx.enum_parameter_values
                .entry(key)
                .or_insert_with(|| value.clone());
        }
    }
}

fn context_constant_footprint(ctx: &Context) -> usize {
    ctx.parameter_values.len()
        + ctx.real_parameter_values.len()
        + ctx.boolean_parameter_values.len()
        + ctx.enum_parameter_values.len()
        + ctx.constant_values.len()
        + ctx.array_dimensions.len()
}

fn collect_referenced_class_scopes(
    flat: &Model,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    for eq in &flat.equations {
        collect_expression_class_scopes(&eq.residual, live_vars, scopes);
    }
    for eq in &flat.initial_equations {
        collect_expression_class_scopes(&eq.residual, live_vars, scopes);
    }

    for when in &flat.when_clauses {
        collect_expression_class_scopes(&when.condition, live_vars, scopes);
        for eq in &when.equations {
            collect_when_equation_class_scopes(eq, live_vars, scopes);
        }
    }

    for alg in &flat.algorithms {
        for stmt in &alg.statements {
            collect_statement_class_scopes(stmt, live_vars, scopes);
        }
    }

    for var in flat.variables.values() {
        if let Some(binding) = &var.binding {
            collect_expression_class_scopes(binding, live_vars, scopes);
        }
        if let Some(start) = &var.start {
            collect_expression_class_scopes(start, live_vars, scopes);
        }
        if let Some(min) = &var.min {
            collect_expression_class_scopes(min, live_vars, scopes);
        }
        if let Some(max) = &var.max {
            collect_expression_class_scopes(max, live_vars, scopes);
        }
        if let Some(nominal) = &var.nominal {
            collect_expression_class_scopes(nominal, live_vars, scopes);
        }
    }

    for function in flat.functions.values() {
        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                collect_expression_class_scopes(default_expr, live_vars, scopes);
            }
        }
        for statement in &function.body {
            collect_statement_class_scopes(statement, live_vars, scopes);
        }
    }
}

fn collect_expression_class_scopes(
    expr: &Expression,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    for name in refs {
        maybe_add_referenced_class_scope(name.as_str(), live_vars, scopes);
    }
    collect_constructor_class_scopes(expr, live_vars, scopes);
}

fn collect_constructor_class_scopes(
    expr: &Expression,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    match expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            if *is_constructor {
                let constructor_name = name.as_str();
                if constructor_name != "time" && !live_vars.contains(constructor_name) {
                    scopes.insert(constructor_name.to_string());
                }
            }
            for arg in args {
                collect_constructor_class_scopes(arg, live_vars, scopes);
            }
        }
        Expression::BuiltinCall { args, .. } => {
            for arg in args {
                collect_constructor_class_scopes(arg, live_vars, scopes);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_constructor_class_scopes(lhs, live_vars, scopes);
            collect_constructor_class_scopes(rhs, live_vars, scopes);
        }
        Expression::Unary { rhs, .. } => {
            collect_constructor_class_scopes(rhs, live_vars, scopes);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_constructor_class_scopes(cond, live_vars, scopes);
                collect_constructor_class_scopes(value, live_vars, scopes);
            }
            collect_constructor_class_scopes(else_branch, live_vars, scopes);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_constructor_class_scopes(element, live_vars, scopes);
            }
        }
        Expression::Range { start, step, end } => {
            collect_constructor_class_scopes(start, live_vars, scopes);
            if let Some(step) = step {
                collect_constructor_class_scopes(step, live_vars, scopes);
            }
            collect_constructor_class_scopes(end, live_vars, scopes);
        }
        Expression::Index { base, subscripts } => {
            collect_constructor_class_scopes(base, live_vars, scopes);
            for subscript in subscripts {
                if let Subscript::Expr(expr) = subscript {
                    collect_constructor_class_scopes(expr, live_vars, scopes);
                }
            }
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_constructor_class_scopes(expr, live_vars, scopes);
            for index in indices {
                collect_constructor_class_scopes(&index.range, live_vars, scopes);
            }
            if let Some(filter_expr) = filter {
                collect_constructor_class_scopes(filter_expr, live_vars, scopes);
            }
        }
        Expression::FieldAccess { base, .. } => {
            collect_constructor_class_scopes(base, live_vars, scopes);
        }
        _ => {}
    }
}

fn collect_when_equation_class_scopes(
    eq: &flat::WhenEquation,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    match eq {
        flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
            collect_expression_class_scopes(value, live_vars, scopes)
        }
        flat::WhenEquation::Assert { condition, .. } => {
            collect_expression_class_scopes(condition, live_vars, scopes)
        }
        flat::WhenEquation::Terminate { .. } => {}
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (cond, eqs) in branches {
                collect_expression_class_scopes(cond, live_vars, scopes);
                for nested in eqs {
                    collect_when_equation_class_scopes(nested, live_vars, scopes);
                }
            }
            for nested in else_branch {
                collect_when_equation_class_scopes(nested, live_vars, scopes);
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            collect_expression_class_scopes(function, live_vars, scopes)
        }
    }
}

fn collect_statement_class_scopes(
    stmt: &flat::Statement,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            collect_expression_class_scopes(value, live_vars, scopes)
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                collect_expression_class_scopes(&idx.range, live_vars, scopes);
            }
            for nested in equations {
                collect_statement_class_scopes(nested, live_vars, scopes);
            }
        }
        flat::Statement::While(block) => {
            collect_expression_class_scopes(&block.cond, live_vars, scopes);
            for nested in &block.stmts {
                collect_statement_class_scopes(nested, live_vars, scopes);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expression_class_scopes(&block.cond, live_vars, scopes);
                for nested in &block.stmts {
                    collect_statement_class_scopes(nested, live_vars, scopes);
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    collect_statement_class_scopes(nested, live_vars, scopes);
                }
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                collect_expression_class_scopes(&block.cond, live_vars, scopes);
                for nested in &block.stmts {
                    collect_statement_class_scopes(nested, live_vars, scopes);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                collect_expression_class_scopes(arg, live_vars, scopes);
            }
            for output in outputs {
                collect_expression_class_scopes(output, live_vars, scopes);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            collect_expression_class_scopes(value, live_vars, scopes)
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            collect_expression_class_scopes(condition, live_vars, scopes);
            collect_expression_class_scopes(message, live_vars, scopes);
            if let Some(level) = level {
                collect_expression_class_scopes(level, live_vars, scopes);
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn maybe_add_referenced_class_scope(
    name: &str,
    live_vars: &HashSet<String>,
    scopes: &mut HashSet<String>,
) {
    if name == "time" || live_vars.contains(name) {
        return;
    }

    let Some(scope) = path_utils::parent_scope(name) else {
        return;
    };
    let last = path_utils::top_level_last_segment(name);
    let base_last = path_utils::strip_array_index(last);
    if base_last.is_empty() {
        return;
    }

    let normalized_name = format!("{scope}.{base_last}");
    if live_vars.contains(&normalized_name) {
        return;
    }

    scopes.insert(scope.to_string());
}

/// Inject constants from the enclosing class into the flatten context (MLS §7.3).
///
/// When compiling a nested model like `Modelica.Media.Air.MoistAir.BaseProperties`,
/// constants from the enclosing package (e.g., `nX`, `nXi`, `nS`) must be available
/// for for-equation range evaluation. This walks the enclosing class's extends chain
/// to collect all inherited constants.
/// Inject constants from nested class declarations in the model being compiled (MLS §7.3).
///
/// When a model has `package Medium = SomeMedium`, constants like nX, nXi, nS
/// from that package need to be available for for-equation range evaluation.
/// Walks the model's ancestor chain and extracts constants from each ancestor's
/// nested class declarations (following extends chains to concrete types).
fn inject_model_nested_class_constants(tree: &ast::ClassTree, model_name: &str, ctx: &mut Context) {
    let model_ancestors = collect_ancestor_classes(tree, model_name);
    if model_ancestors.is_empty() {
        return;
    }
    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len();
        for ancestor in &model_ancestors {
            extract_nested_class_constants(tree, ancestor, model_name, ctx);
        }
        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len();
        if new == prev {
            break;
        }
    }
}

/// Inject constants from direct model-level extends redeclare package overrides.
///
/// MLS §7.3: `extends(... redeclare package Alias = Pkg)` defines the effective
/// package alias in the derived model scope. Flatten-time constant lookup for
/// expressions like `Alias.nXi` must observe this effective package.
fn inject_model_extends_redeclare_constants(
    tree: &ast::ClassTree,
    model_name: &str,
    ctx: &mut Context,
) {
    let model_class = tree.get_class_by_qualified_name(model_name).or_else(|| {
        tree.get_class_by_qualified_name(crate::path_utils::top_level_last_segment(model_name))
    });
    let Some(model_class) = model_class else {
        return;
    };

    let resolve_context = model_class
        .def_id
        .and_then(|id| tree.def_map.get(&id).map(String::as_str))
        .unwrap_or(model_name);
    let redeclare_packages = collect_model_redeclare_packages(tree, model_class, resolve_context);
    if redeclare_packages.is_empty() {
        return;
    }

    const MAX_PASSES: usize = 5;
    for _pass in 0..MAX_PASSES {
        let prev = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.enum_parameter_values.len();

        for (alias_name, package_class, package_context) in &redeclare_packages {
            extract_constants_from_class_with_prefix(alias_name, package_class, ctx);
            for pkg_ext in &package_class.extends {
                apply_extends_constants_for_scope(tree, alias_name, pkg_ext, package_context, ctx);
            }
        }

        let new = ctx.parameter_values.len()
            + ctx.array_dimensions.len()
            + ctx.boolean_parameter_values.len()
            + ctx.enum_parameter_values.len();
        if new == prev {
            break;
        }
    }
}

/// Collect direct model-level `extends(... redeclare package Alias = Pkg)` entries.
fn collect_model_redeclare_packages<'a>(
    tree: &'a ast::ClassTree,
    model_class: &'a ast::ClassDef,
    resolve_context: &str,
) -> Vec<(String, &'a ast::ClassDef, String)> {
    let mut entries = Vec::new();
    for ext in &model_class.extends {
        for ext_mod in &ext.modifications {
            if let Some(entry) =
                resolve_model_redeclare_package_entry(tree, resolve_context, ext_mod)
            {
                entries.push(entry);
            }
        }
    }
    entries
}

/// Resolve one model-level redeclare modifier to `(alias, package_class, package_context)`.
fn resolve_model_redeclare_package_entry<'a>(
    tree: &'a ast::ClassTree,
    resolve_context: &str,
    ext_mod: &rumoca_ir_ast::ExtendModification,
) -> Option<(String, &'a ast::ClassDef, String)> {
    if !ext_mod.redeclare {
        return None;
    }

    let ast::Expression::Modification { target, value } = &ext_mod.expr else {
        return None;
    };
    if target.parts.len() != 1 {
        return None;
    }

    let alias_name = target.parts[0].ident.text.to_string();
    if !alias_name.starts_with(char::is_uppercase) {
        return None;
    }

    let package_name = redeclare_package_name(value)?;
    let package_class = resolve_class_in_scope(tree, &package_name, resolve_context)
        .0
        .or_else(|| tree.get_class_by_qualified_name(&package_name))?;
    if !matches!(package_class.class_type, rumoca_ir_ast::ClassType::Package) {
        return None;
    }

    let package_context = package_class
        .def_id
        .and_then(|id| tree.def_map.get(&id).cloned())
        .unwrap_or(package_name);
    Some((alias_name, package_class, package_context))
}

/// Extract the replacement package name from a redeclare modifier expression.
fn redeclare_package_name(value: &ast::Expression) -> Option<String> {
    match value {
        ast::Expression::ClassModification { target, .. } => Some(target.to_string()),
        ast::Expression::ComponentReference(cr) => Some(cr.to_string()),
        _ => None,
    }
}

/// Extract constants from nested class declarations of a class definition.
/// For each nested class (e.g., `package Medium = ...`), follow extends chains
/// and extract integer constants and array dimensions.
fn extract_nested_class_constants(
    tree: &ast::ClassTree,
    class_def: &ast::ClassDef,
    resolve_context: &str,
    ctx: &mut Context,
) {
    for (nested_name, nested_class) in &class_def.classes {
        // MLS §7.3 alias constants come from package aliases; nested models/records
        // are not visible as unqualified constant scopes for this model context.
        if !matches!(nested_class.class_type, ast::ClassType::Package) {
            continue;
        }
        extract_constants_from_class_with_prefix(nested_name, nested_class, ctx);
        let nested_context = nested_class
            .def_id
            .and_then(|id| tree.def_map.get(&id).map(String::as_str))
            .unwrap_or(resolve_context);
        for ext in &nested_class.extends {
            apply_extends_constants_for_scope(tree, nested_name, ext, nested_context, ctx);
        }
    }
}

/// Recursively extract constants from an extends chain, using a prefix alias.
/// Uses scope-based resolution for relative extends names.
fn extract_extends_chain_constants(
    tree: &ast::ClassTree,
    alias: &str,
    base_name: &str,
    resolve_context: &str,
    ctx: &mut Context,
) {
    let mut visited = std::collections::HashSet::new();
    extract_extends_chain_constants_inner(
        tree,
        alias,
        base_name,
        resolve_context,
        ctx,
        &mut visited,
    );
}

fn extract_extends_chain_constants_inner(
    tree: &ast::ClassTree,
    alias: &str,
    base_name: &str,
    resolve_context: &str,
    ctx: &mut Context,
    visited: &mut std::collections::HashSet<String>,
) {
    let (base_class, resolved_qname) = resolve_class_in_scope(tree, base_name, resolve_context);
    let Some(base_class) = base_class else {
        return;
    };
    let qname = resolved_qname.unwrap_or_else(|| base_name.to_string());
    if !visited.insert(qname.clone()) {
        return;
    }
    extract_constants_from_class_with_prefix(alias, base_class, ctx);
    for ext in &base_class.extends {
        extract_extends_modification_constants(tree, alias, ext, &qname, ctx);
        if let Some(base_qname) =
            resolve_extends_base_qname(tree, &ext.base_name.to_string(), &qname)
            && base_qname != alias
        {
            extract_extends_modification_constants(tree, &base_qname, ext, &base_qname, ctx);
        }
        extract_extends_chain_constants_inner(
            tree,
            alias,
            &ext.base_name.to_string(),
            &qname,
            ctx,
            visited,
        );
    }
}

/// Extract constant-affecting extends modifiers into the flatten context.
///
/// MLS §7.2/§7.3: extends modifiers override inherited constants and must be
/// applied before evaluating dependent constants such as nS/nX/nXi.
fn extract_extends_modification_constants(
    tree: &ast::ClassTree,
    prefix: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) {
    for ext_mod in &ext.modifications {
        if ext_mod.redeclare {
            continue;
        }
        extract_extends_modification_expr(tree, prefix, &ext_mod.expr, resolve_context, ctx);
    }
}

/// Apply `redeclare package Alias = SomePackage` extends modifiers by injecting
/// constants from the redeclared package into the current scope.
fn extract_extends_redeclare_package_constants(
    tree: &ast::ClassTree,
    prefix: &str,
    ext: &rumoca_ir_ast::Extend,
    resolve_context: &str,
    ctx: &mut Context,
) {
    for ext_mod in &ext.modifications {
        if !ext_mod.redeclare {
            continue;
        }
        let ast::Expression::Modification { target, value } = &ext_mod.expr else {
            continue;
        };
        if target.parts.len() != 1 {
            continue;
        }

        let alias_name = target.parts[0].ident.text.as_ref();
        if !alias_name.starts_with(char::is_uppercase) {
            continue;
        }

        let package_name = match value.as_ref() {
            ast::Expression::ClassModification { target, .. } => target.to_string(),
            ast::Expression::ComponentReference(cr) => cr.to_string(),
            _ => continue,
        };

        let package_class = resolve_class_in_scope(tree, &package_name, resolve_context)
            .0
            .or_else(|| tree.get_class_by_qualified_name(&package_name));
        let Some(package_class) = package_class else {
            continue;
        };
        if !matches!(package_class.class_type, rumoca_ir_ast::ClassType::Package) {
            continue;
        }

        let alias_scope = make_prefixed_name(prefix, alias_name);
        extract_constants_from_class_with_prefix(&alias_scope, package_class, ctx);
        extract_constants_from_class_with_prefix(prefix, package_class, ctx);

        for pkg_ext in &package_class.extends {
            extract_extends_modification_constants(
                tree,
                &alias_scope,
                pkg_ext,
                resolve_context,
                ctx,
            );
            extract_extends_chain_constants(
                tree,
                &alias_scope,
                &pkg_ext.base_name.to_string(),
                resolve_context,
                ctx,
            );

            extract_extends_modification_constants(tree, prefix, pkg_ext, resolve_context, ctx);
            extract_extends_chain_constants(
                tree,
                prefix,
                &pkg_ext.base_name.to_string(),
                resolve_context,
                ctx,
            );
        }
    }
}

/// Walk an extends-modification expression and record scalar/dimension overrides.
fn extract_extends_modification_expr(
    tree: &ast::ClassTree,
    prefix: &str,
    expr: &ast::Expression,
    resolve_context: &str,
    ctx: &mut Context,
) {
    if let ast::Expression::Modification { target, value } = expr {
        let target_name = target.to_string();
        let full_name = make_prefixed_name(prefix, &target_name);
        let mut imports = crate::qualify::ImportMap::default();
        crate::qualify::collect_lexical_package_aliases(tree, resolve_context, &mut imports);
        let qualified_value = crate::qualify::qualify_expression_with_imports(
            value,
            &ast::QualifiedName::new(),
            crate::qualify::QualifyOptions::default(),
            &imports,
        );
        // MLS §7.2: extends modifiers override inherited declarations.
        // Preserve explicitly modified keys from later default-constant extraction.
        ctx.modified_constant_keys.insert(full_name.clone());
        if let Some(val) = try_eval_const_integer_with_scope(&qualified_value, ctx, prefix) {
            insert_with_prefix(
                &mut ctx.parameter_values,
                prefix,
                &target_name,
                &full_name,
                val,
            );
        }
        if let Some(val) = try_eval_const_boolean_with_scope(&qualified_value, ctx, prefix) {
            insert_with_prefix(
                &mut ctx.boolean_parameter_values,
                prefix,
                &target_name,
                &full_name,
                val,
            );
        }
        if let Some(val) = try_eval_const_real_with_scope(&qualified_value, ctx, prefix)
            && val.is_finite()
        {
            insert_with_prefix(
                &mut ctx.real_parameter_values,
                prefix,
                &target_name,
                &full_name,
                val,
            );
        }
        if let Some(val) = try_eval_const_flat_expr_with_scope(&qualified_value, ctx, prefix) {
            insert_with_prefix(
                &mut ctx.constant_values,
                prefix,
                &target_name,
                &full_name,
                val,
            );
        } else if let Some(val) =
            try_extract_named_record_constructor_constant(&qualified_value, ctx, prefix, &full_name)
        {
            insert_with_prefix(
                &mut ctx.constant_values,
                prefix,
                &target_name,
                &full_name,
                val,
            );
        } else if let ast::Expression::ComponentReference(_) = &qualified_value {
            let symbolic =
                crate::ast_lower::expression_from_ast_with_def_map(&qualified_value, None);
            insert_with_prefix(
                &mut ctx.constant_values,
                prefix,
                &target_name,
                &full_name,
                symbolic,
            );
        }
        if let Some(val) = try_eval_const_enum_with_scope(&qualified_value, ctx, prefix) {
            insert_with_prefix(
                &mut ctx.enum_parameter_values,
                prefix,
                &target_name,
                &full_name,
                val,
            );
        }
        if let Some(dims) = infer_dims_from_expr(&qualified_value, ctx, prefix) {
            insert_with_prefix(
                &mut ctx.array_dimensions,
                prefix,
                &target_name,
                &full_name,
                dims,
            );
        }
    }
}

/// Extract integer constants and array dimensions from a class, using a prefix for names.
/// Constants are stored as both `prefix.name` (e.g., `Medium.nX`) and unprefixed `name`.
fn extract_constants_from_class_with_prefix(
    prefix: &str,
    class_def: &ast::ClassDef,
    ctx: &mut Context,
) {
    for (name, comp) in &class_def.components {
        if !matches!(
            comp.variability,
            rumoca_ir_core::Variability::Constant(_) | rumoca_ir_core::Variability::Parameter(_)
        ) {
            continue;
        }
        let binding = comp
            .binding
            .as_ref()
            .or(if !matches!(comp.start, ast::Expression::Empty) {
                Some(&comp.start)
            } else {
                None
            });
        let synthesized = if binding.is_none() {
            synthesize_component_modification_binding(comp)
        } else {
            None
        };
        let expr = binding.or(synthesized.as_ref());
        let Some(expr) = expr else { continue };
        let full_name = make_prefixed_name(prefix, name);
        extract_single_constant_with_prefix(prefix, name, &full_name, comp, expr, ctx);
    }
}

/// Extract a single constant value (integer or array dims) into the context.
fn extract_single_constant_with_prefix(
    prefix: &str,
    name: &str,
    full_name: &str,
    comp: &rumoca_ir_ast::Component,
    expr: &ast::Expression,
    ctx: &mut Context,
) {
    // Flat model variables are the authoritative source for instantiated
    // parameter/constant values. Do not inject declaration defaults for names
    // that already exist in flat output; those defaults can override modifier-
    // derived bindings (MLS §7.2.4).
    if ctx.flat_parameter_constant_keys.contains(full_name) {
        return;
    }

    let type_name = comp.type_name.to_string();
    let preserve_existing = ctx.modified_constant_keys.contains(full_name)
        || ctx.flat_parameter_constant_keys.contains(full_name);
    if let Some(val) = try_extract_named_record_constructor_constant(expr, ctx, prefix, full_name)
        && (!preserve_existing || !ctx.constant_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.constant_values, prefix, name, full_name, val);
    }
    // Integer constants
    if type_name == "Integer"
        && let Some(val) = try_eval_const_integer_with_scope(expr, ctx, prefix)
        && (!preserve_existing || !ctx.parameter_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.parameter_values, prefix, name, full_name, val);
    }
    // Boolean constants (needed for evaluating conditional integer constants like
    // `nXi = if fixedX then 0 else nS - 1` in replaceable packages)
    if type_name == "Boolean"
        && let Some(val) = try_eval_const_boolean_with_scope(expr, ctx, prefix)
        && (!preserve_existing || !ctx.boolean_parameter_values.contains_key(full_name))
    {
        insert_with_prefix(
            &mut ctx.boolean_parameter_values,
            prefix,
            name,
            full_name,
            val,
        );
    }
    // Real constants (and constants of aliased Real-derived units).
    if let Some(val) = try_eval_const_real_with_scope(expr, ctx, prefix)
        && val.is_finite()
        && (!preserve_existing || !ctx.real_parameter_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.real_parameter_values, prefix, name, full_name, val);
    }
    if let Some(val) = try_eval_const_flat_expr_with_scope(expr, ctx, prefix)
        && (!preserve_existing || !ctx.constant_values.contains_key(full_name))
    {
        insert_with_prefix(&mut ctx.constant_values, prefix, name, full_name, val);
    }
    // Enumeration constants (e.g., `ThermoStates = IndependentVariables.ph`)
    if (!preserve_existing || !ctx.enum_parameter_values.contains_key(full_name))
        && let Some(val) = try_eval_const_enum_with_scope(expr, ctx, prefix)
    {
        insert_with_prefix(&mut ctx.enum_parameter_values, prefix, name, full_name, val);
    }
    // Array dimensions from shape
    if (!preserve_existing || !ctx.array_dimensions.contains_key(full_name))
        && !comp.shape.is_empty()
    {
        let dims: Vec<i64> = comp.shape.iter().map(|&d| d as i64).collect();
        insert_with_prefix(&mut ctx.array_dimensions, prefix, name, full_name, dims);
    }
    // Array dimensions from binding (array literal length)
    if (!preserve_existing || !ctx.array_dimensions.contains_key(full_name))
        && let Some(dims) = infer_dims_from_expr(expr, ctx, prefix)
    {
        insert_with_prefix(&mut ctx.array_dimensions, prefix, name, full_name, dims);
    }
}

/// Check if a dotted variable name passes through an expanded array component.
///
/// Returns true if any parent segment (NOT the last segment) contains embedded
/// array subscripts. For example:
/// - `l1sigma.inductor[1].L` → true (parent `inductor[1]` has subscripts)
/// - `plug_p.pin[1]` → false (only the last segment has subscripts)
/// - `l1sigma.inductor[1].v[2]` → true (parent has subscripts)
fn has_embedded_array_subscript_in_parent(name: &str) -> bool {
    let segments = crate::path_utils::parse_path_with_indices(name);
    // Check all segments EXCEPT the last one
    segments[..segments.len().saturating_sub(1)]
        .iter()
        .any(|seg| seg.contains('['))
}

#[cfg(test)]
mod parent_subscript_tests {
    use super::has_embedded_array_subscript_in_parent;

    #[test]
    fn ignores_dot_inside_top_level_subscript() {
        assert!(!has_embedded_array_subscript_in_parent("bus[data.medium]"));
    }

    #[test]
    fn detects_actual_parent_subscript_only() {
        assert!(has_embedded_array_subscript_in_parent(
            "stack.cell[data.medium].x"
        ));
        assert!(!has_embedded_array_subscript_in_parent(
            "stack.cell.x[data.medium]"
        ));
    }
}

/// Build a prefixed name: `prefix.name` or just `name` if prefix is empty.
fn make_prefixed_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{}.{}", prefix, name)
    }
}

fn synthesize_component_modification_binding(comp: &ast::Component) -> Option<ast::Expression> {
    if comp.modifications.is_empty() {
        return None;
    }
    let target = ast::ComponentReference {
        local: false,
        parts: comp
            .type_name
            .to_string()
            .split('.')
            .map(|part| ast::ComponentRefPart {
                ident: rumoca_ir_core::Token {
                    text: std::sync::Arc::from(part),
                    ..Default::default()
                },
                subs: None,
            })
            .collect(),
        def_id: comp.type_name.def_id,
    };
    let modifications = comp
        .modifications
        .iter()
        .map(|(field, value)| ast::Expression::NamedArgument {
            name: rumoca_ir_core::Token {
                text: std::sync::Arc::from(field.as_str()),
                ..Default::default()
            },
            value: std::sync::Arc::new(value.clone()),
        })
        .collect();
    Some(ast::Expression::ClassModification {
        target,
        modifications,
    })
}

/// Insert a value under both the full_name and the unprefixed name.
fn insert_with_prefix<V: Clone>(
    map: &mut rustc_hash::FxHashMap<String, V>,
    prefix: &str,
    name: &str,
    full_name: &str,
    value: V,
) {
    map.insert(full_name.to_string(), value.clone());
    let expose_unprefixed = !prefix.is_empty()
        && !crate::path_utils::has_top_level_dot(prefix)
        && prefix.chars().next().is_some_and(char::is_uppercase);
    if expose_unprefixed {
        map.entry(name.to_string()).or_insert(value);
    }
}

#[cfg(test)]
mod insert_with_prefix_tests {
    use rustc_hash::FxHashMap;

    use super::insert_with_prefix;

    #[test]
    fn exposes_unprefixed_for_single_prefix_with_dot_inside_subscript_expression() {
        let mut map = FxHashMap::default();
        insert_with_prefix(
            &mut map,
            "Alias[data.medium]",
            "nX",
            "Alias[data.medium].nX",
            7_i64,
        );
        assert_eq!(map.get("nX"), Some(&7));
    }

    #[test]
    fn does_not_expose_unprefixed_for_true_dotted_prefix() {
        let mut map = FxHashMap::default();
        insert_with_prefix(
            &mut map,
            "Outer.Alias[data.medium]",
            "nX",
            "Outer.Alias[data.medium].nX",
            7_i64,
        );
        assert_eq!(map.get("nX"), None);
    }
}

#[cfg(test)]
mod nested_class_constant_scope_tests {
    use super::*;
    use std::sync::Arc;

    fn token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: Arc::from(text.to_string()),
            ..rumoca_ir_core::Token::default()
        }
    }

    fn unsigned_integer(text: &str) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: token(text),
        }
    }

    #[test]
    fn extract_nested_class_constants_skips_non_package_nested_classes() {
        let tree = ast::ClassTree::new();
        let mut ctx = Context::new();

        let mut outer = ast::ClassDef {
            name: token("Outer"),
            class_type: ast::ClassType::Model,
            ..Default::default()
        };

        let mut package_alias = ast::ClassDef {
            name: token("PkgAlias"),
            class_type: ast::ClassType::Package,
            ..Default::default()
        };
        package_alias.components.insert(
            "nX".to_string(),
            ast::Component {
                name: "nX".to_string(),
                type_name: ast::Name::from_string("Integer"),
                variability: rumoca_ir_core::Variability::Parameter(
                    rumoca_ir_core::Token::default(),
                ),
                binding: Some(unsigned_integer("2")),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        outer.classes.insert("PkgAlias".to_string(), package_alias);

        let mut non_package_class = ast::ClassDef {
            name: token("CCCV_Cell"),
            class_type: ast::ClassType::Model,
            ..Default::default()
        };
        let mut leaked_component = ast::Component {
            name: "cellData".to_string(),
            type_name: ast::Name::from_string(
                "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
            ),
            variability: rumoca_ir_core::Variability::Parameter(rumoca_ir_core::Token::default()),
            ..Default::default()
        };
        leaked_component
            .modifications
            .insert("Qnom".to_string(), unsigned_integer("18000"));
        non_package_class
            .components
            .insert("cellData".to_string(), leaked_component);
        outer
            .classes
            .insert("CCCV_Cell".to_string(), non_package_class);

        extract_nested_class_constants(&tree, &outer, "Outer", &mut ctx);

        assert_eq!(ctx.parameter_values.get("PkgAlias.nX"), Some(&2));
        assert_eq!(ctx.parameter_values.get("nX"), Some(&2));
        assert!(
            !ctx.constant_values.keys().any(|key| {
                key == "cellData"
                    || key == "CCCV_Cell.cellData"
                    || key.starts_with("CCCV_Cell.cellData.")
            }),
            "non-package nested classes must not inject cellData constants: {:?}",
            ctx.constant_values.keys().collect::<Vec<_>>()
        );
    }
}

fn inject_enclosing_class_constants(tree: &ast::ClassTree, model_name: &str, ctx: &mut Context) {
    let Some(enclosing_name) = crate::path_utils::parent_scope(model_name) else {
        return;
    };
    let ancestors = collect_ancestor_classes(tree, enclosing_name);
    if ancestors.is_empty() {
        return;
    }
    for ancestor in &ancestors {
        extract_nested_class_constants(tree, ancestor, enclosing_name, ctx);
    }
    extract_ancestor_constants_multi_pass(tree, enclosing_name, &ancestors, ctx);
}
