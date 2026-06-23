//! Flatten phase for the Rumoca compiler.
//!
//! This crate implements the flattening pass that converts an ast::InstancedTree to a flat::Model.
//! It produces a flat equation system with globally unique variable names (MLS §5.6).
//!
//! # Overview
//!
//! Flattening is responsible for:
//! - Converting hierarchical instance tree to flat variables
//! - Generating globally unique variable names from resolved instance paths
//! - Expanding connection equations per MLS §9
//! - Converting equations to residual form (0 = residual)
//! - Preserving algorithm sections per SPEC_0020
//!
//! # Tracing (SPEC_0024)
//!
//! Enable the `tracing` feature for detailed diagnostic output:
//! ```bash
//! cargo run -p rumoca --features tracing -- check model.mo --trace-filter rumoca_phase_flatten=debug
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
mod alias_paths;
mod array_comprehension;
mod ast_lower;
mod boolean_eval;
mod connections;
mod connections_builtin;
mod constant_extraction;
#[cfg(test)]
mod context_suffix_tests;
mod enum_literals;
mod equations;
mod errors;
mod function_lowering;
mod function_precollect;
mod functions;
mod name_simplify;
mod outer_refs;
mod path_utils;
mod pipeline;
mod postprocess;
pub mod qualify;
pub(crate) mod record_constant_arrays;
mod static_subscripts;
mod variables;
mod vcg;
mod when_equations;

use std::collections::HashSet;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use indexmap::IndexMap;
use rumoca_core::{ExpressionVisitor, Reference, Span};
use rumoca_core::{OptionalTimer, maybe_elapsed_duration, maybe_start_timer};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use constant_extraction::*;

pub use errors::{FlattenError, FlattenResult};

pub fn ast_expression_to_string(expr: &ast::Expression) -> Option<String> {
    rumoca_eval_ast::eval_instantiate::expr_to_string(expr)
}

type ClassDef = ast::ClassDef;
type ClassInstanceData = ast::ClassInstanceData;
type ClassTree = ast::ClassTree;
type InstanceOverlay = ast::InstanceOverlay;
type InstanceStatement = ast::InstanceStatement;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type QualifiedName = ast::QualifiedName;
type Algorithm = flat::Algorithm;
type Expression = rumoca_core::Expression;
type Function = rumoca_core::Function;
type Literal = rumoca_core::Literal;
type Model = flat::Model;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;

pub(crate) type ResolveDefMap = rumoca_ir_ast::AstIndexMap<rumoca_core::DefId, String>;

pub(crate) use function_precollect::{
    compute_cardinality_counts, extract_simple_path, pre_collect_functions,
};
use pipeline::*;
use postprocess::*;
use record_constant_arrays::{
    is_record_like_type, synthesize_component_modification_binding,
    try_extract_record_array_constructor_constant,
};
use rumoca_eval_flat::phase_constant::{
    ParamEvalContext, build_eval_context, eval_user_func_real, infer_array_dimensions,
    infer_array_dimensions_full_with_functions, looks_like_enum_literal_path,
    try_eval_flat_expr_boolean_with_context, try_eval_flat_expr_real,
    try_eval_integer_with_context, try_infer_better_dims,
};

/// Options controlling flatten strictness.
#[derive(Debug, Clone, Copy)]
pub struct FlattenOptions {
    /// Whether to enforce connection type/dimension validation.
    pub strict_connection_validation: bool,
    /// Whether to shorten flat variable names after flattening.
    ///
    /// MLS §5.6 requires globally unique names but does not require shortening.
    /// The default keeps source-like instance paths for diagnostics, simulation
    /// results, and stable public compiler output.
    pub simplify_variable_names: bool,
}

impl Default for FlattenOptions {
    fn default() -> Self {
        Self {
            strict_connection_validation: true,
            simplify_variable_names: false,
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
pub(crate) fn maybe_record_connections_timing(start: OptionalTimer) {
    if let Some(elapsed) = maybe_elapsed_duration(start) {
        record_connections_timing(elapsed);
    }
}

#[inline]
pub(crate) fn maybe_record_eval_fallback_timing(start: OptionalTimer) {
    if let Some(elapsed) = maybe_elapsed_duration(start) {
        record_eval_fallback_timing(elapsed);
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
    let connections = timing_stat(&CONNECTIONS_TOTAL_NANOS, &CONNECTIONS_CALLS);
    let eval_fallback = timing_stat(&EVAL_FALLBACK_TOTAL_NANOS, &EVAL_FALLBACK_CALLS);
    FlattenPhaseTimingSnapshot {
        connections,
        eval_fallback,
    }
}

/// Check if a qualified name belongs to a disabled conditional component (MLS §4.8).
fn is_in_disabled_component(
    qn: &ast::QualifiedName,
    disabled_components: &indexmap::IndexSet<rumoca_core::ComponentPath>,
) -> bool {
    for disabled in disabled_components {
        if qn.starts_with_component_path(disabled) {
            return true;
        }
    }
    false
}

/// Choose the better (more complete) of two optional dimension vectors.
fn best_dims(a: Option<&Vec<i64>>, b: Option<&Vec<i64>>) -> Option<Vec<i64>> {
    match (a, b) {
        (Some(av), Some(bv)) => Some(if dims_are_better(bv, av) {
            bv.clone()
        } else {
            av.clone()
        }),
        (Some(av), None) => Some(av.clone()),
        (None, Some(bv)) => Some(bv.clone()),
        (None, None) => None,
    }
}

fn dims_are_better(candidate: &[i64], existing: &[i64]) -> bool {
    if candidate.len() > existing.len() {
        return true;
    }
    if candidate.len() != existing.len() {
        return false;
    }

    let candidate_specificity = dimension_specificity(candidate);
    let existing_specificity = dimension_specificity(existing);
    if candidate_specificity != existing_specificity {
        return candidate_specificity > existing_specificity;
    }

    concrete_dim_product(candidate).is_some_and(|candidate_size| {
        concrete_dim_product(existing).is_some_and(|existing_size| candidate_size > existing_size)
    })
}

fn dimension_specificity(dims: &[i64]) -> usize {
    dims.iter().filter(|dim| **dim > 0).count()
}

fn concrete_dim_product(dims: &[i64]) -> Option<i64> {
    dims.iter().try_fold(1i64, |acc, dim| {
        (*dim > 0).then(|| acc.saturating_mul(*dim))
    })
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
    let class_index = ast::ClassDefIndex::from_tree(tree);
    ctx.class_def_ids = std::sync::Arc::new(class_index.def_ids().collect());
    ctx.target_def_names = tree
        .def_map
        .iter()
        .map(|(id, name)| (*id, name.clone()))
        .collect();
    let mut flat = flat::Model::new();
    let component_override_map =
        build_component_override_map(overlay, tree, &class_index, model_name)?;

    initialize_flat_metadata(&mut flat, overlay);
    process_component_instances_for_flatten(
        &mut flat,
        overlay,
        &component_override_map,
        tree,
        &class_index,
    )?;
    let flatten_graph = prepare_context_for_equation_flattening(
        &mut ctx,
        &mut flat,
        overlay,
        tree,
        &class_index,
        model_name,
        &component_override_map,
    )?;
    process_class_instances_for_flatten(
        &mut ctx,
        &mut flat,
        overlay,
        &component_override_map,
        tree,
        &class_index,
    )?;
    finalize_flat_model(FinalizeFlatModelInput {
        ctx: &mut ctx,
        flat: &mut flat,
        overlay,
        tree,
        class_index: &class_index,
        model_name,
        options,
        flatten_graph: &flatten_graph,
        component_override_map: &component_override_map,
    })?;

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
        let short_name = crate::path_utils::top_level_last_segment(qualified_name);
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
fn extract_record_aliases(
    ctx: &mut Context,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
) -> Result<(), FlattenError> {
    for (_def_id, instance_data) in &overlay.components {
        // Record-alias canonicalization is only valid for non-primitive component
        // containers (records/connectors/classes). Primitive scalar bindings like
        // `output Real y = x` are value bindings, not prefix aliases.
        if instance_data.is_primitive {
            continue;
        }
        // Check if this component has a binding that's a simple path
        let Some(binding) = &instance_data.binding else {
            continue;
        };

        // Extract the path from the binding (supports both ComponentReference and FieldAccess)
        let Some(alias_target_unqualified) = extract_simple_path(binding) else {
            continue;
        };

        let alias_source = instance_data.qualified_name.to_component_path();
        let alias_target_unqualified =
            rumoca_core::ComponentPath::from_flat_path(&alias_target_unqualified);

        // Qualify the alias target based on whether it's from a modification binding.
        // For modification bindings, the target is relative to the grandparent scope
        // (where the modification was written). For declaration bindings, it's relative
        // to the parent scope (sibling variables).
        let alias_target = if instance_data.binding_from_modification {
            // Modification bindings: qualify from modifier lexical scope.
            let source_scope = variables::modification_binding_prefix_pub(instance_data, tree)?;
            if source_scope.is_empty() {
                alias_target_unqualified
            } else {
                source_scope
                    .to_component_path()
                    .join(&alias_target_unqualified)
            }
        } else {
            // Declaration bindings: qualify with parent prefix (sibling scope)
            let parent = variables::parent_prefix_pub(&instance_data.qualified_name);
            if parent.is_empty() {
                alias_target_unqualified
            } else {
                parent.to_component_path().join(&alias_target_unqualified)
            }
        };

        // Add the alias if it's not self-referential
        if alias_source != alias_target {
            ctx.record_aliases.insert(alias_source, alias_target);
        }
    }
    ctx.alias_resolution_cache.borrow_mut().clear();

    // Compute transitive closure of aliases (MLS §7.2.3)
    // If A -> B and B -> C, we should have A -> C for efficient resolution.
    compute_transitive_alias_closure(&mut ctx.record_aliases);
    ctx.alias_resolution_cache.borrow_mut().clear();
    Ok(())
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
fn compute_transitive_alias_closure(
    aliases: &mut rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
) {
    synthesize_intermediate_aliases(aliases);
    compute_closure_iterations(aliases, 20);
}

/// Find parent path alias for a prefix (e.g., "stack.cell.stackData" -> "stack.stackData").
fn find_parent_alias(
    prefix: &rumoca_core::ComponentPath,
    aliases: &rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
) -> Option<rumoca_core::ComponentPath> {
    let prefix_parts = prefix.parts();
    if prefix_parts.len() < 3 {
        return None;
    }
    let component_name = prefix_parts.last()?;
    for skip in 1..prefix_parts.len() - 1 {
        let parent = prefix_parts
            .iter()
            .take(prefix_parts.len() - 1 - skip)
            .chain(std::iter::once(component_name))
            .cloned();
        let parent_path = rumoca_core::ComponentPath::from_parts(parent);
        if aliases.contains_key(&parent_path) {
            return Some(parent_path);
        }
    }
    None
}

/// Try to find a synthetic alias for a single prefix.
fn find_synthetic_alias(
    prefix: &rumoca_core::ComponentPath,
    aliases: &rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
) -> Option<rumoca_core::ComponentPath> {
    find_parent_alias(prefix, aliases)
}

/// Synthesize missing intermediate aliases for unresolvable prefixes.
fn synthesize_intermediate_aliases(
    aliases: &mut rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
) {
    let mut synthetic: Vec<(rumoca_core::ComponentPath, rumoca_core::ComponentPath)> = Vec::new();
    for target in aliases.values() {
        for i in (2..target.len()).rev() {
            let prefix = target.prefix(i).expect("prefix index is in range");
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

/// Resolve an alias target through the chain of aliases.
fn resolve_alias_chain(
    target: &rumoca_core::ComponentPath,
    source: &rumoca_core::ComponentPath,
    aliases: &rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
) -> Option<rumoca_core::ComponentPath> {
    let mut current = target.clone();
    for _ in 0..10 {
        if let Some(resolved) =
            alias_paths::resolve_component_alias_once(&current, Some(source), aliases)
        {
            current = resolved;
            continue;
        }
        break;
    }
    (&current != target && &current != source).then_some(current)
}

/// Compute transitive closure through iterative resolution.
fn compute_closure_iterations(
    aliases: &mut rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
    max_iter: usize,
) {
    for _ in 0..max_iter {
        let updates: Vec<(rumoca_core::ComponentPath, rumoca_core::ComponentPath)> = aliases
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
/// Build a prefixed name: `prefix.name` or just `name` if prefix is empty.
fn make_prefixed_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{}.{}", prefix, name)
    }
}

/// Insert a value under both the full_name and the unprefixed name.
fn insert_with_prefix<V: Clone>(
    map: &mut rustc_hash::FxHashMap<String, V>,
    prefix: &str,
    name: &str,
    full_name: &str,
    value: V,
) {
    insert_with_prefix_exposure(
        map,
        exposes_unprefixed_prefix(prefix),
        name,
        full_name,
        value,
    );
}

fn exposes_unprefixed_prefix(prefix: &str) -> bool {
    !prefix.is_empty()
        && !crate::path_utils::has_top_level_dot(prefix)
        && prefix.chars().next().is_some_and(char::is_uppercase)
}

fn insert_with_prefix_exposure<V: Clone>(
    map: &mut rustc_hash::FxHashMap<String, V>,
    expose_unprefixed: bool,
    name: &str,
    full_name: &str,
    value: V,
) {
    map.insert(full_name.to_string(), value.clone());
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

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text.to_string()),
            ..rumoca_core::Token::default()
        }
    }

    fn unsigned_integer(text: &str) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: token(text),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn component_ref_expr(path: &[&str], def_id: rumoca_core::DefId) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: path
                .iter()
                .map(|part| ast::ComponentRefPart {
                    ident: token(part),
                    subs: None,
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
            def_id: Some(def_id),
        })
    }

    #[test]
    fn component_dimension_refs_collect_package_constant_scopes_by_def_id() {
        let nstate_def = rumoca_core::DefId::new(42);
        let mut def_map = crate::ResolveDefMap::default();
        def_map.insert(
            nstate_def,
            "Modelica.Math.Random.Generators.Xorshift128plus.nState".to_string(),
        );
        let mut overlay = ast::InstanceOverlay::default();
        overlay.components.insert(
            ast::InstanceId::new(1),
            ast::InstanceData {
                dims_expr: vec![ast::Subscript::Expression(component_ref_expr(
                    &["generator", "nState"],
                    nstate_def,
                ))],
                ..Default::default()
            },
        );
        let mut scopes = HashSet::new();
        collect_dimension_referenced_class_scopes(&overlay, &HashSet::new(), &def_map, &mut scopes)
            .expect("dimension references should lower for class-scope collection");

        assert!(scopes.contains("Modelica.Math.Random.Generators.Xorshift128plus"));
    }

    #[test]
    fn extract_nested_class_constants_skips_non_package_nested_classes() {
        let tree = ast::ClassTree::new();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let mut ctx = Context::new();

        let mut outer = ast::ClassDef {
            name: token("Outer"),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };

        let mut package_alias = ast::ClassDef {
            name: token("PkgAlias"),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };
        package_alias.components.insert(
            "nX".to_string(),
            ast::Component {
                name: "nX".to_string(),
                type_name: ast::Name::from_string("Integer"),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(unsigned_integer("2")),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        outer.classes.insert("PkgAlias".to_string(), package_alias);

        let mut non_package_class = ast::ClassDef {
            name: token("CCCV_Cell"),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };
        let mut leaked_component = ast::Component {
            name: "cellData".to_string(),
            type_name: ast::Name::from_string(
                "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
            ),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
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

        extract_nested_class_constants(&tree, &class_index, &outer, "Outer", &mut ctx);

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

    #[test]
    fn extract_nested_class_constants_includes_nested_records_with_qualified_prefix() {
        let tree = ast::ClassTree::new();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let mut ctx = Context::new();

        let mut outer = ast::ClassDef {
            name: token("BaseIF97"),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };

        let mut data = ast::ClassDef {
            name: token("data"),
            class_type: rumoca_core::ClassType::Record,
            ..Default::default()
        };
        data.components.insert(
            "n".to_string(),
            ast::Component {
                name: "n".to_string(),
                type_name: ast::Name::from_string("Real"),
                variability: rumoca_core::Variability::Constant(rumoca_core::Token::default()),
                binding: Some(ast::Expression::Array {
                    elements: vec![
                        unsigned_integer("1"),
                        unsigned_integer("2"),
                        unsigned_integer("3"),
                        unsigned_integer("4"),
                        unsigned_integer("5"),
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        outer.classes.insert("data".to_string(), data);

        extract_nested_class_constants_with_prefix(
            &tree,
            &class_index,
            &outer,
            "Modelica.Media.Water.IF97_Utilities.BaseIF97",
            "Modelica.Media.Water.IF97_Utilities.BaseIF97",
            &mut ctx,
        );

        assert!(
            ctx.constant_values
                .contains_key("Modelica.Media.Water.IF97_Utilities.BaseIF97.data.n")
        );
        assert!(ctx.constant_values.contains_key("data.n"));
    }

    #[test]
    fn extract_referenced_nested_class_constants_includes_nested_models_with_qualified_prefix() {
        let tree = ast::ClassTree::new();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let mut ctx = Context::new();

        let mut outer = ast::ClassDef {
            name: token("Glycol47"),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };

        let mut base_properties = ast::ClassDef {
            name: token("BaseProperties"),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };
        base_properties.components.insert(
            "T_start".to_string(),
            ast::Component {
                name: "T_start".to_string(),
                type_name: ast::Name::from_string("Real"),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(unsigned_integer("298")),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        outer
            .classes
            .insert("BaseProperties".to_string(), base_properties);
        let mut internal = ast::ClassDef {
            name: token("Internal"),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };
        internal.components.insert(
            "leaked".to_string(),
            ast::Component {
                name: "leaked".to_string(),
                type_name: ast::Name::from_string("Real"),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(unsigned_integer("1")),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        outer.classes.insert("Internal".to_string(), internal);

        extract_referenced_nested_class_constants_with_prefix(
            &tree,
            &class_index,
            &outer,
            "Modelica.Media.Incompressible.Examples.Glycol47",
            "Modelica.Media.Incompressible.Examples.Glycol47",
            &HashSet::from([
                ("Modelica.Media.Incompressible.Examples.Glycol47.BaseProperties".to_string()),
            ]),
            &mut ctx,
        );

        assert!(ctx.constant_values.contains_key(
            "Modelica.Media.Incompressible.Examples.Glycol47.BaseProperties.T_start"
        ));
        assert!(
            !ctx.constant_values.contains_key("BaseProperties.T_start"),
            "referenced nested classes should not leak bare names into the global context"
        );
        assert!(
            !ctx.constant_values
                .contains_key("Modelica.Media.Incompressible.Examples.Glycol47.Internal.leaked"),
            "unreferenced nested classes should not be walked from referenced package scopes"
        );
    }

    #[test]
    fn extract_referenced_nested_class_constants_uses_inherited_nested_models() {
        let table_based_def = rumoca_core::DefId::new(1);
        let mut table_based = ast::ClassDef {
            name: token("TableBased"),
            class_type: rumoca_core::ClassType::Package,
            def_id: Some(table_based_def),
            ..Default::default()
        };
        let mut base_properties = ast::ClassDef {
            name: token("BaseProperties"),
            class_type: rumoca_core::ClassType::Model,
            ..Default::default()
        };
        base_properties.components.insert(
            "T_start".to_string(),
            ast::Component {
                name: "T_start".to_string(),
                type_name: ast::Name::from_string("Real"),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(unsigned_integer("298")),
                has_explicit_binding: true,
                ..Default::default()
            },
        );
        table_based
            .classes
            .insert("BaseProperties".to_string(), base_properties);

        let mut glycol = ast::ClassDef {
            name: token("Glycol47"),
            class_type: rumoca_core::ClassType::Package,
            ..Default::default()
        };
        glycol.extends.push(ast::Extend {
            base_name: ast::Name::from_string("TableBased"),
            base_def_id: Some(table_based_def),
            location: rumoca_core::Location::default(),
            modifications: vec![],
            break_names: vec![],
            is_protected: false,
            annotation: vec![],
        });

        let mut tree = ast::ClassTree::new();
        tree.definitions
            .classes
            .insert("TableBased".to_string(), table_based);
        tree.def_map
            .insert(table_based_def, "TableBased".to_string());
        tree.name_map
            .insert("TableBased".to_string(), table_based_def);
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let mut ctx = Context::new();

        extract_referenced_nested_class_constants_with_prefix(
            &tree,
            &class_index,
            &glycol,
            "Modelica.Media.Incompressible.Examples.Glycol47",
            "Modelica.Media.Incompressible.Examples.Glycol47",
            &HashSet::from([
                ("Modelica.Media.Incompressible.Examples.Glycol47.BaseProperties".to_string()),
            ]),
            &mut ctx,
        );

        assert!(ctx.constant_values.contains_key(
            "Modelica.Media.Incompressible.Examples.Glycol47.BaseProperties.T_start"
        ));
    }
}

fn inject_enclosing_class_constants(tree: &ast::ClassTree, model_name: &str, ctx: &mut Context) {
    let Some(enclosing_name) = crate::path_utils::parent_scope(model_name) else {
        return;
    };
    let class_index = ast::ClassDefIndex::from_tree(tree);
    let ancestors = collect_ancestor_classes_with_index(tree, &class_index, enclosing_name);
    if ancestors.is_empty() {
        return;
    }
    for ancestor in &ancestors {
        extract_nested_class_constants(tree, &class_index, ancestor, enclosing_name, ctx);
    }
    extract_ancestor_constants_multi_pass(tree, &class_index, enclosing_name, &ancestors, ctx);
}
