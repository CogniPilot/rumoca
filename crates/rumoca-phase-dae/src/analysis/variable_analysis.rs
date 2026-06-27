//! Variable analysis helpers for the ToDae phase.
//!
//! This module contains functions for classifying variables, analyzing
//! connections, validating function calls, and other variable-level analysis
//! needed during the DAE conversion.

use super::{definition_analysis, discrete_partition};
use crate::errors::ToDaeError;
use crate::name_resolution;
use crate::overconstrained_interface;
use crate::path_utils::{
    get_top_level_prefix, is_nested_name, normalized_top_level_names, path_is_in_top_level_set,
    subscript_fallback_chain,
};
use crate::scalar_inference::{collect_var_refs, compute_embedded_range_size};
use crate::when_analysis;

use flat::FallibleStatementVisitor;
use indexmap::IndexSet;
use rumoca_core::{FallibleExpressionVisitor, Span};
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;
use std::collections::{HashMap, HashSet};

type BuiltinFunction = rumoca_core::BuiltinFunction;
type ComponentReference = rumoca_core::ComponentReference;
type EquationOrigin = flat::EquationOrigin;
type Expression = rumoca_core::Expression;
type Function = rumoca_core::Function;
type Model = flat::Model;
type Statement = rumoca_core::Statement;
type VarName = rumoca_core::VarName;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ReachableCall {
    name: VarName,
    span: Span,
}

fn record_reachable_call(reachable_calls: &mut IndexSet<ReachableCall>, name: VarName, span: Span) {
    reachable_calls.insert(ReachableCall { name, span });
}

/// Filter der() variables to only include those that will be classified as states.
///
/// External interface inputs are not states (`der(u)` differentiates an incoming
/// signal), but internal sub-component inputs can become states when they are
/// defined by local equations (e.g. `der(medium.p) = 0` in MSL media examples).
pub(crate) fn filter_state_variables(
    der_vars: IndexSet<VarName>,
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> IndexSet<VarName> {
    let der_vars = filter_overconstrained_alias_states(der_vars, flat);

    der_vars
        .into_iter()
        .filter(|name| {
            flat.variables.get(name).is_none_or(|v| {
                !matches!(&v.causality, rumoca_core::Causality::Input(_))
                    || internal_inputs.contains(name)
            })
        })
        .collect()
}

pub(crate) fn find_overconstrained_derivative_alias_roots(
    der_vars: &IndexSet<VarName>,
    flat: &Model,
) -> FxHashMap<VarName, VarName> {
    let state_record_paths = overconstrained_state_record_paths(der_vars, flat);
    if state_record_paths.len() < 2 {
        return FxHashMap::default();
    }

    let component_of = overconstrained_record_components(flat);
    let component_roots = overconstrained_state_roots(flat, &component_of);
    let mut aliases = FxHashMap::default();

    for (name, record_path) in state_record_paths {
        if top_level_component_has_outputs(&name, flat) {
            continue;
        }
        let Some(component_id) = component_of.get(record_path.as_str()) else {
            continue;
        };
        let Some(root_path) = component_roots.get(component_id) else {
            continue;
        };
        if oc_paths_match(&record_path, root_path) {
            continue;
        }
        let Some(root_name) = root_variable_for_alias(&name, &record_path, root_path, flat) else {
            continue;
        };
        aliases.insert(name, root_name);
    }

    aliases
}

fn top_level_component_has_outputs(name: &VarName, flat: &Model) -> bool {
    let Some(component) = get_top_level_prefix(name.as_str()) else {
        return false;
    };
    let prefix = format!("{component}.");
    flat.variables.iter().any(|(candidate, variable)| {
        candidate.as_str().starts_with(&prefix)
            && matches!(variable.causality, rumoca_core::Causality::Output(_))
    })
}

fn filter_overconstrained_alias_states(
    der_vars: IndexSet<VarName>,
    flat: &Model,
) -> IndexSet<VarName> {
    let alias_roots = find_overconstrained_derivative_alias_roots(&der_vars, flat);
    if alias_roots.is_empty() {
        return der_vars;
    }

    der_vars
        .into_iter()
        .filter(|name| !alias_roots.contains_key(name))
        .collect()
}

fn overconstrained_state_record_paths(
    der_vars: &IndexSet<VarName>,
    flat: &Model,
) -> FxHashMap<VarName, String> {
    let state_record_paths: FxHashMap<VarName, String> = der_vars
        .iter()
        .filter_map(|name| {
            let var = flat.variables.get(name)?;
            if !var.is_overconstrained {
                return None;
            }
            Some((name.clone(), var.oc_record_path.clone()?))
        })
        .collect();

    state_record_paths
}

fn overconstrained_record_components(flat: &Model) -> FxHashMap<&str, usize> {
    let mut record_paths: IndexSet<&str> = IndexSet::new();
    for var in flat.variables.values() {
        if !var.is_overconstrained {
            continue;
        }
        if let Some(path) = &var.oc_record_path {
            record_paths.insert(path.as_str());
        }
    }
    let record_paths: Vec<&str> = record_paths.into_iter().collect();
    let (component_of, _n_components) = overconstrained_interface::build_record_components(
        &record_paths,
        &flat.branches,
        &flat.optional_edges,
    );
    component_of
}

fn root_variable_for_alias(
    alias_name: &VarName,
    alias_record_path: &str,
    root_record_path: &str,
    flat: &Model,
) -> Option<VarName> {
    let suffix = alias_name.as_str().strip_prefix(alias_record_path)?;
    if !suffix.starts_with('.') {
        return None;
    }
    let root_name = VarName::new(format!("{root_record_path}{suffix}"));
    flat.variables.contains_key(&root_name).then_some(root_name)
}

fn overconstrained_state_roots(
    flat: &Model,
    component_of: &FxHashMap<&str, usize>,
) -> HashMap<usize, String> {
    let mut roots = HashMap::default();

    for root in &flat.definite_roots {
        for (&record_path, &component_id) in component_of {
            if oc_paths_match(record_path, root) {
                roots.entry(component_id).or_insert_with(|| root.clone());
            }
        }
    }

    let mut potential_roots = flat.potential_roots.clone();
    potential_roots.sort_by(|(left_path, left_priority), (right_path, right_priority)| {
        left_priority
            .cmp(right_priority)
            .then_with(|| left_path.cmp(right_path))
    });
    for (root, _priority) in potential_roots {
        for (&record_path, &component_id) in component_of {
            if !roots.contains_key(&component_id) && oc_paths_match(record_path, &root) {
                roots.insert(component_id, root.clone());
            }
        }
    }

    roots
}

fn oc_paths_match(path: &str, root: &str) -> bool {
    path == root
        || path
            .strip_prefix(root)
            .is_some_and(|suffix| suffix.starts_with('.'))
        || root
            .strip_prefix(path)
            .is_some_and(|suffix| suffix.starts_with('.'))
}

/// Collect all variables defined by continuous equations (LHS of equations).
/// This helps identify variables that need continuous equations vs those only
/// assigned in when-clauses.
///
/// Important: Connection equations are excluded because they express equality
/// constraints, not definitions. A connection `a = b` doesn't define either
/// variable - it just constrains them to be equal.
/// Collect variables defined by explicit equations in the flat model.
///
/// Returns two sets:
/// - `directly_defined`: Variables that are the LHS of a non-connection, non-flow equation.
///   These have an explicit defining equation, so their bindings should always be skipped.
/// - `all_defined`: Includes directly_defined PLUS variables from flow-sum equations.
///   Flow-sum variables are constrained (not uniquely defined), so bindings should only
///   be skipped if they don't reference other unknowns.
pub(crate) fn collect_continuous_equation_lhs(
    flat: &Model,
) -> (HashSet<VarName>, HashSet<VarName>) {
    let mut directly_defined = HashSet::default();
    let mut all_defined = HashSet::default();

    for eq in &flat.equations {
        // Skip connection equations - they don't define variables, they constrain equality
        if eq.origin.is_connection() {
            continue;
        }

        // Clocked assignment equations belong to the discrete partitions
        // (MLS Appendix B) and must not be treated as continuous definitions.
        if discrete_partition::is_clocked_assignment_equation(eq) {
            continue;
        }

        // MLS §9.2: Flow-sum and unconnected-flow equations define flow variables.
        // Adding them to the defined set allows should_skip_binding_for_explicit_var
        // to correctly skip trivial bindings while preserving physics bindings.
        if matches!(
            eq.origin,
            EquationOrigin::FlowSum { .. } | EquationOrigin::UnconnectedFlow { .. }
        ) {
            let refs = collect_var_refs(&eq.residual);
            all_defined.extend(refs);
            continue;
        }

        // Extract LHS from residual form: lhs - rhs = 0
        if let Some(lhs_name) = extract_equation_lhs(&eq.residual) {
            directly_defined.insert(lhs_name.clone());
            all_defined.insert(lhs_name);
        }
    }

    (directly_defined, all_defined)
}

/// Extract the LHS variable name from a residual expression.
/// Residual form is: lhs - rhs, so we extract the LHS from Binary::Sub.
fn extract_equation_lhs(residual: &Expression) -> Option<VarName> {
    let Expression::Binary { op, lhs, .. } = residual else {
        return None;
    };

    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }

    // LHS could be a VarRef with or without subscripts
    match lhs.as_ref() {
        Expression::VarRef { name, .. } => Some(name.var_name().clone()),
        _ => None,
    }
}

fn connection_alias_pair(eq: &flat::Equation) -> Option<(VarName, VarName)> {
    if !eq.origin.is_connection() {
        return None;
    }
    let Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    let Expression::VarRef { name: lhs_name, .. } = lhs.as_ref() else {
        return None;
    };
    let Expression::VarRef { name: rhs_name, .. } = rhs.as_ref() else {
        return None;
    };
    Some((lhs_name.var_name().clone(), rhs_name.var_name().clone()))
}

fn propagate_discrete_aliases_over_connections(
    flat: &Model,
    discrete_names: &mut HashSet<VarName>,
) {
    let mut changed = true;
    while changed {
        changed = false;
        for eq in &flat.equations {
            let Some((lhs, rhs)) = connection_alias_pair(eq) else {
                continue;
            };
            if discrete_names.contains(&lhs) && discrete_names.insert(rhs.clone()) {
                changed = true;
            }
            if discrete_names.contains(&rhs) && discrete_names.insert(lhs.clone()) {
                changed = true;
            }
        }
    }
}

/// Identify variables that are only assigned in when-clauses (not in continuous equations).
/// These should be classified as discrete, not as outputs or algebraics.
pub(crate) fn find_when_only_vars(
    flat: &Model,
    prefix_children: &FxHashMap<String, Vec<VarName>>,
) -> HashSet<VarName> {
    let mut when_assigned = when_analysis::collect_when_assigned_vars(flat);

    // Clocked assignment equations in regular equation sections are also
    // discrete updates (MLS §16 / Appendix B), even if they are not encoded as
    // explicit when-clauses in the flat model.
    for equation in &flat.equations {
        if !discrete_partition::is_clocked_assignment_equation(equation) {
            continue;
        }
        for target in discrete_partition::residual_lhs_targets(&equation.residual) {
            when_assigned.insert(target);
        }
    }

    // Expand record-level when-assignments to include field children.
    // When a when-clause assigns to a record variable like `k[1]` (Complex type),
    // the flat model has field variables `k[1].re` and `k[1].im`, not `k[1]`.
    // We must mark these fields as when-assigned so they're classified as discrete.
    let expansions: Vec<VarName> = when_assigned
        .iter()
        .flat_map(|name| {
            prefix_children
                .get(name.as_str())
                .into_iter()
                .flatten()
                .cloned()
        })
        .collect();
    when_assigned.extend(expansions);
    propagate_discrete_aliases_over_connections(flat, &mut when_assigned);

    let (_directly_defined, all_defined) = collect_continuous_equation_lhs(flat);
    let continuous_algorithm_defined =
        definition_analysis::collect_continuous_algorithm_defined_vars(flat, prefix_children);

    // Variables that are in when-clauses but NOT defined by continuous equations
    // or continuous algorithm sections. A variable like `dataOut := nextstate`
    // assigned in a continuous algorithm is NOT discrete even if it also appears
    // in when-clauses (MLS §11.1).
    when_assigned
        .into_iter()
        .filter(|v| !all_defined.contains(v) && !continuous_algorithm_defined.contains(v))
        .collect()
}

/// Find variables that are "derivative aliases" - variables defined EXACTLY by
/// equations of the form `y = der(state)` where this is their ONLY definition.
///
/// For example, in `omega = der(gamma)`:
/// - `gamma` is the state (appears in der())
/// - `omega` is a derivative alias IF this equation is EXACTLY `omega = der(gamma)`
///   and `omega` has no other defining equations
///
/// Derivative aliases are implicitly defined by ODE equations and shouldn't count
/// as separate algebraic unknowns in the DAE balance. They're essentially named
/// derivatives: `omega` is just a name for `der(gamma)`.
/// Count interface flow variables per MLS §4.7.
///
/// Per MLS §4.7, flow variables in top-level public connectors count toward
/// the local equation size, not as unknowns. This is because they will receive
/// their defining equations from external connections when the component is used.
///
/// We count top-level flow/stream scalars regardless of whether a local
/// unconnected-flow closure equation exists. Effective contribution is clamped
/// later in `Dae::balance()` to only close a remaining deficit, which prevents
/// over-correction of already balanced models.
///
/// Interface connectors are identified using `flat.top_level_connectors`, which is
/// populated during flatten by checking if each top-level component's class type is
/// `connector`. This replaces the previous heuristic that checked for the absence of
/// `ComponentEquation` origins, which misclassified internal components.
pub(crate) fn count_interface_flows(flat: &Model) -> usize {
    // Normalize top-level connector names by stripping array indices so
    // connector arrays like `plugs_n[1]` match variable prefixes `plugs_n`.
    let normalized_top_level_connectors =
        normalized_top_level_names(flat.top_level_connectors.iter());

    // Count Connection-origin equations that involve stream variables.
    // These equations (e.g., h_outflow_1 = h_outflow_2) are already in f_x,
    // so each one reduces the number of "phantom" interface equations needed.
    // For N stream variables in a connection set, N-1 connection equations
    // are generated, leaving exactly 1 interface equation needed per set.
    let stream_connection_eq_count: usize = flat
        .equations
        .iter()
        .filter(|eq| {
            if let EquationOrigin::Connection { lhs, .. } = &eq.origin {
                // Check if the LHS variable is a stream variable
                flat.variables
                    .get(&VarName::from(lhs.as_str()))
                    .is_some_and(|v| v.stream)
            } else {
                false
            }
        })
        .map(|eq| eq.scalar_count)
        .sum();

    let mut count: usize = 0;
    for (name, var) in &flat.variables {
        // Count flow AND stream variables in top-level public connectors (MLS §4.7)
        if !(var.flow || var.stream) || !var.is_primitive {
            continue;
        }
        // Only count flows in actual top-level connector components
        if path_is_in_top_level_set(name.as_str(), &normalized_top_level_connectors) {
            // Interface contribution is scalar-based (MLS §4.7).
            let scalar_size = interface_scalar_size(&var.dims);
            if scalar_size > 0 {
                count += scalar_size;
            }
        }
    }

    // Subtract stream connection equations already in f_x to avoid double-counting.
    // The raw count includes all connected stream vars, but N-1 of them already
    // have connection equations. Only the remaining 1 per connection set needs
    // an interface equation.
    count.saturating_sub(stream_connection_eq_count)
}

fn interface_scalar_size(dims: &[i64]) -> usize {
    if dims.is_empty() {
        return 1;
    }
    if dims.iter().any(|&d| d <= 0) {
        return 0;
    }
    dims.iter()
        .fold(1usize, |acc, &d| acc.saturating_mul(d as usize))
}

pub(crate) fn count_overconstrained_interface(
    flat: &Model,
    state_vars: &IndexSet<VarName>,
) -> Result<i64, ToDaeError> {
    overconstrained_interface::count_overconstrained_interface(flat, state_vars)
}

/// Check if a variable is a continuous unknown (state or algebraic, not param/const).
pub(crate) fn is_continuous_unknown(
    flat: &Model,
    state_vars: &IndexSet<VarName>,
    name: &VarName,
) -> bool {
    state_vars.contains(name)
        || flat.variables.get(name).is_some_and(|v| {
            !is_external_constructor_handle(flat, name, v)
                && !matches!(
                    v.variability,
                    rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
                )
        })
}

/// True for Modelica ExternalObject-style resource handles.
///
/// Flattening represents an ExternalObject class call as a constructor
/// function carrying external-function metadata. Such a handle is not a numeric
/// DAE unknown; emitting its constructor binding into f_x would force the
/// continuous solver to call native allocation code.
pub(crate) fn is_external_constructor_handle(
    flat: &Model,
    _name: &VarName,
    var: &flat::Variable,
) -> bool {
    let Some(Expression::FunctionCall {
        name: constructor_name,
        is_constructor: true,
        ..
    }) = &var.binding
    else {
        return false;
    };

    flat.functions
        .get(&VarName::new(constructor_name.as_str()))
        .is_some_and(|function| function.is_constructor && function.external.is_some())
}

/// Check if a variable is an internal (non-interface) input that should be promoted.
///
/// Top-level PUBLIC inputs are external interfaces and should remain as inputs.
/// Sub-component inputs (with dots) and protected top-level inputs are internal
/// and should be promoted to algebraics when connected or equation-defined
/// (MLS §4.4.2.2).
pub(crate) fn is_internal_input(name: &VarName, flat: &Model) -> Result<bool, ToDaeError> {
    let var = match flat.variables.get(name) {
        Some(var) => var,
        None => return Ok(false),
    };
    is_internal_input_var(name, var, flat)
}

fn is_internal_input_var(
    name: &VarName,
    var: &flat::Variable,
    flat: &Model,
) -> Result<bool, ToDaeError> {
    if !matches!(&var.causality, rumoca_core::Causality::Input(_)) {
        return Ok(false);
    }
    // Sub-component inputs (with dots) are internal, UNLESS the top-level
    // prefix is a top-level connector (interface inputs stay as inputs)
    // or the top-level parent is itself declared as input (e.g.,
    // `input Record state;` → `state.field` stays external).
    if is_nested_name(name.as_str()) {
        let Some(prefix) = get_top_level_prefix(name.as_str()) else {
            return Err(ToDaeError::runtime_contract_violation_at(
                format!("nested input variable `{name}` has no top-level prefix"),
                var.source_span,
            ));
        };
        if flat.top_level_connectors.contains(&prefix) {
            return Ok(false);
        }
        // If the parent component is a top-level input, its fields are
        // external interface values, not local unknowns.
        if is_top_level_input_component(&prefix, flat) {
            return Ok(false);
        }
        return Ok(true);
    }
    // Top-level protected inputs are internal implementation details (MLS §4.4.2.2)
    Ok(var.is_protected)
}

pub(crate) struct InternalInputIndex {
    inputs: HashSet<VarName>,
    by_component_base: FxHashMap<String, Vec<VarName>>,
}

impl InternalInputIndex {
    pub(crate) fn new(flat: &Model) -> Result<Self, ToDaeError> {
        let mut inputs = HashSet::default();
        let mut by_component_base: FxHashMap<String, Vec<VarName>> = FxHashMap::default();
        for (name, var) in &flat.variables {
            if !is_internal_input_var(name, var, flat)? {
                continue;
            }
            inputs.insert(name.clone());
            if let Some(base) = flat::component_base_name(name.as_str()) {
                by_component_base
                    .entry(base)
                    .or_default()
                    .push(name.clone());
            }
        }
        for names in by_component_base.values_mut() {
            names.sort_by(|lhs, rhs| lhs.as_str().cmp(rhs.as_str()));
        }
        Ok(Self {
            inputs,
            by_component_base,
        })
    }

    pub(crate) fn contains(&self, name: &VarName) -> bool {
        self.inputs.contains(name)
    }

    pub(crate) fn resolve_input(&self, name: &str, flat: &Model) -> Option<VarName> {
        self.resolve_inputs(name, flat).into_iter().next()
    }

    pub(crate) fn resolve_inputs(&self, name: &str, flat: &Model) -> Vec<VarName> {
        let lookup_name = VarName::new(name);
        if let Some((key, _)) = flat.variables.get_key_value(&lookup_name)
            && self.contains(key)
        {
            return vec![key.clone()];
        }
        if let Some(candidate) = subscript_fallback_chain(name)
            .into_iter()
            .find(|candidate| self.contains(candidate))
        {
            return vec![candidate];
        }

        let Some(base) = flat::component_base_name(name) else {
            return Vec::new();
        };
        match self.by_component_base.get(&base) {
            Some(inputs) => inputs.clone(),
            None => Vec::new(),
        }
    }
}

/// Check if a top-level component is declared with input causality.
///
/// When a model declares `input SomeType comp;`, the component itself is an
/// external input. Its fields (e.g., `comp.x`) inherit input causality and
/// should remain as DAE inputs, not be promoted to algebraic unknowns.
fn is_top_level_input_component(prefix: &str, flat: &Model) -> bool {
    flat.top_level_input_components.contains(prefix)
}

/// Get the leading array length for a flattened record prefix.
///
/// For a record array prefix like `statesFM`, child fields such as `statesFM.T`
/// carry the array dimensions. This returns the first dimension size used to
/// derive per-element scalar sizes.
fn record_array_length(base: &str, flat: &Model) -> Option<usize> {
    let prefix_dot = format!("{}.", base);
    flat.variables
        .iter()
        .find(|(name, _)| name.as_str().starts_with(&prefix_dot))
        .and_then(|(_, var)| var.dims.first().copied())
        .and_then(|d| usize::try_from(d).ok())
        .or_else(|| infer_record_array_length_from_indexed_fields(base, flat))
}

fn infer_record_array_length_from_indexed_fields(base: &str, flat: &Model) -> Option<usize> {
    let mut max_index = 0usize;

    for name in flat.variables.keys() {
        let mut prefix = String::new();
        for segment in rumoca_core::ComponentPath::from_flat_path(name.as_str()).parts() {
            if !prefix.is_empty() {
                prefix.push('.');
            }
            prefix.push_str(rumoca_core::strip_array_index(segment));
            if prefix != base {
                continue;
            }
            let Some(first_index) = rumoca_core::parse_scalar_name(segment)
                .and_then(|scalar_name| scalar_name.indices.first().copied())
                .and_then(|index| usize::try_from(index).ok())
                .filter(|index| *index > 0)
            else {
                continue;
            };
            max_index = max_index.max(first_index);
        }
    }

    (max_index > 0).then_some(max_index)
}

/// Compute scalar size for a subscripted record-prefix reference.
///
/// Example: if `statesFM` is an array of 2 records with 5 scalar fields each,
/// then `statesFM[1]` has size 5 and `statesFM[1:2]` has size 10.
pub(crate) fn record_subscript_scalar_size(
    full_name: &str,
    base: &str,
    total: usize,
    flat: &Model,
) -> usize {
    let Some(array_len) = record_array_length(base, flat) else {
        return total;
    };
    if array_len == 0 {
        return 0;
    }

    let per_element = total / array_len;
    // Interpret embedded subscripts on the record prefix against its leading
    // array dimension so range slices contribute the correct scalar multiple.
    let selected = compute_embedded_range_size(full_name, &[array_len as i64], flat);
    per_element.saturating_mul(selected)
}

pub(crate) fn infer_record_subscript_size_from_prefix_chain(
    var_name: &VarName,
    fallback_chain: Vec<VarName>,
    prefix_counts: &FxHashMap<String, usize>,
    flat: &Model,
) -> Option<usize> {
    for base in fallback_chain {
        let Some(&total) = prefix_counts.get(base.as_str()) else {
            continue;
        };
        // MLS §10.2: subscripted record array element
        return Some(record_subscript_scalar_size(
            var_name.as_str(),
            base.as_str(),
            total,
            flat,
        ));
    }
    None
}

fn resolve_var_in_flat(name: &VarName, flat: &Model) -> Option<VarName> {
    if flat.variables.contains_key(name) {
        return Some(name.clone());
    }
    subscript_fallback_chain(name.as_str())
        .into_iter()
        .find(|candidate| flat.variables.contains_key(candidate))
}

/// Mark internal inputs that are connected to discrete/event-driven variables.
///
/// This propagates discrete partition membership across connection alias sets so
/// clocked Real inputs (without explicit `discrete` variability) don't remain as
/// orphan continuous algebraics.
pub(crate) fn find_discrete_connected_internal_inputs(
    flat: &Model,
    when_only_vars: &IndexSet<VarName>,
    internal_inputs: &InternalInputIndex,
) -> HashSet<VarName> {
    use std::collections::VecDeque;

    let mut adjacency: HashMap<VarName, HashSet<VarName>> = HashMap::new();
    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let mut refs: HashSet<VarName> = HashSet::default();
        eq.residual.collect_var_refs(&mut refs);
        let mut resolved: Vec<VarName> = refs
            .iter()
            .filter_map(|name| resolve_var_in_flat(name, flat))
            .collect();
        resolved.sort_unstable_by(|lhs, rhs| lhs.as_str().cmp(rhs.as_str()));
        resolved.dedup();
        for i in 0..resolved.len() {
            for j in (i + 1)..resolved.len() {
                let lhs = resolved[i].clone();
                let rhs = resolved[j].clone();
                adjacency
                    .entry(lhs.clone())
                    .or_default()
                    .insert(rhs.clone());
                adjacency.entry(rhs).or_default().insert(lhs);
            }
        }
    }

    let mut visited: HashSet<VarName> = HashSet::default();
    let mut queue: VecDeque<VarName> = VecDeque::new();
    for (name, var) in &flat.variables {
        if is_when_only_var(name, when_only_vars)
            || var.is_discrete_type
            || matches!(var.variability, rumoca_core::Variability::Discrete(_))
        {
            visited.insert(name.clone());
            queue.push_back(name.clone());
        }
    }

    while let Some(current) = queue.pop_front() {
        let Some(neighbors) = adjacency.get(&current) else {
            continue;
        };
        for neighbor in neighbors {
            if !visited.insert(neighbor.clone()) {
                continue;
            }
            queue.push_back(neighbor.clone());
        }
    }

    visited
        .into_iter()
        .filter(|name| internal_inputs.contains(name))
        .collect()
}

/// Identify inputs that appear in connection equations.
/// These become algebraic variables, not inputs, because the connection
/// equation defines their value from the connected output/state.
pub(crate) fn find_connected_inputs(
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> HashSet<VarName> {
    let mut result = HashSet::default();
    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let mut vars: HashSet<VarName> = HashSet::default();
        eq.residual.collect_var_refs(&mut vars);
        result.extend(
            vars.iter()
                .flat_map(|name| internal_inputs.resolve_inputs(name.as_str(), flat)),
        );
    }
    result
}

/// Find internal inputs that only connect to other internal inputs.
///
/// These input-only alias sets do not introduce local unknowns. Inputs connected
/// to a non-input peer are locally driven and must still count as local unknowns.
pub(crate) fn find_connected_inputs_only_connected_to_inputs(
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> HashSet<VarName> {
    let (input_nodes, adjacency) = input_connection_graph(flat, internal_inputs);
    collect_input_only_connection_components(flat, &input_nodes, &adjacency)
        .into_iter()
        .flat_map(|component| component.inputs)
        .collect()
}

/// Pick one declaration-binding anchor per input-only connection component.
///
/// For a connected set containing only internal inputs, the connection
/// equations provide the aliases and one declaration binding provides the
/// default value. Keeping every bound input in the component would add multiple
/// value anchors and over-constrain the DAE balance.
pub(crate) fn find_connected_input_binding_anchors(
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> HashSet<VarName> {
    let (input_nodes, adjacency) = input_connection_graph(flat, internal_inputs);
    collect_input_only_connection_components(flat, &input_nodes, &adjacency)
        .into_iter()
        .filter_map(|component| {
            flat.variables
                .keys()
                .find(|name| {
                    component.inputs.contains(*name)
                        && flat
                            .variables
                            .get(*name)
                            .is_some_and(|var| var.binding.is_some())
                })
                .cloned()
        })
        .collect()
}

fn input_connection_graph(
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> (HashSet<VarName>, HashMap<VarName, HashSet<VarName>>) {
    let mut input_nodes: HashSet<VarName> = HashSet::default();
    let mut adjacency: HashMap<VarName, HashSet<VarName>> = HashMap::default();

    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }

        let Some(lhs_name) = name_resolution::extract_varref_name(lhs) else {
            continue;
        };
        let Some(rhs_name) = name_resolution::extract_varref_name(rhs) else {
            continue;
        };

        let lhs_input = internal_inputs.resolve_input(lhs_name.as_str(), flat);
        let rhs_input = internal_inputs.resolve_input(rhs_name.as_str(), flat);
        let lhs_node = lhs_input.clone().unwrap_or_else(|| lhs_name.clone());
        let rhs_node = rhs_input.clone().unwrap_or_else(|| rhs_name.clone());

        if lhs_input.is_some() {
            input_nodes.insert(lhs_node.clone());
        }
        if rhs_input.is_some() {
            input_nodes.insert(rhs_node.clone());
        }

        adjacency
            .entry(lhs_node.clone())
            .or_default()
            .insert(rhs_node.clone());
        adjacency.entry(rhs_node).or_default().insert(lhs_node);
    }

    (input_nodes, adjacency)
}

fn collect_input_only_connection_components(
    flat: &Model,
    input_nodes: &HashSet<VarName>,
    adjacency: &HashMap<VarName, HashSet<VarName>>,
) -> Vec<ConnectionComponent> {
    let mut input_only = Vec::new();
    let mut visited = HashSet::default();
    for start in flat
        .variables
        .keys()
        .filter(|name| input_nodes.contains(*name))
    {
        if visited.contains(start) {
            continue;
        }
        let component = collect_connection_component(start, input_nodes, adjacency, &mut visited);
        if component.has_non_input_peer {
            continue;
        }
        input_only.push(component);
    }
    input_only
}

struct ConnectionComponent {
    inputs: Vec<VarName>,
    has_non_input_peer: bool,
}

fn collect_connection_component(
    start: &VarName,
    input_nodes: &HashSet<VarName>,
    adjacency: &HashMap<VarName, HashSet<VarName>>,
    visited: &mut HashSet<VarName>,
) -> ConnectionComponent {
    use std::collections::VecDeque;

    let mut queue = VecDeque::new();
    let mut inputs = Vec::new();
    let mut has_non_input_peer = false;
    visited.insert(start.clone());
    queue.push_back(start.clone());

    while let Some(current) = queue.pop_front() {
        if input_nodes.contains(&current) {
            inputs.push(current.clone());
        } else {
            has_non_input_peer = true;
        }
        let Some(neighbors) = adjacency.get(&current) else {
            continue;
        };
        for neighbor in neighbors {
            if visited.insert(neighbor.clone()) {
                queue.push_back(neighbor.clone());
            }
        }
    }

    ConnectionComponent {
        inputs,
        has_non_input_peer,
    }
}

/// Check if a residual `lhs - rhs = 0` is an intra-component alias where the
/// RHS is an internal input that should be promoted to algebraic.
///
/// This catches the MSL BaseProperties pattern `state.p = p` which flattens to
/// `medium.state.p - medium.p = 0`. Conditions for promotion:
/// 1. Both LHS and RHS are simple VarRefs
/// 2. RHS is an internal input without a binding
/// 3. LHS is NOT connected (not a connector variable)
/// 4. The equation originates from the same component that owns the input
///    (full parent path must match, not just top-level prefix)
///
/// Condition 3 distinguishes BaseProperties aliases (state.p is a record field,
/// not connected) from connector aliases (port.T is connected to the outside).
///
/// Condition 4 prevents false positives when a parent model uses a sub-component's
/// input. E.g., `volume.vessel_ps_static = volume.medium.p` is from the Volume
/// model (origin `volume`), but `volume.medium.p` belongs to `volume.medium`.
/// These don't match, so the promotion is correctly skipped.
fn check_rhs_intra_component_alias(
    lhs: &Expression,
    rhs: &Expression,
    origin: &rumoca_ir_flat::EquationOrigin,
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> Option<VarName> {
    // Both sides must be simple VarRefs (a variable alias equation)
    let Expression::VarRef { name: lhs_name, .. } = lhs else {
        return None;
    };
    let Expression::VarRef { name: rhs_name, .. } = rhs else {
        return None;
    };
    // RHS must be an internal input
    let resolved = internal_inputs.resolve_input(rhs_name.as_str(), flat)?;
    // Skip inputs that already have bindings
    if flat
        .variables
        .get(&resolved)
        .is_some_and(|v| v.binding.is_some())
    {
        return None;
    }
    // Input-to-input aliases mean the RHS input is locally defined by the
    // same alias set. This covers parent components assigning a child input
    // from another internal input, e.g. `motor.omega_cmd = omega_cmd`.
    if internal_inputs
        .resolve_input(lhs_name.as_str(), flat)
        .is_some()
    {
        return Some(resolved);
    }

    // LHS must NOT be connected (distinguishes record aliases from connector aliases).
    // Use full fallback-chain matching so multi-layer indexed aliases (e.g.
    // `conn[1].field[2]`) still resolve to their connected base path.
    let lhs_is_connected = flat
        .variables
        .get(lhs_name.var_name())
        .is_some_and(|v| v.connected)
        || subscript_fallback_chain(lhs_name.as_str())
            .into_iter()
            .any(|candidate| flat.variables.get(&candidate).is_some_and(|v| v.connected));
    if lhs_is_connected {
        return None;
    }
    // The equation must originate from the SAME component that owns the input.
    // For `medium.state.p = medium.p` (from BaseProperties), the input `medium.p`
    // has parent `medium` and the equation origin is `medium` → match.
    // For `volume.vessel_ps_static = volume.medium.p` (from Volume), the input
    // `volume.medium.p` has parent `volume.medium` but the equation origin is
    // `volume` → no match (the Volume model is using medium.p, not defining it).
    let input_parent = resolved.enclosing_scope()?;
    let eq_component = origin.component_name()?;
    if input_parent == eq_component {
        Some(resolved)
    } else {
        None
    }
}

/// Find input variables that are defined by non-connection equations.
///
/// When a model equation assigns to an input variable (e.g., `medium.p = p`),
/// the input is effectively an unknown with a defining equation, not an
/// externally provided value. These inputs should be promoted to algebraic
/// variables per MLS §4.4.2.2.
///
/// The LHS of `lhs - rhs = 0` is always checked: an input on the LHS means
/// the equation directly assigns to it.
///
/// The RHS is only checked when the equation originates from the SAME
/// component as the input (intra-component equation). This handles the MSL
/// BaseProperties pattern where `state.p = p` flattens to
/// `medium.state.p - medium.p = 0` — both variables and the equation belong
/// to "medium". Cross-component equations like `y = medium.p` (from the
/// enclosing model) should NOT promote the input, because `medium.p` is the
/// value source there, not a variable being defined.
pub(crate) fn find_equation_defined_inputs(
    flat: &Model,
    internal_inputs: &InternalInputIndex,
) -> HashSet<VarName> {
    let mut result = HashSet::default();
    for eq in flat
        .equations
        .iter()
        .filter(|eq| !eq.origin.is_connection())
    {
        let Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }
        let mut lhs_internal_inputs = HashSet::default();
        let lhs_is_internal_input_alias =
            collect_alias_internal_inputs(lhs, flat, internal_inputs, &mut lhs_internal_inputs)
                && !lhs_internal_inputs.is_empty();
        collect_lhs_internal_inputs(lhs, flat, internal_inputs, &mut result);
        if lhs_is_internal_input_alias {
            collect_alias_internal_inputs(rhs, flat, internal_inputs, &mut result);
        }
        // Check RHS for internal input VarRef in intra-component alias equations.
        // Skip inputs with bindings (already promoted via binding check, and
        // adding them to connected_inputs would suppress their binding equation).
        else if let Some(resolved) =
            check_rhs_intra_component_alias(lhs, rhs, &eq.origin, flat, internal_inputs)
        {
            result.insert(resolved);
        }
    }
    result
}

fn collect_alias_internal_inputs(
    expr: &Expression,
    flat: &Model,
    internal_inputs: &InternalInputIndex,
    result: &mut HashSet<VarName>,
) -> bool {
    match expr {
        Expression::VarRef { name, .. } => {
            for resolved in internal_inputs.resolve_inputs(name.as_str(), flat) {
                result.insert(resolved);
            }
            true
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => elements
            .iter()
            .all(|element| collect_alias_internal_inputs(element, flat, internal_inputs, result)),
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            collect_alias_internal_inputs(base, flat, internal_inputs, result)
        }
        _ => false,
    }
}

fn collect_lhs_internal_inputs(
    expr: &Expression,
    flat: &Model,
    internal_inputs: &InternalInputIndex,
    result: &mut HashSet<VarName>,
) {
    match expr {
        Expression::VarRef { name, .. } => {
            for resolved in internal_inputs.resolve_inputs(name.as_str(), flat) {
                result.insert(resolved);
            }
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_lhs_internal_inputs(element, flat, internal_inputs, result);
            }
        }
        Expression::Index { base, .. } => {
            collect_lhs_internal_inputs(base, flat, internal_inputs, result);
        }
        Expression::FieldAccess { base, .. } => {
            collect_lhs_internal_inputs(base, flat, internal_inputs, result);
        }
        _ => {}
    }
}

fn is_runtime_intrinsic_function_short_name(short_name: &str) -> bool {
    matches!(
        short_name,
        "ExternalCombiTimeTable"
            | "ExternalCombiTable1D"
            | "ExternalCombiTable2D"
            | "getTimeTableTmax"
            | "getTimeTableTmin"
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
            | "to_degC"
            | "from_degC"
            | "to_deg"
            | "from_deg"
            | "assert"
            | "cardinality"
            | "String"
            | "array"
            // MLS built-in operator record constructor for Complex values.
            | "Complex"
            | "getInstanceName"
            | "loadResource"
            | "isValidTable"
    )
}

fn is_builtin_or_runtime_intrinsic_function(name: &VarName) -> bool {
    let short_name = name.last_segment();
    BuiltinFunction::from_name(short_name).is_some()
        || BuiltinFunction::from_name(&short_name.to_ascii_lowercase()).is_some()
        || is_runtime_intrinsic_function_short_name(short_name)
}

pub(crate) fn resolve_flat_function<'a>(name: &str, flat: &'a Model) -> Option<&'a Function> {
    let lookup_name = VarName::new(name);
    flat.functions.get(&lookup_name).or_else(|| {
        let short_name = lookup_name.last_segment();
        let mut matches = flat
            .functions
            .iter()
            .filter(|(candidate, _)| candidate.last_segment() == short_name);
        let (_, function) = matches.next()?;
        matches.next().is_none().then_some(function)
    })
}

fn validate_function_call_name(
    name: &VarName,
    flat: &Model,
    span: Span,
    callable_formals: Option<&HashSet<VarName>>,
) -> Result<(), ToDaeError> {
    if is_builtin_or_runtime_intrinsic_function(name) {
        return Ok(());
    }
    if callable_formals.is_some_and(|formals| formals.contains(name)) {
        return Ok(());
    }

    let Some(func) = resolve_flat_function(name.as_str(), flat) else {
        return Err(ToDaeError::unresolved_function_call(name.as_str(), span));
    };

    if func.is_constructor {
        return Ok(());
    }

    if func.external.is_none() && func.body.is_empty() {
        let short_name = func.name.last_segment();
        if !is_runtime_intrinsic_function_short_name(short_name) {
            return Err(ToDaeError::function_without_body(func.name.as_str(), span));
        }
    }

    Ok(())
}

fn validate_field_access_functions(
    base: &Expression,
    field: &str,
    flat: &Model,
    span: Span,
    reachable_calls: &mut IndexSet<ReachableCall>,
    callable_formals: Option<&HashSet<VarName>>,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        span: constructor_span,
    } = base
    {
        let owner_span = if constructor_span.is_dummy() {
            span
        } else {
            *constructor_span
        };
        for arg in args {
            validate_flat_expression_functions(
                arg,
                flat,
                owner_span,
                reachable_calls,
                callable_formals,
            )?;
        }

        let selected_name = if args.is_empty() {
            format!("{}.{}", name.as_str(), field)
        } else {
            format!(
                "{}.{} (constructor_args={})",
                name.as_str(),
                field,
                args.len()
            )
        };
        let Some(constructor) = resolve_flat_function(name.as_str(), flat) else {
            if crate::todae_debug_enabled() {
                let short_name = name.last_segment().to_string();
                let total_functions = flat.functions.len();
                crate::log_todae_debug(format!(
                    "DEBUG TODAE missing constructor={} field={} short_name={} total_functions={}",
                    name.as_str(),
                    field,
                    short_name,
                    total_functions
                ));
            }
            return Err(ToDaeError::constructor_field_selection_unresolved(
                selected_name,
                owner_span,
            ));
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == field)
            || constructor.outputs.iter().any(|param| param.name == field);
        let field_resolves_from_positional =
            crate::constructor_field_selection::positional_constructor_arg_for_field(args, field)
                .is_some();
        if !field_known && !field_resolves_from_positional {
            if crate::todae_debug_enabled() {
                let mut available_fields: Vec<String> = constructor
                    .inputs
                    .iter()
                    .map(|param| format!("in:{}", param.name))
                    .chain(
                        constructor
                            .outputs
                            .iter()
                            .map(|param| format!("out:{}", param.name)),
                    )
                    .collect();
                available_fields.sort();
                crate::log_todae_debug(format!(
                    "DEBUG TODAE constructor field missing={} available={available_fields:?}",
                    selected_name
                ));
            }
            return Err(ToDaeError::constructor_field_selection_unresolved(
                selected_name,
                owner_span,
            ));
        }
        return Ok(());
    }

    validate_flat_expression_functions(base, flat, span, reachable_calls, callable_formals)
}

struct FlatFunctionCallValidator<'a> {
    flat: &'a Model,
    inherited_span: Span,
    reachable_calls: &'a mut IndexSet<ReachableCall>,
    callable_formals: Option<&'a HashSet<VarName>>,
}

impl FlatFunctionCallValidator<'_> {
    fn child_span(&self, expr: &Expression) -> Span {
        expr.span().unwrap_or(self.inherited_span)
    }
}

impl FallibleExpressionVisitor for FlatFunctionCallValidator<'_> {
    type Error = ToDaeError;

    fn visit_expression(&mut self, expr: &Expression) -> Result<(), Self::Error> {
        let previous_span = self.inherited_span;
        self.inherited_span = self.child_span(expr);
        let result = if let Expression::FunctionCall {
            args,
            is_constructor: true,
            ..
        } = expr
        {
            for arg in args {
                self.visit_expression(arg)?;
            }
            Ok(())
        } else {
            self.walk_expression(expr)
        };
        self.inherited_span = previous_span;
        result
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
    ) -> Result<(), Self::Error> {
        if !is_constructor {
            validate_function_call_name(
                name.var_name(),
                self.flat,
                self.inherited_span,
                self.callable_formals,
            )?;
        }
        if !is_constructor
            && !is_builtin_or_runtime_intrinsic_function(name.var_name())
            && !self
                .callable_formals
                .is_some_and(|formals| formals.contains(name.var_name()))
            && !resolve_flat_function(name.as_str(), self.flat)
                .is_some_and(|func| func.is_constructor)
        {
            record_reachable_call(
                self.reachable_calls,
                name.var_name().clone(),
                self.inherited_span,
            );
        }
        for arg in args {
            let mut child = FlatFunctionCallValidator {
                flat: self.flat,
                inherited_span: self.child_span(arg),
                reachable_calls: self.reachable_calls,
                callable_formals: self.callable_formals,
            };
            child.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_builtin_call(
        &mut self,
        _function: &BuiltinFunction,
        args: &[Expression],
    ) -> Result<(), Self::Error> {
        for arg in args {
            let mut child = FlatFunctionCallValidator {
                flat: self.flat,
                inherited_span: self.child_span(arg),
                reachable_calls: self.reachable_calls,
                callable_formals: self.callable_formals,
            };
            child.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_field_access(&mut self, base: &Expression, field: &str) -> Result<(), Self::Error> {
        let span = self.child_span(base);
        validate_field_access_functions(
            base,
            field,
            self.flat,
            span,
            self.reachable_calls,
            self.callable_formals,
        )
    }
}

impl FallibleStatementVisitor for FlatFunctionCallValidator<'_> {
    fn visit_statement(&mut self, stmt: &Statement) -> Result<(), Self::Error> {
        let inherited_span = statement_span(stmt).unwrap_or(self.inherited_span);
        let mut child = FlatFunctionCallValidator {
            flat: self.flat,
            inherited_span,
            reachable_calls: self.reachable_calls,
            callable_formals: self.callable_formals,
        };
        child.walk_statement(stmt)
    }

    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        _outputs: &[ComponentReference],
    ) -> Result<(), Self::Error> {
        let name = comp.to_var_name();
        validate_function_call_name(&name, self.flat, self.inherited_span, self.callable_formals)?;
        if !is_builtin_or_runtime_intrinsic_function(&name)
            && !self
                .callable_formals
                .is_some_and(|formals| formals.contains(&name))
        {
            record_reachable_call(self.reachable_calls, name, self.inherited_span);
        }
        for arg in args {
            let mut child = FlatFunctionCallValidator {
                flat: self.flat,
                inherited_span: self.child_span(arg),
                reachable_calls: self.reachable_calls,
                callable_formals: self.callable_formals,
            };
            child.visit_expression(arg)?;
        }
        Ok(())
    }
}

fn validate_flat_expression_functions(
    expr: &Expression,
    flat: &Model,
    span: Span,
    reachable_calls: &mut IndexSet<ReachableCall>,
    callable_formals: Option<&HashSet<VarName>>,
) -> Result<(), ToDaeError> {
    let mut validator = FlatFunctionCallValidator {
        flat,
        inherited_span: expr.span().unwrap_or(span),
        reachable_calls,
        callable_formals,
    };
    validator.visit_expression(expr)
}

fn validate_statement_functions(
    stmt: &Statement,
    flat: &Model,
    span: Span,
    reachable_calls: &mut IndexSet<ReachableCall>,
    callable_formals: Option<&HashSet<VarName>>,
) -> Result<(), ToDaeError> {
    let mut validator = FlatFunctionCallValidator {
        flat,
        inherited_span: statement_span(stmt).unwrap_or(span),
        reachable_calls,
        callable_formals,
    };
    validator.visit_statement(stmt)
}

fn statement_span(stmt: &Statement) -> Option<Span> {
    let span = match stmt {
        Statement::Assignment { span, .. }
        | Statement::For { span, .. }
        | Statement::While { span, .. }
        | Statement::If { span, .. }
        | Statement::When { span, .. }
        | Statement::FunctionCall { span, .. }
        | Statement::Reinit { span, .. }
        | Statement::Assert { span, .. }
        | Statement::Empty { span }
        | Statement::Return { span }
        | Statement::Break { span } => *span,
    };
    (!span.is_dummy()).then_some(span)
}

fn validate_when_equation_functions(
    equation: &rumoca_ir_flat::WhenEquation,
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    match equation {
        rumoca_ir_flat::WhenEquation::Assign { value, span, .. } => {
            validate_flat_expression_functions(value, flat, *span, reachable_calls, None)?
        }
        rumoca_ir_flat::WhenEquation::Reinit { value, span, .. } => {
            validate_flat_expression_functions(value, flat, *span, reachable_calls, None)?
        }
        rumoca_ir_flat::WhenEquation::Assert {
            condition,
            message,
            span,
            ..
        } => {
            validate_flat_expression_functions(condition, flat, *span, reachable_calls, None)?;
            validate_flat_expression_functions(message, flat, *span, reachable_calls, None)?;
        }
        rumoca_ir_flat::WhenEquation::Terminate { message, span, .. } => {
            validate_flat_expression_functions(message, flat, *span, reachable_calls, None)?;
        }
        rumoca_ir_flat::WhenEquation::Conditional {
            branches,
            else_branch,
            span,
            ..
        } => {
            for (condition, equations) in branches {
                validate_flat_expression_functions(condition, flat, *span, reachable_calls, None)?;
                for nested in equations {
                    validate_when_equation_functions(nested, flat, reachable_calls)?;
                }
            }
            for nested in else_branch {
                validate_when_equation_functions(nested, flat, reachable_calls)?;
            }
        }
        rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, span, .. } => {
            validate_flat_expression_functions(function, flat, *span, reachable_calls, None)?
        }
    }
    Ok(())
}

fn function_typed_formals(function: &Function, flat: &Model) -> HashSet<VarName> {
    let mut formals = HashSet::default();
    for param in function.inputs.iter().chain(function.locals.iter()) {
        if param
            .type_class
            .as_ref()
            .is_some_and(|class_type| matches!(class_type, rumoca_core::ClassType::Function))
            || flat
                .functions
                .contains_key(&VarName::new(param.type_name.clone()))
        {
            formals.insert(VarName::new(param.name.clone()));
            formals.insert(VarName::new(format!(
                "{}.{}",
                function.name.as_str(),
                param.name
            )));
        }
    }
    formals
}

fn validate_flat_variable_function_calls(
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        let is_param_or_const = matches!(
            variable.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        );
        if is_param_or_const {
            continue;
        }

        for expr in [
            variable.start.as_ref(),
            variable.min.as_ref(),
            variable.max.as_ref(),
            variable.nominal.as_ref(),
            variable.binding.as_ref(),
        ]
        .into_iter()
        .flatten()
        {
            validate_flat_expression_functions(
                expr,
                flat,
                variable.source_span,
                reachable_calls,
                None,
            )?
        }
    }
    Ok(())
}

fn validate_flat_equation_function_calls(
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    for equation in flat.equations.iter().chain(flat.initial_equations.iter()) {
        validate_flat_expression_functions(
            &equation.residual,
            flat,
            equation.span,
            reachable_calls,
            None,
        )?
    }
    Ok(())
}

fn validate_flat_assertion_function_calls(
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    for assertion in flat
        .assert_equations
        .iter()
        .chain(flat.initial_assert_equations.iter())
    {
        validate_flat_expression_functions(
            &assertion.condition,
            flat,
            assertion.span,
            reachable_calls,
            None,
        )?;
        validate_flat_expression_functions(
            &assertion.message,
            flat,
            assertion.span,
            reachable_calls,
            None,
        )?;
        if let Some(level) = &assertion.level {
            validate_flat_expression_functions(level, flat, assertion.span, reachable_calls, None)?;
        }
    }
    Ok(())
}

fn validate_flat_when_function_calls(
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    for when in &flat.when_clauses {
        validate_flat_expression_functions(
            &when.condition,
            flat,
            when.span,
            reachable_calls,
            None,
        )?;
        for equation in &when.equations {
            validate_when_equation_functions(equation, flat, reachable_calls)?;
        }
    }
    Ok(())
}

fn validate_flat_algorithm_function_calls(
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    for algorithm in flat.algorithms.iter().chain(flat.initial_algorithms.iter()) {
        for statement in &algorithm.statements {
            validate_statement_functions(statement, flat, algorithm.span, reachable_calls, None)?;
        }
    }
    Ok(())
}

fn validate_reachable_function_bodies(
    flat: &Model,
    reachable_calls: &mut IndexSet<ReachableCall>,
) -> Result<(), ToDaeError> {
    let mut visited: HashSet<VarName> = HashSet::default();
    while let Some(call) = reachable_calls.pop() {
        let function_name = call.name;
        if !visited.insert(function_name.clone()) {
            continue;
        }
        let Some(function) = resolve_flat_function(function_name.as_str(), flat) else {
            return Err(ToDaeError::unresolved_function_call(
                function_name.as_str(),
                call.span,
            ));
        };
        let callable_formals = function_typed_formals(function, flat);
        let callable_formals = (!callable_formals.is_empty()).then_some(callable_formals);
        for param in function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
        {
            if let Some(default_expr) = &param.default {
                validate_flat_expression_functions(
                    default_expr,
                    flat,
                    function.span,
                    reachable_calls,
                    callable_formals.as_ref(),
                )?;
            }
        }
        for statement in &function.body {
            validate_statement_functions(
                statement,
                flat,
                function.span,
                reachable_calls,
                callable_formals.as_ref(),
            )?
        }
    }
    Ok(())
}

pub(crate) fn validate_flat_function_calls(flat: &Model) -> Result<(), ToDaeError> {
    let mut reachable_calls: IndexSet<ReachableCall> = IndexSet::new();
    validate_flat_variable_function_calls(flat, &mut reachable_calls)?;
    validate_flat_equation_function_calls(flat, &mut reachable_calls)?;
    validate_flat_assertion_function_calls(flat, &mut reachable_calls)?;
    validate_flat_when_function_calls(flat, &mut reachable_calls)?;
    validate_flat_algorithm_function_calls(flat, &mut reachable_calls)?;
    validate_reachable_function_bodies(flat, &mut reachable_calls)?;
    Ok(())
}

pub(crate) fn is_when_only_var(name: &VarName, when_only_vars: &IndexSet<VarName>) -> bool {
    when_only_vars.contains(name)
        || subscript_fallback_chain(name.as_str())
            .into_iter()
            .any(|candidate| when_only_vars.contains(&candidate))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2)
    }

    fn add_oc_gamma(flat: &mut Model, record_path: &str) -> VarName {
        let name = VarName::new(format!("{record_path}.gamma"));
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name: name.clone(),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(record_path.to_string()),
                oc_eq_constraint_size: Some(0),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        name
    }

    fn add_output(flat: &mut Model, name: &str) {
        let name = VarName::new(name);
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                variability: rumoca_core::Variability::Empty,
                causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    #[test]
    fn overconstrained_alias_states_keep_only_definite_root_record() {
        let mut flat = Model::new();
        let root = add_oc_gamma(&mut flat, "constantSource.port_p.reference");
        add_oc_gamma(&mut flat, "constantSource.port_n.reference");
        let branch = add_oc_gamma(&mut flat, "constantReluctance.port_p.reference");
        add_oc_gamma(&mut flat, "constantReluctance.port_n.reference");
        let other_branch = add_oc_gamma(&mut flat, "leakageWithCoefficient.port_p.reference");
        add_oc_gamma(&mut flat, "leakageWithCoefficient.port_n.reference");
        flat.definite_roots
            .insert("constantSource.port_p.reference".to_string());
        flat.branches.push((
            "constantSource.port_p.reference".to_string(),
            "constantSource.port_n.reference".to_string(),
        ));
        flat.branches.push((
            "constantReluctance.port_p.reference".to_string(),
            "constantReluctance.port_n.reference".to_string(),
        ));
        flat.branches.push((
            "leakageWithCoefficient.port_p.reference".to_string(),
            "leakageWithCoefficient.port_n.reference".to_string(),
        ));
        flat.optional_edges.push((
            "constantSource.port_p.reference".to_string(),
            "constantReluctance.port_p.reference".to_string(),
        ));
        flat.optional_edges.push((
            "constantReluctance.port_n.reference".to_string(),
            "leakageWithCoefficient.port_p.reference".to_string(),
        ));

        let mut states = IndexSet::new();
        states.insert(root.clone());
        states.insert(branch.clone());
        states.insert(other_branch.clone());

        let filtered = filter_overconstrained_alias_states(states, &flat);

        assert_eq!(filtered.len(), 1);
        assert!(filtered.contains(&root));
        assert!(!filtered.contains(&branch));
        assert!(!filtered.contains(&other_branch));
    }

    #[test]
    fn overconstrained_alias_states_leave_rootless_component_unchanged() {
        let mut flat = Model::new();
        let first = add_oc_gamma(&mut flat, "a.reference");
        let second = add_oc_gamma(&mut flat, "b.reference");
        flat.optional_edges
            .push(("a.reference".to_string(), "b.reference".to_string()));

        let mut states = IndexSet::new();
        states.insert(first.clone());
        states.insert(second.clone());

        let filtered = filter_overconstrained_alias_states(states, &flat);

        assert_eq!(filtered.len(), 2);
        assert!(filtered.contains(&first));
        assert!(filtered.contains(&second));
    }

    #[test]
    fn overconstrained_alias_states_choose_lowest_priority_potential_root() {
        let mut flat = Model::new();
        let high_priority = add_oc_gamma(&mut flat, "high.reference");
        let low_priority = add_oc_gamma(&mut flat, "low.reference");
        let branch = add_oc_gamma(&mut flat, "branch.reference");
        flat.potential_roots
            .push(("high.reference".to_string(), 256));
        flat.potential_roots.push(("low.reference".to_string(), 10));
        flat.optional_edges
            .push(("high.reference".to_string(), "branch.reference".to_string()));
        flat.optional_edges
            .push(("branch.reference".to_string(), "low.reference".to_string()));

        let states =
            IndexSet::from_iter([high_priority.clone(), low_priority.clone(), branch.clone()]);
        let alias_roots = find_overconstrained_derivative_alias_roots(&states, &flat);

        assert_eq!(alias_roots.get(&high_priority), Some(&low_priority));
        assert_eq!(alias_roots.get(&branch), Some(&low_priority));
        assert!(!alias_roots.contains_key(&low_priority));
    }

    #[test]
    fn overconstrained_alias_states_keep_output_sensor_derivative_state() {
        let mut flat = Model::new();
        let root = add_oc_gamma(&mut flat, "source.port_p.reference");
        let physical = add_oc_gamma(&mut flat, "reluctance.port_p.reference");
        let sensor = add_oc_gamma(&mut flat, "frequencySensor.port.reference");
        add_output(&mut flat, "frequencySensor.y");
        flat.definite_roots
            .insert("source.port_p.reference".to_string());
        flat.optional_edges.push((
            "source.port_p.reference".to_string(),
            "reluctance.port_p.reference".to_string(),
        ));
        flat.optional_edges.push((
            "source.port_p.reference".to_string(),
            "frequencySensor.port.reference".to_string(),
        ));

        let states = IndexSet::from_iter([root.clone(), physical.clone(), sensor.clone()]);
        let alias_roots = find_overconstrained_derivative_alias_roots(&states, &flat);
        let filtered = filter_overconstrained_alias_states(states, &flat);

        assert_eq!(alias_roots.len(), 1);
        assert_eq!(alias_roots.get(&physical), Some(&root));
        assert!(!alias_roots.contains_key(&sensor));
        assert!(filtered.contains(&root));
        assert!(!filtered.contains(&physical));
        assert!(filtered.contains(&sensor));
    }
}
