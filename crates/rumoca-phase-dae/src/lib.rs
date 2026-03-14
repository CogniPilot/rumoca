//! ToDae phase for the Rumoca compiler.
//!
//! This crate implements the conversion from Model to DAE (Differential-Algebraic Equation)
//! representation per MLS Appendix B.
//!
//! # Overview
//!
//! The ToDae phase is responsible for:
//! - Classifying variables by role (state, algebraic, input, output, parameter, constant, discrete)
//! - Separating equations into ODE, algebraic, and output categories
//! - Identifying state variables (those with derivatives)
//! - Building the hybrid DAE system structure
//!
//! # Example
//!
//! ```ignore
//! use rumoca_phase_dae::to_dae;
//!
//! let flat: Model = flatten(instanced)?;
//! let dae: Dae = to_dae(&flat)?;
//! ```

mod algorithm_lowering;
mod appendix_b_validation;
mod balance_counting;
mod binding_conversion;
mod classification;
mod condition_lowering;
mod connector_input_analysis;
mod convert;
mod definition_analysis;
mod discrete_partition;
mod equation_conversion;
mod errors;
mod initial;
mod name_resolution;
mod overconstrained_interface;
mod path_utils;
mod pre_lowering;
mod reference_validation;
mod runtime_precompute;
mod scalar_inference;
mod scalar_size;
mod when_analysis;
mod when_conversion;
mod when_guard;

use algorithm_lowering::{
    canonicalize_discrete_assignment_equations, lower_algorithms_to_equations,
    route_discrete_event_equations,
};
use condition_lowering::populate_canonical_conditions;
pub(crate) use convert::{
    dae_to_flat_expression, dae_to_flat_var_name, flat_to_dae_expression, flat_to_dae_function_map,
    flat_to_dae_var_name,
};
use indexmap::{IndexMap, IndexSet};
#[cfg(test)]
use path_utils::strip_subscript;
use path_utils::{
    get_top_level_prefix, has_top_level_dot, normalized_top_level_names, path_is_in_top_level_set,
    subscript_fallback_chain,
};
use reference_validation::{validate_dae_constructor_field_projections, validate_dae_references};
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use runtime_precompute::populate_runtime_precompute;
use rustc_hash::FxHashMap;
use scalar_inference::*;
use std::collections::{HashMap, HashSet};
use when_conversion::convert_when_clause;

pub use errors::{ToDaeError, ToDaeResult};

type Dae = dae::Dae;
type Variable = dae::Variable;
type VariableKind = dae::VariableKind;
type BuiltinFunction = flat::BuiltinFunction;
type ComponentReference = flat::ComponentReference;
type ComprehensionIndex = flat::ComprehensionIndex;
type EquationOrigin = flat::EquationOrigin;
type Expression = flat::Expression;
type Function = flat::Function;
type Literal = flat::Literal;
type Model = flat::Model;
type Statement = flat::Statement;
type StatementBlock = flat::StatementBlock;
type Subscript = flat::Subscript;
type VarName = flat::VarName;
type Variability = rumoca_ir_core::Variability;

/// Options controlling ToDAE conversion strictness.
#[derive(Debug, Clone, Copy)]
pub struct ToDaeOptions {
    /// Whether to return an error for non-partial unbalanced models.
    pub error_on_unbalanced: bool,
}

impl Default for ToDaeOptions {
    fn default() -> Self {
        Self {
            error_on_unbalanced: true,
        }
    }
}

/// Filter der() variables to only include those that will be classified as states.
///
/// External interface inputs are not states (`der(u)` differentiates an incoming
/// signal), but internal sub-component inputs can become states when they are
/// defined by local equations (e.g. `der(medium.p) = 0` in MSL media examples).
fn filter_state_variables(der_vars: HashSet<VarName>, flat: &Model) -> HashSet<VarName> {
    der_vars
        .into_iter()
        .filter(|name| {
            flat.variables.get(name).is_none_or(|v| {
                !matches!(&v.causality, rumoca_ir_core::Causality::Input(_))
                    || is_internal_input(name, flat)
            })
        })
        .collect()
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
fn collect_continuous_equation_lhs(flat: &Model) -> (HashSet<VarName>, HashSet<VarName>) {
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

    if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
        return None;
    }

    // LHS could be a VarRef with or without subscripts
    match lhs.as_ref() {
        Expression::VarRef { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn connection_alias_pair(eq: &flat::Equation) -> Option<(VarName, VarName)> {
    if !eq.origin.is_connection() {
        return None;
    }
    let Expression::Binary { op, lhs, rhs } = &eq.residual else {
        return None;
    };
    if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
        return None;
    }
    let Expression::VarRef { name: lhs_name, .. } = lhs.as_ref() else {
        return None;
    };
    let Expression::VarRef { name: rhs_name, .. } = rhs.as_ref() else {
        return None;
    };
    Some((lhs_name.clone(), rhs_name.clone()))
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
fn find_when_only_vars(
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
fn count_interface_flows(flat: &Model) -> usize {
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

fn count_overconstrained_interface(flat: &Model, state_vars: &HashSet<VarName>) -> i64 {
    overconstrained_interface::count_overconstrained_interface(flat, state_vars)
}

#[cfg(test)]
fn build_record_components<'a>(
    record_paths: &[&'a str],
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> (FxHashMap<&'a str, usize>, usize) {
    overconstrained_interface::build_record_components(record_paths, branches, optional_edges)
}

/// Check if a variable is a continuous unknown (state or algebraic, not param/const).
fn is_continuous_unknown(flat: &Model, state_vars: &HashSet<VarName>, name: &VarName) -> bool {
    state_vars.contains(name)
        || flat.variables.get(name).is_some_and(|v| {
            !matches!(
                v.variability,
                rumoca_ir_flat::Variability::Constant(_)
                    | rumoca_ir_flat::Variability::Parameter(_)
            )
        })
}

/// Check if a variable is an internal (non-interface) input that should be promoted.
///
/// Top-level PUBLIC inputs are external interfaces and should remain as inputs.
/// Sub-component inputs (with dots) and protected top-level inputs are internal
/// and should be promoted to algebraics when connected or equation-defined
/// (MLS §4.4.2.2).
fn is_internal_input(name: &VarName, flat: &Model) -> bool {
    let var = match flat.variables.get(name) {
        Some(v) => v,
        None => return false,
    };
    if !matches!(&var.causality, rumoca_ir_core::Causality::Input(_)) {
        return false;
    }
    // Sub-component inputs (with dots) are internal, UNLESS the top-level
    // prefix is a top-level connector (interface inputs stay as inputs)
    // or the top-level parent is itself declared as input (e.g.,
    // `input Record state;` → `state.field` stays external).
    if has_top_level_dot(name.as_str()) {
        let prefix = get_top_level_prefix(name.as_str()).unwrap_or_default();
        if flat.top_level_connectors.contains(&prefix) {
            return false;
        }
        // If the parent component is a top-level input, its fields are
        // external interface values, not local unknowns.
        if is_top_level_input_component(&prefix, flat) {
            return false;
        }
        return true;
    }
    // Top-level protected inputs are internal implementation details (MLS §4.4.2.2)
    var.is_protected
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
    let indexed_prefix = format!("{base}[");
    let mut max_index = 0usize;

    for name in flat.variables.keys() {
        let Some(rest) = name.as_str().strip_prefix(&indexed_prefix) else {
            continue;
        };
        let Some(bracket_end) = rest.find(']') else {
            continue;
        };
        let first_index_text = rest[..bracket_end]
            .split(',')
            .next()
            .map(str::trim)
            .unwrap_or_default();
        let Ok(first_index) = first_index_text.parse::<usize>() else {
            continue;
        };
        max_index = max_index.max(first_index);
    }

    (max_index > 0).then_some(max_index)
}

/// Compute scalar size for a subscripted record-prefix reference.
///
/// Example: if `statesFM` is an array of 2 records with 5 scalar fields each,
/// then `statesFM[1]` has size 5 and `statesFM[1:2]` has size 10.
fn record_subscript_scalar_size(full_name: &str, base: &str, total: usize, flat: &Model) -> usize {
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

fn infer_record_subscript_size_from_prefix_chain(
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

/// Resolve a var ref name to its internal input base name.
///
/// Returns the name (or its base name with subscripts stripped) if it refers
/// to an internal input variable. Array connection equations produce per-element
/// names like `sum.u[1]` while the flat variable map stores the base `sum.u`.
fn resolve_internal_input(name: &VarName, flat: &Model) -> Option<VarName> {
    if is_internal_input(name, flat) {
        return Some(name.clone());
    }
    subscript_fallback_chain(name)
        .into_iter()
        .find(|candidate| is_internal_input(candidate, flat))
}

fn resolve_var_in_flat(name: &VarName, flat: &Model) -> Option<VarName> {
    if flat.variables.contains_key(name) {
        return Some(name.clone());
    }
    subscript_fallback_chain(name)
        .into_iter()
        .find(|candidate| flat.variables.contains_key(candidate))
}

/// Mark internal inputs that are connected to discrete/event-driven variables.
///
/// This propagates discrete partition membership across connection alias sets so
/// clocked Real inputs (without explicit `discrete` variability) don't remain as
/// orphan continuous algebraics.
fn find_discrete_connected_internal_inputs(
    flat: &Model,
    when_only_vars: &HashSet<VarName>,
) -> HashSet<VarName> {
    use std::collections::VecDeque;

    let mut adjacency: HashMap<VarName, HashSet<VarName>> = HashMap::new();
    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let mut refs = HashSet::default();
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
            || matches!(var.variability, rumoca_ir_core::Variability::Discrete(_))
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
        .filter(|name| is_internal_input(name, flat))
        .collect()
}

/// Identify inputs that appear in connection equations.
/// These become algebraic variables, not inputs, because the connection
/// equation defines their value from the connected output/state.
fn find_connected_inputs(flat: &Model) -> HashSet<VarName> {
    let mut result = HashSet::default();
    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let mut vars = HashSet::default();
        eq.residual.collect_var_refs(&mut vars);
        result.extend(
            vars.iter()
                .filter_map(|name| resolve_internal_input(name, flat)),
        );
    }
    result
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
) -> Option<VarName> {
    // Both sides must be simple VarRefs (a variable alias equation)
    let Expression::VarRef { name: lhs_name, .. } = lhs else {
        return None;
    };
    let Expression::VarRef { name: rhs_name, .. } = rhs else {
        return None;
    };
    // RHS must be an internal input
    let resolved = resolve_internal_input(rhs_name, flat)?;
    // Skip inputs that already have bindings
    if flat
        .variables
        .get(&resolved)
        .is_some_and(|v| v.binding.is_some())
    {
        return None;
    }
    // LHS must NOT be connected (distinguishes record aliases from connector aliases).
    // Use full fallback-chain matching so multi-layer indexed aliases (e.g.
    // `conn[1].field[2]`) still resolve to their connected base path.
    let lhs_is_connected = flat.variables.get(lhs_name).is_some_and(|v| v.connected)
        || subscript_fallback_chain(lhs_name)
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
    let input_parent = resolved
        .as_str()
        .rsplit_once('.')
        .map(|(p, _)| p)
        .unwrap_or("");
    let eq_component = origin.component_name().unwrap_or_default();
    if !input_parent.is_empty() && input_parent == eq_component {
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
fn find_equation_defined_inputs(flat: &Model) -> HashSet<VarName> {
    let mut result = HashSet::default();
    for eq in flat
        .equations
        .iter()
        .filter(|eq| !eq.origin.is_connection())
    {
        let Expression::Binary { op, lhs, rhs } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
            continue;
        }
        // Check LHS for internal input VarRef (always valid)
        if let Expression::VarRef { name, .. } = lhs.as_ref()
            && let Some(resolved) = resolve_internal_input(name, flat)
        {
            result.insert(resolved);
        }
        // Check RHS for internal input VarRef in intra-component alias equations.
        // Skip inputs with bindings (already promoted via binding check, and
        // adding them to connected_inputs would suppress their binding equation).
        if let Some(resolved) = check_rhs_intra_component_alias(lhs, rhs, &eq.origin, flat) {
            result.insert(resolved);
        }
    }
    result
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
    let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    BuiltinFunction::from_name(short_name).is_some()
        || BuiltinFunction::from_name(&short_name.to_ascii_lowercase()).is_some()
        || is_runtime_intrinsic_function_short_name(short_name)
}

fn resolve_flat_function<'a>(name: &VarName, flat: &'a Model) -> Option<&'a Function> {
    // Strict lookup only: function calls must already be fully resolved during
    // compile/lower phases. No suffix/name heuristics here.
    flat.functions.get(name)
}

fn validate_function_call_name(name: &VarName, flat: &Model, span: Span) -> Result<(), ToDaeError> {
    if is_builtin_or_runtime_intrinsic_function(name) {
        return Ok(());
    }

    let Some(func) = resolve_flat_function(name, flat) else {
        return Err(ToDaeError::unresolved_function_call(name.as_str(), span));
    };

    if func.external.is_none() && func.body.is_empty() {
        let short_name = func
            .name
            .as_str()
            .rsplit('.')
            .next()
            .unwrap_or(func.name.as_str());
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
    reachable_calls: &mut Vec<VarName>,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
    } = base
    {
        for arg in args {
            validate_flat_expression_functions(arg, flat, span, reachable_calls)?;
        }

        // Complex constructors commonly project `.re`/`.im` without explicit
        // constructor signatures in flat.functions.
        if matches!(field, "re" | "im") {
            return Ok(());
        }

        let projected_name = if args.is_empty() {
            format!("{}.{}", name.as_str(), field)
        } else {
            format!(
                "{}.{} (constructor_args={})",
                name.as_str(),
                field,
                args.len()
            )
        };
        let Some(constructor) = resolve_flat_function(name, flat) else {
            if std::env::var("RUMOCA_DEBUG_TODAE").is_ok() {
                let short_name = name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .unwrap_or(name.as_str())
                    .to_string();
                let total_functions = flat.functions.len();
                eprintln!(
                    "DEBUG TODAE missing constructor={} field={} short_name={} total_functions={}",
                    name.as_str(),
                    field,
                    short_name,
                    total_functions
                );
            }
            return Err(ToDaeError::constructor_field_projection_unresolved(
                projected_name,
                span,
            ));
        };

        let field_known = constructor.inputs.iter().any(|param| param.name == field)
            || constructor.outputs.iter().any(|param| param.name == field);
        if !field_known {
            if std::env::var("RUMOCA_DEBUG_TODAE").is_ok() {
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
                eprintln!(
                    "DEBUG TODAE constructor field missing={} available={available_fields:?}",
                    projected_name
                );
            }
            return Err(ToDaeError::constructor_field_projection_unresolved(
                projected_name,
                span,
            ));
        }
        return Ok(());
    }

    validate_flat_expression_functions(base, flat, span, reachable_calls)
}

fn validate_flat_expression_functions(
    expr: &Expression,
    flat: &Model,
    span: Span,
    reachable_calls: &mut Vec<VarName>,
) -> Result<(), ToDaeError> {
    match expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            if !is_constructor {
                validate_function_call_name(name, flat, span)?;
                if !is_builtin_or_runtime_intrinsic_function(name) {
                    reachable_calls.push(name.clone());
                }
            }
            for arg in args {
                validate_flat_expression_functions(arg, flat, span, reachable_calls)?;
            }
        }
        Expression::BuiltinCall { args, .. } => {
            for arg in args {
                validate_flat_expression_functions(arg, flat, span, reachable_calls)?;
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            validate_flat_expression_functions(lhs, flat, span, reachable_calls)?;
            validate_flat_expression_functions(rhs, flat, span, reachable_calls)?;
        }
        Expression::Unary { rhs, .. } => {
            validate_flat_expression_functions(rhs, flat, span, reachable_calls)?
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                validate_flat_expression_functions(cond, flat, span, reachable_calls)?;
                validate_flat_expression_functions(value, flat, span, reachable_calls)?;
            }
            validate_flat_expression_functions(else_branch, flat, span, reachable_calls)?;
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                validate_flat_expression_functions(element, flat, span, reachable_calls)?;
            }
        }
        Expression::Range { start, step, end } => {
            validate_flat_expression_functions(start, flat, span, reachable_calls)?;
            if let Some(step) = step {
                validate_flat_expression_functions(step, flat, span, reachable_calls)?;
            }
            validate_flat_expression_functions(end, flat, span, reachable_calls)?;
        }
        Expression::Index { base, subscripts } => {
            validate_flat_expression_functions(base, flat, span, reachable_calls)?;
            for subscript in subscripts {
                if let Subscript::Expr(expr) = subscript {
                    validate_flat_expression_functions(expr, flat, span, reachable_calls)?;
                }
            }
        }
        Expression::FieldAccess { base, field } => {
            validate_field_access_functions(base, field, flat, span, reachable_calls)?;
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            validate_flat_expression_functions(expr, flat, span, reachable_calls)?;
            for index in indices {
                validate_flat_expression_functions(&index.range, flat, span, reachable_calls)?;
            }
            if let Some(filter_expr) = filter {
                validate_flat_expression_functions(filter_expr, flat, span, reachable_calls)?;
            }
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
    Ok(())
}

fn component_reference_to_var_name(comp: &ComponentReference) -> VarName {
    comp.to_var_name()
}

fn validate_statement_functions(
    stmt: &Statement,
    flat: &Model,
    span: Span,
    reachable_calls: &mut Vec<VarName>,
) -> Result<(), ToDaeError> {
    match stmt {
        Statement::Assignment { value, .. } => {
            validate_flat_expression_functions(value, flat, span, reachable_calls)?
        }
        Statement::For { indices, equations } => {
            for index in indices {
                validate_flat_expression_functions(&index.range, flat, span, reachable_calls)?;
            }
            for nested in equations {
                validate_statement_functions(nested, flat, span, reachable_calls)?;
            }
        }
        Statement::While(block) => {
            validate_flat_expression_functions(&block.cond, flat, span, reachable_calls)?;
            for nested in &block.stmts {
                validate_statement_functions(nested, flat, span, reachable_calls)?;
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                validate_flat_expression_functions(&block.cond, flat, span, reachable_calls)?;
                for nested in &block.stmts {
                    validate_statement_functions(nested, flat, span, reachable_calls)?;
                }
            }
            if let Some(else_block) = else_block {
                for nested in else_block {
                    validate_statement_functions(nested, flat, span, reachable_calls)?;
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                validate_flat_expression_functions(&block.cond, flat, span, reachable_calls)?;
                for nested in &block.stmts {
                    validate_statement_functions(nested, flat, span, reachable_calls)?;
                }
            }
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            let name = component_reference_to_var_name(comp);
            validate_function_call_name(&name, flat, span)?;
            if !is_builtin_or_runtime_intrinsic_function(&name) {
                reachable_calls.push(name);
            }
            for arg in args {
                validate_flat_expression_functions(arg, flat, span, reachable_calls)?;
            }
            for output in outputs {
                validate_flat_expression_functions(output, flat, span, reachable_calls)?;
            }
        }
        Statement::Reinit { value, .. } => {
            validate_flat_expression_functions(value, flat, span, reachable_calls)?
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            validate_flat_expression_functions(condition, flat, span, reachable_calls)?;
            validate_flat_expression_functions(message, flat, span, reachable_calls)?;
            if let Some(level) = level {
                validate_flat_expression_functions(level, flat, span, reachable_calls)?;
            }
        }
        Statement::Empty | Statement::Return | Statement::Break => {}
    }
    Ok(())
}

fn validate_when_equation_functions(
    equation: &rumoca_ir_flat::WhenEquation,
    flat: &Model,
    reachable_calls: &mut Vec<VarName>,
) -> Result<(), ToDaeError> {
    match equation {
        rumoca_ir_flat::WhenEquation::Assign { value, span, .. } => {
            validate_flat_expression_functions(value, flat, *span, reachable_calls)?
        }
        rumoca_ir_flat::WhenEquation::Reinit { value, span, .. } => {
            validate_flat_expression_functions(value, flat, *span, reachable_calls)?
        }
        rumoca_ir_flat::WhenEquation::Assert {
            condition, span, ..
        } => validate_flat_expression_functions(condition, flat, *span, reachable_calls)?,
        rumoca_ir_flat::WhenEquation::Terminate { .. } => {}
        rumoca_ir_flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (condition, equations) in branches {
                validate_flat_expression_functions(condition, flat, Span::DUMMY, reachable_calls)?;
                for nested in equations {
                    validate_when_equation_functions(nested, flat, reachable_calls)?;
                }
            }
            for nested in else_branch {
                validate_when_equation_functions(nested, flat, reachable_calls)?;
            }
        }
        rumoca_ir_flat::WhenEquation::FunctionCallOutputs { function, span, .. } => {
            validate_flat_expression_functions(function, flat, *span, reachable_calls)?
        }
    }
    Ok(())
}

fn validate_flat_function_calls(flat: &Model) -> Result<(), ToDaeError> {
    let mut reachable_calls: Vec<VarName> = Vec::new();

    for variable in flat.variables.values() {
        let is_param_or_const = matches!(
            variable.variability,
            rumoca_ir_flat::Variability::Parameter(_) | rumoca_ir_flat::Variability::Constant(_)
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
            validate_flat_expression_functions(expr, flat, Span::DUMMY, &mut reachable_calls)?;
        }
    }

    for equation in flat.equations.iter().chain(flat.initial_equations.iter()) {
        validate_flat_expression_functions(
            &equation.residual,
            flat,
            equation.span,
            &mut reachable_calls,
        )?;
    }

    for assertion in flat
        .assert_equations
        .iter()
        .chain(flat.initial_assert_equations.iter())
    {
        validate_flat_expression_functions(
            &assertion.condition,
            flat,
            assertion.span,
            &mut reachable_calls,
        )?;
        validate_flat_expression_functions(
            &assertion.message,
            flat,
            assertion.span,
            &mut reachable_calls,
        )?;
        if let Some(level) = &assertion.level {
            validate_flat_expression_functions(level, flat, assertion.span, &mut reachable_calls)?;
        }
    }

    for when in &flat.when_clauses {
        validate_flat_expression_functions(&when.condition, flat, when.span, &mut reachable_calls)?;
        for equation in &when.equations {
            validate_when_equation_functions(equation, flat, &mut reachable_calls)?;
        }
    }

    for algorithm in flat.algorithms.iter().chain(flat.initial_algorithms.iter()) {
        for statement in &algorithm.statements {
            validate_statement_functions(statement, flat, algorithm.span, &mut reachable_calls)?;
        }
    }

    let mut visited: HashSet<VarName> = HashSet::default();
    while let Some(function_name) = reachable_calls.pop() {
        if !visited.insert(function_name.clone()) {
            continue;
        }
        let Some(function) = resolve_flat_function(&function_name, flat) else {
            return Err(ToDaeError::unresolved_function_call(
                function_name.as_str(),
                Span::DUMMY,
            ));
        };
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
                    &mut reachable_calls,
                )?;
            }
        }
        for statement in &function.body {
            validate_statement_functions(statement, flat, function.span, &mut reachable_calls)?;
        }
    }

    Ok(())
}

/// Takes a reference to avoid cloning the Model when the caller also needs it.
pub fn to_dae(flat: &Model) -> Result<Dae, ToDaeError> {
    to_dae_with_options(flat, ToDaeOptions::default())
}

/// Convert a Model to DAE with configurable strictness.
pub fn to_dae_with_options(flat: &Model, options: ToDaeOptions) -> Result<Dae, ToDaeError> {
    let mut dae = Dae::new();

    // Fail fast on unresolved or non-executable function calls so unsupported
    // evaluation paths don't leak into simulation.
    validate_flat_function_calls(flat)?;

    // MLS §4.7: Propagate partial status and class type for balance checking
    dae.is_partial = flat.is_partial;
    dae.class_type = flat.class_type.clone();
    dae.model_description = flat.model_description.clone();

    // Build prefix-to-children index once for O(1) record field lookups
    let prefix_children = build_prefix_children(flat);

    // First pass: identify state variables, connected inputs, and when-only vars
    let der_vars = classification::find_state_variables(flat);
    let state_vars = filter_state_variables(der_vars, flat);
    let mut connected_inputs = find_connected_inputs(flat);
    // Also promote inputs defined by model equations (e.g., `medium.p = p`)
    let eq_defined = find_equation_defined_inputs(flat);
    connected_inputs.extend(eq_defined);
    let connector_input_members =
        connector_input_analysis::find_top_level_connector_input_members(flat, &state_vars);
    let when_only_vars = find_when_only_vars(flat, &prefix_children);
    let discrete_connected_inputs = find_discrete_connected_internal_inputs(flat, &when_only_vars);

    // Derivative alias detection is intentionally disabled. Equations like `a = der(w)`
    // define both the state derivative AND the algebraic variable `a`. The proper solution
    // is to let the backend handle this: in codegen, equations of the form `var = der(state)`
    // become direct assignments (e.g., `a = ode[idx_w]`) and the balance checker counts
    // them as ODE equations.
    let derivative_aliases = HashSet::default();

    // Second pass: classify all variables
    let classification_inputs = ClassificationInputs {
        prefix_children: &prefix_children,
        state_vars: &state_vars,
        connected_inputs: &connected_inputs,
        discrete_connected_inputs: &discrete_connected_inputs,
        connector_input_members: &connector_input_members,
        when_only_vars: &when_only_vars,
        derivative_aliases: &derivative_aliases,
    };
    classify_variables(&mut dae, flat, &classification_inputs);

    // Collect variables defined by algorithm outputs (including record field expansion)
    // Per MLS §11.1, algorithm sections contribute equations for assigned variables.
    // We need to skip binding equations for these variables to avoid double-counting.
    let algorithm_defined_vars =
        definition_analysis::collect_algorithm_defined_vars(flat, &prefix_children);

    // Collect record fields that are already defined by record-level equations.
    // When `cc = func(...)` defines all fields of record `cc`, and `cc.m_capgd`
    // also has a binding, skip the binding to avoid double-counting.
    let record_eq_defined_vars =
        definition_analysis::collect_record_equation_defined_vars(flat, &prefix_children);

    // Third pass: convert variable bindings to equations (MLS §4.4.1)
    convert_bindings_to_equations(
        &mut dae,
        flat,
        &state_vars,
        &connected_inputs,
        &algorithm_defined_vars,
        &record_eq_defined_vars,
    );

    // Build prefix count map for efficient scalar count inference
    let prefix_counts = build_prefix_counts(flat);

    // Fourth pass: classify equations
    classify_equations(&mut dae, flat, &prefix_counts);

    // Process initial equations
    initial::convert_initial_equations(&mut dae, flat, &prefix_counts, infer_equation_scalar_count);

    // Process when clauses into discrete update sets.
    for when in &flat.when_clauses {
        let dae_when = convert_when_clause(when, &state_vars, flat)?;
        route_discrete_event_equations(&mut dae, &dae_when);
    }

    // Process model/initial algorithms strictly through equation lowering.
    lower_algorithms_to_equations(&mut dae, flat)?;
    canonicalize_discrete_assignment_equations(&mut dae);
    populate_canonical_conditions(&mut dae, flat);
    // MLS §3.7.5: Lower pre() operator calls to dedicated parameter symbols.
    // This must run after equation construction but before parameter sorting,
    // so that the new __pre__ parameters are included in dependency ordering.
    pre_lowering::lower_pre_operator(&mut dae);

    dae.functions = flat_to_dae_function_map(&flat.functions);
    dae.enum_literal_ordinals = flat.enum_literal_ordinals.clone();

    // Sort parameters so that start-value dependencies are satisfied in order.
    // If parameter A's start expression references parameter B, B must appear
    // before A so that code generators can evaluate start values sequentially.
    sort_parameters_by_start_dependency(&mut dae);

    populate_runtime_precompute(&mut dae)?;
    appendix_b_validation::validate_appendix_b_invariants(&dae)?;

    // MLS §4.7: Count interface flow variables (flows in top-level connectors)
    // These count toward the equation size, not as unknowns, because they'll
    // receive their defining equations from external connections.
    dae.interface_flow_count = count_interface_flows(flat);

    // MLS §9.4: Propagate break edge excess from flatten phase.
    dae.oc_break_edge_scalar_count = flat.oc_break_edge_scalar_count;

    // MLS §4.8: Count overconstrained connector interface contributions.
    // Uses all OC vars for connectivity, filtered to components with top-level vars.
    // Positive = root equations needed, negative = redundant equations (cycles).
    let oc_correction = count_overconstrained_interface(flat, &state_vars);
    if oc_correction >= 0 {
        dae.overconstrained_interface_count = oc_correction;
    } else {
        dae.overconstrained_interface_count = 0;
        dae.oc_break_edge_scalar_count += (-oc_correction) as usize;
    }

    // Final compile-phase safety check on the generated DAE catches unresolved
    // constructor field projections introduced during ToDae conversion.
    validate_dae_constructor_field_projections(&dae)?;
    let known_flat_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    validate_dae_references(&dae, &known_flat_var_names)?;

    if options.error_on_unbalanced
        && !dae.is_partial
        && rumoca_eval_dae::analysis::balance(&dae) != 0
    {
        let (equations, unknowns) = balance_counting::compute_balance_counts(&dae);
        return Err(ToDaeError::unbalanced(equations, unknowns));
    }

    Ok(dae)
}

/// Determine if an algebraic variable should be stored in discretes or derivative_aliases.
/// Returns Some(map_name) if not a regular algebraic, None if it's a regular algebraic.
enum AlgebraicCategory {
    Discrete,
    DerivativeAlias,
    Regular,
}

fn categorize_algebraic(
    name: &VarName,
    var: &flat::Variable,
    when_only_vars: &HashSet<VarName>,
    derivative_aliases: &HashSet<VarName>,
) -> AlgebraicCategory {
    // When-only vars or unused expandable connector members are discrete (MLS §9.1.3)
    let is_unused_expandable =
        var.from_expandable_connector && !var.connected && var.binding.is_none();
    if is_when_only_var(name, when_only_vars) || is_unused_expandable {
        AlgebraicCategory::Discrete
    } else if derivative_aliases.contains(name) {
        AlgebraicCategory::DerivativeAlias
    } else {
        AlgebraicCategory::Regular
    }
}

fn is_when_only_var(name: &VarName, when_only_vars: &HashSet<VarName>) -> bool {
    when_only_vars.contains(name)
        || subscript_fallback_chain(name)
            .into_iter()
            .any(|candidate| when_only_vars.contains(&candidate))
}

fn has_clocked_or_event_binding(var: &flat::Variable) -> bool {
    var.binding
        .as_ref()
        .is_some_and(discrete_partition::expression_contains_clocked_or_event_operators)
}

/// Classify all variables from Model into DAE categories.
struct ClassificationInputs<'a> {
    prefix_children: &'a FxHashMap<String, Vec<VarName>>,
    state_vars: &'a HashSet<VarName>,
    connected_inputs: &'a HashSet<VarName>,
    discrete_connected_inputs: &'a HashSet<VarName>,
    connector_input_members: &'a HashSet<VarName>,
    when_only_vars: &'a HashSet<VarName>,
    derivative_aliases: &'a HashSet<VarName>,
}

fn classify_variables(dae: &mut Dae, flat: &Model, inputs: &ClassificationInputs<'_>) {
    let known_var_names: HashSet<String> = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    for (name, var) in &flat.variables {
        // Skip non-primitive aggregate variables whose primitive fields are
        // represented separately (MLS §4.8). Keep non-primitive leaves so
        // connector-typed scalar aliases remain available in the DAE.
        if !var.is_primitive && inputs.prefix_children.contains_key(name.as_str()) {
            continue;
        }

        let kind = classification::classify_variable(var, inputs.state_vars);
        let mut dae_var = create_dae_variable(name, var, &known_var_names);
        inherit_scalarized_start_from_base(name, flat, &mut dae_var, &known_var_names);

        // Top-level connector members connected only to internal inputs act as
        // external values and should remain inputs (not algebraic unknowns).
        if inputs.connector_input_members.contains(name) {
            dae.inputs.insert(flat_to_dae_var_name(name), dae_var);
            continue;
        }

        match kind {
            VariableKind::State => {
                dae.states.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Algebraic => {
                if has_clocked_or_event_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                let category = categorize_algebraic(
                    name,
                    var,
                    inputs.when_only_vars,
                    inputs.derivative_aliases,
                );
                match category {
                    AlgebraicCategory::Discrete => {
                        insert_discrete_var(dae, name, dae_var, var);
                    }
                    AlgebraicCategory::DerivativeAlias => {
                        dae.derivative_aliases
                            .insert(flat_to_dae_var_name(name), dae_var);
                    }
                    AlgebraicCategory::Regular => {
                        dae.algebraics.insert(flat_to_dae_var_name(name), dae_var);
                    }
                };
            }
            VariableKind::Input => {
                if has_clocked_or_event_binding(var) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                if is_when_only_var(name, inputs.when_only_vars) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                if inputs.discrete_connected_inputs.contains(name) {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                // Internal inputs that appear in der() are local dynamic unknowns.
                // External interface inputs remain inputs (handled below).
                if inputs.state_vars.contains(name) && is_internal_input(name, flat) {
                    dae.states.insert(flat_to_dae_var_name(name), dae_var);
                    continue;
                }
                // Connected inputs or inputs with bindings become algebraic (MLS §4.4.1)
                if !(inputs.connected_inputs.contains(name) || var.binding.is_some()) {
                    dae.inputs.insert(flat_to_dae_var_name(name), dae_var);
                    continue;
                }

                // Discrete-valued inputs (Boolean/Integer/enum) must stay in
                // the event-driven partition. Promoting them to continuous
                // algebraics creates false balance deficits because their
                // defining equations are routed to f_m/f_z (MLS Appendix B).
                let is_discrete_input = var.is_discrete_type
                    || matches!(var.variability, rumoca_ir_core::Variability::Discrete(_));
                if is_discrete_input {
                    insert_discrete_var(dae, name, dae_var, var);
                    continue;
                }
                dae.algebraics.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Output => {
                if is_when_only_var(name, inputs.when_only_vars)
                    || has_clocked_or_event_binding(var)
                {
                    insert_discrete_var(dae, name, dae_var, var);
                } else {
                    dae.outputs.insert(flat_to_dae_var_name(name), dae_var);
                }
            }
            VariableKind::Parameter => {
                dae.parameters.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Constant => {
                dae.constants.insert(flat_to_dae_var_name(name), dae_var);
            }
            VariableKind::Discrete => {
                insert_discrete_var(dae, name, dae_var, var);
            }
            VariableKind::Derivative => {} // Implicit, not stored
        }
    }
}

fn inherit_scalarized_start_from_base(
    name: &VarName,
    flat: &Model,
    dae_var: &mut Variable,
    known_var_names: &HashSet<String>,
) {
    if dae_var.start.is_some() {
        return;
    }

    let Some(base_name) = flat::component_base_name(name.as_str()) else {
        return;
    };
    if base_name == name.as_str() {
        return;
    }

    let base_var_name = VarName::new(base_name.clone());
    let Some(base_var) = flat.variables.get(&base_var_name) else {
        return;
    };
    let Some(base_start) = base_var.start.as_ref() else {
        return;
    };

    if matches!(
        base_start,
        Expression::Array { .. } | Expression::Tuple { .. }
    ) {
        return;
    }

    let projected_start = project_scalarized_start_expr(base_start, &base_name, name.as_str());
    dae_var.start = Some(flat_to_dae_expression(&rewrite_start_expr_missing_refs(
        &projected_start,
        known_var_names,
    )));
    if dae_var.fixed.is_none() {
        dae_var.fixed = base_var.fixed;
    }
}

fn project_scalarized_start_expr(
    base_start: &Expression,
    base_name: &str,
    scalar_name: &str,
) -> Expression {
    let Some(suffix) = scalar_name.strip_prefix(base_name) else {
        return base_start.clone();
    };
    if !suffix.starts_with('.') {
        return base_start.clone();
    }

    match base_start {
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => Expression::VarRef {
            name: VarName::new(format!("{}{}", name.as_str(), suffix)),
            subscripts: vec![],
        },
        _ => base_start.clone(),
    }
}

/// Insert a variable into the appropriate discrete map (discrete_reals or discrete_valued).
///
/// Per MLS B.1, discrete Real variables (z) go to `discrete_reals`, while
/// Boolean/Integer/enum variables (m) go to `discrete_valued`.
/// Flat IR carries `is_discrete_type`, which identifies Integer/Boolean/enum
/// variables (MLS §4.5). Those are routed to `m`; other discrete variables
/// remain in `z`.
fn insert_discrete_var(
    dae: &mut Dae,
    name: &VarName,
    dae_var: dae::Variable,
    var: &flat::Variable,
) {
    if var.is_discrete_type {
        dae.discrete_valued
            .insert(flat_to_dae_var_name(name), dae_var);
    } else {
        dae.discrete_reals
            .insert(flat_to_dae_var_name(name), dae_var);
    }
}
fn convert_bindings_to_equations(
    dae: &mut Dae,
    flat: &Model,
    state_vars: &HashSet<VarName>,
    connected_inputs: &HashSet<VarName>,
    algorithm_defined_vars: &HashSet<VarName>,
    record_eq_defined_vars: &HashSet<VarName>,
) {
    binding_conversion::convert_bindings_to_equations(
        dae,
        flat,
        state_vars,
        connected_inputs,
        algorithm_defined_vars,
        record_eq_defined_vars,
    );
}

// =============================================================================
// Record function parameter decomposition (DAE level)
// =============================================================================

/// Known record types and their field names for decomposition.
const KNOWN_RECORD_FIELDS: &[(&str, &[&str])] = &[
    ("Complex", &["re", "im"]),
    ("Modelica.Units.SI.ComplexVoltage", &["re", "im"]),
    ("Modelica.Units.SI.ComplexCurrent", &["re", "im"]),
];

fn dae_record_fields(type_name: &str) -> Option<&'static [&'static str]> {
    for &(name, fields) in KNOWN_RECORD_FIELDS {
        if type_name == name || type_name.ends_with(&format!(".{name}")) {
            return Some(fields);
        }
    }
    None
}

/// Decompose record-typed function params in the DAE. This rewrites function
/// signatures (replacing `c: Complex` with `c_re, c_im: Real`) and decomposes
/// call-site arguments throughout all DAE equations.
/// Decompose record-typed function params in the DAE for code generators.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn lower_record_function_params_dae(dae: &mut Dae) {
    // Identify functions with record params and rewrite their signatures.
    let mut decomp_map: HashMap<String, Vec<(usize, Vec<&'static str>)>> = HashMap::new();

    for (func_name, func) in dae.functions.iter_mut() {
        let mut decomposed: Vec<(usize, String, Vec<&'static str>)> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some(fields) = dae_record_fields(&input.type_name) {
                decomposed.push((idx, input.name.clone(), fields.to_vec()));
            }
        }
        if decomposed.is_empty() {
            continue;
        }

        // Replace record inputs with scalar field inputs
        let old_inputs = std::mem::take(&mut func.inputs);
        for (idx, input) in old_inputs.into_iter().enumerate() {
            let Some((_, param_name, fields)) = decomposed.iter().find(|(i, _, _)| *i == idx)
            else {
                func.inputs.push(input);
                continue;
            };
            for field in fields {
                func.inputs.push(dae::FunctionParam {
                    name: format!("{param_name}_{field}"),
                    type_name: "Real".to_string(),
                    dims: vec![],
                    default: None,
                    description: None,
                });
            }
        }

        let entry: Vec<(usize, Vec<&str>)> = decomposed
            .iter()
            .map(|(idx, _, fields)| (*idx, fields.clone()))
            .collect();
        decomp_map.insert(func_name.as_str().to_string(), entry);
    }

    if decomp_map.is_empty() {
        return;
    }

    // Rewrite call sites in all DAE equations and function bodies.
    for eq in &mut dae.f_x {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.f_z {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.f_m {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.f_c {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.initial_equations {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for func in dae.functions.values_mut() {
        for stmt in &mut func.body {
            decompose_record_args_dae_stmt(stmt, &decomp_map);
        }
    }
}

fn decompose_record_args_dae_stmt(
    stmt: &mut dae::Statement,
    map: &HashMap<String, Vec<(usize, Vec<&str>)>>,
) {
    match stmt {
        dae::Statement::Assignment { value, .. } => {
            decompose_record_args_dae_expr(value, map);
        }
        dae::Statement::For { indices, equations } => {
            for idx in indices {
                decompose_record_args_dae_expr(&mut idx.range, map);
            }
            for inner in equations {
                decompose_record_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::While(block) => {
            decompose_record_args_dae_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                decompose_record_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                decompose_record_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_args_dae_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    decompose_record_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args.iter_mut() {
                decompose_record_args_dae_expr(arg, map);
            }
            for out in outputs {
                decompose_record_args_dae_expr(out, map);
            }
        }
        dae::Statement::Reinit { value, .. } => {
            decompose_record_args_dae_expr(value, map);
        }
        dae::Statement::Assert {
            condition,
            message,
            level,
        } => {
            decompose_record_args_dae_expr(condition, map);
            decompose_record_args_dae_expr(message, map);
            if let Some(l) = level {
                decompose_record_args_dae_expr(l, map);
            }
        }
        dae::Statement::When(blocks) => {
            for block in blocks {
                decompose_record_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::Empty | dae::Statement::Return | dae::Statement::Break => {}
    }
}

fn decompose_record_args_dae_expr(
    expr: &mut dae::Expression,
    map: &HashMap<String, Vec<(usize, Vec<&str>)>>,
) {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                decompose_record_args_dae_expr(arg, map);
            }
            let Some(decomposed) = map.get(name.as_str()) else {
                return;
            };
            let old_args = std::mem::take(args);
            let mut old_idx = 0;
            for (param_idx, fields) in decomposed {
                while old_idx < *param_idx && old_idx < old_args.len() {
                    args.push(old_args[old_idx].clone());
                    old_idx += 1;
                }
                if old_idx < old_args.len() {
                    expand_dae_record_arg(&old_args[old_idx], fields, args);
                    old_idx += 1;
                }
            }
            while old_idx < old_args.len() {
                args.push(old_args[old_idx].clone());
                old_idx += 1;
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            decompose_record_args_dae_expr(lhs, map);
            decompose_record_args_dae_expr(rhs, map);
        }
        dae::Expression::Unary { rhs, .. } => {
            decompose_record_args_dae_expr(rhs, map);
        }
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                decompose_record_args_dae_expr(arg, map);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (c, t) in branches {
                decompose_record_args_dae_expr(c, map);
                decompose_record_args_dae_expr(t, map);
            }
            decompose_record_args_dae_expr(else_branch, map);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for e in elements {
                decompose_record_args_dae_expr(e, map);
            }
        }
        dae::Expression::Range { start, step, end } => {
            decompose_record_args_dae_expr(start, map);
            if let Some(s) = step {
                decompose_record_args_dae_expr(s, map);
            }
            decompose_record_args_dae_expr(end, map);
        }
        dae::Expression::Index { base, .. } | dae::Expression::FieldAccess { base, .. } => {
            decompose_record_args_dae_expr(base, map);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            decompose_record_args_dae_expr(expr, map);
            for idx in indices {
                decompose_record_args_dae_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                decompose_record_args_dae_expr(f, map);
            }
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn named_constructor_arg_dae<'a>(
    ctor_args: &'a [dae::Expression],
    field: &str,
) -> Option<&'a dae::Expression> {
    for arg in ctor_args {
        if let dae::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn expand_dae_record_arg(arg: &dae::Expression, fields: &[&str], out: &mut Vec<dae::Expression>) {
    // Constructor call → extract positional/named args
    if let dae::Expression::FunctionCall {
        args: ctor_args,
        is_constructor: true,
        ..
    } = arg
    {
        let positional: Vec<&dae::Expression> = ctor_args
            .iter()
            .filter(|a| {
                !matches!(a, dae::Expression::FunctionCall { is_constructor: true, name, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
            })
            .collect();

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg_dae(ctor_args, field);
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else {
                out.push(dae::Expression::Literal(dae::Literal::Real(0.0)));
            }
        }
        return;
    }

    // Variable reference → emit field VarRefs
    if let dae::Expression::VarRef { name, .. } = arg {
        for field in fields {
            out.push(dae::Expression::VarRef {
                name: dae::VarName::new(format!("{}.{}", name.as_str(), field)),
                subscripts: vec![],
            });
        }
        return;
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(dae::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.to_string(),
        });
    }
}

/// Insert size arguments for variable-size array params at DAE call sites.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn insert_array_size_args_dae(dae: &mut Dae) {
    let array_param_map: HashMap<String, Vec<usize>> = dae
        .functions
        .iter()
        .filter_map(|(name, func)| {
            let indices: Vec<usize> = func
                .inputs
                .iter()
                .enumerate()
                .filter(|(_, p)| !p.dims.is_empty())
                .map(|(i, _)| i)
                .collect();
            if indices.is_empty() {
                None
            } else {
                Some((name.as_str().to_string(), indices))
            }
        })
        .collect();

    if array_param_map.is_empty() {
        return;
    }

    for eq in &mut dae.f_x {
        insert_size_args_dae_expr(&mut eq.rhs, &array_param_map);
    }
    for eq in &mut dae.f_z {
        insert_size_args_dae_expr(&mut eq.rhs, &array_param_map);
    }
    for eq in &mut dae.f_m {
        insert_size_args_dae_expr(&mut eq.rhs, &array_param_map);
    }
    for func in dae.functions.values_mut() {
        for stmt in &mut func.body {
            insert_size_args_dae_stmt(stmt, &array_param_map);
        }
    }
}

fn insert_size_args_dae_stmt(stmt: &mut dae::Statement, map: &HashMap<String, Vec<usize>>) {
    match stmt {
        dae::Statement::Assignment { value, .. } => insert_size_args_dae_expr(value, map),
        dae::Statement::For { indices, equations } => {
            for idx in indices {
                insert_size_args_dae_expr(&mut idx.range, map);
            }
            for inner in equations {
                insert_size_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::While(block) => {
            insert_size_args_dae_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                insert_size_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                insert_size_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_dae_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    insert_size_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_dae_expr(arg, map);
            }
            for out in outputs {
                insert_size_args_dae_expr(out, map);
            }
        }
        dae::Statement::Reinit { value, .. } => insert_size_args_dae_expr(value, map),
        dae::Statement::Assert {
            condition,
            message,
            level,
        } => {
            insert_size_args_dae_expr(condition, map);
            insert_size_args_dae_expr(message, map);
            if let Some(l) = level {
                insert_size_args_dae_expr(l, map);
            }
        }
        dae::Statement::When(blocks) => {
            for block in blocks {
                insert_size_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::Empty | dae::Statement::Return | dae::Statement::Break => {}
    }
}

fn insert_size_args_dae_expr(expr: &mut dae::Expression, map: &HashMap<String, Vec<usize>>) {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_dae_expr(arg, map);
            }
            let Some(array_indices) = map.get(name.as_str()) else {
                return;
            };
            let mut new_args = std::mem::take(args);
            for &param_idx in array_indices.iter().rev() {
                if param_idx < new_args.len() {
                    let size_expr = dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Size,
                        args: vec![
                            new_args[param_idx].clone(),
                            dae::Expression::Literal(dae::Literal::Integer(1)),
                        ],
                    };
                    new_args.insert(param_idx + 1, size_expr);
                }
            }
            *args = new_args;
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            insert_size_args_dae_expr(lhs, map);
            insert_size_args_dae_expr(rhs, map);
        }
        dae::Expression::Unary { rhs, .. } => insert_size_args_dae_expr(rhs, map),
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                insert_size_args_dae_expr(arg, map);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (c, t) in branches {
                insert_size_args_dae_expr(c, map);
                insert_size_args_dae_expr(t, map);
            }
            insert_size_args_dae_expr(else_branch, map);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for e in elements {
                insert_size_args_dae_expr(e, map);
            }
        }
        dae::Expression::Range { start, step, end } => {
            insert_size_args_dae_expr(start, map);
            if let Some(s) = step {
                insert_size_args_dae_expr(s, map);
            }
            insert_size_args_dae_expr(end, map);
        }
        dae::Expression::Index { base, .. } | dae::Expression::FieldAccess { base, .. } => {
            insert_size_args_dae_expr(base, map);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            insert_size_args_dae_expr(expr, map);
            for idx in indices {
                insert_size_args_dae_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                insert_size_args_dae_expr(f, map);
            }
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

// =============================================================================

/// Topologically sort parameters so that start-value dependencies are ordered.
///
/// If parameter A's `start` expression references parameter B, then B must
/// appear before A in the parameter map. This ensures code generators that
/// evaluate start values sequentially produce correct numeric results.
///
/// Falls back to the original order if cycles are detected.
fn sort_parameters_by_start_dependency(dae: &mut Dae) {
    let param_names: IndexSet<dae::VarName> = dae.parameters.keys().cloned().collect();
    if param_names.len() <= 1 {
        return;
    }

    // Build adjacency: param → set of params its start expression depends on
    let mut deps: HashMap<usize, Vec<usize>> = HashMap::new();
    for (idx, (_, var)) in dae.parameters.iter().enumerate() {
        let Some(ref start_expr) = var.start else {
            continue;
        };
        for ref_name in collect_expression_var_refs(start_expr) {
            let Some(dep_idx) = param_names.get_index_of(&ref_name) else {
                continue;
            };
            if dep_idx != idx {
                deps.entry(idx).or_default().push(dep_idx);
            }
        }
    }

    // If no dependencies exist, nothing to reorder.
    if deps.is_empty() {
        return;
    }

    // Kahn's algorithm for topological sort
    let n = param_names.len();
    let mut in_degree = vec![0usize; n];
    // Build forward edges: if node depends on dep, then dep → node
    let mut forward: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (&node, predecessors) in &deps {
        for &pred in predecessors {
            forward[pred].push(node);
            in_degree[node] += 1;
        }
    }

    let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut sorted_indices = Vec::with_capacity(n);
    while let Some(node) = queue.pop_front() {
        sorted_indices.push(node);
        for &next in &forward[node] {
            in_degree[next] -= 1;
            if in_degree[next] == 0 {
                queue.push_back(next);
            }
        }
    }

    if sorted_indices.len() != n {
        // Cycle detected — keep original order (conservative fallback)
        return;
    }

    // Rebuild the parameters IndexMap in sorted order
    let old_params: Vec<(dae::VarName, dae::Variable)> = dae.parameters.drain(..).collect();
    for &idx in &sorted_indices {
        let (name, var) = old_params[idx].clone();
        dae.parameters.insert(name, var);
    }
}

/// Collect all VarRef names from an expression tree.
fn collect_expression_var_refs(expr: &dae::Expression) -> Vec<dae::VarName> {
    let mut refs = Vec::new();
    collect_var_refs_recursive(expr, &mut refs);
    refs
}

fn collect_var_refs_recursive(expr: &dae::Expression, refs: &mut Vec<dae::VarName>) {
    match expr {
        dae::Expression::VarRef { name, .. } => {
            refs.push(name.clone());
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_var_refs_recursive(lhs, refs);
            collect_var_refs_recursive(rhs, refs);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_var_refs_recursive(rhs, refs);
        }
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                collect_var_refs_recursive(arg, refs);
            }
        }
        dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_refs_recursive(arg, refs);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_var_refs_recursive(cond, refs);
                collect_var_refs_recursive(then_expr, refs);
            }
            collect_var_refs_recursive(else_branch, refs);
        }
        dae::Expression::Array { elements, .. } => {
            for elem in elements {
                collect_var_refs_recursive(elem, refs);
            }
        }
        dae::Expression::Tuple { elements } => {
            for elem in elements {
                collect_var_refs_recursive(elem, refs);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_var_refs_recursive(start, refs);
            if let Some(step_expr) = step {
                collect_var_refs_recursive(step_expr, refs);
            }
            collect_var_refs_recursive(end, refs);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_var_refs_recursive(expr, refs);
            for idx in indices {
                collect_var_refs_recursive(&idx.range, refs);
            }
            if let Some(f) = filter {
                collect_var_refs_recursive(f, refs);
            }
        }
        dae::Expression::Index { base, .. } => {
            collect_var_refs_recursive(base, refs);
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_var_refs_recursive(base, refs);
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

#[cfg(test)]
fn should_keep_connected_input_binding(
    kind: &VariableKind,
    name: &VarName,
    var: &flat::Variable,
    connected_inputs_only_connected_to_inputs: &HashSet<VarName>,
) -> bool {
    binding_conversion::should_keep_connected_input_binding(
        kind,
        name,
        var,
        connected_inputs_only_connected_to_inputs,
    )
}

#[cfg(test)]
fn build_unknown_prefix_children(unknowns: &HashSet<VarName>) -> FxHashMap<String, Vec<VarName>> {
    binding_conversion::build_unknown_prefix_children(unknowns)
}

#[cfg(test)]
fn should_skip_binding_for_explicit_var(
    name: &VarName,
    var: &flat::Variable,
    unknowns: &HashSet<VarName>,
    unknown_prefix_children: &FxHashMap<String, Vec<VarName>>,
) -> bool {
    binding_conversion::should_skip_binding_for_explicit_var(
        name,
        var,
        unknowns,
        unknown_prefix_children,
    )
}

#[cfg(test)]
fn collect_vars_with_unknown_rhs(flat: &Model, unknowns: &HashSet<VarName>) -> HashSet<VarName> {
    binding_conversion::collect_vars_with_unknown_rhs(flat, unknowns)
}

#[cfg(test)]
fn collect_when_statement_targets(
    statements: &[rumoca_ir_flat::Statement],
    targets: &mut HashSet<VarName>,
) {
    when_analysis::collect_when_statement_targets(statements, targets);
}

#[cfg(test)]
fn find_top_level_connector_input_members(
    flat: &Model,
    state_vars: &HashSet<VarName>,
) -> HashSet<VarName> {
    connector_input_analysis::find_top_level_connector_input_members(flat, state_vars)
}

/// Check if a connection equation connects only input variables.
/// Such equations are identity constraints (aliases), not defining equations,
/// and should not be counted in the balance.
#[cfg(test)]
fn is_input_input_connection(eq: &rumoca_ir_flat::Equation, dae: &Dae) -> bool {
    equation_conversion::is_input_input_connection(eq, dae)
}

#[cfg(test)]
fn is_input_default_equation(eq: &rumoca_ir_flat::Equation, flat: &Model, dae: &Dae) -> bool {
    equation_conversion::is_input_default_equation(eq, flat, dae)
}

#[cfg(test)]
fn get_output_in_input_output_connection(
    eq: &rumoca_ir_flat::Equation,
    dae: &Dae,
) -> Option<VarName> {
    equation_conversion::get_component_alias_connection_side(eq, dae).map(|(name, _)| name)
}

fn classify_equations(dae: &mut Dae, flat: &Model, prefix_counts: &FxHashMap<String, usize>) {
    equation_conversion::classify_equations(dae, flat, prefix_counts);
}

#[cfg(test)]
mod tests;
#[cfg(test)]
mod tests_conditions;
