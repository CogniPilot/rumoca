//! Variable analysis helpers for the ToDae phase.
//!
//! This module contains functions for classifying variables, analyzing
//! connections, validating function calls, and other variable-level analysis
//! needed during the DAE conversion.

use crate::discrete_partition;
use crate::errors::ToDaeError;
use crate::overconstrained_interface;
use crate::path_utils::{
    get_top_level_prefix, has_top_level_dot, normalized_top_level_names, path_is_in_top_level_set,
    subscript_fallback_chain,
};
use crate::scalar_inference::{collect_var_refs, compute_embedded_range_size};
use crate::{definition_analysis, when_analysis};

use rumoca_core::Span;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;
use std::collections::{HashMap, HashSet};

type BuiltinFunction = flat::BuiltinFunction;
type ComponentReference = flat::ComponentReference;
type EquationOrigin = flat::EquationOrigin;
type Expression = flat::Expression;
type Function = flat::Function;
type Model = flat::Model;
type Statement = flat::Statement;
type Subscript = flat::Subscript;
type VarName = flat::VarName;

/// Filter der() variables to only include those that will be classified as states.
///
/// External interface inputs are not states (`der(u)` differentiates an incoming
/// signal), but internal sub-component inputs can become states when they are
/// defined by local equations (e.g. `der(medium.p) = 0` in MSL media examples).
pub(crate) fn filter_state_variables(der_vars: HashSet<VarName>, flat: &Model) -> HashSet<VarName> {
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

pub(crate) fn count_overconstrained_interface(flat: &Model, state_vars: &HashSet<VarName>) -> i64 {
    overconstrained_interface::count_overconstrained_interface(flat, state_vars)
}

/// Check if a variable is a continuous unknown (state or algebraic, not param/const).
pub(crate) fn is_continuous_unknown(
    flat: &Model,
    state_vars: &HashSet<VarName>,
    name: &VarName,
) -> bool {
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
pub(crate) fn is_internal_input(name: &VarName, flat: &Model) -> bool {
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

/// Resolve a var ref name to its internal input base name.
///
/// Returns the name (or its base name with subscripts stripped) if it refers
/// to an internal input variable. Array connection equations produce per-element
/// names like `sum.u[1]` while the flat variable map stores the base `sum.u`.
pub(crate) fn resolve_internal_input(name: &VarName, flat: &Model) -> Option<VarName> {
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
pub(crate) fn find_discrete_connected_internal_inputs(
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
pub(crate) fn find_connected_inputs(flat: &Model) -> HashSet<VarName> {
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
pub(crate) fn find_equation_defined_inputs(flat: &Model) -> HashSet<VarName> {
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

pub(crate) fn resolve_flat_function<'a>(name: &VarName, flat: &'a Model) -> Option<&'a Function> {
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

pub(crate) fn component_reference_to_var_name(comp: &ComponentReference) -> VarName {
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

pub(crate) fn validate_flat_function_calls(flat: &Model) -> Result<(), ToDaeError> {
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

pub(crate) fn is_when_only_var(name: &VarName, when_only_vars: &HashSet<VarName>) -> bool {
    when_only_vars.contains(name)
        || subscript_fallback_chain(name)
            .into_iter()
            .any(|candidate| when_only_vars.contains(&candidate))
}
