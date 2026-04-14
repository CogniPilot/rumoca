//! Continuous equation filtering/conversion for ToDAE.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::discrete_partition::{ResidualDiscreteBucket, classify_residual_discrete_bucket};
use crate::name_resolution;
use crate::path_utils::{
    get_top_level_prefix, normalized_top_level_names, path_is_in_top_level_set,
    subscript_fallback_chain,
};
use crate::{flat_to_dae_expression, flat_to_dae_var_name};

pub(super) fn is_input_input_connection(eq: &flat::Equation, dae: &dae::Dae) -> bool {
    // Only check connection equations
    if !eq.origin.is_connection() {
        return false;
    }

    // Connection equations have form: lhs - rhs = 0
    let flat::Expression::Binary { op, lhs, rhs } = &eq.residual else {
        return false;
    };
    if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
        return false;
    }

    // Both sides must be inputs (not unknowns)
    let lhs_name = name_resolution::extract_varref_name(lhs);
    let rhs_name = name_resolution::extract_varref_name(rhs);
    match (lhs_name, rhs_name) {
        (Some(l), Some(r)) => {
            name_resolution::is_dae_input_name(dae, &l)
                && name_resolution::is_dae_input_name(dae, &r)
        }
        _ => false,
    }
}

/// Check if an equation defines an input variable with a constant/parameter value.
///
/// Equations of the form `input_var = literal` where `input_var` is an input
/// and the RHS is a literal (0, 1, false, etc.) should not be counted in the
/// balance because inputs are not unknowns - they're externally provided values.
/// These equations represent default values for unconnected inputs.
///
/// MLS §4.4.2.2: Input variables are known externally.
pub(super) fn is_input_default_equation(
    eq: &flat::Equation,
    flat: &flat::Model,
    dae: &dae::Dae,
) -> bool {
    // Connection equations are handled separately (may define outputs)
    if eq.origin.is_connection() {
        return false;
    }

    // Equations have form: lhs - rhs = 0
    let flat::Expression::Binary { op, lhs, rhs } = &eq.residual else {
        return false;
    };
    if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
        return false;
    }

    // Check if LHS is an input variable
    let lhs_name = name_resolution::extract_varref_name(lhs);
    let is_lhs_input = lhs_name
        .as_ref()
        .is_some_and(|name| name_resolution::is_dae_input_name(dae, name));
    if !is_lhs_input {
        return false;
    }

    // Preserve component-internal input defaults. These are often the only
    // defining equations for internal connector/control variables (e.g. StateGraph
    // transition conditions), and dropping them leaves runtime-discrete equations
    // underconstrained. Only top-level interface inputs are externally provided.
    let is_top_level_interface_input = lhs_name
        .as_ref()
        .and_then(|name| get_top_level_prefix(name.as_str()))
        .is_some_and(|prefix| flat.top_level_input_components.contains(prefix.as_str()));
    if !is_top_level_interface_input {
        return false;
    }

    // Input defaults may be literals or fixed-value expressions
    // (parameters/constants/discretes). Do not skip structural input alias
    // constraints like `u1 = u2`.
    let mut var_refs = Vec::new();
    super::collect_var_refs_skip_reductions(rhs, &mut var_refs);
    if var_refs.is_empty() {
        return true;
    }
    var_refs
        .iter()
        .all(|var_ref| name_resolution::is_dae_fixed_value_name(dae, &var_ref.name))
}

/// Get the component-defined connection side whose peer is not a continuous unknown.
///
/// Returns `(candidate_alias_side, peer_side)`.
///
/// If `candidate_alias_side` already has a component equation, this connection can
/// be a redundant alias constraint and may be skipped.
pub(super) fn get_component_alias_connection_side(
    eq: &flat::Equation,
    dae: &dae::Dae,
) -> Option<(flat::VarName, Option<flat::VarName>)> {
    if !eq.origin.is_connection() {
        return None;
    }

    let flat::Expression::Binary { op, lhs, rhs } = &eq.residual else {
        return None;
    };
    if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
        return None;
    }

    let lhs_name = name_resolution::extract_varref_name(lhs);
    let rhs_name = name_resolution::extract_varref_name(rhs);

    let lhs_is_unknown = lhs_name
        .as_ref()
        .is_some_and(|l| name_resolution::is_dae_continuous_unknown_name(dae, l));
    let rhs_is_unknown = rhs_name
        .as_ref()
        .is_some_and(|r| name_resolution::is_dae_continuous_unknown_name(dae, r));

    if !rhs_is_unknown {
        lhs_name.map(|lhs| (lhs, rhs_name))
    } else if !lhs_is_unknown {
        rhs_name.map(|rhs| (rhs, lhs_name))
    } else {
        None
    }
}

fn is_top_level_interface_input_name(
    name: &flat::VarName,
    flat: &flat::Model,
    dae: &dae::Dae,
) -> bool {
    if !name_resolution::is_dae_input_name(dae, name) {
        return false;
    }
    get_top_level_prefix(name.as_str())
        .is_some_and(|prefix| flat.top_level_input_components.contains(prefix.as_str()))
}

fn output_alias_skip_reason(
    eq: &flat::Equation,
    ctx: &EqFilterContext<'_>,
    dae: &dae::Dae,
) -> Option<flat::VarName> {
    let (output_name, peer_name) = get_component_alias_connection_side(eq, dae)?;
    if !output_has_component_equation(&output_name, ctx.outputs_with_component_eqs) {
        return None;
    }

    // Preserve discrete output aliases (e.g. BooleanTable internal output ->
    // block output). Dropping these can disconnect runtime-discrete signal paths.
    let is_discrete_signal = |name: &flat::VarName| {
        name_resolution::resolve_var_name_with_subscript_fallback(name, |candidate| {
            dae.discrete_valued
                .contains_key(&flat_to_dae_var_name(candidate))
                || dae
                    .discrete_reals
                    .contains_key(&flat_to_dae_var_name(candidate))
                || ctx.flat.variables.get(candidate).is_some_and(|var| {
                    var.is_discrete_type
                        || matches!(var.variability, rumoca_ir_core::Variability::Discrete(_))
                })
        })
        .is_some()
    };
    if is_discrete_signal(&output_name) || peer_name.as_ref().is_some_and(is_discrete_signal) {
        return None;
    }

    // Preserve internal input aliases (e.g. connector inputs in component internals).
    // Dropping these aliases disconnects discrete signal paths.
    let preserve_internal_input_alias = peer_name.as_ref().is_some_and(|peer| {
        let peer_is_discrete =
            name_resolution::resolve_var_name_with_subscript_fallback(peer, |candidate| {
                dae.discrete_valued
                    .contains_key(&flat_to_dae_var_name(candidate))
                    || dae
                        .discrete_reals
                        .contains_key(&flat_to_dae_var_name(candidate))
                    || ctx.flat.variables.get(candidate).is_some_and(|var| {
                        var.is_discrete_type
                            || matches!(var.variability, rumoca_ir_core::Variability::Discrete(_))
                    })
            })
            .is_some();
        let peer_is_component_consumed =
            name_resolution::resolve_name_against_set(peer, ctx.non_connection_rhs_var_refs)
                .is_some();
        peer_is_discrete
            && peer_is_component_consumed
            && !is_top_level_interface_input_name(peer, ctx.flat, dae)
    });
    if preserve_internal_input_alias {
        return None;
    }

    Some(output_name)
}

/// Collect variables that have component equations (non-connection equations).
///
/// This is used to determine when alias-style connections can be skipped.
fn collect_vars_with_component_equations(flat: &flat::Model) -> HashSet<flat::VarName> {
    let mut outputs_with_equations = HashSet::default();

    for eq in &flat.equations {
        // Skip connection equations - we're looking for component equations
        if eq.origin.is_connection() {
            continue;
        }

        // Check if this equation defines an output
        let flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
            continue;
        }

        if let Some(name) = name_resolution::extract_varref_name(lhs) {
            outputs_with_equations.insert(name);
        }
    }

    // Algorithm output assignments also define component variables.
    for algorithm in &flat.algorithms {
        for output in &algorithm.outputs {
            outputs_with_equations.insert(output.clone());
        }
    }

    outputs_with_equations
}

fn collect_non_connection_rhs_var_refs(flat: &flat::Model) -> HashSet<flat::VarName> {
    let mut rhs_var_refs = HashSet::default();

    for eq in &flat.equations {
        if eq.origin.is_connection() {
            continue;
        }

        match &eq.residual {
            flat::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(_),
                rhs,
                ..
            } => {
                let mut refs = Vec::new();
                super::collect_var_refs_skip_reductions(rhs, &mut refs);
                rhs_var_refs.extend(refs.into_iter().map(|var_ref| var_ref.name));
            }
            expr => {
                let mut refs = Vec::new();
                super::collect_var_refs_skip_reductions(expr, &mut refs);
                rhs_var_refs.extend(refs.into_iter().map(|var_ref| var_ref.name));
            }
        }
    }

    rhs_var_refs
}

fn output_has_component_equation(
    output_name: &flat::VarName,
    outputs_with_component_eqs: &HashSet<flat::VarName>,
) -> bool {
    if outputs_with_component_eqs.contains(output_name) {
        return true;
    }
    subscript_fallback_chain(output_name)
        .into_iter()
        .any(|candidate| outputs_with_component_eqs.contains(&candidate))
}

fn collect_top_level_overconstrained_connectors(flat: &flat::Model) -> HashSet<String> {
    // Normalize connector array names: `plug[1]` and `plug` should map to
    // the same top-level connector when applying interface-flow rules.
    let normalized_top_level_connectors =
        normalized_top_level_names(flat.top_level_connectors.iter());

    flat.variables
        .iter()
        .filter(|(_, var)| var.is_overconstrained)
        .filter_map(|(name, _)| get_top_level_prefix(name.as_str()))
        .filter(|prefix| normalized_top_level_connectors.contains(prefix.as_str()))
        .collect()
}

fn is_unconnected_flow_from_top_level_overconstrained_connector(
    eq: &flat::Equation,
    top_level_oc_connectors: &HashSet<String>,
) -> bool {
    let flat::EquationOrigin::UnconnectedFlow { variable } = &eq.origin else {
        return false;
    };
    path_is_in_top_level_set(variable, top_level_oc_connectors)
}

#[derive(Default)]
struct EqFilterStats {
    kept_connection: usize,
    kept_flow_sum: usize,
    kept_unconnected_flow: usize,
    kept_other: usize,
    skipped_duplicates: usize,
    skipped_top_level_oc: usize,
    skipped_input_input: usize,
    skipped_output_alias: usize,
    skipped_input_default: usize,
    skipped_explicit_zero: usize,
    skipped_inferred_zero: usize,
}

impl EqFilterStats {
    fn record_kept(&mut self, origin: &flat::EquationOrigin) {
        match origin {
            flat::EquationOrigin::Connection { .. } => self.kept_connection += 1,
            flat::EquationOrigin::FlowSum { .. } => self.kept_flow_sum += 1,
            flat::EquationOrigin::UnconnectedFlow { .. } => self.kept_unconnected_flow += 1,
            _ => self.kept_other += 1,
        }
    }

    fn log(&self) {
        eprintln!(
            "eq-filter: kept(connection={}, flow_sum={}, unconnected_flow={}, other={}) skipped(dup={}, top_level_oc={}, input_input={}, output_alias={}, input_default={}, explicit_zero={}, inferred_zero={})",
            self.kept_connection,
            self.kept_flow_sum,
            self.kept_unconnected_flow,
            self.kept_other,
            self.skipped_duplicates,
            self.skipped_top_level_oc,
            self.skipped_input_input,
            self.skipped_output_alias,
            self.skipped_input_default,
            self.skipped_explicit_zero,
            self.skipped_inferred_zero
        );
    }
}

fn classify_equation_scalar_count(
    eq: &flat::Equation,
    flat: &flat::Model,
    prefix_counts: &FxHashMap<String, usize>,
    inferred_scalar_count: usize,
    lhs_scalar_count: Option<usize>,
) -> usize {
    if matches!(eq.origin, flat::EquationOrigin::UnconnectedFlow { .. }) {
        return eq.scalar_count.max(1);
    }
    if matches!(eq.origin, flat::EquationOrigin::FlowSum { .. }) {
        return super::infer_flow_sum_scalar_count(&eq.residual, flat, prefix_counts)
            .unwrap_or_else(|| eq.scalar_count.max(inferred_scalar_count.max(1)));
    }
    if matches!(eq.origin, flat::EquationOrigin::Connection { .. }) {
        return eq.scalar_count.max(1);
    }
    lhs_scalar_count.unwrap_or_else(|| eq.scalar_count.max(inferred_scalar_count.max(1)))
}

/// Convert flat equations to DAE form and add them to the unified f_x vector.
///
/// All continuous equations go into `dae.f_x` (MLS B.1a) — no ODE/algebraic/output split.
/// Equation classification (which equation solves which variable) is deferred to
/// structural analysis.
fn log_skip(debug_eq_filter: bool, reason: &str, eq: &flat::Equation) {
    if debug_eq_filter {
        eprintln!(
            "eq-filter skip[{reason}] origin={} residual={:?}",
            eq.origin, eq.residual
        );
    }
}

fn log_kept_if_matches_patterns(eq: &flat::Equation, scalar_count: usize) {
    let Ok(patterns) = std::env::var("RUMOCA_DEBUG_EQ_FILTER_PATTERNS") else {
        return;
    };
    let patterns: Vec<_> = patterns
        .split(',')
        .map(str::trim)
        .filter(|pattern| !pattern.is_empty())
        .collect();
    if patterns.is_empty() {
        return;
    }
    let origin = eq.origin.to_string();
    let residual = format!("{:?}", eq.residual);
    if patterns
        .iter()
        .any(|pattern| origin.contains(pattern) || residual.contains(pattern))
    {
        eprintln!(
            "eq-filter kept origin={} scalar_count={} residual={residual}",
            eq.origin, scalar_count
        );
    }
}

struct EqFilterContext<'a> {
    flat: &'a flat::Model,
    outputs_with_component_eqs: &'a HashSet<flat::VarName>,
    non_connection_rhs_var_refs: &'a HashSet<flat::VarName>,
    top_level_oc_connectors: &'a HashSet<String>,
    debug_eq_filter: bool,
}

fn skip_equation_pre_classification(
    eq: &flat::Equation,
    ctx: &EqFilterContext<'_>,
    dae: &dae::Dae,
    seen_residuals: &mut HashSet<String>,
    stats: &mut EqFilterStats,
) -> bool {
    let residual_key = format!("{:?}", eq.residual);
    if !seen_residuals.insert(residual_key) {
        stats.skipped_duplicates += 1;
        log_skip(ctx.debug_eq_filter, "duplicate", eq);
        return true;
    }

    if is_unconnected_flow_from_top_level_overconstrained_connector(eq, ctx.top_level_oc_connectors)
    {
        stats.skipped_top_level_oc += 1;
        log_skip(ctx.debug_eq_filter, "top_level_oc", eq);
        return true;
    }

    if is_input_input_connection(eq, dae) {
        stats.skipped_input_input += 1;
        log_skip(ctx.debug_eq_filter, "input_input", eq);
        return true;
    }

    if let Some(output_name) = output_alias_skip_reason(eq, ctx, dae) {
        stats.skipped_output_alias += 1;
        if ctx.debug_eq_filter {
            eprintln!(
                "eq-filter skip[output_alias:{}] origin={} residual={:?}",
                output_name.as_str(),
                eq.origin,
                eq.residual
            );
        }
        return true;
    }

    if is_input_default_equation(eq, ctx.flat, dae) {
        stats.skipped_input_default += 1;
        log_skip(ctx.debug_eq_filter, "input_default", eq);
        return true;
    }

    if eq.scalar_count == 0 {
        stats.skipped_explicit_zero += 1;
        log_skip(ctx.debug_eq_filter, "explicit_zero", eq);
        return true;
    }

    false
}

fn compute_scalar_count(
    eq: &flat::Equation,
    flat: &flat::Model,
    prefix_counts: &FxHashMap<String, usize>,
    linearized_embedded_lhs_bases: &HashSet<flat::VarName>,
    stats: &mut EqFilterStats,
    debug_eq_filter: bool,
) -> Option<usize> {
    let inferred_scalar_count =
        super::infer_equation_scalar_count(&eq.residual, flat, prefix_counts);
    let lhs_scalar_count = super::extract_lhs_var_size_with_linearized_bases(
        &eq.residual,
        flat,
        prefix_counts,
        linearized_embedded_lhs_bases,
    );

    if inferred_scalar_count == 0 && eq.scalar_count <= 1 {
        log_skip(debug_eq_filter, "inferred_zero-pre", eq);
        return None;
    }

    let scalar_count = classify_equation_scalar_count(
        eq,
        flat,
        prefix_counts,
        inferred_scalar_count,
        lhs_scalar_count,
    );

    if scalar_count == 0 {
        stats.skipped_inferred_zero += 1;
        log_skip(debug_eq_filter, "inferred_zero", eq);
        return None;
    }

    Some(scalar_count)
}

fn route_classified_equation(dae: &mut dae::Dae, eq: &flat::Equation, dae_eq: dae::Equation) {
    let discrete_bucket = classify_residual_discrete_bucket(dae, &eq.residual);

    if eq.origin.is_connection() {
        // Keep continuous alias equations in f_x, but route alias equations
        // targeting discrete variables to the discrete partitions.
        match discrete_bucket {
            Some(ResidualDiscreteBucket::DiscreteValued) => {
                if !push_explicit_discrete_assignments(dae, &dae_eq, true) {
                    dae.f_m.push(dae_eq);
                }
            }
            Some(ResidualDiscreteBucket::DiscreteReal | ResidualDiscreteBucket::Mixed) => {
                dae.f_z.push(dae_eq);
            }
            None => {
                dae.f_x.push(dae_eq);
            }
        }
        return;
    }

    match discrete_bucket {
        Some(ResidualDiscreteBucket::DiscreteValued) => {
            if !push_explicit_discrete_assignments(dae, &dae_eq, true) {
                dae.f_m.push(dae_eq);
            }
        }
        Some(ResidualDiscreteBucket::DiscreteReal | ResidualDiscreteBucket::Mixed) => {
            // Tuple equations can mix Real and discrete-valued targets
            // before scalarization. Keep them in the discrete update set
            // (f_z) so they are excluded from continuous residual solving.
            dae.f_z.push(dae_eq);
        }
        None => {
            dae.f_x.push(dae_eq);
        }
    }
}

fn is_numeric_zero(expr: &flat::Expression) -> bool {
    match expr {
        flat::Expression::Literal(flat::Literal::Integer(0)) => true,
        flat::Expression::Literal(flat::Literal::Real(v)) => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

fn pre_of_target(target: &flat::VarName) -> flat::Expression {
    flat::Expression::BuiltinCall {
        function: flat::BuiltinFunction::Pre,
        args: vec![flat::Expression::VarRef {
            name: target.clone(),
            subscripts: vec![],
        }],
    }
}

fn const_subscript_index_expr(expr: &flat::Expression) -> Option<i64> {
    let value = match expr {
        flat::Expression::Literal(flat::Literal::Integer(value)) => *value as f64,
        flat::Expression::Literal(flat::Literal::Real(value)) => *value,
        flat::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => -const_subscript_index_expr(rhs)? as f64,
        flat::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Plus(_),
            rhs,
        } => const_subscript_index_expr(rhs)? as f64,
        flat::Expression::Binary { op, lhs, rhs } => {
            let lhs = const_subscript_index_expr(lhs)? as f64;
            let rhs = const_subscript_index_expr(rhs)? as f64;
            match op {
                rumoca_ir_core::OpBinary::Add(_) => lhs + rhs,
                rumoca_ir_core::OpBinary::Sub(_) => lhs - rhs,
                rumoca_ir_core::OpBinary::Mul(_) => lhs * rhs,
                rumoca_ir_core::OpBinary::Div(_) => lhs / rhs,
                _ => return None,
            }
        }
        _ => return None,
    };

    (value.is_finite() && value.fract() == 0.0).then_some(value as i64)
}

fn render_subscript(subscript: &flat::Subscript) -> String {
    match subscript {
        flat::Subscript::Index(index) => index.to_string(),
        flat::Subscript::Colon => ":".to_string(),
        // MLS Chapter 10 indexing: explicit assignment targets must resolve
        // constant scalar subscripts to the concrete element name they define.
        flat::Subscript::Expr(expr) => const_subscript_index_expr(expr)
            .map_or_else(|| format!("{expr:?}"), |index| index.to_string()),
    }
}

fn append_rendered_subscripts(
    name: &flat::VarName,
    subscripts: &[flat::Subscript],
) -> flat::VarName {
    if subscripts.is_empty() {
        return name.clone();
    }
    let rendered = subscripts
        .iter()
        .map(render_subscript)
        .collect::<Vec<_>>()
        .join(",");
    flat::VarName::new(format!("{}[{rendered}]", name.as_str()))
}

fn explicit_assignment_target(expr: &flat::Expression) -> Option<flat::VarName> {
    match expr {
        // MLS Appendix B B.1c with MLS Chapter 10 indexing: explicit discrete
        // assignments must preserve the full indexed/field-qualified target.
        flat::Expression::VarRef { name, subscripts } => {
            Some(append_rendered_subscripts(name, subscripts))
        }
        flat::Expression::Index { base, subscripts } => {
            let base = explicit_assignment_target(base)?;
            Some(append_rendered_subscripts(&base, subscripts))
        }
        flat::Expression::FieldAccess { base, field } => {
            let base = explicit_assignment_target(base)?;
            Some(flat::VarName::new(format!("{}.{}", base.as_str(), field)))
        }
        _ => None,
    }
}

fn collect_explicit_discrete_assignments(
    expr: &flat::Expression,
) -> Option<HashMap<flat::VarName, flat::Expression>> {
    match expr {
        flat::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if is_numeric_zero(lhs) {
                if let Some(assignments) = collect_explicit_discrete_assignments(rhs) {
                    return Some(assignments);
                }
                let target = explicit_assignment_target(rhs)?;
                let mut result = HashMap::new();
                result.insert(target, flat::Expression::Literal(flat::Literal::Integer(0)));
                return Some(result);
            }
            if is_numeric_zero(rhs) {
                if let Some(assignments) = collect_explicit_discrete_assignments(lhs) {
                    return Some(assignments);
                }
                let target = explicit_assignment_target(lhs)?;
                let mut result = HashMap::new();
                result.insert(target, flat::Expression::Literal(flat::Literal::Integer(0)));
                return Some(result);
            }
            let name = explicit_assignment_target(lhs)?;
            let mut result = HashMap::new();
            result.insert(name, rhs.as_ref().clone());
            Some(result)
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            let mut branch_maps = Vec::new();
            for (condition, value) in branches {
                let assignments = collect_explicit_discrete_assignments(value)?;
                branch_maps.push((condition.clone(), assignments));
            }

            let else_map = collect_explicit_discrete_assignments(else_branch)?;
            let mut target_order = Vec::<flat::VarName>::new();
            let mut seen = HashSet::new();
            for (_, assignments) in &branch_maps {
                for target in assignments.keys() {
                    push_unique_target(&mut target_order, &mut seen, target);
                }
            }
            for target in else_map.keys() {
                push_unique_target(&mut target_order, &mut seen, target);
            }

            let mut result = HashMap::new();
            for target in target_order {
                let branch_values = branch_maps
                    .iter()
                    .map(|(condition, assignments)| {
                        let rhs = assignments
                            .get(&target)
                            .cloned()
                            .unwrap_or_else(|| pre_of_target(&target));
                        (condition.clone(), rhs)
                    })
                    .collect();
                let else_value = else_map
                    .get(&target)
                    .cloned()
                    .unwrap_or_else(|| pre_of_target(&target));
                result.insert(
                    target,
                    flat::Expression::If {
                        branches: branch_values,
                        else_branch: Box::new(else_value),
                    },
                );
            }

            Some(result)
        }
        flat::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => collect_explicit_discrete_assignments(rhs),
        _ => None,
    }
}

fn push_unique_target(
    target_order: &mut Vec<flat::VarName>,
    seen: &mut HashSet<flat::VarName>,
    target: &flat::VarName,
) {
    if !seen.insert(target.clone()) {
        return;
    }
    target_order.push(target.clone());
}

fn discrete_target_scalar_count(
    dae: &dae::Dae,
    target: &flat::VarName,
    fallback_scalar_count: usize,
) -> usize {
    if let Some(variable) = dae.discrete_valued.get(&flat_to_dae_var_name(target)) {
        return variable.size().max(1);
    }

    // Subscripted assignments should count as scalar updates.
    for candidate in subscript_fallback_chain(target) {
        if dae
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(&candidate))
        {
            return 1;
        }
    }

    fallback_scalar_count.max(1)
}

fn push_explicit_discrete_assignments(
    dae: &mut dae::Dae,
    equation: &dae::Equation,
    discrete_valued: bool,
) -> bool {
    let rhs = crate::dae_to_flat_expression(&equation.rhs);
    let Some(assignments) = collect_explicit_discrete_assignments(&rhs) else {
        return false;
    };

    let mut ordered: Vec<_> = assignments.into_iter().collect();
    ordered.sort_unstable_by(|(lhs, _), (rhs, _)| lhs.as_str().cmp(rhs.as_str()));

    for (target, rhs) in ordered {
        let scalar_count = if discrete_valued {
            discrete_target_scalar_count(dae, &target, equation.scalar_count)
        } else {
            equation.scalar_count.max(1)
        };
        let explicit = dae::Equation::explicit_with_scalar_count(
            flat_to_dae_var_name(&target),
            flat_to_dae_expression(&rhs),
            equation.span,
            format!("explicit {}", equation.origin),
            scalar_count,
        );
        if discrete_valued {
            dae.f_m.push(explicit);
        } else {
            dae.f_z.push(explicit);
        }
    }

    true
}

pub(super) fn classify_equations(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    prefix_counts: &FxHashMap<String, usize>,
) {
    let outputs_with_component_eqs = collect_vars_with_component_equations(flat);
    let non_connection_rhs_var_refs = collect_non_connection_rhs_var_refs(flat);
    let top_level_oc_connectors = collect_top_level_overconstrained_connectors(flat);
    let linearized_embedded_lhs_bases = super::collect_linearized_embedded_lhs_bases(flat);
    let debug_eq_filter = std::env::var("RUMOCA_DEBUG_EQ_FILTER").is_ok();
    let mut seen_residuals: HashSet<String> = HashSet::default();
    let mut stats = EqFilterStats::default();
    let filter_ctx = EqFilterContext {
        flat,
        outputs_with_component_eqs: &outputs_with_component_eqs,
        non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
        top_level_oc_connectors: &top_level_oc_connectors,
        debug_eq_filter,
    };

    for eq in &flat.equations {
        if skip_equation_pre_classification(eq, &filter_ctx, dae, &mut seen_residuals, &mut stats) {
            continue;
        }

        let Some(scalar_count) = compute_scalar_count(
            eq,
            flat,
            prefix_counts,
            &linearized_embedded_lhs_bases,
            &mut stats,
            debug_eq_filter,
        ) else {
            continue;
        };

        log_kept_if_matches_patterns(eq, scalar_count);
        if debug_eq_filter {
            stats.record_kept(&eq.origin);
        }
        let dae_eq = dae::Equation::residual_array(
            flat_to_dae_expression(&eq.residual),
            eq.span,
            eq.origin.to_string(),
            scalar_count,
        );
        route_classified_equation(dae, eq, dae_eq);
    }

    if debug_eq_filter {
        stats.log();
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use rumoca_core::Span;
    use rumoca_ir_dae as dae;
    use rumoca_ir_flat as flat;

    use super::{EqFilterContext, output_alias_skip_reason, output_has_component_equation};

    #[test]
    fn test_output_has_component_equation_matches_unsubscripted_base() {
        let outputs_with_component_eqs: HashSet<flat::VarName> =
            [flat::VarName::new("y")].into_iter().collect();
        assert!(output_has_component_equation(
            &flat::VarName::new("y[2]"),
            &outputs_with_component_eqs
        ));
    }

    #[test]
    fn test_output_has_component_equation_matches_multilayer_unsubscripted_base() {
        let outputs_with_component_eqs: HashSet<flat::VarName> =
            [flat::VarName::new("bus.signal")].into_iter().collect();
        assert!(output_has_component_equation(
            &flat::VarName::new("bus[1].signal[2]"),
            &outputs_with_component_eqs
        ));
    }

    #[test]
    fn test_output_alias_skip_preserves_internal_input_alias_connection() {
        let flat_model = flat::Model::new();
        let outputs_with_component_eqs: HashSet<flat::VarName> =
            [flat::VarName::new("booleanPulse1.y")]
                .into_iter()
                .collect();
        let non_connection_rhs_var_refs: HashSet<flat::VarName> =
            [flat::VarName::new("multiSwitch1.u")].into_iter().collect();
        let top_level_oc_connectors: HashSet<String> = HashSet::default();
        let ctx = EqFilterContext {
            flat: &flat_model,
            outputs_with_component_eqs: &outputs_with_component_eqs,
            non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
            top_level_oc_connectors: &top_level_oc_connectors,
            debug_eq_filter: false,
        };

        let mut dae_model = dae::Dae::new();
        dae_model.inputs.insert(
            dae::VarName::new("multiSwitch1.u"),
            dae::Variable::new(dae::VarName::new("multiSwitch1.u")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("multiSwitch1.u"),
            dae::Variable::new(dae::VarName::new("multiSwitch1.u")),
        );

        let eq = flat::Equation::new(
            flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("booleanPulse1.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("multiSwitch1.u[1]"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            flat::EquationOrigin::Connection {
                lhs: "booleanPulse1.y".to_string(),
                rhs: "multiSwitch1.u[1]".to_string(),
            },
        );

        assert!(
            output_alias_skip_reason(&eq, &ctx, &dae_model).is_none(),
            "internal input alias connections must be preserved"
        );
    }

    #[test]
    fn test_output_alias_skip_preserves_discrete_output_alias_connection() {
        let mut flat_model = flat::Model::new();
        flat_model.variables.insert(
            flat::VarName::new("table1.y"),
            flat::Variable {
                name: flat::VarName::new("table1.y"),
                is_discrete_type: true,
                ..Default::default()
            },
        );
        flat_model.variables.insert(
            flat::VarName::new("table1.realToBoolean.y"),
            flat::Variable {
                name: flat::VarName::new("table1.realToBoolean.y"),
                is_discrete_type: true,
                ..Default::default()
            },
        );

        let outputs_with_component_eqs: HashSet<flat::VarName> =
            [flat::VarName::new("table1.realToBoolean.y")]
                .into_iter()
                .collect();
        let non_connection_rhs_var_refs: HashSet<flat::VarName> = HashSet::default();
        let top_level_oc_connectors: HashSet<String> = HashSet::default();
        let ctx = EqFilterContext {
            flat: &flat_model,
            outputs_with_component_eqs: &outputs_with_component_eqs,
            non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
            top_level_oc_connectors: &top_level_oc_connectors,
            debug_eq_filter: false,
        };

        let mut dae_model = dae::Dae::new();
        dae_model.outputs.insert(
            dae::VarName::new("table1.y"),
            dae::Variable::new(dae::VarName::new("table1.y")),
        );
        dae_model.outputs.insert(
            dae::VarName::new("table1.realToBoolean.y"),
            dae::Variable::new(dae::VarName::new("table1.realToBoolean.y")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("table1.y"),
            dae::Variable::new(dae::VarName::new("table1.y")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("table1.realToBoolean.y"),
            dae::Variable::new(dae::VarName::new("table1.realToBoolean.y")),
        );

        let eq = flat::Equation::new(
            flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("table1.realToBoolean.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("table1.y"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            flat::EquationOrigin::Connection {
                lhs: "table1.realToBoolean.y".to_string(),
                rhs: "table1.y".to_string(),
            },
        );

        assert!(
            output_alias_skip_reason(&eq, &ctx, &dae_model).is_none(),
            "discrete output alias connections must be preserved"
        );
    }

    #[test]
    fn test_output_alias_skip_applies_when_both_sides_are_component_defined() {
        let flat_model = flat::Model::new();
        let outputs_with_component_eqs: HashSet<flat::VarName> =
            [flat::VarName::new("source.y"), flat::VarName::new("sink.u")]
                .into_iter()
                .collect();
        let non_connection_rhs_var_refs: HashSet<flat::VarName> = HashSet::default();
        let top_level_oc_connectors: HashSet<String> = HashSet::default();
        let ctx = EqFilterContext {
            flat: &flat_model,
            outputs_with_component_eqs: &outputs_with_component_eqs,
            non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
            top_level_oc_connectors: &top_level_oc_connectors,
            debug_eq_filter: false,
        };

        let dae_model = dae::Dae::new();

        let eq = flat::Equation::new(
            flat::Expression::Binary {
                op: flat::OpBinary::Sub(Default::default()),
                lhs: Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("source.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(flat::Expression::VarRef {
                    name: flat::VarName::new("sink.u"),
                    subscripts: vec![],
                }),
            },
            Span::DUMMY,
            flat::EquationOrigin::Connection {
                lhs: "source.y".to_string(),
                rhs: "sink.u".to_string(),
            },
        );

        assert_eq!(
            output_alias_skip_reason(&eq, &ctx, &dae_model).as_ref(),
            Some(&flat::VarName::new("source.y")),
            "alias skip should apply for non-preserved output aliases"
        );
    }
}
