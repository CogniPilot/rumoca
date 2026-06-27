//! Continuous equation filtering/conversion for ToDAE.
//!
//! SPEC_0021 file-size exception: this module still owns the current Flat
//! equation-to-DAE conversion path. split plan: move connector/input boundary
//! filtering, structured-equation expansion, and expression conversion context
//! into separate sibling modules as each concern receives focused tests.

use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::dae_lowering::record_constructor_fields_from_metadata;
use crate::discrete_partition::{ResidualDiscreteBucket, classify_residual_discrete_bucket};
use crate::errors::ToDaeError;
use crate::name_resolution;
use crate::path_utils::{
    get_top_level_prefix, normalized_top_level_names, path_is_in_top_level_set,
};
use crate::{
    flat_to_dae_expression_with_refs, flat_to_dae_var_name, remap_flat_structured_equations,
};

pub(super) fn is_input_input_connection(eq: &flat::Equation, dae: &dae::Dae) -> bool {
    // Only check connection equations
    if !eq.origin.is_connection() {
        return false;
    }

    // Connection equations have form: lhs - rhs = 0
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
        return false;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
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

fn is_flat_stream_name(flat: &flat::Model, name: &rumoca_core::VarName) -> bool {
    name_resolution::resolve_var_name_with_subscript_fallback(name, |candidate| {
        flat.variables
            .get(candidate)
            .is_some_and(|variable| variable.stream)
    })
    .is_some()
}

fn connection_sides(eq: &flat::Equation) -> Option<(rumoca_core::VarName, rumoca_core::VarName)> {
    if !eq.origin.is_connection() {
        return None;
    }

    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }

    Some((
        name_resolution::extract_varref_name(lhs)?,
        name_resolution::extract_varref_name(rhs)?,
    ))
}

pub(super) fn is_stream_stream_connection(eq: &flat::Equation, flat: &flat::Model) -> bool {
    let Some((lhs_name, rhs_name)) = connection_sides(eq) else {
        return false;
    };
    is_flat_stream_name(flat, &lhs_name) && is_flat_stream_name(flat, &rhs_name)
}

fn should_skip_stream_stream_connection(eq: &flat::Equation, ctx: &EqFilterContext<'_>) -> bool {
    let Some((lhs_name, rhs_name)) = connection_sides(eq) else {
        return false;
    };
    if !is_stream_stream_connection(eq, ctx.flat) {
        return false;
    }

    let lhs_consumed =
        name_resolution::resolve_name_against_set(&lhs_name, ctx.non_connection_rhs_var_refs)
            .is_some();
    let rhs_consumed =
        name_resolution::resolve_name_against_set(&rhs_name, ctx.non_connection_rhs_var_refs)
            .is_some();
    !(lhs_consumed && rhs_consumed)
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
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
        return false;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
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
) -> Option<(rumoca_core::VarName, Option<rumoca_core::VarName>)> {
    if !eq.origin.is_connection() {
        return None;
    }

    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
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
    name: &rumoca_core::VarName,
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
) -> Option<rumoca_core::VarName> {
    let (output_name, peer_name) = get_component_alias_connection_side(eq, dae)?;
    if !output_has_component_equation(&output_name, ctx.outputs_with_component_eqs) {
        return None;
    }

    // Preserve discrete output aliases (e.g. BooleanTable internal output ->
    // block output). Dropping these can disconnect runtime-discrete signal paths.
    let is_discrete_signal = |name: &rumoca_core::VarName| {
        name_resolution::resolve_var_name_with_subscript_fallback(name, |candidate| {
            dae.variables
                .discrete_valued
                .contains_key(&flat_to_dae_var_name(candidate))
                || dae
                    .variables
                    .discrete_reals
                    .contains_key(&flat_to_dae_var_name(candidate))
        })
        .is_some()
            || ctx.flat.variables.get(name).is_some_and(|var| {
                var.is_discrete_type
                    || matches!(var.variability, rumoca_core::Variability::Discrete(_))
            })
    };
    if is_discrete_signal(&output_name) || peer_name.as_ref().is_some_and(is_discrete_signal) {
        return None;
    }

    // Preserve internal input aliases (e.g. connector inputs in component internals).
    // Dropping these aliases disconnects discrete signal paths.
    let preserve_internal_input_alias = peer_name.as_ref().is_some_and(|peer| {
        let peer_is_discrete = is_discrete_signal(peer);
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
fn collect_vars_with_component_equations(flat: &flat::Model) -> HashSet<rumoca_core::VarName> {
    let mut outputs_with_equations = HashSet::default();

    for eq in &flat.equations {
        // Skip connection equations - we're looking for component equations
        if eq.origin.is_connection() {
            continue;
        }

        // Check if this equation defines an output
        let rumoca_core::Expression::Binary { op, lhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }

        if let Some(name) = name_resolution::extract_varref_name(lhs) {
            outputs_with_equations.insert(name);
        }
    }

    // Algorithm output assignments also define component variables.
    for algorithm in &flat.algorithms {
        for output in &algorithm.outputs {
            outputs_with_equations.insert(output.var_name().clone());
        }
    }

    outputs_with_equations
}

fn collect_non_connection_rhs_var_refs(flat: &flat::Model) -> HashSet<rumoca_core::VarName> {
    let mut rhs_var_refs = HashSet::default();

    for eq in &flat.equations {
        if eq.origin.is_connection() {
            continue;
        }

        match &eq.residual {
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
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
    output_name: &rumoca_core::VarName,
    outputs_with_component_eqs: &HashSet<rumoca_core::VarName>,
) -> bool {
    if outputs_with_component_eqs.contains(output_name) {
        return true;
    }
    rumoca_core::component_path_base_name(output_name.as_str())
        .map(rumoca_core::VarName::new)
        .is_some_and(|base| outputs_with_component_eqs.contains(&base))
}

fn collect_top_level_overconstrained_connectors(flat: &flat::Model) -> IndexSet<String> {
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
    top_level_oc_connectors: &IndexSet<String>,
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
    skipped_top_level_oc: usize,
    skipped_input_input: usize,
    skipped_stream_stream: usize,
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
        crate::log_equation_filter_debug(format!(
            "eq-filter: kept(connection={}, flow_sum={}, unconnected_flow={}, other={}) skipped(top_level_oc={}, input_input={}, stream_stream={}, output_alias={}, input_default={}, explicit_zero={}, inferred_zero={})",
            self.kept_connection,
            self.kept_flow_sum,
            self.kept_unconnected_flow,
            self.kept_other,
            self.skipped_top_level_oc,
            self.skipped_input_input,
            self.skipped_stream_stream,
            self.skipped_output_alias,
            self.skipped_input_default,
            self.skipped_explicit_zero,
            self.skipped_inferred_zero
        ));
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
        return match super::infer_flow_sum_scalar_count(&eq.residual, flat, prefix_counts) {
            Some(flow_sum_count) => flow_sum_count,
            None => eq.scalar_count.max(inferred_scalar_count.max(1)),
        };
    }
    if matches!(eq.origin, flat::EquationOrigin::Connection { .. }) {
        return eq.scalar_count.max(1);
    }
    match lhs_scalar_count {
        Some(count) => count,
        None => eq.scalar_count.max(inferred_scalar_count.max(1)),
    }
}

/// Convert flat equations to DAE form and add them to the unified f_x vector.
///
/// All continuous equations go into `dae.continuous.equations` (MLS B.1a) — no ODE/algebraic/output split.
/// Equation classification (which equation solves which variable) is deferred to
/// structural analysis.
fn log_skip(debug_eq_filter: bool, reason: &str, eq: &flat::Equation) {
    if debug_eq_filter {
        crate::log_equation_filter_debug(format!(
            "eq-filter skip[{reason}] origin={} residual={:?}",
            eq.origin, eq.residual
        ));
    }
}

fn log_kept(debug_eq_filter: bool, eq: &flat::Equation, scalar_count: usize) {
    if debug_eq_filter {
        crate::log_equation_filter_debug(format!(
            "eq-filter kept origin={} scalar_count={} residual={:?}",
            eq.origin, scalar_count, eq.residual
        ));
    }
}

struct EqFilterContext<'a> {
    flat: &'a flat::Model,
    outputs_with_component_eqs: &'a HashSet<rumoca_core::VarName>,
    non_connection_rhs_var_refs: &'a HashSet<rumoca_core::VarName>,
    top_level_oc_connectors: &'a IndexSet<String>,
    debug_eq_filter: bool,
}

fn skip_equation_pre_classification(
    eq: &flat::Equation,
    ctx: &EqFilterContext<'_>,
    dae: &dae::Dae,
    stats: &mut EqFilterStats,
) -> bool {
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

    if should_skip_stream_stream_connection(eq, ctx) {
        stats.skipped_stream_stream += 1;
        log_skip(ctx.debug_eq_filter, "stream_stream", eq);
        return true;
    }

    if let Some(output_name) = output_alias_skip_reason(eq, ctx, dae) {
        stats.skipped_output_alias += 1;
        if ctx.debug_eq_filter {
            crate::log_equation_filter_debug(format!(
                "eq-filter skip[output_alias:{}] origin={} residual={:?}",
                output_name.as_str(),
                eq.origin,
                eq.residual
            ));
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
    linearized_embedded_lhs_bases: &HashSet<rumoca_core::VarName>,
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

fn record_field_specs_for_call(
    name: &rumoca_core::Reference,
    is_constructor: bool,
    flat: &flat::Model,
) -> Option<Vec<RecordFieldSpec>> {
    let function = flat.functions.get(name.var_name())?;
    let fields = if is_constructor || function.is_constructor {
        function.inputs.clone()
    } else {
        let [output] = function.outputs.as_slice() else {
            return None;
        };
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            return None;
        }
        record_constructor_fields_from_metadata(flat.functions.iter(), &output.type_name)?
    };
    RecordFieldSpec::from_params(fields)
}

#[derive(Debug, Clone)]
struct RecordFieldSpec {
    param: rumoca_core::FunctionParam,
    match_by_name: bool,
}

impl RecordFieldSpec {
    fn from_params(params: Vec<rumoca_core::FunctionParam>) -> Option<Vec<Self>> {
        for param in &params {
            assert!(
                param.def_id.is_some(),
                "record field `{}` has no DefId; record equation expansion requires resolved field identity",
                param.name
            );
        }
        (!params.is_empty()).then(|| {
            params
                .into_iter()
                .map(|param| Self {
                    param,
                    match_by_name: false,
                })
                .collect::<Vec<_>>()
        })
    }

    fn name(&self) -> &str {
        self.param.name.as_str()
    }

    fn default(&self) -> Option<rumoca_core::Expression> {
        self.param.default.clone()
    }

    fn field_access(
        &self,
        base: rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::FieldAccess {
            base: Box::new(base),
            field: self.param.name.clone(),
            span,
        }
    }

    fn is_lhs_derived(&self) -> bool {
        self.match_by_name
    }

    fn matches_component_ref(
        &self,
        field_ref: &rumoca_core::ComponentReference,
        symbol_ancestry: &IndexMap<rumoca_core::DefId, Vec<rumoca_core::DefId>>,
    ) -> bool {
        self.param.def_id.is_some_and(|expected| {
            let name_matches = self.match_by_name
                && field_ref
                    .parts
                    .last()
                    .is_some_and(|part| part.ident == self.param.name);
            name_matches
                || field_ref.def_id == Some(expected)
                || field_ref.def_id.is_some_and(|actual| {
                    symbol_ancestry
                        .get(&actual)
                        .is_some_and(|ancestry| ancestry.contains(&expected))
                })
        })
    }
}

fn record_field_specs_for_rhs(
    rhs: &rumoca_core::Expression,
    flat: &flat::Model,
) -> Option<Vec<RecordFieldSpec>> {
    match rhs {
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } => record_field_specs_for_call(name, *is_constructor, flat),
        _ => None,
    }
}

fn named_constructor_arg<'a>(
    args: &'a [rumoca_core::Expression],
    field: &RecordFieldSpec,
) -> Option<&'a rumoca_core::Expression> {
    args.iter().find_map(|arg| {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = arg
        else {
            return None;
        };
        (name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field.name()))
            .then(|| args.first())
            .flatten()
    })
}

fn constructor_field_arg(
    args: &[rumoca_core::Expression],
    field: &RecordFieldSpec,
    index: usize,
) -> Option<rumoca_core::Expression> {
    if let Some(arg) = named_constructor_arg(args, field) {
        return Some(arg.clone());
    }
    args.iter()
        .filter(|arg| {
            !matches!(arg, rumoca_core::Expression::FunctionCall { name, is_constructor: true, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
        })
        .nth(index)
        .cloned()
        .or_else(|| field.default())
}

fn rhs_field_expression(
    rhs: &rumoca_core::Expression,
    field: &RecordFieldSpec,
    index: usize,
    flat: &flat::Model,
    equation_span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = rhs
        && flat
            .functions
            .get(name.var_name())
            .is_some_and(|function| *is_constructor || function.is_constructor)
        && let Some(arg) = constructor_field_arg(args, field, index)
    {
        return arg;
    }

    let span = match rhs.span() {
        Some(span) => span,
        None => equation_span,
    };
    field.field_access(rhs.clone(), span)
}

fn reference_for_variable(field_var: &flat::Variable) -> rumoca_core::Reference {
    if let Some(component_ref) = field_var.component_ref.clone() {
        return rumoca_core::Reference::with_component_reference(
            field_var.name.as_str(),
            component_ref,
        );
    }
    rumoca_core::Reference::from_var_name(field_var.name.clone())
}

fn field_residual(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn field_var_ref(field_var: &flat::Variable) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: reference_for_variable(field_var),
        subscripts: Vec::new(),
        span: field_var.source_span,
    }
}

fn field_lhs_expression(
    field_vars: &[&flat::Variable],
    equation_span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if let [field_var] = field_vars {
        return field_var_ref(field_var);
    }
    rumoca_core::Expression::Array {
        elements: field_vars
            .iter()
            .map(|field_var| field_var_ref(field_var))
            .collect(),
        is_matrix: false,
        span: equation_span,
    }
}

fn field_scalar_count(field_vars: &[&flat::Variable]) -> usize {
    field_vars
        .iter()
        .map(|field_var| super::compute_var_size(&field_var.dims).max(1))
        .sum::<usize>()
        .max(1)
}

fn component_ref_matches_record_field(
    lhs_ref: &rumoca_core::ComponentReference,
    field_ref: &rumoca_core::ComponentReference,
    field: &RecordFieldSpec,
    symbol_ancestry: &IndexMap<rumoca_core::DefId, Vec<rumoca_core::DefId>>,
    flat: &flat::Model,
) -> bool {
    let lhs_parts = lhs_ref.parts.as_slice();
    let field_parts = field_ref.parts.as_slice();
    if field_parts.len() != lhs_parts.len() + 1
        || !field.matches_component_ref(field_ref, symbol_ancestry)
    {
        return false;
    }

    let Some(lhs_leaf_index) = lhs_parts.len().checked_sub(1) else {
        return false;
    };
    let field_leaf = &field_parts[field_parts.len() - 1];
    for (lhs_index, lhs_part) in lhs_parts.iter().enumerate() {
        let field_part = &field_parts[lhs_index];
        if lhs_part.ident != field_part.ident {
            return false;
        }
        if lhs_index != lhs_leaf_index {
            if !subscripts_match_semantically(&lhs_part.subs, &field_part.subs) {
                return false;
            }
            continue;
        }
        if record_owner_subscripts_match(lhs_part, field_part, field_leaf, flat) {
            continue;
        }
        return false;
    }
    true
}

fn component_ref_is_record_field_child(
    lhs_ref: &rumoca_core::ComponentReference,
    field_ref: &rumoca_core::ComponentReference,
    flat: &flat::Model,
) -> bool {
    let lhs_parts = lhs_ref.parts.as_slice();
    let field_parts = field_ref.parts.as_slice();
    if field_parts.len() != lhs_parts.len() + 1 {
        return false;
    }

    let Some(lhs_leaf_index) = lhs_parts.len().checked_sub(1) else {
        return false;
    };
    let field_leaf = &field_parts[field_parts.len() - 1];
    for (lhs_index, lhs_part) in lhs_parts.iter().enumerate() {
        let field_part = &field_parts[lhs_index];
        if lhs_part.ident != field_part.ident {
            return false;
        }
        if lhs_index != lhs_leaf_index {
            if !subscripts_match_semantically(&lhs_part.subs, &field_part.subs) {
                return false;
            }
            continue;
        }
        if record_owner_subscripts_match(lhs_part, field_part, field_leaf, flat) {
            continue;
        }
        return false;
    }
    true
}

fn record_owner_subscripts_match(
    lhs_part: &rumoca_core::ComponentRefPart,
    field_owner_part: &rumoca_core::ComponentRefPart,
    field_leaf: &rumoca_core::ComponentRefPart,
    flat: &flat::Model,
) -> bool {
    if lhs_part.subs.is_empty() {
        return true;
    }
    if subscripts_match_semantically(&lhs_part.subs, &field_owner_part.subs) {
        return true;
    }
    if subscripts_select_field_owner(&lhs_part.subs, &field_owner_part.subs, flat) {
        return true;
    }
    field_owner_part.subs.is_empty() && subscript_prefix_matches(&field_leaf.subs, &lhs_part.subs)
}

fn subscripts_select_field_owner(
    lhs: &[rumoca_core::Subscript],
    field_owner: &[rumoca_core::Subscript],
    flat: &flat::Model,
) -> bool {
    lhs.len() == field_owner.len()
        && lhs
            .iter()
            .zip(field_owner.iter())
            .all(|(selection, concrete)| subscript_selects_index(selection, concrete, flat))
}

fn subscript_selects_index(
    selection: &rumoca_core::Subscript,
    concrete: &rumoca_core::Subscript,
    flat: &flat::Model,
) -> bool {
    let rumoca_core::Subscript::Index { value, .. } = concrete else {
        return subscript_matches_semantically(selection, concrete);
    };
    match selection {
        rumoca_core::Subscript::Index {
            value: selected, ..
        } => selected == value,
        rumoca_core::Subscript::Expr { expr, .. } => {
            expression_selects_index(expr, *value, flat).unwrap_or(false)
        }
        rumoca_core::Subscript::Colon { .. } => true,
    }
}

fn expression_selects_index(
    expr: &rumoca_core::Expression,
    index: i64,
    flat: &flat::Model,
) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            let start = eval_integer_expression(start, flat)?;
            let end = eval_integer_expression(end, flat)?;
            let step = step
                .as_deref()
                .map(|step| eval_integer_expression(step, flat))
                .unwrap_or(Some(1))?;
            if step == 0 {
                return Some(false);
            }
            Some(if step > 0 {
                index >= start && index <= end && (index - start) % step == 0
            } else {
                index <= start && index >= end && (start - index) % (-step) == 0
            })
        }
        _ => eval_integer_expression(expr, flat).map(|selected| selected == index),
    }
}

fn eval_integer_expression(expr: &rumoca_core::Expression, flat: &flat::Model) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let binding = &flat.variables.get(name.var_name())?.binding;
            let Some(binding) = binding else {
                return None;
            };
            if let Some(index) = eval_single_array_subscript(subscripts, flat) {
                let values = eval_integer_array_expression(binding, flat, 0)?;
                let selected = usize::try_from(index.checked_sub(1)?).ok()?;
                return values.get(selected).copied();
            }
            eval_integer_expression(binding, flat)
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => Some(eval_integer_expression(lhs, flat)? + eval_integer_expression(rhs, flat)?),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => Some(eval_integer_expression(lhs, flat)? - eval_integer_expression(rhs, flat)?),
        _ => None,
    }
}

fn eval_single_array_subscript(
    subscripts: &[rumoca_core::Subscript],
    flat: &flat::Model,
) -> Option<i64> {
    let [subscript] = subscripts else {
        return None;
    };
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => eval_integer_expression(expr, flat),
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn eval_integer_array_expression(
    expr: &rumoca_core::Expression,
    flat: &flat::Model,
    depth: u8,
) -> Option<Vec<i64>> {
    if depth > 8 {
        return None;
    }
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix: false,
            ..
        } => elements
            .iter()
            .map(|element| eval_integer_expression(element, flat))
            .collect(),
        rumoca_core::Expression::VarRef { name, .. } => {
            let binding = flat.variables.get(name.var_name())?.binding.as_ref()?;
            eval_integer_array_expression(binding, flat, depth + 1)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. }
            if function_name_matches(name.as_str(), "indexNonPositiveSequence")
                && args.len() == 1 =>
        {
            let m = eval_integer_expression(&args[0], flat)?;
            index_non_positive_sequence(m)
        }
        _ => None,
    }
}

fn function_name_matches(name: &str, leaf: &str) -> bool {
    name == leaf || name.rsplit('.').next() == Some(leaf)
}

fn number_of_symmetric_base_systems(m: i64) -> Option<i64> {
    if m <= 0 {
        return None;
    }
    if m % 2 != 0 || m == 2 {
        return Some(1);
    }
    Some(2 * number_of_symmetric_base_systems(m / 2)?)
}

fn index_non_positive_sequence(m: i64) -> Option<Vec<i64>> {
    let n_base = number_of_symmetric_base_systems(m)?;
    let m_base = m.checked_div(n_base)?;
    if m_base == 1 {
        return Some(Vec::new());
    }
    if m_base == 2 {
        return Some((1..=n_base).map(|k| 2 + 2 * (k - 1)).collect());
    }

    let mut values = Vec::new();
    for k in 1..=n_base {
        for value in 2..=m_base {
            values.push(value + m_base * (k - 1));
        }
    }
    Some(values)
}

fn subscripts_match_semantically(
    lhs: &[rumoca_core::Subscript],
    rhs: &[rumoca_core::Subscript],
) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs.iter())
            .all(|(lhs, rhs)| subscript_matches_semantically(lhs, rhs))
}

fn subscript_matches_semantically(
    lhs: &rumoca_core::Subscript,
    rhs: &rumoca_core::Subscript,
) -> bool {
    match (lhs, rhs) {
        (
            rumoca_core::Subscript::Index { value: lhs, .. },
            rumoca_core::Subscript::Index { value: rhs, .. },
        ) => lhs == rhs,
        (rumoca_core::Subscript::Colon { .. }, rumoca_core::Subscript::Colon { .. }) => true,
        (
            rumoca_core::Subscript::Expr { expr: lhs, .. },
            rumoca_core::Subscript::Expr { expr: rhs, .. },
        ) => rumoca_core::expressions_semantically_equal(lhs, rhs),
        _ => false,
    }
}

fn subscript_prefix_matches(
    subscripts: &[rumoca_core::Subscript],
    prefix: &[rumoca_core::Subscript],
) -> bool {
    subscripts.len() >= prefix.len()
        && subscripts
            .iter()
            .zip(prefix.iter())
            .all(|(subscript, expected)| subscript_matches_semantically(subscript, expected))
}

fn record_field_sort_key(
    lhs_ref: &rumoca_core::ComponentReference,
    field_ref: &rumoca_core::ComponentReference,
) -> Vec<i64> {
    let Some(lhs_leaf_index) = lhs_ref.parts.len().checked_sub(1) else {
        return Vec::new();
    };
    let owner_subscripts = &field_ref.parts[lhs_leaf_index].subs;
    let field_leaf_subscripts = field_ref
        .parts
        .last()
        .map(|part| part.subs.as_slice())
        .unwrap_or(&[]);
    let sort_subscripts = if owner_subscripts.is_empty() {
        field_leaf_subscripts
    } else {
        owner_subscripts
    };
    sort_subscripts
        .iter()
        .filter_map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => Some(*value),
            _ => None,
        })
        .collect()
}

fn record_field_variables<'a>(
    lhs_ref: &rumoca_core::Reference,
    field: &RecordFieldSpec,
    flat: &'a flat::Model,
    span: rumoca_core::Span,
) -> Result<Vec<&'a flat::Variable>, ToDaeError> {
    let Some(lhs_component_ref) = lhs_ref.component_ref() else {
        return Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "record equation for `{}` has no structured component reference",
                lhs_ref.as_str()
            ),
            span,
        ));
    };

    let mut field_vars = flat
        .variables
        .values()
        .filter_map(|field_var| {
            let field_ref = field_var.component_ref.as_ref()?;
            component_ref_matches_record_field(
                lhs_component_ref,
                field_ref,
                field,
                &flat.symbol_ancestry,
                flat,
            )
            .then_some((field_var, field_ref))
        })
        .collect::<Vec<_>>();

    field_vars.sort_by_key(|(_, field_ref)| record_field_sort_key(lhs_component_ref, field_ref));
    Ok(field_vars
        .into_iter()
        .map(|(field_var, _)| field_var)
        .collect())
}

fn record_field_expansion_error(
    lhs_name: &rumoca_core::Reference,
    field: &RecordFieldSpec,
    span: rumoca_core::Span,
) -> ToDaeError {
    ToDaeError::runtime_contract_violation_at(
        format!(
            "record equation for `{}` returned field `{}` but no matching flat field variable exists",
            lhs_name.as_str(),
            field.name()
        ),
        span,
    )
}

fn record_field_specs_for_lhs(
    lhs_ref: &rumoca_core::Reference,
    flat: &flat::Model,
    span: rumoca_core::Span,
) -> Result<Option<Vec<RecordFieldSpec>>, ToDaeError> {
    let Some(lhs_component_ref) = lhs_ref.component_ref() else {
        return Ok(None);
    };

    let mut fields = IndexMap::<String, rumoca_core::DefId>::new();
    for field_var in flat.variables.values() {
        if !field_var.is_primitive {
            continue;
        }
        let Some(field_ref) = field_var.component_ref.as_ref() else {
            continue;
        };
        if !component_ref_is_record_field_child(lhs_component_ref, field_ref, flat) {
            continue;
        }
        let Some(field_leaf) = field_ref.parts.last() else {
            continue;
        };
        let Some(def_id) = field_ref.def_id else {
            return Err(ToDaeError::runtime_contract_violation_at(
                format!(
                    "record equation for `{}` has field `{}` without DefId",
                    lhs_ref.as_str(),
                    field_leaf.ident
                ),
                span,
            ));
        };
        fields.entry(field_leaf.ident.clone()).or_insert(def_id);
    }

    let specs = fields
        .into_iter()
        .map(|(name, def_id)| RecordFieldSpec {
            param: rumoca_core::FunctionParam::new(name, "Real", span).with_def_id(def_id),
            match_by_name: true,
        })
        .collect::<Vec<_>>();
    Ok((!specs.is_empty()).then_some(specs))
}

fn lhs_record_reference(
    lhs: &rumoca_core::Expression,
) -> Option<(rumoca_core::Reference, rumoca_core::Span, bool)> {
    match lhs {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => Some((name.clone(), *span, false)),
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            if !base_subscripts.is_empty() {
                return None;
            }
            let mut component_ref = name.component_ref()?.clone();
            component_ref
                .parts
                .last_mut()?
                .subs
                .extend(subscripts.clone());
            Some((
                rumoca_core::Reference::from_component_reference(component_ref),
                *span,
                true,
            ))
        }
        _ => None,
    }
}

pub(crate) fn expand_record_field_equation(
    eq: &flat::Equation,
    flat: &flat::Model,
) -> Result<Option<Vec<flat::Equation>>, ToDaeError> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.residual
    else {
        return Ok(None);
    };
    let Some((lhs_name, lhs_span, lhs_is_indexed_selection)) = lhs_record_reference(lhs.as_ref())
    else {
        return Ok(None);
    };
    if flat
        .variables
        .get(lhs_name.var_name())
        .is_some_and(|var| var.is_primitive)
    {
        return Ok(None);
    }

    let field_specs = match record_field_specs_for_rhs(rhs, flat) {
        Some(field_specs) => field_specs,
        None => match record_field_specs_for_lhs(&lhs_name, flat, lhs_span)? {
            Some(field_specs) => field_specs,
            None => return Ok(None),
        },
    };
    let mut equations = Vec::new();
    for (index, field) in field_specs.iter().enumerate() {
        let field_vars = record_field_variables(&lhs_name, field, flat, lhs_span)?;
        if field_vars.is_empty() {
            if field.is_lhs_derived() || lhs_is_indexed_selection {
                continue;
            }
            return Err(record_field_expansion_error(&lhs_name, field, eq.span));
        }
        let scalar_count = field_scalar_count(&field_vars);
        equations.push(flat::Equation::new_array(
            field_residual(
                field_lhs_expression(&field_vars, eq.span),
                rhs_field_expression(rhs, field, index, flat, eq.span),
                eq.span,
            ),
            eq.span,
            eq.origin.clone(),
            scalar_count,
        ));
    }

    Ok((!equations.is_empty()).then_some(equations))
}

fn route_classified_equation(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    eq: &flat::Equation,
    dae_eq: dae::Equation,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    discrete_valued_binding_targets: &HashSet<rumoca_core::VarName>,
) -> Result<(), ToDaeError> {
    let discrete_bucket = classify_residual_discrete_bucket(dae, &eq.residual);

    if matches!(discrete_bucket, Some(ResidualDiscreteBucket::Mixed))
        && let Some(split_equations) = split_mixed_tuple_equation(dae, flat, eq)?
    {
        for split_eq in split_equations {
            let split_scalar_count = split_eq.scalar_count;
            let split_dae_eq = dae::Equation::residual_array(
                flat_to_dae_expression_with_refs(&split_eq.residual, flat)?,
                split_eq.span,
                split_eq.origin.to_string(),
                split_scalar_count,
            );
            route_classified_equation(
                dae,
                flat,
                &split_eq,
                split_dae_eq,
                discrete_valued_lhs_counts,
                discrete_valued_binding_targets,
            )?;
        }
        return Ok(());
    }

    if eq.origin.is_connection() {
        // Keep continuous alias equations in f_x, but route alias equations
        // targeting discrete variables to the discrete partitions.
        match discrete_bucket {
            Some(ResidualDiscreteBucket::DiscreteValued) => {
                if !push_explicit_discrete_assignments(
                    dae,
                    flat,
                    &dae_eq,
                    true,
                    discrete_valued_lhs_counts,
                    discrete_valued_binding_targets,
                )? {
                    dae.discrete.valued_updates.push(dae_eq);
                }
            }
            Some(ResidualDiscreteBucket::DiscreteReal | ResidualDiscreteBucket::Mixed) => {
                dae.discrete.real_updates.push(dae_eq);
            }
            None => {
                dae.continuous.equations.push(dae_eq);
            }
        }
        return Ok(());
    }

    match discrete_bucket {
        Some(ResidualDiscreteBucket::DiscreteValued) => {
            if !push_explicit_discrete_assignments(
                dae,
                flat,
                &dae_eq,
                true,
                discrete_valued_lhs_counts,
                discrete_valued_binding_targets,
            )? {
                dae.discrete.valued_updates.push(dae_eq);
            }
        }
        Some(ResidualDiscreteBucket::DiscreteReal | ResidualDiscreteBucket::Mixed) => {
            dae.discrete.real_updates.push(dae_eq);
        }
        None => {
            dae.continuous.equations.push(dae_eq);
        }
    }
    Ok(())
}

fn split_mixed_tuple_equation(
    dae: &dae::Dae,
    flat: &flat::Model,
    eq: &flat::Equation,
) -> Result<Option<Vec<flat::Equation>>, ToDaeError> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &eq.residual
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::Tuple { elements, .. } = lhs.as_ref() else {
        return Ok(None);
    };
    let rhs_elements = tuple_rhs_output_expressions(rhs, elements.len(), flat, *span)?;
    let mut split = Vec::new();
    for (lhs_element, rhs_element) in elements.iter().zip(rhs_elements) {
        if matches!(lhs_element, rumoca_core::Expression::Empty { .. }) {
            continue;
        }
        let scalar_count = tuple_lhs_element_scalar_count(dae, flat, lhs_element, eq.span)?;
        split.push(flat::Equation::new_array(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(lhs_element.clone()),
                rhs: Box::new(rhs_element),
                span: *span,
            },
            eq.span,
            eq.origin.clone(),
            scalar_count,
        ));
    }
    Ok(Some(split))
}

fn tuple_rhs_output_expressions(
    rhs: &rumoca_core::Expression,
    lhs_count: usize,
    flat: &flat::Model,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
    match rhs {
        rumoca_core::Expression::Tuple { elements, .. } => {
            if elements.len() != lhs_count {
                return Err(tuple_arity_error(lhs_count, elements.len(), span));
            }
            Ok(elements.clone())
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span: call_span,
        } => tuple_function_output_expressions(name, args, *call_span, lhs_count, flat),
        rumoca_core::Expression::FunctionCall {
            name,
            span: call_span,
            ..
        } => Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "mixed tuple assignment RHS `{}` is a constructor, not a multi-output function call",
                name.as_str()
            ),
            *call_span,
        )),
        _ => Err(ToDaeError::runtime_contract_violation_at(
            "mixed tuple assignment RHS is neither a tuple nor a multi-output function call",
            rhs.span().unwrap_or(span),
        )),
    }
}

fn tuple_function_output_expressions(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    lhs_count: usize,
    flat: &flat::Model,
) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
    let function = flat.functions.get(name.var_name()).ok_or_else(|| {
        ToDaeError::runtime_contract_violation_at(
            format!(
                "mixed tuple assignment RHS function `{}` is missing from Flat.functions",
                name.as_str()
            ),
            span,
        )
    })?;
    if function.outputs.len() != lhs_count {
        return Err(tuple_arity_error(lhs_count, function.outputs.len(), span));
    }
    Ok(function
        .outputs
        .iter()
        .map(|output| rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated(format!("{}.{}", name.as_str(), output.name)),
            args: args.to_vec(),
            is_constructor: false,
            span,
        })
        .collect())
}

fn tuple_arity_error(lhs_count: usize, rhs_count: usize, span: rumoca_core::Span) -> ToDaeError {
    ToDaeError::runtime_contract_violation_at(
        format!(
            "tuple assignment arity mismatch: {lhs_count} lhs output(s), {rhs_count} rhs output(s)"
        ),
        span,
    )
}

fn tuple_lhs_element_scalar_count(
    dae: &dae::Dae,
    flat: &flat::Model,
    lhs: &rumoca_core::Expression,
    fallback_span: rumoca_core::Span,
) -> Result<usize, ToDaeError> {
    let target = explicit_assignment_target_name(lhs).ok_or_else(|| {
        ToDaeError::runtime_contract_violation_at(
            "mixed tuple assignment LHS element is not a structured assignment target",
            lhs.span().unwrap_or(fallback_span),
        )
    })?;
    if let Some(size) = dae_variable_size(dae, &target) {
        return Ok(size.max(1));
    }
    if let Some(variable) = flat.variables.get(&target) {
        return Ok(super::compute_var_size(&variable.dims).max(1));
    }
    Err(ToDaeError::runtime_contract_violation_at(
        format!("tuple assignment target `{target}` has no Flat or DAE variable metadata"),
        lhs.span().unwrap_or(fallback_span),
    ))
}

fn dae_variable_size(dae: &dae::Dae, target: &rumoca_core::VarName) -> Option<usize> {
    let name = flat_to_dae_var_name(target);
    dae.variables
        .states
        .get(&name)
        .or_else(|| dae.variables.algebraics.get(&name))
        .or_else(|| dae.variables.outputs.get(&name))
        .or_else(|| dae.variables.inputs.get(&name))
        .or_else(|| dae.variables.discrete_reals.get(&name))
        .or_else(|| dae.variables.discrete_valued.get(&name))
        .or_else(|| dae.variables.parameters.get(&name))
        .or_else(|| dae.variables.constants.get(&name))
        .map(|variable| variable.size())
}

fn is_numeric_zero(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            ..
        } => true,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

fn pre_of_target(
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![rumoca_core::Expression::VarRef {
            name: target.clone().into(),
            subscripts: vec![],
            span,
        }],
        span,
    }
}

fn const_subscript_index_expr(expr: &rumoca_core::Expression) -> Option<i64> {
    let value = match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => *value as f64,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => *value,
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => -const_subscript_index_expr(rhs)? as f64,
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Plus,
            rhs,
            ..
        } => const_subscript_index_expr(rhs)? as f64,
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = const_subscript_index_expr(lhs)? as f64;
            let rhs = const_subscript_index_expr(rhs)? as f64;
            match op {
                rumoca_core::OpBinary::Add => lhs + rhs,
                rumoca_core::OpBinary::Sub => lhs - rhs,
                rumoca_core::OpBinary::Mul => lhs * rhs,
                rumoca_core::OpBinary::Div => lhs / rhs,
                _ => return None,
            }
        }
        _ => return None,
    };

    (value.is_finite() && value.fract() == 0.0).then_some(value as i64)
}

fn render_subscript(subscript: &rumoca_core::Subscript) -> String {
    match subscript {
        rumoca_core::Subscript::Index { value: index, .. } => index.to_string(),
        rumoca_core::Subscript::Colon { .. } => ":".to_string(),
        // MLS Chapter 10 indexing: explicit assignment targets must resolve
        // constant scalar subscripts to the concrete element name they define.
        rumoca_core::Subscript::Expr { expr, .. } => const_subscript_index_expr(expr)
            .map_or_else(|| format!("{expr:?}"), |index| index.to_string()),
    }
}

fn append_rendered_subscripts(
    name: &str,
    subscripts: &[rumoca_core::Subscript],
) -> rumoca_core::VarName {
    if subscripts.is_empty() {
        return rumoca_core::VarName::new(name);
    }
    let rendered = subscripts
        .iter()
        .map(render_subscript)
        .collect::<Vec<_>>()
        .join(",");
    rumoca_core::VarName::new(format!("{name}[{rendered}]"))
}

struct ExplicitAssignmentTarget {
    name: rumoca_core::VarName,
    has_subscripts: bool,
}

fn explicit_lhs_reference_from_target(
    target: &rumoca_core::VarName,
    flat: &flat::Model,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, ToDaeError> {
    if let Some(variable) = flat.variables.get(target)
        && let Some(component_ref) = variable.component_ref.clone()
    {
        return Ok(rumoca_core::Reference::with_component_reference(
            target.as_str(),
            component_ref,
        ));
    }
    if let Some(scalar_name) = rumoca_core::parse_scalar_name(target.as_str())
        && let Some(variable) = flat
            .variables
            .get(&rumoca_core::VarName::new(scalar_name.base))
        && let Some(component_ref) = variable.component_ref.clone()
    {
        return Ok(rumoca_core::Reference::with_component_reference(
            target.as_str(),
            component_reference_with_scalar_indices(
                component_ref,
                &scalar_name.indices,
                explicit_target_subscript_span(variable.source_span, span)?,
            )?,
        ));
    }
    Err(ToDaeError::runtime_contract_violation_at(
        format!(
            "explicit discrete assignment target `{target}` lost structured component-reference metadata"
        ),
        flat.variables
            .get(target)
            .map(|variable| variable.source_span)
            .unwrap_or(span),
    ))
}

fn component_reference_with_scalar_indices(
    mut component_ref: rumoca_core::ComponentReference,
    indices: &[i64],
    span: rumoca_core::ProvenanceSpan,
) -> Result<rumoca_core::ComponentReference, ToDaeError> {
    if let Some(part) = component_ref.parts.last_mut() {
        for index in indices {
            part.subs
                .push(rumoca_core::Subscript::generated_index_with_provenance(
                    *index, span,
                ));
        }
    }
    Ok(component_ref)
}

fn explicit_target_subscript_span(
    variable_span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::ProvenanceSpan, ToDaeError> {
    owner_span
        .require_provenance("explicit discrete assignment target subscript")
        .map_err(|err| {
            if variable_span.is_dummy() {
                ToDaeError::runtime_metadata_violation(err.to_string())
            } else {
                ToDaeError::runtime_metadata_violation_at(err.to_string(), variable_span)
            }
        })
}

fn explicit_assignment_target(expr: &rumoca_core::Expression) -> Option<ExplicitAssignmentTarget> {
    match expr {
        // MLS Appendix B B.1c with MLS Chapter 10 indexing: explicit discrete
        // assignments must preserve the full indexed/field-qualified target.
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Some(ExplicitAssignmentTarget {
            name: append_rendered_subscripts(name.as_str(), subscripts),
            has_subscripts: !subscripts.is_empty(),
        }),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = explicit_assignment_target(base)?;
            Some(ExplicitAssignmentTarget {
                name: append_rendered_subscripts(base.name.as_str(), subscripts),
                has_subscripts: base.has_subscripts || !subscripts.is_empty(),
            })
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let base = explicit_assignment_target(base)?;
            Some(ExplicitAssignmentTarget {
                name: rumoca_core::VarName::new(format!("{}.{}", base.name.as_str(), field)),
                has_subscripts: base.has_subscripts,
            })
        }
        _ => None,
    }
}

fn explicit_assignment_target_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    explicit_assignment_target(expr).map(|target| target.name)
}

fn collect_explicit_discrete_assignments(
    expr: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
) -> Result<Option<HashMap<rumoca_core::VarName, rumoca_core::Expression>>, ToDaeError> {
    collect_explicit_discrete_assignments_with_binding_targets(
        expr,
        dae,
        discrete_valued_lhs_counts,
        equation_span,
        &HashSet::new(),
    )
}

fn collect_explicit_discrete_assignments_with_binding_targets(
    expr: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
    binding_targets: &HashSet<rumoca_core::VarName>,
) -> Result<Option<HashMap<rumoca_core::VarName, rumoca_core::Expression>>, ToDaeError> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => collect_binary_explicit_discrete_assignments(
            lhs,
            rhs,
            dae,
            discrete_valued_lhs_counts,
            equation_span,
            binding_targets,
        ),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => collect_if_explicit_discrete_assignments(
            branches,
            else_branch,
            dae,
            discrete_valued_lhs_counts,
            equation_span,
        ),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => collect_explicit_discrete_assignments_with_binding_targets(
            rhs,
            dae,
            discrete_valued_lhs_counts,
            equation_span,
            binding_targets,
        ),
        _ => Ok(None),
    }
}

fn collect_binary_explicit_discrete_assignments(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
    binding_targets: &HashSet<rumoca_core::VarName>,
) -> Result<Option<HashMap<rumoca_core::VarName, rumoca_core::Expression>>, ToDaeError> {
    if let Some(assignments) = collect_oriented_discrete_alias_assignment(
        lhs,
        rhs,
        dae,
        discrete_valued_lhs_counts,
        equation_span,
        binding_targets,
    )? {
        return Ok(Some(assignments));
    }
    if is_numeric_zero(lhs) {
        return collect_zero_rhs_discrete_assignment(
            rhs,
            dae,
            discrete_valued_lhs_counts,
            equation_span,
        );
    }
    if is_numeric_zero(rhs) {
        return collect_zero_rhs_discrete_assignment(
            lhs,
            dae,
            discrete_valued_lhs_counts,
            equation_span,
        );
    }
    let Some(name) = explicit_assignment_target_name(lhs) else {
        return Ok(None);
    };
    let mut result = HashMap::new();
    result.insert(name, rhs.clone());
    Ok(Some(result))
}

fn collect_zero_rhs_discrete_assignment(
    expr: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
) -> Result<Option<HashMap<rumoca_core::VarName, rumoca_core::Expression>>, ToDaeError> {
    if let Some(assignments) =
        collect_explicit_discrete_assignments(expr, dae, discrete_valued_lhs_counts, equation_span)?
    {
        return Ok(Some(assignments));
    }
    let Some(target) = explicit_assignment_target_name(expr) else {
        return Ok(None);
    };
    let mut result = HashMap::new();
    result.insert(target, integer_zero_expr(equation_span)?);
    Ok(Some(result))
}

fn integer_zero_expr(span: rumoca_core::Span) -> Result<rumoca_core::Expression, ToDaeError> {
    let owner = span
        .require_provenance("explicit discrete assignment zero")
        .map_err(|err| ToDaeError::runtime_metadata_violation(err.to_string()))?;
    Ok(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(0),
        span: owner.span(),
    })
}

fn collect_if_explicit_discrete_assignments(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
) -> Result<Option<HashMap<rumoca_core::VarName, rumoca_core::Expression>>, ToDaeError> {
    let Some((branch_maps, else_map)) = collect_discrete_assignment_branches(
        branches,
        else_branch,
        dae,
        discrete_valued_lhs_counts,
        equation_span,
    )?
    else {
        return Ok(None);
    };
    let target_order = ordered_discrete_assignment_targets(&branch_maps, &else_map);
    Ok(Some(build_if_discrete_assignment_map(
        target_order,
        &branch_maps,
        &else_map,
        equation_span,
    )))
}

type DiscreteAssignmentMap = HashMap<rumoca_core::VarName, rumoca_core::Expression>;
type DiscreteBranchAssignments = Vec<(rumoca_core::Expression, DiscreteAssignmentMap)>;

fn collect_discrete_assignment_branches(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
) -> Result<Option<(DiscreteBranchAssignments, DiscreteAssignmentMap)>, ToDaeError> {
    let mut branch_maps = Vec::new();
    for (condition, value) in branches {
        let Some(assignments) = collect_explicit_discrete_assignments(
            value,
            dae,
            discrete_valued_lhs_counts,
            equation_span,
        )?
        else {
            return Ok(None);
        };
        branch_maps.push((condition.clone(), assignments));
    }
    let Some(else_map) = collect_explicit_discrete_assignments(
        else_branch,
        dae,
        discrete_valued_lhs_counts,
        equation_span,
    )?
    else {
        return Ok(None);
    };
    Ok(Some((branch_maps, else_map)))
}

fn ordered_discrete_assignment_targets(
    branch_maps: &DiscreteBranchAssignments,
    else_map: &DiscreteAssignmentMap,
) -> Vec<rumoca_core::VarName> {
    let mut target_order = Vec::<rumoca_core::VarName>::new();
    let mut seen = HashSet::new();
    for (_, assignments) in branch_maps {
        for target in assignments.keys() {
            push_unique_target(&mut target_order, &mut seen, target);
        }
    }
    for target in else_map.keys() {
        push_unique_target(&mut target_order, &mut seen, target);
    }
    target_order
}

fn build_if_discrete_assignment_map(
    target_order: Vec<rumoca_core::VarName>,
    branch_maps: &DiscreteBranchAssignments,
    else_map: &DiscreteAssignmentMap,
    equation_span: rumoca_core::Span,
) -> DiscreteAssignmentMap {
    let mut result = HashMap::new();
    for target in target_order {
        result.insert(
            target.clone(),
            build_if_discrete_assignment_expr(&target, branch_maps, else_map, equation_span),
        );
    }
    result
}

fn build_if_discrete_assignment_expr(
    target: &rumoca_core::VarName,
    branch_maps: &DiscreteBranchAssignments,
    else_map: &DiscreteAssignmentMap,
    equation_span: rumoca_core::Span,
) -> rumoca_core::Expression {
    let branch_values = branch_maps
        .iter()
        .map(|(condition, assignments)| {
            let rhs = assignments
                .get(target)
                .cloned()
                .unwrap_or_else(|| pre_of_target(target, equation_span));
            (condition.clone(), rhs)
        })
        .collect();
    let else_value = else_map
        .get(target)
        .cloned()
        .unwrap_or_else(|| pre_of_target(target, equation_span));
    rumoca_core::Expression::If {
        branches: branch_values,
        else_branch: Box::new(else_value),
        span: equation_span,
    }
}

fn collect_oriented_discrete_alias_assignment(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae: &dae::Dae,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    equation_span: rumoca_core::Span,
    binding_targets: &HashSet<rumoca_core::VarName>,
) -> Result<Option<HashMap<rumoca_core::VarName, rumoca_core::Expression>>, ToDaeError> {
    let Some(lhs_target) = explicit_assignment_target(lhs) else {
        return Ok(None);
    };
    let Some(rhs_target) = explicit_assignment_target(rhs) else {
        return Ok(None);
    };
    if lhs_target.name == rhs_target.name {
        return Ok(None);
    }
    if lhs_target.has_subscripts {
        return Ok(None);
    }
    if !is_discrete_valued_target(dae, &lhs_target.name)
        || !is_discrete_valued_target(dae, &rhs_target.name)
    {
        return Ok(None);
    }

    if binding_targets.contains(&lhs_target.name) && !binding_targets.contains(&rhs_target.name) {
        let mut result = HashMap::new();
        result.insert(rhs_target.name, lhs.clone());
        return Ok(Some(result));
    }
    if binding_targets.contains(&rhs_target.name) && !binding_targets.contains(&lhs_target.name) {
        let mut result = HashMap::new();
        result.insert(lhs_target.name, rhs.clone());
        return Ok(Some(result));
    }

    let lhs_definitions = required_discrete_lhs_count(
        &lhs_target.name,
        discrete_valued_lhs_counts,
        expression_or_context_span(lhs, equation_span),
    )?;
    let rhs_definitions = required_discrete_lhs_count(
        &rhs_target.name,
        discrete_valued_lhs_counts,
        expression_or_context_span(rhs, equation_span),
    )?;
    if lhs_definitions <= 1 || rhs_definitions != 0 {
        return Ok(None);
    }

    let mut result = HashMap::new();
    result.insert(rhs_target.name, lhs.clone());
    Ok(Some(result))
}

fn expression_or_context_span(
    expr: &rumoca_core::Expression,
    context_span: rumoca_core::Span,
) -> rumoca_core::Span {
    match expr.span() {
        Some(span) => span,
        None => context_span,
    }
}

fn lookup_discrete_lhs_count(
    target: &rumoca_core::VarName,
    counts: &HashMap<rumoca_core::VarName, usize>,
) -> Option<usize> {
    counts.get(target).copied()
}

fn required_discrete_lhs_count(
    target: &rumoca_core::VarName,
    counts: &HashMap<rumoca_core::VarName, usize>,
    span: rumoca_core::Span,
) -> Result<usize, ToDaeError> {
    lookup_discrete_lhs_count(target, counts).ok_or_else(|| {
        ToDaeError::runtime_contract_violation_at(
            format!("missing discrete-valued LHS definition count for `{target}`"),
            span,
        )
    })
}

fn is_discrete_valued_target(dae: &dae::Dae, target: &rumoca_core::VarName) -> bool {
    dae.variables
        .discrete_valued
        .contains_key(&flat_to_dae_var_name(target))
}

fn push_unique_target(
    target_order: &mut Vec<rumoca_core::VarName>,
    seen: &mut HashSet<rumoca_core::VarName>,
    target: &rumoca_core::VarName,
) {
    if !seen.insert(target.clone()) {
        return;
    }
    target_order.push(target.clone());
}

fn discrete_target_scalar_count(
    dae: &dae::Dae,
    target: &rumoca_core::VarName,
    equation_scalar_count: usize,
) -> usize {
    if let Some(variable) = dae
        .variables
        .discrete_valued
        .get(&flat_to_dae_var_name(target))
    {
        return variable.size().max(1);
    }

    equation_scalar_count.max(1)
}

fn push_explicit_discrete_assignments(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    equation: &dae::Equation,
    discrete_valued: bool,
    discrete_valued_lhs_counts: &HashMap<rumoca_core::VarName, usize>,
    discrete_valued_binding_targets: &HashSet<rumoca_core::VarName>,
) -> Result<bool, ToDaeError> {
    let rhs = crate::dae_to_flat_expression(&equation.rhs);
    let Some(assignments) = collect_explicit_discrete_assignments_with_binding_targets(
        &rhs,
        dae,
        discrete_valued_lhs_counts,
        equation.span,
        discrete_valued_binding_targets,
    )?
    else {
        return Ok(false);
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
            explicit_lhs_reference_from_target(&target, flat, equation.span)?,
            flat_to_dae_expression_with_refs(&rhs, flat)?,
            equation.span,
            format!("explicit {}", equation.origin),
            scalar_count,
        );
        if discrete_valued {
            dae.discrete.valued_updates.push(explicit);
        } else {
            dae.discrete.real_updates.push(explicit);
        }
    }

    Ok(true)
}

pub(super) fn classify_equations(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Result<(), ToDaeError> {
    let outputs_with_component_eqs = collect_vars_with_component_equations(flat);
    let non_connection_rhs_var_refs = collect_non_connection_rhs_var_refs(flat);
    let top_level_oc_connectors = collect_top_level_overconstrained_connectors(flat);
    let linearized_embedded_lhs_bases = super::collect_linearized_embedded_lhs_bases(flat);
    let discrete_valued_lhs_counts = collect_discrete_valued_lhs_target_counts(dae, flat);
    let discrete_valued_binding_targets = collect_discrete_valued_binding_targets(dae, flat);
    let debug_eq_filter = crate::equation_filter_debug_enabled();
    let mut stats = EqFilterStats::default();
    let filter_ctx = EqFilterContext {
        flat,
        outputs_with_component_eqs: &outputs_with_component_eqs,
        non_connection_rhs_var_refs: &non_connection_rhs_var_refs,
        top_level_oc_connectors: &top_level_oc_connectors,
        debug_eq_filter,
    };

    let mut flat_to_fx_index: IndexMap<usize, usize> = IndexMap::new();

    for (flat_idx, eq) in flat.equations.iter().enumerate() {
        if skip_equation_pre_classification(eq, &filter_ctx, dae, &mut stats) {
            continue;
        }

        let fx_index_before = dae.continuous.equations.len();
        let expanded_equations =
            expand_record_field_equation(eq, flat)?.unwrap_or_else(|| vec![eq.clone()]);
        for expanded_eq in &expanded_equations {
            let Some(scalar_count) = compute_scalar_count(
                expanded_eq,
                flat,
                prefix_counts,
                &linearized_embedded_lhs_bases,
                &mut stats,
                debug_eq_filter,
            ) else {
                continue;
            };

            log_kept(debug_eq_filter, expanded_eq, scalar_count);
            if debug_eq_filter {
                stats.record_kept(&expanded_eq.origin);
            }
            let dae_eq = dae::Equation::residual_array(
                flat_to_dae_expression_with_refs(&expanded_eq.residual, flat)?,
                expanded_eq.span,
                expanded_eq.origin.to_string(),
                scalar_count,
            );
            route_classified_equation(
                dae,
                flat,
                expanded_eq,
                dae_eq,
                &discrete_valued_lhs_counts,
                &discrete_valued_binding_targets,
            )?;
        }
        if dae.continuous.equations.len() == fx_index_before + 1 {
            flat_to_fx_index.insert(flat_idx, fx_index_before);
        }
    }

    dae.continuous.structured_equations =
        remap_flat_structured_equations(&flat.structured_equations, &flat_to_fx_index);

    if debug_eq_filter {
        stats.log();
    }
    Ok(())
}

fn collect_discrete_valued_lhs_target_counts(
    dae: &dae::Dae,
    flat: &flat::Model,
) -> HashMap<rumoca_core::VarName, usize> {
    let mut counts = HashMap::new();
    for target in dae.variables.discrete_valued.keys() {
        counts.insert(crate::dae_to_flat_var_name(target), 0);
    }
    for equation in &flat.equations {
        if classify_residual_discrete_bucket(dae, &equation.residual)
            != Some(ResidualDiscreteBucket::DiscreteValued)
        {
            continue;
        }
        for target in crate::discrete_partition::residual_lhs_targets(&equation.residual) {
            if is_discrete_valued_target(dae, &target) {
                *counts.entry(target).or_insert(0) += 1;
            }
        }
    }
    counts
}

fn collect_discrete_valued_binding_targets(
    dae: &dae::Dae,
    flat: &flat::Model,
) -> HashSet<rumoca_core::VarName> {
    flat.variables
        .iter()
        .filter(|(name, variable)| {
            variable.binding.is_some() && is_discrete_valued_target(dae, name)
        })
        .map(|(name, _)| name.clone())
        .collect()
}

#[cfg(test)]
mod tests;
