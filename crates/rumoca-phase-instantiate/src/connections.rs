//! Connection extraction for the instantiate phase (MLS §9).
//!
//! This module extracts connect() statements from equations and converts
//! them to ast::InstanceConnection structs.

use rumoca_core::{ComponentPath, SourceMap, Span, scoped_component_path_candidates};
use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, OuterValues, evaluate_component_condition_with_outer_values,
};
use rumoca_ir_ast as ast;

use crate::errors::{InstantiateError, InstantiateResult};
use crate::inheritance::required_location_to_span;

/// Parameters for connection extraction, including both boolean and integer values.
#[derive(Default)]
pub struct ConnectionParams<'a> {
    /// Boolean parameters for evaluating conditional branches.
    pub bools: rustc_hash::FxHashMap<String, bool>,
    /// Integer parameters for evaluating for-loop ranges.
    pub integers: rustc_hash::FxHashMap<String, i64>,
    /// Real structural values used by the shared AST scalar evaluator.
    pub reals: rustc_hash::FxHashMap<String, f64>,
    /// Declaration/modifier context for the canonical structural evaluator.
    pub eval_ctx: Option<&'a InstantiateEvalCtx<'a>>,
}

impl ConnectionParams<'_> {
    /// Create a new ConnectionParams with no values.
    pub fn new() -> Self {
        Self::default()
    }
}

/// Extract connection statements from a list of equations (MLS §9).
///
/// Recursively extracts `connect(A, B)` from nested structures
/// like if-equations and for-equations, evaluating conditions using
/// the provided parameter context.
pub fn extract_connections(
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
) -> InstantiateResult<Vec<ast::InstanceConnection>> {
    if connection_params_debug_enabled() {
        let mut ints: Vec<_> = params.integers.iter().collect();
        ints.sort_by(|a, b| a.0.cmp(b.0));
        let sample = ints
            .iter()
            .take(80)
            .map(|(key, value)| format!("{key}={value}"))
            .collect::<Vec<_>>()
            .join(", ");
        log_connection_params_debug(format!(
            "extract_connections prefix={} int_params={} sample=[{}]",
            prefix,
            ints.len(),
            sample
        ));
    }

    let mut connections = Vec::new();
    let mut expansion_budget = ConnectionExpansionBudget::new();

    for eq in equations {
        extract_connections_from_equation(
            &mut connections,
            eq,
            prefix,
            params,
            source_map,
            &mut expansion_budget,
        )?;
    }

    Ok(connections)
}

fn connection_params_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(
            target: "rumoca_phase_instantiate::connections",
            tracing::Level::DEBUG
        )
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

fn log_connection_params_debug(message: String) {
    #[cfg(feature = "tracing")]
    tracing::debug!(target: "rumoca_phase_instantiate::connections", message = %message);

    #[cfg(not(feature = "tracing"))]
    let _ = message;
}

/// Extract connections from an equation, recursively handling nested structures.
fn extract_connections_from_equation(
    connections: &mut Vec<ast::InstanceConnection>,
    eq: &ast::Equation,
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
    expansion_budget: &mut ConnectionExpansionBudget,
) -> InstantiateResult<()> {
    match eq {
        ast::Equation::Connect { lhs, rhs, .. } => {
            let span =
                required_location_to_span(eq.get_location(), source_map, "connect equation")?;
            validate_connection_subscripts(lhs, span)?;
            validate_connection_subscripts(rhs, span)?;

            // Preserve range/slice connects as one authoritative structured
            // family. Flattening derives the scalar union-find view once.
            if let Some(connection) =
                try_compact_range_subscript_connection(lhs, rhs, prefix, &params.integers, span)?
            {
                connections.push(connection);
            } else {
                let a = component_ref_to_qualified_name(lhs, prefix, &params.integers, span)?;
                let b = component_ref_to_qualified_name(rhs, prefix, &params.integers, span)?;

                connections.push(
                    ast::InstanceConnection::scalar(
                        a,
                        b,
                        None, // Resolved later during flattening
                        span,
                        prefix.to_flat_string(),
                    )
                    .map_err(|error| invalid_connection(error, span))?,
                );
            }
            Ok(())
        }

        ast::Equation::If {
            cond_blocks,
            else_block,
        } => extract_connections_from_if_equation(
            connections,
            cond_blocks,
            else_block,
            prefix,
            params,
            source_map,
            expansion_budget,
        ),

        ast::Equation::For { indices, equations } => {
            // For for-equations, expand the loop and extract connections from each iteration
            // MLS §8.3.3: for-equations iterate over a set of equations
            extract_connections_from_for_equation(
                connections,
                indices,
                equations,
                prefix,
                params,
                source_map,
                expansion_budget,
            )
        }

        // Other equation types don't contain connections
        _ => Ok(()),
    }
}

fn validate_connection_subscripts(
    reference: &ast::ComponentReference,
    span: Span,
) -> InstantiateResult<()> {
    let unsupported = reference
        .parts
        .iter()
        .flat_map(|part| part.subs.iter().flatten())
        .find_map(|subscript| match subscript {
            ast::Subscript::Range { .. } => Some((
                "`:`".to_string(),
                "whole-dimension selection is not yet supported",
                span,
            )),
            _ => ast::subscript_required_value_violation(subscript).map(|violation| {
                let selector = match subscript {
                    ast::Subscript::Empty => "Subscript::Empty".to_string(),
                    _ => format!("`{subscript}`"),
                };
                let violation_span = violation.span.unwrap_or(span);
                (selector, violation.kind.description(), violation_span)
            }),
        });
    if let Some((selector, reason, span)) = unsupported {
        return Err(Box::new(
            InstantiateError::unsupported_connection_subscript(selector, reason.to_string(), span),
        ));
    }
    Ok(())
}

/// Extract connections from an if-equation.
///
/// A conditional connection is structural: exactly one branch owns its
/// connection set. If that branch cannot be decided during translation, there
/// is no conservative union of the branches -- doing so would fabricate
/// connections that the source program may have disabled.
fn extract_connections_from_if_equation(
    connections: &mut Vec<ast::InstanceConnection>,
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
    expansion_budget: &mut ConnectionExpansionBudget,
) -> InstantiateResult<()> {
    let contains_connection = cond_blocks
        .iter()
        .any(|block| equations_contain_connect(&block.eqs))
        || else_block.as_deref().is_some_and(equations_contain_connect);
    if !contains_connection {
        return Ok(());
    }

    for block in cond_blocks {
        let Some(enabled) = evaluate_connection_condition(&block.cond, prefix, params) else {
            return Err(Box::new(InstantiateError::structural_param_error(
                block.cond.to_string(),
                "cannot decide a connection if-equation branch during translation".to_string(),
                required_location_to_span(
                    block.cond.get_location(),
                    source_map,
                    "connection if-equation condition",
                )?,
            )));
        };
        if enabled {
            for nested_eq in &block.eqs {
                extract_connections_from_equation(
                    connections,
                    nested_eq,
                    prefix,
                    params,
                    source_map,
                    expansion_budget,
                )?;
            }
            return Ok(());
        }
    }

    if let Some(else_eqs) = else_block {
        for nested_eq in else_eqs {
            extract_connections_from_equation(
                connections,
                nested_eq,
                prefix,
                params,
                source_map,
                expansion_budget,
            )?;
        }
    }
    Ok(())
}

fn evaluate_connection_condition(
    condition: &ast::Expression,
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
) -> Option<bool> {
    params
        .eval_ctx
        .and_then(|eval_ctx| {
            evaluate_component_condition_with_outer_values(
                eval_ctx,
                condition,
                OuterValues::new(&params.bools, &params.reals),
            )
        })
        .or_else(|| try_eval_bool_expr(condition, &params.bools, &params.integers, prefix))
}

/// Extract connections from a for-equation by expanding the loop.
///
/// MLS §8.3.3: For-equations iterate over a set of equations.
/// For connections, we need to expand the loop and substitute the index
/// variable in subscripts with concrete values.
fn extract_connections_from_for_equation(
    connections: &mut Vec<ast::InstanceConnection>,
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
    expansion_budget: &mut ConnectionExpansionBudget,
) -> InstantiateResult<()> {
    if !equations_contain_connect(equations) {
        return Ok(());
    }

    if indices.is_empty() {
        return extract_indexless_connections(
            connections,
            equations,
            prefix,
            params,
            source_map,
            expansion_budget,
        );
    }

    let binder_names = indices
        .iter()
        .map(|index| index.ident.text.as_ref())
        .collect::<Vec<_>>();
    let selected_equations =
        select_invariant_connection_branches(equations, &binder_names, prefix, params, source_map)?;
    if !equations_contain_connect(&selected_equations) {
        return Ok(());
    }

    if let Some(families) = try_extract_regular_connection_families(
        indices,
        &selected_equations,
        prefix,
        params,
        source_map,
    )? {
        connections.extend(families);
        return Ok(());
    }

    require_materialized_connection_budget(
        indices,
        &selected_equations,
        prefix,
        params,
        source_map,
        expansion_budget,
    )?;

    let first_index = &indices[0];
    let remaining_indices = &indices[1..];
    let index_name = &first_index.ident.text;

    // Try to evaluate the range to get concrete index values, using integer params.
    match expand_for_range(
        &first_index.range,
        &params.integers,
        prefix,
        expansion_budget,
    ) {
        ForRangeExpansion::Values(range_values) => {
            for value in range_values {
                let (substituted_indices, shadowed) =
                    substitute_index_in_for_indices(remaining_indices, index_name, value);
                let substituted = if shadowed {
                    selected_equations.clone()
                } else {
                    selected_equations
                        .iter()
                        .map(|equation| substitute_index_in_equation(equation, index_name, value))
                        .collect()
                };

                // Recursively process with remaining indices
                extract_connections_from_for_equation(
                    connections,
                    &substituted_indices,
                    &substituted,
                    prefix,
                    params,
                    source_map,
                    expansion_budget,
                )?;
            }
            return Ok(());
        }
        ForRangeExpansion::MaterializationLimit { count, remaining } => {
            return connection_materialization_limit_error(
                first_index,
                index_name,
                count,
                remaining,
                source_map,
            );
        }
        ForRangeExpansion::Unevaluable => {}
    }

    unevaluable_connection_range_error(first_index, index_name, prefix, source_map)
}

fn extract_indexless_connections(
    connections: &mut Vec<ast::InstanceConnection>,
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
    expansion_budget: &mut ConnectionExpansionBudget,
) -> InstantiateResult<()> {
    for equation in equations {
        extract_connections_from_equation(
            connections,
            equation,
            prefix,
            params,
            source_map,
            expansion_budget,
        )?;
    }
    Ok(())
}

fn connection_materialization_limit_error(
    index: &rumoca_ir_ast::ForIndex,
    index_name: &str,
    count: usize,
    remaining: usize,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    Err(Box::new(InstantiateError::structural_param_error(
        index_name.to_string(),
        format!(
            "connection for-equation fallback would materialize {count} iterations; SPEC_0032 structural-work limit is {MAX_MATERIALIZED_CONNECTION_ITERATIONS} and remaining transaction budget is {remaining}"
        ),
        required_location_to_span(
            index.range.get_location(),
            source_map,
            "connection for-equation range",
        )?,
    )))
}

fn unevaluable_connection_range_error(
    index: &rumoca_ir_ast::ForIndex,
    index_name: &str,
    prefix: &ast::QualifiedName,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    Err(Box::new(InstantiateError::structural_param_error(
        index_name.to_string(),
        format!(
            "cannot evaluate connection for-equation range `{}` in `{prefix}`",
            index.range
        ),
        required_location_to_span(
            index.range.get_location(),
            source_map,
            "connection for-equation range",
        )?,
    )))
}

fn require_materialized_connection_budget(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
    expansion_budget: &ConnectionExpansionBudget,
) -> InstantiateResult<()> {
    let Some(work) =
        estimate_materialized_connection_work(indices, equations, &params.integers, prefix)
    else {
        return Ok(());
    };
    if work > expansion_budget.remaining {
        return Err(Box::new(InstantiateError::structural_param_error(
            indices[0].ident.text.to_string(),
            format!(
                "connection for-equation fallback requires {work} iterations (would materialize {work}); SPEC_0032 structural-work limit is {MAX_MATERIALIZED_CONNECTION_ITERATIONS} and remaining transaction budget is {}",
                expansion_budget.remaining
            ),
            required_location_to_span(
                indices[0].range.get_location(),
                source_map,
                "connection for-equation range",
            )?,
        )));
    }
    Ok(())
}

fn select_invariant_connection_branches(
    equations: &[ast::Equation],
    binder_names: &[&str],
    prefix: &ast::QualifiedName,
    params: &ConnectionParams<'_>,
    source_map: &SourceMap,
) -> InstantiateResult<Vec<ast::Equation>> {
    let mut selected = Vec::new();
    for equation in equations {
        let ast::Equation::If {
            cond_blocks,
            else_block,
        } = equation
        else {
            selected.push(equation.clone());
            continue;
        };
        if cond_blocks.iter().any(|block| {
            rumoca_ir_ast::collect_component_refs(&block.cond)
                .iter()
                .any(|reference| {
                    matches!(reference.parts.as_slice(), [part]
                        if binder_names.contains(&part.ident.text.as_ref()))
                })
        }) {
            selected.push(equation.clone());
            continue;
        }

        let mut chosen = else_block.as_deref().unwrap_or_default();
        for block in cond_blocks {
            let Some(enabled) = evaluate_connection_condition(&block.cond, prefix, params) else {
                return Err(Box::new(InstantiateError::structural_param_error(
                    block.cond.to_string(),
                    "cannot decide a connection if-equation branch during translation".to_string(),
                    required_location_to_span(
                        block.cond.get_location(),
                        source_map,
                        "connection if-equation condition",
                    )?,
                )));
            };
            if enabled {
                chosen = &block.eqs;
                break;
            }
        }
        selected.extend(chosen.iter().cloned());
    }
    Ok(selected)
}

fn try_extract_regular_connection_families(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    params: &ConnectionParams,
    source_map: &SourceMap,
) -> InstantiateResult<Option<Vec<ast::InstanceConnection>>> {
    let (indices, equations) = rectangular_connection_body(indices, equations);
    let Some(domain) = regular_connection_domain(&indices, prefix, &params.integers) else {
        return Ok(None);
    };
    let binder_names = indices
        .iter()
        .map(|index| index.ident.text.as_ref())
        .collect::<Vec<_>>();
    if equations.iter().any(|equation| {
        matches!(
            equation,
            ast::Equation::For { .. } | ast::Equation::If { .. }
        )
    }) {
        return Ok(None);
    }
    let mut result = Vec::new();
    for equation in equations {
        let ast::Equation::Connect { lhs, rhs } = equation else {
            continue;
        };
        let Some(a_template) =
            connection_endpoint_template(lhs, prefix, &binder_names, &params.integers)?
        else {
            return Ok(None);
        };
        let Some(b_template) =
            connection_endpoint_template(rhs, prefix, &binder_names, &params.integers)?
        else {
            return Ok(None);
        };
        let span = required_location_to_span(
            equation.get_location(),
            source_map,
            "vectorized connect equation",
        )?;
        result.push(
            ast::InstanceConnection::family(
                domain.clone(),
                a_template,
                b_template,
                None,
                span,
                prefix.to_flat_string(),
            )
            .map_err(|error| invalid_connection(error, span))?,
        );
    }
    Ok((!result.is_empty()).then_some(result))
}

fn rectangular_connection_body<'a>(
    indices: &'a [rumoca_ir_ast::ForIndex],
    equations: &'a [ast::Equation],
) -> (Vec<&'a rumoca_ir_ast::ForIndex>, &'a [ast::Equation]) {
    let mut all_indices = indices.iter().collect::<Vec<_>>();
    let mut body = equations;
    while let [
        ast::Equation::For {
            indices: nested,
            equations: nested_body,
        },
    ] = body
    {
        all_indices.extend(nested);
        body = nested_body;
    }
    (all_indices, body)
}

fn regular_connection_domain(
    indices: &[&rumoca_ir_ast::ForIndex],
    prefix: &ast::QualifiedName,
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> Option<rumoca_core::StructuredIndexDomain> {
    let mut names = std::collections::HashSet::new();
    let mut binders = Vec::new();
    for (id, index) in indices.iter().enumerate() {
        let name = index.ident.text.as_ref();
        if !names.insert(name) {
            return None;
        }
        let (lower, step, upper) = connection_range_bounds(&index.range, int_params, prefix)?;
        let binder = rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::from_ordinal(id)?,
            display_name: name.to_string(),
            lower,
            upper,
            step,
        };
        binders.push(binder);
    }
    let domain = rumoca_core::StructuredIndexDomain { binders };
    domain.scalar_count().ok()?;
    Some(domain)
}

fn connection_range_bounds(
    expression: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    prefix: &ast::QualifiedName,
) -> Option<(i64, i64, i64)> {
    let ast::Expression::Range {
        start, step, end, ..
    } = expression
    else {
        return None;
    };
    let lower = expr_to_i64_with_params(start, int_params, prefix)?;
    let upper = expr_to_i64_with_params(end, int_params, prefix)?;
    let step = match step {
        Some(value) => expr_to_i64_with_params(value, int_params, prefix)?,
        None => 1,
    };
    (step != 0).then_some((lower, step, upper))
}

fn connection_endpoint_template(
    reference: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    binder_names: &[&str],
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> InstantiateResult<Option<ast::InstanceConnectionEndpoint>> {
    let rank = binder_names.len();
    let mut parts = prefix
        .parts
        .iter()
        .map(|(name, subscripts)| {
            (
                name.clone(),
                subscripts
                    .iter()
                    .map(|value| rumoca_core::AffineForm::constant(*value, rank))
                    .collect(),
            )
        })
        .collect::<Vec<_>>();
    for part in &reference.parts {
        let mut subscripts = Vec::new();
        for subscript in part.subs.as_deref().unwrap_or(&[]) {
            let ast::Subscript::Expression(expression) = subscript else {
                return Ok(None);
            };
            let Some(form) = connection_affine_form(expression, binder_names, int_params, prefix)
            else {
                return Ok(None);
            };
            subscripts.push(form);
        }
        parts.push((part.ident.text.to_string(), subscripts));
    }
    ast::InstanceConnectionEndpoint::new(parts)
        .map(Some)
        .map_err(|error| invalid_connection(error, reference.span))
}

fn connection_affine_form(
    expression: &ast::Expression,
    binder_names: &[&str],
    int_params: &rustc_hash::FxHashMap<String, i64>,
    prefix: &ast::QualifiedName,
) -> Option<rumoca_core::AffineForm> {
    let rank = binder_names.len();
    if let ast::Expression::ComponentReference(reference) = expression
        && reference.parts.len() == 1
        && reference.parts[0].subs.is_none()
        && let Some(index) = binder_names
            .iter()
            .position(|name| *name == reference.parts[0].ident.text.as_ref())
    {
        return Some(rumoca_core::AffineForm::unit_binder(index, rank));
    }
    if let Some(value) = expr_to_i64_with_params(expression, int_params, prefix) {
        return Some(rumoca_core::AffineForm::constant(value, rank));
    }
    match expression {
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = connection_affine_form(lhs, binder_names, int_params, prefix)?;
            let rhs = connection_affine_form(rhs, binder_names, int_params, prefix)?;
            connection_affine_binary(op, &lhs, &rhs)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            let rhs = connection_affine_form(rhs, binder_names, int_params, prefix)?;
            match op {
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Some(rhs),
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                    checked_scale_affine(&rhs, -1)
                }
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner, .. } => {
            connection_affine_form(inner, binder_names, int_params, prefix)
        }
        _ => None,
    }
}

fn connection_affine_binary(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::AffineForm,
    rhs: &rumoca_core::AffineForm,
) -> Option<rumoca_core::AffineForm> {
    use rumoca_core::OpBinary;
    match op {
        OpBinary::Add | OpBinary::AddElem => checked_add_affine(lhs, rhs, 1),
        OpBinary::Sub | OpBinary::SubElem => checked_add_affine(lhs, rhs, -1),
        OpBinary::Mul | OpBinary::MulElem if lhs.is_binder_free() => {
            checked_scale_affine(rhs, lhs.constant)
        }
        OpBinary::Mul | OpBinary::MulElem if rhs.is_binder_free() => {
            checked_scale_affine(lhs, rhs.constant)
        }
        OpBinary::Div | OpBinary::DivElem if rhs.is_binder_free() && rhs.constant != 0 => {
            checked_divide_affine(lhs, rhs.constant)
        }
        _ => None,
    }
}

fn checked_add_affine(
    lhs: &rumoca_core::AffineForm,
    rhs: &rumoca_core::AffineForm,
    rhs_scale: i64,
) -> Option<rumoca_core::AffineForm> {
    if lhs.coeffs.len() != rhs.coeffs.len() {
        return None;
    }
    Some(rumoca_core::AffineForm {
        constant: checked_affine_sum(lhs.constant, rhs.constant, rhs_scale)?,
        coeffs: lhs
            .coeffs
            .iter()
            .zip(&rhs.coeffs)
            .map(|(lhs, rhs)| checked_affine_sum(*lhs, *rhs, rhs_scale))
            .collect::<Option<Vec<_>>>()?,
    })
}

fn checked_affine_sum(lhs: i64, rhs: i64, rhs_scale: i64) -> Option<i64> {
    i64::try_from(i128::from(lhs) + i128::from(rhs) * i128::from(rhs_scale)).ok()
}

fn checked_scale_affine(
    form: &rumoca_core::AffineForm,
    scale: i64,
) -> Option<rumoca_core::AffineForm> {
    Some(rumoca_core::AffineForm {
        constant: i64::try_from(i128::from(form.constant) * i128::from(scale)).ok()?,
        coeffs: form
            .coeffs
            .iter()
            .map(|coefficient| i64::try_from(i128::from(*coefficient) * i128::from(scale)).ok())
            .collect::<Option<Vec<_>>>()?,
    })
}

fn checked_divide_affine(
    form: &rumoca_core::AffineForm,
    divisor: i64,
) -> Option<rumoca_core::AffineForm> {
    if form.constant.checked_rem(divisor)? != 0
        || form
            .coeffs
            .iter()
            .any(|coefficient| coefficient.checked_rem(divisor) != Some(0))
    {
        return None;
    }
    Some(rumoca_core::AffineForm {
        constant: form.constant.checked_div(divisor)?,
        coeffs: form
            .coeffs
            .iter()
            .map(|coefficient| coefficient.checked_div(divisor))
            .collect::<Option<Vec<_>>>()?,
    })
}

fn substitute_index_in_for_indices(
    indices: &[rumoca_ir_ast::ForIndex],
    var_name: &str,
    value: i64,
) -> (Vec<rumoca_ir_ast::ForIndex>, bool) {
    let mut shadowed = false;
    let indices = indices
        .iter()
        .map(|index| {
            // A binder is not in scope in its own range. Substitute the
            // outer value there, then preserve the newly shadowing binder in
            // every later range in this same for-clause.
            let range = if shadowed {
                index.range.clone()
            } else {
                substitute_index_in_expr(&index.range, var_name, value)
            };
            if index.ident.text.as_ref() == var_name {
                shadowed = true;
            }
            rumoca_ir_ast::ForIndex {
                ident: index.ident.clone(),
                range,
            }
        })
        .collect();
    (indices, shadowed)
}

fn equations_contain_connect(equations: &[ast::Equation]) -> bool {
    use std::ops::ControlFlow::Break;

    struct ConnectFinder(bool);

    impl rumoca_ir_ast::Visitor for ConnectFinder {
        fn visit_connect(
            &mut self,
            _lhs: &ast::ComponentReference,
            _rhs: &ast::ComponentReference,
        ) -> std::ops::ControlFlow<()> {
            self.0 = true;
            Break(())
        }
    }

    let mut finder = ConnectFinder(false);
    for equation in equations {
        if rumoca_ir_ast::Visitor::visit_equation(&mut finder, equation).is_break() {
            return true;
        }
    }
    finder.0
}

/// Try to expand a for-loop range to concrete integer values.
///
/// Uses integer parameters to resolve parameter references like `m` in `1:m`.
/// SPEC_0032 §7 whole-extraction fallback-iteration budget.
const MAX_MATERIALIZED_CONNECTION_ITERATIONS: usize = 1_000_000;

struct ConnectionExpansionBudget {
    remaining: usize,
}

impl ConnectionExpansionBudget {
    fn new() -> Self {
        Self {
            remaining: MAX_MATERIALIZED_CONNECTION_ITERATIONS,
        }
    }

    fn reserve(&mut self, count: usize) -> Result<(), usize> {
        let remaining = self.remaining;
        self.remaining = self.remaining.checked_sub(count).ok_or(remaining)?;
        Ok(())
    }
}

enum ForRangeExpansion {
    Values(Vec<i64>),
    MaterializationLimit { count: usize, remaining: usize },
    Unevaluable,
}

fn estimate_materialized_connection_work(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Equation],
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<usize> {
    let own = indices.iter().try_fold(1usize, |count, index| {
        let domain = for_range_domain(&index.range, int_params, scope)?;
        let cardinality = domain.scalar_count().ok()?;
        Some(count.saturating_mul(cardinality))
    })?;
    let nested = equations.iter().fold(0usize, |total, equation| {
        let work = match equation {
            ast::Equation::For { indices, equations } => {
                estimate_materialized_connection_work(indices, equations, int_params, scope)
                    .unwrap_or(0)
            }
            ast::Equation::If {
                cond_blocks,
                else_block,
            } => cond_blocks
                .iter()
                .map(|block| nested_materialized_work(&block.eqs, int_params, scope))
                .chain(
                    else_block
                        .iter()
                        .map(|branch| nested_materialized_work(branch, int_params, scope)),
                )
                .max()
                .unwrap_or(0),
            _ => 0,
        };
        total.saturating_add(work)
    });
    Some(own.saturating_mul(nested.max(1)))
}

fn nested_materialized_work(
    equations: &[ast::Equation],
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> usize {
    equations.iter().fold(0usize, |total, equation| {
        let work = match equation {
            ast::Equation::For { indices, equations } => {
                estimate_materialized_connection_work(indices, equations, int_params, scope)
                    .unwrap_or(0)
            }
            ast::Equation::If {
                cond_blocks,
                else_block,
            } => cond_blocks
                .iter()
                .map(|block| nested_materialized_work(&block.eqs, int_params, scope))
                .chain(
                    else_block
                        .iter()
                        .map(|branch| nested_materialized_work(branch, int_params, scope)),
                )
                .max()
                .unwrap_or(0),
            _ => 0,
        };
        total.saturating_add(work)
    })
}

fn for_range_domain(
    range_expr: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<rumoca_core::StructuredIndexDomain> {
    // MLS §8.3.2 / EQN-008: a for-equation iterator expression is a
    // vector. A scalar Integer is not shorthand for `1:n`.
    let ast::Expression::Range {
        start, step, end, ..
    } = range_expr
    else {
        return None;
    };
    let lower = expr_to_i64_with_params(start, int_params, scope)?;
    let step = step.as_ref().map_or(Some(1), |step| {
        expr_to_i64_with_params(step, int_params, scope)
    })?;
    let upper = expr_to_i64_with_params(end, int_params, scope)?;
    Some(rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "__expanded_connection_index".to_string(),
            lower,
            upper,
            step,
        }],
    })
}

fn expand_for_range(
    range_expr: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
    budget: &mut ConnectionExpansionBudget,
) -> ForRangeExpansion {
    let Some(domain) = for_range_domain(range_expr, int_params, scope) else {
        return ForRangeExpansion::Unevaluable;
    };
    let Ok(domain) = domain.validated() else {
        return ForRangeExpansion::Unevaluable;
    };
    let count = domain.scalar_count();
    if let Err(remaining) = budget.reserve(count) {
        return ForRangeExpansion::MaterializationLimit { count, remaining };
    }
    let mut values = Vec::new();
    if values.try_reserve_exact(count).is_err() {
        return ForRangeExpansion::Unevaluable;
    }
    for ordinal in 0..count {
        let Some(tuple) = domain.index_tuple_at(ordinal) else {
            return ForRangeExpansion::Unevaluable;
        };
        let Some(value) = tuple.first() else {
            return ForRangeExpansion::Unevaluable;
        };
        values.push(*value);
    }
    ForRangeExpansion::Values(values)
}

/// Try to evaluate an expression to i64, using parameter lookup if needed.
/// Handles literals, parameter references, arithmetic, and `div()`.
fn expr_to_i64_with_params(
    expr: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<i64> {
    match expr {
        // Literal integer
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),

        // Parameter reference (single-part or multi-part like cellData.nRC)
        ast::Expression::ComponentReference(cr)
            if !cr.parts.is_empty() && cr.parts.iter().all(|p| p.subs.is_none()) =>
        {
            resolve_int_param_ref(cr, int_params, scope)
        }

        // Binary arithmetic
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let l = expr_to_i64_with_params(lhs, int_params, scope)?;
            let r = expr_to_i64_with_params(rhs, int_params, scope)?;
            rumoca_core::eval_ast_integer_binary(op, l, r)
        }

        // Unary
        ast::Expression::Unary { op, rhs, .. } => {
            let val = expr_to_i64_with_params(rhs, int_params, scope)?;
            eval_unary_i64(op, val)
        }

        // Parenthesized
        ast::Expression::Parenthesized { inner, .. } => {
            expr_to_i64_with_params(inner, int_params, scope)
        }

        // Built-in div() function
        ast::Expression::FunctionCall { comp, args, .. }
            if comp.parts.len() == 1
                && comp.parts[0].subs.is_none()
                && comp.parts[0].ident.text.as_ref() == "div"
                && args.len() == 2 =>
        {
            let a = expr_to_i64_with_params(&args[0], int_params, scope)?;
            let b = expr_to_i64_with_params(&args[1], int_params, scope)?;
            a.checked_div(b)
        }

        _ => None,
    }
}

/// Resolve a component reference to an integer parameter value in lexical scope.
fn resolve_int_param_ref(
    cr: &ast::ComponentReference,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<i64> {
    let name = component_ref_path_no_subscripts(cr)?;
    let scope = scope.to_component_path();
    for candidate in scoped_component_path_candidates(&name, &scope) {
        if let Some(value) = int_params.get(candidate.as_str()) {
            return Some(*value);
        }
    }
    None
}

fn component_ref_path_no_subscripts(cr: &ast::ComponentReference) -> Option<ComponentPath> {
    if cr.parts.is_empty() || cr.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    Some(ComponentPath::from_parts(
        cr.parts.iter().map(|part| part.ident.text.as_ref()),
    ))
}

/// Substitute an index variable with a concrete value in an equation.
fn substitute_index_in_equation(eq: &ast::Equation, var_name: &str, value: i64) -> ast::Equation {
    match eq {
        ast::Equation::Connect { lhs, rhs } => ast::Equation::Connect {
            lhs: substitute_index_in_comp_ref(lhs, var_name, value),
            rhs: substitute_index_in_comp_ref(rhs, var_name, value),
        },
        ast::Equation::For { indices, equations } => {
            let mut shadowed = false;
            let indices = indices
                .iter()
                .map(|index| {
                    // A for-index is not in scope in its own range. Shadowing
                    // starts after that range, for later indices and the body.
                    let range = if shadowed {
                        index.range.clone()
                    } else {
                        substitute_index_in_expr(&index.range, var_name, value)
                    };
                    if index.ident.text.as_ref() == var_name {
                        shadowed = true;
                    }
                    rumoca_ir_ast::ForIndex {
                        ident: index.ident.clone(),
                        range,
                    }
                })
                .collect();
            let equations = if shadowed {
                equations.clone()
            } else {
                equations
                    .iter()
                    .map(|equation| substitute_index_in_equation(equation, var_name, value))
                    .collect()
            };
            ast::Equation::For { indices, equations }
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => ast::Equation::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| rumoca_ir_ast::EquationBlock {
                    cond: substitute_index_in_expr(&block.cond, var_name, value),
                    eqs: block
                        .eqs
                        .iter()
                        .map(|e| substitute_index_in_equation(e, var_name, value))
                        .collect(),
                })
                .collect(),
            else_block: else_block.as_ref().map(|eqs| {
                eqs.iter()
                    .map(|e| substitute_index_in_equation(e, var_name, value))
                    .collect()
            }),
        },
        // Other equation types are returned as-is
        other => other.clone(),
    }
}

/// Substitute an index variable with a concrete value in a component reference.
fn substitute_index_in_comp_ref(
    comp_ref: &ast::ComponentReference,
    var_name: &str,
    value: i64,
) -> ast::ComponentReference {
    ast::ComponentReference {
        local: comp_ref.local,
        parts: comp_ref
            .parts
            .iter()
            .map(|part| rumoca_ir_ast::ComponentRefPart {
                ident: part.ident.clone(),
                subs: part.subs.as_ref().map(|subs| {
                    subs.iter()
                        .map(|sub| substitute_index_in_subscript(sub, var_name, value))
                        .collect()
                }),
                def_id: part.def_id,
            })
            .collect(),
        span: comp_ref.span,
        qualified_display_name: comp_ref.qualified_display_name.clone(),
    }
}

/// Substitute an index variable with a concrete value in a subscript.
fn substitute_index_in_subscript(
    sub: &ast::Subscript,
    var_name: &str,
    value: i64,
) -> ast::Subscript {
    match sub {
        ast::Subscript::Expression(expr) => {
            ast::Subscript::Expression(substitute_index_in_expr(expr, var_name, value))
        }
        other => other.clone(),
    }
}

/// Substitute an index variable with a concrete value in an expression.
fn substitute_index_in_expr(expr: &ast::Expression, var_name: &str, value: i64) -> ast::Expression {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            // Check if this is a simple reference to the index variable
            if cr.parts.len() == 1
                && cr.parts[0].subs.is_none()
                && cr.parts[0].ident.text.as_ref() == var_name
            {
                // Replace with integer literal
                ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: rumoca_core::Token {
                        text: std::sync::Arc::from(value.to_string()),
                        location: cr.parts[0].ident.location.clone(),
                        token_number: 0,
                        token_type: 0,
                    },
                    span: cr.span,
                }
            } else {
                // Substitute in subscripts
                ast::Expression::ComponentReference(substitute_index_in_comp_ref(
                    cr, var_name, value,
                ))
            }
        }
        ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
            op: op.clone(),
            lhs: std::sync::Arc::new(substitute_index_in_expr(lhs, var_name, value)),
            rhs: std::sync::Arc::new(substitute_index_in_expr(rhs, var_name, value)),
            span: *span,
        },
        ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
            op: op.clone(),
            rhs: std::sync::Arc::new(substitute_index_in_expr(rhs, var_name, value)),
            span: *span,
        },
        ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
            inner: std::sync::Arc::new(substitute_index_in_expr(inner, var_name, value)),
            span: *span,
        },
        ast::Expression::Array {
            elements,
            is_matrix,
            span,
        } => ast::Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_index_in_expr(e, var_name, value))
                .collect(),
            is_matrix: *is_matrix,
            span: *span,
        },
        ast::Expression::FunctionCall {
            comp,
            args,
            is_partial_application,
            span,
        } => ast::Expression::FunctionCall {
            comp: substitute_index_in_comp_ref(comp, var_name, value),
            args: args
                .iter()
                .map(|a| substitute_index_in_expr(a, var_name, value))
                .collect(),
            is_partial_application: *is_partial_application,
            span: *span,
        },
        ast::Expression::DerivativeCall { args, span } => ast::Expression::DerivativeCall {
            args: args
                .iter()
                .map(|arg| substitute_index_in_expr(arg, var_name, value))
                .collect(),
            span: *span,
        },
        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => ast::Expression::Range {
            start: std::sync::Arc::new(substitute_index_in_expr(start, var_name, value)),
            step: step
                .as_ref()
                .map(|s| std::sync::Arc::new(substitute_index_in_expr(s, var_name, value))),
            end: std::sync::Arc::new(substitute_index_in_expr(end, var_name, value)),
            span: *span,
        },
        // Other expressions are returned as-is
        other => other.clone(),
    }
}

/// Try to evaluate a boolean expression using parameter values.
fn try_eval_bool_expr(
    expr: &ast::Expression,
    bool_params: &rustc_hash::FxHashMap<String, bool>,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<bool> {
    match expr {
        // Literal boolean (true or false)
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },

        // Parameter reference
        ast::Expression::ComponentReference(cr) => resolve_bool_param_ref(cr, bool_params, scope),

        // Not expression
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: inner,
            ..
        } => try_eval_bool_expr(inner, bool_params, int_params, scope).map(|v| !v),

        // And expression
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            let l = try_eval_bool_expr(lhs, bool_params, int_params, scope)?;
            let r = try_eval_bool_expr(rhs, bool_params, int_params, scope)?;
            Some(l && r)
        }

        // Or expression
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            let l = try_eval_bool_expr(lhs, bool_params, int_params, scope)?;
            let r = try_eval_bool_expr(rhs, bool_params, int_params, scope)?;
            Some(l || r)
        }

        ast::Expression::Binary {
            op: op @ (rumoca_core::OpBinary::Eq | rumoca_core::OpBinary::Neq),
            lhs,
            rhs,
            ..
        } => {
            if let (Some(lhs), Some(rhs)) = (
                try_eval_bool_expr(lhs, bool_params, int_params, scope),
                try_eval_bool_expr(rhs, bool_params, int_params, scope),
            ) {
                return Some(if *op == rumoca_core::OpBinary::Eq {
                    lhs == rhs
                } else {
                    lhs != rhs
                });
            }
            let lhs = expr_to_i64_with_params(lhs, int_params, scope)?;
            let rhs = expr_to_i64_with_params(rhs, int_params, scope)?;
            Some(if *op == rumoca_core::OpBinary::Eq {
                lhs == rhs
            } else {
                lhs != rhs
            })
        }

        // Integer comparison expressions (e.g., i > 1 after index substitution)
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let l = expr_to_i64_with_params(lhs, int_params, scope)?;
            let r = expr_to_i64_with_params(rhs, int_params, scope)?;
            match op {
                rumoca_core::OpBinary::Gt => Some(l > r),
                rumoca_core::OpBinary::Ge => Some(l >= r),
                rumoca_core::OpBinary::Lt => Some(l < r),
                rumoca_core::OpBinary::Le => Some(l <= r),
                rumoca_core::OpBinary::Eq => Some(l == r),
                rumoca_core::OpBinary::Neq => Some(l != r),
                _ => None,
            }
        }

        // Parenthesized boolean expression
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_bool_expr(inner, bool_params, int_params, scope)
        }

        _ => None,
    }
}

fn resolve_bool_param_ref(
    cr: &ast::ComponentReference,
    bool_params: &rustc_hash::FxHashMap<String, bool>,
    scope: &ast::QualifiedName,
) -> Option<bool> {
    let name = component_ref_path_no_subscripts(cr)?;
    let scope = scope.to_component_path();
    for candidate in scoped_component_path_candidates(&name, &scope) {
        if let Some(value) = bool_params.get(candidate.as_str()) {
            return Some(*value);
        }
    }
    None
}

/// Convert a ast::ComponentReference to a ast::QualifiedName with prefix.
///
/// Uses `int_params` to resolve parameter references in subscripts (e.g.,
/// `transferFunction[na].y` where `na=2` becomes `transferFunction[2].y`).
fn component_ref_to_qualified_name(
    comp_ref: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    span: Span,
) -> InstantiateResult<ast::QualifiedName> {
    let mut qn = prefix.clone();

    for part in &comp_ref.parts {
        let subscripts = part
            .subs
            .as_deref()
            .unwrap_or_default()
            .iter()
            .map(|subscript| {
                subscript_to_i64(subscript, int_params, prefix).ok_or_else(|| {
                    Box::new(InstantiateError::unsupported_connection_subscript(
                        format!("`{subscript}`"),
                        "selector is not an evaluable scalar Integer".to_string(),
                        span,
                    ))
                })
            })
            .collect::<InstantiateResult<Vec<_>>>()?;

        qn.push(part.ident.text.to_string(), subscripts);
    }

    Ok(qn)
}

/// Try to convert a subscript to an i64, resolving parameter references.
fn subscript_to_i64(
    sub: &ast::Subscript,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<i64> {
    match sub {
        ast::Subscript::Expression(expr) => expr_to_i64_with_params(expr, int_params, scope),
        ast::Subscript::Range { .. } | ast::Subscript::Empty => None,
    }
}

/// Evaluate a unary integer operation.
fn eval_unary_i64(op: &rumoca_core::OpUnary, val: i64) -> Option<i64> {
    match op {
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => val.checked_neg(),
        rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Some(val),
        _ => None,
    }
}

#[derive(Clone, Copy)]
struct CompactConnectionRange {
    part_index: usize,
    subscript_index: usize,
    dimension: usize,
    start: i64,
    step: i64,
    count: usize,
}

fn try_compact_range_subscript_connection(
    lhs: &ast::ComponentReference,
    rhs: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    span: Span,
) -> InstantiateResult<Option<ast::InstanceConnection>> {
    let lhs_ranges = compact_connection_ranges(lhs, int_params, prefix).ok_or_else(|| {
        Box::new(InstantiateError::structural_param_error(
            "connection range".to_string(),
            "cannot evaluate connection range subscript".to_string(),
            span,
        ))
    })?;
    let rhs_ranges = compact_connection_ranges(rhs, int_params, prefix).ok_or_else(|| {
        Box::new(InstantiateError::structural_param_error(
            "connection range".to_string(),
            "cannot evaluate connection range subscript".to_string(),
            span,
        ))
    })?;
    if lhs_ranges.is_empty() && rhs_ranges.is_empty() {
        return Ok(None);
    }
    if !lhs_ranges.is_empty()
        && !rhs_ranges.is_empty()
        && !connection_range_shapes_match(&lhs_ranges, &rhs_ranges)
    {
        return Err(Box::new(InstantiateError::array_dim_mismatch(
            "connect".to_string(),
            connection_range_shape(&lhs_ranges),
            connection_range_shape(&rhs_ranges),
            span,
        )));
    }
    let shape = if lhs_ranges.is_empty() {
        connection_range_counts(&rhs_ranges)
    } else {
        connection_range_counts(&lhs_ranges)
    };
    let domain = rumoca_core::StructuredIndexDomain {
        binders: connection_range_binders(&shape, span)?,
    };
    let a_template = range_connection_endpoint_template(
        lhs,
        prefix,
        int_params,
        &lhs_ranges,
        shape.len(),
        span,
    )?;
    let b_template = range_connection_endpoint_template(
        rhs,
        prefix,
        int_params,
        &rhs_ranges,
        shape.len(),
        span,
    )?;
    Ok(Some(
        ast::InstanceConnection::family(
            domain,
            a_template,
            b_template,
            None,
            span,
            prefix.to_flat_string(),
        )
        .map_err(|error| invalid_connection(error, span))?,
    ))
}

fn connection_range_counts(ranges: &[CompactConnectionRange]) -> Vec<usize> {
    ranges.iter().map(|range| range.count).collect()
}

fn connection_range_shape(ranges: &[CompactConnectionRange]) -> String {
    format!("{:?}", connection_range_counts(ranges))
}

fn connection_range_shapes_match(
    lhs: &[CompactConnectionRange],
    rhs: &[CompactConnectionRange],
) -> bool {
    lhs.len() == rhs.len() && lhs.iter().zip(rhs).all(|(lhs, rhs)| lhs.count == rhs.count)
}

fn connection_range_binders(
    shape: &[usize],
    span: Span,
) -> InstantiateResult<Vec<rumoca_core::StructuredIndexBinder>> {
    shape
        .iter()
        .copied()
        .enumerate()
        .map(|(dimension, count)| {
            Ok(rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::from_ordinal(dimension).ok_or_else(
                    || {
                        Box::new(InstantiateError::array_dim_mismatch(
                            "connect".to_string(),
                            "rank within typed binder identity range".to_string(),
                            shape.len().to_string(),
                            span,
                        ))
                    },
                )?,
                display_name: format!("__connection_index_{}", dimension + 1),
                lower: 1,
                upper: i64::try_from(count).map_err(|_| {
                    Box::new(InstantiateError::array_dim_mismatch(
                        "connect".to_string(),
                        "range extent within i64".to_string(),
                        count.to_string(),
                        span,
                    ))
                })?,
                step: 1,
            })
        })
        .collect()
}

fn compact_connection_ranges(
    reference: &ast::ComponentReference,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    prefix: &ast::QualifiedName,
) -> Option<Vec<CompactConnectionRange>> {
    let mut found = Vec::new();
    for (part_index, part) in reference.parts.iter().enumerate() {
        for (subscript_index, subscript) in part.subs.as_deref().unwrap_or(&[]).iter().enumerate() {
            let ast::Subscript::Expression(ast::Expression::Range {
                start, step, end, ..
            }) = subscript
            else {
                continue;
            };
            let start = expr_to_i64_with_params(start, int_params, prefix)?;
            let end = expr_to_i64_with_params(end, int_params, prefix)?;
            let step = match step {
                Some(step) => expr_to_i64_with_params(step, int_params, prefix)?,
                None => 1,
            };
            let domain = rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "__connection_range".to_string(),
                    lower: start,
                    upper: end,
                    step,
                }],
            };
            let count = domain.scalar_count().ok()?;
            found.push(CompactConnectionRange {
                part_index,
                subscript_index,
                dimension: found.len(),
                start,
                step,
                count,
            });
        }
    }
    Some(found)
}

fn range_connection_endpoint_template(
    reference: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    ranges: &[CompactConnectionRange],
    rank: usize,
    span: Span,
) -> InstantiateResult<ast::InstanceConnectionEndpoint> {
    let mut parts = prefix
        .parts
        .iter()
        .map(|(name, subscripts)| {
            (
                name.clone(),
                subscripts
                    .iter()
                    .map(|value| rumoca_core::AffineForm::constant(*value, rank))
                    .collect(),
            )
        })
        .collect::<Vec<_>>();
    for (part_index, part) in reference.parts.iter().enumerate() {
        let mut subscripts = Vec::new();
        for (subscript_index, subscript) in part.subs.as_deref().unwrap_or(&[]).iter().enumerate() {
            if let Some(range) = ranges.iter().copied().find(|range| {
                range.part_index == part_index && range.subscript_index == subscript_index
            }) {
                subscripts.push(connection_range_affine_form(range, rank, reference.span)?);
                continue;
            }
            let ast::Subscript::Expression(expression) = subscript else {
                return Err(Box::new(
                    InstantiateError::unsupported_connection_subscript(
                        format!("`{subscript}`"),
                        "selector is not an evaluable scalar Integer".to_string(),
                        span,
                    ),
                ));
            };
            let value =
                expr_to_i64_with_params(expression, int_params, prefix).ok_or_else(|| {
                    Box::new(InstantiateError::unsupported_connection_subscript(
                        format!("`{subscript}`"),
                        "selector is not an evaluable scalar Integer".to_string(),
                        span,
                    ))
                })?;
            subscripts.push(rumoca_core::AffineForm::constant(value, rank));
        }
        parts.push((part.ident.text.to_string(), subscripts));
    }
    if ranges.is_empty() {
        let Some((_, subscripts)) = parts.last_mut() else {
            return Err(Box::new(InstantiateError::array_dim_mismatch(
                "connect".to_string(),
                "non-empty endpoint".to_string(),
                "empty endpoint".to_string(),
                reference.span,
            )));
        };
        for dimension in 0..rank {
            subscripts.push(rumoca_core::AffineForm::unit_binder(dimension, rank));
        }
    }
    ast::InstanceConnectionEndpoint::new(parts).map_err(|error| invalid_connection(error, span))
}

fn invalid_connection(
    error: ast::InstanceConnectionConstructionError,
    span: Span,
) -> Box<InstantiateError> {
    Box::new(InstantiateError::array_dim_mismatch(
        "connect".to_string(),
        "valid checked connection evidence".to_string(),
        error.to_string(),
        span,
    ))
}

fn connection_range_affine_form(
    range: CompactConnectionRange,
    rank: usize,
    span: Span,
) -> InstantiateResult<rumoca_core::AffineForm> {
    let constant = range.start.checked_sub(range.step).ok_or_else(|| {
        Box::new(InstantiateError::array_dim_mismatch(
            "connect".to_string(),
            "affine range within i64".to_string(),
            format!("{}:{}", range.start, range.step),
            span,
        ))
    })?;
    let mut coeffs = vec![0; rank];
    let Some(coefficient) = coeffs.get_mut(range.dimension) else {
        return Err(Box::new(InstantiateError::array_dim_mismatch(
            "connect".to_string(),
            "range dimension within endpoint rank".to_string(),
            range.dimension.to_string(),
            span,
        )));
    };
    *coefficient = range.step;
    Ok(rumoca_core::AffineForm { constant, coeffs })
}

/// Check if an equation is a connect statement.
pub(crate) fn is_connect_equation(eq: &ast::Equation) -> bool {
    matches!(eq, ast::Equation::Connect { .. })
}

/// Filter out connect equations from a list.
pub fn filter_out_connections(equations: &[ast::Equation]) -> Vec<ast::Equation> {
    equations
        .iter()
        .filter(|eq| !is_connect_equation(eq))
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests;
