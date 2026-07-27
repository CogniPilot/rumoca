//! Connection extraction for the instantiate phase (MLS §9).
//!
//! This module extracts connect() statements from equations and converts
//! them to ast::InstanceConnection structs.

use rumoca_core::{ComponentPath, SourceMap, Span, scoped_component_path_candidates};
use rumoca_ir_ast as ast;

use crate::errors::{InstantiateError, InstantiateResult};
use crate::inheritance::required_location_to_span;

/// Parameters for connection extraction, including both boolean and integer values.
#[derive(Debug, Clone, Default)]
pub struct ConnectionParams {
    /// Boolean parameters for evaluating conditional branches.
    pub bools: rustc_hash::FxHashMap<String, bool>,
    /// Integer parameters for evaluating for-loop ranges.
    pub integers: rustc_hash::FxHashMap<String, i64>,
}

impl ConnectionParams {
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
    params: &ConnectionParams,
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

    for eq in equations {
        extract_connections_from_equation(&mut connections, eq, prefix, params, source_map)?;
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
    params: &ConnectionParams,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    match eq {
        ast::Equation::Connect { lhs, rhs, .. } => {
            let span =
                required_location_to_span(eq.get_location(), source_map, "connect equation")?;

            // Preserve range/slice connects as one authoritative structured
            // family. Flattening derives the scalar union-find view once.
            if let Some(connection) =
                try_compact_range_subscript_connection(lhs, rhs, prefix, &params.integers, span)?
            {
                connections.push(connection);
            } else {
                let a = component_ref_to_qualified_name(lhs, prefix, &params.integers);
                let b = component_ref_to_qualified_name(rhs, prefix, &params.integers);

                connections.push(ast::InstanceConnection {
                    a,
                    b,
                    connector_type: None, // Resolved later during flattening
                    span,
                    scope: prefix.to_flat_string(),
                    family: None,
                });
            }
            Ok(())
        }

        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            // For if-equations, try to evaluate the condition using parameters.
            // If the condition is a simple parameter reference, use it to select the branch.
            // Otherwise, extract connections from ALL branches (conservative).
            extract_connections_from_if_equation(
                connections,
                cond_blocks,
                else_block,
                prefix,
                params,
                source_map,
            )
        }

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
            )
        }

        // Other equation types don't contain connections
        _ => Ok(()),
    }
}

/// Extract connections from an if-equation.
///
/// Tries to evaluate the condition using parameters. If successful, only
/// extracts from the selected branch. Otherwise, extracts from all branches
/// to ensure no connections are missed.
fn extract_connections_from_if_equation(
    connections: &mut Vec<ast::InstanceConnection>,
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    params: &ConnectionParams,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    let selected_branch = try_select_branch(cond_blocks, else_block, prefix, params);

    if let Some(branch_eqs) = selected_branch {
        // Condition was evaluated - only extract from selected branch
        for nested_eq in &branch_eqs {
            extract_connections_from_equation(connections, nested_eq, prefix, params, source_map)?;
        }
        return Ok(());
    }

    // Condition couldn't be evaluated, so retain the existing conservative
    // branch extraction behavior for structural if-equations.
    extract_connections_from_all_branches(
        connections,
        cond_blocks,
        else_block,
        prefix,
        params,
        source_map,
    )
}

/// Extract connections from all branches of an if-equation.
///
/// Used when the condition cannot be evaluated at compile time.
fn extract_connections_from_all_branches(
    connections: &mut Vec<ast::InstanceConnection>,
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    params: &ConnectionParams,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    for block in cond_blocks {
        for nested_eq in &block.eqs {
            extract_connections_from_equation(connections, nested_eq, prefix, params, source_map)?;
        }
    }
    if let Some(else_eqs) = else_block {
        for nested_eq in else_eqs {
            extract_connections_from_equation(connections, nested_eq, prefix, params, source_map)?;
        }
    }
    Ok(())
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
    params: &ConnectionParams,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    if !equations_contain_connect(equations) {
        return Ok(());
    }

    if indices.is_empty() {
        // No indices, just process the equations directly
        for eq in equations {
            extract_connections_from_equation(connections, eq, prefix, params, source_map)?;
        }
        return Ok(());
    }

    if let Some(families) =
        try_extract_regular_connection_families(indices, equations, prefix, params, source_map)?
    {
        connections.extend(families);
        return Ok(());
    }

    // Get the first index and expand it
    let first_index = &indices[0];
    let remaining_indices = &indices[1..];
    let index_name = &first_index.ident.text;

    // Try to evaluate the range to get concrete index values, using integer params
    if let Some(range_values) = expand_for_range(&first_index.range, &params.integers, prefix) {
        for value in range_values {
            // Substitute the index variable with this value in all equations
            let substituted: Vec<ast::Equation> = equations
                .iter()
                .map(|eq| substitute_index_in_equation(eq, index_name, value))
                .collect();
            let substituted_indices =
                substitute_index_in_for_indices(remaining_indices, index_name, value);

            // Recursively process with remaining indices
            extract_connections_from_for_equation(
                connections,
                &substituted_indices,
                &substituted,
                prefix,
                params,
                source_map,
            )?;
        }
        return Ok(());
    }

    Err(Box::new(InstantiateError::structural_param_error(
        index_name.to_string(),
        format!(
            "cannot evaluate connection for-equation range `{}` in `{prefix}`",
            first_index.range
        ),
        required_location_to_span(
            first_index.range.get_location(),
            source_map,
            "connection for-equation range",
        )?,
    )))
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
    if domain.scalar_count().ok() == Some(0) {
        return Ok(Some(Vec::new()));
    }
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
            connection_endpoint_template(lhs, prefix, &binder_names, &params.integers)
        else {
            return Ok(None);
        };
        let Some(b_template) =
            connection_endpoint_template(rhs, prefix, &binder_names, &params.integers)
        else {
            return Ok(None);
        };
        let span = required_location_to_span(
            equation.get_location(),
            source_map,
            "vectorized connect equation",
        )?;
        let family = ast::InstanceConnectionFamily {
            domain: domain.clone(),
            a: a_template,
            b: b_template,
        };
        let tuple = family
            .domain
            .index_tuple_at(0)
            .map_err(|error| {
                Box::new(InstantiateError::structural_param_error(
                    "connection family".to_string(),
                    format!("invalid structured connection domain: {error}"),
                    span,
                ))
            })?
            .ok_or_else(|| {
                Box::new(InstantiateError::structural_param_error(
                    "connection family".to_string(),
                    "structured connection domain is empty".to_string(),
                    span,
                ))
            })?;
        let a = rumoca_eval_ast::connection::evaluate_connection_endpoint(&family.a, &tuple)
            .map_err(|reason| {
                Box::new(InstantiateError::structural_param_error(
                    "connection family".to_string(),
                    reason,
                    span,
                ))
            })?;
        let b = rumoca_eval_ast::connection::evaluate_connection_endpoint(&family.b, &tuple)
            .map_err(|reason| {
                Box::new(InstantiateError::structural_param_error(
                    "connection family".to_string(),
                    reason,
                    span,
                ))
            })?;
        result.push(ast::InstanceConnection {
            a,
            b,
            connector_type: None,
            span,
            scope: prefix.to_flat_string(),
            family: Some(family),
        });
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
            id,
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
) -> Option<ast::InstanceConnectionEndpoint> {
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
                return None;
            };
            subscripts.push(connection_affine_form(
                expression,
                binder_names,
                int_params,
                prefix,
            )?);
        }
        parts.push((part.ident.text.to_string(), subscripts));
    }
    Some(ast::InstanceConnectionEndpoint { parts })
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
) -> Vec<rumoca_ir_ast::ForIndex> {
    indices
        .iter()
        .map(|idx| {
            let range = if idx.ident.text.as_ref() == var_name {
                idx.range.clone()
            } else {
                substitute_index_in_expr(&idx.range, var_name, value)
            };
            rumoca_ir_ast::ForIndex {
                ident: idx.ident.clone(),
                range,
            }
        })
        .collect()
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
fn expand_for_range(
    range_expr: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    scope: &ast::QualifiedName,
) -> Option<Vec<i64>> {
    let (lower, step, upper) = match range_expr {
        ast::Expression::Range {
            start, step, end, ..
        } => {
            let lower = expr_to_i64_with_params(start, int_params, scope)?;
            let upper = expr_to_i64_with_params(end, int_params, scope)?;
            let step = match step {
                Some(step) => expr_to_i64_with_params(step, int_params, scope)?,
                None => 1,
            };
            (lower, step, upper)
        }
        // Single expression (like just `m` meaning 1:m)
        _ => {
            let n = expr_to_i64_with_params(range_expr, int_params, scope)?;
            (1, 1, n)
        }
    };
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "__expanded_connection_index".to_string(),
            lower,
            upper,
            step,
        }],
    };
    let count = domain.scalar_count().ok()?;
    let mut values = Vec::new();
    values.try_reserve_exact(count).ok()?;
    for ordinal in 0..count {
        values.push(*domain.index_tuple_at(ordinal).ok()??.first()?);
    }
    Some(values)
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
            eval_binary_i64(op, l, r)
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
        ast::Equation::For { indices, equations } => ast::Equation::For {
            indices: indices
                .iter()
                .map(|idx| {
                    // Respect loop-variable shadowing: if the nested loop reuses the same
                    // identifier, do not substitute inside its range expression.
                    let range = if idx.ident.text.as_ref() == var_name {
                        idx.range.clone()
                    } else {
                        substitute_index_in_expr(&idx.range, var_name, value)
                    };
                    rumoca_ir_ast::ForIndex {
                        ident: idx.ident.clone(),
                        range,
                    }
                })
                .collect(),
            equations: equations
                .iter()
                .map(|e| substitute_index_in_equation(e, var_name, value))
                .collect(),
        },
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
            })
            .collect(),
        def_id: comp_ref.def_id,
        span: comp_ref.span,
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

/// Try to select a branch based on parameter values.
///
/// Returns Some(equations) if a branch was selected, None if the condition
/// couldn't be evaluated at this stage.
fn try_select_branch(
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    scope: &ast::QualifiedName,
    params: &ConnectionParams,
) -> Option<Vec<ast::Equation>> {
    for block in cond_blocks {
        if let Some(value) = try_eval_bool_expr(&block.cond, &params.bools, &params.integers, scope)
        {
            if value {
                return Some(block.eqs.clone());
            }
            // Condition is false, continue to next branch
        } else {
            // Condition couldn't be evaluated, give up
            return None;
        }
    }

    // All conditions were false - return else branch
    Some(else_block.clone().unwrap_or_default())
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
) -> ast::QualifiedName {
    let mut qn = prefix.clone();

    for part in &comp_ref.parts {
        // Convert subscripts to i64, resolving parameter references via int_params
        let subscripts: Vec<i64> = if let Some(subs) = &part.subs {
            subs.iter()
                .filter_map(|sub| subscript_to_i64(sub, int_params, prefix))
                .collect()
        } else {
            Vec::new()
        };

        qn.push(part.ident.text.to_string(), subscripts);
    }

    qn
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

/// Evaluate a binary integer operation.
fn eval_binary_i64(op: &rumoca_core::OpBinary, l: i64, r: i64) -> Option<i64> {
    match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => l.checked_add(r),
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => l.checked_sub(r),
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => l.checked_mul(r),
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => l.checked_div(r),
        _ => None,
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
    let a_template =
        range_connection_endpoint_template(lhs, prefix, int_params, &lhs_ranges, shape.len())?;
    let b_template =
        range_connection_endpoint_template(rhs, prefix, int_params, &rhs_ranges, shape.len())?;
    let family = ast::InstanceConnectionFamily {
        domain,
        a: a_template,
        b: b_template,
    };
    let first = vec![1; shape.len()];
    let a = rumoca_eval_ast::connection::evaluate_connection_endpoint(&family.a, &first).map_err(
        |reason| {
            Box::new(InstantiateError::structural_param_error(
                "connection range".to_string(),
                reason,
                span,
            ))
        },
    )?;
    let b = rumoca_eval_ast::connection::evaluate_connection_endpoint(&family.b, &first).map_err(
        |reason| {
            Box::new(InstantiateError::structural_param_error(
                "connection range".to_string(),
                reason,
                span,
            ))
        },
    )?;
    Ok(Some(ast::InstanceConnection {
        a,
        b,
        connector_type: None,
        span,
        scope: prefix.to_flat_string(),
        family: Some(family),
    }))
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
                id: dimension,
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
                    id: 0,
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
                return Err(Box::new(InstantiateError::structural_param_error(
                    "connection subscript".to_string(),
                    "non-expression subscript in range connection".to_string(),
                    reference.span,
                )));
            };
            let value =
                expr_to_i64_with_params(expression, int_params, prefix).ok_or_else(|| {
                    Box::new(InstantiateError::structural_param_error(
                        "connection subscript".to_string(),
                        format!("cannot evaluate connection subscript `{expression}`"),
                        reference.span,
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
    Ok(ast::InstanceConnectionEndpoint { parts })
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
mod tests {
    use super::*;

    const TEST_FILE: &str = "connections.mo";

    fn test_source_map() -> SourceMap {
        let mut source_map = SourceMap::new();
        source_map.add(TEST_FILE, "connect(a.p, b.n); for i in 1:2 loop end for;");
        source_map
    }

    fn make_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: std::sync::Arc::from(text),
            location: rumoca_core::Location {
                start_line: 1,
                start_column: 1,
                end_line: 1,
                end_column: 2,
                start: 0,
                end: 1,
                source: rumoca_core::SourceId::from_source_name(TEST_FILE),
            },
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_comp_ref(names: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: names
                .iter()
                .map(|name| ast::ComponentRefPart {
                    ident: make_token(name),
                    subs: None,
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(make_comp_ref(names))
    }

    fn make_integer_terminal(value: &str) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_range_expr(start: ast::Expression, end: ast::Expression) -> ast::Expression {
        ast::Expression::Range {
            start: std::sync::Arc::new(start),
            step: None,
            end: std::sync::Arc::new(end),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_comp_ref_with_sub(expr: ast::Expression, names: &[&str]) -> ast::ComponentReference {
        make_comp_ref_with_sub_at(expr, names, 0)
    }

    fn make_comp_ref_with_sub_at(
        expr: ast::Expression,
        names: &[&str],
        sub_part_index: usize,
    ) -> ast::ComponentReference {
        let mut parts = Vec::new();
        for (i, name) in names.iter().enumerate() {
            parts.push(ast::ComponentRefPart {
                ident: make_token(name),
                subs: if i == sub_part_index {
                    Some(vec![ast::Subscript::Expression(expr.clone())])
                } else {
                    None
                },
            });
        }
        ast::ComponentReference {
            local: false,
            parts,
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_extract_connection() {
        let eq = ast::Equation::Connect {
            lhs: make_comp_ref(&["a", "p"]),
            rhs: make_comp_ref(&["b", "n"]),
        };

        let prefix = ast::QualifiedName::new();
        let source_map = test_source_map();
        let connections =
            extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map).unwrap();

        assert_eq!(connections.len(), 1);
        assert_eq!(connections[0].a.to_flat_string(), "a.p");
        assert_eq!(connections[0].b.to_flat_string(), "b.n");
    }

    #[test]
    fn test_extract_connection_expands_range_on_non_first_part() {
        // Regression: connect(mux2.y, mux5.u[1:2]) must expand even when
        // the range subscript is on the second component-reference part.
        let range = ast::Expression::Range {
            start: std::sync::Arc::new(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: make_token("1"),
                span: rumoca_core::Span::DUMMY,
            }),
            step: None,
            end: std::sync::Arc::new(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: make_token("2"),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        let eq = ast::Equation::Connect {
            lhs: make_comp_ref(&["mux2", "y"]),
            rhs: make_comp_ref_with_sub_at(range, &["mux5", "u"], 1),
        };

        let prefix = ast::QualifiedName::new();
        let source_map = test_source_map();
        let connections =
            extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map).unwrap();

        assert_eq!(connections.len(), 1);
        assert!(connections[0].family.is_some());
        let mut got: Vec<(String, String)> =
            rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
                .expect("valid range connection family")
                .into_iter()
                .map(|connection| (connection.a.to_flat_string(), connection.b.to_flat_string()))
                .collect();
        got.sort();

        assert_eq!(
            got,
            vec![
                ("mux2.y[1]".to_string(), "mux5.u[1]".to_string()),
                ("mux2.y[2]".to_string(), "mux5.u[2]".to_string()),
            ]
        );
    }

    #[test]
    fn test_extract_connection_preserves_multidimensional_ranges() {
        let mut lhs = make_comp_ref(&["a"]);
        lhs.parts[0].subs = Some(vec![
            ast::Subscript::Expression(make_range_expr(
                make_integer_terminal("1"),
                make_integer_terminal("2"),
            )),
            ast::Subscript::Expression(make_range_expr(
                make_integer_terminal("4"),
                make_integer_terminal("5"),
            )),
        ]);
        let mut rhs = make_comp_ref(&["b"]);
        rhs.parts[0].subs = Some(vec![
            ast::Subscript::Expression(make_range_expr(
                make_integer_terminal("7"),
                make_integer_terminal("8"),
            )),
            ast::Subscript::Expression(make_range_expr(
                make_integer_terminal("9"),
                make_integer_terminal("10"),
            )),
        ]);

        let connections = extract_connections(
            &[ast::Equation::Connect { lhs, rhs }],
            &ast::QualifiedName::new(),
            &ConnectionParams::new(),
            &test_source_map(),
        )
        .expect("multidimensional range connection should instantiate");

        assert_eq!(connections.len(), 1);
        let family = connections[0]
            .family
            .as_ref()
            .expect("multidimensional range connection should remain compact");
        assert_eq!(family.domain.extents(), Ok(vec![2, 2]));
        let members = rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
            .expect("multidimensional connection family should evaluate")
            .into_iter()
            .map(|member| (member.a.to_flat_string(), member.b.to_flat_string()))
            .collect::<Vec<_>>();
        assert_eq!(
            members,
            vec![
                ("a[1,4]".to_string(), "b[7,9]".to_string()),
                ("a[1,5]".to_string(), "b[7,10]".to_string()),
                ("a[2,4]".to_string(), "b[8,9]".to_string()),
                ("a[2,5]".to_string(), "b[8,10]".to_string()),
            ]
        );
    }

    #[test]
    fn empty_connection_ranges_produce_empty_scalar_views() {
        let empty_range = make_range_expr(make_integer_terminal("1"), make_integer_terminal("0"));
        assert_eq!(
            expand_for_range(
                &empty_range,
                &rustc_hash::FxHashMap::default(),
                &ast::QualifiedName::new(),
            ),
            Some(Vec::new())
        );

        let mut lhs = make_comp_ref(&["a"]);
        lhs.parts[0].subs = Some(vec![ast::Subscript::Expression(empty_range.clone())]);
        let mut rhs = make_comp_ref(&["b"]);
        rhs.parts[0].subs = Some(vec![ast::Subscript::Expression(empty_range)]);
        let connections = extract_connections(
            &[ast::Equation::Connect { lhs, rhs }],
            &ast::QualifiedName::new(),
            &ConnectionParams::new(),
            &test_source_map(),
        )
        .expect("an empty range connection is valid");

        assert_eq!(connections.len(), 1);
        assert_eq!(
            connections[0]
                .family
                .as_ref()
                .expect("empty connection stays structured")
                .domain
                .scalar_count(),
            Ok(0)
        );
        assert!(
            rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
                .expect("empty family has a valid derived view")
                .is_empty()
        );
    }

    #[test]
    fn test_extract_connections_nested_for_range_depends_on_outer_index() {
        let eq = nested_dependent_for_connection_eq();
        let prefix = ast::QualifiedName::new();
        let source_map = test_source_map();
        let params = ConnectionParams::new();
        let conns = extract_connections(&[eq], &prefix, &params, &source_map).unwrap();

        let mut got: Vec<(String, String)> = conns
            .iter()
            .flat_map(|connection| {
                rumoca_eval_ast::connection::scalar_connection_members(connection)
                    .expect("valid structured connection")
            })
            .map(|connection| (connection.a.to_flat_string(), connection.b.to_flat_string()))
            .collect();
        got.sort();

        let expected = vec![
            ("a[1]".to_string(), "b[2]".to_string()),
            ("a[1]".to_string(), "b[3]".to_string()),
            ("a[2]".to_string(), "b[3]".to_string()),
        ];
        assert_eq!(got, expected);
    }

    #[test]
    fn test_extract_connections_multi_index_range_depends_on_prior_index() {
        let eq = multi_index_dependent_for_connection_eq();
        let prefix = ast::QualifiedName::new();
        let source_map = test_source_map();
        let params = ConnectionParams::new();
        let conns = extract_connections(&[eq], &prefix, &params, &source_map).unwrap();

        let mut got: Vec<(String, String)> = conns
            .iter()
            .flat_map(|connection| {
                rumoca_eval_ast::connection::scalar_connection_members(connection)
                    .expect("valid structured connection")
            })
            .map(|connection| (connection.a.to_flat_string(), connection.b.to_flat_string()))
            .collect();
        got.sort();

        let expected = vec![
            ("a[1]".to_string(), "b[2]".to_string()),
            ("a[1]".to_string(), "b[3]".to_string()),
            ("a[2]".to_string(), "b[3]".to_string()),
        ];
        assert_eq!(got, expected);
    }

    #[test]
    fn test_extract_connections_skips_non_connection_for_equation_range() {
        let eq = ast::Equation::For {
            indices: vec![rumoca_ir_ast::ForIndex {
                ident: make_token("i"),
                range: ast::Expression::ComponentReference(make_comp_ref(&["nout"])),
            }],
            equations: vec![ast::Equation::Simple {
                lhs: make_comp_ref_expr(&["aux", "i"]),
                rhs: make_integer_terminal("0"),
            }],
        };

        let prefix = ast::QualifiedName::new();
        let source_map = SourceMap::new();
        let connections =
            extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map).unwrap();

        assert!(connections.is_empty());
    }

    #[test]
    fn test_extract_regular_for_connection_preserves_one_symbolic_family() {
        let eq = ast::Equation::For {
            indices: vec![rumoca_ir_ast::ForIndex {
                ident: make_token("i"),
                range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("3")),
            }],
            equations: vec![ast::Equation::Connect {
                lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["a"]),
                rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
            }],
        };

        let connections = extract_connections(
            &[eq],
            &ast::QualifiedName::new(),
            &ConnectionParams::new(),
            &test_source_map(),
        )
        .expect("regular vectorized connection should instantiate");

        assert_eq!(connections.len(), 1);
        let family = connections[0]
            .family
            .as_ref()
            .expect("regular vectorized connection must stay symbolic");
        assert_eq!(family.domain.scalar_count(), Ok(3));
        let members = rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
            .expect("symbolic connection must expose a valid derived scalar view")
            .into_iter()
            .map(|member| (member.a.to_flat_string(), member.b.to_flat_string()))
            .collect::<Vec<_>>();
        assert_eq!(
            members,
            vec![
                ("a[1]".to_string(), "b[1]".to_string()),
                ("a[2]".to_string(), "b[2]".to_string()),
                ("a[3]".to_string(), "b[3]".to_string()),
            ]
        );
    }

    #[test]
    fn connection_integer_folding_declines_overflow_without_panicking() {
        assert_eq!(
            eval_binary_i64(&rumoca_core::OpBinary::Div, i64::MIN, -1),
            None
        );
        assert_eq!(
            eval_binary_i64(&rumoca_core::OpBinary::Add, i64::MAX, 1),
            None
        );
        assert_eq!(eval_unary_i64(&rumoca_core::OpUnary::Minus, i64::MIN), None);
        assert_eq!(
            checked_divide_affine(
                &rumoca_core::AffineForm {
                    constant: i64::MIN,
                    coeffs: vec![0],
                },
                -1,
            ),
            None
        );
    }

    fn nested_dependent_for_connection_eq() -> ast::Equation {
        let outer_idx = rumoca_ir_ast::ForIndex {
            ident: make_token("j"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("2")),
        };
        let inner_idx = rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(j_plus_one_expr(), make_integer_terminal("3")),
        };
        ast::Equation::For {
            indices: vec![outer_idx],
            equations: vec![ast::Equation::For {
                indices: vec![inner_idx],
                equations: vec![ast::Equation::Connect {
                    lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["j"]), &["a"]),
                    rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
                }],
            }],
        }
    }

    fn multi_index_dependent_for_connection_eq() -> ast::Equation {
        let prior_idx = rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("2")),
        };
        let dependent_idx = rumoca_ir_ast::ForIndex {
            ident: make_token("j"),
            range: make_range_expr(i_plus_one_expr(), make_integer_terminal("3")),
        };
        ast::Equation::For {
            indices: vec![prior_idx, dependent_idx],
            equations: vec![ast::Equation::Connect {
                lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["a"]),
                rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["j"]), &["b"]),
            }],
        }
    }

    fn i_plus_one_expr() -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: std::sync::Arc::new(make_comp_ref_expr(&["i"])),
            rhs: std::sync::Arc::new(make_integer_terminal("1")),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn j_plus_one_expr() -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: std::sync::Arc::new(make_comp_ref_expr(&["j"])),
            rhs: std::sync::Arc::new(make_integer_terminal("1")),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_component_ref_subscript_resolves_leaf_integer_param_key() {
        // resistor[cellData.nRC].n should keep the subscript when only leaf key
        // The full component-reference path is available in int_params.
        let sub_expr = ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![
                ast::ComponentRefPart {
                    ident: make_token("cellData"),
                    subs: None,
                },
                ast::ComponentRefPart {
                    ident: make_token("nRC"),
                    subs: None,
                },
            ],
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        });
        let cr = make_comp_ref_with_sub(sub_expr, &["resistor", "n"]);
        let prefix = ast::QualifiedName::new();
        let mut int_params = rustc_hash::FxHashMap::default();
        int_params.insert("cellData.nRC".to_string(), 2);

        let qn = component_ref_to_qualified_name(&cr, &prefix, &int_params);
        assert_eq!(qn.to_flat_string(), "resistor[2].n");
    }

    #[test]
    fn test_component_ref_subscript_resolves_scoped_dotted_param_key() {
        // cellData.nRC resolves from cell.cellData.nRC only when the instance
        // scope is cell.
        let sub_expr = ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![
                ast::ComponentRefPart {
                    ident: make_token("cellData"),
                    subs: None,
                },
                ast::ComponentRefPart {
                    ident: make_token("nRC"),
                    subs: None,
                },
            ],
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        });
        let cr = make_comp_ref_with_sub(sub_expr, &["resistor", "n"]);
        let prefix = ast::QualifiedName::from_dotted("cell");
        let mut int_params = rustc_hash::FxHashMap::default();
        int_params.insert("cell.cellData.nRC".to_string(), 2);

        let qn = component_ref_to_qualified_name(&cr, &prefix, &int_params);
        assert_eq!(qn.to_flat_string(), "cell.resistor[2].n");
    }

    #[test]
    fn test_component_ref_subscript_does_not_scan_suffix_param_keys() {
        let cr = ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("nRC"),
                subs: None,
            }],
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        };
        let mut int_params = rustc_hash::FxHashMap::default();
        int_params.insert("cellData.fake_nRC".to_string(), 4);
        int_params.insert("cellData.real.nRC".to_string(), 2);

        let scope = ast::QualifiedName::new();
        assert_eq!(resolve_int_param_ref(&cr, &int_params, &scope), None);
    }
}
