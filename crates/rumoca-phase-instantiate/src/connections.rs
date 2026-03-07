//! Connection extraction for the instantiate phase (MLS §9).
//!
//! This module extracts connect() statements from equations and converts
//! them to ast::InstanceConnection structs.

use rumoca_core::{SourceMap, Span};
use rumoca_ir_ast as ast;

use crate::inheritance::option_location_to_span;

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
) -> Vec<ast::InstanceConnection> {
    if std::env::var("RUMOCA_DEBUG_CONNECTION_PARAMS")
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false)
    {
        let mut ints: Vec<_> = params.integers.iter().collect();
        ints.sort_by(|a, b| a.0.cmp(b.0));
        eprintln!(
            "-- extract_connections debug [{}] int_params={} --",
            prefix,
            ints.len()
        );
        for (k, v) in ints.iter().take(80) {
            eprintln!("  {k} = {v}");
        }
    }

    let mut connections = Vec::new();

    for eq in equations {
        extract_connections_from_equation(&mut connections, eq, prefix, params, source_map);
    }

    connections
}

/// Extract connections from an equation, recursively handling nested structures.
fn extract_connections_from_equation(
    connections: &mut Vec<ast::InstanceConnection>,
    eq: &ast::Equation,
    prefix: &ast::QualifiedName,
    params: &ConnectionParams,
    source_map: &SourceMap,
) {
    match eq {
        ast::Equation::Connect { lhs, rhs, .. } => {
            let span = option_location_to_span(eq.get_location(), source_map);

            // Check if either side has range subscripts (e.g., u[1:2])
            // If so, expand into individual scalar connections
            if let Some(expanded) =
                try_expand_range_subscript_connection(lhs, rhs, prefix, &params.integers, span)
            {
                connections.extend(expanded);
            } else {
                let a = component_ref_to_qualified_name(lhs, prefix, &params.integers);
                let b = component_ref_to_qualified_name(rhs, prefix, &params.integers);

                connections.push(ast::InstanceConnection {
                    a,
                    b,
                    connector_type: None, // Resolved later during flattening
                    span,
                    scope: prefix.to_flat_string(),
                });
            }
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
            );
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
            );
        }

        // Other equation types don't contain connections
        _ => {}
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
) {
    let selected_branch = try_select_branch(cond_blocks, else_block, params);

    if let Some(branch_eqs) = selected_branch {
        // Condition was evaluated - only extract from selected branch
        for nested_eq in &branch_eqs {
            extract_connections_from_equation(connections, nested_eq, prefix, params, source_map);
        }
    } else {
        // Condition couldn't be evaluated - extract from all branches
        extract_connections_from_all_branches(
            connections,
            cond_blocks,
            else_block,
            prefix,
            params,
            source_map,
        );
    }
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
) {
    for block in cond_blocks {
        for nested_eq in &block.eqs {
            extract_connections_from_equation(connections, nested_eq, prefix, params, source_map);
        }
    }
    if let Some(else_eqs) = else_block {
        for nested_eq in else_eqs {
            extract_connections_from_equation(connections, nested_eq, prefix, params, source_map);
        }
    }
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
) {
    if indices.is_empty() {
        // No indices, just process the equations directly
        for eq in equations {
            extract_connections_from_equation(connections, eq, prefix, params, source_map);
        }
        return;
    }

    // Get the first index and expand it
    let first_index = &indices[0];
    let remaining_indices = &indices[1..];
    let index_name = &first_index.ident.text;

    // Try to evaluate the range to get concrete index values, using integer params
    if let Some(range_values) = expand_for_range(&first_index.range, &params.integers) {
        for value in range_values {
            // Substitute the index variable with this value in all equations
            let substituted: Vec<ast::Equation> = equations
                .iter()
                .map(|eq| substitute_index_in_equation(eq, index_name, value))
                .collect();

            // Recursively process with remaining indices
            extract_connections_from_for_equation(
                connections,
                remaining_indices,
                &substituted,
                prefix,
                params,
                source_map,
            );
        }
    } else {
        // Range couldn't be evaluated - fall back to extracting without expansion
        // This may lose subscript information, but avoids failing completely
        for eq in equations {
            extract_connections_from_equation(connections, eq, prefix, params, source_map);
        }
    }
}

/// Try to expand a for-loop range to concrete integer values.
///
/// Uses integer parameters to resolve parameter references like `m` in `1:m`.
fn expand_for_range(
    range_expr: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> Option<Vec<i64>> {
    match range_expr {
        ast::Expression::Range { start, step, end } => {
            let start_val = expr_to_i64_with_params(start, int_params)?;
            let end_val = expr_to_i64_with_params(end, int_params)?;
            let step_val = step
                .as_ref()
                .and_then(|s| expr_to_i64_with_params(s, int_params))
                .unwrap_or(1);

            if step_val == 0 {
                return None;
            }

            // Pre-calculate capacity to avoid reallocations
            let count = if step_val > 0 {
                ((end_val - start_val) / step_val + 1).max(0) as usize
            } else {
                ((start_val - end_val) / (-step_val) + 1).max(0) as usize
            };
            let mut values = Vec::with_capacity(count);
            let mut current = start_val;
            if step_val > 0 {
                while current <= end_val {
                    values.push(current);
                    current += step_val;
                }
            } else {
                while current >= end_val {
                    values.push(current);
                    current += step_val;
                }
            }
            Some(values)
        }
        // Single expression (like just `m` meaning 1:m)
        _ => {
            let n = expr_to_i64_with_params(range_expr, int_params)?;
            if n >= 1 {
                Some((1..=n).collect())
            } else {
                None
            }
        }
    }
}

/// Try to evaluate an expression to i64, using parameter lookup if needed.
/// Handles literals, parameter references, arithmetic, and `div()`.
fn expr_to_i64_with_params(
    expr: &ast::Expression,
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> Option<i64> {
    match expr {
        // Literal integer
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse().ok(),

        // Parameter reference (single-part or multi-part like cellData.nRC)
        ast::Expression::ComponentReference(cr)
            if !cr.parts.is_empty() && cr.parts.iter().all(|p| p.subs.is_none()) =>
        {
            resolve_int_param_ref(cr, int_params)
        }

        // Binary arithmetic
        ast::Expression::Binary { op, lhs, rhs } => {
            let l = expr_to_i64_with_params(lhs, int_params)?;
            let r = expr_to_i64_with_params(rhs, int_params)?;
            eval_binary_i64(op, l, r)
        }

        // Unary
        ast::Expression::Unary { op, rhs } => {
            let val = expr_to_i64_with_params(rhs, int_params)?;
            eval_unary_i64(op, val)
        }

        // Parenthesized
        ast::Expression::Parenthesized { inner } => expr_to_i64_with_params(inner, int_params),

        // Built-in div() function
        ast::Expression::FunctionCall { comp, args }
            if comp.parts.len() == 1
                && comp.parts[0].subs.is_none()
                && comp.parts[0].ident.text.as_ref() == "div"
                && args.len() == 2 =>
        {
            let a = expr_to_i64_with_params(&args[0], int_params)?;
            let b = expr_to_i64_with_params(&args[1], int_params)?;
            if b == 0 { None } else { Some(a / b) }
        }

        _ => None,
    }
}

/// Resolve a component reference to an integer parameter value.
///
/// Uses progressively looser matching:
/// 1. Exact key match (`a.b.c`)
/// 2. Unique dotted suffix match (`x.a.b.c`)
/// 3. Unique leaf-name match (`c`)
///
/// This preserves indexed connect references when integer parameter maps contain
/// local names (`nRC`) or fully-qualified names (`cell.cellData.nRC`).
fn resolve_int_param_ref(
    cr: &ast::ComponentReference,
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> Option<i64> {
    let dotted: String = cr
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    if let Some(v) = int_params.get(dotted.as_str()) {
        return Some(*v);
    }

    let dotted_suffix = format!(".{dotted}");
    let mut suffix_match: Option<i64> = None;
    for (k, v) in int_params {
        if k.ends_with(&dotted_suffix) {
            if suffix_match.is_some() {
                suffix_match = None;
                break;
            }
            suffix_match = Some(*v);
        }
    }
    if suffix_match.is_some() {
        return suffix_match;
    }

    let leaf = cr.parts.last()?.ident.text.as_ref();
    if let Some(v) = int_params.get(leaf) {
        return Some(*v);
    }

    let leaf_suffix = format!(".{leaf}");
    let mut leaf_match: Option<i64> = None;
    for (k, v) in int_params {
        if k.ends_with(&leaf_suffix) {
            if leaf_match.is_some() {
                leaf_match = None;
                break;
            }
            leaf_match = Some(*v);
        }
    }
    leaf_match
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
                    token: rumoca_ir_core::Token {
                        text: std::sync::Arc::from(value.to_string()),
                        location: cr.parts[0].ident.location.clone(),
                        token_number: 0,
                        token_type: 0,
                    },
                }
            } else {
                // Substitute in subscripts
                ast::Expression::ComponentReference(substitute_index_in_comp_ref(
                    cr, var_name, value,
                ))
            }
        }
        ast::Expression::Binary { op, lhs, rhs } => ast::Expression::Binary {
            op: op.clone(),
            lhs: std::sync::Arc::new(substitute_index_in_expr(lhs, var_name, value)),
            rhs: std::sync::Arc::new(substitute_index_in_expr(rhs, var_name, value)),
        },
        ast::Expression::Unary { op, rhs } => ast::Expression::Unary {
            op: op.clone(),
            rhs: std::sync::Arc::new(substitute_index_in_expr(rhs, var_name, value)),
        },
        ast::Expression::Parenthesized { inner } => ast::Expression::Parenthesized {
            inner: std::sync::Arc::new(substitute_index_in_expr(inner, var_name, value)),
        },
        ast::Expression::Array {
            elements,
            is_matrix,
        } => ast::Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_index_in_expr(e, var_name, value))
                .collect(),
            is_matrix: *is_matrix,
        },
        ast::Expression::FunctionCall { comp, args } => ast::Expression::FunctionCall {
            comp: substitute_index_in_comp_ref(comp, var_name, value),
            args: args
                .iter()
                .map(|a| substitute_index_in_expr(a, var_name, value))
                .collect(),
        },
        ast::Expression::Range { start, step, end } => ast::Expression::Range {
            start: std::sync::Arc::new(substitute_index_in_expr(start, var_name, value)),
            step: step
                .as_ref()
                .map(|s| std::sync::Arc::new(substitute_index_in_expr(s, var_name, value))),
            end: std::sync::Arc::new(substitute_index_in_expr(end, var_name, value)),
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
    params: &ConnectionParams,
) -> Option<Vec<ast::Equation>> {
    for block in cond_blocks {
        if let Some(value) = try_eval_bool_expr(&block.cond, &params.bools, &params.integers) {
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
) -> Option<bool> {
    match expr {
        // Literal boolean (true or false)
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },

        // Parameter reference
        ast::Expression::ComponentReference(cr) => {
            let name = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            bool_params.get(&name).copied()
        }

        // Not expression
        ast::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Not(_),
            rhs: inner,
        } => try_eval_bool_expr(inner, bool_params, int_params).map(|v| !v),

        // And expression
        ast::Expression::Binary {
            op: rumoca_ir_core::OpBinary::And(_),
            lhs,
            rhs,
        } => {
            let l = try_eval_bool_expr(lhs, bool_params, int_params)?;
            let r = try_eval_bool_expr(rhs, bool_params, int_params)?;
            Some(l && r)
        }

        // Or expression
        ast::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Or(_),
            lhs,
            rhs,
        } => {
            let l = try_eval_bool_expr(lhs, bool_params, int_params)?;
            let r = try_eval_bool_expr(rhs, bool_params, int_params)?;
            Some(l || r)
        }

        // Integer comparison expressions (e.g., i > 1 after index substitution)
        ast::Expression::Binary { op, lhs, rhs } => {
            let l = expr_to_i64_with_params(lhs, int_params)?;
            let r = expr_to_i64_with_params(rhs, int_params)?;
            match op {
                rumoca_ir_core::OpBinary::Gt(_) => Some(l > r),
                rumoca_ir_core::OpBinary::Ge(_) => Some(l >= r),
                rumoca_ir_core::OpBinary::Lt(_) => Some(l < r),
                rumoca_ir_core::OpBinary::Le(_) => Some(l <= r),
                rumoca_ir_core::OpBinary::Eq(_) => Some(l == r),
                rumoca_ir_core::OpBinary::Neq(_) => Some(l != r),
                _ => None,
            }
        }

        // Parenthesized boolean expression
        ast::Expression::Parenthesized { inner } => {
            try_eval_bool_expr(inner, bool_params, int_params)
        }

        _ => None,
    }
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
                .filter_map(|sub| subscript_to_i64(sub, int_params))
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
) -> Option<i64> {
    match sub {
        ast::Subscript::Expression(expr) => expr_to_i64_with_params(expr, int_params),
        ast::Subscript::Range { .. } | ast::Subscript::Empty => None,
    }
}

/// Evaluate a binary integer operation.
fn eval_binary_i64(op: &rumoca_ir_core::OpBinary, l: i64, r: i64) -> Option<i64> {
    match op {
        rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => Some(l + r),
        rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => Some(l - r),
        rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => Some(l * r),
        rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => {
            if r == 0 {
                None
            } else {
                Some(l / r)
            }
        }
        _ => None,
    }
}

/// Evaluate a unary integer operation.
fn eval_unary_i64(op: &rumoca_ir_core::OpUnary, val: i64) -> Option<i64> {
    match op {
        rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => Some(-val),
        rumoca_ir_core::OpUnary::Plus(_) | rumoca_ir_core::OpUnary::DotPlus(_) => Some(val),
        _ => None,
    }
}

/// Try to expand a connect statement with range subscripts into individual scalar connections.
///
/// For `connect(a.y, b.u[1:2])`, this expands to:
///   `connect(a.y[1], b.u[1])`, `connect(a.y[2], b.u[2])`
///
/// Returns None if neither side has range subscripts (normal connection).
fn try_expand_range_subscript_connection(
    lhs: &ast::ComponentReference,
    rhs: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    int_params: &rustc_hash::FxHashMap<String, i64>,
    span: Span,
) -> Option<Vec<ast::InstanceConnection>> {
    let lhs_range = extract_range_subscript(lhs, int_params);
    let rhs_range = extract_range_subscript(rhs, int_params);

    // If neither side has range subscripts, return None (use normal path)
    if lhs_range.is_none() && rhs_range.is_none() {
        return None;
    }

    // Expand the range(s) into individual scalar connections
    let mut expanded = Vec::new();

    match (lhs_range, rhs_range) {
        (Some((lhs_part_idx, lhs_values)), Some((rhs_part_idx, rhs_values))) => {
            // Both sides have ranges - they must be the same length
            if lhs_values.len() != rhs_values.len() {
                return None; // Mismatch, fall through to normal path
            }
            for (lv, rv) in lhs_values.iter().zip(rhs_values.iter()) {
                let new_lhs = replace_range_with_index(lhs, lhs_part_idx, *lv);
                let new_rhs = replace_range_with_index(rhs, rhs_part_idx, *rv);
                let a = component_ref_to_qualified_name(&new_lhs, prefix, int_params);
                let b = component_ref_to_qualified_name(&new_rhs, prefix, int_params);
                expanded.push(ast::InstanceConnection {
                    a,
                    b,
                    connector_type: None,
                    span,
                    scope: prefix.to_flat_string(),
                });
            }
        }
        (Some((lhs_part_idx, lhs_values)), None) => {
            // Only LHS has range - expand LHS, keep RHS as-is or add matching indices
            let b_base = component_ref_to_qualified_name(rhs, prefix, int_params);
            for (i, lv) in lhs_values.iter().enumerate() {
                let new_lhs = replace_range_with_index(lhs, lhs_part_idx, *lv);
                let a = component_ref_to_qualified_name(&new_lhs, prefix, int_params);
                // If RHS has no subscripts, add matching index
                let b = add_index_to_qualified_name(&b_base, (i + 1) as i64);
                expanded.push(ast::InstanceConnection {
                    a,
                    b,
                    connector_type: None,
                    span,
                    scope: prefix.to_flat_string(),
                });
            }
        }
        (None, Some((rhs_part_idx, rhs_values))) => {
            // Only RHS has range - expand RHS, add matching indices to LHS
            let a_base = component_ref_to_qualified_name(lhs, prefix, int_params);
            for (i, rv) in rhs_values.iter().enumerate() {
                let new_rhs = replace_range_with_index(rhs, rhs_part_idx, *rv);
                let b = component_ref_to_qualified_name(&new_rhs, prefix, int_params);
                let a = add_index_to_qualified_name(&a_base, (i + 1) as i64);
                expanded.push(ast::InstanceConnection {
                    a,
                    b,
                    connector_type: None,
                    span,
                    scope: prefix.to_flat_string(),
                });
            }
        }
        (None, None) => unreachable!(), // Already checked above
    }

    if expanded.is_empty() {
        None
    } else {
        Some(expanded)
    }
}

/// Extract range subscript information from a component reference.
///
/// Returns Some((part_index, values)) if a range subscript like [1:3] is found.
/// `part_index` is the index into the component reference parts where the range is.
fn extract_range_subscript(
    comp_ref: &ast::ComponentReference,
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> Option<(usize, Vec<i64>)> {
    for (part_idx, part) in comp_ref.parts.iter().enumerate() {
        let Some(subs) = part.subs.as_ref() else {
            continue;
        };
        for sub in subs {
            let values = try_expand_subscript_range(sub, int_params);
            if let Some(values) = values {
                return Some((part_idx, values));
            }
        }
    }
    None
}

/// Try to expand a single subscript range into concrete index values.
fn try_expand_subscript_range(
    sub: &ast::Subscript,
    int_params: &rustc_hash::FxHashMap<String, i64>,
) -> Option<Vec<i64>> {
    let (start, step, end) = match sub {
        ast::Subscript::Expression(ast::Expression::Range { start, step, end }) => {
            (start, step, end)
        }
        _ => return None,
    };
    let start_val = expr_to_i64_with_params(start, int_params)?;
    let end_val = expr_to_i64_with_params(end, int_params)?;
    let step_val = step
        .as_ref()
        .and_then(|s| expr_to_i64_with_params(s, int_params))
        .unwrap_or(1);
    if step_val == 0 {
        return None;
    }
    let mut values = Vec::new();
    let mut current = start_val;
    if step_val > 0 {
        while current <= end_val {
            values.push(current);
            current += step_val;
        }
    } else {
        while current >= end_val {
            values.push(current);
            current += step_val;
        }
    }
    Some(values)
}

/// Replace a range subscript at the given part index with a concrete integer index.
fn replace_range_with_index(
    comp_ref: &ast::ComponentReference,
    part_idx: usize,
    value: i64,
) -> ast::ComponentReference {
    ast::ComponentReference {
        local: comp_ref.local,
        parts: comp_ref
            .parts
            .iter()
            .enumerate()
            .map(|(i, part)| {
                if i == part_idx {
                    rumoca_ir_ast::ComponentRefPart {
                        ident: part.ident.clone(),
                        subs: Some(vec![ast::Subscript::Expression(
                            ast::Expression::Terminal {
                                terminal_type: ast::TerminalType::UnsignedInteger,
                                token: rumoca_ir_core::Token {
                                    text: std::sync::Arc::from(value.to_string()),
                                    location: part.ident.location.clone(),
                                    token_number: 0,
                                    token_type: 0,
                                },
                            },
                        )]),
                    }
                } else {
                    part.clone()
                }
            })
            .collect(),
        def_id: comp_ref.def_id,
    }
}

/// Add an index subscript to the last part of a qualified name.
fn add_index_to_qualified_name(qn: &ast::QualifiedName, index: i64) -> ast::QualifiedName {
    let mut result = qn.clone();
    if let Some(last) = result.parts.last_mut() {
        last.1.push(index);
    }
    result
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

    fn make_token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: std::sync::Arc::from(text),
            location: rumoca_ir_core::Location::default(),
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
        }
    }

    #[test]
    fn test_extract_connection() {
        let eq = ast::Equation::Connect {
            lhs: make_comp_ref(&["a", "p"]),
            rhs: make_comp_ref(&["b", "n"]),
        };

        let prefix = ast::QualifiedName::new();
        let source_map = SourceMap::new();
        let connections =
            extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map);

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
            }),
            step: None,
            end: std::sync::Arc::new(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: make_token("2"),
            }),
        };
        let eq = ast::Equation::Connect {
            lhs: make_comp_ref(&["mux2", "y"]),
            rhs: make_comp_ref_with_sub_at(range, &["mux5", "u"], 1),
        };

        let prefix = ast::QualifiedName::new();
        let source_map = SourceMap::new();
        let connections =
            extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map);

        let mut got: Vec<(String, String)> = connections
            .iter()
            .map(|c| (c.a.to_flat_string(), c.b.to_flat_string()))
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
    fn test_extract_connections_nested_for_range_depends_on_outer_index() {
        // for j in 1:2 loop
        //   for i in j+1:3 loop
        //     connect(a[j], b[i]);
        //   end for;
        // end for;
        //
        // Expected expansion:
        // j=1 -> i=2,3 => connect(a[1], b[2]), connect(a[1], b[3])
        // j=2 -> i=3   => connect(a[2], b[3])
        let outer_idx = rumoca_ir_ast::ForIndex {
            ident: make_token("j"),
            range: ast::Expression::Range {
                start: std::sync::Arc::new(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: make_token("1"),
                }),
                step: None,
                end: std::sync::Arc::new(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: make_token("2"),
                }),
            },
        };
        let inner_idx = rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: ast::Expression::Range {
                start: std::sync::Arc::new(ast::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(make_token("+")),
                    lhs: std::sync::Arc::new(ast::Expression::ComponentReference(
                        ast::ComponentReference {
                            local: false,
                            parts: vec![ast::ComponentRefPart {
                                ident: make_token("j"),
                                subs: None,
                            }],
                            def_id: None,
                        },
                    )),
                    rhs: std::sync::Arc::new(ast::Expression::Terminal {
                        terminal_type: ast::TerminalType::UnsignedInteger,
                        token: make_token("1"),
                    }),
                }),
                step: None,
                end: std::sync::Arc::new(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: make_token("3"),
                }),
            },
        };

        let eq = ast::Equation::For {
            indices: vec![outer_idx],
            equations: vec![ast::Equation::For {
                indices: vec![inner_idx],
                equations: vec![ast::Equation::Connect {
                    lhs: ast::ComponentReference {
                        local: false,
                        parts: vec![ast::ComponentRefPart {
                            ident: make_token("a"),
                            subs: Some(vec![ast::Subscript::Expression(
                                ast::Expression::ComponentReference(ast::ComponentReference {
                                    local: false,
                                    parts: vec![ast::ComponentRefPart {
                                        ident: make_token("j"),
                                        subs: None,
                                    }],
                                    def_id: None,
                                }),
                            )]),
                        }],
                        def_id: None,
                    },
                    rhs: ast::ComponentReference {
                        local: false,
                        parts: vec![ast::ComponentRefPart {
                            ident: make_token("b"),
                            subs: Some(vec![ast::Subscript::Expression(
                                ast::Expression::ComponentReference(ast::ComponentReference {
                                    local: false,
                                    parts: vec![ast::ComponentRefPart {
                                        ident: make_token("i"),
                                        subs: None,
                                    }],
                                    def_id: None,
                                }),
                            )]),
                        }],
                        def_id: None,
                    },
                }],
            }],
        };

        let prefix = ast::QualifiedName::new();
        let source_map = SourceMap::new();
        let params = ConnectionParams::new();
        let conns = extract_connections(&[eq], &prefix, &params, &source_map);

        let mut got: Vec<(String, String)> = conns
            .iter()
            .map(|c| (c.a.to_flat_string(), c.b.to_flat_string()))
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
    fn test_component_ref_subscript_resolves_leaf_integer_param_key() {
        // resistor[cellData.nRC].n should keep the subscript when only leaf key
        // nRC is available in int_params.
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
        });
        let cr = make_comp_ref_with_sub(sub_expr, &["resistor", "n"]);
        let prefix = ast::QualifiedName::new();
        let mut int_params = rustc_hash::FxHashMap::default();
        int_params.insert("nRC".to_string(), 2);

        let qn = component_ref_to_qualified_name(&cr, &prefix, &int_params);
        assert_eq!(qn.to_flat_string(), "resistor[2].n");
    }

    #[test]
    fn test_component_ref_subscript_resolves_unique_dotted_suffix_param_key() {
        // cellData.nRC should resolve from a qualified key like x.cellData.nRC.
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
        });
        let cr = make_comp_ref_with_sub(sub_expr, &["resistor", "n"]);
        let prefix = ast::QualifiedName::new();
        let mut int_params = rustc_hash::FxHashMap::default();
        int_params.insert("cell.cellData.nRC".to_string(), 2);

        let qn = component_ref_to_qualified_name(&cr, &prefix, &int_params);
        assert_eq!(qn.to_flat_string(), "resistor[2].n");
    }
}
