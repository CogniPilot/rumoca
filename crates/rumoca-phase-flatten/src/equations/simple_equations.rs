use super::*;

pub(super) fn expand_to_simple_equations(
    ctx: &Context,
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    operators: &ast::ConnectionOperatorCatalog,
) -> Result<Vec<SimpleEquation>, FlattenError> {
    let mut result = Vec::new();

    for eq in equations {
        match eq {
            ast::Equation::Empty => {
                return Err(FlattenError::invalid_ast_recovery(
                    "Equation::Empty is a parser-recovery node",
                    span,
                ));
            }
            ast::Equation::Simple { lhs, rhs } => {
                let expanded = expand_array_assignment(lhs, rhs);
                result.extend(expanded);
            }

            ast::Equation::For { indices, equations } => {
                // Expand for-equation to simple equations
                let expanded =
                    expand_for_to_simple(ctx, indices, equations, prefix, span, operators)?;
                result.extend(expanded);
            }

            ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                // For nested if-equations, try constant condition evaluation first
                if let Some(selected) =
                    try_select_constant_branch(ctx, cond_blocks, else_block, prefix, operators)?
                {
                    let expanded =
                        expand_to_simple_equations(ctx, &selected, prefix, span, operators)?;
                    result.extend(expanded);
                } else {
                    // Non-constant nested if-equation - expand recursively
                    let nested = expand_nested_if_to_simple(
                        ctx,
                        cond_blocks,
                        else_block,
                        prefix,
                        span,
                        operators,
                    )?;
                    result.extend(nested);
                }
            }

            ast::Equation::Connect { .. }
            | ast::Equation::Assert { .. }
            | ast::Equation::When(_)
            | ast::Equation::FunctionCall { .. } => {
                // Skip these - they don't contribute to regular flat equations:
                // - Connect: handled separately in connections module
                // - Assert: runtime checks, not equation system
                // - When: handled separately by flatten_when_equation
                // - FunctionCall: typically assert(), Modelica.Utilities.*, etc.
            }
        }
    }

    Ok(result)
}

/// Expand a simple equation with an array RHS into per-element equations.
///
/// For `x = {e1, e2, e3}` where x is a ast::ComponentReference, produces:
/// `x[1] = e1, x[2] = e2, x[3] = e3`
///
/// Handles nested arrays recursively for multi-dimensional cases.
/// Falls back to a single equation when the RHS is not an array.
pub(super) fn expand_array_assignment(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
) -> Vec<SimpleEquation> {
    // A named aggregate is one authoritative tensor equation. Preserve it so
    // conditional branches such as `x = zeros(2)` and `x = {a, b}` have the
    // same owner cardinality; scalar rows derive from that owner downstream.
    if matches!(lhs, ast::Expression::ComponentReference(_)) {
        return vec![SimpleEquation {
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        }];
    }
    let rhs_elements = match rhs {
        ast::Expression::Array { elements, .. } if !elements.is_empty() => elements,
        _ => {
            return vec![SimpleEquation {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }];
        }
    };
    expand_array_lhs_elements(lhs, rhs, rhs_elements)
}

/// Expand array assignment given the RHS elements extracted from an Array expression.
pub(super) fn expand_array_lhs_elements(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    rhs_elements: &[ast::Expression],
) -> Vec<SimpleEquation> {
    match lhs {
        ast::Expression::Array {
            elements: lhs_elements,
            ..
        } => lhs_elements
            .iter()
            .zip(rhs_elements.iter())
            .flat_map(|(l, r)| expand_array_assignment(l, r))
            .collect(),
        _ => vec![SimpleEquation {
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        }],
    }
}

/// Expand a for-equation to simple equations.
pub(super) fn expand_for_to_simple(
    ctx: &Context,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    operators: &ast::ConnectionOperatorCatalog,
) -> Result<Vec<SimpleEquation>, FlattenError> {
    if indices.is_empty() {
        return expand_to_simple_equations(ctx, equations, prefix, span, operators);
    }

    let first_index = &indices[0];
    let remaining_indices = &indices[1..];

    let index_values = expand_range_indices(ctx, &first_index.range, prefix, span)?;
    let index_name = &first_index.ident.text;

    let mut result = Vec::new();
    for value in index_values {
        // Substitute index variable in all equations
        let substituted: Vec<ast::Equation> = equations
            .iter()
            .map(|eq| substitute_index_in_equation(eq, index_name, value))
            .collect();

        // Recursively expand remaining indices
        let expanded = expand_for_to_simple(
            ctx,
            remaining_indices,
            &substituted,
            prefix,
            span,
            operators,
        )?;
        result.extend(expanded);
    }

    Ok(result)
}
