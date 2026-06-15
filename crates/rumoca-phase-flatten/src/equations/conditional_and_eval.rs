// SPEC_0021 file-size exception: split plan is to move focused conditional
// equation helpers into owned submodules after BOPTEST parity stabilization.
use super::*;

/// Expand a nested non-constant if-equation to simple equations with conditional RHS.
///
/// For an if-equation like:
/// ```text
/// if cond then
///   x = a;
/// else
///   x = b;
/// end if;
/// ```
///
/// Produces:
/// ```text
/// SimpleEquation { lhs: x, rhs: if cond then a else b }
/// ```
pub(super) fn expand_nested_if_to_simple(
    ctx: &Context,
    cond_blocks: &[EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &QualifiedName,
    span: rumoca_core::Span,
) -> Result<Vec<SimpleEquation>, FlattenError> {
    if cond_blocks.is_empty() {
        return Ok(vec![]);
    }

    // Expand each branch to simple equations
    let mut expanded_branches: Vec<(ast::Expression, Vec<SimpleEquation>)> = Vec::new();

    for block in cond_blocks {
        let simple_eqs = expand_to_simple_equations(ctx, &block.eqs, prefix, span)?;
        expanded_branches.push((block.cond.clone(), simple_eqs));
    }

    let else_simple_eqs = if let Some(else_eqs) = else_block {
        expand_to_simple_equations(ctx, else_eqs, prefix, span)?
    } else {
        vec![]
    };

    // Verify all branches have the same number of equations
    let num_equations = expanded_branches[0].1.len();
    for (i, (_, eqs)) in expanded_branches.iter().enumerate().skip(1) {
        if eqs.len() != num_equations {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "nested if-equation: branch {} has {} equations, but first branch has {}",
                    i + 1,
                    eqs.len(),
                    num_equations
                ),
                span,
            ));
        }
    }

    if !else_simple_eqs.is_empty() && else_simple_eqs.len() != num_equations {
        return Err(FlattenError::unsupported_equation(
            format!(
                "nested if-equation: else branch has {} equations, but if branch has {}",
                else_simple_eqs.len(),
                num_equations
            ),
            span,
        ));
    }

    // Create simple equations with conditional residual semantics.
    // Use `(if ... then residual_i else residual_j) = 0` so branch equations
    // that target different variables remain semantically correct.
    let mut result = Vec::new();
    for eq_idx in 0..num_equations {
        let branches: Vec<(ast::Expression, ast::Expression)> = expanded_branches
            .iter()
            .map(|(cond, eqs)| (cond.clone(), build_simple_equation_residual(&eqs[eq_idx])))
            .collect();

        let else_residual = if !else_simple_eqs.is_empty() {
            build_simple_equation_residual(&else_simple_eqs[eq_idx])
        } else {
            ast::Expression::Binary {
                op: OpBinary::Sub,
                lhs: Arc::new(expanded_branches[0].1[eq_idx].lhs.clone()),
                rhs: Arc::new(zero_real_expr()),
                span,
            }
        };

        let conditional_residual = ast::Expression::If {
            branches,
            else_branch: Arc::new(else_residual),
            span,
        };

        result.push(SimpleEquation {
            lhs: conditional_residual,
            rhs: zero_real_expr(),
        });
    }

    Ok(result)
}

/// Create a single conditional equation from pre-expanded simple equations.
pub(super) struct ConditionalEquationContext<'a> {
    pub(super) ctx: &'a Context,
    pub(super) prefix: &'a QualifiedName,
    pub(super) span: rumoca_core::Span,
    pub(super) origin: &'a rumoca_ir_flat::EquationOrigin,
    pub(super) imports: &'a crate::qualify::ImportMap,
    pub(super) def_map: Option<&'a crate::ResolveDefMap>,
}

pub(super) fn create_conditional_equation_from_simple(
    expanded_branches: &[(ast::Expression, Vec<SimpleEquation>)],
    else_simple_eqs: &[SimpleEquation],
    eq_idx: usize,
    context: &ConditionalEquationContext<'_>,
) -> Result<flat::Equation, FlattenError> {
    // Build a conditional residual directly:
    // if c1 then (lhs1-rhs1) elseif c2 then (lhs2-rhs2) else (lhsN-rhsN)
    // This preserves MLS semantics even when branch equations do not share the same lhs.
    let conditional_residual = build_conditional_residual_from_simple(
        expanded_branches,
        else_simple_eqs,
        eq_idx,
        context.span,
    )?;
    let residual = qualify_expression_imports_with_def_map_ctx(
        &conditional_residual,
        context.prefix,
        context.imports,
        context.def_map,
        context.ctx,
    )?;
    Ok(flat::Equation::new(
        residual,
        context.span,
        context.origin.clone(),
    ))
}

fn build_simple_equation_residual(eq: &SimpleEquation) -> ast::Expression {
    ast::Expression::Binary {
        op: OpBinary::Sub,
        lhs: Arc::new(eq.lhs.clone()),
        rhs: Arc::new(eq.rhs.clone()),
        span: eq.lhs.span(),
    }
}

fn zero_real_expr() -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: TerminalType::UnsignedReal,
        token: Token {
            text: std::sync::Arc::from("0.0"),
            ..Default::default()
        },
        span: rumoca_core::Span::DUMMY,
    }
}

/// Build a conditional residual expression from pre-expanded simple equations.
fn build_conditional_residual_from_simple(
    expanded_branches: &[(ast::Expression, Vec<SimpleEquation>)],
    else_simple_eqs: &[SimpleEquation],
    eq_idx: usize,
    span: rumoca_core::Span,
) -> Result<ast::Expression, FlattenError> {
    // Collect (condition, residual) pairs.
    let branches: Vec<(ast::Expression, ast::Expression)> = expanded_branches
        .iter()
        .map(|(cond, eqs)| (cond.clone(), build_simple_equation_residual(&eqs[eq_idx])))
        .collect();

    // Get else branch residual.
    let else_residual = if !else_simple_eqs.is_empty() {
        build_simple_equation_residual(&else_simple_eqs[eq_idx])
    } else {
        // Preserve prior lowering semantics for if-equations without else:
        // residual = lhs - (if cond then rhs else 0)  => else residual is lhs - 0.
        ast::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Arc::new(expanded_branches[0].1[eq_idx].lhs.clone()),
            rhs: Arc::new(zero_real_expr()),
            span,
        }
    };

    // Build the if-expression
    Ok(ast::Expression::If {
        branches,
        else_branch: Arc::new(else_residual),
        span,
    })
}

/// Flatten a simple equation within a constant-branch list, including empty range
/// elimination (MLS §10.5).
fn flatten_simple_in_list(
    ctx: &Context,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &QualifiedName,
    span: rumoca_core::Span,
    origin: &rumoca_ir_flat::EquationOrigin,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::Equation>, FlattenError> {
    // MLS §10.5: Skip equations with empty range subscripts
    if has_empty_range_subscript(ctx, lhs, prefix) || has_empty_range_subscript(ctx, rhs, prefix) {
        return Ok(vec![]);
    }

    // Keep array comprehensions in equations by expanding structural ranges.
    let lhs = expand_array_comprehensions_in_expression(ctx, lhs, prefix, span)?;
    let rhs = expand_array_comprehensions_in_expression(ctx, rhs, prefix, span)?;

    let residual = make_residual(ctx, &lhs, &rhs, prefix, def_map)?;
    let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, prefix, ctx);
    if scalar_count == 0 {
        return Ok(vec![]);
    }
    let equation = if scalar_count == 1 {
        flat::Equation::new(residual, span, origin.clone())
    } else {
        flat::Equation::new_array(residual, span, origin.clone(), scalar_count)
    };
    Ok(vec![equation])
}

/// Flatten a list of equations (used for expanded for/if-equations).
/// Returns both regular equations and when-clauses (MLS §8.3.3/§8.3.5 allows nested when).
pub(super) fn flatten_equations_list(
    ctx: &Context,
    equations: &[ast::Equation],
    prefix: &QualifiedName,
    span: rumoca_core::Span,
    origin: &rumoca_ir_flat::EquationOrigin,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<FlattenedEquations, FlattenError> {
    let mut result = FlattenedEquations::default();
    for eq in equations {
        match eq {
            ast::Equation::Simple { lhs, rhs } => {
                let eqs = flatten_simple_in_list(ctx, lhs, rhs, prefix, span, origin, def_map)?;
                result.equations.extend(eqs);
            }
            ast::Equation::For { indices, equations } => {
                let expanded =
                    expand_for_equation(ctx, indices, equations, prefix, span, origin, def_map)?;
                result.append(expanded);
            }
            ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                let expanded = expand_if_equation(
                    ctx,
                    cond_blocks,
                    else_block,
                    prefix,
                    span,
                    origin,
                    def_map,
                )?;
                result.append(expanded);
            }
            ast::Equation::Assert {
                condition,
                message,
                level,
            } => {
                let imports = &ctx.current_imports;
                let assert_eq = AssertEquation::new(
                    qualify_expression_imports_with_def_map_ctx(
                        condition, prefix, imports, def_map, ctx,
                    )?,
                    qualify_expression_imports_with_def_map_ctx(
                        message, prefix, imports, def_map, ctx,
                    )?,
                    level
                        .as_ref()
                        .map(|expr| {
                            qualify_expression_imports_with_def_map_ctx(
                                expr, prefix, imports, def_map, ctx,
                            )
                        })
                        .transpose()?,
                    span,
                );
                result.assert_equations.push(assert_eq);
            }
            ast::Equation::Empty | ast::Equation::Connect { .. } => {
                // Skip these
            }
            ast::Equation::When(blocks) => {
                // MLS §8.3.3/§8.3.5: When-equations inside for-loops are allowed.
                // Flatten each when-block with the current prefix (which includes for-loop indices)
                let clauses =
                    crate::when_equations::flatten_when_blocks(ctx, blocks, prefix, span, def_map)?;
                result.when_clauses.extend(clauses);
            }
            ast::Equation::FunctionCall { comp, args } => {
                let flattened =
                    flatten_function_call_equation(ctx, comp, args, prefix, span, def_map)?;
                if flattened.is_empty() && !is_side_effect_only_function(comp) {
                    return Err(FlattenError::unsupported_equation(
                        format!(
                            "function call equation '{}' in nested context not yet supported",
                            comp
                        ),
                        span,
                    ));
                }
                if flattened.is_empty() {
                    // Side-effect-only calls don't contribute to DAE equations.
                    continue;
                }
                result.append(flattened);
            }
        }
    }
    Ok(result)
}

/// Expand a range expression to a list of integer indices.
///
/// Supports:
/// - Literal ranges like 1:3 → [1, 2, 3]
/// - Literal ranges with step like 1:2:5 → [1, 3, 5]
/// - Parameter references like 1:n where n is an integer parameter
pub(crate) fn expand_range_indices(
    ctx: &Context,
    range_expr: &ast::Expression,
    prefix: &QualifiedName,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, FlattenError> {
    let scope = {
        let s = prefix.to_flat_string();
        if s.is_empty() {
            "<root>".to_string()
        } else {
            s
        }
    };
    match range_expr {
        ast::Expression::Range {
            start, step, end, ..
        } => {
            let start_val = try_eval_integer_with_ctx(ctx, start, prefix).ok_or_else(|| {
                FlattenError::unsupported_equation(
                    format!(
                        "for-equation range start must be a constant integer or parameter (scope `{scope}`, got `{}`)",
                        format_subscript_expr(start),
                    ),
                    span,
                )
            })?;
            let end_val = try_eval_integer_with_ctx(ctx, end, prefix).ok_or_else(|| {
                FlattenError::unsupported_equation(
                    format!(
                        "for-equation range end must be a constant integer or parameter (scope `{scope}`, got `{}`)",
                        format_subscript_expr(end),
                    ),
                    span,
                )
            })?;
            let step_val = match step {
                Some(s) => try_eval_integer_with_ctx(ctx, s, prefix).ok_or_else(|| {
                    FlattenError::unsupported_equation(
                        format!(
                            "for-equation range step must be a constant integer or parameter (scope `{scope}`, got `{}`)",
                            format_subscript_expr(s),
                        ),
                        span,
                    )
                })?,
                None => 1,
            };

            if step_val == 0 {
                return Err(FlattenError::unsupported_equation(
                    "for-equation range step cannot be zero",
                    span,
                ));
            }

            let mut indices = Vec::new();
            if step_val > 0 {
                let mut i = start_val;
                while i <= end_val {
                    indices.push(i);
                    i += step_val;
                }
            } else {
                let mut i = start_val;
                while i >= end_val {
                    indices.push(i);
                    i += step_val;
                }
            }
            Ok(indices)
        }
        ast::Expression::FunctionCall { comp, args, .. } if is_linspace_function_call(comp) => {
            expand_linspace_range_indices(ctx, args, prefix, &scope, span)
        }
        // Handle a single integer as 1:n
        _ => {
            if let Some(n) = try_eval_integer_with_ctx(ctx, range_expr, prefix) {
                Ok((1..=n).collect())
            } else {
                Err(FlattenError::unsupported_equation(
                    format!(
                        "for-equation range must be a constant range expression or parameter (scope `{scope}`, got `{}`)",
                        format_subscript_expr(range_expr)
                    ),
                    span,
                ))
            }
        }
    }
}

fn is_linspace_function_call(comp: &ast::ComponentReference) -> bool {
    comp.parts
        .last()
        .is_some_and(|part| part.ident.text.as_ref() == "linspace")
}

fn expand_linspace_range_indices(
    ctx: &Context,
    args: &[ast::Expression],
    prefix: &QualifiedName,
    scope: &str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, FlattenError> {
    if args.len() != 3 {
        return Err(FlattenError::unsupported_equation(
            format!("for-equation linspace range requires exactly 3 arguments (scope `{scope}`)"),
            span,
        ));
    }

    let start = try_eval_integer_with_ctx(ctx, &args[0], prefix).ok_or_else(|| {
        FlattenError::unsupported_equation(
            format!(
                "for-equation linspace start must be a constant integer or parameter (scope `{scope}`, got `{}`)",
                format_subscript_expr(&args[0]),
            ),
            span,
        )
    })?;
    let end = try_eval_integer_with_ctx(ctx, &args[1], prefix).ok_or_else(|| {
        FlattenError::unsupported_equation(
            format!(
                "for-equation linspace end must be a constant integer or parameter (scope `{scope}`, got `{}`)",
                format_subscript_expr(&args[1]),
            ),
            span,
        )
    })?;
    let count = try_eval_integer_with_ctx(ctx, &args[2], prefix).ok_or_else(|| {
        FlattenError::unsupported_equation(
            format!(
                "for-equation linspace count must be a constant integer or parameter (scope `{scope}`, got `{}`)",
                format_subscript_expr(&args[2]),
            ),
            span,
        )
    })?;

    if count <= 0 {
        return Err(FlattenError::unsupported_equation(
            format!("for-equation linspace count must be positive (scope `{scope}`, got {count})"),
            span,
        ));
    }
    if count == 1 {
        return Ok(vec![start]);
    }

    let mut indices = Vec::with_capacity(count as usize);
    for i in 0..count {
        let numerator = i * (end - start);
        let value = start + numerator / (count - 1);
        indices.push(value);
    }
    Ok(indices)
}

/// Try to evaluate an expression to a constant integer, with parameter lookup.
fn try_eval_integer_with_ctx(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &QualifiedName,
) -> Option<i64> {
    let result = match expr {
        ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),

        ast::Expression::ComponentReference(cr) => {
            // Look up in parameter values with parent scope resolution
            let val = lookup_parameter_in_scope(ctx, cr, prefix);
            #[cfg(feature = "tracing")]
            if val.is_none() {
                let qualified_name = build_qualified_name(prefix, cr);
                debug!(param = %qualified_name, known_params = ?ctx.parameter_values.keys().take(10).collect::<Vec<_>>(), "parameter not found in any scope");
            }
            val
        }

        ast::Expression::Unary { op, rhs, .. } => {
            let val = try_eval_integer_with_ctx(ctx, rhs, prefix)?;
            match op {
                rumoca_core::OpUnary::Minus => Some(-val),
                rumoca_core::OpUnary::Plus => Some(val),
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner, .. } => {
            try_eval_integer_with_ctx(ctx, inner, prefix)
        }

        ast::Expression::Binary { op, lhs, rhs, .. } => {
            // Support simple arithmetic for parameter expressions like n-1, n+1
            let l = try_eval_integer_with_ctx(ctx, lhs, prefix)?;
            let r = try_eval_integer_with_ctx(ctx, rhs, prefix)?;
            rumoca_core::eval_ast_integer_binary(op, l, r)
        }

        ast::Expression::FunctionCall { comp, args, .. } => {
            try_eval_builtin_function(ctx, comp, args, prefix)
        }

        _ => {
            // Fall back to rumoca_eval_const for complex expressions
            #[cfg(feature = "tracing")]
            debug!(
                expr_kind = std::any::type_name_of_val(expr),
                "trying rumoca_eval_const for unhandled expression kind"
            );
            try_eval_with_rumoca_eval_const(ctx, expr, prefix)
        }
    };

    // If simple evaluation failed, try the full evaluator as fallback
    let result = result.or_else(|| {
        #[cfg(feature = "tracing")]
        debug!("simple evaluation failed, trying rumoca_eval_const fallback");
        try_eval_with_rumoca_eval_const(ctx, expr, prefix)
    });

    #[cfg(feature = "tracing")]
    if result.is_some() {
        debug!(result = ?result, "expression evaluated");
    }

    result
}

/// Evaluate a builtin function call to an integer value for for-loop ranges (MLS §3.7.2).
fn try_eval_builtin_function(
    ctx: &Context,
    comp: &ComponentReference,
    args: &[ast::Expression],
    prefix: &QualifiedName,
) -> Option<i64> {
    let func_name = comp
        .parts
        .last()
        .map(|p| p.ident.text.as_ref())
        .unwrap_or("");
    let eval = |e: &ast::Expression| try_eval_integer_with_ctx(ctx, e, prefix);
    match func_name {
        "size" => try_eval_size_call(ctx, args, prefix),
        "max" => try_eval_max_min(ctx, args, prefix, true),
        "min" => try_eval_max_min(ctx, args, prefix, false),
        "abs" if args.len() == 1 => eval(&args[0]).map(|v| v.abs()),
        "sign" if args.len() == 1 => eval(&args[0]).map(|v| v.signum()),
        "integer" if args.len() == 1 => eval(&args[0]),
        "div" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            rumoca_core::eval_integer_div_builtin(x, y)
        }
        "mod" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            if y != 0 {
                Some(((x % y) + y) % y)
            } else {
                None
            }
        }
        "rem" if args.len() == 2 => {
            let (x, y) = (eval(&args[0])?, eval(&args[1])?);
            if y != 0 { Some(x % y) } else { None }
        }
        _ => {
            #[cfg(feature = "tracing")]
            tracing::warn!(
                function = func_name,
                "unhandled function in for-equation range"
            );
            None
        }
    }
}

/// Check if an expression contains component references with range subscripts
/// that evaluate to empty ranges (e.g., `x[2:1]` where start > end).
///
/// MLS §10.5: Array slicing with empty ranges produces zero elements,
/// so equations referencing such slices should be skipped entirely.
pub(super) fn has_empty_range_subscript(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &QualifiedName,
) -> bool {
    match expr {
        ast::Expression::ComponentReference(cr) => cr_has_empty_range_subscript(ctx, cr, prefix),
        ast::Expression::Binary { lhs, rhs, .. } => {
            has_empty_range_subscript(ctx, lhs, prefix)
                || has_empty_range_subscript(ctx, rhs, prefix)
        }
        ast::Expression::Unary { rhs, .. } => has_empty_range_subscript(ctx, rhs, prefix),
        ast::Expression::FunctionCall { args, .. } => {
            // Don't recurse into function call arguments.  A function
            // can legitimately accept an empty‐range array and return
            // a well‐formed result (e.g. cat(1, {u}, {}) = {u},
            // previous({}) = {}).  Only direct component references
            // with empty ranges should trigger equation elimination.
            // However, we still check the first argument for common
            // patterns like der(x[2:1]) where the function is der.
            args.iter().any(|a| matches!(a, ast::Expression::ComponentReference(cr) if cr_has_empty_range_subscript(ctx, cr, prefix)))
        }
        ast::Expression::Parenthesized { inner, .. } => {
            has_empty_range_subscript(ctx, inner, prefix)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            // For if‐expressions, try to evaluate the condition.
            // Only check the branch that will actually be taken.
            // If the condition is non‐constant, conservatively return
            // false — the empty range may be in a dead branch (e.g.
            // Clocked.FractionalDelay where n=0 makes u_buffer[1:0]
            // appear only in the else branch that is never executed).
            for (cond, then_expr) in branches {
                match try_eval_boolean_with_ctx_inner(cond, Some(ctx), prefix) {
                    Some(true) => return has_empty_range_subscript(ctx, then_expr, prefix),
                    Some(false) => continue,
                    None => return false, // non‐constant → don't drop
                }
            }
            // All conditions were false → check else branch
            has_empty_range_subscript(ctx, else_branch, prefix)
        }
        _ => false,
    }
}

/// Check if a component reference has any range subscripts that evaluate to empty.
fn cr_has_empty_range_subscript(
    ctx: &Context,
    cr: &ComponentReference,
    prefix: &QualifiedName,
) -> bool {
    let has_range_subs: Vec<_> = cr
        .parts
        .iter()
        .filter_map(|p| p.subs.as_ref())
        .flatten()
        .filter(|sub| {
            matches!(
                sub,
                ast::Subscript::Expression(ast::Expression::Range { .. })
            )
        })
        .collect();
    if has_range_subs.is_empty() {
        return false;
    }
    cr.parts
        .iter()
        .filter_map(|p| p.subs.as_ref())
        .flatten()
        .any(|sub| is_empty_range_subscript(ctx, sub, prefix))
}

/// Check if a subscript is a range expression that evaluates to an empty range.
fn is_empty_range_subscript(
    ctx: &Context,
    sub: &rumoca_ir_ast::Subscript,
    prefix: &QualifiedName,
) -> bool {
    let (start, step, end) = match sub {
        ast::Subscript::Expression(ast::Expression::Range {
            start, step, end, ..
        }) => (start, step, end),
        _ => return false,
    };
    let Some(s) = try_eval_integer_with_ctx(ctx, start, prefix) else {
        return false;
    };
    let Some(e) = try_eval_integer_with_ctx(ctx, end, prefix) else {
        return false;
    };
    let step_val = step
        .as_ref()
        .and_then(|sv| try_eval_integer_with_ctx(ctx, sv, prefix))
        .unwrap_or(1);
    if step_val > 0 {
        s > e
    } else if step_val < 0 {
        s < e
    } else {
        false // step_val == 0 is invalid, treat as non-empty
    }
}

/// Try to look up array dimensions in the current scope or any parent scope.
///
/// Per MLS scope resolution rules, a reference like `Ptable` in a nested component
/// should first try `Model.Ptable`, then `Ptable` at root scope.
/// This mirrors `lookup_parameter_in_scope()` for integer parameters.
fn lookup_array_dimensions_in_scope(
    ctx: &Context,
    cr: &ComponentReference,
    prefix: &QualifiedName,
) -> Option<Vec<i64>> {
    // First try fully qualified
    let qualified = build_qualified_name(prefix, cr);
    if let Some(dims) = lookup_array_dims_with_unindexed_scope(ctx, &qualified) {
        #[cfg(feature = "tracing")]
        debug!(
            array = %qualified,
            dims = ?dims,
            "found array dimensions (fully qualified)"
        );
        return Some(dims);
    }

    // Try parent scopes
    let mut current_prefix = prefix.clone();
    while let Some(parent) = get_parent_prefix(&current_prefix) {
        let parent_qualified = build_qualified_name(&parent, cr);
        if let Some(dims) = lookup_array_dims_with_unindexed_scope(ctx, &parent_qualified) {
            #[cfg(feature = "tracing")]
            debug!(
                original = %qualified,
                found = %parent_qualified,
                dims = ?dims,
                "array dimensions resolved in parent scope"
            );
            return Some(dims);
        }
        current_prefix = parent;
    }

    // Try unqualified (root scope) - just the component reference itself
    let unqualified = cr
        .parts
        .iter()
        .map(format_component_ref_part)
        .collect::<Vec<_>>()
        .join(".");
    if let Some(dims) = lookup_array_dims_with_unindexed_scope(ctx, &unqualified) {
        #[cfg(feature = "tracing")]
        debug!(
            original = %qualified,
            found = %unqualified,
            dims = ?dims,
            "array dimensions resolved at root scope"
        );
        return Some(dims);
    }

    #[cfg(feature = "tracing")]
    debug!(
        array = %qualified,
        "array dimensions not found in any scope"
    );
    None
}

/// Try to evaluate a size() function call.
///
/// Supports:
/// - `size(A, i)` - returns the size of dimension i (1-indexed)
/// - `size(A)` with single dimension array - returns the size of dimension 1
///
/// Uses scope resolution per MLS to find array dimensions in current or parent scopes.
fn try_eval_size_call(
    ctx: &Context,
    args: &[ast::Expression],
    prefix: &QualifiedName,
) -> Option<i64> {
    if args.is_empty() || args.len() > 2 {
        #[cfg(feature = "tracing")]
        warn!(arg_count = args.len(), "size() requires 1 or 2 arguments");
        return None;
    }

    // First argument should be a component reference to an array
    let cr = match &args[0] {
        ast::Expression::ComponentReference(cr) => cr,
        _ => {
            #[cfg(feature = "tracing")]
            warn!("size() first argument must be a component reference");
            return None;
        }
    };

    // Build array name for tracing (always needed when tracing is enabled)
    #[cfg(feature = "tracing")]
    let array_name = build_qualified_name(prefix, cr);
    #[cfg(feature = "tracing")]
    debug!(array = %array_name, "looking up array dimensions for size()");

    // Get array dimensions with scope resolution
    let dims = match lookup_array_dimensions_in_scope(ctx, cr, prefix) {
        Some(d) => d,
        None => {
            return None;
        }
    };

    if args.len() == 1 {
        // size(A) for 1D array returns the size
        if dims.len() == 1 {
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, size = dims[0], "size(A) for 1D array");
            Some(dims[0])
        } else {
            #[cfg(feature = "tracing")]
            warn!(array = %array_name, ndims = dims.len(), "size(A) needs explicit dim for multi-dimensional");
            // For multi-dimensional arrays, size(A) without dim is not directly evaluable
            // to an integer - it returns a vector. Return None.
            None
        }
    } else {
        // size(A, dim) - second argument is the dimension (1-indexed)
        let dim = try_eval_integer_with_ctx(ctx, &args[1], prefix)?;
        if dim >= 1 && (dim as usize) <= dims.len() {
            let result = dims[(dim as usize) - 1];
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, dim = dim, result = result, "size(A, dim) evaluated");
            Some(result)
        } else {
            #[cfg(feature = "tracing")]
            warn!(array = %array_name, dim = dim, ndims = dims.len(), "dimension out of range");
            None
        }
    }
}

/// Try to evaluate max(a, b) or min(a, b) function call.
///
/// Supports:
/// - Binary form: `max(a, b)` / `min(a, b)` - returns max/min of two integers
/// - Array form: `max([a; b; c])` / `min([...])` - returns max/min of array elements
///
/// MLS §3.7.2: max/min are reduction functions that operate on scalars or arrays.
fn try_eval_max_min(
    ctx: &Context,
    args: &[ast::Expression],
    prefix: &QualifiedName,
    is_max: bool,
) -> Option<i64> {
    #[cfg(feature = "tracing")]
    let func_name = if is_max { "max" } else { "min" };

    if args.is_empty() {
        #[cfg(feature = "tracing")]
        warn!(func = func_name, "called with no arguments");
        return None;
    }

    if args.len() == 2 {
        // Binary form: max(a, b) or min(a, b)
        #[cfg(feature = "tracing")]
        debug!(func = func_name, "evaluating binary form");
        let a = try_eval_integer_with_ctx(ctx, &args[0], prefix)?;
        let b = try_eval_integer_with_ctx(ctx, &args[1], prefix)?;
        let result = if is_max { a.max(b) } else { a.min(b) };
        #[cfg(feature = "tracing")]
        debug!(
            func = func_name,
            a = a,
            b = b,
            result = result,
            "binary form evaluated"
        );
        Some(result)
    } else if args.len() == 1 {
        // Array form: max([a; b; c]) or min([...])
        // The single argument should be an array literal
        match &args[0] {
            ast::Expression::Array { elements, .. } => {
                #[cfg(feature = "tracing")]
                debug!(
                    func = func_name,
                    element_count = elements.len(),
                    "evaluating array form"
                );
                // Flatten nested arrays and evaluate all elements
                let values: Option<Vec<i64>> = elements
                    .iter()
                    .filter_map(|row| match row {
                        ast::Expression::Array {
                            elements: inner, ..
                        } => {
                            // Handle [a; b; c] format (row vector concatenation)
                            inner
                                .iter()
                                .map(|e| try_eval_integer_with_ctx(ctx, e, prefix))
                                .collect::<Option<Vec<_>>>()
                        }
                        e => try_eval_integer_with_ctx(ctx, e, prefix).map(|v| vec![v]),
                    })
                    .flatten()
                    .map(Some)
                    .collect();

                let values = values?;
                #[cfg(feature = "tracing")]
                debug!(func = func_name, values = ?values, "array elements evaluated");
                if values.is_empty() {
                    None
                } else if is_max {
                    values.into_iter().max()
                } else {
                    values.into_iter().min()
                }
            }
            // Single scalar argument - just return it
            _ => {
                #[cfg(feature = "tracing")]
                debug!(func = func_name, "single scalar argument");
                try_eval_integer_with_ctx(ctx, &args[0], prefix)
            }
        }
    } else {
        #[cfg(feature = "tracing")]
        warn!(
            func = func_name,
            arg_count = args.len(),
            "unexpected argument count"
        );
        None
    }
}

/// Substitute an index variable with a concrete value in an equation.
pub(crate) fn substitute_index_in_equation(
    eq: &ast::Equation,
    var_name: &str,
    value: i64,
) -> ast::Equation {
    let sub_eq = |e: &ast::Equation| substitute_index_in_equation(e, var_name, value);
    let sub_expr = |e: &ast::Expression| substitute_index_in_expression(e, var_name, value);
    match eq {
        ast::Equation::Simple { lhs, rhs } => ast::Equation::Simple {
            lhs: sub_expr(lhs),
            rhs: sub_expr(rhs),
        },
        ast::Equation::For { indices, equations } => ast::Equation::For {
            // Substitute in nested for-loop range expressions (MLS §8.3.3)
            indices: indices
                .iter()
                .map(|idx| ForIndex {
                    ident: idx.ident.clone(),
                    range: sub_expr(&idx.range),
                })
                .collect(),
            equations: equations.iter().map(&sub_eq).collect(),
        },
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => ast::Equation::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| EquationBlock {
                    cond: sub_expr(&block.cond),
                    eqs: block.eqs.iter().map(&sub_eq).collect(),
                })
                .collect(),
            else_block: else_block
                .as_ref()
                .map(|eqs| eqs.iter().map(&sub_eq).collect()),
        },
        ast::Equation::When(blocks) => ast::Equation::When(
            blocks
                .iter()
                .map(|block| EquationBlock {
                    cond: sub_expr(&block.cond),
                    eqs: block.eqs.iter().map(&sub_eq).collect(),
                })
                .collect(),
        ),
        ast::Equation::Connect { lhs, rhs } => ast::Equation::Connect {
            lhs: substitute_index_in_component_ref(lhs, var_name, value),
            rhs: substitute_index_in_component_ref(rhs, var_name, value),
        },
        ast::Equation::FunctionCall { comp, args } => ast::Equation::FunctionCall {
            comp: substitute_index_in_component_ref(comp, var_name, value),
            args: args.iter().map(&sub_expr).collect(),
        },
        ast::Equation::Assert {
            condition,
            message,
            level,
        } => ast::Equation::Assert {
            condition: sub_expr(condition),
            message: sub_expr(message),
            level: level.as_ref().map(sub_expr),
        },
        ast::Equation::Empty => ast::Equation::Empty,
    }
}

/// Substitute an index variable with a concrete value in a component reference.
fn substitute_index_in_component_ref(
    cr: &ComponentReference,
    var_name: &str,
    value: i64,
) -> ComponentReference {
    ComponentReference {
        local: cr.local,
        parts: cr
            .parts
            .iter()
            .map(|part| ComponentRefPart {
                ident: part.ident.clone(),
                subs: part.subs.as_ref().map(|subs| {
                    subs.iter()
                        .map(|sub| substitute_index_in_subscript(sub, var_name, value))
                        .collect()
                }),
            })
            .collect(),
        def_id: cr.def_id,
        span: cr.span,
    }
}

/// Substitute an index variable with a concrete value in an expression.
pub(crate) fn substitute_index_in_expression(
    expr: &ast::Expression,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    match expr {
        ast::Expression::ComponentReference(cr) => {
            substitute_index_component_reference_expression(cr, var_name, value)
        }

        ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(substitute_index_in_expression(lhs, var_name, value)),
            rhs: Arc::new(substitute_index_in_expression(rhs, var_name, value)),
            span: *span,
        },

        ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(substitute_index_in_expression(rhs, var_name, value)),
            span: *span,
        },

        ast::Expression::FunctionCall { comp, args, span } => {
            substitute_index_in_function_call_expression(comp, args, *span, var_name, value)
        }

        ast::Expression::ClassModification {
            target,
            modifications,
            span,
            ..
        } => substitute_index_in_class_modification_expression(
            target,
            modifications,
            *span,
            var_name,
            value,
        ),

        ast::Expression::NamedArgument {
            name,
            value: arg,
            span,
        } => substitute_index_in_named_argument_expression(name, arg, *span, var_name, value),

        ast::Expression::Modification {
            target,
            value: modification,
            span,
        } => substitute_index_in_modification_expression(
            target,
            modification,
            *span,
            var_name,
            value,
        ),

        ast::Expression::Array {
            elements,
            is_matrix,
            span,
        } => substitute_index_in_array_expression(elements, *is_matrix, *span, var_name, value),

        ast::Expression::If {
            branches,
            else_branch,
            span,
        } => substitute_index_in_if_expression(branches, else_branch, *span, var_name, value),

        ast::Expression::Parenthesized { inner, span } => {
            substitute_index_in_parenthesized_expression(inner, *span, var_name, value)
        }

        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            span,
        } => substitute_index_in_array_comprehension(
            expr,
            indices,
            filter.as_deref(),
            *span,
            var_name,
            value,
        ),

        ast::Expression::ArrayIndex {
            base,
            subscripts,
            span,
        } => substitute_index_in_array_index_expression(base, subscripts, *span, var_name, value),

        ast::Expression::FieldAccess { base, field, span } => {
            substitute_index_in_field_access_expression(base, field, *span, var_name, value)
        }

        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => substitute_index_in_range_expression(
            start,
            step.as_deref(),
            end,
            *span,
            var_name,
            value,
        ),

        ast::Expression::Tuple { elements, span } => {
            substitute_index_in_tuple_expression(elements, *span, var_name, value)
        }

        // Terminal and empty expressions don't need substitution.
        _ => expr.clone(),
    }
}

fn substitute_index_in_function_call_expression(
    comp: &ComponentReference,
    args: &[ast::Expression],
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: substitute_index_in_component_ref(comp, var_name, value),
        args: substitute_index_in_expression_list(args, var_name, value),
        span,
    }
}

fn substitute_index_in_class_modification_expression(
    target: &ComponentReference,
    modifications: &[ast::Expression],
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::ClassModification {
        target: substitute_index_in_component_ref(target, var_name, value),
        modifications: substitute_index_in_expression_list(modifications, var_name, value),
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
        span,
    }
}

fn substitute_index_in_named_argument_expression(
    name: &rumoca_core::Token,
    arg: &ast::Expression,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::NamedArgument {
        name: name.clone(),
        value: Arc::new(substitute_index_in_expression(arg, var_name, value)),
        span,
    }
}

fn substitute_index_in_modification_expression(
    target: &ComponentReference,
    modification: &ast::Expression,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::Modification {
        target: substitute_index_in_component_ref(target, var_name, value),
        value: Arc::new(substitute_index_in_expression(
            modification,
            var_name,
            value,
        )),
        span,
    }
}

fn substitute_index_in_tuple_expression(
    elements: &[ast::Expression],
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::Tuple {
        elements: substitute_index_in_expression_list(elements, var_name, value),
        span,
    }
}

fn substitute_index_in_parenthesized_expression(
    inner: &ast::Expression,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::Parenthesized {
        inner: Arc::new(substitute_index_in_expression(inner, var_name, value)),
        span,
    }
}

fn substitute_index_in_array_expression(
    elements: &[ast::Expression],
    is_matrix: bool,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::Array {
        elements: substitute_index_in_expression_list(elements, var_name, value),
        is_matrix,
        span,
    }
}

fn substitute_index_in_array_index_expression(
    base: &ast::Expression,
    subscripts: &[ast::Subscript],
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::ArrayIndex {
        base: Arc::new(substitute_index_in_expression(base, var_name, value)),
        subscripts: subscripts
            .iter()
            .map(|sub| substitute_index_in_subscript(sub, var_name, value))
            .collect(),
        span,
    }
}

fn substitute_index_in_field_access_expression(
    base: &ast::Expression,
    field: &str,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::FieldAccess {
        base: Arc::new(substitute_index_in_expression(base, var_name, value)),
        field: field.to_string(),
        span,
    }
}

fn substitute_index_component_reference_expression(
    cr: &ast::ComponentReference,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    if cr.parts.len() == 1 && &*cr.parts[0].ident.text == var_name && cr.parts[0].subs.is_none() {
        return ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: std::sync::Arc::from(value.to_string()),
                ..Default::default()
            },
            span: cr.span,
        };
    }
    ast::Expression::ComponentReference(substitute_index_in_component_ref(cr, var_name, value))
}

fn substitute_index_in_expression_list(
    expressions: &[ast::Expression],
    var_name: &str,
    value: i64,
) -> Vec<ast::Expression> {
    expressions
        .iter()
        .map(|expr| substitute_index_in_expression(expr, var_name, value))
        .collect()
}

fn substitute_index_in_if_expression(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::If {
        branches: branches
            .iter()
            .map(|(cond, then_expr)| {
                (
                    substitute_index_in_expression(cond, var_name, value),
                    substitute_index_in_expression(then_expr, var_name, value),
                )
            })
            .collect(),
        else_branch: Arc::new(substitute_index_in_expression(else_branch, var_name, value)),
        span,
    }
}

fn substitute_index_in_array_comprehension(
    expr: &Arc<ast::Expression>,
    indices: &[ForIndex],
    filter: Option<&ast::Expression>,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    let mut shadowed = false;
    let indices = indices
        .iter()
        .map(|idx| substitute_index_in_for_index(idx, &mut shadowed, var_name, value))
        .collect();
    ast::Expression::ArrayComprehension {
        expr: substitute_index_in_comprehension_body(expr, shadowed, var_name, value),
        indices,
        filter: substitute_index_in_comprehension_filter(filter, shadowed, var_name, value),
        span,
    }
}

fn substitute_index_in_for_index(
    idx: &ForIndex,
    shadowed: &mut bool,
    var_name: &str,
    value: i64,
) -> ForIndex {
    let range = if *shadowed {
        idx.range.clone()
    } else {
        substitute_index_in_expression(&idx.range, var_name, value)
    };
    if idx.ident.text.as_ref() == var_name {
        *shadowed = true;
    }
    ForIndex {
        ident: idx.ident.clone(),
        range,
    }
}

fn substitute_index_in_comprehension_body(
    expr: &Arc<ast::Expression>,
    shadowed: bool,
    var_name: &str,
    value: i64,
) -> Arc<ast::Expression> {
    if shadowed {
        expr.clone()
    } else {
        Arc::new(substitute_index_in_expression(expr, var_name, value))
    }
}

fn substitute_index_in_comprehension_filter(
    filter: Option<&ast::Expression>,
    shadowed: bool,
    var_name: &str,
    value: i64,
) -> Option<Arc<ast::Expression>> {
    if shadowed {
        filter.cloned().map(Arc::new)
    } else {
        filter.map(|f| Arc::new(substitute_index_in_expression(f, var_name, value)))
    }
}

fn substitute_index_in_range_expression(
    start: &ast::Expression,
    step: Option<&ast::Expression>,
    end: &ast::Expression,
    span: rumoca_core::Span,
    var_name: &str,
    value: i64,
) -> ast::Expression {
    ast::Expression::Range {
        start: Arc::new(substitute_index_in_expression(start, var_name, value)),
        step: step.map(|s| Arc::new(substitute_index_in_expression(s, var_name, value))),
        end: Arc::new(substitute_index_in_expression(end, var_name, value)),
        span,
    }
}

/// Substitute index variable in a subscript.
fn substitute_index_in_subscript(
    sub: &rumoca_ir_ast::Subscript,
    var_name: &str,
    value: i64,
) -> rumoca_ir_ast::Subscript {
    match sub {
        ast::Subscript::Expression(expr) => {
            ast::Subscript::Expression(substitute_index_in_expression(expr, var_name, value))
        }
        rumoca_ir_ast::Subscript::Range { token } => rumoca_ir_ast::Subscript::Range {
            token: token.clone(),
        },
        rumoca_ir_ast::Subscript::Empty => rumoca_ir_ast::Subscript::Empty,
    }
}

/// Build a rumoca_eval_flat::constant::EvalContext from the Context.
///
/// This allows using the rumoca_eval_const crate for more complex expression evaluation
/// while still leveraging the parameter values collected during flattening.
///
/// If a ClassTree is provided, functions will be looked up on-demand during evaluation.
pub(crate) fn build_eval_context(ctx: &Context, tree: Option<&ClassTree>) -> EvalContext {
    let parameter_capacity = ctx.parameter_values.len()
        + ctx.boolean_parameter_values.len()
        + ctx.enum_parameter_values.len()
        + ctx.array_dimensions.len();
    let mut eval_ctx = EvalContext::with_capacity(parameter_capacity, 0, ctx.functions.len() * 2);

    // Add integer parameters
    for (name, value) in &ctx.parameter_values {
        eval_ctx.add_parameter(name.clone(), Value::Integer(*value));
    }

    // Add boolean parameters
    for (name, value) in &ctx.boolean_parameter_values {
        eval_ctx.add_parameter(name.clone(), Value::Bool(*value));
    }

    // Add enum parameters
    for (name, value) in &ctx.enum_parameter_values {
        // The value is a qualified enum literal like "Type.Literal"
        // Parse it into type and literal parts
        if let Some((type_name, literal)) = crate::path_utils::scope_split(value) {
            eval_ctx.add_parameter(
                name.clone(),
                Value::Enum(type_name.to_string(), literal.to_string()),
            );
        }
    }

    // Add array dimensions as array values (for size() evaluation)
    // This is a simplified representation - we create arrays of the right size
    // but with placeholder values, since we mainly need the dimensions
    for (name, dims) in &ctx.array_dimensions {
        if dims.len() == 1 {
            // 1D array
            let arr: Vec<Value> = (0..dims[0]).map(|_| Value::Integer(0)).collect();
            eval_ctx.add_parameter(name.clone(), Value::Array(arr));
        } else if dims.len() == 2 {
            // 2D array
            let arr: Vec<Value> = (0..dims[0])
                .map(|_| Value::Array((0..dims[1]).map(|_| Value::Integer(0)).collect()))
                .collect();
            eval_ctx.add_parameter(name.clone(), Value::Array(arr));
        }
        // For higher dimensions, we could extend this pattern
    }

    // Add functions from flatten context (pre-collected)
    for func in ctx.functions.values() {
        eval_ctx.add_function(func.clone());
    }

    // If ClassTree is available, look up additional functions that might be called
    // during evaluation. This enables lazy function lookup for user-defined functions.
    // The functions are cached in the eval context to avoid repeated lookups.
    if let Some(_tree) = tree {
        // Functions are looked up on-demand in the evaluator.
        // We could pre-scan expressions here, but lazy lookup is simpler.
    }

    eval_ctx
}

/// Evaluate an AST expression using the rumoca_eval_const crate.
///
/// This is a fallback for complex expressions that the simpler ad-hoc evaluation
/// in `try_eval_integer_with_ctx` cannot handle.
fn try_eval_with_rumoca_eval_const(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &QualifiedName,
) -> Option<i64> {
    let fallback_start = crate::maybe_start_timer();
    // Convert AST expression to qualified ast::Expression
    let Ok(flat_expr) =
        qualify_expression_imports_with_def_map_ctx(expr, prefix, &ctx.current_imports, None, ctx)
    else {
        return None;
    };

    // Reuse the per-flatten base evaluation context to avoid rebuilding
    // parameter/function maps on every complex-expression fallback.
    let eval_ctx = ctx.eval_fallback_context();

    // Try to evaluate
    let result = rumoca_eval_flat::constant::try_eval_integer(&flat_expr, eval_ctx);
    crate::maybe_record_eval_fallback_timing(fallback_start);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn make_comp_ref(path: &str) -> ComponentReference {
        ComponentReference {
            local: false,
            parts: crate::path_utils::segments(path)
                .into_iter()
                .map(|name| ComponentRefPart {
                    ident: Token {
                        text: Arc::from(name.to_string()),
                        ..Token::default()
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_comp_ref_with_first_index(path: &str, index: i64) -> ComponentReference {
        let mut cr = make_comp_ref(path);
        let sub = ast::Subscript::Expression(ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: Arc::from(index.to_string()),
                ..Token::default()
            },
            span: rumoca_core::Span::DUMMY,
        });
        if let Some(first) = cr.parts.first_mut() {
            first.subs = Some(vec![sub]);
        }
        cr
    }

    fn make_int(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: Arc::from(value.to_string()),
                ..Token::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: make_comp_ref(name),
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_for_index(name: &str, start: i64, end: i64) -> ForIndex {
        ForIndex {
            ident: Token {
                text: Arc::from(name.to_string()),
                ..Token::default()
            },
            range: ast::Expression::Range {
                start: Arc::new(make_int(start)),
                step: None,
                end: Arc::new(make_int(end)),
                span: rumoca_core::Span::DUMMY,
            },
        }
    }

    #[test]
    fn test_try_eval_integer_with_ctx_div_operator_requires_exact_quotient() {
        let ctx = Context::new();
        let expr = ast::Expression::Binary {
            op: OpBinary::Div,
            lhs: Arc::new(make_int(7)),
            rhs: Arc::new(make_int(2)),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(
            try_eval_integer_with_ctx(&ctx, &expr, &QualifiedName::new()),
            None
        );
    }

    #[test]
    fn test_try_eval_integer_with_ctx_div_builtin_remains_truncating() {
        let ctx = Context::new();
        let expr = make_call("div", vec![make_int(7), make_int(2)]);

        assert_eq!(
            try_eval_integer_with_ctx(&ctx, &expr, &QualifiedName::new()),
            Some(3)
        );
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_dot_product_is_scalar() {
        let mut ctx = Context::new();
        ctx.array_dimensions.insert("a".to_string(), vec![3]);
        ctx.array_dimensions.insert("b".to_string(), vec![3]);

        let lhs = ast::Expression::Binary {
            op: OpBinary::Mul,
            lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("a"))),
            rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("b"))),
            span: rumoca_core::Span::DUMMY,
        };
        let rhs = make_int(0);

        let scalar_count =
            infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
        assert_eq!(scalar_count, 1);
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_matrix_vector_result() {
        let mut ctx = Context::new();
        ctx.array_dimensions.insert("y".to_string(), vec![2]);
        ctx.array_dimensions.insert("A".to_string(), vec![2, 2]);
        ctx.array_dimensions.insert("x".to_string(), vec![2]);

        let lhs = ast::Expression::ComponentReference(make_comp_ref("y"));
        let rhs = ast::Expression::Binary {
            op: OpBinary::Mul,
            lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("A"))),
            rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("x"))),
            span: rumoca_core::Span::DUMMY,
        };

        let scalar_count =
            infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
        assert_eq!(scalar_count, 2);
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_fallback_uses_lhs_dims() {
        let mut ctx = Context::new();
        ctx.array_dimensions.insert("y".to_string(), vec![3]);

        let lhs = ast::Expression::ComponentReference(make_comp_ref("y"));
        let rhs = make_call("zeros", vec![make_int(3)]);

        let scalar_count =
            infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
        assert_eq!(scalar_count, 3);
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_zero_sized_array() {
        let mut ctx = Context::new();
        ctx.array_dimensions.insert("y".to_string(), vec![0]);

        let lhs = ast::Expression::ComponentReference(make_comp_ref("y"));
        let rhs = make_int(0);

        let scalar_count =
            infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
        assert_eq!(scalar_count, 0);
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_indexed_parent_keeps_field_dims() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("medium_T[2].state.X".to_string(), vec![2]);

        let lhs = ast::Expression::ComponentReference(make_comp_ref_with_first_index(
            "medium_T.state.X",
            2,
        ));
        let rhs = make_call("zeros", vec![make_int(2)]);

        let scalar_count =
            infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
        assert_eq!(scalar_count, 2);
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_indexed_ref_uses_unscripted_dims() {
        let mut ctx = Context::new();
        ctx.array_dimensions.insert("A".to_string(), vec![3]);

        let lhs = ast::Expression::ComponentReference(make_comp_ref_with_first_index("A", 1));
        let rhs = make_int(0);

        let scalar_count =
            infer_simple_equation_scalar_count(&lhs, &rhs, &QualifiedName::new(), &ctx);
        assert_eq!(scalar_count, 1);
    }

    #[test]
    fn test_infer_simple_equation_scalar_count_prefix_with_dot_inside_subscript_uses_field_dims() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("bus[data.medium]".to_string(), vec![5]);
        ctx.array_dimensions
            .insert("bus[data.medium].x".to_string(), vec![2]);

        let lhs = ast::Expression::ComponentReference(make_comp_ref("x"));
        let rhs = make_call("zeros", vec![make_int(2)]);
        let prefix = QualifiedName {
            parts: vec![("bus[data.medium]".to_string(), Vec::new())],
        };

        let scalar_count = infer_simple_equation_scalar_count(&lhs, &rhs, &prefix, &ctx);
        assert_eq!(scalar_count, 2);
    }

    #[test]
    fn test_infer_size_constant_from_dims_ns_uses_substance_names() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("m.medium.substanceNames".to_string(), vec![4]);
        ctx.array_dimensions
            .insert("m.medium.X".to_string(), vec![7]);

        let prefix = QualifiedName::from_dotted("m.medium");
        assert_eq!(infer_size_constant_from_dims(&ctx, "nS", &prefix), Some(4));
    }

    #[test]
    fn test_infer_size_constant_from_dims_ns_does_not_fallback_to_x() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("m.medium.X".to_string(), vec![7]);

        let prefix = QualifiedName::from_dotted("m.medium");
        assert_eq!(infer_size_constant_from_dims(&ctx, "nS", &prefix), None);
    }

    #[test]
    fn test_lookup_parameter_in_scope_uses_unindexed_array_element_scope() {
        let mut ctx = Context::new();
        ctx.parameter_values
            .insert("adaptor.filter.transferFunction[1].nx".to_string(), 1_i64);

        let prefix = QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
        let cref = make_comp_ref("nx");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(1_i64));
    }

    #[test]
    fn test_lookup_parameter_in_scope_prefers_indexed_scope_override() {
        let mut ctx = Context::new();
        ctx.parameter_values
            .insert("adaptor.filter.transferFunction.nx".to_string(), 1_i64);
        ctx.parameter_values.insert(
            "adaptor.filter[1].transferFunction[1].nx".to_string(),
            0_i64,
        );

        let prefix = QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
        let cref = make_comp_ref("nx");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(0_i64));
    }

    #[test]
    fn test_lookup_parameter_in_scope_infers_filter_nr_from_unindexed_array_dims() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("filter.r".to_string(), vec![2_i64]);

        let prefix = QualifiedName::from_dotted("filter[2]");
        let cref = make_comp_ref("nr");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(2_i64));
    }

    #[test]
    fn test_lookup_parameter_in_scope_infers_table_nout_from_columns_dimension() {
        let mut ctx = Context::new();
        ctx.array_dimensions.insert(
            "stack.cell[1,2].cell.ocv_soc.columns".to_string(),
            vec![1_i64],
        );

        let prefix = QualifiedName::from_dotted("stack.cell[1,2].cell.ocv_soc");
        let cref = make_comp_ref("nout");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(1_i64));
    }

    #[test]
    fn test_size_call_uses_unindexed_array_element_scope() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("adaptor.filter.transferFunction[1].a".to_string(), vec![2]);

        let prefix = QualifiedName::from_dotted("adaptor.filter[1].transferFunction[1]");
        let expr = make_call(
            "size",
            vec![
                ast::Expression::ComponentReference(make_comp_ref("a")),
                make_int(1),
            ],
        );

        assert_eq!(try_eval_integer_with_ctx(&ctx, &expr, &prefix), Some(2_i64));
    }

    #[test]
    fn test_expand_range_indices_supports_linspace_parameter_range() {
        let mut ctx = Context::new();
        ctx.parameter_values.insert("plant.m".to_string(), 3_i64);

        let prefix = QualifiedName::from_dotted("plant");
        let range = make_call(
            "linspace",
            vec![
                make_int(1),
                ast::Expression::ComponentReference(make_comp_ref("m")),
                ast::Expression::ComponentReference(make_comp_ref("m")),
            ],
        );

        assert_eq!(
            expand_range_indices(&ctx, &range, &prefix, rumoca_core::Span::DUMMY).unwrap(),
            vec![1, 2, 3]
        );
    }

    #[test]
    fn test_lookup_parameter_in_scope_infers_medium_nxi_from_array_dims() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("m.dynBal.mXi".to_string(), vec![1]);

        let prefix = QualifiedName::from_dotted("m.dynBal");
        let cref = make_comp_ref("Medium.nXi");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(1_i64));
    }

    #[test]
    fn test_lookup_parameter_in_scope_does_not_drop_type_alias_segment() {
        let mut ctx = Context::new();
        ctx.parameter_values.insert("m.nXi".to_string(), 9_i64);
        ctx.parameter_values
            .insert("m.medium.nXi".to_string(), 2_i64);

        let prefix = QualifiedName::from_dotted("m");
        let cref = make_comp_ref("Medium.nXi");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), Some(2_i64));

        ctx.parameter_values.remove("m.medium.nXi");
        assert_eq!(lookup_parameter_in_scope(&ctx, &cref, &prefix), None);
    }

    #[test]
    fn test_flatten_for_equation_records_iteration_grouping() {
        let ctx = Context::new();
        let inst_eq = InstanceEquation {
            equation: ast::Equation::For {
                indices: vec![make_for_index("i", 1, 3)],
                equations: vec![ast::Equation::Simple {
                    lhs: ast::Expression::ComponentReference(make_comp_ref("y")),
                    rhs: ast::Expression::ComponentReference(make_comp_ref("i")),
                }],
            },
            origin: QualifiedName::from_dotted("M"),
            source_scope: None,
            source_scope_id: None,
            span: rumoca_core::Span::DUMMY,
        };

        let flattened =
            flatten_equation_with_def_map(&ctx, &inst_eq, &QualifiedName::new(), None).unwrap();
        assert_eq!(flattened.equations.len(), 3);
        assert_eq!(flattened.for_equations.len(), 1);
        let for_eq = &flattened.for_equations[0];
        assert_eq!(for_eq.index_names, vec!["i".to_string()]);
        assert_eq!(for_eq.first_equation_index, 0);
        let counts: Vec<usize> = for_eq
            .iterations
            .iter()
            .map(|it| it.equation_count)
            .collect();
        assert_eq!(counts, vec![1, 1, 1]);
        let values: Vec<Vec<i64>> = for_eq
            .iterations
            .iter()
            .map(|it| it.index_values.clone())
            .collect();
        assert_eq!(values, vec![vec![1], vec![2], vec![3]]);
    }

    #[test]
    fn test_flatten_nested_for_equation_records_cartesian_iterations() {
        let ctx = Context::new();
        let inst_eq = InstanceEquation {
            equation: ast::Equation::For {
                indices: vec![make_for_index("i", 1, 2), make_for_index("j", 1, 2)],
                equations: vec![ast::Equation::Simple {
                    lhs: ast::Expression::ComponentReference(make_comp_ref("y")),
                    rhs: ast::Expression::Binary {
                        op: OpBinary::Add,
                        lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("i"))),
                        rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
                        span: rumoca_core::Span::DUMMY,
                    },
                }],
            },
            origin: QualifiedName::from_dotted("M"),
            source_scope: None,
            source_scope_id: None,
            span: rumoca_core::Span::DUMMY,
        };

        let flattened =
            flatten_equation_with_def_map(&ctx, &inst_eq, &QualifiedName::new(), None).unwrap();
        assert_eq!(flattened.equations.len(), 4);
        assert_eq!(flattened.for_equations.len(), 1);
        let for_eq = &flattened.for_equations[0];
        assert_eq!(for_eq.index_names, vec!["i".to_string(), "j".to_string()]);
        let values: Vec<Vec<i64>> = for_eq
            .iterations
            .iter()
            .map(|it| it.index_values.clone())
            .collect();
        assert_eq!(values, vec![vec![1, 1], vec![1, 2], vec![2, 1], vec![2, 2]]);
    }

    #[test]
    fn test_substitute_index_descends_into_array_comprehension_body() {
        let expr = ast::Expression::ArrayComprehension {
            expr: Arc::new(ast::Expression::Binary {
                op: OpBinary::Add,
                lhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
                rhs: Arc::new(ast::Expression::ComponentReference(make_comp_ref("k"))),
                span: rumoca_core::Span::DUMMY,
            }),
            indices: vec![make_for_index("k", 1, 3)],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        };

        let substituted = substitute_index_in_expression(&expr, "j", 2);

        let ast::Expression::ArrayComprehension { expr, indices, .. } = substituted else {
            panic!("expected array comprehension");
        };
        assert_eq!(indices[0].ident.text.as_ref(), "k");
        let ast::Expression::Binary { lhs, rhs, .. } = expr.as_ref() else {
            panic!("expected binary comprehension body");
        };
        assert_eq!(lhs.as_ref(), &make_int(2));
        assert!(matches!(
            rhs.as_ref(),
            ast::Expression::ComponentReference(cr) if cr.parts[0].ident.text.as_ref() == "k"
        ));
    }

    #[test]
    fn test_substitute_index_respects_array_comprehension_shadowing() {
        let expr = ast::Expression::ArrayComprehension {
            expr: Arc::new(ast::Expression::ComponentReference(make_comp_ref("j"))),
            indices: vec![make_for_index("j", 1, 3)],
            filter: Some(Arc::new(ast::Expression::ComponentReference(
                make_comp_ref("j"),
            ))),
            span: rumoca_core::Span::DUMMY,
        };

        let substituted = substitute_index_in_expression(&expr, "j", 2);

        let ast::Expression::ArrayComprehension { expr, filter, .. } = substituted else {
            panic!("expected array comprehension");
        };
        assert!(matches!(
            expr.as_ref(),
            ast::Expression::ComponentReference(cr) if cr.parts[0].ident.text.as_ref() == "j"
        ));
        let Some(filter) = filter else {
            panic!("expected filter");
        };
        assert!(matches!(
            filter.as_ref(),
            ast::Expression::ComponentReference(cr) if cr.parts[0].ident.text.as_ref() == "j"
        ));
    }

    #[test]
    fn test_eval_fallback_timing_stats_record_calls() {
        // Snapshot baseline before our calls (other parallel tests may
        // increment the global atomic counter concurrently).
        let baseline = crate::flatten_phase_timing_stats().eval_fallback.calls;

        let ctx = Context::new();
        assert!(!ctx.has_cached_eval_fallback_context());

        let prefix = QualifiedName::new();
        assert_eq!(
            try_eval_with_rumoca_eval_const(&ctx, &make_int(7), &prefix),
            Some(7)
        );
        assert!(ctx.has_cached_eval_fallback_context());
        let first_ctx_ptr = ctx.eval_fallback_context() as *const _;

        assert_eq!(
            try_eval_with_rumoca_eval_const(&ctx, &make_int(11), &prefix),
            Some(11)
        );
        let second_ctx_ptr = ctx.eval_fallback_context() as *const _;
        assert_eq!(first_ctx_ptr, second_ctx_ptr);

        let stats = crate::flatten_phase_timing_stats();
        assert!(
            stats.eval_fallback.calls >= baseline + 2,
            "expected at least 2 new eval_fallback calls, got {} (baseline was {})",
            stats.eval_fallback.calls,
            baseline,
        );
    }
}
