//! When-equation flattening for the flatten phase.
//!
//! This module handles flattening of when-equations per MLS §8.3.5.
//! When-equations are used for discrete event handling and can contain:
//! - Simple assignments
//! - reinit() statements
//! - assert() and terminate() statements
//! - if-equations (conditional assignments)
//! - for-equations (expanded inline)
//!
//! Nested when-equations are NOT allowed (EQN-005).

use rumoca_ir_ast as ast;

use rumoca_ir_flat as flat;

use crate::equations::{build_qualified_name, expand_range_indices, substitute_index_in_equation};
use crate::errors::FlattenError;
use crate::{Context, qualify_expression_imports_with_def_map};

/// Flatten a when-equation to a list of WhenClauses.
///
/// Each flat::WhenClause represents one "when" or "elsewhen" branch with its condition
/// and the discrete equations that should be executed when the condition becomes true.
pub(crate) fn flatten_when_equation(
    ctx: &Context,
    inst_eq: &ast::InstanceEquation,
    prefix: &ast::QualifiedName,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::WhenClause>, FlattenError> {
    let span = inst_eq.span;

    match &inst_eq.equation {
        ast::Equation::When(blocks) => flatten_when_blocks(ctx, blocks, prefix, span, def_map),
        _ => {
            // Not a when-equation, return empty
            Ok(vec![])
        }
    }
}

/// Flatten one complete when-equation branch list (`when` + `elsewhen` blocks).
///
/// MLS §8.3.5 (EQN-013): Different branches of a when/elsewhen equation must
/// assign the same set of left-hand-side component references, unless all
/// switching conditions are parameter (structural) expressions.
pub(crate) fn flatten_when_blocks(
    ctx: &Context,
    blocks: &[ast::EquationBlock],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::WhenClause>, FlattenError> {
    let mut clauses = Vec::with_capacity(blocks.len());
    for block in blocks {
        let clause = flatten_when_block(ctx, block, prefix, span, def_map)?;
        clauses.push(clause);
    }

    validate_when_branch_targets(ctx, blocks, &clauses, prefix, span)?;
    Ok(clauses)
}

/// Flatten a single when/elsewhen block to a flat::WhenClause.
pub(crate) fn flatten_when_block(
    ctx: &Context,
    block: &ast::EquationBlock,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<flat::WhenClause, FlattenError> {
    // Qualify the condition expression
    let condition =
        qualify_expression_imports_with_def_map(&block.cond, prefix, &ctx.current_imports, def_map);

    let mut clause = flat::WhenClause::new(condition, span);

    // Flatten each equation in the block
    for eq in &block.eqs {
        let when_eqs = flatten_when_body_equation(ctx, eq, prefix, span, def_map)?;
        for weq in when_eqs {
            clause.add_equation(weq);
        }
    }

    Ok(clause)
}

/// Flatten an equation inside a when-clause body.
///
/// MLS §8.3.5 restricts what can appear in when-equations:
/// - Simple assignments: `v = expr`
/// - reinit() statements (handled specially)
/// - assert() statements
/// - terminate() statements
/// - if-equations (conditional assignments)
/// - for-equations (expanded inline)
///
/// Nested when-equations are NOT allowed (EQN-005).
fn flatten_when_body_equation(
    ctx: &Context,
    eq: &ast::Equation,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::WhenEquation>, FlattenError> {
    match eq {
        ast::Equation::Simple { lhs, rhs } => {
            flatten_when_simple_equation(lhs, rhs, prefix, span, &ctx.current_imports, def_map)
                .map(|opt| opt.into_iter().collect())
        }

        ast::Equation::FunctionCall { comp, args } => {
            flatten_when_function_call(comp, args, prefix, span, &ctx.current_imports, def_map)
                .map(|opt| opt.into_iter().collect())
        }

        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            // If-equations inside when-clauses are allowed per MLS §8.3.5
            flatten_when_if_equation(ctx, cond_blocks, else_block, prefix, span, def_map)
                .map(|opt| opt.into_iter().collect())
        }

        ast::Equation::When(_) => {
            // MLS §8.3.5: Nested when-equations are not allowed (EQN-005)
            Err(FlattenError::unsupported_equation(
                "nested when-equations are not allowed (MLS §8.3.5)",
                span,
            ))
        }

        ast::Equation::For { indices, equations } => {
            // For-equations inside when-equations: expand to multiple assignments
            // This is valid per MLS §8.3.5 when the for-equation contains allowed content
            flatten_when_for_equation(ctx, indices, equations, prefix, span, def_map)
        }

        ast::Equation::Empty => Ok(vec![]),

        _ => {
            // Other equation types (connect) are not allowed in when-equations
            Err(FlattenError::unsupported_equation(
                "only simple assignments, reinit(), if-equations, and for-equations are allowed in when-equations",
                span,
            ))
        }
    }
}

/// Flatten an if-equation inside a when-clause.
///
/// MLS §8.3.5 allows if-equations inside when-clauses for conditional
/// discrete variable updates. Each branch contains equations that are
/// only executed when the branch condition is true.
///
/// Per MLS §8.3.5: The branches of an if-equation inside when-equations must
/// have the same set of component references on the left-hand side, unless all
/// switching conditions are parameter expressions.
fn flatten_when_if_equation(
    ctx: &Context,
    cond_blocks: &[ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    let mut branches = Vec::new();

    // Process each if/elseif branch
    for block in cond_blocks {
        let condition = qualify_expression_imports_with_def_map(
            &block.cond,
            prefix,
            &ctx.current_imports,
            def_map,
        );
        let mut branch_eqs = Vec::new();

        for eq in &block.eqs {
            let when_eqs = flatten_when_body_equation(ctx, eq, prefix, span, def_map)?;
            branch_eqs.extend(when_eqs);
        }

        branches.push((condition, branch_eqs));
    }

    // Process else branch
    let else_eqs = if let Some(else_equations) = else_block {
        let mut eqs = Vec::new();
        for eq in else_equations {
            let when_eqs = flatten_when_body_equation(ctx, eq, prefix, span, def_map)?;
            eqs.extend(when_eqs);
        }
        eqs
    } else {
        Vec::new()
    };

    // MLS §8.3.5 validation: all branches must assign to the same set of variables,
    // unless all switching conditions are parameter expressions.
    let all_conditions_structural = cond_blocks
        .iter()
        .all(|block| crate::boolean_eval::is_structural_expression(ctx, &block.cond, prefix));

    if !all_conditions_structural && branches.len() > 1 {
        let first_targets = collect_when_eq_targets(&branches[0].1);
        for (i, (_, branch_eqs)) in branches.iter().enumerate().skip(1) {
            let targets = collect_when_eq_targets(branch_eqs);
            if targets != first_targets {
                return Err(FlattenError::unsupported_equation(
                    format!(
                        "MLS §8.3.5: if-equation branches in when-clause must assign to the same variables. \
                         Branch 1 assigns to [{}], branch {} assigns to [{}]",
                        first_targets
                            .iter()
                            .map(|v| v.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                        i + 1,
                        targets
                            .iter()
                            .map(|v| v.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                    ),
                    span,
                ));
            }
        }
        // Also validate the else branch if present
        if !else_eqs.is_empty() {
            let else_targets = collect_when_eq_targets(&else_eqs);
            if else_targets != first_targets {
                return Err(FlattenError::unsupported_equation(
                    format!(
                        "MLS §8.3.5: if-equation branches in when-clause must assign to the same variables. \
                         Branch 1 assigns to [{}], else branch assigns to [{}]",
                        first_targets
                            .iter()
                            .map(|v| v.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                        else_targets
                            .iter()
                            .map(|v| v.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                    ),
                    span,
                ));
            }
        }
    }

    let origin = "if-equation in when-clause".to_string();
    Ok(Some(flat::WhenEquation::conditional(
        branches, else_eqs, span, origin,
    )))
}

/// Collect the set of LHS assignment targets from a list of when-equations.
fn collect_when_eq_targets(eqs: &[flat::WhenEquation]) -> std::collections::HashSet<flat::VarName> {
    let mut targets = std::collections::HashSet::new();
    for eq in eqs {
        match eq {
            flat::WhenEquation::Assign { target, .. } => {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Reinit { state, .. } => {
                targets.insert(state.clone());
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, .. } => {
                for out in outputs {
                    targets.insert(out.clone());
                }
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, branch_eqs) in branches {
                    targets.extend(collect_when_eq_targets(branch_eqs));
                }
                targets.extend(collect_when_eq_targets(else_branch));
            }
            flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {}
        }
    }
    targets
}

fn validate_when_branch_targets(
    ctx: &Context,
    blocks: &[ast::EquationBlock],
    clauses: &[flat::WhenClause],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    if clauses.len() <= 1 {
        return Ok(());
    }

    let all_conditions_structural = blocks
        .iter()
        .all(|block| crate::boolean_eval::is_structural_expression(ctx, &block.cond, prefix));
    if all_conditions_structural {
        return Ok(());
    }

    let first_targets = collect_when_eq_targets(&clauses[0].equations);
    for (index, clause) in clauses.iter().enumerate().skip(1) {
        let targets = collect_when_eq_targets(&clause.equations);
        if targets != first_targets {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "MLS §8.3.5: when/elsewhen branches must assign to the same variables. \
                     Branch 1 assigns to [{}], branch {} assigns to [{}]",
                    first_targets
                        .iter()
                        .map(|v| v.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                    index + 1,
                    targets
                        .iter()
                        .map(|v| v.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                span,
            ));
        }
    }

    Ok(())
}

/// Flatten a simple assignment in a when-clause: `target = value`
///
/// Handles both simple assignments like `x = expr` and tuple assignments
/// like `(a, b) = func(args)` for multi-output function calls.
fn flatten_when_simple_equation(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    // Check for tuple assignment (multi-output function call)
    if let ast::Expression::Tuple { elements } = lhs {
        // Extract output variable names from the tuple
        let mut outputs = Vec::new();
        for elem in elements {
            match elem {
                ast::Expression::ComponentReference(cr) => {
                    let name = build_qualified_name(prefix, cr);
                    outputs.push(flat::VarName::new(name));
                }
                _ => {
                    return Err(FlattenError::unsupported_equation(
                        "tuple elements in when-equation must be simple variable references",
                        span,
                    ));
                }
            }
        }

        // Qualify the function call expression
        let function = qualify_expression_imports_with_def_map(rhs, prefix, imports, def_map);
        let origin = format!(
            "when equation multi-output assignment to ({})",
            outputs
                .iter()
                .map(|v| v.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );

        return Ok(Some(flat::WhenEquation::function_call_outputs(
            outputs, function, span, origin,
        )));
    }

    // Simple single-target assignment
    let target = extract_assignment_target(lhs, prefix)?;
    let value = qualify_expression_imports_with_def_map(rhs, prefix, imports, def_map);
    let origin = format!("when equation assignment to {}", target);
    Ok(Some(flat::WhenEquation::assign(
        target, value, span, origin,
    )))
}

/// Flatten a function call in a when-clause (reinit, assert, terminate).
fn flatten_when_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    let first_part = comp.parts.first().ok_or_else(|| {
        FlattenError::unsupported_equation("invalid function call in when-equation", span)
    })?;

    let func_name = &first_part.ident.text;

    // Build full qualified name for checking qualified side-effect functions
    let full_name: String = comp
        .parts
        .iter()
        .map(|p| p.ident.text.to_string())
        .collect::<Vec<_>>()
        .join(".");
    let last_part = comp.parts.last().map(|p| &*p.ident.text).unwrap_or("");

    // Check for known functions (by first part or full qualified name)
    match &**func_name {
        "reinit" => flatten_reinit_call(args, prefix, span, imports, def_map),
        "assert" => flatten_assert_call(args, prefix, span, imports, def_map),
        "terminate" => flatten_terminate_call(args, span),
        // print() is a side-effect function that outputs debug messages
        // It doesn't contribute to the DAE system, so we skip it
        "print" => Ok(None),
        // Handle qualified Streams.* functions which are side-effects
        "Streams" | "Modelica" => {
            // Check if it's a known Streams function (print, close, error, etc.)
            if last_part == "print"
                || last_part == "close"
                || last_part == "error"
                || full_name.ends_with(".print")
                || full_name.ends_with(".close")
                || full_name.ends_with(".error")
            {
                Ok(None) // Skip side-effect functions
            } else {
                Err(FlattenError::unsupported_equation(
                    format!("unsupported function '{}' in when-equation", full_name),
                    span,
                ))
            }
        }
        _ => Err(FlattenError::unsupported_equation(
            format!("unsupported function '{}' in when-equation", func_name),
            span,
        )),
    }
}

/// Flatten a for-equation inside a when-clause.
///
/// For-equations inside when-clauses are expanded by iterating over the
/// index range and recursively flattening each expanded equation.
///
/// Example:
/// ```modelica
/// when change(index) then
///   for i in 1:n loop
///     k[i] = if index == i then 1 else 0;
///   end for;
/// end when;
/// ```
/// Expands to assignments: k[1] = ..., k[2] = ..., etc.
fn flatten_when_for_equation(
    ctx: &Context,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::WhenEquation>, FlattenError> {
    // For now, support single index
    if indices.is_empty() {
        return Ok(vec![]);
    }

    let index = &indices[0];
    let var_name = index.ident.text.to_string();

    // Try to evaluate the range using context-aware evaluation
    let range_values = match expand_range_indices(ctx, &index.range, prefix, span) {
        Ok(values) => values,
        Err(_) => {
            return Ok(vec![]);
        }
    };

    // Collect all expanded equations
    let mut all_when_eqs = Vec::new();

    for value in range_values {
        // Substitute the index variable in all nested equations
        for eq in equations {
            let substituted = substitute_index_in_equation(eq, &var_name, value);
            // Recursively flatten the substituted equation
            let when_eqs = flatten_when_body_equation(ctx, &substituted, prefix, span, def_map)?;
            all_when_eqs.extend(when_eqs);
        }
    }

    Ok(all_when_eqs)
}

/// Flatten an assert() call: assert(condition, message, level?)
fn flatten_assert_call(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    if args.len() < 2 {
        return Err(FlattenError::unsupported_equation(
            "assert() requires at least 2 arguments (condition, message)",
            span,
        ));
    }

    let condition = qualify_expression_imports_with_def_map(&args[0], prefix, imports, def_map);
    let message =
        extract_string_literal(&args[1]).unwrap_or_else(|| "assertion failed".to_string());
    let origin = "assert in when-clause".to_string();

    Ok(Some(flat::WhenEquation::assert(
        condition, message, span, origin,
    )))
}

/// Flatten a terminate() call: terminate(message)
fn flatten_terminate_call(
    args: &[ast::Expression],
    span: rumoca_core::Span,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    if args.is_empty() {
        return Err(FlattenError::unsupported_equation(
            "terminate() requires 1 argument (message)",
            span,
        ));
    }

    let message =
        extract_string_literal(&args[0]).unwrap_or_else(|| "simulation terminated".to_string());
    let origin = "terminate in when-clause".to_string();

    Ok(Some(flat::WhenEquation::terminate(message, span, origin)))
}

/// Extract a string literal from an expression.
fn extract_string_literal(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
        } => {
            // Remove surrounding quotes
            let text = &token.text;
            if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
                Some(text[1..text.len() - 1].to_string())
            } else {
                Some(text.to_string())
            }
        }
        _ => None,
    }
}

/// Flatten a reinit() call: reinit(x, expr)
fn flatten_reinit_call(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    if args.len() != 2 {
        return Err(FlattenError::unsupported_equation(
            "reinit() requires exactly 2 arguments",
            span,
        ));
    }

    let state = extract_assignment_target(&args[0], prefix)?;
    let value = qualify_expression_imports_with_def_map(&args[1], prefix, imports, def_map);
    let origin = format!("reinit({})", state);

    // Note: EQN-016 validation (reinit target must be state) is done in ToDae phase
    // where we have full variable classification
    Ok(Some(flat::WhenEquation::reinit(state, value, span, origin)))
}

/// Extract the target variable name from an assignment LHS.
fn extract_assignment_target(
    lhs: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Result<flat::VarName, FlattenError> {
    match lhs {
        ast::Expression::ComponentReference(cr) => {
            let name = build_qualified_name(prefix, cr);
            Ok(flat::VarName::new(name))
        }
        _ => {
            // LHS must be a simple variable reference
            Err(FlattenError::unsupported_equation(
                "when-equation LHS must be a simple variable reference",
                rumoca_core::Span::DUMMY,
            ))
        }
    }
}
