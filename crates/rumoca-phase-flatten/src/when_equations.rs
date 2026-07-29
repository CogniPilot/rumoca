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

use crate::equations::{
    build_qualified_name, decode_assert_arguments, decode_terminate_arguments,
    expand_range_indices, substitute_index_in_equation,
};
use crate::errors::FlattenError;
use crate::{Context, qualify_expression_imports_with_def_map_ctx};

/// Flatten one source when-equation to its complete semantic owner.
pub(crate) fn flatten_when_equation(
    ctx: &Context,
    inst_eq: &ast::InstanceEquation,
    prefix: &ast::QualifiedName,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenChain>, FlattenError> {
    let span = inst_eq.span;

    match &inst_eq.equation {
        ast::Equation::When(blocks) => {
            flatten_when_blocks(ctx, blocks, prefix, span, def_map).map(Some)
        }
        _ => Ok(None),
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
) -> Result<flat::WhenChain, FlattenError> {
    let Some((first, else_when)) = blocks.split_first() else {
        return Err(FlattenError::unsupported_equation(
            "when-equation requires a first branch",
            span,
        ));
    };
    let first = flatten_when_block(ctx, first, prefix, span, def_map)?;
    let mut chain = flat::WhenChain::new(first, span);
    for block in else_when {
        chain.push_else_when(flatten_when_block(ctx, block, prefix, span, def_map)?);
    }

    validate_when_branch_targets(ctx, blocks, &chain, prefix, span)?;
    Ok(chain)
}

/// Flatten a single ordered branch of a when/elsewhen chain.
pub(crate) fn flatten_when_block(
    ctx: &Context,
    block: &ast::EquationBlock,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<flat::WhenBranch, FlattenError> {
    // Qualify the condition expression
    let condition = qualify_expression_imports_with_def_map_ctx(
        &block.cond,
        prefix,
        &ctx.current_imports,
        def_map,
        ctx,
        None,
    )?;

    let mut branch = flat::WhenBranch::new(condition, block.cond.span());

    // Flatten each equation in the block
    for eq in &block.eqs {
        let when_eqs = flatten_when_body_equation(ctx, eq, prefix, span, def_map)?;
        for weq in when_eqs {
            branch.add_equation(weq);
        }
    }

    Ok(branch)
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
            flatten_when_simple_equation(ctx, lhs, rhs, prefix, span, &ctx.current_imports, def_map)
                .map(|opt| opt.into_iter().collect())
        }

        ast::Equation::FunctionCall { comp, args, .. } => {
            flatten_when_function_call(ctx, comp, args, prefix, span, &ctx.current_imports, def_map)
                .map(|opt| opt.into_iter().collect())
        }

        ast::Equation::Assert {
            condition,
            message,
            level,
        } => flatten_when_assert_equation(
            WhenAssertLowering {
                ctx,
                prefix,
                span,
                imports: &ctx.current_imports,
                def_map,
            },
            condition,
            message,
            level.as_ref(),
        )
        .map(|assertion| vec![assertion]),

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
        let condition = qualify_expression_imports_with_def_map_ctx(
            &block.cond,
            prefix,
            &ctx.current_imports,
            def_map,
            ctx,
            None,
        )?;
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
        Some(eqs)
    } else {
        None
    };

    // MLS §8.3.5 validation: all branches must assign to the same set of variables,
    // unless all switching conditions are parameter expressions.
    let all_conditions_structural = cond_blocks
        .iter()
        .all(|block| crate::boolean_eval::is_structural_expression(ctx, &block.cond, prefix));

    if !all_conditions_structural {
        let Some((_, first_equations)) = branches.first() else {
            return Err(FlattenError::unsupported_equation(
                "if-equation in when-clause requires at least one conditional branch",
                span,
            ));
        };
        let first_targets = collect_when_eq_targets(first_equations);
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
        if let Some(else_eqs) = &else_eqs {
            let else_targets = collect_when_eq_targets(else_eqs);
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
fn collect_when_eq_targets(
    eqs: &[flat::WhenEquation],
) -> std::collections::HashSet<rumoca_core::VarName> {
    let mut targets = std::collections::HashSet::new();
    for eq in eqs {
        match eq {
            flat::WhenEquation::Assign { target, .. } => {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Reinit { .. } => {}
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
                if let Some(else_branch) = else_branch {
                    targets.extend(collect_when_eq_targets(else_branch));
                }
            }
            flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {}
        }
    }
    targets
}

fn validate_when_branch_targets(
    ctx: &Context,
    blocks: &[ast::EquationBlock],
    chain: &flat::WhenChain,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    if blocks.len() <= 1 {
        return Ok(());
    }

    let all_conditions_structural = blocks
        .iter()
        .all(|block| crate::boolean_eval::is_structural_expression(ctx, &block.cond, prefix));
    if all_conditions_structural {
        return Ok(());
    }

    let first_targets = collect_when_eq_targets(&chain.first().equations);
    for (index, branch) in chain.branches().skip(1).enumerate() {
        let targets = collect_when_eq_targets(&branch.equations);
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
                    index + 2,
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
    ctx: &Context,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    // Check for tuple assignment (multi-output function call)
    if let ast::Expression::Tuple { elements, .. } = lhs {
        // Extract output variable names from the tuple
        let mut outputs = Vec::new();
        for elem in elements {
            match elem {
                ast::Expression::ComponentReference(cr) => {
                    let name = build_qualified_name(prefix, cr);
                    outputs.push(rumoca_core::VarName::new(name));
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
        let function =
            qualify_expression_imports_with_def_map_ctx(rhs, prefix, imports, def_map, ctx, None)?;
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
    let value =
        qualify_expression_imports_with_def_map_ctx(rhs, prefix, imports, def_map, ctx, None)?;
    let origin = format!("when equation assignment to {}", target);
    Ok(Some(flat::WhenEquation::assign(
        target, value, span, origin,
    )))
}

/// Flatten a function call in a when-clause (reinit, assert, terminate).
fn flatten_when_function_call(
    ctx: &Context,
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
    // Check for known functions (by first part or full qualified name)
    match &**func_name {
        "reinit" => flatten_reinit_call(ctx, args, prefix, span, imports, def_map),
        "assert" => flatten_assert_call(ctx, args, prefix, span, imports, def_map),
        "terminate" => flatten_terminate_call(ctx, args, prefix, span, imports, def_map),
        "print" => Err(FlattenError::unsupported_equation(
            "print() in a when-equation requires a typed checked event-call owner",
            span,
        )),
        "Streams" | "Modelica" => {
            if is_known_streams_side_effect_call(comp) {
                Err(FlattenError::unsupported_equation(
                    format!(
                        "{full_name}() in a when-equation requires a typed checked event-call owner"
                    ),
                    span,
                ))
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

fn is_known_streams_side_effect_call(comp: &ast::ComponentReference) -> bool {
    let first = comp
        .parts
        .first()
        .map(|part| part.ident.text.as_ref())
        .unwrap_or("");
    let has_streams_segment = comp
        .parts
        .iter()
        .any(|part| part.ident.text.as_ref() == "Streams");
    let is_streams_scope = first == "Streams" || (first == "Modelica" && has_streams_segment);
    if !is_streams_scope {
        return false;
    }

    matches!(
        comp.parts.last().map(|part| part.ident.text.as_ref()),
        Some("print" | "close" | "error")
    )
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
    expand_when_for_indices(ctx, indices, equations.to_vec(), prefix, span, def_map)
}

fn expand_when_for_indices(
    ctx: &Context,
    indices: &[ast::ForIndex],
    equations: Vec<ast::Equation>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::WhenEquation>, FlattenError> {
    let Some((index, rest)) = indices.split_first() else {
        let mut all_when_eqs = Vec::new();
        for eq in &equations {
            all_when_eqs.extend(flatten_when_body_equation(ctx, eq, prefix, span, def_map)?);
        }
        return Ok(all_when_eqs);
    };

    let var_name = index.ident.text.to_string();
    let range_values = expand_range_indices(ctx, &index.range, prefix, span)?;
    let mut all_when_eqs = Vec::new();

    for value in range_values {
        let substituted = equations
            .iter()
            .map(|eq| substitute_index_in_equation(eq, &var_name, value))
            .collect();
        all_when_eqs.extend(expand_when_for_indices(
            ctx,
            rest,
            substituted,
            prefix,
            span,
            def_map,
        )?);
    }

    Ok(all_when_eqs)
}

/// Flatten an assert() call: assert(condition, message, level?)
fn flatten_assert_call(
    ctx: &Context,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    let decoded = decode_assert_arguments(args, span)?;
    flatten_when_assert_equation(
        WhenAssertLowering {
            ctx,
            prefix,
            span,
            imports,
            def_map,
        },
        decoded.condition,
        decoded.message,
        decoded.level,
    )
    .map(Some)
}

#[derive(Clone, Copy)]
struct WhenAssertLowering<'a> {
    ctx: &'a Context,
    prefix: &'a ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &'a crate::qualify::ImportMap,
    def_map: Option<&'a crate::ResolveDefMap>,
}

fn flatten_when_assert_equation(
    lowering: WhenAssertLowering<'_>,
    condition: &ast::Expression,
    message: &ast::Expression,
    level: Option<&ast::Expression>,
) -> Result<flat::WhenEquation, FlattenError> {
    let condition = qualify_expression_imports_with_def_map_ctx(
        condition,
        lowering.prefix,
        lowering.imports,
        lowering.def_map,
        lowering.ctx,
        None,
    )?;
    let message = qualify_expression_imports_with_def_map_ctx(
        message,
        lowering.prefix,
        lowering.imports,
        lowering.def_map,
        lowering.ctx,
        None,
    )?;
    let level = level
        .map(|level| {
            qualify_expression_imports_with_def_map_ctx(
                level,
                lowering.prefix,
                lowering.imports,
                lowering.def_map,
                lowering.ctx,
                None,
            )
        })
        .transpose()?;
    Ok(flat::WhenEquation::assert(
        condition,
        message,
        level,
        lowering.span,
        "assert in when-clause",
    ))
}

/// Flatten a terminate() call: terminate(message)
fn flatten_terminate_call(
    ctx: &Context,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    imports: &crate::qualify::ImportMap,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Option<flat::WhenEquation>, FlattenError> {
    let message = decode_terminate_arguments(args, span)?;
    let message =
        qualify_expression_imports_with_def_map_ctx(message, prefix, imports, def_map, ctx, None)?;
    let origin = "terminate in when-clause".to_string();

    Ok(Some(flat::WhenEquation::terminate(message, span, origin)))
}

/// Flatten a reinit() call: reinit(x, expr)
fn flatten_reinit_call(
    ctx: &Context,
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
    let value =
        qualify_expression_imports_with_def_map_ctx(&args[1], prefix, imports, def_map, ctx, None)?;
    let origin = format!("reinit({})", state);

    // Note: EQN-016 validation (reinit target must be state) is done in ToDae phase
    // where we have full variable classification
    Ok(Some(flat::WhenEquation::reinit(state, value, span, origin)))
}

/// Extract the target variable name from an assignment LHS.
fn extract_assignment_target(
    lhs: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Result<rumoca_core::VarName, FlattenError> {
    match lhs {
        ast::Expression::ComponentReference(cr) => {
            let name = build_qualified_name(prefix, cr);
            Ok(rumoca_core::VarName::new(name))
        }
        _ => {
            // LHS must be a simple variable reference
            Err(FlattenError::unsupported_equation(
                "when-equation LHS must be a simple variable reference",
                lhs.span(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        extract_assignment_target, flatten_when_blocks, flatten_when_body_equation,
        flatten_when_for_equation, flatten_when_function_call, flatten_when_if_equation,
        is_known_streams_side_effect_call,
    };
    use crate::errors::FlattenError;
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::TerminalType;
    use rumoca_ir_flat as flat;
    use std::sync::Arc;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text.to_string()),
            ..rumoca_core::Token::default()
        }
    }

    fn comp_ref(path: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: rumoca_core::ComponentPath::from_flat_path(path)
                .into_parts()
                .into_iter()
                .map(|part| ast::ComponentRefPart {
                    ident: token(&part),
                    subs: None,
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn int_expr(value: i64) -> ast::Expression {
        int_expr_with_span(value, rumoca_core::Span::DUMMY)
    }

    fn int_expr_with_span(value: i64, span: rumoca_core::Span) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: token(&value.to_string()),
            span,
        }
    }

    fn bool_expr(value: bool, span: rumoca_core::Span) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token: token(if value { "true" } else { "false" }),
            span,
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("when_equations_test.mo"),
            10,
            14,
        )
    }

    fn range_expr(start: i64, end: i64) -> ast::Expression {
        ast::Expression::Range {
            start: Arc::new(int_expr(start)),
            step: None,
            end: Arc::new(int_expr(end)),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn var_expr(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(comp_ref(name))
    }

    fn var_expr_with_span(name: &str, span: rumoca_core::Span) -> ast::Expression {
        let mut reference = comp_ref(name);
        reference.span = span;
        ast::Expression::ComponentReference(reference)
    }

    fn for_index(name: &str, start: i64, end: i64) -> ast::ForIndex {
        ast::ForIndex {
            ident: token(name),
            range: range_expr(start, end),
        }
    }

    fn indexed_var_expr(name: &str, subscripts: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: token(name),
                subs: Some(
                    subscripts
                        .iter()
                        .map(|name| ast::Subscript::Expression(var_expr(name)))
                        .collect(),
                ),
            }],
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        })
    }

    fn assignment(target: &str, value: i64) -> ast::Equation {
        ast::Equation::Simple {
            lhs: var_expr(target),
            rhs: int_expr(value),
        }
    }

    fn named_argument(
        name: &str,
        value: ast::Expression,
        span: rumoca_core::Span,
    ) -> ast::Expression {
        ast::Expression::NamedArgument {
            name: token(name),
            value: Arc::new(value),
            span,
        }
    }

    #[test]
    fn when_elsewhen_retains_one_owner_and_ordered_branch_spans() {
        let source = rumoca_core::SourceId::from_source_name("when_chain_test.mo");
        let owner_span = rumoca_core::Span::from_offsets(source, 5, 80);
        let first_span = rumoca_core::Span::from_offsets(source, 10, 14);
        let second_span = rumoca_core::Span::from_offsets(source, 40, 45);
        let blocks = vec![
            ast::EquationBlock {
                cond: bool_expr(true, first_span),
                eqs: vec![ast::Equation::Simple {
                    lhs: var_expr("m"),
                    rhs: int_expr(1),
                }],
            },
            ast::EquationBlock {
                cond: bool_expr(false, second_span),
                eqs: vec![ast::Equation::Simple {
                    lhs: var_expr("m"),
                    rhs: int_expr(2),
                }],
            },
        ];

        let chain = flatten_when_blocks(
            &crate::Context::default(),
            &blocks,
            &ast::QualifiedName::new(),
            owner_span,
            None,
        )
        .expect("flatten one when/elsewhen owner");

        assert_eq!(chain.span(), owner_span);
        assert_eq!(chain.branch_count(), 2);
        let branches = chain.branches().collect::<Vec<_>>();
        assert_eq!(branches[0].span, first_span);
        assert_eq!(branches[1].span, second_span);
        assert!(matches!(
            &branches[0].condition,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                ..
            }
        ));
        assert!(matches!(
            &branches[1].condition,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                ..
            }
        ));
    }

    #[test]
    fn when_producer_rejects_an_empty_branch_list() {
        let span = test_span();
        let error = flatten_when_blocks(
            &crate::Context::default(),
            &[],
            &ast::QualifiedName::new(),
            span,
            None,
        )
        .expect_err("source when owner requires its first branch");

        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation {
                description,
                span: error_span,
            } if description == "when-equation requires a first branch" && error_span == span
        ));
    }

    #[test]
    fn when_chain_still_rejects_mismatched_dynamic_branch_targets() {
        let source = rumoca_core::SourceId::from_source_name("when_chain_targets_test.mo");
        let owner_span = rumoca_core::Span::from_offsets(source, 5, 80);
        let blocks = vec![
            ast::EquationBlock {
                cond: var_expr_with_span(
                    "first_trigger",
                    rumoca_core::Span::from_offsets(source, 10, 23),
                ),
                eqs: vec![ast::Equation::Simple {
                    lhs: var_expr("first_target"),
                    rhs: int_expr(1),
                }],
            },
            ast::EquationBlock {
                cond: var_expr_with_span(
                    "second_trigger",
                    rumoca_core::Span::from_offsets(source, 40, 54),
                ),
                eqs: vec![ast::Equation::Simple {
                    lhs: var_expr("second_target"),
                    rhs: int_expr(2),
                }],
            },
        ];

        let error = flatten_when_blocks(
            &crate::Context::default(),
            &blocks,
            &ast::QualifiedName::new(),
            owner_span,
            None,
        )
        .expect_err("dynamic branches with different targets violate EQN-013");

        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span, .. } if span == owner_span
        ));
    }

    #[test]
    fn when_assert_preserves_optional_level() {
        let span = test_span();
        let level_span = rumoca_core::Span::from_offsets(span.source, 30, 31);
        let equation = ast::Equation::Assert {
            condition: bool_expr(true, span),
            message: int_expr(1),
            level: Some(int_expr_with_span(2, level_span)),
        };

        let equations = flatten_when_body_equation(
            &crate::Context::default(),
            &equation,
            &ast::QualifiedName::new(),
            span,
            None,
        )
        .expect("source assert is a checked when action");

        let [
            flat::WhenEquation::Assert {
                level: Some(level),
                span: found,
                ..
            },
        ] = equations.as_slice()
        else {
            panic!("optional assertion level must remain in Flat");
        };
        assert_eq!(*found, span);
        assert_eq!(level.span(), Some(level_span));
        assert!(matches!(
            level.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                ..
            }
        ));
    }

    #[test]
    fn when_assert_and_terminate_reject_extra_or_missing_arguments() {
        let ctx = crate::Context::default();
        let prefix = ast::QualifiedName::new();
        let span = test_span();
        for count in [1, 4] {
            let args = (0..count).map(int_expr).collect::<Vec<_>>();
            let error = flatten_when_function_call(
                &ctx,
                &comp_ref("assert"),
                &args,
                &prefix,
                span,
                &ctx.current_imports,
                None,
            )
            .expect_err("assert arity must fail at its source owner");
            assert!(matches!(
                error,
                FlattenError::UnsupportedEquation { span: found, .. } if found == span
            ));
        }
        for count in [0, 2] {
            let args = (0..count).map(int_expr).collect::<Vec<_>>();
            let error = flatten_when_function_call(
                &ctx,
                &comp_ref("terminate"),
                &args,
                &prefix,
                span,
                &ctx.current_imports,
                None,
            )
            .expect_err("terminate arity must fail at its source owner");
            assert!(matches!(
                error,
                FlattenError::UnsupportedEquation { span: found, .. } if found == span
            ));
        }
    }

    #[test]
    fn when_assert_decoder_rejects_duplicate_and_unknown_named_arguments() {
        let ctx = crate::Context::default();
        let prefix = ast::QualifiedName::new();
        let span = test_span();
        let duplicate = vec![
            bool_expr(true, span),
            int_expr(1),
            named_argument("message", int_expr(2), span),
        ];
        let unknown = vec![
            bool_expr(true, span),
            int_expr(1),
            named_argument("severity", int_expr(2), span),
        ];
        for args in [&duplicate, &unknown] {
            let error = flatten_when_function_call(
                &ctx,
                &comp_ref("assert"),
                args,
                &prefix,
                span,
                &ctx.current_imports,
                None,
            )
            .expect_err("every assert slot must be known and filled once");
            assert!(matches!(
                error,
                FlattenError::UnsupportedEquation { span: found, .. } if found == span
            ));
        }
    }

    #[test]
    fn when_terminate_decoder_unwraps_only_named_message() {
        let ctx = crate::Context::default();
        let prefix = ast::QualifiedName::new();
        let span = test_span();
        let decoded = flatten_when_function_call(
            &ctx,
            &comp_ref("terminate"),
            &[named_argument("message", int_expr_with_span(7, span), span)],
            &prefix,
            span,
            &ctx.current_imports,
            None,
        )
        .expect("named terminate message is a single checked slot")
        .expect("terminate remains an event action");
        assert!(matches!(
            decoded,
            flat::WhenEquation::Terminate {
                message: rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(7),
                    ..
                },
                ..
            }
        ));

        let error = flatten_when_function_call(
            &ctx,
            &comp_ref("terminate"),
            &[named_argument("text", int_expr(1), span)],
            &prefix,
            span,
            &ctx.current_imports,
            None,
        )
        .expect_err("unknown terminate slot must fail at the call");
        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span: found, .. } if found == span
        ));
    }

    #[test]
    fn nested_when_if_rejects_mismatched_explicit_else_target() {
        let span = test_span();
        let blocks = vec![ast::EquationBlock {
            cond: var_expr_with_span("active", span),
            eqs: vec![assignment("first", 1)],
        }];
        let error = flatten_when_if_equation(
            &crate::Context::default(),
            &blocks,
            &Some(vec![assignment("second", 2)]),
            &ast::QualifiedName::new(),
            span,
            None,
        )
        .expect_err("one if plus else must compare target sets");

        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span: found, .. } if found == span
        ));
    }

    #[test]
    fn nested_when_if_distinguishes_absent_from_explicit_empty_else() {
        let span = test_span();
        let blocks = vec![ast::EquationBlock {
            cond: var_expr_with_span("active", span),
            eqs: vec![assignment("target", 1)],
        }];
        let absent = flatten_when_if_equation(
            &crate::Context::default(),
            &blocks,
            &None,
            &ast::QualifiedName::new(),
            span,
            None,
        )
        .expect("an absent else remains absent")
        .expect("conditional owner is retained");
        assert!(matches!(
            absent,
            flat::WhenEquation::Conditional {
                else_branch: None,
                ..
            }
        ));

        let error = flatten_when_if_equation(
            &crate::Context::default(),
            &blocks,
            &Some(Vec::new()),
            &ast::QualifiedName::new(),
            span,
            None,
        )
        .expect_err("an explicit empty else has an empty target set");
        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span: found, .. } if found == span
        ));
    }

    #[test]
    fn streams_error_in_when_fails_instead_of_disappearing() {
        let ctx = crate::Context::default();
        let span = test_span();
        let error = flatten_when_function_call(
            &ctx,
            &comp_ref("Modelica.Utilities.Streams.error"),
            &[int_expr(1)],
            &ast::QualifiedName::new(),
            span,
            &ctx.current_imports,
            None,
        )
        .expect_err("unrepresented Streams.error must never become an empty branch");

        let FlattenError::UnsupportedEquation {
            description,
            span: found,
        } = error
        else {
            panic!("Streams.error must report a source-owned unsupported equation");
        };
        assert_eq!(found, span);
        assert!(description.contains("typed checked event-call owner"));
    }

    #[test]
    fn assignment_target_error_uses_invalid_lhs_span() {
        let span = test_span();
        let lhs = ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: token("1"),
            span,
        };

        let err = extract_assignment_target(&lhs, &ast::QualifiedName::new())
            .expect_err("non-reference LHS should fail");

        assert!(
            matches!(
                err,
                FlattenError::UnsupportedEquation { span: error_span, .. }
                    if error_span == span
            ),
            "invalid LHS diagnostic should use the LHS source span: {err:?}"
        );
    }

    #[test]
    fn streams_side_effect_matching_uses_structured_parts() {
        assert!(is_known_streams_side_effect_call(&comp_ref(
            "Streams.print"
        )));
        assert!(is_known_streams_side_effect_call(&comp_ref(
            "Modelica.Utilities.Streams.close"
        )));
        assert!(is_known_streams_side_effect_call(&comp_ref(
            "Modelica.Utilities.Streams.error"
        )));
        assert!(!is_known_streams_side_effect_call(&comp_ref(
            "Modelica.Utilities.FakeStreams.print"
        )));
        assert!(!is_known_streams_side_effect_call(&comp_ref(
            "Modelica.Utilities.Streams.myprint"
        )));
    }

    #[test]
    fn when_for_equation_expands_all_index_ranges() {
        let ctx = crate::Context::default();
        let indices = vec![for_index("i", 1, 2), for_index("j", 1, 2)];
        let equations = vec![ast::Equation::Simple {
            lhs: indexed_var_expr("y", &["i", "j"]),
            rhs: ast::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Arc::new(var_expr("i")),
                rhs: Arc::new(var_expr("j")),
                span: rumoca_core::Span::DUMMY,
            },
        }];

        let expanded = flatten_when_for_equation(
            &ctx,
            &indices,
            &equations,
            &ast::QualifiedName::new(),
            rumoca_core::Span::DUMMY,
            None,
        )
        .unwrap();

        let targets = expanded
            .iter()
            .map(|eq| match eq {
                flat::WhenEquation::Assign { target, .. } => target.as_str().to_string(),
                other => panic!("expected assignment, got {other:?}"),
            })
            .collect::<std::collections::HashSet<_>>();

        assert_eq!(targets.len(), 4);
        assert!(targets.contains("y[1,1]"));
        assert!(targets.contains("y[1,2]"));
        assert!(targets.contains("y[2,1]"));
        assert!(targets.contains("y[2,2]"));
    }
}
