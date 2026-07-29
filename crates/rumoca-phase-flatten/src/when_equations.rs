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

#[cfg(test)]
mod tests;

use std::collections::BTreeSet;

use indexmap::map::Entry;
use rumoca_ir_ast as ast;

use rumoca_ir_flat as flat;

use crate::equations::{
    build_qualified_name, decode_assert_arguments, decode_terminate_arguments,
    expand_range_indices, substitute_index_in_equation,
};
use crate::errors::FlattenError;
use crate::{Context, qualify_expression_imports_with_def_map_ctx};

type DefinitionMap = flat::VarNameIndexMap<rumoca_core::Span>;

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
    validate_when_equation_definitions(&branch.equations)?;

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
        } => flatten_when_if_equation(ctx, cond_blocks, else_block, prefix, span, def_map),

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
) -> Result<Vec<flat::WhenEquation>, FlattenError> {
    if cond_blocks.is_empty() {
        return Err(FlattenError::unsupported_equation(
            "if-equation in when-clause requires at least one conditional branch",
            span,
        ));
    }
    if let StructuralWhenSelection::Selected(active) =
        select_structural_when_branch(ctx, cond_blocks, else_block, prefix)
    {
        return flatten_when_equation_sequence(ctx, active, prefix, span, def_map);
    }

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

    let origin = "if-equation in when-clause".to_string();
    Ok(vec![flat::WhenEquation::conditional(
        branches, else_eqs, span, origin,
    )])
}

enum StructuralWhenSelection<'a> {
    Selected(&'a [ast::Equation]),
    Dynamic,
}

fn select_structural_when_branch<'a>(
    ctx: &Context,
    cond_blocks: &'a [ast::EquationBlock],
    else_block: &'a Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
) -> StructuralWhenSelection<'a> {
    for block in cond_blocks {
        match crate::boolean_eval::try_eval_structural_boolean(ctx, &block.cond, prefix) {
            Some(true) => return StructuralWhenSelection::Selected(&block.eqs),
            Some(false) => {}
            None => return StructuralWhenSelection::Dynamic,
        }
    }
    StructuralWhenSelection::Selected(else_block.as_deref().unwrap_or(&[]))
}

fn flatten_when_equation_sequence(
    ctx: &Context,
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<Vec<flat::WhenEquation>, FlattenError> {
    let mut flattened = Vec::new();
    for equation in equations {
        flattened.extend(flatten_when_body_equation(
            ctx, equation, prefix, span, def_map,
        )?);
    }
    Ok(flattened)
}

/// Collect the set of LHS assignment targets from a list of when-equations.
fn collect_when_eq_targets(eqs: &[flat::WhenEquation]) -> BTreeSet<rumoca_core::VarName> {
    let mut targets = BTreeSet::new();
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

fn validate_when_equation_definitions(
    equations: &[flat::WhenEquation],
) -> Result<(), FlattenError> {
    summarize_when_definitions(equations).map(|_| ())
}

fn summarize_when_definitions(
    equations: &[flat::WhenEquation],
) -> Result<DefinitionMap, FlattenError> {
    let mut definitions = DefinitionMap::default();
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, span, .. } => {
                insert_when_definition(&mut definitions, target.clone(), *span)?;
            }
            flat::WhenEquation::Reinit { state, span, .. } => {
                insert_when_definition(&mut definitions, state.clone(), *span)?;
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, span, .. } => {
                for output in outputs {
                    insert_when_definition(&mut definitions, output.clone(), *span)?;
                }
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                let mut alternatives = DefinitionMap::default();
                for (_, branch) in branches {
                    merge_alternative_definitions(
                        &mut alternatives,
                        summarize_when_definitions(branch)?,
                    );
                }
                if let Some(else_branch) = else_branch {
                    merge_alternative_definitions(
                        &mut alternatives,
                        summarize_when_definitions(else_branch)?,
                    );
                }
                for (target, span) in alternatives {
                    insert_when_definition(&mut definitions, target, span)?;
                }
            }
            flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {}
        }
    }
    Ok(definitions)
}

fn merge_alternative_definitions(definitions: &mut DefinitionMap, alternative: DefinitionMap) {
    for (target, span) in alternative {
        definitions.entry(target).or_insert(span);
    }
}

fn insert_when_definition(
    definitions: &mut DefinitionMap,
    target: rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    match definitions.entry(target) {
        Entry::Vacant(entry) => {
            entry.insert(span);
            Ok(())
        }
        Entry::Occupied(entry) => Err(FlattenError::unsupported_equation(
            format!(
                "when branch target `{}` is defined more than once",
                entry.key()
            ),
            span,
        )),
    }
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
    let target_span = lhs.span();
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
            outputs,
            function,
            target_span,
            origin,
        )));
    }

    // Simple single-target assignment
    let target = extract_assignment_target(lhs, prefix)?;
    let value =
        qualify_expression_imports_with_def_map_ctx(rhs, prefix, imports, def_map, ctx, None)?;
    let origin = format!("when equation assignment to {}", target);
    Ok(Some(flat::WhenEquation::assign(
        target,
        value,
        target_span,
        origin,
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
    let target_span = args[0].span();

    // Note: EQN-016 validation (reinit target must be state) is done in ToDae phase
    // where we have full variable classification
    Ok(Some(flat::WhenEquation::reinit(
        state,
        value,
        target_span,
        origin,
    )))
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
