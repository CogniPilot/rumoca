use rumoca_eval_ast::eval_instantiate::{InstantiateEvalCtx, evaluate_component_condition};
use rumoca_ir_ast as ast;

use super::connections;
use super::inheritance::required_location_to_span;
use super::source_scope::{location_source_scope, location_source_scope_id};
use super::{InstantiateContext, InstantiateResult};

/// Convert algorithm statements to instance statements.
pub(super) fn algorithms_to_instance(
    ctx: &InstantiateContext,
    algorithms: &[Vec<rumoca_ir_ast::Statement>],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> InstantiateResult<Vec<Vec<ast::InstanceStatement>>> {
    algorithms
        .iter()
        .map(|stmts| {
            stmts
                .iter()
                .map(|stmt| {
                    let location = stmt.get_location();
                    Ok(ast::InstanceStatement {
                        statement: stmt.clone(),
                        origin: origin.clone(),
                        source_scope: location_source_scope(ctx, location),
                        source_scope_id: location_source_scope_id(ctx, location),
                        span: required_location_to_span(
                            location,
                            source_map,
                            "algorithm statement",
                        )?,
                    })
                })
                .collect()
        })
        .collect()
}

/// Convert borrowed equations to instance equations.
pub(super) fn equations_to_instance_cloned(
    ctx: &InstantiateContext,
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
    eval_ctx: Option<&InstantiateEvalCtx<'_>>,
) -> InstantiateResult<Vec<ast::InstanceEquation>> {
    equations_to_instance(ctx, equations, origin, source_map, eval_ctx, false)
}

/// Convert non-connection equations to instance equations in one pass.
pub(super) fn equations_to_instance_without_connections(
    ctx: &InstantiateContext,
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
    eval_ctx: Option<&InstantiateEvalCtx<'_>>,
) -> InstantiateResult<Vec<ast::InstanceEquation>> {
    equations_to_instance(ctx, equations, origin, source_map, eval_ctx, true)
}

fn equations_to_instance(
    ctx: &InstantiateContext,
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
    eval_ctx: Option<&InstantiateEvalCtx<'_>>,
    remove_connections: bool,
) -> InstantiateResult<Vec<ast::InstanceEquation>> {
    let mut instances = Vec::new();
    append_instance_equations(
        &mut instances,
        ctx,
        equations,
        origin,
        source_map,
        eval_ctx,
        remove_connections,
    )?;
    Ok(instances)
}

fn append_instance_equations(
    instances: &mut Vec<ast::InstanceEquation>,
    ctx: &InstantiateContext,
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
    eval_ctx: Option<&InstantiateEvalCtx<'_>>,
    remove_connections: bool,
) -> InstantiateResult<()> {
    for equation in equations {
        if let Some(selected) = select_structural_if_branch(equation, eval_ctx) {
            append_instance_equations(
                instances,
                ctx,
                selected,
                origin,
                source_map,
                eval_ctx,
                remove_connections,
            )?;
            continue;
        }
        if remove_connections && connections::is_connect_equation(equation) {
            continue;
        }
        let location = equation.get_location();
        instances.push(ast::InstanceEquation {
            equation: equation.clone(),
            origin: origin.clone(),
            source_scope: location_source_scope(ctx, location),
            source_scope_id: location_source_scope_id(ctx, location),
            span: required_location_to_span(location, source_map, "equation")?,
        });
    }
    Ok(())
}

/// Select an if-equation branch at instantiation time.
///
/// This is deliberately narrow. The instantiation-time condition evaluator was
/// written for conditional components (MLS §4.4.5), where the condition is
/// required to be a parameter expression of the enclosing class. Applied to an
/// arbitrary if-equation it answers questions it cannot see the data for: it
/// reads a variable's `start` attribute as if it were the variable's value, and
/// it treats a qualified name it failed to resolve (`Medium.ThermoStates`
/// through a package alias) as an enumeration literal. Both turn "unknown" into
/// a confident wrong branch, which SPEC_0008 forbids and which silently deletes
/// equations.
///
/// So a branch is chosen here only when the answer cannot be fabricated:
///
/// * the condition names nothing — a literal expression is decided by itself; or
/// * the branches contribute different numbers of equations, which MLS §8.3.4
///   permits only for a parameter-expression condition. Such an if-equation
///   cannot survive to the DAE unresolved, so the model itself asserts the
///   condition is known at translation time.
///
/// Every other if-equation is a legal runtime conditional and is left intact
/// for Flat, which owns the full constant environment (package alias constants,
/// extends modifiers) that this phase lacks.
fn select_structural_if_branch<'a>(
    equation: &'a ast::Equation,
    eval_ctx: Option<&InstantiateEvalCtx<'_>>,
) -> Option<&'a [ast::Equation]> {
    let ast::Equation::If {
        cond_blocks,
        else_block,
    } = equation
    else {
        return None;
    };
    let eval_ctx = eval_ctx?;
    if !if_selection_is_grounded(cond_blocks, else_block.as_deref()) {
        return None;
    }
    for block in cond_blocks {
        match evaluate_component_condition(eval_ctx, &block.cond) {
            Some(true) => return Some(&block.eqs),
            Some(false) => {}
            None => return None,
        }
    }
    Some(else_block.as_deref().unwrap_or_default())
}

/// True when an if-equation may be decided at instantiation time: either every
/// condition is reference-free, or the branches contribute different numbers of
/// equations (MLS §8.3.4), counting a missing `else` as an empty branch.
fn if_selection_is_grounded(
    cond_blocks: &[ast::EquationBlock],
    else_block: Option<&[ast::Equation]>,
) -> bool {
    let all_conditions_are_literal = cond_blocks
        .iter()
        .all(|block| ast::collect_component_refs(&block.cond).is_empty());
    if all_conditions_are_literal {
        return true;
    }
    let first = cond_blocks.first().map_or(0, |block| block.eqs.len());
    cond_blocks.iter().any(|block| block.eqs.len() != first)
        || else_block.map_or(0, <[ast::Equation]>::len) != first
}
