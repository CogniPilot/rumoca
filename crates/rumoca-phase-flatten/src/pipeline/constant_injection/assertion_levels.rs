use super::flat;
use flat::{AssertionLevel, AssertionLevelLiterals};
use rumoca_core::{BuiltinFunction, Expression, Statement};

/// The literal declarations the scope tree predefines (MLS §8.3.7).
pub(crate) fn predefined_assertion_levels(
    tree: &rumoca_ir_ast::ClassTree,
) -> AssertionLevelLiterals {
    let literal = |name| {
        tree.scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_parts([
                "AssertionLevel",
                name,
            ]))
    };
    AssertionLevelLiterals {
        error: literal("error"),
        warning: literal("warning"),
    }
}

/// Settle the MLS §8.3.7 level of every assertion of the model and its
/// functions.
///
/// A level is classified by the predefined literal declaration it targets.
/// An explicit `AssertionLevel.error` is the default level and is normalized
/// to the omitted form; `AssertionLevel.warning` becomes its settled form
/// (`flat::AssertionLevel::settled_expression`). "If the level is AssertionLevel.warning, the current
/// evaluation is not aborted" and "the assert(..) statement shall have no
/// influence on the behavior of the model. For example, by evaluating the
/// condition to report the message an event is not triggered": a
/// warning-level condition is therefore evaluated as `noEvent(condition)`, so
/// no phase derives a relation, crossing function, or event from it. A level
/// that is not one of the predefined literals keeps the assertion unchanged
/// for the downstream owners, which reject it.
pub(crate) fn settle_assertion_levels(flat: &mut flat::Model, levels: AssertionLevelLiterals) {
    for assertion in flat
        .assert_equations
        .iter_mut()
        .chain(flat.initial_assert_equations.iter_mut())
    {
        settle(
            levels,
            &mut assertion.level,
            &mut assertion.condition,
            assertion.span,
        );
    }
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        settle_statements(&mut algorithm.statements, levels);
    }
    for function in flat.functions.values_mut() {
        settle_statements(&mut function.body, levels);
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            settle_when_equations(&mut branch.equations, levels);
        }
    }
}

/// Normalize one level and, at warning level, the condition it guards.
/// `owner` is the span of the assertion, which a settled level or condition
/// without a span of its own takes.
fn settle(
    levels: AssertionLevelLiterals,
    level: &mut Option<Expression>,
    condition: &mut Expression,
    owner: rumoca_core::Span,
) {
    match levels.classify(level.as_ref()) {
        Some(AssertionLevel::Error) => *level = None,
        Some(AssertionLevel::Warning) => {
            *level = settled_warning(level.as_ref(), owner);
            without_events(condition, owner);
        }
        None => {}
    }
}

fn settled_warning(level: Option<&Expression>, owner: rumoca_core::Span) -> Option<Expression> {
    let span = level.and_then(Expression::span).unwrap_or(owner);
    AssertionLevel::Warning.settled_expression(span)
}

fn settle_boxed(
    levels: AssertionLevelLiterals,
    level: &mut Option<Box<Expression>>,
    condition: &mut Expression,
    owner: rumoca_core::Span,
) {
    match levels.classify(level.as_deref()) {
        Some(AssertionLevel::Error) => *level = None,
        Some(AssertionLevel::Warning) => {
            *level = settled_warning(level.as_deref(), owner).map(Box::new);
            without_events(condition, owner);
        }
        None => {}
    }
}

/// Wrap a warning-level condition in `noEvent` unless it already is.
fn without_events(condition: &mut Expression, owner: rumoca_core::Span) {
    if matches!(
        condition,
        Expression::BuiltinCall {
            function: BuiltinFunction::NoEvent,
            ..
        }
    ) {
        return;
    }
    let span = condition.span().unwrap_or(owner);
    let inner = std::mem::replace(condition, Expression::Empty { span });
    *condition = Expression::BuiltinCall {
        function: BuiltinFunction::NoEvent,
        args: vec![inner],
        span,
    };
}

fn settle_statements(statements: &mut [Statement], levels: AssertionLevelLiterals) {
    for statement in statements {
        settle_statement(statement, levels);
    }
}

fn settle_statement(statement: &mut Statement, levels: AssertionLevelLiterals) {
    match statement {
        Statement::Assert {
            condition,
            level,
            span,
            ..
        } => settle_boxed(levels, level, condition, *span),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } if outputs.iter().all(Option::is_none)
            && rumoca_core::runtime_flow_action_function_short_name(comp.as_str())
                == Some("assert")
            && args.len() == 3 =>
        {
            match levels.classify(args.get(2)) {
                Some(AssertionLevel::Error) => args.truncate(2),
                Some(AssertionLevel::Warning) => {
                    if let Some(settled) = settled_warning(args.get(2), *span) {
                        args[2] = settled;
                    }
                    without_events(&mut args[0], *span);
                }
                None => {}
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                settle_statements(&mut block.stmts, levels);
            }
            if let Some(block) = else_block {
                settle_statements(block, levels);
            }
        }
        Statement::For { equations, .. } => settle_statements(equations, levels),
        Statement::While { block, .. } => settle_statements(&mut block.stmts, levels),
        Statement::When { blocks, .. } => {
            for block in blocks {
                settle_statements(&mut block.stmts, levels);
            }
        }
        _ => {}
    }
}

fn settle_when_equations(equations: &mut [flat::WhenEquation], levels: AssertionLevelLiterals) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assert {
                condition,
                level,
                span,
                ..
            } => settle_boxed(levels, level, condition, *span),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, branch) in branches.iter_mut() {
                    settle_when_equations(branch, levels);
                }
                if let Some(branch) = else_branch {
                    settle_when_equations(branch, levels);
                }
            }
            _ => {}
        }
    }
}
