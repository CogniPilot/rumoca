use super::*;
use crate::assertion_actions::{
    AlgorithmAssertionAction, AssertionScope, lower_algorithm_assertion_to_event_action,
};
use rumoca_core::{OpBinary, OpUnary};

pub(super) struct LoweredAlgorithmBody {
    pub(super) assignments: IndexMap<VarName, AlgorithmAssignment>,
    pub(super) event_actions: Vec<rumoca_ir_dae::DaeEventAction>,
}

struct AlgorithmAssertionParts<'a> {
    condition: &'a Expression,
    message: &'a Expression,
    level: Option<&'a Expression>,
    span: Span,
}

pub(super) fn is_algorithm_assertion_statement(statement: &Statement) -> bool {
    matches!(statement, Statement::Assert { .. })
        || matches!(
            statement,
            Statement::FunctionCall { comp, .. } if is_unqualified_assert(comp)
        )
}

pub(super) fn is_noop_algorithm_statement(statement: &Statement) -> bool {
    if is_algorithm_assertion_statement(statement) {
        return false;
    }
    match statement {
        Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => true,
        Statement::FunctionCall { outputs, .. } => outputs.is_empty(),
        Statement::For { equations, .. } => equations.iter().all(is_noop_algorithm_statement),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .all(|block| block.stmts.iter().all(is_noop_algorithm_statement))
                && else_block
                    .as_ref()
                    .is_none_or(|stmts| stmts.iter().all(is_noop_algorithm_statement))
        }
        Statement::When { blocks, .. } => blocks
            .iter()
            .all(|block| block.stmts.iter().all(is_noop_algorithm_statement)),
        Statement::Assignment { .. }
        | Statement::While { .. }
        | Statement::Reinit { .. }
        | Statement::Assert { .. } => false,
    }
}

fn is_unqualified_assert(comp: &ComponentReference) -> bool {
    matches!(
        comp.parts.as_slice(),
        [part] if part.ident == "assert" && part.subs.is_empty()
    )
}

fn algorithm_assertion_parts(
    statement: &Statement,
) -> Result<Option<AlgorithmAssertionParts<'_>>, String> {
    match statement {
        Statement::Assert {
            condition,
            message,
            level,
            span,
        } => Ok(Some(AlgorithmAssertionParts {
            condition,
            message,
            level: level.as_deref(),
            span: *span,
        })),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } if is_unqualified_assert(comp) => {
            if !outputs.is_empty() {
                return Err("AssertOutputAssignment".to_string());
            }
            let (condition, message, level) = match args.as_slice() {
                [condition, message] => (condition, message, None),
                [condition, message, level] => (condition, message, Some(level)),
                _ => return Err(format!("AssertArity({})", args.len())),
            };
            Ok(Some(AlgorithmAssertionParts {
                condition,
                message,
                level,
                span: *span,
            }))
        }
        _ => Ok(None),
    }
}

pub(super) fn collect_algorithm_statements_with_assertions(
    dae: &Dae,
    flat: &Model,
    statements: &[Statement],
    algorithm_span: Span,
    algorithm_origin: &str,
    scope: AssertionScope,
) -> Result<LoweredAlgorithmBody, String> {
    let mut known_targets = HashSet::new();
    for statement in statements {
        if algorithm_assertion_parts(statement)?.is_some() {
            continue;
        }
        for target in collect_statement_targets(dae, flat, statement)? {
            known_targets.insert(target);
        }
    }

    let mut assignments = IndexMap::new();
    let mut current_values = IndexMap::new();
    let mut event_actions = Vec::new();
    for statement in statements {
        if let Some(assertion) = algorithm_assertion_parts(statement)? {
            event_actions.push(lower_ordered_assertion(
                OrderedAssertionContext {
                    dae,
                    flat,
                    current_values: &current_values,
                    known_targets: &known_targets,
                    algorithm_span,
                    algorithm_origin,
                    scope,
                },
                assertion,
            )?);
            continue;
        }
        collect_nested_assertions(
            OrderedAssertionContext {
                dae,
                flat,
                current_values: &current_values,
                known_targets: &known_targets,
                algorithm_span,
                algorithm_origin,
                scope,
            },
            statement,
            None,
            &mut event_actions,
        )?;
        for (target, value, span, origin) in lower_statement_assignments_with_context(
            dae,
            flat,
            statement,
            &current_values,
            &known_targets,
        )? {
            let rewritten =
                rewrite_algorithm_current_refs(dae, &value, &current_values, &known_targets)?;
            let normalized = normalize_algorithm_current_value(dae, &target, &rewritten, span)
                .map_err(|err| err.to_string())?;
            current_values.insert(target.clone(), normalized.clone());
            assignments.insert(target.clone(), (target, normalized, span, origin));
        }
    }

    Ok(LoweredAlgorithmBody {
        assignments: collapse_overlapping_array_assignments(dae, assignments)?,
        event_actions,
    })
}

fn collect_nested_assertions(
    context: OrderedAssertionContext<'_>,
    statement: &Statement,
    guard: Option<Expression>,
    actions: &mut Vec<rumoca_ir_dae::DaeEventAction>,
) -> Result<(), String> {
    if let Some(assertion) = algorithm_assertion_parts(statement)? {
        actions.push(lower_guarded_assertion(context, assertion, guard)?);
        return Ok(());
    }
    let Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    else {
        if statement_contains_assertion(statement) {
            return Err(format!(
                "Nested{}",
                unsupported_algorithm_statement_tag(statement)
            ));
        }
        return Ok(());
    };

    let mut remaining_guard = guard;
    for block in cond_blocks {
        let branch_guard = and_guard(
            remaining_guard.clone(),
            block.cond.clone(),
            expression_span_or(&block.cond, *span),
        );
        for nested in &block.stmts {
            collect_nested_assertions(context, nested, Some(branch_guard.clone()), actions)?;
        }
        remaining_guard = Some(and_guard(
            remaining_guard,
            not_guard(block.cond.clone(), expression_span_or(&block.cond, *span)),
            *span,
        ));
    }
    if let Some(statements) = else_block {
        for nested in statements {
            collect_nested_assertions(context, nested, remaining_guard.clone(), actions)?;
        }
    }
    Ok(())
}

pub(super) fn statement_contains_assertion(statement: &Statement) -> bool {
    if is_algorithm_assertion_statement(statement) {
        return true;
    }
    match statement {
        Statement::For { equations, .. } => equations.iter().any(statement_contains_assertion),
        Statement::While { block, .. } => block.stmts.iter().any(statement_contains_assertion),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| block.stmts.iter().any(statement_contains_assertion))
                || else_block
                    .as_ref()
                    .is_some_and(|statements| statements.iter().any(statement_contains_assertion))
        }
        Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| block.stmts.iter().any(statement_contains_assertion)),
        Statement::Empty { .. }
        | Statement::Assignment { .. }
        | Statement::Return { .. }
        | Statement::Break { .. }
        | Statement::FunctionCall { .. }
        | Statement::Reinit { .. }
        | Statement::Assert { .. } => false,
    }
}

fn lower_guarded_assertion(
    context: OrderedAssertionContext<'_>,
    assertion: AlgorithmAssertionParts<'_>,
    guard: Option<Expression>,
) -> Result<rumoca_ir_dae::DaeEventAction, String> {
    let mut action = lower_ordered_assertion(context, assertion)?;
    let Some(guard) = guard else {
        return Ok(action);
    };
    let guard = rewrite_algorithm_current_refs(
        context.dae,
        &guard,
        context.current_values,
        context.known_targets,
    )?;
    let guard =
        flat_to_dae_expression_with_refs(&guard, context.flat).map_err(|err| err.to_string())?;
    let span = action.span;
    action.condition = Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(guard),
        rhs: Box::new(action.condition),
        span,
    };
    Ok(action)
}

fn and_guard(lhs: Option<Expression>, rhs: Expression, span: Span) -> Expression {
    let Some(lhs) = lhs else {
        return rhs;
    };
    Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn not_guard(expression: Expression, span: Span) -> Expression {
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(expression),
        span,
    }
}

fn expression_span_or(expression: &Expression, owner_span: Span) -> Span {
    expression.span().unwrap_or(owner_span)
}

fn lower_ordered_assertion(
    context: OrderedAssertionContext<'_>,
    assertion: AlgorithmAssertionParts<'_>,
) -> Result<rumoca_ir_dae::DaeEventAction, String> {
    reject_reads_before_assignment(&context, &assertion)?;
    let span = if assertion.span.is_dummy() {
        context.algorithm_span
    } else {
        assertion.span
    };
    let condition = rewrite_algorithm_current_refs(
        context.dae,
        assertion.condition,
        context.current_values,
        context.known_targets,
    )?;
    let message = rewrite_algorithm_current_refs(
        context.dae,
        assertion.message,
        context.current_values,
        context.known_targets,
    )?;
    let level = assertion
        .level
        .map(|level| {
            rewrite_algorithm_current_refs(
                context.dae,
                level,
                context.current_values,
                context.known_targets,
            )
        })
        .transpose()?;
    let section = match context.scope {
        AssertionScope::Runtime => "algorithm assert",
        AssertionScope::Initial => "initial algorithm assert",
    };
    lower_algorithm_assertion_to_event_action(
        context.dae,
        context.flat,
        &AlgorithmAssertionAction {
            condition,
            message,
            level,
            span,
            origin: format!("{section} ({})", context.algorithm_origin),
        },
        context.scope,
    )
    .map_err(|err| err.to_string())
}

fn reject_reads_before_assignment(
    context: &OrderedAssertionContext<'_>,
    assertion: &AlgorithmAssertionParts<'_>,
) -> Result<(), String> {
    let mut refs = HashSet::new();
    assertion.condition.collect_var_refs(&mut refs);
    assertion.message.collect_var_refs(&mut refs);
    if let Some(level) = assertion.level {
        level.collect_var_refs(&mut refs);
    }
    let current_targets = context
        .current_values
        .keys()
        .cloned()
        .collect::<HashSet<_>>();
    let unsupported_target = refs
        .into_iter()
        .filter_map(|reference| resolve_name_against_set(&reference, context.known_targets))
        .filter(|target| resolve_name_against_set(target, &current_targets).is_none())
        .min_by(|lhs, rhs| lhs.as_str().cmp(rhs.as_str()));
    if let Some(target) = unsupported_target {
        return Err(format!("AssertReadsTargetBeforeAssignment({target})"));
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct OrderedAssertionContext<'a> {
    dae: &'a Dae,
    flat: &'a Model,
    current_values: &'a IndexMap<VarName, Expression>,
    known_targets: &'a HashSet<VarName>,
    algorithm_span: Span,
    algorithm_origin: &'a str,
    scope: AssertionScope,
}
