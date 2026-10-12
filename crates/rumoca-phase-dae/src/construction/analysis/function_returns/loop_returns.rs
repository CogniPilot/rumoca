//! MLS §11.2.6: a finite loop's return ends the enclosing function algorithm.
use super::*;

pub(in crate::construction::analysis) fn contains_for_return(
    statements: &[rumoca_core::Statement],
) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For { equations, .. } => contains_return(equations),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_for_return(&block.stmts))
                || else_block.as_deref().is_some_and(contains_for_return)
        }
        _ => false,
    })
}

pub(super) fn normalize(
    statements: &[rumoca_core::Statement],
    span: Span,
) -> Result<NormalizedFunctionReturns, ToDaeError> {
    require_span(span, "function loop return state")?;
    let target = rumoca_core::function_return_state_name(span.start.0);
    let mut guards = vec![store(&target, false, span)];
    let mut normalized = vec![rumoca_core::Statement::Empty { span }];
    normalized.extend(sequence(statements, &target, &mut guards, false)?);
    Ok(NormalizedFunctionReturns {
        statements: normalized,
        guards,
        has_returns: true,
    })
}

fn store(target: &VarName, value: bool, span: Span) -> GeneratedBooleanDefinition {
    GeneratedBooleanDefinition {
        mutable: true,
        target: target.clone(),
        value: Expression::Literal {
            value: Literal::Boolean(value),
            span,
        },
        span,
    }
}

fn active(target: &VarName, span: Span) -> Expression {
    Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(Expression::VarRef {
            name: Reference::generated(target.as_str()),
            subscripts: Vec::new(),
            span,
        }),
        span,
    }
}

fn sequence(
    statements: &[rumoca_core::Statement],
    target: &VarName,
    guards: &mut Vec<GeneratedBooleanDefinition>,
    mut may_have_returned: bool,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut normalized = Vec::with_capacity(statements.len());
    for statement in statements {
        let span = required_statement_span(statement, "function loop return continuation")?;
        let next = normalize_statement(statement, target, guards, may_have_returned, span)?;
        normalized.push(if may_have_returned {
            guarded_statement(&next, active(target, span))
        } else {
            next
        });
        may_have_returned |= contains_return(std::slice::from_ref(statement));
    }
    Ok(normalized)
}

fn normalize_statement(
    statement: &rumoca_core::Statement,
    target: &VarName,
    guards: &mut Vec<GeneratedBooleanDefinition>,
    may_have_returned: bool,
    span: Span,
) -> Result<rumoca_core::Statement, ToDaeError> {
    Ok(match statement {
        rumoca_core::Statement::Return { .. } => {
            guards.push(store(target, true, span));
            rumoca_core::Statement::Empty { span }
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => rumoca_core::Statement::For {
            indices: indices.clone(),
            equations: sequence(equations, target, guards, true)?,
            span,
        },
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => rumoca_core::Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| {
                    Ok(rumoca_core::StatementBlock {
                        cond: block.cond.clone(),
                        stmts: sequence(&block.stmts, target, guards, may_have_returned)?,
                    })
                })
                .collect::<Result<_, ToDaeError>>()?,
            else_block: else_block
                .as_ref()
                .map(|body| sequence(body, target, guards, may_have_returned))
                .transpose()?,
            span,
        },
        rumoca_core::Statement::While { block, .. } if contains_return(&block.stmts) => {
            return Err(unsupported_statement(
                statement,
                "function while return",
                "function return",
                "a return inside a while requires a checked exit owner",
            ));
        }
        _ => statement.clone(),
    })
}
