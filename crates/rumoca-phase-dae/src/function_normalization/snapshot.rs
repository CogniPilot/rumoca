use rumoca_core::GeneratedFunctionLocals;

use super::{Branch, GeneratedDefinition, Guard, NormalizationError, NormalizedStatement as N};

/// Recurse in lexical position. A definition inside an outer loop remains
/// inside that loop, and is refreshed once on each visit to its source site.
pub(super) fn snapshot_sequence<'locals>(
    statements: Vec<N<'locals>>,
    locals: &mut GeneratedFunctionLocals<'_, 'locals>,
) -> Result<Vec<N<'locals>>, NormalizationError> {
    let mut result = Vec::new();
    for statement in statements {
        result.extend(snapshot_statement(statement, locals)?);
    }
    Ok(result)
}

fn snapshot_statement<'locals>(
    statement: N<'locals>,
    locals: &mut GeneratedFunctionLocals<'_, 'locals>,
) -> Result<Vec<N<'locals>>, NormalizationError> {
    let statement = match statement {
        N::For {
            indices,
            statements,
            span,
        } => N::For {
            indices,
            statements: snapshot_sequence(statements, locals)?,
            span,
        },
        N::While {
            condition,
            statements,
            span,
        } => N::While {
            condition,
            statements: snapshot_sequence(statements, locals)?,
            span,
        },
        N::If {
            branches,
            fallback,
            span,
        } => {
            return snapshot_conditional(branches, fallback, span, locals);
        }
        N::When { branches, span } => N::When {
            branches: snapshot_branches(branches, locals)?,
            span,
        },
        source_or_definition => source_or_definition,
    };
    Ok(vec![statement])
}

fn snapshot_branches<'locals>(
    branches: Vec<Branch<'locals>>,
    locals: &mut GeneratedFunctionLocals<'_, 'locals>,
) -> Result<Vec<Branch<'locals>>, NormalizationError> {
    branches
        .into_iter()
        .map(|branch| {
            Ok(Branch {
                condition: branch.condition,
                statements: snapshot_sequence(branch.statements, locals)?,
            })
        })
        .collect()
}

fn snapshot_conditional<'locals>(
    mut branches: Vec<Branch<'locals>>,
    fallback: Option<Vec<N<'locals>>>,
    span: rumoca_core::Span,
    locals: &mut GeneratedFunctionLocals<'_, 'locals>,
) -> Result<Vec<N<'locals>>, NormalizationError> {
    let has_loops = branches
        .iter()
        .any(|branch| contains_loop(&branch.statements))
        || fallback.as_deref().is_some_and(contains_loop);
    let mut prefix = Vec::new();
    if has_loops {
        let mut remaining = Guard::Literal(true);
        for branch in &mut branches {
            let local = locals.reserve_boolean(span)?;
            let value = Guard::If {
                condition: Box::new(remaining.clone()),
                if_true: Box::new(branch.condition.clone()),
                if_false: Box::new(Guard::Literal(false)),
            };
            prefix.push(N::Definition(GeneratedDefinition::construct(
                locals,
                local.key(),
                value,
            )?));
            branch.condition = Guard::Local(local);
            remaining = Guard::And(
                Box::new(remaining),
                Box::new(Guard::Not(Box::new(Guard::Local(local)))),
            );
        }
    }
    let branches = snapshot_branches(branches, locals)?;
    let fallback = fallback
        .map(|body| snapshot_sequence(body, locals))
        .transpose()?;
    prefix.push(N::If {
        branches,
        fallback,
        span,
    });
    Ok(prefix)
}

fn contains_loop(statements: &[N<'_>]) -> bool {
    statements.iter().any(|statement| match statement {
        N::For { .. } | N::While { .. } => true,
        N::If {
            branches, fallback, ..
        } => {
            branches
                .iter()
                .any(|branch| contains_loop(&branch.statements))
                || fallback.as_deref().is_some_and(contains_loop)
        }
        N::When { branches, .. } => branches
            .iter()
            .any(|branch| contains_loop(&branch.statements)),
        N::Source(_) | N::Definition(_) => false,
    })
}
