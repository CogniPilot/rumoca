//! Definite definitions of introduced locals, separate from source definedness.
//! A lexical loop may read an enclosing snapshot, but newly defined locals
//! cannot escape it or rely on another iteration's reaching definition.

use std::collections::HashSet;

use rumoca_core::{GeneratedFunctionLocalKey, Span};

use super::{Branch, Guard, NormalizationError, NormalizedStatement as N};

type Definitions<'locals> = HashSet<GeneratedFunctionLocalKey<'locals>>;

pub(super) fn check(statements: &[N<'_>]) -> Result<(), NormalizationError> {
    definition_sites(statements, &mut HashSet::new())?;
    sequence(statements, &mut HashSet::new())
}

fn definition_sites<'locals>(
    statements: &[N<'locals>],
    sites: &mut Definitions<'locals>,
) -> Result<(), NormalizationError> {
    for statement in statements {
        match statement {
            N::Definition(definition) if !sites.insert(definition.target()) => {
                return Err(NormalizationError::DuplicateGeneratedDefinition {
                    span: definition.span(),
                });
            }
            N::For { statements, .. } | N::While { statements, .. } => {
                definition_sites(statements, sites)?
            }
            N::If {
                branches, fallback, ..
            } => {
                branch_sites(branches, sites)?;
                if let Some(fallback) = fallback {
                    definition_sites(fallback, sites)?;
                }
            }
            N::When { branches, .. } => branch_sites(branches, sites)?,
            N::Source(_) | N::Definition(_) => {}
        }
    }
    Ok(())
}

fn branch_sites<'locals>(
    branches: &[Branch<'locals>],
    sites: &mut Definitions<'locals>,
) -> Result<(), NormalizationError> {
    for branch in branches {
        definition_sites(&branch.statements, sites)?;
    }
    Ok(())
}

fn sequence<'locals>(
    statements: &[N<'locals>],
    defined: &mut Definitions<'locals>,
) -> Result<(), NormalizationError> {
    for statement in statements {
        match statement {
            N::Source(_) => {}
            N::Definition(definition) => {
                guard(definition.rhs(), defined)?;
                defined.insert(definition.target());
            }
            N::For { statements, .. } => sequence(statements, &mut defined.clone())?,
            N::While {
                condition,
                statements,
                ..
            } => {
                guard(condition, defined)?;
                sequence(statements, &mut defined.clone())?;
            }
            N::If {
                branches, fallback, ..
            } => conditional(branches, fallback.as_deref(), defined)?,
            N::When { branches, .. } => {
                for branch in branches {
                    guard(&branch.condition, defined)?;
                    sequence(&branch.statements, &mut defined.clone())?;
                }
            }
        }
    }
    Ok(())
}

fn conditional<'locals>(
    branches: &[Branch<'locals>],
    fallback: Option<&[N<'locals>]>,
    defined: &mut Definitions<'locals>,
) -> Result<(), NormalizationError> {
    let incoming = defined.clone();
    let mut fallthrough = incoming.clone();
    if let Some(fallback) = fallback {
        sequence(fallback, &mut fallthrough)?;
    }
    let mut common = fallthrough;
    for branch in branches {
        guard(&branch.condition, &incoming)?;
        let mut branch_defined = incoming.clone();
        sequence(&branch.statements, &mut branch_defined)?;
        common.retain(|key| branch_defined.contains(key));
    }
    *defined = common;
    Ok(())
}

fn guard<'locals>(
    value: &Guard<'locals>,
    defined: &Definitions<'locals>,
) -> Result<(), NormalizationError> {
    match value {
        Guard::Local(local) if !defined.contains(&local.key()) => {
            Err(undefined(local.provenance()))
        }
        Guard::Not(value) => guard(value, defined),
        Guard::And(left, right) => {
            guard(left, defined)?;
            guard(right, defined)
        }
        Guard::If {
            condition,
            if_true,
            if_false,
        } => {
            guard(condition, defined)?;
            guard(if_true, defined)?;
            guard(if_false, defined)
        }
        Guard::Source(_) | Guard::Local(_) | Guard::Literal(_) => Ok(()),
    }
}

fn undefined(span: Span) -> NormalizationError {
    NormalizationError::UndefinedGeneratedLocal { span }
}
