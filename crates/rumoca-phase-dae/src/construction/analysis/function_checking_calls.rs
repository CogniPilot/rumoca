//! MLS §12.3 zero-output call statements inside function bodies whose only
//! effect is checking.
//!
//! A function may call another purely to check its arguments: MSL 4.1.0's
//! `Modelica.Utilities.Streams.error(message)` is `assert(false, message,
//! AssertionLevel.error)`, and `Modelica.Math.Nonlinear.solveOneNonlinearEquation`
//! calls it when its interval does not bracket a root. Such a call computes
//! nothing, so its meaning is the assertions its body raises, exactly as for
//! a checking call in an initial algorithm ([`super::initial_algorithms`]).
//!
//! The prepared Flat model replays each such call into those assertions, with
//! the callee's inputs bound to the call's arguments, before any function is
//! certified, so certification and lowering read one body. The acceptance
//! contract is the initial-algorithm one: the callee declares no output, is
//! pure and not external, writes only its own locals, and its body holds only
//! assertions, `if` chains over them, local assignments and empty
//! statements. A callee with a `for` is not replayed here: its loop bounds may
//! read the caller's inputs, which have no translation-time value. A call
//! outside the contract is left as it is and keeps the call-statement refusal
//! that names it.

use super::initial_algorithms::{
    CheckingCall, checking_call, expand_checking_call, reject_unsupported_checking_call,
};
use super::*;

/// Replay every checking call in every function body of `flat`.
pub(super) fn replay(mut flat: Cow<'_, flat::Model>) -> Result<Cow<'_, flat::Model>, ToDaeError> {
    let constants = EvalContext::new();
    let mut updates = Vec::new();
    for (name, function) in &flat.functions {
        if let Some(body) = replay_statements(&flat, &function.body, &constants)? {
            updates.push((name.clone(), body));
        }
    }
    for (name, body) in updates {
        if let Some(function) = flat.to_mut().functions.get_mut(&name) {
            function.body = body;
        }
    }
    Ok(flat)
}

/// `statements` with each checking call replayed, or `None` when none is.
fn replay_statements(
    flat: &flat::Model,
    statements: &[rumoca_core::Statement],
    constants: &EvalContext,
) -> Result<Option<Vec<rumoca_core::Statement>>, ToDaeError> {
    let mut changed = false;
    let mut replayed = Vec::with_capacity(statements.len());
    for statement in statements {
        if let Some(assertions) = checking_call(flat, statement)
            .filter(is_candidate)
            .and_then(|call| replayed_assertions(&call, constants))
        {
            replayed.extend(assertions);
            changed = true;
            continue;
        }
        let nested = replay_nested(flat, statement, constants)?;
        changed |= nested.is_some();
        replayed.push(nested.unwrap_or_else(|| statement.clone()));
    }
    Ok(changed.then_some(replayed))
}

/// The assertions `call` raises, or `None` when its callee is outside the
/// contract. Such a call is left as it is, so certification refuses it by
/// name as a call statement that reads no result; the initial-algorithm
/// refusal of the contract describes a different owner.
fn replayed_assertions(
    call: &CheckingCall<'_>,
    constants: &EvalContext,
) -> Option<Vec<rumoca_core::Statement>> {
    reject_unsupported_checking_call(call).ok()?;
    expand_checking_call(call, constants).ok()
}

/// `statement` with the checking calls of its nested blocks replayed.
fn replay_nested(
    flat: &flat::Model,
    statement: &rumoca_core::Statement,
    constants: &EvalContext,
) -> Result<Option<rumoca_core::Statement>, ToDaeError> {
    let block = |block: &rumoca_core::StatementBlock| -> Result<_, ToDaeError> {
        Ok(
            replay_statements(flat, &block.stmts, constants)?.map(|stmts| {
                rumoca_core::StatementBlock {
                    cond: block.cond.clone(),
                    stmts,
                }
            }),
        )
    };
    let mut rebuilt = statement.clone();
    let changed = match &mut rebuilt {
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            let mut changed = false;
            for cond_block in cond_blocks.iter_mut() {
                if let Some(replaced) = block(cond_block)? {
                    *cond_block = replaced;
                    changed = true;
                }
            }
            if let Some(statements) = else_block
                && let Some(replaced) = replay_statements(flat, statements, constants)?
            {
                *statements = replaced;
                changed = true;
            }
            changed
        }
        rumoca_core::Statement::For { equations, .. } => {
            match replay_statements(flat, equations, constants)? {
                Some(replaced) => {
                    *equations = replaced;
                    true
                }
                None => false,
            }
        }
        rumoca_core::Statement::While { block: body, .. } => match block(body)? {
            Some(replaced) => {
                *body = replaced;
                true
            }
            None => false,
        },
        _ => false,
    };
    Ok(changed.then_some(rebuilt))
}

/// A call whose callee could only check: no output, pure, not external, and
/// no `for` in its body. Anything else keeps the call-statement refusal.
fn is_candidate(call: &CheckingCall<'_>) -> bool {
    let function = call.function();
    function.outputs.is_empty()
        && function.pure
        && function.external.is_none()
        && !contains_for(&function.body)
}

fn contains_for(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::For { .. } => true,
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| contains_for(&block.stmts))
                || else_block.as_deref().is_some_and(contains_for)
        }
        rumoca_core::Statement::While { block, .. } => contains_for(&block.stmts),
        _ => false,
    })
}
