//! Completed reduction of the original conditional owner's direct outgoing states.

use super::*;

pub(super) fn reduce<'dae, T: proof::SameProof>(
    view: dae::DaeView<'dae>,
    conditional: dae::FunctionConditionalId<'dae>,
    value: dae::FunctionValueId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
    memo: &proof::Memo<proof::StateKey<'dae>, T>,
    expression: impl Copy
    + Fn(
        dae::DaeView<'dae>,
        dae::ExprId<'dae>,
        Option<&CallFrame<'_, 'dae>>,
        &ProofQuery<'dae>,
    ) -> Option<T>,
) -> Option<T> {
    memo.get_or_compute(proof::state_key(conditional, value, frame), || {
        let outgoing = reduce_outgoing(
            view.function_conditional(conditional)?,
            value,
            |condition| Ok::<_, std::convert::Infallible>(literal(view, condition, frame, query)),
            |assigned| Ok(state(view, assigned, frame, query, memo, expression)),
        );
        let Ok(reduced) = outgoing;
        reduced
    })
}

/// The one value every outgoing state of `owner` for `value` agrees on, with a
/// guard that reduces to literal `false` dropping its branch and the first
/// reducing to literal `true` ending the walk. `guard` reduces a condition and
/// `outgoing` an assigned state; either may stop the walk with an error, which
/// lets a caller that computes them on demand resume it once they exist.
pub(super) fn reduce_outgoing<'dae, T: proof::SameProof, E>(
    owner: dae::FunctionConditionalBodyView<'dae>,
    value: dae::FunctionValueId<'dae>,
    mut guard: impl FnMut(dae::ExprId<'dae>) -> Result<Option<NumericSourceScalar>, E>,
    mut outgoing: impl FnMut(dae::FunctionAssignedState<'dae>) -> Result<Option<T>, E>,
) -> Result<Option<T>, E> {
    let mut result = None;
    for (condition, branch) in owner.conditions().zip(owner.branches()) {
        let guard = match guard(condition)? {
            Some(NumericSourceScalar::Boolean(false)) => continue,
            guard => guard,
        };
        if guard.is_some_and(|guard| !matches!(guard, NumericSourceScalar::Boolean(true))) {
            return Ok(None);
        }
        let Some(assigned) = branch.outgoing(value) else {
            return Ok(None);
        };
        let Some(next) = outgoing(assigned)? else {
            return Ok(None);
        };
        if merge(&mut result, next).is_none() {
            return Ok(None);
        }
        if guard.is_some() {
            return Ok(result);
        }
    }
    let Some(assigned) = owner.fallback().outgoing(value) else {
        return Ok(None);
    };
    let Some(next) = outgoing(assigned)? else {
        return Ok(None);
    };
    if merge(&mut result, next).is_none() {
        return Ok(None);
    }
    Ok(result)
}

fn merge<T: proof::SameProof>(result: &mut Option<T>, next: T) -> Option<()> {
    if result.is_some_and(|result| !result.same(next)) {
        return None;
    }
    *result = Some(next);
    Some(())
}

fn state<'dae, T: proof::SameProof>(
    view: dae::DaeView<'dae>,
    state: dae::FunctionAssignedState<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
    memo: &proof::Memo<proof::StateKey<'dae>, T>,
    expression: impl Copy
    + Fn(
        dae::DaeView<'dae>,
        dae::ExprId<'dae>,
        Option<&CallFrame<'_, 'dae>>,
        &ProofQuery<'dae>,
    ) -> Option<T>,
) -> Option<T> {
    match state {
        dae::FunctionAssignedState::Assigned(definition) => expression(
            view,
            view.function_definition(definition)?.rhs(),
            frame,
            query,
        ),
        dae::FunctionAssignedState::Conditional { owner, value } => {
            reduce(view, owner, value, frame, query, memo, expression)
        }
        dae::FunctionAssignedState::Unassigned => None,
    }
}
