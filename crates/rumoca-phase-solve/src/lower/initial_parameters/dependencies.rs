//! Dependency selection shares the scalar lowering owner's immutable guard.

use std::collections::{BTreeSet, HashSet};

use rumoca_ir_dae as dae;

use crate::LowerError;
use crate::lower::scalar::ScalarSelector;

/// MLS §8.6/§18.6: an immutable selected arm determines the binding's reads;
/// tunable defaults cannot remove a dependency. SPEC_0007's initialization
/// owner retains unknown arms and requires a coupled owner for real cycles.
pub(super) fn binding_parameter_reads<'dae>(
    view: dae::DaeView<'dae>,
    binding: dae::ExprId<'dae>,
) -> Result<BTreeSet<u32>, LowerError> {
    let selector = ScalarSelector::new(view, None);
    let mut reads = BTreeSet::new();
    let mut pending = vec![binding];
    let mut visited = HashSet::new();
    let mut traversal = dae::ExpressionTraversal::new();
    while let Some(root) = pending.pop() {
        let mut failure = None;
        traversal.visit_pruned(view, [root], |id, expression| {
            if failure.is_some() || !visited.insert(id) {
                return false;
            }
            match expression.operation() {
                dae::ExpressionOperation::Conditional(operands) => {
                    failure = queue_conditional_reads(&selector, operands, &mut pending).err();
                    false
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                    reads.insert(parameter.index());
                    false
                }
                _ => true,
            }
        });
        if let Some(error) = failure {
            return Err(error);
        }
    }
    Ok(reads)
}

/// Conditional operands are checked condition/value pairs plus one fallback.
/// Visit every tested condition, every potentially selected arm, and the
/// fallback only when no immutable true condition already selected a value.
fn queue_conditional_reads<'dae>(
    selector: &ScalarSelector<'dae>,
    operands: dae::ExpressionOperands<'dae>,
    pending: &mut Vec<dae::ExprId<'dae>>,
) -> Result<(), LowerError> {
    let mut operands = operands.iter();
    while let Some(condition) = operands.next() {
        let Some(value) = operands.next() else {
            pending.push(condition);
            break;
        };
        pending.push(condition);
        // This query has no scalar domain point: retain all reads of a
        // binder-dependent aggregate rather than expand its compact domain.
        let guard = match selector.translation_guard(condition) {
            Ok(guard) => guard,
            Err(LowerError::NonComputable { .. }) => None,
            Err(error) => return Err(error),
        };
        match guard {
            Some(false) => {}
            Some(true) => {
                pending.push(value);
                break;
            }
            None => pending.push(value),
        }
    }
    Ok(())
}
