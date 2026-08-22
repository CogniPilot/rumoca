//! Proven ranges of the scalar Integer locals of a lowered function body.
//!
//! A dynamic array index is admitted only when its range is proven inside the
//! subscripted extent, because the emitted code indexes without a runtime
//! check. Function locals earn a range here: an assignment records the range
//! of the value it stores, and the record is narrowed at every control-flow
//! join so that what a reader finds always bounds every value that reader can
//! observe.
//!
//! The join over a conditional is the union over its reaching definitions: an
//! arm that assigns the local contributes the range it stores, and an arm that
//! leaves the local alone contributes the range that held before the
//! conditional, because the prior value survives an untaken arm. A reaching
//! value with no proven range makes the union unproven, never optimistic: the
//! index is then refused, which costs an export but never a bad access.

use super::*;

/// Ranges proven at one point of a function body, keyed by local lexeme.
pub(super) type LocalIntegerBounds = HashMap<String, (i64, i64)>;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    /// Record the proven range of a scalar Integer local, or forget any range
    /// previously proven for it when the new value has none.
    pub(super) fn remember_local_integer_bounds(
        &mut self,
        name: gast::Name,
        value: &gast::Expression,
    ) {
        let lexeme = name.lexeme().to_owned();
        match self.integer_expression_bounds(value) {
            Some(bounds) => {
                self.local_integer_bounds.insert(lexeme, bounds);
            }
            None => {
                self.local_integer_bounds.remove(&lexeme);
            }
        }
    }

    /// Forget the proven ranges of the locals a nested block may assign.
    ///
    /// A range proven outside a loop does not survive into it if the body
    /// reassigns the local, because a reader would then see a value from a
    /// previous iteration; and a range proven inside does not survive out,
    /// because the loop may run zero times. Only the locals the block writes
    /// are affected: an index established before an inner loop and merely read
    /// inside it keeps its range, which is the ordinary shape of a back
    /// substitution walking its right-hand sides.
    pub(super) fn forget_assigned_local_integer_bounds(&mut self, assigned: &HashSet<String>) {
        self.local_integer_bounds
            .retain(|name, _| !assigned.contains(name));
    }
}

/// Fold one arm's exit ranges into the union of the arms folded so far.
///
/// A local the union no longer bounds on every arm folded so far is dropped:
/// the union holds only what every reaching definition supports.
fn fold_reaching_arm(reaching: &mut Option<LocalIntegerBounds>, exit: &LocalIntegerBounds) {
    let Some(mut folded) = reaching.take() else {
        *reaching = Some(exit.clone());
        return;
    };
    folded.retain(|name, bounds| {
        let Some((minimum, maximum)) = exit.get(name) else {
            return false;
        };
        *bounds = (bounds.0.min(*minimum), bounds.1.max(*maximum));
        true
    });
    *reaching = Some(folded);
}

/// The union of the local ranges proven along each arm of one conditional.
///
/// Built by lowering every arm from the ranges that held at the conditional's
/// entry and folding the ranges each arm leaves proven into a running union.
/// Exactly one arm runs, so the union bounds every reaching value; a local no
/// arm writes carries its entry range through every arm and so survives the
/// union unchanged.
pub(super) struct ConditionalIntegerBounds {
    entry: LocalIntegerBounds,
    reaching: Option<LocalIntegerBounds>,
}

impl ConditionalIntegerBounds {
    /// Capture the ranges proven where the conditional begins.
    pub(super) fn enter(lowerer: &ExpressionLowerer<'_, '_>) -> Self {
        Self {
            entry: lowerer.local_integer_bounds.clone(),
            reaching: None,
        }
    }

    /// Restore the entry ranges before an arm is lowered.
    ///
    /// An arm's condition and body both run only when the preceding arms did
    /// not, so neither may read a range another arm established.
    pub(super) fn start_arm(&self, lowerer: &mut ExpressionLowerer<'_, '_>) {
        lowerer.local_integer_bounds.clone_from(&self.entry);
    }

    /// Fold the ranges the just-lowered arm leaves proven into the union.
    pub(super) fn finish_arm(&mut self, lowerer: &ExpressionLowerer<'_, '_>) {
        fold_reaching_arm(&mut self.reaching, &lowerer.local_integer_bounds);
    }

    /// Install the union as the ranges proven after the conditional.
    pub(super) fn commit(self, lowerer: &mut ExpressionLowerer<'_, '_>) {
        lowerer.local_integer_bounds = self.reaching.unwrap_or(self.entry);
    }
}

#[cfg(test)]
mod tests {
    use super::{LocalIntegerBounds, fold_reaching_arm};

    fn bounds(entries: &[(&str, (i64, i64))]) -> LocalIntegerBounds {
        entries
            .iter()
            .map(|(name, range)| ((*name).to_owned(), *range))
            .collect()
    }

    fn join(arms: &[LocalIntegerBounds]) -> LocalIntegerBounds {
        let mut reaching = None;
        for exit in arms {
            fold_reaching_arm(&mut reaching, exit);
        }
        reaching.unwrap_or_default()
    }

    #[test]
    fn union_spans_every_arm() {
        let joined = join(&[
            bounds(&[("row", (1, 2))]),
            bounds(&[("row", (4, 4))]),
            bounds(&[("row", (3, 3))]),
        ]);
        assert_eq!(joined.get("row"), Some(&(1, 4)));
    }

    #[test]
    fn an_unproven_arm_leaves_the_union_unproven() {
        let joined = join(&[
            bounds(&[("row", (1, 2))]),
            LocalIntegerBounds::new(),
            bounds(&[("row", (3, 3))]),
        ]);
        assert_eq!(joined.get("row"), None);
    }

    #[test]
    fn an_unproven_first_arm_leaves_the_union_unproven() {
        let joined = join(&[
            LocalIntegerBounds::new(),
            bounds(&[("row", (1, 2))]),
            bounds(&[("row", (3, 3))]),
        ]);
        assert_eq!(joined.get("row"), None);
    }

    #[test]
    fn a_local_no_arm_writes_keeps_its_range() {
        let entry = bounds(&[("column", (1, 4))]);
        let mut written = entry.clone();
        written.insert("row".to_owned(), (2, 2));
        let joined = join(&[written, entry]);
        assert_eq!(joined.get("column"), Some(&(1, 4)));
        assert_eq!(joined.get("row"), None);
    }
}
