//! Redundant loop closures a source system proves before any reduction.

use super::constraints::{self, DifferentiationFacts};

impl super::SourceStructuralAnalysis<'_> {
    /// Whether the analyzed source is structurally singular (read from its
    /// existing verdict) and holds a redundant loop closure (SPEC_0053 §1): a
    /// holonomic constraint, read off the source system itself, whose
    /// differentiation proof closes only at second order and whose residual
    /// defines no state directly.
    ///
    /// Direct demotion removes only states some residual defines explicitly,
    /// so such a closure survives every demotion into the reducer's manifold
    /// and classifies redundant there; state selection can decide the
    /// reduction from this proof without running the reducer.
    #[must_use]
    pub fn holds_redundant_loop_closure(&self) -> bool {
        if !self.is_singular() {
            return false;
        }
        self.model().inspect(|view| {
            let facts = DifferentiationFacts::collect(view);
            let direct = constraints::direct_state_constraints(view, &facts);
            let defining = direct
                .admissible
                .iter()
                .chain(&direct.conditional)
                .map(|constraint| constraint.owner)
                .collect::<Vec<_>>();
            constraints::index_reduction_constraints(view, &facts)
                .iter()
                .any(|constraint| {
                    constraint.lifted_algebraic.is_none()
                        && constraint.proof.maximum_order == 2
                        && !defining.contains(&constraint.owner)
                })
        })
    }
}
