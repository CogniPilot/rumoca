//! Identity and within-group domination for materialized function calls.
//!
//! Statement-group boundaries clear the memo in `ExpressionLowerer`; no entry
//! in this module authorizes cross-group scheduling or reuse.

use super::*;

#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct MaterializedFunctionCallKey {
    pub(super) call_path: Vec<MaterializedCallKey>,
    pub(super) iteration_path: Vec<IterationPointId>,
    pub(super) activation_path: Vec<ConditionalActivationKey>,
    pub(super) owner: u32,
    pub(super) function: u32,
    pub(super) arguments: Vec<u32>,
}

impl MaterializedFunctionCallKey {
    pub(super) fn same_invocation(&self, other: &Self) -> bool {
        self.owner == other.owner
            && self.function == other.function
            && self.arguments == other.arguments
            && self.call_path == other.call_path
            && self.iteration_path == other.iteration_path
    }

    /// Whether this already-emitted call dominates a use in `current`.
    ///
    /// The construction-issued owner distinguishes source invocations. Within
    /// that owner, every guard fact required by the earlier emission must also
    /// hold at the later use. The projection kind is deliberately absent from
    /// a guard fact: one checked DAE conditional can reach GALEC through the
    /// function-correlation and scalar-expression views, but its ordered
    /// condition identities and selected branch remain the same proof.
    pub(super) fn dominates(&self, current: &Self) -> bool {
        self.same_invocation(current)
            && self.activation_path.iter().all(|required| {
                current.activation_path.iter().any(|active| {
                    required.operands == active.operands
                        && required.selection == active.selection
                        && required.branch == active.branch
                })
            })
    }
}
