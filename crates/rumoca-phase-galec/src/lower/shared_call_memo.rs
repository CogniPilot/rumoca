//! The memo that lets one emitted call serve every later use of it.
//!
//! A materialized call is remembered by the guard facts that held where it was
//! emitted, and a later use may take its result temporaries only where all of
//! those facts hold again. That domination rule is the whole admission proof
//! for sharing a call across statement groups, so it lives here with the group
//! boundary operations that carry the memo across one, and with the record of
//! which schedule node wrote each entry.

use super::*;

#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct MaterializedFunctionCallKey {
    pub(super) call_path: Vec<MaterializedCallKey>,
    pub(super) activation_path: Vec<ConditionalActivationKey>,
    pub(super) owner: u32,
    pub(super) function: u32,
    pub(super) arguments: Vec<u32>,
}

impl MaterializedFunctionCallKey {
    /// Whether this already-emitted call dominates a use in `current`.
    ///
    /// The construction-issued owner distinguishes source invocations. Within
    /// that owner, every guard fact required by the earlier emission must also
    /// hold at the later use. The projection kind is deliberately absent from
    /// a guard fact: one checked DAE conditional can reach GALEC through the
    /// function-correlation and scalar-expression views, but its ordered
    /// condition identities and selected branch remain the same proof.
    pub(super) fn dominates(&self, current: &Self) -> bool {
        self.owner == current.owner
            && self.function == current.function
            && self.arguments == current.arguments
            && self.call_path == current.call_path
            && self.activation_path.iter().all(|required| {
                current.activation_path.iter().any(|active| {
                    required.operands == active.operands && required.branch == active.branch
                })
            })
    }
}

pub(super) type SharedMaterializedFunctionCalls =
    HashMap<MaterializedFunctionCallKey, Vec<gast::Name>>;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    /// Open one schedulable state-assignment group, naming the shared-call
    /// temporaries it may restore.
    ///
    /// A group's guard lowers before its first statement boundary, so the memo
    /// has to be installed here rather than only at that boundary: otherwise a
    /// guard reads whatever the previous group left behind, which for an
    /// activation-keyed memo is temporaries written under a different guard.
    pub(super) fn begin_shared_call_group(&mut self, shared: &SharedMaterializedFunctionCalls) {
        self.finish_statement_group();
        self.materialized_function_calls.clone_from(shared);
    }

    /// Finish one schedulable state-assignment group while retaining only
    /// call temporaries initialized by a dominating shared-call node.
    pub(super) fn take_prefix_statements_with_shared_calls(
        &mut self,
        shared: &SharedMaterializedFunctionCalls,
    ) -> Vec<gast::Spanned<gast::Statement>> {
        self.finish_statement_group();
        self.materialized_function_calls.clone_from(shared);
        self.drain_prefix_statements()
    }

    pub(super) fn shared_materialized_function_calls(&self) -> SharedMaterializedFunctionCalls {
        self.materialized_function_calls.clone()
    }

    /// Attribute to `node` every shared-call memo entry it added over `before`.
    ///
    /// The entries a node writes are the difference between the memo it
    /// inherited and the memo it leaves, so this needs no bookkeeping inside
    /// the materialization itself and cannot miss an entry a nested argument
    /// lowering produced.
    pub(super) fn register_shared_call_node(
        &mut self,
        before: &SharedMaterializedFunctionCalls,
        node: u32,
    ) {
        let added = self
            .materialized_function_calls
            .keys()
            .filter(|key| !before.contains_key(*key))
            .cloned()
            .collect::<Vec<_>>();
        for key in added {
            self.scheduled_shared_calls.insert(key, node);
        }
    }

    /// Take the scheduled shared calls the group just lowered has consumed.
    pub(super) fn take_consumed_scheduled_calls(&mut self) -> HashSet<u32> {
        std::mem::take(&mut self.consumed_scheduled_calls)
    }
}
