//! Function locals that already hold a lowered primitive expression's value.
//!
//! Function construction hands the projection the current right-hand side of a
//! value for every later sequential read of it. Recording the assignment that
//! stands between them turns those reads into reads of one local, instead of
//! re-expanding that right-hand side and its lazy branch producers at each one.
//!
//! Whether a read may take the local is a domination question, so every fact
//! here carries the ordered guard facts that were active where the assignment
//! was emitted, and is readable only where every one of them holds again.

use std::collections::{HashMap, HashSet};

use rumoca_ir_galec::ast as gast;

/// One guard fact: an ordered conditional, named by its condition identities,
/// with the branch of it that the code carrying this fact runs under.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct ConditionalActivationKey {
    pub(super) kind: ConditionalActivationKind,
    pub(super) operands: Vec<u32>,
    pub(super) branch: u32,
}

/// The projection view a guard fact was issued from. It is deliberately absent
/// from every comparison here and in `MaterializedFunctionCallKey::dominates`:
/// one checked DAE conditional reaches GALEC through several views, and its
/// ordered condition identities with the selected branch are the whole proof.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ConditionalActivationKind {
    ConditionalScalar,
    ConditionalRecord,
    FunctionConditional,
    ArraySelection,
    ArrayUpdate,
    Concatenation,
}

/// The assigned-local facts live at one point of one function body.
#[derive(Clone, Default)]
pub(super) struct AssignedPrimitives {
    live: HashMap<u32, Assigned>,
    carried: HashSet<u32>,
}

/// One local that holds a value, and the guard facts under which it does.
#[derive(Clone)]
struct Assigned {
    name: gast::Name,
    activation: Vec<ConditionalActivationKey>,
}

/// The facts that were live on entering a branch.
#[derive(Clone)]
pub(super) struct AssignedPrimitiveSnapshot {
    live: HashMap<u32, Assigned>,
}

impl AssignedPrimitives {
    /// The local a read of `expression` may take under the active guard facts.
    pub(super) fn read(
        &self,
        expression: u32,
        active: &[ConditionalActivationKey],
    ) -> Option<gast::Name> {
        let assigned = self.live.get(&expression)?;
        covers(active, &assigned.activation).then(|| assigned.name.clone())
    }

    /// Record that `target` now holds `expression`, and drop what that store
    /// invalidates.
    ///
    /// A fact survives only when its guard facts cannot hold where this store
    /// runs: two paths that select different branches of one conditional are
    /// disjoint, so the store never reaches a reader of that fact. Every other
    /// fact naming `target` goes, because a reader of it may sit after this
    /// store on a path both describe.
    pub(super) fn remember(
        &mut self,
        expression: u32,
        target: gast::Name,
        activation: &[ConditionalActivationKey],
    ) {
        self.live.retain(|_, assigned| {
            assigned.name != target || disjoint(&assigned.activation, activation)
        });
        self.remember_joined(expression, target, activation);
    }

    /// Record that `target` holds a joined value whose every branch has already
    /// stored into it.
    ///
    /// The stores are the branch assignments, which invalidated what they had
    /// to when they were emitted. The join moves no value of its own, so it
    /// adds the wider fact without dropping the narrower per-branch ones:
    /// inside a branch the local still holds that branch's value, which is what
    /// a correlated sibling reads.
    pub(super) fn remember_joined(
        &mut self,
        expression: u32,
        target: gast::Name,
        activation: &[ConditionalActivationKey],
    ) {
        self.live.insert(
            expression,
            Assigned {
                name: target,
                activation: activation.to_vec(),
            },
        );
    }

    pub(super) fn snapshot(&self) -> AssignedPrimitiveSnapshot {
        AssignedPrimitiveSnapshot {
            live: self.live.clone(),
        }
    }

    /// Revert to the facts that held on entering a branch, minus the ones a
    /// correlated sibling is proven to need.
    ///
    /// Leaving a branch normally erases everything it assigned, because outside
    /// the branch the local may hold nothing of the kind. A carried value is
    /// one [`carry`](Self::carry) named because a sibling definition of the
    /// same group would otherwise expand it into an assignment that overwrites
    /// what it reads. Its fact stays live, and stays readable only under its
    /// own recorded guard, which is where the sibling reads it.
    pub(super) fn restore(&mut self, snapshot: &AssignedPrimitiveSnapshot) {
        let carried = std::mem::take(&mut self.carried);
        self.live
            .retain(|expression, _| carried.contains(expression));
        for (expression, assigned) in &snapshot.live {
            self.live.insert(*expression, assigned.clone());
        }
        self.carried = carried;
    }

    /// Name the values whose facts survive a branch boundary, and return the
    /// previous naming so one group can put it back.
    pub(super) fn carry(&mut self, carried: HashSet<u32>) -> HashSet<u32> {
        std::mem::replace(&mut self.carried, carried)
    }
}

/// Whether every fact in `required` is among the facts `active` holds.
fn covers(active: &[ConditionalActivationKey], required: &[ConditionalActivationKey]) -> bool {
    required.iter().all(|required| {
        active
            .iter()
            .any(|active| required.operands == active.operands && required.branch == active.branch)
    })
}

/// Whether two sets of guard facts can never hold on the same execution.
///
/// One ordered conditional selects exactly one branch, so facts naming the same
/// condition operands with different branches describe disjoint paths. Anything
/// else counts as reachable together, the conservative answer for the
/// invalidation query that asks this.
fn disjoint(left: &[ConditionalActivationKey], right: &[ConditionalActivationKey]) -> bool {
    left.iter().any(|left| {
        right
            .iter()
            .any(|right| left.operands == right.operands && left.branch != right.branch)
    })
}
