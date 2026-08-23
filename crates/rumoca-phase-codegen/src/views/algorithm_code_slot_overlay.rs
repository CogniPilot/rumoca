//! Which slots *inside one working-memory region* may share storage, and the
//! evidence that they may.
//!
//! The sibling module `algorithm_code_overlay` decides which **regions** share
//! storage. Its unit is the region and its relation is a property of the call
//! graph, which makes it exact at region granularity and blind below it: a
//! function whose body is a long `if/else if` chain gets one region sized for
//! the sum of every arm, even though only one arm ever runs. This module is the
//! layer under that one. Its unit is the **slot**, and its relation is a
//! property of the owner's own statement structure.
//!
//! # The relation
//!
//! Every slot carries a **home**: the innermost scope of its owner's body that
//! contains every use of it, computed by `LocalPlacements` as the common prefix
//! of the scope paths of the uses it saw. A home that ends inside an arm of a
//! conditional is therefore positive evidence that nothing outside that arm
//! ever names the slot.
//!
//! Two slots may share storage when their homes are **exclusive arms**:
//!
//! * walk both homes together to the first step where they differ;
//! * that step must be a branching step of the **same** conditional on both
//!   sides (two different `elseif` arms, or an arm and the `else`), so at most
//!   one of the two bodies runs per execution of that conditional;
//! * and no step *before* the divergence may be a loop body, so that
//!   conditional is entered at most once per activation of the owner.
//!
//! Everything that is not that positive proof is a refusal. A home that is a
//! prefix of the other is a refusal: the shorter home spans the conditional the
//! longer one sits inside, so the outer slot is live across it. Divergence at
//! two *different* statements of one body is a refusal: consecutive statements
//! both run. A slot with no home at all is a refusal.
//!
//! # Why one execution per activation is enough
//!
//! Working memory carries nothing between activations of its owner. That is not
//! an assumption added here: it is the property the region overlay already
//! rests on, since a region is shared with every never-concurrent owner and any
//! of them may run between two calls of this one and overwrite it. So a slot's
//! live range is contained in one activation, and if the conditional that
//! separates two slots' arms runs at most once per activation, then within
//! every live range at most one of the two arms ran and at most one of the two
//! slots was ever touched.
//!
//! The loop test is what makes that second clause true, and it is a genuine
//! strengthening rather than bookkeeping: a conditional inside a `for` can take
//! arm A on one iteration and arm B on the next, so two arm-local slots there
//! *are* live at overlapping moments even though neither is named outside its
//! own arm.
//!
//! # Why the relation cannot be applied wrongly
//!
//! The same construction the region overlay uses. An [`ArmOverlayClass`] is a
//! set of slots that share one piece of storage, and its member list holds
//! [`permission::MayShareSlot`] values rather than slots. That type's field and
//! constructor are private to [`permission`], and the single expression that
//! builds one sits inside `permit`, which returns `None` unless
//! [`SlotHomes::never_concurrent`] holds against **every** member the class
//! already holds. No slot enters a class without the prover having answered yes
//! for every pair the class then contains.

use std::collections::BTreeMap;

use super::algorithm_code_scopes::{ScopePath, ScopeStep};

/// Where each slot of one region is live, by slot name.
///
/// A slot absent from this map has no home the projection could establish and
/// shares storage with nothing.
#[derive(Debug)]
pub(super) struct SlotHomes<'a> {
    home: BTreeMap<&'a str, ScopePath>,
}

impl<'a> SlotHomes<'a> {
    /// The homes as the placement analysis observed them.
    ///
    /// There is no closure to build and no precondition to fail on, unlike the
    /// call graph's acyclicity: exclusivity is decided pair by pair from the two
    /// homes alone, and a home this map does not hold is simply no evidence.
    pub(super) fn observed(home: BTreeMap<&'a str, ScopePath>) -> Self {
        Self { home }
    }

    /// Whether two slots can never be live at the same moment, and so whether
    /// they may share one piece of storage.
    ///
    /// This is the whole soundness question, answered in one place. Every
    /// answer that is not a positive proof is `false`: a slot this map does not
    /// know, and a slot compared with itself, share storage with nothing.
    fn never_concurrent(&self, left: &str, right: &str) -> bool {
        if left == right {
            return false;
        }
        match (self.home.get(left), self.home.get(right)) {
            (Some(left_home), Some(right_home)) => exclusive_arms(left_home, right_home),
            _ => false,
        }
    }
}

/// Whether two homes are arms of one conditional that is entered at most once
/// per activation of the owner.
///
/// See the module note for what each clause buys. The order matters only for
/// reading: the loop test guards the prefix the two homes share, and the
/// divergence test guards the step where they part.
fn exclusive_arms(left: &[ScopeStep], right: &[ScopeStep]) -> bool {
    let shared = left
        .iter()
        .zip(right)
        .take_while(|(step, other)| step == other)
        .count();
    if left
        .iter()
        .take(shared)
        .any(|step| matches!(step, ScopeStep::ForBody { .. }))
    {
        return false;
    }
    // One home a prefix of the other is not a divergence: the shorter one spans
    // the conditional the longer one sits inside.
    let (Some(here), Some(there)) = (left.get(shared).map(arm_of), right.get(shared).map(arm_of))
    else {
        return false;
    };
    match (here, there) {
        (Some((conditional, arm)), Some((other, other_arm))) => {
            conditional == other && arm != other_arm
        }
        _ => false,
    }
}

/// Which arm of which conditional a scope step selects: the statement index of
/// the conditional, and the arm within it, with the `else` spelled as `None`.
///
/// A loop body selects no arm of anything, so it answers `None` and can never
/// be half of an exclusion.
fn arm_of(step: &ScopeStep) -> Option<(usize, Option<usize>)> {
    match step {
        ScopeStep::IfBranch { statement, branch } => Some((*statement, Some(*branch))),
        ScopeStep::IfElse { statement } => Some((*statement, None)),
        ScopeStep::ForBody { .. } => None,
    }
}

/// Storage-sharing permission for slots, and the classes it builds.
///
/// Nothing outside this module can put a slot into a class: an
/// [`ArmOverlayClass`]'s member list holds [`MayShareSlot`] values whose field
/// and constructor are private here, and [`permit`] is the only expression that
/// builds one.
mod permission {
    use super::SlotHomes;

    /// Evidence that one slot may share storage with every slot already in one
    /// particular [`ArmOverlayClass`].
    ///
    /// # Theorem (a class is pairwise never-concurrent)
    ///
    /// Let `C` be an `ArmOverlayClass`. Then for every two distinct slots `a`
    /// and `b` in `C`, `homes.never_concurrent(a, b)` holds for the homes every
    /// call that built `C` was given.
    ///
    /// *Proof, by induction on the construction of `C`.* `C` is built only by
    /// [`ArmOverlayClass::founded_on`] and extended only by
    /// [`ArmOverlayClass::join`], because `members` is private to this module
    /// and its element type has no other constructor. `founded_on` yields a
    /// class of one slot, which contains no pair, so the claim holds vacuously.
    /// Suppose it holds for `C` and `join(homes, slot)` extends it. `join`
    /// pushes only the value [`permit`] returned, and `permit` returns `Some`
    /// only when `never_concurrent(member, slot)` holds for every `member` of
    /// `self.members`. So the claim holds for every pair involving `slot`, and
    /// by hypothesis for every pair not involving it.
    ///
    /// `never_concurrent` is symmetric, because `exclusive_arms` is symmetric
    /// in its two homes, so the order in which pairs were checked does not
    /// matter; and it is false for a slot against itself, so a slot cannot
    /// enter a class twice.
    struct MayShareSlot<'a> {
        slot: &'a str,
        /// Position of the slot in the region's declaration order, which is the
        /// order a target prints the class's members in.
        index: usize,
    }

    /// A set of slots that share one piece of storage: one union inside the
    /// region struct in the emitted C.
    pub(super) struct ArmOverlayClass<'a> {
        members: Vec<MayShareSlot<'a>>,
    }

    impl<'a> ArmOverlayClass<'a> {
        /// A class holding one slot alone.
        ///
        /// The permission is minted against no members and so is vacuous, which
        /// is exactly right: a class of one contains no pair to prove. It is
        /// still minted, because the member list admits nothing else.
        pub(super) fn founded_on(homes: &SlotHomes<'a>, slot: &'a str, index: usize) -> Self {
            Self {
                members: permit(homes, &[], slot, index).into_iter().collect(),
            }
        }

        /// Admit a slot if it may share this class's storage, and report
        /// whether it did.
        ///
        /// This is the only way a class ever gains a member, and the permission
        /// it consumes is minted here, against this class's own member list, in
        /// the same expression that pushes it. Nothing can run in between.
        pub(super) fn join(&mut self, homes: &SlotHomes<'a>, slot: &'a str, index: usize) -> bool {
            let Some(permission) = permit(homes, &self.members, slot, index) else {
                return false;
            };
            self.members.push(permission);
            true
        }

        /// The class's slot positions, in the order they were admitted.
        pub(super) fn indices(&self) -> impl Iterator<Item = usize> + '_ {
            self.members.iter().map(|permission| permission.index)
        }
    }

    /// The single minting site for [`MayShareSlot`].
    fn permit<'a>(
        homes: &SlotHomes<'a>,
        members: &[MayShareSlot<'a>],
        slot: &'a str,
        index: usize,
    ) -> Option<MayShareSlot<'a>> {
        members
            .iter()
            .all(|member| homes.never_concurrent(member.slot, slot))
            .then_some(MayShareSlot { slot, index })
    }
}

use permission::ArmOverlayClass;

/// Which slots of one region share storage with which others.
#[derive(Debug, Default)]
pub(super) struct ArmOverlay {
    /// Overlay ordinal of every slot that shares storage with at least one
    /// other, keyed by position in the region's declaration order. A slot
    /// absent from this map owns its storage outright and a target prints it as
    /// a plain struct member.
    shared: BTreeMap<usize, usize>,
}

impl ArmOverlay {
    /// The overlay `index` shares, or `None` for a slot that owns its storage.
    pub(super) fn overlay_of(&self, index: usize) -> Option<usize> {
        self.shared.get(&index).copied()
    }
}

/// Place a region's slots into arm-overlay classes, largest slot first.
///
/// `sized` pairs each slot's name with its bytes in the region's declaration
/// order, or `None` where an extent is not a literal and the projection cannot
/// size it.
///
/// # The policy, and what it is not
///
/// Which classes exist is a *policy* question; whether a class is sound is not.
/// Soundness is settled entirely by [`ArmOverlayClass`], so this function is
/// free to be a heuristic and cannot be free to be wrong.
///
/// **Descending size, first fit.** A class costs its largest member, so placing
/// the largest slot first makes that cost the founder's and every later member
/// rides along for nothing. First fit rather than the sibling module's best fit
/// because the two situations differ: admitting an owner into a region class
/// blocks that class for everything the owner reaches, so *which* class it
/// joins matters, while admitting a slot blocks its class only for the other
/// slots of its own arm, which every other class blocks equally.
///
/// **An unsizable slot is never overlaid.** Its region has no total, so every
/// target that prints regions fails closed on it long before a union would
/// matter, and leaving it alone keeps this function from choosing a layout on
/// the strength of a size it does not have.
///
/// A class of one is dropped rather than printed: a union with a single member
/// is the same storage as the member and only costs the reader a name. The
/// surviving classes are numbered by the earliest slot each holds, so the
/// emitted struct reads in declaration order.
pub(super) fn plan<'a>(homes: &SlotHomes<'a>, sized: &[(&'a str, Option<usize>)]) -> ArmOverlay {
    let mut order: Vec<(usize, &'a str, usize)> = sized
        .iter()
        .enumerate()
        .filter_map(|(index, (slot, bytes))| bytes.map(|bytes| (index, *slot, bytes)))
        .collect();
    order.sort_by(|left, right| right.2.cmp(&left.2).then_with(|| left.0.cmp(&right.0)));

    let mut classes: Vec<ArmOverlayClass<'a>> = Vec::new();
    for (index, slot, _) in order {
        // `join` re-asks the prover for every pair, so a class that will not
        // have this slot declines and the search moves on to the next.
        let mut landed = false;
        for class in &mut classes {
            landed = class.join(homes, slot, index);
            if landed {
                break;
            }
        }
        if !landed {
            classes.push(ArmOverlayClass::founded_on(homes, slot, index));
        }
    }

    let mut shared_classes: Vec<Vec<usize>> = classes
        .iter()
        .map(|class| class.indices().collect::<Vec<_>>())
        .filter(|members| members.len() > 1)
        .collect();
    shared_classes.sort_by_key(|members| members.iter().copied().min().unwrap_or(usize::MAX));
    let shared = shared_classes
        .into_iter()
        .enumerate()
        .flat_map(|(ordinal, members)| members.into_iter().map(move |index| (index, ordinal)))
        .collect();
    ArmOverlay { shared }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// One arm of the conditional at statement `statement`.
    fn arm(statement: usize, branch: usize) -> ScopeStep {
        ScopeStep::IfBranch { statement, branch }
    }

    fn otherwise(statement: usize) -> ScopeStep {
        ScopeStep::IfElse { statement }
    }

    fn loop_body(statement: usize) -> ScopeStep {
        ScopeStep::ForBody { statement }
    }

    fn homes_of<'a>(entries: &[(&'a str, ScopePath)]) -> SlotHomes<'a> {
        SlotHomes::observed(entries.iter().cloned().collect())
    }

    /// What the arm overlay is FOR: two slots in two arms of one `if/elseif`
    /// chain are never both touched, so one piece of storage holds either.
    #[test]
    fn two_arms_of_one_conditional_may_share_storage() {
        let homes = homes_of(&[("left", vec![arm(4, 0)]), ("right", vec![arm(4, 1)])]);
        assert!(homes.never_concurrent("left", "right"));
        assert!(homes.never_concurrent("right", "left"));

        let mut class = ArmOverlayClass::founded_on(&homes, "left", 0);
        assert!(class.join(&homes, "right", 1));
        assert_eq!(class.indices().collect::<Vec<_>>(), vec![0, 1]);
    }

    /// The `else` arm is an arm: the chain's final slot shares with the rest.
    #[test]
    fn the_else_arm_shares_with_the_branches() {
        let homes = homes_of(&[
            ("branch", vec![arm(4, 6)]),
            ("fallback", vec![otherwise(4)]),
        ]);
        assert!(homes.never_concurrent("branch", "fallback"));
    }

    /// NEGATIVE CONTROL. A slot read in two different arms has the enclosing
    /// scope as its home, and the prover must refuse it a shared slot: it is
    /// live across the whole conditional, so whichever arm runs, the other
    /// arm's slot would be overwriting a value this one still needs.
    #[test]
    fn the_prover_refuses_a_slot_used_in_two_arms() {
        let homes = homes_of(&[
            ("spans_the_chain", Vec::new()),
            ("arm_local", vec![arm(4, 1)]),
        ]);
        assert!(!homes.never_concurrent("spans_the_chain", "arm_local"));
        assert!(!homes.never_concurrent("arm_local", "spans_the_chain"));

        let mut class = ArmOverlayClass::founded_on(&homes, "arm_local", 0);
        assert!(!class.join(&homes, "spans_the_chain", 1));
        assert_eq!(
            class.indices().collect::<Vec<_>>(),
            vec![0],
            "a refused join must leave the class untouched"
        );
    }

    /// NEGATIVE CONTROL. A slot whose home merely *contains* the other's is
    /// still live across the inner conditional, so nesting is not exclusion.
    #[test]
    fn the_prover_refuses_an_enclosing_home() {
        let homes = homes_of(&[
            ("outer", vec![arm(4, 0)]),
            ("inner", vec![arm(4, 0), arm(2, 1)]),
        ]);
        assert!(!homes.never_concurrent("outer", "inner"));
        assert!(!homes.never_concurrent("inner", "outer"));
    }

    /// NEGATIVE CONTROL. Two arms of one conditional inside a `for` are not
    /// exclusive across the loop: arm A on one iteration and arm B on the next
    /// are both live inside a single activation of the owner.
    #[test]
    fn the_prover_refuses_arms_of_a_conditional_inside_a_loop() {
        let homes = homes_of(&[
            ("left", vec![loop_body(1), arm(0, 0)]),
            ("right", vec![loop_body(1), arm(0, 1)]),
        ]);
        assert!(!homes.never_concurrent("left", "right"));

        // The same two arms outside the loop are exactly what the overlay is
        // for, so the refusal above is the loop and nothing else.
        let unlooped = homes_of(&[("left", vec![arm(0, 0)]), ("right", vec![arm(0, 1)])]);
        assert!(unlooped.never_concurrent("left", "right"));
    }

    /// A loop *inside* an arm is no obstacle: the arm still runs at most once,
    /// so whatever the loop does to the slot stays inside that one run.
    #[test]
    fn a_loop_inside_an_arm_does_not_block_sharing() {
        let homes = homes_of(&[
            ("left", vec![arm(0, 0), loop_body(3)]),
            ("right", vec![arm(0, 1), loop_body(7)]),
        ]);
        assert!(homes.never_concurrent("left", "right"));
    }

    /// NEGATIVE CONTROL. Two arms of two *different* conditionals are not
    /// exclusive: consecutive statements both run.
    #[test]
    fn the_prover_refuses_arms_of_different_conditionals() {
        let homes = homes_of(&[("first", vec![arm(1, 0)]), ("second", vec![arm(2, 1)])]);
        assert!(!homes.never_concurrent("first", "second"));
    }

    /// A slot the projection could not place has no home, and no home is a
    /// refusal rather than a free pass.
    #[test]
    fn a_slot_without_a_home_shares_with_nothing() {
        let homes = homes_of(&[("known", vec![arm(0, 0)])]);
        assert!(!homes.never_concurrent("known", "stranger"));
        let mut class = ArmOverlayClass::founded_on(&homes, "known", 0);
        assert!(!class.join(&homes, "stranger", 1));
    }

    /// A class admits a slot only against EVERY member it already holds. Here
    /// `third` is exclusive with `first` but nested inside `second`'s arm.
    #[test]
    fn a_join_is_proven_against_every_member() {
        let homes = homes_of(&[
            ("first", vec![arm(0, 0)]),
            ("second", vec![arm(0, 1)]),
            ("third", vec![arm(0, 1), arm(5, 0)]),
        ]);
        let mut class = ArmOverlayClass::founded_on(&homes, "first", 0);
        assert!(class.join(&homes, "second", 1));
        assert!(
            !class.join(&homes, "third", 2),
            "`third` is exclusive with `first` but sits inside `second`'s arm"
        );
    }

    /// Descending-size placement is what turns the relation into small storage:
    /// the heavy slot founds the class and the compatible slots ride along. One
    /// arm holding two slots is what forces a second class open.
    #[test]
    fn the_largest_slot_founds_a_class_and_compatible_slots_ride_along() {
        let homes = homes_of(&[
            ("a_big", vec![arm(0, 0)]),
            ("a_small", vec![arm(0, 0)]),
            ("b_big", vec![arm(0, 1)]),
            ("b_small", vec![arm(0, 1)]),
        ]);
        let sized = [
            ("a_big", Some(900)),
            ("a_small", Some(12)),
            ("b_big", Some(900)),
            ("b_small", Some(16)),
        ];
        let overlay = plan(&homes, &sized);
        assert_eq!(overlay.overlay_of(0), Some(0));
        assert_eq!(overlay.overlay_of(2), Some(0), "the two big slots overlay");
        assert_eq!(overlay.overlay_of(1), Some(1));
        assert_eq!(
            overlay.overlay_of(3),
            Some(1),
            "and so do the two small ones"
        );
    }

    /// A slot that shares with nothing is left as a plain member rather than
    /// wrapped in a union of one.
    #[test]
    fn a_slot_that_shares_with_nothing_is_not_overlaid() {
        let homes = homes_of(&[
            ("spans_the_chain", Vec::new()),
            ("arm_local", vec![arm(0, 0)]),
        ]);
        let sized = [("spans_the_chain", Some(900)), ("arm_local", Some(900))];
        let overlay = plan(&homes, &sized);
        assert_eq!(overlay.overlay_of(0), None);
        assert_eq!(overlay.overlay_of(1), None);
    }

    /// A slot whose extent is not a literal is left out of the overlay: the
    /// policy never chooses a layout on the strength of a size it lacks.
    #[test]
    fn an_unsizable_slot_is_never_overlaid() {
        let homes = homes_of(&[
            ("measured", vec![arm(0, 0)]),
            ("unsizable", vec![arm(0, 1)]),
            ("other", vec![arm(0, 2)]),
        ]);
        let sized = [
            ("measured", Some(900)),
            ("unsizable", None),
            ("other", Some(900)),
        ];
        let overlay = plan(&homes, &sized);
        assert_eq!(overlay.overlay_of(1), None);
        assert_eq!(overlay.overlay_of(0), Some(0));
        assert_eq!(overlay.overlay_of(2), Some(0));
    }

    /// Placement is a pure function of the homes and the sizes, not of the
    /// order the caller happened to collect them in.
    #[test]
    fn overlay_ordinals_follow_declaration_order() {
        let homes = homes_of(&[
            ("late_big", vec![arm(0, 0)]),
            ("late_big_peer", vec![arm(0, 1)]),
            ("early_small", vec![arm(0, 0)]),
            ("early_small_peer", vec![arm(0, 1)]),
        ]);
        let sized = [
            ("late_big", Some(4)),
            ("late_big_peer", Some(4)),
            ("early_small", Some(900)),
            ("early_small_peer", Some(900)),
        ];
        let overlay = plan(&homes, &sized);
        assert_eq!(
            overlay.overlay_of(0),
            Some(0),
            "the earliest slot names overlay 0 however heavy it is"
        );
        assert_eq!(overlay.overlay_of(1), Some(0));
        assert_eq!(overlay.overlay_of(2), Some(1));
        assert_eq!(overlay.overlay_of(3), Some(1));
    }
}
