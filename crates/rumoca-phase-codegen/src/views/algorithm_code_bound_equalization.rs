//! How wide an overlaid slot is *declared*, where a narrower declaration lets a
//! C compiler read one union sibling's bound as another's.
//!
//! The two sibling modules beside this one decide **which** storage is shared:
//! `algorithm_code_overlay` across owners, `algorithm_code_slot_overlay` across
//! the arms of one conditional. Both are exact about lifetimes and say nothing
//! about declared extents, because nothing that executes depends on them: an
//! overlaid slot is written before it is read within one activation, and the
//! bytes past the shorter sibling's end are never touched. This module is about
//! the declaration a *reader* of the emitted header sees, and about the reader
//! that matters here: the C compiler's own object-size analysis.
//!
//! # The clash
//!
//! An overlay group is a union, so every region starts at the union's address
//! and two regions' slots can begin at the same byte offset holding arrays of
//! different lengths. That is sound storage. It is not a shape a compiler's
//! value numbering keeps apart: two address expressions with the same value are
//! unified into one, the survivor carries one particular access path, and the
//! object size read off that path is the surviving sibling's. When the survivor
//! is the shorter array and the address is handed to a parameter declared with
//! a longer bound (`const float q[4]`), the compiler reports a read past the
//! end of an object, on code that performs no such read.
//!
//! Concretely, on the RDD2 flight controller, `SE23_Quat_inverse` starts a `[3]`
//! and `SE23_Quat_product` starts a `[4]` at one offset of one group, both
//! functions inline into `LogLinear_stateError`, and the `[4]` handed to
//! `rotate`'s `const float[4]` is diagnosed against the `[3]`'s twelve bytes.
//!
//! # The rule
//!
//! At every **call-boundary bound-clash address** (an offset the emitted code
//! hands to a parameter declaring `N` bytes, where some region of the group
//! starts an array shorter than `N` there), every *other* array starting at
//! that offset is declared long enough to cover `N`.
//!
//! Three clauses carry their weight:
//!
//! * **`N` is the demanded bound, not the largest sibling.** An address whose
//!   siblings are all at least `N` cannot produce a short-object diagnostic
//!   whichever access path the compiler keeps, so a sibling already that long
//!   is left exactly as the block declared it. Equalizing up to the *largest*
//!   array at the offset would silence the same diagnostics and cost bytes no
//!   argument asks for.
//! * **Only the leading extent grows.** Every extent after the first is the
//!   element stride of the one before it, so widening the first changes no
//!   element's address; widening any other would move elements and is not a
//!   layout change at all.
//! * **The slot the boundary itself names is never widened.** If *it* is
//!   shorter than the bound it is handed to, the emitted code really does read
//!   past an object and the diagnostic is a true positive. This pass refuses
//!   the whole group in that case rather than growing the argument until the
//!   compiler stops asking; see [`equalize`]. The same clause leaves a slot
//!   with its own boundary demand at its declared extent even when a sibling
//!   at its offset demands more: widening it instead was tried and traded the
//!   silence for a fresh host-preflight report at SO3_Quat_product, so the
//!   narrower reading is the measured choice (see "What it does not close").
//!
//! # Why this is a layout change and not a semantic one
//!
//! Growing a declared extent is monotone in storage: every element the checked
//! block names keeps its address within its slot, and the slot keeps at least
//! the bytes it had. What moves is the offset of the slots *after* the grown
//! one inside the same region, which is what a struct layout is free to do; no
//! emitted statement names an offset. Nothing here shrinks a slot, reorders
//! members, or moves a region between groups, so neither overlay prover's
//! argument is touched.
//!
//! # What it does not close
//!
//! A **scalar** at a clash address cannot be equalized: there is no extent to
//! widen and inventing one would change the declared type. A group holding a
//! slot this projection cannot size is refused whole, because an offset model
//! over an unsizable member would be a guess. Both are refusals, and a refusal
//! here costs a diagnostic on generated code, never a wrong layout.
//!
//! An array whose **own address is handed to a shorter bound** is also left
//! as the block declared it, even when a sibling at its offset demands more
//! (`widen` skips any slot with its own call-boundary demand). The live
//! instance: ControllerScratch_attitudeControl.error at ControllerScratchGroup2
//! offset 0, 12 bytes beside from_DCM.q's 16. Removing the skip keeps the ARM
//! build clean but trips `SO3_Quat_product` reading 16 bytes from a region of
//! size 12 under the strict host preflight, so the narrower clause is a
//! measured choice, pinned by
//! `a_slot_with_its_own_shorter_demand_keeps_its_declared_extent`, not an
//! oversight.

use std::collections::BTreeMap;

/// One slot of one region, as this pass sees it.
///
/// Everything is in the storage model the layout accounting already uses: a
/// `Real` or `Integer` element is four bytes and four-byte aligned, a `Boolean`
/// is one byte and byte aligned, and an array's alignment is its element's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct SlotBounds {
    /// The slot's alignment: its scalar's width.
    pub(super) align: usize,
    /// Bytes in one slice of the leading dimension: every extent after the
    /// first, times the scalar's width. A scalar's slice is the scalar, and is
    /// never zero, so the division in [`widen`] is total.
    pub(super) stride: usize,
    /// The declared leading extent. One for a scalar.
    pub(super) leading: usize,
    /// Whether the slot is an array at all. A scalar has no extent to widen and
    /// is never grown, though it still occupies its offset.
    pub(super) is_array: bool,
    /// Bytes the emitted code needs at this slot's address because it hands
    /// that address to a parameter declared with that bound, and the largest such
    /// bound where one address reaches several parameters. `None` for a slot no
    /// call boundary names.
    pub(super) call_boundary_bytes: Option<usize>,
}

impl SlotBounds {
    /// Bytes the block declared this slot with.
    fn declared_bytes(self) -> usize {
        self.leading * self.stride
    }
}

/// The slots of one overlay group, addressed as `[region][member][slot]`.
///
/// A member is a union of one or more slots (one when the slot owns its
/// storage, more when the arm overlay put them together), so every slot of a
/// member shares the member's offset. A region is a struct of members, and the
/// group is a union of regions, so every region starts at offset zero.
pub(super) type GroupBounds = Vec<Vec<Vec<SlotBounds>>>;

/// The leading extent each slot is *declared* with, in the same shape as the
/// input. Equal to the input's `leading` everywhere the rule found nothing to
/// do.
pub(super) type GroupExtents = Vec<Vec<Vec<usize>>>;

/// How many times the rule is re-applied to its own output before this pass
/// gives up and equalizes nothing.
///
/// Widening a slot moves the members after it, which can bring a *different*
/// pair of siblings to one offset, so the rule has to be re-asked against the
/// layout it just produced. It settles: a slot's target is one of the finitely
/// many bounds the block's parameters declare, never a function of how wide the
/// pass has already made something, so no round can chase its own output
/// upwards. The cap is the guard against that reasoning being wrong on a shape
/// nobody has written yet, and hitting it returns the block's own declared
/// extents rather than a half-applied rule.
///
/// Eight is roughly twice what the pinned corpus asks for: the RDD2 controller
/// and navigation estimator settle after two widening rounds and the strapdown
/// UKF after three, each measured by lowering this constant until the emitted
/// header changed.
const ROUNDS: usize = 8;

/// Declare every array sharing a call-boundary bound-clash address long enough
/// for the bound that address is handed to.
///
/// Returns the leading extent of every slot. The result is the block's own
/// declared extents when the group holds no such address, when nothing there
/// clashes, when the iteration did not settle, or when a slot is *itself*
/// shorter than the bound it is handed to. The last of those is refused because
/// it is a genuine short read in the emitted code, and a layout pass that
/// widened the argument would be hiding it from the only tool that reports it.
pub(super) fn equalize(group: &GroupBounds) -> GroupExtents {
    let declared: GroupExtents = group
        .iter()
        .map(|region| {
            region
                .iter()
                .map(|member| member.iter().map(|slot| slot.leading).collect())
                .collect()
        })
        .collect();
    let short_argument = group.iter().flatten().flatten().any(|slot| {
        slot.call_boundary_bytes
            .is_some_and(|needed| slot.declared_bytes() < needed)
    });
    if short_argument {
        return declared;
    }
    let mut current = declared.clone();
    for _ in 0..ROUNDS {
        let next = widen(group, &current);
        if next == current {
            return current;
        }
        current = next;
    }
    declared
}

/// One application of the rule to one layout.
fn widen(group: &GroupBounds, extents: &GroupExtents) -> GroupExtents {
    let placed = place(group, extents);
    // The bytes an address is handed to a parameter as. Only these offsets are
    // call-boundary addresses; every other offset in the group is storage no
    // declared bound is ever read against.
    let mut demand: BTreeMap<usize, usize> = BTreeMap::new();
    for slot in &placed {
        if let Some(needed) = slot.bounds.call_boundary_bytes {
            let entry = demand.entry(slot.offset).or_insert(needed);
            *entry = (*entry).max(needed);
        }
    }
    let mut widened = extents.clone();
    for slot in &placed {
        // A scalar has no extent to widen, and the slot the boundary itself
        // names is the argument rather than a sibling of it.
        if !slot.bounds.is_array || slot.bounds.call_boundary_bytes.is_some() {
            continue;
        }
        let Some(target) = demand.get(&slot.offset).copied() else {
            continue;
        };
        if slot.bytes >= target {
            continue;
        }
        // The smallest leading extent whose slices cover `target`.
        let leading = target.div_ceil(slot.bounds.stride);
        let cell = &mut widened[slot.region][slot.member][slot.slot];
        *cell = (*cell).max(leading);
    }
    widened
}

/// One slot with the offset and size this layout gives it.
struct PlacedSlot {
    region: usize,
    member: usize,
    slot: usize,
    offset: usize,
    bytes: usize,
    bounds: SlotBounds,
}

/// Lay every region out and record where each slot lands.
///
/// Members follow one another in declaration order, each aligned to the widest
/// alignment among its own slots and sized by the largest of them; every slot
/// of a member shares the member's offset, because a member holding more than
/// one slot is a union. That is the target's own rule for these declarations,
/// which are arrays of scalars and nothing else.
fn place(group: &GroupBounds, extents: &GroupExtents) -> Vec<PlacedSlot> {
    let mut placed = Vec::new();
    for (region_index, region) in group.iter().enumerate() {
        let mut cursor = 0usize;
        for (member_index, member) in region.iter().enumerate() {
            let sizes: Vec<usize> = member
                .iter()
                .enumerate()
                .map(|(slot_index, slot)| {
                    extents[region_index][member_index][slot_index] * slot.stride
                })
                .collect();
            let align = member.iter().map(|slot| slot.align).max().unwrap_or(1);
            let size = sizes.iter().copied().max().unwrap_or(0);
            let offset = cursor.next_multiple_of(align);
            for (slot_index, slot) in member.iter().enumerate() {
                placed.push(PlacedSlot {
                    region: region_index,
                    member: member_index,
                    slot: slot_index,
                    offset,
                    bytes: sizes[slot_index],
                    bounds: *slot,
                });
            }
            cursor = offset + size;
        }
    }
    placed
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A `Real` array slot `[leading][…]` whose slice is `rest` elements wide,
    /// with no call boundary on it.
    fn real(leading: usize, rest: usize) -> SlotBounds {
        SlotBounds {
            align: 4,
            stride: 4 * rest,
            leading,
            is_array: true,
            call_boundary_bytes: None,
        }
    }

    /// A `Real` scalar slot.
    fn scalar() -> SlotBounds {
        SlotBounds {
            align: 4,
            stride: 4,
            leading: 1,
            is_array: false,
            call_boundary_bytes: None,
        }
    }

    /// The same slot, with its address handed to a parameter declaring `bytes`.
    fn handed(slot: SlotBounds, bytes: usize) -> SlotBounds {
        SlotBounds {
            call_boundary_bytes: Some(bytes),
            ..slot
        }
    }

    /// Each slot on its own member, which is what a region with no arm overlay
    /// looks like.
    fn plain(slots: &[SlotBounds]) -> Vec<Vec<SlotBounds>> {
        slots.iter().map(|slot| vec![*slot]).collect()
    }

    /// The offset and size this layout gives every slot, region by region: the
    /// byte-level statement a test can hold the pass to.
    fn layout(group: &GroupBounds, extents: &GroupExtents) -> Vec<(usize, usize)> {
        place(group, extents)
            .iter()
            .map(|slot| (slot.offset, slot.bytes))
            .collect()
    }

    /// WHAT THE PASS IS FOR, in the controller's own shape. Two regions of one
    /// group start differently sized arrays at one offset and the longer one is
    /// handed to a `[4]` parameter, so the shorter is declared `[4]` too.
    #[test]
    fn a_clash_at_a_call_boundary_address_is_equalized() {
        let group: GroupBounds = vec![
            plain(&[real(4, 1), real(3, 1)]),
            plain(&[real(4, 1), handed(real(4, 1), 16)]),
        ];
        let extents = equalize(&group);
        assert_eq!(
            extents,
            vec![vec![vec![4], vec![4]], vec![vec![4], vec![4]]],
            "the [3] sharing the call-boundary address must be declared [4]"
        );
        assert_eq!(
            layout(&group, &extents),
            vec![(0, 16), (16, 16), (0, 16), (16, 16)],
            "and both regions must then carry sixteen bytes at offset 16"
        );
    }

    /// NEGATIVE CONTROL. With the call boundary removed and nothing else
    /// changed, the very same clash is left standing, which is the coordinate
    /// the compiler's diagnostic fires at, so a pass that stopped widening here
    /// would reintroduce exactly the report this one retires.
    #[test]
    fn removing_the_call_boundary_reintroduces_the_clash() {
        let group: GroupBounds = vec![
            plain(&[real(4, 1), real(3, 1)]),
            plain(&[real(4, 1), real(4, 1)]),
        ];
        let extents = equalize(&group);
        assert_eq!(
            extents,
            vec![vec![vec![4], vec![3]], vec![vec![4], vec![4]]],
            "without a call boundary at the address there is nothing to equalize"
        );
        assert_eq!(
            layout(&group, &extents),
            vec![(0, 16), (16, 12), (0, 16), (16, 16)],
            "offset 16 still starts a twelve-byte array beside a sixteen-byte one"
        );
    }

    /// A group whose call-boundary addresses are covered by every sibling is
    /// not a clash, and the pass must return the declared layout byte for byte,
    /// including the siblings that are LARGER than the demanded bound: those
    /// can never produce a short-object diagnostic and so must not be touched.
    #[test]
    fn a_clash_free_group_is_untouched() {
        let group: GroupBounds = vec![
            plain(&[real(4, 1), handed(real(3, 1), 12)]),
            plain(&[real(9, 1), real(3, 1)]),
            plain(&[handed(real(4, 1), 16), real(9, 1)]),
        ];
        let extents = equalize(&group);
        assert_eq!(
            extents,
            vec![
                vec![vec![4], vec![3]],
                vec![vec![9], vec![3]],
                vec![vec![4], vec![9]]
            ],
            "every array at a boundary address already covers its bound"
        );
        assert_eq!(
            layout(&group, &extents),
            vec![(0, 16), (16, 12), (0, 36), (36, 12), (0, 16), (16, 36)],
            "no offset and no size may move in a clash-free group"
        );
    }

    /// A slot whose own address is handed a shorter bound keeps its declared
    /// extent even though its sibling at the same offset demands more: the
    /// exception is a measured decision the suite owns, not an accident of
    /// the widening loop's skip.
    #[test]
    fn a_slot_with_its_own_shorter_demand_keeps_its_declared_extent() {
        let group: GroupBounds = vec![
            plain(&[handed(real(3, 1), 12)]),
            plain(&[handed(real(4, 1), 16)]),
        ];
        let extents = equalize(&group);
        assert_eq!(
            extents,
            vec![vec![vec![3]], vec![vec![4]]],
            "the self-demanded shorter slot stays as the block declared it"
        );
        assert_eq!(
            layout(&group, &extents),
            vec![(0, 12), (0, 16)],
            "the clash coordinates remain, by decision rather than omission"
        );
    }

    /// Only the leading extent moves: a `[3][3]` sharing an address handed a
    /// forty-byte bound becomes `[4][3]`, so every element keeps its address
    /// and the slot merely covers more bytes.
    #[test]
    fn only_the_leading_extent_of_a_matrix_grows() {
        let group: GroupBounds = vec![plain(&[real(3, 3)]), plain(&[handed(real(10, 1), 40)])];
        let extents = equalize(&group);
        assert_eq!(
            extents,
            vec![vec![vec![4]], vec![vec![10]]],
            "a [3][3] must cover forty bytes as [4][3], never as [3][4]"
        );
        assert_eq!(layout(&group, &extents), vec![(0, 48), (0, 40)]);
    }

    /// A scalar at a clash address has no extent to widen. The arrays there are
    /// still equalized, and the scalar is left alone rather than becoming one.
    #[test]
    fn a_scalar_at_a_clash_address_is_left_alone() {
        let group: GroupBounds = vec![
            plain(&[scalar()]),
            plain(&[real(3, 1)]),
            plain(&[handed(real(4, 1), 16)]),
        ];
        assert_eq!(
            equalize(&group),
            vec![vec![vec![1]], vec![vec![4]], vec![vec![4]]],
            "the scalar keeps its single element; the arrays agree on four"
        );
    }

    /// Slots the arm overlay put in one member share that member's offset, so
    /// they are siblings in exactly the sense this rule is about, inside a
    /// single region.
    #[test]
    fn arm_overlay_siblings_are_equalized_against_one_another() {
        let group: GroupBounds = vec![vec![vec![real(3, 1), handed(real(4, 1), 16)]]];
        assert_eq!(
            equalize(&group),
            vec![vec![vec![4, 4]]],
            "two slots of one arm-overlay union start at the same offset"
        );
    }

    /// Widening moves the members after it, so the rule is re-asked against its
    /// own output: the first round closes the clash at offset zero and thereby
    /// produces the layout at which the second round's clash is visible.
    #[test]
    fn the_rule_is_reapplied_to_its_own_output() {
        let group: GroupBounds = vec![
            plain(&[real(3, 1), handed(real(4, 1), 16)]),
            plain(&[handed(real(4, 1), 16), real(3, 1), real(4, 1)]),
        ];
        assert_eq!(
            equalize(&group),
            vec![vec![vec![4], vec![4]], vec![vec![4], vec![4], vec![4]]],
            "round one moves region 0's boundary slot from offset 12 to 16, \
             where round two finds region 1's [3]"
        );
    }

    /// FAIL CLOSED. A slot shorter than the bound its own address is handed to
    /// is a genuine short read in the emitted code. The pass must decline the
    /// whole group rather than widen the argument until the compiler stops
    /// reporting it.
    #[test]
    fn a_short_argument_refuses_the_whole_group() {
        let group: GroupBounds = vec![
            plain(&[real(3, 1), handed(real(3, 1), 16)]),
            plain(&[real(4, 1), real(3, 1)]),
        ];
        assert_eq!(
            equalize(&group),
            vec![vec![vec![3], vec![3]], vec![vec![4], vec![3]]],
            "the group is returned exactly as the block declared it"
        );
    }

    /// An empty group is a block with no working memory, and asking this pass
    /// about it must not be a special case anywhere upstream.
    #[test]
    fn an_empty_group_equalizes_to_nothing() {
        assert_eq!(equalize(&Vec::new()), Vec::<Vec<Vec<usize>>>::new());
    }
}
