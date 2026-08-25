//! Which slots *inside one working-memory region* may share the region's flat
//! value arena, and the evidence that they may.
//!
//! The sibling module `algorithm_code_overlay` decides which **regions** share
//! storage; `algorithm_code_slot_overlay` shares slots that live in mutually
//! exclusive arms of one conditional. Both are blind to the commonest waste in
//! a long straight-line body: two 15x15 temporaries whose uses are entirely
//! sequential, the first dead before the second is born, each holding 900
//! bytes for the whole call. This module is the layer that sees that. Its unit
//! is the **slot**, its storage is one flat `float` array per region (the
//! region's *value arena*), and its relation is decided from where in the
//! owner's printed statements each slot is named.
//!
//! # The relation
//!
//! Every use of a slot is recorded as a [`SlotUse`]: the chain of scopes from
//! the owner's body down to the statement that names it, plus that statement's
//! index inside its innermost scope. Two slots may share arena storage when
//! [`SlotUses::never_concurrent`] proves their uses can never be live at the
//! same moment, by walking both use sets down the scope tree together:
//!
//! * at each depth, each slot's uses span an interval of statement indices;
//!   **disjoint intervals are a proof**: the body of a scope that is entered
//!   at most once per activation runs top to bottom exactly once, so every use
//!   of one slot is sequenced before every use of the other, and working
//!   memory carries nothing between activations (the region overlay already
//!   rests on that: any never-concurrent owner may run in between and
//!   overwrite everything);
//! * intervals that overlap on more than a single shared statement are a
//!   refusal: both slots are named while the other may be live;
//! * both confined to one statement, entered through **different arms of that
//!   statement's conditional**, is a proof: at most one arm runs per entry.
//!   This is the exclusive-arms relation of `algorithm_code_slot_overlay`,
//!   reproved here so one prover carries the whole arena;
//! * both confined to the same arm of that statement descends and asks again
//!   one scope deeper;
//! * both confined to one **`for` body is a refusal**, always. Inside a loop,
//!   statement order within one iteration says nothing about liveness: a value
//!   written late in iteration `k` may be read early in iteration `k + 1`, so
//!   neither the interval argument nor the arm argument survives the back
//!   edge. The descent therefore never steps into a loop body, which keeps
//!   every interval it does compare inside code that runs at most once per
//!   activation. A loop *inside* one slot's interval is no obstacle: the whole
//!   loop statement is simply part of that interval.
//!
//! Everything that is not one of those positive proofs is a refusal, including
//! a slot with no recorded use and a slot compared against itself.
//!
//! # Why the relation cannot be applied wrongly
//!
//! The same construction the two sibling provers use. An [`ArenaLayout`]'s
//! placement list holds [`permission::PlacedSlot`] values rather than names.
//! That type's fields and constructor are private to [`permission`], and the
//! single expression that builds one sits inside `admit`, which returns `None`
//! unless [`SlotUses::never_concurrent`] holds against **every** already
//! placed slot whose byte range overlaps the candidate's. There is therefore
//! no path by which two slots come to overlap in the arena without the prover
//! having answered yes for that pair.
//!
//! # One shape per call-boundary offset
//!
//! Liveness alone would let any two disjoint slots share an offset. The
//! placement adds one more requirement, enforced where the permission is
//! minted rather than merely preferred by the policy: when either of two slots
//! sharing an offset is a **call-boundary** slot, whose address the emitted
//! code hands to a declared array parameter, the two must have **identical
//! declared extents**.
//!
//! The reason is the C the offsets are emitted into, and it is the same
//! phenomenon `algorithm_code_bound_equalization` exists for one level up.
//! Each slot is reached through a pointer to its own array type, and an
//! address that two different array types both name is an address a compiler's
//! value numbering may unify: the survivor carries one access path, and an
//! object size read off that path is the surviving type's, not the requesting
//! one's. A `[3][3]` and a `[4]` at one offset is exactly that shape, and the
//! argument handed from it to a `const float[4]` formal is then diagnosed as a
//! read past the end of an object the code never performs.
//!
//! The restriction is limited to call-boundary slots because that is where the
//! question is asked: an offset no declared bound is ever handed produces no
//! object-size query, so two shapes there cost nothing. Nothing about values is
//! at stake either way. Every slot is a `float` array written before it is read
//! inside its own live range, so no read ever observes bytes another type
//! wrote; what a mixed-shape call-boundary offset costs is a diagnostic on
//! correct code, which is a thing this compiler refuses to emit.
//!
//! This is a refusal, so it can only cost bytes.
//!
//! # Alignment
//!
//! The arena is a `float` array and every resident is a `Real` slot, so every
//! offset that is a multiple of [`ARENA_ALIGN`] bytes is naturally aligned for
//! every access the emitted code performs. [`plan`] rounds every candidate
//! offset up to that multiple and refuses any slot whose size is not one. If a
//! wider scalar is ever admitted, its offsets must be rounded to a multiple of
//! its own alignment as well; the refusal is what forces that decision to be
//! taken here rather than inherited silently.

use std::collections::BTreeMap;

use super::algorithm_code_scopes::{ScopePath, ScopeStep};

/// Bytes of one arena element, and the alignment every placement keeps: the
/// width of the `float` the arena is declared over.
pub(super) const ARENA_ALIGN: usize = 4;

/// One place a region slot is named in the statements a C target prints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct SlotUse {
    /// The scopes entered on the way to the statement, outermost first.
    pub(super) scopes: ScopePath,
    /// Index of the naming statement inside the innermost scope.
    pub(super) statement: usize,
}

/// The statement index a use occupies at `depth` of its scope chain: the
/// statement whose sub-scope the use sits in, or the naming statement itself
/// once the chain is exhausted.
fn index_at(place: &SlotUse, depth: usize) -> usize {
    match place.scopes.get(depth) {
        Some(ScopeStep::IfBranch { statement, .. })
        | Some(ScopeStep::IfElse { statement })
        | Some(ScopeStep::ForBody { statement }) => *statement,
        None => place.statement,
    }
}

/// The scope step every use of one slot takes at `depth`, or `None` when any
/// use ends at this depth or the uses take different steps. `None` is always a
/// refusal for the caller: a slot that touches the statement itself, or more
/// than one of its arms, is live across the whole statement.
fn common_step(uses: &[SlotUse], depth: usize) -> Option<&ScopeStep> {
    let mut steps = uses.iter().map(|place| place.scopes.get(depth));
    let first = steps.next().flatten()?;
    steps
        .all(|step| step.is_some_and(|step| step == first))
        .then_some(first)
}

/// Which arm of which conditional a scope step selects, exactly as the arm
/// overlay prover reads it: the statement index of the conditional and the arm
/// within it, `else` spelled as `None`. A loop body selects no arm and can
/// never be half of an exclusion.
fn arm_of(step: &ScopeStep) -> Option<(usize, Option<usize>)> {
    match step {
        ScopeStep::IfBranch { statement, branch } => Some((*statement, Some(*branch))),
        ScopeStep::IfElse { statement } => Some((*statement, None)),
        ScopeStep::ForBody { .. } => None,
    }
}

/// Whether two use sets can never be live at the same moment, compared at
/// `depth` of their scope chains. See the module note for each clause.
///
/// Terminates because every recursive call increases `depth`, and a use whose
/// chain is exhausted makes `common_step` answer `None`, which ends in a
/// refusal.
fn disjoint(left: &[SlotUse], right: &[SlotUse], depth: usize) -> bool {
    let span = |uses: &[SlotUse]| {
        let mut indices = uses.iter().map(|place| index_at(place, depth));
        let first = indices.next()?;
        Some(indices.fold((first, first), |(low, high), index| {
            (low.min(index), high.max(index))
        }))
    };
    let (Some((left_low, left_high)), Some((right_low, right_high))) = (span(left), span(right))
    else {
        return false;
    };
    // The interval proof: one slot's every use is sequenced before the
    // other's first, inside a scope that runs at most once per activation.
    if left_high < right_low || right_high < left_low {
        return true;
    }
    // Overlapping intervals separate only when both collapse to one shared
    // statement whose sub-scopes can still tell them apart.
    if left_low != left_high || right_low != right_high || left_low != right_low {
        return false;
    }
    let (Some(left_step), Some(right_step)) = (common_step(left, depth), common_step(right, depth))
    else {
        return false;
    };
    if left_step == right_step {
        return match left_step {
            // The back edge of a loop orders nothing across iterations; no
            // clause below survives it, so the descent stops here.
            ScopeStep::ForBody { .. } => false,
            ScopeStep::IfBranch { .. } | ScopeStep::IfElse { .. } => {
                disjoint(left, right, depth + 1)
            }
        };
    }
    // Two different steps of one statement: exclusive arms of its
    // conditional are a proof, anything else a refusal.
    match (arm_of(left_step), arm_of(right_step)) {
        (Some((conditional, arm)), Some((other, other_arm))) => {
            conditional == other && arm != other_arm
        }
        _ => false,
    }
}

/// Where each slot of one region is named, by slot name.
///
/// A slot absent from this map has no use the projection could place and
/// shares storage with nothing.
#[derive(Debug)]
pub(super) struct SlotUses<'a> {
    uses: BTreeMap<&'a str, Vec<SlotUse>>,
}

impl<'a> SlotUses<'a> {
    /// The uses as the emission-faithful walk observed them.
    ///
    /// The walk is the caller's: it must record a use for every statement the
    /// target *prints* a slot's name in, kernel rewrites included. Recording a
    /// use the target does not print only widens a live range, so the walk is
    /// free to over-approximate and never to miss.
    pub(super) fn observed(uses: BTreeMap<&'a str, Vec<SlotUse>>) -> Self {
        Self { uses }
    }

    /// Whether two slots can never be live at the same moment, and so whether
    /// their arena byte ranges may overlap.
    ///
    /// This is the whole soundness question, answered in one place. Every
    /// answer that is not a positive proof is `false`: a slot this map does
    /// not know, and a slot compared with itself, share storage with nothing.
    pub(super) fn never_concurrent(&self, left: &str, right: &str) -> bool {
        if left == right {
            return false;
        }
        match (self.uses.get(left), self.uses.get(right)) {
            (Some(left_uses), Some(right_uses)) => disjoint(left_uses, right_uses, 0),
            _ => false,
        }
    }

    /// Whether the walk recorded any use of `name` at all: the eligibility
    /// gate for entering the arena. A slot without one is not *unused*, it is
    /// unplaceable, and unplaceable fails closed into a plain member.
    pub(super) fn knows(&self, name: &str) -> bool {
        self.uses.contains_key(name)
    }
}

/// Arena-placement permission, and the layout it builds.
///
/// Nothing outside this module can put a slot at an arena offset: an
/// [`ArenaLayout`]'s list holds [`PlacedSlot`] values whose fields and
/// constructor are private here, and [`admit`](ArenaLayout::admit) holds the
/// only expression that builds one.
mod permission {
    use super::SlotUses;

    /// Evidence that one slot may occupy `[offset, offset + bytes)` beside
    /// every slot already placed in one particular [`ArenaLayout`].
    ///
    /// # Theorem (overlapping placements are pairwise never-concurrent)
    ///
    /// Let `L` be an `ArenaLayout`. Then for every two distinct placed slots
    /// `a` and `b` in `L` whose byte ranges overlap,
    /// `uses.never_concurrent(a, b)` holds for the uses every call that built
    /// `L` was given.
    ///
    /// *Proof, by induction on the construction of `L`.* `L` starts empty and
    /// gains members only through [`ArenaLayout::admit`], because `placed` is
    /// private to this module and its element type has no other constructor.
    /// An empty layout contains no pair, so the claim holds vacuously.
    /// Suppose it holds for `L` and `admit(uses, candidate, …)` extends it.
    /// `admit` pushes only when `never_concurrent(member, candidate)` held for
    /// every placed `member` whose range overlaps the new one, so the claim
    /// holds for every overlapping pair involving the candidate, and by
    /// hypothesis for every pair not involving it. The relation is symmetric
    /// (the interval and arm clauses are symmetric in their two operands), so
    /// the order pairs were checked in does not matter.
    ///
    /// The same induction carries the shape invariant: `admit` also requires
    /// [`shapes_may_meet`](super::shapes_may_meet) against every overlapping
    /// member, so every two placements at overlapping offsets in `L` either
    /// have identical extents or are both free of any call boundary. That is
    /// what keeps every offset a C compiler is asked about a single type; see
    /// the module note.
    #[derive(Debug)]
    pub(super) struct PlacedSlot<'a> {
        name: &'a str,
        /// Position of the slot in the region's declaration order.
        index: usize,
        offset: usize,
        bytes: usize,
        /// The declared extents, kept so an overlapping admission at a call
        /// boundary can be refused unless the two shapes agree.
        extents: Vec<usize>,
        /// Whether this slot's address reaches a declared array parameter.
        call_boundary: bool,
    }

    impl<'a> PlacedSlot<'a> {
        pub(super) fn name(&self) -> &'a str {
            self.name
        }

        pub(super) fn index(&self) -> usize {
            self.index
        }

        pub(super) fn offset(&self) -> usize {
            self.offset
        }

        pub(super) fn bytes(&self) -> usize {
            self.bytes
        }

        pub(super) fn extents(&self) -> &[usize] {
            &self.extents
        }

        pub(super) fn call_boundary(&self) -> bool {
            self.call_boundary
        }
    }

    /// One region's arena as it is being laid out.
    #[derive(Debug, Default)]
    pub(super) struct ArenaLayout<'a> {
        placed: Vec<PlacedSlot<'a>>,
    }

    impl<'a> ArenaLayout<'a> {
        /// Admit `name` at `[offset, offset + bytes)` if the prover certifies
        /// it against every placed slot that range overlaps, and report
        /// whether it did.
        ///
        /// This is the only way the layout ever gains a member, and the check
        /// runs here, against this layout's own list, in the same expression
        /// that pushes the result. Nothing can run in between.
        pub(super) fn admit(
            &mut self,
            uses: &SlotUses<'a>,
            candidate: &super::ArenaCandidate<'a>,
            offset: usize,
            bytes: usize,
        ) -> bool {
            let permitted = self
                .placed
                .iter()
                .filter(|other| {
                    offset < other.offset + other.bytes && other.offset < offset + bytes
                })
                .all(|other| {
                    uses.never_concurrent(other.name, candidate.name)
                        && super::shapes_may_meet(
                            other.extents.as_slice(),
                            other.call_boundary,
                            candidate.extents.as_slice(),
                            candidate.call_boundary,
                        )
                });
            if permitted {
                self.placed.push(PlacedSlot {
                    name: candidate.name,
                    index: candidate.index,
                    offset,
                    bytes,
                    extents: candidate.extents.clone(),
                    call_boundary: candidate.call_boundary,
                });
            }
            permitted
        }

        pub(super) fn placed(&self) -> &[PlacedSlot<'a>] {
            &self.placed
        }

        /// One past the last occupied byte: the arena's size so far.
        pub(super) fn end(&self) -> usize {
            self.placed
                .iter()
                .map(|slot| slot.offset + slot.bytes)
                .max()
                .unwrap_or(0)
        }
    }
}

use permission::ArenaLayout;

/// One slot offered to the placement.
#[derive(Debug, Clone)]
pub(super) struct ArenaCandidate<'a> {
    /// The slot's source name, as [`SlotUses`] knows it.
    pub(super) name: &'a str,
    /// Position in the region's declaration order, the key the plan reports.
    pub(super) index: usize,
    /// Storage the slot needs, or `None` where the projection cannot size it.
    pub(super) bytes: Option<usize>,
    /// The extents the slot is declared with. Two slots share an offset only
    /// when these are equal, unless neither is a call-boundary slot; see the
    /// module note.
    pub(super) extents: Vec<usize>,
    /// Whether the emitted code hands this slot's address to a declared array
    /// parameter. A call boundary is where a C compiler asks how large the
    /// object at an address is, so it is where two shapes at one offset stop
    /// being free.
    pub(super) call_boundary: bool,
}

/// Where each arena slot landed: byte offsets keyed by position in the
/// region's declaration order, and the arena's total size.
#[derive(Debug)]
pub(super) struct ArenaPlan {
    /// Byte offset of every placed slot, keyed by declaration-order index.
    pub(super) offsets: BTreeMap<usize, usize>,
    /// Total arena bytes: one past the highest occupied byte.
    pub(super) bytes: usize,
}

/// Whether two slots may occupy overlapping bytes as far as the emitted C's
/// object-size analysis is concerned.
///
/// Identical extents always may: one offset then names one type. Different
/// extents may only when neither slot's address reaches a declared array
/// parameter, because that is the only place the size of the object at an
/// address is asked about. See the module note.
fn shapes_may_meet(
    left: &[usize],
    left_boundary: bool,
    right: &[usize],
    right_boundary: bool,
) -> bool {
    left == right || !(left_boundary || right_boundary)
}

/// Round `offset` up to the arena's element alignment.
fn align_up(offset: usize) -> usize {
    offset.div_ceil(ARENA_ALIGN) * ARENA_ALIGN
}

/// Place a region's arena-eligible slots into one flat arena, largest slot
/// first.
///
/// `sized` offers each eligible slot with its bytes and its declared extents.
/// The caller has already filtered eligibility (a `Real` array with literal
/// extents whose size is a multiple of [`ARENA_ALIGN`], not an output the
/// caller reads back, with at least one recorded use); a slot whose size is
/// `None` or misaligned is skipped here as a second fail-closed gate.
///
/// # The policy, and what it is not
///
/// Which offsets are chosen is a *policy* question; whether an overlap is
/// sound is not. Soundness is settled entirely by [`ArenaLayout`], so this
/// function is free to be a heuristic and cannot be free to be wrong.
///
/// **Descending size, lowest fitting offset.** Slots are placed largest first
/// (declaration order breaking ties), each at the lowest aligned offset whose
/// every already-placed overlap the prover certifies. Largest first is the
/// same reasoning the sibling planners use: the big slots found the layout and
/// the small ones ride into its gaps. The result is deterministic in the
/// region's declaration order and sizes alone.
///
/// The chooser scans gaps between the *conflicting* placements only (those the
/// prover refuses against the candidate, and those of a different declared
/// shape); [`ArenaLayout::admit`] then re-checks the chosen offset against
/// every overlapping placement. If the two ever disagreed, the candidate falls
/// back to the aligned end of the arena, where it overlaps nothing and the
/// admission is vacuous: a chooser defect costs bytes, never soundness.
pub(super) fn plan<'a>(uses: &SlotUses<'a>, sized: &[ArenaCandidate<'a>]) -> ArenaPlan {
    let mut order: Vec<(&ArenaCandidate<'a>, usize)> = sized
        .iter()
        .filter_map(|candidate| candidate.bytes.map(|bytes| (candidate, bytes)))
        .filter(|(_, bytes)| *bytes > 0 && bytes % ARENA_ALIGN == 0)
        .collect();
    order.sort_by(|left, right| {
        right
            .1
            .cmp(&left.1)
            .then_with(|| left.0.index.cmp(&right.0.index))
    });

    let mut layout = ArenaLayout::default();
    for (candidate, bytes) in order {
        // Offsets this slot must avoid: the ranges of every placed slot the
        // prover refuses to overlap it with, and of every one whose declared
        // shape differs from this one's.
        let mut forbidden: Vec<(usize, usize)> = layout
            .placed()
            .iter()
            .filter(|other| {
                !uses.never_concurrent(other.name(), candidate.name)
                    || !shapes_may_meet(
                        other.extents(),
                        other.call_boundary(),
                        candidate.extents.as_slice(),
                        candidate.call_boundary,
                    )
            })
            .map(|other| (other.offset(), other.offset() + other.bytes()))
            .collect();
        forbidden.sort_unstable();
        let mut offset = 0usize;
        for (start, end) in forbidden {
            if offset + bytes <= start {
                break;
            }
            offset = align_up(offset.max(end));
        }
        if !layout.admit(uses, candidate, offset, bytes) {
            let fallback = align_up(layout.end());
            let landed = layout.admit(uses, candidate, fallback, bytes);
            debug_assert!(landed, "an offset past every placement overlaps nothing");
            if !landed {
                continue;
            }
        }
    }

    let offsets = layout
        .placed()
        .iter()
        .map(|slot| (slot.index(), slot.offset()))
        .collect();
    ArenaPlan {
        offsets,
        bytes: layout.end(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(statement: usize) -> SlotUse {
        SlotUse {
            scopes: Vec::new(),
            statement,
        }
    }

    fn within(scopes: &[ScopeStep], statement: usize) -> SlotUse {
        SlotUse {
            scopes: scopes.to_vec(),
            statement,
        }
    }

    fn arm(statement: usize, branch: usize) -> ScopeStep {
        ScopeStep::IfBranch { statement, branch }
    }

    fn otherwise(statement: usize) -> ScopeStep {
        ScopeStep::IfElse { statement }
    }

    fn loop_body(statement: usize) -> ScopeStep {
        ScopeStep::ForBody { statement }
    }

    fn uses_of<'a>(entries: &[(&'a str, Vec<SlotUse>)]) -> SlotUses<'a> {
        SlotUses::observed(entries.iter().cloned().collect())
    }

    /// One candidate of `bytes` bytes, declared as a one-dimensional array of
    /// that many floats. Placement fixtures that mean to exercise sharing use
    /// this, so every candidate carries the same shape for a given size and
    /// the shape rule never silently decides the outcome.
    fn candidate<'a>(name: &'a str, index: usize, bytes: usize) -> ArenaCandidate<'a> {
        ArenaCandidate {
            name,
            index,
            bytes: Some(bytes),
            extents: vec![bytes / ARENA_ALIGN],
            call_boundary: false,
        }
    }

    /// A candidate the projection could not size.
    fn unsized_candidate<'a>(name: &'a str, index: usize) -> ArenaCandidate<'a> {
        ArenaCandidate {
            name,
            index,
            bytes: None,
            extents: Vec::new(),
            call_boundary: false,
        }
    }

    /// What the arena is FOR: two temporaries whose uses are entirely
    /// sequential are never live together, so one piece of storage holds
    /// either.
    #[test]
    fn sequential_temporaries_may_share_storage() {
        let uses = uses_of(&[("early", vec![at(2), at(5)]), ("late", vec![at(6), at(9)])]);
        assert!(uses.never_concurrent("early", "late"));
        assert!(uses.never_concurrent("late", "early"));
    }

    /// NEGATIVE CONTROL. Interleaved uses overlap in time; the prover must
    /// refuse, because between `long`'s first and last use a shared slot would
    /// overwrite a value it still holds.
    #[test]
    fn the_prover_refuses_interleaved_uses() {
        let uses = uses_of(&[("long", vec![at(2), at(9)]), ("inside", vec![at(5), at(6)])]);
        assert!(!uses.never_concurrent("long", "inside"));
        assert!(!uses.never_concurrent("inside", "long"));
    }

    /// Touching at one statement is a refusal: a statement that reads one slot
    /// and writes the other holds both at once.
    #[test]
    fn the_prover_refuses_uses_meeting_at_one_statement() {
        let uses = uses_of(&[("source", vec![at(2), at(5)]), ("target", vec![at(5)])]);
        assert!(!uses.never_concurrent("source", "target"));
    }

    /// A use inside a nested scope occupies the enclosing statement: a loop
    /// inside one slot's interval is part of that interval, and sequencing
    /// against the whole loop still separates the pair.
    #[test]
    fn a_nested_use_occupies_its_enclosing_statement() {
        let uses = uses_of(&[
            ("looped", vec![within(&[loop_body(3)], 0), at(4)]),
            ("later", vec![at(5), at(7)]),
        ]);
        assert!(uses.never_concurrent("looped", "later"));
    }

    /// The exclusive-arms proof, reproved by this prover: two slots confined
    /// to two arms of one conditional share, and so do a branch arm and the
    /// `else`.
    #[test]
    fn exclusive_arms_of_one_conditional_may_share_storage() {
        let uses = uses_of(&[
            ("left", vec![within(&[arm(4, 0)], 1)]),
            ("right", vec![within(&[arm(4, 1)], 0)]),
            ("fallback", vec![within(&[otherwise(4)], 2)]),
        ]);
        assert!(uses.never_concurrent("left", "right"));
        assert!(uses.never_concurrent("left", "fallback"));
        assert!(uses.never_concurrent("right", "fallback"));
    }

    /// NEGATIVE CONTROL. Arms of one conditional inside a `for` body are not
    /// exclusive across iterations, and neither is sequence: a value written
    /// late in iteration `k` is read early in iteration `k + 1`. The descent
    /// must stop at the loop body.
    #[test]
    fn the_prover_refuses_everything_inside_one_loop_body() {
        let uses = uses_of(&[
            ("first", vec![within(&[loop_body(1)], 0)]),
            ("second", vec![within(&[loop_body(1)], 5)]),
            ("arm_a", vec![within(&[loop_body(1), arm(2, 0)], 0)]),
            ("arm_b", vec![within(&[loop_body(1), arm(2, 1)], 0)]),
        ]);
        assert!(!uses.never_concurrent("first", "second"));
        assert!(!uses.never_concurrent("arm_a", "arm_b"));
    }

    /// Sequence *inside one arm* still separates: the arm's body runs at most
    /// once per activation, so the descent continues through it.
    #[test]
    fn sequential_uses_inside_one_arm_may_share_storage() {
        let uses = uses_of(&[
            (
                "first",
                vec![within(&[arm(3, 0)], 0), within(&[arm(3, 0)], 1)],
            ),
            ("second", vec![within(&[arm(3, 0)], 2)]),
        ]);
        assert!(uses.never_concurrent("first", "second"));
    }

    /// NEGATIVE CONTROL. A slot that spans a conditional refuses every slot
    /// inside it: nesting is not exclusion, exactly as the arm overlay rules.
    #[test]
    fn the_prover_refuses_a_spanning_slot_against_an_arm_local() {
        let uses = uses_of(&[
            ("spans", vec![at(2), at(6)]),
            ("arm_local", vec![within(&[arm(4, 1)], 0)]),
        ]);
        assert!(!uses.never_concurrent("spans", "arm_local"));
        // And a use AT the conditional itself blocks descent into its arms.
        let touches = uses_of(&[
            ("condition_read", vec![at(4)]),
            ("arm_local", vec![within(&[arm(4, 1)], 0)]),
        ]);
        assert!(!touches.never_concurrent("condition_read", "arm_local"));
    }

    /// A slot the walk never placed shares with nothing, and neither does a
    /// slot against itself.
    #[test]
    fn an_unknown_slot_shares_with_nothing() {
        let uses = uses_of(&[("known", vec![at(0)])]);
        assert!(!uses.never_concurrent("known", "stranger"));
        assert!(!uses.never_concurrent("known", "known"));
    }

    /// Placement: sequential slots overlap in the arena, an interleaved slot
    /// is pushed past them, and the total is the interleaved pair's sum rather
    /// than the whole inventory's.
    #[test]
    fn the_plan_overlaps_exactly_the_proven_pairs() {
        let uses = uses_of(&[
            ("early", vec![at(0), at(1)]),
            ("late", vec![at(2), at(3)]),
            ("spans", vec![at(0), at(3)]),
        ]);
        let sized = [
            candidate("early", 0, 900),
            candidate("late", 1, 900),
            candidate("spans", 2, 400),
        ];
        let resolved = plan(&uses, &sized);
        assert_eq!(resolved.offsets[&0], resolved.offsets[&1], "{resolved:?}");
        let spans = resolved.offsets[&2];
        assert!(
            spans >= 900,
            "`spans` overlaps a slot it is live beside: {resolved:?}"
        );
        assert_eq!(resolved.bytes, 1300, "{resolved:?}");
    }

    /// A small slot rides into a gap between conflicting placements rather
    /// than growing the arena.
    #[test]
    fn a_small_slot_lands_in_the_lowest_fitting_gap() {
        // `wide` spans everything, so the two smaller slots start after it;
        // they are never live together and carry one shape, so they land on
        // one offset rather than growing the arena further.
        let uses = uses_of(&[
            ("wide", vec![at(0), at(9)]),
            ("first", vec![at(1), at(2)]),
            ("second", vec![at(3), at(4)]),
        ]);
        let sized = [
            candidate("wide", 0, 400),
            candidate("first", 1, 100),
            candidate("second", 2, 100),
        ];
        let resolved = plan(&uses, &sized);
        assert_eq!(resolved.offsets[&0], 0);
        assert_eq!(resolved.offsets[&1], 400);
        assert_eq!(
            resolved.offsets[&2], 400,
            "`second` is never live beside `first` and shares its bytes: {resolved:?}"
        );
        assert_eq!(resolved.bytes, 500);
    }

    /// Every offset the plan chooses keeps the arena's element alignment, and
    /// a misaligned or unsizable slot is left out entirely.
    #[test]
    fn placement_is_aligned_and_fails_closed_on_unsizable_slots() {
        let uses = uses_of(&[
            ("a", vec![at(0)]),
            ("b", vec![at(1)]),
            ("odd", vec![at(2)]),
            ("unsized", vec![at(3)]),
        ]);
        let sized = [
            candidate("a", 0, 12),
            candidate("b", 1, 8),
            ArenaCandidate {
                name: "odd",
                index: 2,
                bytes: Some(3),
                extents: vec![1],
                call_boundary: false,
            },
            unsized_candidate("unsized", 3),
        ];
        let resolved = plan(&uses, &sized);
        for offset in resolved.offsets.values() {
            assert_eq!(offset % ARENA_ALIGN, 0, "{resolved:?}");
        }
        assert!(!resolved.offsets.contains_key(&2), "{resolved:?}");
        assert!(!resolved.offsets.contains_key(&3), "{resolved:?}");
    }

    /// Placement is a pure function of the uses and the sizes: the same
    /// inventory in the same declaration order lands identically however the
    /// caller assembled it.
    #[test]
    fn placement_is_deterministic() {
        let uses = uses_of(&[
            ("a", vec![at(0), at(2)]),
            ("b", vec![at(3), at(4)]),
            ("c", vec![at(1), at(4)]),
        ]);
        let sized = [
            candidate("a", 0, 64),
            candidate("b", 1, 64),
            candidate("c", 2, 32),
        ];
        let first = plan(&uses, &sized);
        let second = plan(&uses, &sized);
        assert_eq!(first.offsets, second.offsets);
        assert_eq!(first.bytes, second.bytes);
    }

    /// NEGATIVE CONTROL for the emitted C. Two slots whose live ranges are
    /// disjoint but whose declared extents differ never share an offset when
    /// either address reaches a declared formal: one address named by two
    /// array types is what a compiler's value numbering may unify, and the
    /// object size it then reads is the surviving type's.
    #[test]
    fn two_shapes_never_share_one_call_boundary_offset() {
        let uses = uses_of(&[("matrix", vec![at(0), at(1)]), ("row", vec![at(2), at(3)])]);
        assert!(
            uses.never_concurrent("matrix", "row"),
            "the liveness relation alone would allow this"
        );
        let mixed = |boundary: bool| {
            [
                ArenaCandidate {
                    name: "matrix",
                    index: 0,
                    bytes: Some(36),
                    extents: vec![3, 3],
                    call_boundary: false,
                },
                ArenaCandidate {
                    name: "row",
                    index: 1,
                    bytes: Some(16),
                    extents: vec![4],
                    call_boundary: boundary,
                },
            ]
        };
        let resolved = plan(&uses, &mixed(true));
        assert_ne!(
            resolved.offsets[&0], resolved.offsets[&1],
            "a [3][3] and a [4] handed to a formal must not name one address: {resolved:?}"
        );
        // The restriction is the call boundary and nothing else: the same two
        // shapes with no declared bound handed either address share freely,
        // because no object-size question is ever asked there.
        let unasked = plan(&uses, &mixed(false));
        assert_eq!(
            unasked.offsets[&0], unasked.offsets[&1],
            "two shapes no formal ever sees may share: {unasked:?}"
        );
        // And matching shapes share whether or not a boundary names them.
        let matched = [
            candidate("matrix", 0, 36),
            ArenaCandidate {
                call_boundary: true,
                ..candidate("row", 1, 36)
            },
        ];
        let shared = plan(&uses, &matched);
        assert_eq!(shared.offsets[&0], shared.offsets[&1], "{shared:?}");
    }

    /// THE soundness property, checked against the finished plan the way the
    /// census tests check the region overlay: every pair of placements whose
    /// byte ranges overlap is a pair the relation proves never concurrent.
    #[test]
    fn overlapping_placements_are_pairwise_never_concurrent() {
        let entries: Vec<(&str, Vec<SlotUse>)> = vec![
            ("s0", vec![at(0), at(3)]),
            ("s1", vec![at(4), at(6)]),
            ("s2", vec![at(2), at(5)]),
            ("s3", vec![within(&[arm(7, 0)], 0)]),
            ("s4", vec![within(&[arm(7, 1)], 0)]),
            ("s5", vec![within(&[loop_body(8)], 0)]),
            ("s6", vec![within(&[loop_body(8)], 1)]),
        ];
        let uses = uses_of(&entries);
        let sized: Vec<ArenaCandidate<'_>> = entries
            .iter()
            .enumerate()
            .map(|(index, (name, _))| candidate(name, index, 40 + 4 * index))
            .collect();
        let resolved = plan(&uses, &sized);
        for (left, right) in pairs(entries.len()) {
            let (Some(low), Some(other)) =
                (resolved.offsets.get(&left), resolved.offsets.get(&right))
            else {
                continue;
            };
            let bytes = sized[left].bytes.unwrap();
            let other_bytes = sized[right].bytes.unwrap();
            if *low >= other + other_bytes || *other >= low + bytes {
                continue;
            }
            let (left_name, right_name) = (entries[left].0, entries[right].0);
            assert!(
                uses.never_concurrent(left_name, right_name),
                "`{left_name}` and `{right_name}` overlap without a proof: {resolved:?}"
            );
        }
    }

    /// Every unordered pair of positions below `count`.
    fn pairs(count: usize) -> Vec<(usize, usize)> {
        (0..count)
            .flat_map(|left| (left + 1..count).map(move |right| (left, right)))
            .collect()
    }
}
