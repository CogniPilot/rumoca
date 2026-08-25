//! Target-neutral expression shape evidence for checked Algorithm Code.
// SPEC_0021 file-size exception: this view currently co-locates checked typed
// expression projection, semantic-use analysis, and scratch-layout accounting.
// The overlay relation and the placement policy already live in
// `algorithm_code_overlay`; split plan: move semantic-use analysis and the
// remaining scratch-layout accounting into sibling modules while keeping this
// file as the typed template-view facade.

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use rumoca_ir_galec::ast;
use serde::Serialize;

use super::algorithm_code_arena::{self as arena, ARENA_ALIGN, SlotUse};
use super::algorithm_code_bound_equalization::{self as bound_equalization, SlotBounds};
use super::algorithm_code_overlay::{self as overlay, CallGraph, Owner};
use super::algorithm_code_scopes::{LocalPlacements, ScopePath, ScopeStep};
use super::algorithm_code_slot_overlay::{self as slot_overlay, SlotHomes};
use super::source_trace::{SourceTrace, SourceTraceResolver, TraceLegend};

/// The call-boundary marshalling rewrite. A child module rather than a sibling
/// under `views`: it reads this view's statement, reference and kernel types in
/// full, and those are this module's own vocabulary rather than an interface
/// the rest of the crate has any use for.
mod marshalling;

#[derive(Debug, Clone, Serialize)]
pub(super) struct TypedBlockView<'a> {
    name: &'a ast::Name,
    interface: &'a [ast::InterfaceVariable],
    compartments: &'a [ast::StateCompartment],
    protected: &'a [ast::ProtectedEntity],
    error_signals: &'a [ast::Identifier],
    protected_functions: Vec<TypedFunctionView<'a>>,
    startup: TypedMethodView<'a>,
    recalibrate: TypedMethodView<'a>,
    do_step: TypedMethodView<'a>,
    public_functions: Vec<TypedFunctionView<'a>>,
    semantic_uses: SemanticOperationUses,
    /// The block's resolved working-memory layout — which regions exist, which
    /// of them share storage, and how much storage that is.
    ///
    /// This is a decision, taken here, not in a template. A target prints the
    /// groups in the order given and spells each region as a member of its
    /// group; it performs no arithmetic and reasons about no liveness.
    scratch_layout: ScratchLayoutView<'a>,
}

/// The array intermediates one function or method owns inside the block's
/// working memory.
///
/// **Liveness.** Every owner gets its own slots (no two owners ever share a
/// slot), so a slot's live range is contained in the enclosing call. Storage is
/// nonetheless shared, by two separate rules over two separate relations:
/// regions across owners, and slots within one owner across the arms of one
/// conditional. See [`ScratchLayoutView`] for both, and the two prover modules
/// it names for why each is sound.
///
/// **Instancing.** The overlaid aggregate is a member of the caller-provided
/// block state. Distinct block-state objects therefore own disjoint mutable
/// working-memory slots by construction (SPEC_0034 GAL-039).
#[derive(Debug, Clone, Serialize)]
struct ScratchRegionView<'a> {
    /// Who owns this region. The overlay prover reasons about owners, not about
    /// the two projections below it: the distinction between a method and a
    /// function is load-bearing for the relation, and a user function is free to
    /// be named `dostep`.
    #[serde(skip)]
    owner: Owner<'a>,
    /// Nonempty exactly when the owner needs a region at all.
    used: bool,
    /// Source name of the owning user function. `None` for a block method.
    function: Option<&'a str>,
    /// Fixed spelling of the owning block method. `None` for a user function.
    method: Option<&'static str>,
    /// The overlay group this region shares storage with — an index into
    /// [`ScratchLayoutView::groups`]. A target spells the region as a member of
    /// this group and does no reasoning of its own about which group that is.
    group: usize,
    /// Slot storage this region needs, in bytes, under the model in
    /// [`slot_bytes`]. `None` when a slot's extent is not a literal: every
    /// target that can print a region fails closed on such a slot long before
    /// this number is read.
    bytes: Option<usize>,
    /// What a target declares, in the order it declares it: the region's
    /// declarations (reachable array locals, then the owner's output
    /// parameters), with every set that shares storage collapsed into one
    /// member. See [`ScratchMemberView`].
    members: Vec<ScratchMemberView<'a>>,
    /// The arm overlay each slot landed in, by **source** name, or `None` for a
    /// slot that owns its storage. A reference names a slot, so this is how a
    /// body spells the one it reads.
    #[serde(skip)]
    slot_overlays: HashMap<&'a str, Option<usize>>,
    /// The region's flat value arena, or `None` when no pair of slots was
    /// proven able to share one. Slots placed here are removed from `members`;
    /// a target declares the arena as one `float` array and reaches each slot
    /// through a typed pointer at its fixed offset. The offsets are decided by
    /// the prover in `algorithm_code_arena` and only printed by a target.
    arena: Option<ArenaRegionView<'a>>,
}

/// One region's value arena as a target declares it.
#[derive(Debug, Clone, Serialize)]
struct ArenaRegionView<'a> {
    /// The arena's extent in `float` elements: total bytes over [`ARENA_ALIGN`].
    floats: usize,
    /// The placed slots, lowest offset first (ties in declaration order), each
    /// carrying the shape a target derives its typed pointer from.
    slots: Vec<ArenaSlotView<'a>>,
}

/// One slot living in a region's value arena.
#[derive(Debug, Clone, Serialize)]
struct ArenaSlotView<'a> {
    #[serde(flatten)]
    slot: ScratchSlotView<'a>,
    /// The slot's offset into the arena, in `float` elements.
    offset: usize,
    /// The slot's extent in `float` elements, so a template can print the
    /// occupied range without doing arithmetic of its own.
    floats: usize,
}

/// One member of a region's storage in the emitted C: either a slot on its own,
/// or a set of slots that share one piece of storage because the arms they are
/// local to are mutually exclusive.
///
/// The overlay decision behind a multi-slot member is taken by the prover in
/// `algorithm_code_slot_overlay` and only printed here; see that module for the
/// relation and the induction that keeps a class pairwise never-concurrent.
#[derive(Debug, Clone, Serialize)]
struct ScratchMemberView<'a> {
    /// `None` for a slot that owns its storage, which a target prints as a
    /// plain struct member. `Some(ordinal)` for a shared member, which a target
    /// prints as a union and names by this ordinal.
    overlay: Option<usize>,
    /// Storage this member needs, in bytes, under the model in
    /// [`slot_bytes`]: the largest of its slots. `None` when an extent is not a
    /// literal, exactly as a region's own total is.
    bytes: Option<usize>,
    /// The slots sharing this member, in declaration order. Exactly one when
    /// `overlay` is `None`, and at least two otherwise.
    slots: Vec<ScratchSlotView<'a>>,
}

/// One slot as a target declares it: a name, a scalar, and the extents.
///
/// The extents are carried here rather than read back off the checked
/// declaration because they are a *layout* decision this view owns. They are
/// the block's own declared extents everywhere except at a call-boundary
/// bound-clash address, where `algorithm_code_bound_equalization` widens the
/// leading one; see that module for the rule and for why widening the leading
/// extent moves no element.
#[derive(Debug, Clone, Serialize)]
struct ScratchSlotView<'a> {
    /// The slot's source name. A target allocates a C identifier from it with
    /// its own symbol policy; nothing here spells one.
    name: &'a ast::Name,
    /// The scalar the slot holds, or `None` for a compartment-typed slot: no
    /// target that prints regions accepts one, and this is what it fails on.
    scalar: Option<ast::ScalarType>,
    /// The extents a target declares, outermost first. `None` when a declared
    /// dimension is not a literal integer, which is the same condition
    /// [`slot_bytes`] refuses to size.
    extents: Option<Vec<usize>>,
    /// The leading extent the checked block declared, present only where this
    /// slot's was widened. A reader of the emitted header sees the widened
    /// bound; this is what says it was a layout decision and not the model's.
    equalized_from: Option<usize>,
    /// Storage this slot needs, in bytes, under the model in [`slot_bytes`],
    /// after any widening. `None` exactly where `extents` is.
    bytes: Option<usize>,
}

impl<'a> ScratchSlotView<'a> {
    fn new(slot: &'a ast::VariableDeclaration) -> Self {
        let shape = slot_shape(slot);
        Self {
            name: &slot.name,
            scalar: shape.as_ref().map(|(kind, _)| *kind),
            extents: shape.as_ref().map(|(_, extents)| extents.clone()),
            equalized_from: None,
            bytes: shape.as_ref().and_then(|(kind, extents)| {
                extents
                    .iter()
                    .try_fold(scalar_bytes(*kind), |bytes, extent| {
                        bytes.checked_mul(*extent)
                    })
            }),
        }
    }

    /// The slot's own name, for the maps that key on it.
    fn lexeme(&self) -> &'a str {
        self.name.lexeme()
    }

    /// Declare this slot with `leading` in its outermost dimension.
    ///
    /// Only ever called by [`equalize_bounds`], and only with a value the
    /// equalization derived from this slot's own stride, so the recomputed size
    /// cannot overflow where the declared one did not.
    fn widen_leading_to(&mut self, leading: usize) {
        let (Some(extents), Some(kind)) = (self.extents.as_mut(), self.scalar) else {
            return;
        };
        let Some(first) = extents.first_mut() else {
            return;
        };
        if leading <= *first {
            return;
        }
        self.equalized_from = Some(*first);
        *first = leading;
        self.bytes = extents
            .iter()
            .try_fold(scalar_bytes(kind), |bytes, extent| {
                bytes.checked_mul(*extent)
            });
    }

    /// How this slot enters the bound equalization, or `None` for a slot the
    /// projection cannot size, which makes its whole group unequalizable.
    ///
    /// `call_boundary_bytes` is filled in by the caller, which is the only
    /// place that knows which addresses the emitted code hands to a declared
    /// parameter bound.
    fn bounds(&self) -> Option<SlotBounds> {
        let (extents, kind) = (self.extents.as_ref()?, self.scalar?);
        let align = scalar_bytes(kind);
        let stride = extents
            .iter()
            .skip(1)
            .try_fold(align, |bytes, extent| bytes.checked_mul(*extent))?;
        // A zero extent would make the stride zero and the equalization's
        // division meaningless. Nothing this compiler emits declares one, and
        // refusing is cheaper than reasoning about it.
        if stride == 0 {
            return None;
        }
        let leading = extents.first().copied().unwrap_or(1);
        if leading == 0 {
            return None;
        }
        Some(SlotBounds {
            align,
            stride,
            leading,
            is_array: !extents.is_empty(),
            call_boundary_bytes: None,
        })
    }
}

impl<'a> ScratchRegionView<'a> {
    fn new(
        owner: Owner<'a>,
        slots: &[&'a ast::VariableDeclaration],
        placements: &LocalPlacements<'a>,
    ) -> Self {
        let (function, method) = match owner {
            Owner::Function(name) => (Some(name), None),
            Owner::Method(spelling) => (None, Some(spelling)),
        };
        let members = overlay_slots(slots, placements);
        let slot_overlays = members
            .iter()
            .flat_map(|member| {
                member
                    .slots
                    .iter()
                    .map(move |slot| (slot.lexeme(), member.overlay))
            })
            .collect();
        Self {
            owner,
            used: !members.is_empty(),
            function,
            method,
            // Resolved from the block's call graph once every owner is
            // projected: the overlay is decided from region SIZES as well as
            // from the graph, so it cannot be taken before the regions exist.
            group: 0,
            bytes: total_bytes(members.iter().map(|member| member.bytes)),
            members,
            slot_overlays,
            // Filled in by `adopt_arena` once the owner's statements are
            // final: the live ranges the arena is placed from are ranges in
            // the statements a target prints, not in the checked AST.
            arena: None,
        }
    }

    /// How this region is named in a diagnostic and in the generated summary:
    /// the *source* spelling, never a target's allocated C identifier.
    fn owner(&self) -> &'a str {
        self.owner.name()
    }
}

/// Collapse a region's slots into the members a target declares.
///
/// The overlay ordinals come from the prover; this only groups the slots the
/// prover put together and keeps declaration order, so a member appears where
/// its earliest slot was declared and the emitted struct still reads in the
/// order the projection produced.
fn overlay_slots<'a>(
    slots: &[&'a ast::VariableDeclaration],
    placements: &LocalPlacements<'a>,
) -> Vec<ScratchMemberView<'a>> {
    let homes = SlotHomes::observed(
        slots
            .iter()
            .filter_map(|slot| {
                let name = slot.name.lexeme();
                placements.home(name).map(|home| (name, home.to_vec()))
            })
            .collect(),
    );
    let sized: Vec<(&'a str, Option<usize>)> = slots
        .iter()
        .map(|slot| (slot.name.lexeme(), slot_bytes(slot)))
        .collect();
    let overlay = slot_overlay::plan(&homes, &sized);
    let mut members: Vec<ScratchMemberView<'a>> = Vec::new();
    let mut placed: BTreeMap<usize, usize> = BTreeMap::new();
    for (index, slot) in slots.iter().enumerate() {
        let view = ScratchSlotView::new(slot);
        let Some(ordinal) = overlay.overlay_of(index) else {
            members.push(ScratchMemberView {
                overlay: None,
                bytes: view.bytes,
                slots: vec![view],
            });
            continue;
        };
        match placed
            .get(&ordinal)
            .and_then(|position| members.get_mut(*position))
        {
            Some(member) => {
                member.bytes = widest_bytes([member.bytes, view.bytes]);
                member.slots.push(view);
            }
            None => {
                placed.insert(ordinal, members.len());
                members.push(ScratchMemberView {
                    overlay: Some(ordinal),
                    bytes: view.bytes,
                    slots: vec![view],
                });
            }
        }
    }
    members
}

/// The block's whole working-memory decision: which regions share storage, in
/// what order a target declares them, and what it costs.
///
/// # The overlay rule
///
/// Working memory is a sequence of **groups**. A group is a set of regions that
/// share one piece of storage; a target realizes it as a union. Every region is
/// in exactly one group, and a group is a set of owners the call-graph prover
/// in `algorithm_code_overlay` certified pairwise **never concurrent**: on an
/// acyclic call graph two owners are live at the same moment exactly when one
/// reaches the other, so two owners neither of which reaches the other lie on no
/// common call chain and their regions may be one piece of storage.
///
/// The soundness of that is not re-argued here and is not re-checked by a
/// target. A group's membership is built out of permission values the prover
/// alone can mint, so a group whose members are not pairwise never-concurrent
/// cannot be constructed; see `algorithm_code_overlay` for the induction. What
/// this view adds is *accounting*: it orders the groups, sizes them, and prints
/// what the decision cost.
///
/// Which groups exist is a policy, and the policy is largest-region-first: the
/// heaviest region founds a group and every later region that may share it
/// rides along for nothing. It is a heuristic, so the floor it is measured
/// against is published beside it: the heaviest call chain, whose members are
/// pairwise caller and callee and so may never share.
///
/// # The second rule, one level down
///
/// A group is exact at region granularity and says nothing below it: a function
/// whose body is a long `if/else if` chain gets one region sized for the sum of
/// every arm even though one arm runs. Inside a region, slots are therefore
/// overlaid a second time, by the separate prover in
/// `algorithm_code_slot_overlay`, over a separate relation: two slots share when
/// each is used in exactly one arm of one conditional, the arms exclude one
/// another, and the conditional is entered at most once per activation.
///
/// This is still NOT a general interference analysis over slots. A liveness
/// proof checked statement by statement is what a general one needs, and a wrong
/// overlay is a silent wrong-code defect; the relation here is decided from the
/// placement analysis's own scope paths and refuses everything it cannot read
/// off them, including two slots in one arm, a slot that spans the conditional,
/// and any conditional a loop encloses.
#[derive(Debug, Clone, Serialize)]
struct ScratchLayoutView<'a> {
    /// The groups, in declaration order; empty exactly when the block declares
    /// no working memory at all. A group is never empty, and each carries its
    /// own `ordinal` — a target loops over this and needs no emptiness test and
    /// no counter of its own.
    groups: Vec<ScratchGroupView<'a>>,
    /// The overlay group of every user function that owns a region, keyed by
    /// its **source** name. A call site knows only the name it calls, so this
    /// is how it spells the callee's region when it reads results back.
    region_groups: BTreeMap<&'a str, usize>,
    /// Total slot storage, in bytes, under the model in [`slot_bytes`]: the
    /// sum over groups of the largest region in each. Excludes whatever padding
    /// the target's own layout rules add, so it is a faithful account of the
    /// decision taken here and not a prediction of `sizeof`.
    bytes: Option<usize>,
    /// The least slot storage any never-concurrent overlay of these regions
    /// could use, from `CallGraph::least_overlay_bytes`. `bytes` equal to this
    /// says the placement policy left nothing on the table; `bytes` above it is
    /// the exact amount a cleverer policy could still win. `None` when a region
    /// cannot be sized, exactly as `bytes` is.
    least_bytes: Option<usize>,
    /// The heaviest root-to-leaf call chain and its total region bytes: the
    /// most legible part of the floor above, and the answer to "why is it this
    /// big". Every member of a chain is a caller of every member after it, so no
    /// two of them may share storage. Chain members are source names, outermost
    /// first.
    chain: Vec<&'a str>,
    chain_bytes: Option<usize>,
    /// How many arm overlays the regions hold between them, and how many slots
    /// those overlays share. The saving is already inside `bytes`, so this is
    /// what makes it visible: without it a reader sees a total and cannot tell
    /// how much of it came from below region granularity.
    arm_overlays: usize,
    arm_overlay_slots: usize,
    /// The same visibility for the value arenas: how many regions carry one,
    /// how many slots they hold, and the bytes those slots would need laid end
    /// to end against the bytes the arenas actually occupy.
    arena_regions: usize,
    arena_slots: usize,
    arena_slot_bytes: usize,
    arena_bytes: usize,
    /// One line of prose stating the facts above, for a target to print
    /// verbatim above the declaration. Formatted here so that a target needs no
    /// conditional for the not-a-literal-extent case.
    summary: String,
    /// The same, for the arm overlay. Its own line rather than a clause of
    /// `summary`: the two decisions are taken by two provers over two different
    /// relations, and a reader who wants to know why a region is the size it is
    /// reads them one at a time.
    arm_summary: String,
    /// And one more line for the value arenas, for the same reason: a third
    /// prover over a third relation, stated on its own.
    arena_summary: String,
}

/// One overlay group: the regions that share a single piece of storage.
#[derive(Debug, Clone, Serialize)]
struct ScratchGroupView<'a> {
    /// Position in [`ScratchLayoutView::groups`], and the group's identity in
    /// a generated identifier.
    ordinal: usize,
    /// Slot storage the group needs: the largest of its members.
    bytes: Option<usize>,
    /// Members, in the block's fixed region order. Never empty.
    regions: Vec<ScratchRegionView<'a>>,
}

/// Bytes one scalar of `kind` occupies, under the storage model every C target
/// of this view uses: `efmiFloat32`, `efmiInteger32`, `efmiBool`.
///
/// This is a *model*, used to report and to compare regions. It is not the
/// target's layout — the target owns padding and alignment — which is why
/// nothing correctness-bearing is derived from it.
fn scalar_bytes(kind: ast::ScalarType) -> usize {
    match kind {
        ast::ScalarType::Real | ast::ScalarType::Integer => 4,
        ast::ScalarType::Boolean => 1,
    }
}

/// Storage one region slot needs, or `None` if its extent is not a literal
/// integer.
fn slot_bytes(slot: &ast::VariableDeclaration) -> Option<usize> {
    let (kind, extents) = slot_shape(slot)?;
    extents
        .iter()
        .try_fold(scalar_bytes(kind), |bytes, extent| {
            bytes.checked_mul(*extent)
        })
}

/// The scalar a slot holds and the literal extents it was declared with.
///
/// `None` where the projection has no answer, which is the same condition
/// [`slot_bytes`] refuses on: a compartment-typed slot has no target-neutral
/// width, and a dimension that is not a literal integer has no extent. Every
/// target that prints regions fails closed on such a slot.
fn slot_shape(slot: &ast::VariableDeclaration) -> Option<(ast::ScalarType, Vec<usize>)> {
    let ast::TypeRef::Primitive(kind) = &slot.ty else {
        return None;
    };
    let mut extents = Vec::with_capacity(slot.dimensions.len());
    for dimension in &slot.dimensions {
        let ast::Dimension::Expr(ast::Expression::Integer(extent)) = dimension else {
            return None;
        };
        extents.push(usize::try_from(*extent).ok()?);
    }
    Some((*kind, extents))
}

/// Fold a group's, a member's or a chain's part sizes, propagating "not a
/// literal".
fn total_bytes(parts: impl IntoIterator<Item = Option<usize>>) -> Option<usize> {
    parts
        .into_iter()
        .try_fold(0usize, |sum, part| sum.checked_add(part?))
}

/// The larger of two sizes, propagating "not a literal" rather than treating a
/// missing size as a small one.
fn widest_bytes(parts: impl IntoIterator<Item = Option<usize>>) -> Option<usize> {
    parts
        .into_iter()
        .try_fold(0usize, |widest, part| Some(widest.max(part?)))
}

fn bytes_text(bytes: Option<usize>) -> String {
    bytes.map_or_else(
        || "an extent this projection cannot size".to_owned(),
        |value| format!("{value} bytes"),
    )
}

impl<'a> ScratchLayoutView<'a> {
    /// Collect the block's regions into the groups the prover placed them in and
    /// account for the result.
    fn resolve(
        regions: Vec<ScratchRegionView<'a>>,
        chain: Vec<&'a str>,
        least_bytes: Option<usize>,
    ) -> Self {
        // Regions carry their group; this reads that back rather than deciding
        // anything. Ordinals are contiguous from zero because every group the
        // placement built has at least the member that founded it, but nothing
        // here depends on that: the ordinal travels with the group, so a target
        // loops over whatever is here and needs no counter of its own.
        let mut ordinals: Vec<usize> = regions.iter().map(|region| region.group).collect();
        ordinals.sort_unstable();
        ordinals.dedup();
        let groups: Vec<ScratchGroupView<'a>> = ordinals
            .into_iter()
            .map(|ordinal| {
                let members: Vec<_> = regions
                    .iter()
                    .filter(|region| region.group == ordinal)
                    .cloned()
                    .collect();
                ScratchGroupView {
                    ordinal,
                    bytes: widest_bytes(members.iter().map(|region| region.bytes)),
                    regions: members,
                }
            })
            .collect();
        let region_groups = regions
            .iter()
            .filter_map(|region| region.function.map(|name| (name, region.group)))
            .collect();
        let bytes = total_bytes(groups.iter().map(|group| group.bytes));
        // A chain member that owns no region contributes nothing — that is a
        // real zero, not an unsizable extent, so it must not poison the total.
        let chain_bytes = total_bytes(chain.iter().map(|name| {
            regions
                .iter()
                .find(|region| region.owner() == *name)
                .map_or(Some(0), |region| region.bytes)
        }));
        let arm_members: Vec<&ScratchMemberView<'a>> = regions
            .iter()
            .flat_map(|region| &region.members)
            .filter(|member| member.overlay.is_some())
            .collect();
        let arm_overlays = arm_members.len();
        let arm_overlay_slots = arm_members.iter().map(|member| member.slots.len()).sum();
        let arm_summary = match arm_overlays {
            0 => "no arm overlay: no slot is provably local to one arm of a conditional".to_owned(),
            _ => format!(
                "{arm_overlays} arm overlay(s) hold {arm_overlay_slots} slots that are local to \
                 mutually exclusive arms",
            ),
        };
        let arenas: Vec<&ArenaRegionView<'a>> = regions
            .iter()
            .filter_map(|region| region.arena.as_ref())
            .collect();
        let arena_regions = arenas.len();
        let arena_slots: usize = arenas.iter().map(|arena| arena.slots.len()).sum();
        let arena_slot_bytes: usize = arenas
            .iter()
            .flat_map(|arena| &arena.slots)
            .filter_map(|slot| slot.slot.bytes)
            .sum();
        let arena_bytes: usize = arenas.iter().map(|arena| arena.floats * ARENA_ALIGN).sum();
        let arena_summary = match arena_regions {
            0 => "no value arena: no two slots were proven live-disjoint in the printed statement \
                  order"
                .to_owned(),
            _ => format!(
                "{arena_regions} value arena(s) hold {arena_slots} slots whose extents sum to \
                 {arena_slot_bytes} bytes in {arena_bytes} bytes of storage; every pair of slots \
                 at overlapping offsets is proven never live together",
            ),
        };
        let summary = if groups.is_empty() {
            "no working memory: every intermediate fits in a frame".to_owned()
        } else {
            format!(
                "{} overlay group(s), {} of slot storage{}; heaviest call chain {} at {}",
                groups.len(),
                bytes_text(bytes),
                minimality_text(bytes, least_bytes),
                if chain.is_empty() {
                    "(none)".to_owned()
                } else {
                    chain.join(" -> ")
                },
                bytes_text(chain_bytes),
            )
        };
        Self {
            groups,
            region_groups,
            bytes,
            least_bytes,
            chain,
            chain_bytes,
            arm_overlays,
            arm_overlay_slots,
            arena_regions,
            arena_slots,
            arena_slot_bytes,
            arena_bytes,
            summary,
            arm_summary,
            arena_summary,
        }
    }
}

/// How the achieved total compares with the floor, for the generated summary.
///
/// Says nothing when either number is missing: a block with an unsizable region
/// has no total to compare, and a reviewer must never read a comparison that was
/// made against a guess.
fn minimality_text(bytes: Option<usize>, least_bytes: Option<usize>) -> String {
    match (bytes, least_bytes) {
        (Some(achieved), Some(least)) if achieved == least => {
            ", the least a never-concurrent overlay of these regions can use".to_owned()
        }
        (Some(_), Some(least)) => format!(", against a floor of {least} bytes"),
        _ => String::new(),
    }
}

/// Target-neutral inventory of operations present in checked Algorithm Code.
/// Templates use this semantic evidence to emit only the runtime support that
/// the block can reach.
#[derive(Debug, Clone, Default, Serialize)]
struct SemanticOperationUses {
    sign: bool,
    integer_conversion: bool,
    real_min: bool,
    real_max: bool,
    /// Set by [`SquareForm::BoundMultiply`]: the block squares at least one
    /// Real base that has to be bound before it is multiplied.
    real_square: bool,
    integer_min: bool,
    integer_max: bool,
    division_towards_zero: bool,
    compare_lt: bool,
    compare_gt: bool,
    compare_le: bool,
    compare_ge: bool,
    compare_eq: bool,
    compare_ne: bool,
    bounded_selection: bool,
}

impl SemanticOperationUses {
    fn observe_call(&mut self, function: &str) {
        match function {
            "sign" => self.sign = true,
            "integer" => self.integer_conversion = true,
            "min" => self.real_min = true,
            "max" => self.real_max = true,
            "imin" => self.integer_min = true,
            "imax" => self.integer_max = true,
            "divisionTowardsZero" => self.division_towards_zero = true,
            _ => {}
        }
    }

    fn observe_real_comparison(&mut self, op: ast::BinaryOp) {
        match op {
            ast::BinaryOp::Lt => self.compare_lt = true,
            ast::BinaryOp::Gt => self.compare_gt = true,
            ast::BinaryOp::Le => self.compare_le = true,
            ast::BinaryOp::Ge => self.compare_ge = true,
            ast::BinaryOp::Eq => self.compare_eq = true,
            ast::BinaryOp::Ne => self.compare_ne = true,
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Serialize)]
struct TypedFunctionView<'a> {
    kind: ast::FunctionKind,
    name: &'a ast::Name,
    /// Anchor of the whole `function … end …;` declaration, so a target can
    /// head each emitted C function with the Modelica text it implements.
    trace: Option<SourceTrace>,
    signals: &'a [ast::Identifier],
    /// The GALEC signature, unfiltered. This is what an Algorithm Code
    /// rendering prints; it is not what a target with a block context has to
    /// spell as formal parameters.
    parameters: &'a [ast::Parameter],
    /// The parameters a context-passing target still has to pass per call:
    /// the inputs. Outputs are delivered through the owner's context region.
    input_parameters: Vec<TypedParameterView<'a>>,
    locals: &'a [ast::VariableDeclaration],
    c_locals: Vec<TypedLocalView<'a>>,
    scratch: ScratchRegionView<'a>,
    /// Whether the body actually reaches the region, i.e. whether an alias
    /// pointer to it would be read.
    uses_scratch: bool,
    statements: Vec<TypedSpannedStatement<'a>>,
}

/// One declaration a target gives automatic storage, and whether that target
/// still owes it an unused-entity marker.
///
/// The marker — `(void)&x;` in C — is not algorithm. It exists only to keep a
/// declaration the body never reads from failing a build under `-Werror`, and
/// every one of them is an executable statement a structural-coverage
/// obligation has to account for. So the question "is this one needed" is
/// answered here, once, from the same reachability walk that decided the
/// declaration's scope, rather than by emitting one unconditionally.
#[derive(Debug, Clone, Serialize)]
struct TypedLocalView<'a> {
    #[serde(flatten)]
    decl: &'a ast::VariableDeclaration,
    /// True exactly when the generated body never reads the declaration, so a
    /// target that diagnoses set-but-unused entities needs a marker. See
    /// `LocalPlacements::is_read` for what counts as a read and why the
    /// definition is the conservative one.
    needs_unused_marker: bool,
}

/// One formal parameter a target still spells, and whether it owes it a marker.
///
/// Checked construction proves an input parameter is never written, so any
/// mention of one is a read: a parameter the body mentions needs no `(void)x;`
/// and a parameter it never mentions does.
#[derive(Debug, Clone, Serialize)]
struct TypedParameterView<'a> {
    #[serde(flatten)]
    parameter: &'a ast::Parameter,
    needs_unused_marker: bool,
}

#[derive(Debug, Clone, Serialize)]
struct TypedMethodView<'a> {
    signals: &'a [ast::PredefinedSignal],
    locals: &'a [ast::VariableDeclaration],
    c_locals: Vec<TypedLocalView<'a>>,
    scratch: ScratchRegionView<'a>,
    uses_scratch: bool,
    /// Block state variables this method writes **whole**, on **every** path
    /// through it (definite assignment), sorted by declaration spelling.
    ///
    /// This is the determinacy evidence a target needs at a method boundary
    /// that runs before anything else has written the block state — the
    /// `Startup` return boundary. `rumoca-eval-galec` skips an uninitialized
    /// slot in `execution.rs::limit_all`; a target whose block state is caller
    /// -allocated memory has no `is_initialized` flag to consult, so the only
    /// slots it may touch there are the ones the method is proven to have
    /// written. A *may*-write set would not do: a variable assigned in one
    /// arm of an `if` is not written on the other, and reading it back would
    /// be an indeterminate read rather than a saturation.
    ///
    /// Hence: sequence unions, `if` intersects every branch *and* the `else`
    /// body (a missing `else` contributes nothing), and `for` contributes
    /// nothing at all (a checked GALEC loop may still be a zero-trip domain).
    /// Element writes (`self.x[i] := …`) never enter the set either — they
    /// leave the rest of the array indeterminate, and the saturation a target
    /// applies is whole-variable.
    definite_state_writes: Vec<&'a str>,
    statements: Vec<TypedSpannedStatement<'a>>,
}

/// Block state variables definitely written by a statement sequence
/// (see [`TypedMethodView::definite_state_writes`]).
fn definite_state_writes(statements: &[ast::Spanned<ast::Statement>]) -> BTreeSet<&str> {
    let mut written = BTreeSet::new();
    for statement in statements {
        match &statement.node {
            ast::Statement::Assignment { target, .. } => {
                written.extend(whole_state_target(target));
            }
            ast::Statement::MultiAssignment { targets, .. } => {
                for target in targets {
                    written.extend(whole_state_target(target));
                }
            }
            ast::Statement::If(conditional) => {
                written.extend(definite_conditional_writes(conditional));
            }
            // A checked `for` loop carries statically evaluated bounds, but a
            // zero-trip domain is legal, so its body writes nothing that holds
            // on every path. `Call`/`Limit`/`Signal` initialize no state.
            ast::Statement::For(_)
            | ast::Statement::Call(_)
            | ast::Statement::Limit(_)
            | ast::Statement::Signal(_) => {}
        }
    }
    written
}

/// The intersection an `if` contributes: only what *every* reachable exit
/// wrote. Without an `else` body one exit writes nothing, so the whole
/// statement contributes nothing.
fn definite_conditional_writes(conditional: &ast::IfStatement) -> BTreeSet<&str> {
    let Some(else_body) = conditional.else_body.as_deref() else {
        return BTreeSet::new();
    };
    let mut common = definite_state_writes(else_body);
    for branch in &conditional.branches {
        let branch_writes = definite_state_writes(&branch.body);
        common.retain(|name| branch_writes.contains(name));
    }
    common
}

/// The block state variable a reference writes in full, if it writes one:
/// a single unsubscripted `self.x`. A subscripted or compartment-qualified
/// target writes a part, which is not evidence that the declaration is
/// determinate.
fn whole_state_target(reference: &ast::Reference) -> Option<&str> {
    let ast::Reference::State(parts) = reference else {
        return None;
    };
    match parts.as_slice() {
        [part] if part.subscripts.is_empty() => Some(part.name.lexeme()),
        _ => None,
    }
}

#[derive(Debug, Clone, Serialize)]
struct TypedSpannedStatement<'a> {
    trace: Option<SourceTrace>,
    /// A C-family target's decision to print this statement as a call into the
    /// shared array-kernel library instead of as the loop nest the checked
    /// Algorithm Code spells.
    ///
    /// Carried as a sibling of the node, never as a rewrite of it, for the same
    /// reason [`TypedReferenceView::context_resident`] and
    /// `TypedExpressionNodeView::Binary::square_form` are sibling fields:
    /// this view is shared with the Algorithm Code (`.alg`) rendering, and a
    /// target-specific *emission* decision must not change what the checked
    /// GALEC prints. `model.alg.jinja` never reads this field, so the `.alg`
    /// is byte-identical with and without every kernel in this module.
    kernel: Option<KernelStatementView<'a>>,
    node: TypedStatementView<'a>,
}

/// How a C-family target prints one statement that the shared array-kernel
/// library covers.
///
/// Each variant carries everything the template needs to print the call, so the
/// template performs no detection of its own — it selects on `kind` and prints.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
enum KernelStatementView<'a> {
    /// This statement was absorbed into a kernel call a neighbouring statement
    /// prints, and a C-family target emits nothing at all for it — not even its
    /// trace comment, which the surviving statement of the group carries.
    Absorbed,
    /// A call whose marshalling temporaries were dropped: the statement a
    /// C-family target prints in place of the one beside it, with each retired
    /// temporary replaced by the aggregate its copy touched.
    ///
    /// Carried here rather than written back over the node for the reason every
    /// other decision in this enum is: `model.alg.jinja` never reads this
    /// field, so the checked Algorithm Code still prints the call and the copy
    /// exactly as the block spells them. See
    /// [`super::algorithm_code_marshalling`] for the proof each replacement
    /// needs.
    Marshalled {
        statement: Box<TypedStatementView<'a>>,
    },
    /// A guarded whole-tensor loop nest printed with its index space
    /// restricted to the guard's own rectangle: the checked GALEC spells
    ///
    /// ```text
    /// for i in 1:n loop for j in 1:m loop
    ///   T[i][j] := if a <= i and i <= b and c <= j and j <= d
    ///              then V(i, j) else T[i][j];
    /// end for; end for;
    /// ```
    ///
    /// and the C target prints the same statement with `i` running `a:b` and
    /// `j` running `c:d`, the selector gone. Outside the rectangle the
    /// original iteration stored an element's own value back into it, which
    /// leaves every bit of memory as it was, and the conditional evaluated
    /// only integer comparisons of the loop indices against literals, which
    /// no observer can count. Inside the rectangle `V` is evaluated exactly
    /// as often as before (the selector evaluated only its chosen arm), so
    /// the restriction is bit-preserving with no purity demand on `V`.
    ///
    /// The replacement is a rebuilt statement printed by the ordinary
    /// statement machinery, exactly like [`Marshalled`](Self::Marshalled).
    IndexRestricted {
        statement: Box<TypedStatementView<'a>>,
    },
    /// A whole-array assignment whose every element receives the same literal.
    /// The target's loop nest is still printed down to its innermost dimension,
    /// which becomes one `rumoca_galec_fill_*` call over that run.
    Fill { value: TypedExpressionView<'a> },
    /// `target := sum(k in 1:count) lhs[…][k] * rhs[…][k]`, where dropping each
    /// operand's final subscript names a contiguous run of exactly `count`
    /// elements — so the whole reduction is one `rumoca_galec_dot_real` call.
    ///
    /// `lhs` and `rhs` are the operand references with that final subscript
    /// dropped: the runs themselves.
    Dot {
        count: i64,
        target: TypedReferenceView<'a>,
        lhs: TypedReferenceView<'a>,
        rhs: TypedReferenceView<'a>,
    },
    /// A whole loop over one result row of a matrix product:
    /// `target[…][j] := sum(k) scale(k) * source[k][j]` for every `j`, where
    /// `source`'s row is contiguous but its *column* — the walk the reduction
    /// would need — is not.
    ///
    /// A rank-1 kernel cannot read that column, so the emission instead
    /// accumulates the whole result row at once: zero it, then add each
    /// `scale(k)`-scaled source row into it. For a fixed `j` that performs the
    /// identical products in the identical ascending-`k` order into an
    /// identical `float`, so it is bit-preserving; it just interleaves the
    /// independent `j` sums instead of running them one after another. The `j`
    /// loop disappears entirely.
    ///
    /// `count` is the row length, `extent` the contracted extent, `iterator`
    /// the contracted index the emitted loop still runs over.
    ScaledAdd {
        count: i64,
        zero: TypedExpressionView<'a>,
        target: TypedReferenceView<'a>,
        iterator: &'a ast::Name,
        extent: i64,
        scale: Box<TypedExpressionView<'a>>,
        source: TypedReferenceView<'a>,
    },
    /// The [`ScaledAdd`](Self::ScaledAdd) row accumulation with a *fused*
    /// store: the reduction feeds a larger per-element expression instead of
    /// landing bare, i.e.
    ///
    /// ```text
    /// for j in 1:count loop
    ///   acc := 0.0;
    ///   for k in 1:extent loop acc := acc + (scale(k) * source[k][j]); end for;
    ///   target[…][j] := f(…, acc);
    /// end for;
    /// ```
    ///
    /// The emission declares the retired scalar accumulator as a
    /// `count`-element row in a compound block, accumulates the whole row
    /// first (zero it, then add each `scale(k)`-scaled source row), and then
    /// runs the store loop with `acc` respelled as that row's `j`-th element
    /// (`store_value` arrives with the substitution already made). For a
    /// fixed `j` the identical products are accumulated in the identical
    /// ascending-`k` order into an identical `float`, and each store computes
    /// the identical expression over that identical value in the identical
    /// `j` order, so the substitution is bit-preserving. The store expression
    /// is restricted to effect-free arithmetic ([`effect_free_store`]), so
    /// moving the accumulation ahead of every store reorders no observable
    /// effect.
    ScaledAddFused {
        count: i64,
        zero: TypedExpressionView<'a>,
        /// The retired scalar accumulator, re-declared as the row.
        row_local: &'a ast::Name,
        /// The row target the kernel calls accumulate into: the row local,
        /// unsubscripted.
        target: TypedReferenceView<'a>,
        /// The result-row index the emitted store loop runs over.
        column: &'a ast::Name,
        iterator: &'a ast::Name,
        extent: i64,
        scale: Box<TypedExpressionView<'a>>,
        source: TypedReferenceView<'a>,
        store_target: TypedReferenceView<'a>,
        store_value: Box<TypedExpressionView<'a>>,
    },
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedStatementView<'a> {
    Assignment {
        target: TypedReferenceView<'a>,
        value: TypedExpressionView<'a>,
    },
    MultiAssignment {
        targets: Vec<TypedReferenceView<'a>>,
        call: TypedCallView<'a>,
    },
    Call(TypedCallView<'a>),
    If(TypedIfStatementView<'a>),
    For(Box<TypedForView<'a>>),
    Limit(Vec<TypedLimitTargetView<'a>>),
    Signal(&'a [ast::Identifier]),
}

#[derive(Debug, Clone, Serialize)]
struct TypedIfStatementView<'a> {
    branches: Vec<TypedIfBranchView<'a>>,
    else_body: Option<Vec<TypedSpannedStatement<'a>>>,
    else_c_locals: Vec<TypedLocalView<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedIfBranchView<'a> {
    condition: TypedConditionView<'a>,
    c_locals: Vec<TypedLocalView<'a>>,
    body: Vec<TypedSpannedStatement<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedForView<'a> {
    iterator: &'a Option<ast::Name>,
    start: TypedExpressionView<'a>,
    step: Option<TypedExpressionView<'a>>,
    stop: TypedExpressionView<'a>,
    c_locals: Vec<TypedLocalView<'a>>,
    body: Vec<TypedSpannedStatement<'a>>,
    /// The row contraction this loop legalizes, as the GALEC lowering
    /// attested it. Carried by reference and holding only names and extents,
    /// so no scan of this view counts an operand twice; the operand views
    /// themselves stay where every scan already finds them, in `body`.
    #[serde(skip)]
    row_contraction: Option<&'a ast::RowContraction>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedLimitTargetView<'a> {
    SelfState,
    Reference(TypedReferenceView<'a>),
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedConditionView<'a> {
    Expression(TypedExpressionView<'a>),
    SignalCheck(&'a ast::SignalCheck),
}

#[derive(Debug, Clone, Serialize)]
struct TypedCallView<'a> {
    function: &'a ast::Name,
    lifted_base: Option<&'static str>,
    user_function: bool,
    arguments: Vec<TypedExpressionView<'a>>,
    /// For a user call: the callee's output declarations, in signature order.
    /// A target that returns outputs through the callee's context region reads
    /// them back from these slots instead of passing out pointers; the order
    /// is the order the call's assignment targets are written in.
    outputs: Vec<&'a ast::VariableDeclaration>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedReferenceView<'a> {
    rank: usize,
    extents: Option<Vec<usize>>,
    scalar: Option<ast::ScalarType>,
    /// `true` when this `local` reference names a slot in the enclosing
    /// owner's context region rather than an automatic object in the frame.
    /// Carried as a sibling flag rather than a new reference *kind* on
    /// purpose: the Algorithm Code rendering shares this view and knows only
    /// `local` and `state`, and a target-specific storage decision must not
    /// change what the checked GALEC prints.
    context_resident: bool,
    /// The arm overlay inside that region the slot is reached through, or
    /// `None` for a slot that owns its storage and for a reference that is not
    /// context-resident at all. A target names the overlay by this ordinal; it
    /// takes no view of its own on which slots share one.
    context_overlay: Option<usize>,
    /// `true` when the context-resident slot this reference names lives in its
    /// region's value arena rather than in a declared member. A target reaches
    /// such a slot through the typed pointer it declared at the owner's entry,
    /// so it prints the bare identifier and ignores `context_overlay`. Set by
    /// `mark_arena_references` after the arena is placed, which is after every
    /// reference is projected; false everywhere else, including for the `.alg`
    /// rendering, which reads none of the `context_*` fields.
    context_arena: bool,
    /// The literal extents the reference's *final* declaration was declared
    /// with, before this reference's own subscripts are applied — so
    /// `Some([15, 15])` for both `P` and `P[i][j]` where `P` is a `[15, 15]`
    /// array, and `None` for an iterator, a compartment, or a declaration whose
    /// dimensions are not literal.
    ///
    /// `extents` above answers "what shape does this reference *evaluate* to";
    /// this answers "what object does it index into", which is what a target
    /// needs to know whether dropping the last subscript names a contiguous run
    /// and how long that run is. Neither is derivable from the other.
    declared_extents: Option<Vec<usize>>,
    #[serde(flatten)]
    node: TypedReferenceNodeView<'a>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedReferenceNodeView<'a> {
    Local(TypedRefPartView<'a>),
    State(Vec<TypedRefPartView<'a>>),
}

/// One `name[subscripts]` step of a reference.
///
/// The subscripts are the *typed* expression views, not the checked AST
/// expressions they came from. A raw subscript carries no shape evidence and
/// no storage decision, so a template that printed it directly was printing an
/// expression it could not answer questions about — including whether a name
/// inside it lives in the frame or in the block context.
#[derive(Debug, Clone, Serialize)]
struct TypedRefPartView<'a> {
    name: &'a ast::Name,
    subscripts: Vec<TypedExpressionView<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedExpressionView<'a> {
    rank: usize,
    extents: Option<Vec<usize>>,
    scalar: Option<ast::ScalarType>,
    #[serde(flatten)]
    node: TypedExpressionNodeView<'a>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
enum TypedExpressionNodeView<'a> {
    Bool(bool),
    Integer(i64),
    Real(f64),
    Ref(TypedReferenceView<'a>),
    Size {
        array: TypedReferenceView<'a>,
        dimension: Box<TypedExpressionView<'a>>,
    },
    Call(TypedCallView<'a>),
    Paren(Box<TypedExpressionView<'a>>),
    If(TypedIfExpressionView<'a>),
    BoundedSelection(Box<TypedBoundedSelectionView<'a>>),
    Array(Vec<TypedExpressionView<'a>>),
    Neg(TypedReferenceView<'a>),
    Not(Box<TypedExpressionView<'a>>),
    Binary {
        op: ast::BinaryOp,
        precedence_class: ast::PrecedenceClass,
        associativity: ast::Associativity,
        lhs: Box<TypedExpressionView<'a>>,
        rhs: Box<TypedExpressionView<'a>>,
        /// Which spelling a C-family target gives this node when it is
        /// `<real> ^ 2`. See [`SquareForm`] and [`real_square_form`] for the
        /// exact conditions and for why the multiply is the *more* accurate of
        /// the two spellings.
        ///
        /// Carried as a sibling field rather than a rewritten `Mul` node on
        /// purpose, for the same reason
        /// [`TypedReferenceView::context_resident`] is a flag: this view is
        /// shared by the Algorithm Code (`.alg`) rendering, and a
        /// target-specific *emission* decision must not change what the checked
        /// GALEC prints. `x ^ 2` stays `x ^ 2` in `model.alg.jinja`.
        square_form: SquareForm,
    },
}

/// How a C-family target spells a power.
///
/// A square over a Real base is always emitted as a multiply; the variants
/// differ only in where the operand that is multiplied comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
enum SquareForm {
    /// Not a square over a Real base: the target prints its `powf` call.
    Power,
    /// The base is cheap and effect-free to print twice, so the target prints
    /// `(lhs * lhs)` and needs no binding of its own.
    Multiply,
    /// The base is neither, so the target prints `rumoca_galec_square(lhs)`.
    /// The helper's parameter is the binding: the base's rendered text appears
    /// once and is therefore evaluated once, exactly as the `powf` call
    /// evaluated it once.
    BoundMultiply,
}

#[derive(Debug, Clone, Serialize)]
struct TypedIfExpressionView<'a> {
    branches: Vec<(TypedExpressionView<'a>, TypedExpressionView<'a>)>,
    else_value: Box<TypedExpressionView<'a>>,
}

#[derive(Debug, Clone, Serialize)]
struct TypedBoundedSelectionView<'a> {
    reference: TypedReferenceView<'a>,
    extents: Vec<u32>,
    galec: TypedIfExpressionView<'a>,
}

/// Project a checked block, anchoring every statement and function against
/// `sources` so the emitted trace names a path a reviewer can open.
///
/// The traced-file legend is returned beside the view: it is the set of sources
/// the projection actually reached, which is much smaller than the session's
/// source map (a model instantiates far more library files than it keeps
/// statements from) and is what the emitted file documents.
pub(super) fn block<'a>(
    block: &'a ast::Block,
    sources: &'a rumoca_core::SourceMap,
) -> Result<(TypedBlockView<'a>, TraceLegend), String> {
    let shapes = BlockShapes::new(block, sources);
    // The overlay below is only sound on an acyclic call graph, so this runs
    // first and the layout is derived from the same `shapes.functions` map it
    // just certified. A cycle fails the whole projection rather than producing
    // a layout that proves nothing.
    shapes.require_acyclic_calls()?;
    let mut protected_functions = block
        .protected_functions
        .iter()
        .map(|function| shapes.function(function))
        .collect::<Result<Vec<_>, _>>()?;
    let mut startup = shapes.method(&block.startup, "startup")?;
    let mut recalibrate = shapes.method(&block.recalibrate, "recalibrate")?;
    let mut do_step = shapes.method(&block.do_step, "dostep")?;
    let mut public_functions = block
        .public_functions
        .iter()
        .map(|function| shapes.function(function))
        .collect::<Result<Vec<_>, _>>()?;
    let graph = CallGraph::prove(call_edges(block, &shapes.functions))?;
    place_regions(
        &graph,
        [
            &mut startup.scratch,
            &mut recalibrate.scratch,
            &mut do_step.scratch,
        ]
        .into_iter()
        .chain(
            protected_functions
                .iter_mut()
                .chain(&mut public_functions)
                .map(|function| &mut function.scratch),
        ),
    );
    equalize_block_bounds(
        &shapes.functions,
        [&mut startup, &mut recalibrate, &mut do_step],
        protected_functions
            .iter_mut()
            .chain(&mut public_functions)
            .collect(),
    );
    let semantic_uses = shapes.semantic_uses.borrow().clone();
    let regions: Vec<_> = protected_functions
        .iter()
        .map(|function| function.scratch.clone())
        .chain([
            startup.scratch.clone(),
            recalibrate.scratch.clone(),
            do_step.scratch.clone(),
        ])
        .chain(public_functions.iter().map(|f| f.scratch.clone()))
        .filter(|region| region.used)
        .collect();
    let weights = regions
        .iter()
        .map(|region| (region.owner, region.bytes.unwrap_or(0)))
        .collect();
    let chain = graph
        .heaviest_chain(&weights)
        .into_iter()
        .map(Owner::name)
        .collect();
    // The floor is only meaningful when every region has a size: one unsizable
    // extent and there is no total to compare it against.
    let least_bytes = regions
        .iter()
        .map(|region| region.bytes.map(|bytes| (region.owner, bytes)))
        .collect::<Option<BTreeMap<_, _>>>()
        .map(|sizes| graph.least_overlay_bytes(&sizes));
    let scratch_layout = ScratchLayoutView::resolve(regions, chain, least_bytes);
    // Read the legend after every method and function has been projected: the
    // resolver only knows a source once a statement in it has been anchored.
    let traces = shapes.traces.legend();
    Ok((
        TypedBlockView {
            name: &block.name,
            interface: &block.interface,
            compartments: &block.compartments,
            protected: &block.protected,
            error_signals: &block.error_signals,
            protected_functions,
            startup,
            recalibrate,
            do_step,
            public_functions,
            semantic_uses,
            scratch_layout,
        },
        traces,
    ))
}

/// The block's call graph, as the overlay prover wants it: one entry per owner,
/// including an owner that calls nothing, holding only the callees the block
/// declares.
///
/// A name the block does not declare is a builtin, not a call edge. Everything
/// else comes straight from [`collect_called_functions`], which is the overlay's
/// single soundness input: a call edge that walk does not see is a caller and a
/// callee the overlay does not separate.
fn call_edges<'a>(
    block: &'a ast::Block,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
) -> BTreeMap<Owner<'a>, BTreeSet<Owner<'a>>> {
    let declared = |called: BTreeSet<&'a str>| -> BTreeSet<Owner<'a>> {
        called
            .into_iter()
            .filter_map(|callee| functions.get_key_value(callee))
            .map(|(name, _)| Owner::Function(name))
            .collect()
    };
    let mut edges = BTreeMap::new();
    for (spelling, method) in [
        ("startup", &block.startup),
        ("recalibrate", &block.recalibrate),
        ("dostep", &block.do_step),
    ] {
        let mut called = BTreeSet::new();
        collect_called_functions(&method.statements, &mut called);
        edges.insert(Owner::Method(spelling), declared(called));
    }
    for (name, function) in functions {
        let mut called = BTreeSet::new();
        collect_called_functions(&function.statements, &mut called);
        edges.insert(Owner::Function(name), declared(called));
    }
    edges
}

/// Decide which regions share storage and record each region's group.
///
/// Only a region that exists is placed: an owner whose intermediates all fit in
/// a frame owns nothing to overlay, and leaving it out keeps it from occupying
/// a group and inflating the account. Such a region is never printed either, so
/// the group it keeps is never read.
fn place_regions<'a, 'view>(
    graph: &CallGraph<'a>,
    regions: impl Iterator<Item = &'view mut ScratchRegionView<'a>>,
) where
    'a: 'view,
{
    let mut regions: Vec<&'view mut ScratchRegionView<'a>> =
        regions.filter(|region| region.used).collect();
    let sized: Vec<(Owner<'a>, Option<usize>)> = regions
        .iter()
        .map(|region| (region.owner, region.bytes))
        .collect();
    let plan = overlay::plan(graph, &sized);
    for region in &mut regions {
        // Every placed region has a group: `plan` places every owner it is
        // given, founding a group where none admits it.
        region.group = plan.class_of(region.owner).unwrap_or_default();
    }
}

/// Every context-region slot the emitted bodies hand to a **declared array
/// parameter**, with the bytes that parameter declares, keyed by the owner
/// whose region the slot belongs to.
///
/// # Why this reads the projected statements
///
/// The question the bound equalization asks is "which addresses does the
/// generated code pass to which bounds", and the only faithful answer is the
/// statement list a target prints. So this walks that list: it follows the
/// marshalling rewrite where one landed, and skips a statement a kernel
/// absorbed, because those are exactly the statements that do and do not reach
/// the file. Reading the checked AST instead would answer for a call the
/// rewrite has since retargeted, and grepping the emitted C would put a second
/// decision procedure beside the emitter that can disagree with it.
///
/// Only a *whole* context-resident local counts. A subscripted reference names
/// an element, not the slot's address; a state reference is not overlaid
/// storage at all; and a formal with no declared dimensions carries no bound
/// for a compiler to check an object size against.
fn call_boundary_bounds<'a, 'view>(
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    owners: impl Iterator<Item = (Owner<'a>, &'view [TypedSpannedStatement<'a>])>,
) -> BTreeMap<Owner<'a>, BTreeMap<&'a str, usize>>
where
    'a: 'view,
{
    owners
        .map(|(owner, statements)| {
            let mut found = BTreeMap::new();
            boundaries_in_statements(statements, functions, &mut found);
            (owner, found)
        })
        .collect()
}

fn boundaries_in_statements<'a>(
    statements: &[TypedSpannedStatement<'a>],
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    found: &mut BTreeMap<&'a str, usize>,
) {
    for statement in statements {
        match &statement.kernel {
            // Absorbed into a neighbour's kernel call: a target emits nothing
            // at all for it, so it hands no address to anything.
            Some(KernelStatementView::Absorbed) => {}
            // The marshalling rewrite's replacement is the statement that
            // reaches the file; the node beside it is not printed.
            Some(KernelStatementView::Marshalled { statement }) => {
                boundaries_in_statement(statement, functions, found);
            }
            Some(KernelStatementView::IndexRestricted { statement }) => {
                boundaries_in_statement(statement, functions, found);
            }
            // The remaining kernels replace an assignment with a call into the
            // shared array library, whose parameters declare no bound. The node
            // is still the statement's own arithmetic, and a user call inside
            // it would still be emitted, so it is walked.
            _ => boundaries_in_statement(&statement.node, functions, found),
        }
    }
}

fn boundaries_in_statement<'a>(
    statement: &TypedStatementView<'a>,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    found: &mut BTreeMap<&'a str, usize>,
) {
    match statement {
        TypedStatementView::Assignment { value, .. } => {
            boundaries_in_expression(value, functions, found);
        }
        TypedStatementView::MultiAssignment { call, .. } | TypedStatementView::Call(call) => {
            boundaries_in_call(call, functions, found);
        }
        TypedStatementView::If(conditional) => {
            for branch in &conditional.branches {
                if let TypedConditionView::Expression(condition) = &branch.condition {
                    boundaries_in_expression(condition, functions, found);
                }
                boundaries_in_statements(&branch.body, functions, found);
            }
            if let Some(body) = &conditional.else_body {
                boundaries_in_statements(body, functions, found);
            }
        }
        TypedStatementView::For(loop_) => {
            boundaries_in_expression(&loop_.start, functions, found);
            if let Some(step) = &loop_.step {
                boundaries_in_expression(step, functions, found);
            }
            boundaries_in_expression(&loop_.stop, functions, found);
            boundaries_in_statements(&loop_.body, functions, found);
        }
        TypedStatementView::Limit(_) | TypedStatementView::Signal(_) => {}
    }
}

fn boundaries_in_call<'a>(
    call: &TypedCallView<'a>,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    found: &mut BTreeMap<&'a str, usize>,
) {
    for argument in &call.arguments {
        boundaries_in_expression(argument, functions, found);
    }
    if !call.user_function {
        return;
    }
    let Some(callee) = functions.get(call.function.lexeme()) else {
        return;
    };
    let inputs = callee
        .parameters
        .iter()
        .filter(|parameter| parameter.direction == ast::Direction::Input);
    for (parameter, argument) in inputs.zip(&call.arguments) {
        // A formal with no declared dimensions is a scalar: nothing declares a
        // bound at the address, so nothing can be diagnosed against one.
        if parameter.decl.dimensions.is_empty() {
            continue;
        }
        let (Some(bytes), Some(slot)) = (slot_bytes(&parameter.decl), whole_region_slot(argument))
        else {
            continue;
        };
        let entry = found.entry(slot).or_insert(bytes);
        *entry = (*entry).max(bytes);
    }
}

fn boundaries_in_expression<'a>(
    value: &TypedExpressionView<'a>,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    found: &mut BTreeMap<&'a str, usize>,
) {
    match &value.node {
        TypedExpressionNodeView::Call(call) => boundaries_in_call(call, functions, found),
        TypedExpressionNodeView::Paren(inner) | TypedExpressionNodeView::Not(inner) => {
            boundaries_in_expression(inner, functions, found);
        }
        TypedExpressionNodeView::Binary { lhs, rhs, .. } => {
            boundaries_in_expression(lhs, functions, found);
            boundaries_in_expression(rhs, functions, found);
        }
        TypedExpressionNodeView::If(conditional) => {
            for (condition, branch) in &conditional.branches {
                boundaries_in_expression(condition, functions, found);
                boundaries_in_expression(branch, functions, found);
            }
            boundaries_in_expression(&conditional.else_value, functions, found);
        }
        TypedExpressionNodeView::BoundedSelection(selection) => {
            for (condition, branch) in &selection.galec.branches {
                boundaries_in_expression(condition, functions, found);
                boundaries_in_expression(branch, functions, found);
            }
            boundaries_in_expression(&selection.galec.else_value, functions, found);
        }
        TypedExpressionNodeView::Array(elements) => {
            for element in elements {
                boundaries_in_expression(element, functions, found);
            }
        }
        TypedExpressionNodeView::Size { dimension, .. } => {
            boundaries_in_expression(dimension, functions, found);
        }
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_)
        | TypedExpressionNodeView::Ref(_)
        | TypedExpressionNodeView::Neg(_) => {}
    }
}

/// The region slot an argument names *as a whole object*, or `None` for
/// anything else: a subscripted reference names an element, a state reference
/// is not overlaid storage, and a computed argument has no address at all.
fn whole_region_slot<'a>(argument: &TypedExpressionView<'a>) -> Option<&'a str> {
    let TypedExpressionNodeView::Ref(reference) = &argument.node else {
        return None;
    };
    if !reference.context_resident {
        return None;
    }
    let TypedReferenceNodeView::Local(part) = &reference.node else {
        return None;
    };
    part.subscripts.is_empty().then(|| part.name.lexeme())
}

/// Read the block's call-boundary addresses off its projected bodies and widen
/// the layout for them.
///
/// Runs after placement, because a bound-clash address is a property of a
/// GROUP: two regions' slots meeting at one offset. Placement itself is
/// untouched: the equalization only widens declarations and never moves a
/// region between groups, so the never-concurrent proof is the same one either
/// way.
fn equalize_block_bounds<'a>(
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
    methods: [&mut TypedMethodView<'a>; 3],
    mut user_functions: Vec<&mut TypedFunctionView<'a>>,
) {
    const SPELLINGS: [&str; 3] = ["startup", "recalibrate", "dostep"];
    let owners: Vec<(Owner<'a>, &[TypedSpannedStatement<'a>])> = SPELLINGS
        .iter()
        .zip(&methods)
        .map(|(spelling, method)| (Owner::Method(spelling), method.statements.as_slice()))
        .chain(user_functions.iter().map(|function| {
            (
                Owner::Function(function.name.lexeme()),
                function.statements.as_slice(),
            )
        }))
        .collect();
    let boundaries = call_boundary_bounds(functions, owners.into_iter());
    equalize_bounds(
        methods
            .into_iter()
            .map(|method| &mut method.scratch)
            .chain(user_functions.iter_mut().map(|f| &mut f.scratch)),
        &boundaries,
    );
}

/// Declare every array sharing a call-boundary bound-clash address wide enough
/// for the bound that address is handed to.
///
/// Group by group, because a clash is between the slots of two regions that
/// share one union. See `algorithm_code_bound_equalization` for the rule; this
/// only assembles its input, applies its answer, and re-adds the sizes.
///
/// A group holding one slot this projection cannot size is skipped whole: an
/// offset model over an unsizable member would be a guess, and every target
/// that prints regions fails closed on such a slot anyway.
fn equalize_bounds<'a, 'view>(
    regions: impl Iterator<Item = &'view mut ScratchRegionView<'a>>,
    boundaries: &BTreeMap<Owner<'a>, BTreeMap<&'a str, usize>>,
) where
    'a: 'view,
{
    let mut regions: Vec<&'view mut ScratchRegionView<'a>> =
        regions.filter(|region| region.used).collect();
    let groups: BTreeSet<usize> = regions.iter().map(|region| region.group).collect();
    for group in groups {
        let members: Vec<usize> = regions
            .iter()
            .enumerate()
            .filter(|(_, region)| region.group == group)
            .map(|(index, _)| index)
            .collect();
        let Some(bounds) = group_bounds(&regions, &members, boundaries) else {
            continue;
        };
        let extents = bound_equalization::equalize(&bounds);
        for (position, index) in members.into_iter().enumerate() {
            adopt_extents(regions[index], &extents[position]);
        }
    }
}

/// One group's slots in the shape the equalization takes them, or `None` where
/// any of them cannot be sized.
fn group_bounds<'a>(
    regions: &[&mut ScratchRegionView<'a>],
    members: &[usize],
    boundaries: &BTreeMap<Owner<'a>, BTreeMap<&'a str, usize>>,
) -> Option<bound_equalization::GroupBounds> {
    members
        .iter()
        .map(|index| region_bounds(&*regions[*index], boundaries))
        .collect()
}

fn region_bounds<'a>(
    region: &ScratchRegionView<'a>,
    boundaries: &BTreeMap<Owner<'a>, BTreeMap<&'a str, usize>>,
) -> Option<Vec<Vec<SlotBounds>>> {
    let handed = boundaries.get(&region.owner);
    region
        .members
        .iter()
        .map(|member| {
            member
                .slots
                .iter()
                .map(|slot| bounds_of(slot, handed))
                .collect()
        })
        .collect()
}

/// One slot's shape, carrying the largest declared parameter bound the emitted
/// code hands this slot's address to.
fn bounds_of<'a>(
    slot: &ScratchSlotView<'a>,
    handed: Option<&BTreeMap<&'a str, usize>>,
) -> Option<SlotBounds> {
    let mut bounds = slot.bounds()?;
    bounds.call_boundary_bytes = handed.and_then(|handed| handed.get(slot.lexeme()).copied());
    Some(bounds)
}

/// Declare one region's slots with the extents the equalization chose, and
/// re-add the sizes that follow from them.
fn adopt_extents(region: &mut ScratchRegionView<'_>, extents: &[Vec<usize>]) {
    for (member, leading) in region.members.iter_mut().zip(extents) {
        for (slot, leading) in member.slots.iter_mut().zip(leading) {
            slot.widen_leading_to(*leading);
        }
        member.bytes = widest_bytes(member.slots.iter().map(|slot| slot.bytes));
    }
    resize(region);
}

/// Re-add a region's total from the storage it now holds: its declared
/// members plus its value arena.
///
/// One function rather than the expression repeated at each site that changes
/// a region's storage. The arena is a member of the region struct like any
/// other, and a total that forgot it would understate every budget the header
/// and the manifest publish; the equalization pass dropped exactly that way
/// before this existed.
fn resize(region: &mut ScratchRegionView<'_>) {
    let arena = region
        .arena
        .as_ref()
        .map_or(0, |arena| arena.floats * ARENA_ALIGN);
    region.bytes =
        total_bytes(region.members.iter().map(|member| member.bytes)).map(|bytes| bytes + arena);
}

/// Every place the emitted statements name each context-resident slot, in the
/// coordinates the arena prover reasons over: the scope chain down from the
/// owner's body plus the statement index inside the innermost scope.
///
/// # Why this reads the projected statements
///
/// The same reason [`call_boundary_bounds`] does: the live ranges the arena is
/// placed from are ranges in the statements a target *prints*. The marshalling
/// rewrite moves a slot's read to the call it is forwarded into, and a kernel
/// carries respelled operands of its own, so a walk over the checked AST would
/// place a slot from mentions the file no longer has and miss the ones it
/// gained.
///
/// # The over-approximation contract
///
/// A recorded use only ever *widens* a live range, so this walk is free to
/// record generously and forbidden to miss: it walks the node of every
/// statement (including one a kernel absorbed or replaced, whose mentions the
/// surviving kernel call carries at a neighbouring index) and every kernel
/// payload beside it. What it must never do is skip a construct the templates
/// print a slot's name in; the match arms below are exhaustive so a new
/// statement or expression form fails the build here rather than silently
/// shortening a live range.
///
/// The generosity is measured, not assumed. Restricting the walk to exactly
/// the statements the templates print (skipping the node beside an absorbed,
/// marshalled, index-restricted or contraction kernel) was tried on the RDD2
/// navigation estimator and moved the block's slot budget by zero bytes, so
/// the safe reading is kept: a use this walk invents costs bytes at worst,
/// and a use it misses is a silent wrong-code defect.
fn arena_slot_uses<'a>(
    statements: &[TypedSpannedStatement<'a>],
) -> BTreeMap<&'a str, Vec<SlotUse>> {
    let mut found = BTreeMap::new();
    let mut scopes: ScopePath = Vec::new();
    arena_uses_in_statements(statements, &mut scopes, &mut found);
    found
}

fn arena_record_use<'a>(
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
    name: &'a str,
    scopes: &ScopePath,
    statement: usize,
) {
    let place = SlotUse {
        scopes: scopes.clone(),
        statement,
    };
    let uses = found.entry(name).or_default();
    if uses.last() != Some(&place) {
        uses.push(place);
    }
}

fn arena_uses_in_statements<'a>(
    statements: &[TypedSpannedStatement<'a>],
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    for (index, statement) in statements.iter().enumerate() {
        if let Some(kernel) = &statement.kernel {
            arena_uses_in_kernel(kernel, index, scopes, found);
        }
        arena_uses_in_statement(&statement.node, index, scopes, found);
    }
}

fn arena_uses_in_kernel<'a>(
    kernel: &KernelStatementView<'a>,
    index: usize,
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    match kernel {
        // An absorbed statement prints nothing; its own node is still walked
        // by the caller, which is the generous side of the contract.
        KernelStatementView::Absorbed => {}
        KernelStatementView::Marshalled { statement }
        | KernelStatementView::IndexRestricted { statement } => {
            arena_uses_in_statement(statement, index, scopes, found);
        }
        KernelStatementView::Fill { value } => {
            arena_uses_in_expression(value, index, scopes, found);
        }
        KernelStatementView::Dot {
            target, lhs, rhs, ..
        } => {
            arena_uses_in_reference(target, index, scopes, found);
            arena_uses_in_reference(lhs, index, scopes, found);
            arena_uses_in_reference(rhs, index, scopes, found);
        }
        KernelStatementView::ScaledAdd {
            zero,
            target,
            scale,
            source,
            ..
        } => {
            arena_uses_in_expression(zero, index, scopes, found);
            arena_uses_in_reference(target, index, scopes, found);
            arena_uses_in_expression(scale, index, scopes, found);
            arena_uses_in_reference(source, index, scopes, found);
        }
        KernelStatementView::ScaledAddFused {
            zero,
            target,
            scale,
            source,
            store_target,
            store_value,
            ..
        } => {
            arena_uses_in_expression(zero, index, scopes, found);
            arena_uses_in_reference(target, index, scopes, found);
            arena_uses_in_expression(scale, index, scopes, found);
            arena_uses_in_reference(source, index, scopes, found);
            arena_uses_in_reference(store_target, index, scopes, found);
            arena_uses_in_expression(store_value, index, scopes, found);
        }
    }
}

fn arena_uses_in_statement<'a>(
    statement: &TypedStatementView<'a>,
    index: usize,
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    match statement {
        TypedStatementView::Assignment { target, value } => {
            arena_uses_in_reference(target, index, scopes, found);
            arena_uses_in_expression(value, index, scopes, found);
        }
        TypedStatementView::MultiAssignment { targets, call } => {
            for target in targets {
                arena_uses_in_reference(target, index, scopes, found);
            }
            arena_uses_in_call(call, index, scopes, found);
        }
        TypedStatementView::Call(call) => arena_uses_in_call(call, index, scopes, found),
        TypedStatementView::If(conditional) => {
            for (branch_index, branch) in conditional.branches.iter().enumerate() {
                arena_uses_in_condition(&branch.condition, index, scopes, found);
                scopes.push(ScopeStep::IfBranch {
                    statement: index,
                    branch: branch_index,
                });
                arena_uses_in_statements(&branch.body, scopes, found);
                scopes.pop();
            }
            if let Some(body) = &conditional.else_body {
                scopes.push(ScopeStep::IfElse { statement: index });
                arena_uses_in_statements(body, scopes, found);
                scopes.pop();
            }
        }
        TypedStatementView::For(loop_) => {
            arena_uses_in_expression(&loop_.start, index, scopes, found);
            if let Some(step) = &loop_.step {
                arena_uses_in_expression(step, index, scopes, found);
            }
            arena_uses_in_expression(&loop_.stop, index, scopes, found);
            scopes.push(ScopeStep::ForBody { statement: index });
            arena_uses_in_statements(&loop_.body, scopes, found);
            scopes.pop();
        }
        TypedStatementView::Limit(targets) => {
            for target in targets {
                match target {
                    TypedLimitTargetView::SelfState => {}
                    TypedLimitTargetView::Reference(reference) => {
                        arena_uses_in_reference(reference, index, scopes, found);
                    }
                }
            }
        }
        TypedStatementView::Signal(_) => {}
    }
}

/// A branch condition's mentions. A signal check names its closure by spelling
/// alone, and recording that keeps a slot of the same name live over the whole
/// conditional rather than guessing it is not the one meant.
fn arena_uses_in_condition<'a>(
    condition: &TypedConditionView<'a>,
    index: usize,
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    match condition {
        TypedConditionView::Expression(value) => {
            arena_uses_in_expression(value, index, scopes, found);
        }
        TypedConditionView::SignalCheck(check) => {
            if let Some(closure) = &check.closure {
                arena_record_use(found, closure.as_str(), scopes, index);
            }
        }
    }
}

fn arena_uses_in_call<'a>(
    call: &TypedCallView<'a>,
    index: usize,
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    for argument in &call.arguments {
        arena_uses_in_expression(argument, index, scopes, found);
    }
}

fn arena_uses_in_reference<'a>(
    reference: &TypedReferenceView<'a>,
    index: usize,
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    let parts: &[TypedRefPartView<'a>] = match &reference.node {
        TypedReferenceNodeView::Local(part) => {
            if reference.context_resident {
                arena_record_use(found, part.name.lexeme(), scopes, index);
            }
            std::slice::from_ref(part)
        }
        TypedReferenceNodeView::State(parts) => parts.as_slice(),
    };
    for subscript in parts.iter().flat_map(|part| &part.subscripts) {
        arena_uses_in_expression(subscript, index, scopes, found);
    }
}

fn arena_uses_in_expression<'a>(
    value: &TypedExpressionView<'a>,
    index: usize,
    scopes: &mut ScopePath,
    found: &mut BTreeMap<&'a str, Vec<SlotUse>>,
) {
    match &value.node {
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_) => {}
        TypedExpressionNodeView::Ref(reference) | TypedExpressionNodeView::Neg(reference) => {
            arena_uses_in_reference(reference, index, scopes, found);
        }
        TypedExpressionNodeView::Size { array, dimension } => {
            arena_uses_in_reference(array, index, scopes, found);
            arena_uses_in_expression(dimension, index, scopes, found);
        }
        TypedExpressionNodeView::Call(call) => arena_uses_in_call(call, index, scopes, found),
        TypedExpressionNodeView::Paren(inner) | TypedExpressionNodeView::Not(inner) => {
            arena_uses_in_expression(inner, index, scopes, found);
        }
        TypedExpressionNodeView::If(conditional) => {
            for (condition, branch) in &conditional.branches {
                arena_uses_in_expression(condition, index, scopes, found);
                arena_uses_in_expression(branch, index, scopes, found);
            }
            arena_uses_in_expression(&conditional.else_value, index, scopes, found);
        }
        TypedExpressionNodeView::BoundedSelection(selection) => {
            arena_uses_in_reference(&selection.reference, index, scopes, found);
            for (condition, branch) in &selection.galec.branches {
                arena_uses_in_expression(condition, index, scopes, found);
                arena_uses_in_expression(branch, index, scopes, found);
            }
            arena_uses_in_expression(&selection.galec.else_value, index, scopes, found);
        }
        TypedExpressionNodeView::Array(elements) => {
            for element in elements {
                arena_uses_in_expression(element, index, scopes, found);
            }
        }
        TypedExpressionNodeView::Binary { lhs, rhs, .. } => {
            arena_uses_in_expression(lhs, index, scopes, found);
            arena_uses_in_expression(rhs, index, scopes, found);
        }
    }
}

/// Whether one slot may enter its region's value arena at all.
///
/// Everything here is a fail-closed gate, not a proof; the proof is the
/// pairwise relation the placement consumes.
///
/// * Only a **`Real` array** with literal extents: the arena is a `float`
///   array, so a `Real` resident is reached through pointers of its own
///   element type and every access stays a `float` access into it; an
///   `Integer` or `Boolean` slot would need an arena of its own type and is
///   left as a declared member instead.
/// * Not a name in `withheld`: a function's **output parameters** are read
///   back by the caller after the function returns, a lifetime no analysis of
///   this body can see, so they keep declared members exactly as they keep
///   their exclusion from the marshalling retirement; and a slot spelled like
///   a **user function** would shadow that function's file-scope definition
///   once it becomes a block-scope pointer.
/// * A use the walk recorded: a slot the printed statements never name has no
///   live range to place.
fn eligible_for_arena<'a>(
    slot: &ScratchSlotView<'a>,
    withheld: &HashSet<&'a str>,
    uses: &arena::SlotUses<'a>,
) -> bool {
    slot.scalar == Some(ast::ScalarType::Real)
        && slot
            .extents
            .as_ref()
            .is_some_and(|extents| !extents.is_empty())
        && slot
            .bytes
            .is_some_and(|bytes| bytes > 0 && bytes % ARENA_ALIGN == 0)
        && !withheld.contains(slot.lexeme())
        && uses.knows(slot.lexeme())
}

/// Give one owner's region a value arena, from the statements a target prints.
///
/// Runs once per owner, after the projection's fixed point has settled, so the
/// live ranges are ranges in the final statement list; and before the regions
/// are placed into groups, so the group overlay and its floor are computed
/// from the shrunken region sizes.
///
/// The rewrite is layout only. Every slot keeps its extents and its element
/// order; what changes is where the slot's first byte sits (a fixed offset in
/// one `float` array instead of a declared member) and how a target spells the
/// access path. No statement, no operand and no operation order is touched, so
/// the emitted arithmetic is bit-identical with and without the arena.
fn adopt_arena<'a>(
    region: &mut ScratchRegionView<'a>,
    statements: &mut [TypedSpannedStatement<'a>],
    withheld: &HashSet<&'a str>,
    functions: &HashMap<&'a str, &'a ast::UserFunction>,
) {
    if region.members.is_empty() {
        return;
    }
    let uses = arena::SlotUses::observed(arena_slot_uses(statements));
    // The addresses this owner hands to a declared array parameter, read off
    // the same statements a target prints and by the same walk the bound
    // equalization uses. At those offsets a C compiler asks how large the
    // object is, so two declared shapes may not meet there.
    let mut boundaries = BTreeMap::new();
    boundaries_in_statements(statements, functions, &mut boundaries);
    let flattened: Vec<&ScratchSlotView<'a>> = region
        .members
        .iter()
        .flat_map(|member| member.slots.iter())
        .collect();
    let eligible: Vec<arena::ArenaCandidate<'a>> = flattened
        .iter()
        .enumerate()
        .filter(|(_, slot)| eligible_for_arena(slot, withheld, &uses))
        .map(|(index, slot)| arena::ArenaCandidate {
            name: slot.lexeme(),
            index,
            bytes: slot.bytes,
            extents: slot.extents.clone().unwrap_or_default(),
            call_boundary: boundaries.contains_key(slot.lexeme()),
        })
        .collect();
    if eligible.len() < 2 {
        return;
    }
    let resolved = arena::plan(&uses, &eligible);
    let absorbed: usize = eligible
        .iter()
        .filter(|candidate| resolved.offsets.contains_key(&candidate.index))
        .filter_map(|candidate| candidate.bytes)
        .sum();
    // An arena that packs nothing tighter than the members it replaces is not
    // taken: the members stay, the header stays legible, and nothing changes.
    if resolved.offsets.len() < 2 || resolved.bytes >= absorbed {
        return;
    }
    let mut placed: HashMap<&'a str, usize> = HashMap::new();
    let mut slots: Vec<ArenaSlotView<'a>> = Vec::new();
    for (index, offset) in &resolved.offsets {
        let slot = flattened[*index];
        placed.insert(slot.lexeme(), *offset);
        slots.push(ArenaSlotView {
            slot: slot.clone(),
            offset: offset / ARENA_ALIGN,
            floats: slot.bytes.unwrap_or(0) / ARENA_ALIGN,
        });
    }
    slots.sort_by(|left, right| {
        left.offset
            .cmp(&right.offset)
            .then_with(|| left.slot.lexeme().cmp(right.slot.lexeme()))
    });
    for member in &mut region.members {
        member
            .slots
            .retain(|slot| !placed.contains_key(slot.lexeme()));
        member.bytes = widest_bytes(member.slots.iter().map(|slot| slot.bytes));
    }
    region.members.retain(|member| !member.slots.is_empty());
    region.arena = Some(ArenaRegionView {
        floats: resolved.bytes / ARENA_ALIGN,
        slots,
    });
    resize(region);
    region.used = true;
    let names: HashSet<&'a str> = placed.into_keys().collect();
    mark_arena_references(statements, &names);
}

/// Set [`TypedReferenceView::context_arena`] on every reference that names an
/// arena-placed slot, kernel payloads included.
///
/// A missed reference here is not a wrong value, it is a failed build: the
/// slot's declared member is gone from the header, so a spelling that still
/// says `ctx-><slot>` does not compile. The walk therefore mirrors
/// [`arena_slot_uses`] arm for arm.
fn mark_arena_references<'a>(
    statements: &mut [TypedSpannedStatement<'a>],
    placed: &HashSet<&'a str>,
) {
    for statement in statements {
        if let Some(kernel) = &mut statement.kernel {
            mark_arena_in_kernel(kernel, placed);
        }
        mark_arena_in_statement(&mut statement.node, placed);
    }
}

fn mark_arena_in_kernel<'a>(kernel: &mut KernelStatementView<'a>, placed: &HashSet<&'a str>) {
    match kernel {
        KernelStatementView::Absorbed => {}
        KernelStatementView::Marshalled { statement }
        | KernelStatementView::IndexRestricted { statement } => {
            mark_arena_in_statement(statement, placed);
        }
        KernelStatementView::Fill { value } => mark_arena_in_expression(value, placed),
        KernelStatementView::Dot {
            target, lhs, rhs, ..
        } => {
            mark_arena_in_reference(target, placed);
            mark_arena_in_reference(lhs, placed);
            mark_arena_in_reference(rhs, placed);
        }
        KernelStatementView::ScaledAdd {
            zero,
            target,
            scale,
            source,
            ..
        } => {
            mark_arena_in_expression(zero, placed);
            mark_arena_in_reference(target, placed);
            mark_arena_in_expression(scale, placed);
            mark_arena_in_reference(source, placed);
        }
        KernelStatementView::ScaledAddFused {
            zero,
            target,
            scale,
            source,
            store_target,
            store_value,
            ..
        } => {
            mark_arena_in_expression(zero, placed);
            mark_arena_in_reference(target, placed);
            mark_arena_in_expression(scale, placed);
            mark_arena_in_reference(source, placed);
            mark_arena_in_reference(store_target, placed);
            mark_arena_in_expression(store_value, placed);
        }
    }
}

fn mark_arena_in_statement<'a>(statement: &mut TypedStatementView<'a>, placed: &HashSet<&'a str>) {
    match statement {
        TypedStatementView::Assignment { target, value } => {
            mark_arena_in_reference(target, placed);
            mark_arena_in_expression(value, placed);
        }
        TypedStatementView::MultiAssignment { targets, call } => {
            for target in targets {
                mark_arena_in_reference(target, placed);
            }
            mark_arena_in_call(call, placed);
        }
        TypedStatementView::Call(call) => mark_arena_in_call(call, placed),
        TypedStatementView::If(conditional) => {
            for branch in &mut conditional.branches {
                if let TypedConditionView::Expression(condition) = &mut branch.condition {
                    mark_arena_in_expression(condition, placed);
                }
                mark_arena_references(&mut branch.body, placed);
            }
            if let Some(body) = &mut conditional.else_body {
                mark_arena_references(body, placed);
            }
        }
        TypedStatementView::For(loop_) => {
            mark_arena_in_expression(&mut loop_.start, placed);
            if let Some(step) = &mut loop_.step {
                mark_arena_in_expression(step, placed);
            }
            mark_arena_in_expression(&mut loop_.stop, placed);
            mark_arena_references(&mut loop_.body, placed);
        }
        TypedStatementView::Limit(targets) => {
            for target in targets {
                if let TypedLimitTargetView::Reference(reference) = target {
                    mark_arena_in_reference(reference, placed);
                }
            }
        }
        TypedStatementView::Signal(_) => {}
    }
}

fn mark_arena_in_call<'a>(call: &mut TypedCallView<'a>, placed: &HashSet<&'a str>) {
    for argument in &mut call.arguments {
        mark_arena_in_expression(argument, placed);
    }
}

fn mark_arena_in_reference<'a>(reference: &mut TypedReferenceView<'a>, placed: &HashSet<&'a str>) {
    if reference.context_resident
        && let TypedReferenceNodeView::Local(part) = &reference.node
        && placed.contains(part.name.lexeme())
    {
        reference.context_arena = true;
    }
    match &mut reference.node {
        TypedReferenceNodeView::Local(part) => {
            for subscript in &mut part.subscripts {
                mark_arena_in_expression(subscript, placed);
            }
        }
        TypedReferenceNodeView::State(parts) => {
            for subscript in parts.iter_mut().flat_map(|part| &mut part.subscripts) {
                mark_arena_in_expression(subscript, placed);
            }
        }
    }
}

fn mark_arena_in_expression<'a>(value: &mut TypedExpressionView<'a>, placed: &HashSet<&'a str>) {
    match &mut value.node {
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_) => {}
        TypedExpressionNodeView::Ref(reference) | TypedExpressionNodeView::Neg(reference) => {
            mark_arena_in_reference(reference, placed);
        }
        TypedExpressionNodeView::Size { array, dimension } => {
            mark_arena_in_reference(array, placed);
            mark_arena_in_expression(dimension, placed);
        }
        TypedExpressionNodeView::Call(call) => mark_arena_in_call(call, placed),
        TypedExpressionNodeView::Paren(inner) | TypedExpressionNodeView::Not(inner) => {
            mark_arena_in_expression(inner, placed);
        }
        TypedExpressionNodeView::If(conditional) => {
            for (condition, branch) in &mut conditional.branches {
                mark_arena_in_expression(condition, placed);
                mark_arena_in_expression(branch, placed);
            }
            mark_arena_in_expression(&mut conditional.else_value, placed);
        }
        TypedExpressionNodeView::BoundedSelection(selection) => {
            mark_arena_in_reference(&mut selection.reference, placed);
            for (condition, branch) in &mut selection.galec.branches {
                mark_arena_in_expression(condition, placed);
                mark_arena_in_expression(branch, placed);
            }
            mark_arena_in_expression(&mut selection.galec.else_value, placed);
        }
        TypedExpressionNodeView::Array(elements) => {
            for element in elements {
                mark_arena_in_expression(element, placed);
            }
        }
        TypedExpressionNodeView::Binary { lhs, rhs, .. } => {
            mark_arena_in_expression(lhs, placed);
            mark_arena_in_expression(rhs, placed);
        }
    }
}

/// Whether a declaration is delivered through the owner's context region
/// rather than the frame.
///
/// Two rules, and only two:
///
/// * **Array locals.** They are what fills a frame — a 15x15 `float`
///   intermediate is 900 bytes — and they are the reason a worst-case stack
///   depth has to be argued rather than read off a link map. Scalars stay in
///   the frame: they cost a word, and pushing them behind the context pointer
///   would take registers away from the arithmetic for nothing.
/// * **Output parameters, whatever their rank.** These carry two costs today:
///   a full-size frame buffer inside the callee *and* a formal parameter at
///   every signature. Delivering them through the region removes both.
fn context_resident(declaration: &ast::VariableDeclaration, is_output: bool) -> bool {
    is_output || !declaration.dimensions.is_empty()
}

/// The declarations the marshalling rewrite may remove from a region: the
/// owner's own locals, and nothing else.
///
/// The owner is projected once per answer this returns, because the region, the
/// arm overlay inside it and every reference that names a slot are all derived
/// from the surviving slot set. A projection carrying a retired slot would
/// place it, size the region for it and hand every reference an overlay ordinal
/// chosen with it present, so the only way to see the layout without it is to
/// derive the layout again. The set grows monotonically and is bounded by the
/// declaration list, so the loop that does this terminates; in practice it
/// settles on the second projection, because a forwarding decision reads only
/// the statement structure and the declared shapes, neither of which the first
/// answer changed.
fn retirable_locals(locals: &[ast::VariableDeclaration]) -> HashSet<&str> {
    locals
        .iter()
        .map(|declaration| declaration.name.lexeme())
        .collect()
}

/// The frame declarations at one scope, each carrying whether the target still
/// owes it an unused-entity marker.
///
/// `frame_at` — not `at` — at **every** scope, including nested ones. An
/// array-valued local lives in the block's working memory; declaring one in the
/// frame as well produced an automatic object that nothing ever named, and the
/// unconditional `(void)&x;` that used to ride on every declaration hid it. It
/// was 207 dead declarations in the RDD2 estimator alone, and `-Wunused-
/// variable` names every one of them the moment the marker is emitted only
/// where it is needed.
fn frame_locals<'a>(
    placements: &LocalPlacements<'a>,
    path: &[ScopeStep],
) -> Vec<TypedLocalView<'a>> {
    placements
        .frame_at(path)
        .into_iter()
        .map(|decl| TypedLocalView {
            decl,
            needs_unused_marker: !placements.is_read(decl.name.lexeme()),
        })
        .collect()
}

/// A function's input parameters, each carrying whether the body mentions it.
///
/// Checked construction proves an input is never written, so a mention is a
/// read and the two questions collapse into one.
fn input_parameters<'a>(function: &'a ast::UserFunction) -> Vec<TypedParameterView<'a>> {
    let inputs: Vec<_> = function
        .parameters
        .iter()
        .filter(|parameter| parameter.direction == ast::Direction::Input)
        .collect();
    let names = inputs
        .iter()
        .map(|parameter| parameter.decl.name.lexeme())
        .collect::<HashSet<_>>();
    let mentioned = super::algorithm_code_scopes::mentioned(&names, &function.statements);
    inputs
        .into_iter()
        .map(|parameter| TypedParameterView {
            parameter,
            needs_unused_marker: !mentioned.contains(parameter.decl.name.lexeme()),
        })
        .collect()
}

/// A function's output parameters, in signature order. That order is the one a
/// multi-target call writes its targets in.
fn output_parameters(function: &ast::UserFunction) -> impl Iterator<Item = &ast::Parameter> {
    function
        .parameters
        .iter()
        .filter(|parameter| parameter.direction == ast::Direction::Output)
}

/// Every function name a statement sequence calls, at any depth.
///
/// # This walk is the overlay's single soundness input
///
/// `require_acyclic_calls` and [`call_depths`] both derive the call graph from
/// here, and the working-memory overlay is sound only because that graph is
/// complete: a call edge this walk does not see is a caller and a callee the
/// overlay does not separate, which is a silent wrong-code defect and not a
/// missing optimization.
///
/// It therefore walks **every expression position the AST admits**, including
/// ones no checked block reaches today — an assignment target's subscripts, a
/// `limit` target's subscripts, a signal check's fallback. Several are
/// unreachable because the validator constrains the form or because the C
/// targets fail closed on a user call in expression position. "Unreachable
/// today" is not a property this walk may depend on: it is target-neutral, and
/// the cost of walking a branch that never fires is nothing.
///
/// When adding an AST node, add it here, and prefer an exhaustive `match` over
/// a catch-all so that the compiler asks the question.
fn collect_called_functions<'a>(
    statements: &'a [ast::Spanned<ast::Statement>],
    called: &mut BTreeSet<&'a str>,
) {
    for statement in statements {
        match &statement.node {
            // An assignment TARGET is not a call, but its subscripts are
            // expressions and may contain one.
            ast::Statement::Assignment { target, value } => {
                collect_called_in_reference(target, called);
                collect_called_in_expression(value, called);
            }
            ast::Statement::MultiAssignment { targets, call } => {
                for target in targets {
                    collect_called_in_reference(target, called);
                }
                called.insert(call.function.lexeme());
                for argument in &call.arguments {
                    collect_called_in_expression(argument, called);
                }
            }
            ast::Statement::Call(call) => {
                called.insert(call.function.lexeme());
                for argument in &call.arguments {
                    collect_called_in_expression(argument, called);
                }
            }
            ast::Statement::If(conditional) => {
                collect_called_in_conditional(conditional, called);
            }
            ast::Statement::For(for_loop) => {
                collect_called_in_expression(&for_loop.start, called);
                if let Some(step) = &for_loop.step {
                    collect_called_in_expression(step, called);
                }
                collect_called_in_expression(&for_loop.stop, called);
                collect_called_functions(&for_loop.body, called);
            }
            ast::Statement::Limit(targets) => {
                collect_called_in_limit_targets(targets, called);
            }
            ast::Statement::Signal(_) => {}
        }
    }
}

fn collect_called_in_limit_targets<'a>(
    targets: &'a [ast::LimitTarget],
    called: &mut BTreeSet<&'a str>,
) {
    for reference in targets.iter().filter_map(|target| match target {
        ast::LimitTarget::SelfState => None,
        ast::LimitTarget::Reference(reference) => Some(reference),
    }) {
        collect_called_in_reference(reference, called);
    }
}

fn collect_called_in_conditional<'a>(
    conditional: &'a ast::IfStatement,
    called: &mut BTreeSet<&'a str>,
) {
    for branch in &conditional.branches {
        match &branch.condition {
            ast::Condition::Expression(expression) => {
                collect_called_in_expression(expression, called);
            }
            // A signal check's fallback is an expression like any other. No
            // checked block reaches here with a call in it today — the
            // validator constrains the form — but see the note on
            // `collect_called_functions`: a call edge this walk does not see is
            // a call edge the overlay does not separate.
            ast::Condition::SignalCheck(check) => {
                if let Some(fallback) = &check.fallback {
                    collect_called_in_expression(fallback, called);
                }
            }
        }
        collect_called_functions(&branch.body, called);
    }
    if let Some(body) = &conditional.else_body {
        collect_called_functions(body, called);
    }
}

/// Every function name a reference's subscripts call.
///
/// A subscript is an expression, so `x[f(i)]` is a call edge. The C targets
/// fail closed on a user call in expression position rather than emitting one,
/// so this is unreachable through them — but the collector is target-neutral
/// and is the overlay's single soundness input, so it walks what the AST
/// allows rather than what one target happens to accept.
fn collect_called_in_reference<'a>(reference: &'a ast::Reference, called: &mut BTreeSet<&'a str>) {
    let parts = match reference {
        ast::Reference::Local(part) => std::slice::from_ref(part),
        ast::Reference::State(parts) => parts.as_slice(),
    };
    for subscript in parts.iter().flat_map(|part| &part.subscripts) {
        collect_called_in_expression(subscript, called);
    }
}

fn collect_called_in_expression<'a>(
    expression: &'a ast::Expression,
    called: &mut BTreeSet<&'a str>,
) {
    match expression {
        ast::Expression::Call(call) => {
            called.insert(call.function.lexeme());
            for argument in &call.arguments {
                collect_called_in_expression(argument, called);
            }
        }
        ast::Expression::Paren(value) | ast::Expression::Not(value) => {
            collect_called_in_expression(value, called);
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_called_in_expression(lhs, called);
            collect_called_in_expression(rhs, called);
        }
        ast::Expression::If(value) => {
            for branch in &value.branches {
                collect_called_in_expression(&branch.0, called);
                collect_called_in_expression(&branch.1, called);
            }
            collect_called_in_expression(&value.else_value, called);
        }
        ast::Expression::Array(values) => {
            for value in values {
                collect_called_in_expression(value, called);
            }
        }
        ast::Expression::Size { array, dimension } => {
            collect_called_in_reference(array, called);
            collect_called_in_expression(dimension, called);
        }
        // A reference is not a call, but its SUBSCRIPTS are expressions.
        ast::Expression::Ref(reference) | ast::Expression::Neg(reference) => {
            collect_called_in_reference(reference, called);
        }
        ast::Expression::Bool(_) | ast::Expression::Integer(_) | ast::Expression::Real(_) => {}
    }
}

struct BlockShapes<'a> {
    state: HashMap<&'a str, &'a ast::VariableDeclaration>,
    compartments: HashMap<&'a str, &'a ast::StateCompartment>,
    functions: HashMap<&'a str, &'a ast::UserFunction>,
    semantic_uses: RefCell<SemanticOperationUses>,
    /// Set while lowering one owner's body when a reference resolves into that
    /// owner's context region. Read straight after, so a target knows whether
    /// an alias to the region would be read at all.
    context_reads: RefCell<bool>,
    traces: SourceTraceResolver<'a>,
}

impl<'a> BlockShapes<'a> {
    fn new(block: &'a ast::Block, sources: &'a rumoca_core::SourceMap) -> Self {
        let state = block
            .interface
            .iter()
            .map(|variable| (variable.decl.name.lexeme(), &variable.decl))
            .chain(
                block
                    .protected
                    .iter()
                    .map(|variable| (variable.decl.name.lexeme(), &variable.decl)),
            )
            .collect();
        let compartments = block
            .compartments
            .iter()
            .map(|compartment| (compartment.name.lexeme(), compartment))
            .collect();
        let functions = block
            .protected_functions
            .iter()
            .chain(&block.public_functions)
            .map(|function| (function.name.lexeme(), function))
            .collect();
        Self {
            state,
            compartments,
            functions,
            semantic_uses: RefCell::new(SemanticOperationUses::default()),
            context_reads: RefCell::new(false),
            traces: SourceTraceResolver::new(sources),
        }
    }

    /// Fail closed on a call cycle.
    ///
    /// Giving every function its own context region is sound exactly because a
    /// function is never simultaneously active with itself: the region's live
    /// range is the call. Recursion would break that, silently, by letting an
    /// inner activation overwrite the outer one's intermediates. Checked
    /// Algorithm Code is not supposed to contain a cycle; this refuses to
    /// generate rather than trust that.
    fn require_acyclic_calls(&self) -> Result<(), String> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Mark {
            Active,
            Done,
        }
        fn walk<'a>(
            name: &'a str,
            functions: &HashMap<&'a str, &'a ast::UserFunction>,
            marks: &mut HashMap<&'a str, Mark>,
        ) -> Result<(), String> {
            match marks.get(name) {
                Some(Mark::Done) => return Ok(()),
                Some(Mark::Active) => {
                    return Err(format!(
                        "checked Algorithm Code function `{name}` is reachable from itself; \
                         context-resident intermediates require an acyclic call graph"
                    ));
                }
                None => {}
            }
            marks.insert(name, Mark::Active);
            let mut callees = BTreeSet::new();
            if let Some(function) = functions.get(name) {
                collect_called_functions(&function.statements, &mut callees);
            }
            let reachable = callees
                .into_iter()
                .filter_map(|callee| functions.get_key_value(callee).map(|(key, _)| *key))
                .collect::<Vec<_>>();
            for callee in reachable {
                walk(callee, functions, marks)?;
            }
            marks.insert(name, Mark::Done);
            Ok(())
        }
        let mut marks = HashMap::new();
        let mut roots = self.functions.keys().copied().collect::<Vec<_>>();
        roots.sort_unstable();
        for name in roots {
            walk(name, &self.functions, &mut marks)?;
        }
        Ok(())
    }

    fn method(
        &self,
        method: &'a ast::BlockMethod,
        spelling: &'static str,
    ) -> Result<TypedMethodView<'a>, String> {
        let placements = LocalPlacements::derive(&method.locals, &method.statements);
        let retirable = retirable_locals(&method.locals);
        let mut retired = HashSet::new();
        loop {
            let slots = method
                .locals
                .iter()
                .filter(|declaration| {
                    placements.is_placed(declaration.name.lexeme())
                        && context_resident(declaration, false)
                        && !retired.contains(declaration.name.lexeme())
                })
                .collect::<Vec<_>>();
            let mut scratch = ScratchRegionView::new(Owner::Method(spelling), &slots, &placements);
            let scope = ScopeShapes::new(self, &method.locals, &scratch);
            *self.context_reads.borrow_mut() = false;
            let mut statements =
                scope.statements(&method.statements, &placements, &mut Vec::new())?;
            let frame = frame_locals(&placements, &[]);
            let absorbed = kernelize(&mut statements, &frame);
            let forwarded = marshalling::forward(&mut statements, &retirable);
            if !forwarded.is_subset(&retired) {
                retired.extend(forwarded);
                continue;
            }
            // The statements are final; a method withholds nothing but the
            // user function names a block-scope pointer would shadow.
            let withheld: HashSet<&str> = self.functions.keys().copied().collect();
            adopt_arena(&mut scratch, &mut statements, &withheld, &self.functions);
            let uses_scratch = *self.context_reads.borrow();
            return Ok(TypedMethodView {
                signals: &method.signals,
                locals: &method.locals,
                c_locals: surviving_locals(frame, &absorbed),
                scratch,
                uses_scratch,
                definite_state_writes: definite_state_writes(&method.statements)
                    .into_iter()
                    .collect(),
                statements,
            });
        }
    }

    fn function(&self, function: &'a ast::UserFunction) -> Result<TypedFunctionView<'a>, String> {
        let placements = LocalPlacements::derive(&function.locals, &function.statements);
        // Only a local is retirable. An output parameter is a slot the caller
        // reads back after this function returns, and no analysis of this body
        // can see that use.
        let retirable = retirable_locals(&function.locals);
        let mut retired = HashSet::new();
        loop {
            // Declaration order inside the region: reachable array locals, then
            // the outputs. Both are addressed by name, so the order is only
            // about producing the same struct from the same block every time.
            let slots = function
                .locals
                .iter()
                .filter(|declaration| {
                    placements.is_placed(declaration.name.lexeme())
                        && context_resident(declaration, false)
                        && !retired.contains(declaration.name.lexeme())
                })
                .chain(output_parameters(function).map(|parameter| &parameter.decl))
                .collect::<Vec<_>>();
            let mut scratch = ScratchRegionView::new(
                Owner::Function(function.name.lexeme()),
                &slots,
                &placements,
            );
            let scope = ScopeShapes::for_function(self, function, &scratch);
            *self.context_reads.borrow_mut() = false;
            let mut statements =
                scope.statements(&function.statements, &placements, &mut Vec::new())?;
            let frame = frame_locals(&placements, &[]);
            let absorbed = kernelize(&mut statements, &frame);
            let forwarded = marshalling::forward(&mut statements, &retirable);
            if !forwarded.is_subset(&retired) {
                retired.extend(forwarded);
                continue;
            }
            // The statements are final. Outputs are withheld from the arena
            // because the caller reads them back after this function returns,
            // a live range no walk over this body can see; function names are
            // withheld because a block-scope pointer of that spelling would
            // shadow the file-scope definition.
            let withheld: HashSet<&str> = self
                .functions
                .keys()
                .copied()
                .chain(output_parameters(function).map(|parameter| parameter.decl.name.lexeme()))
                .collect();
            adopt_arena(&mut scratch, &mut statements, &withheld, &self.functions);
            let uses_scratch = *self.context_reads.borrow();
            return Ok(TypedFunctionView {
                kind: function.kind,
                name: &function.name,
                trace: self.traces.trace(&function.span),
                signals: &function.signals,
                parameters: &function.parameters,
                input_parameters: input_parameters(function),
                locals: &function.locals,
                c_locals: surviving_locals(frame, &absorbed),
                scratch,
                uses_scratch,
                statements,
            });
        }
    }

    fn state_reference_shape(&self, parts: &[ast::RefPart]) -> Result<ShapeEvidence, String> {
        let Some(first) = parts.first() else {
            return Err("checked state reference has no parts".to_owned());
        };
        let mut declaration = self
            .state
            .get(first.name.lexeme())
            .copied()
            .ok_or_else(|| {
                format!(
                    "checked state reference `{}` is unresolved",
                    first.name.lexeme()
                )
            })?;
        let mut remaining = reference_shape(declaration, first)?;
        for part in &parts[1..] {
            if remaining.rank != 0 {
                return Err(format!(
                    "checked component array `{}` is not fully subscripted",
                    declaration.name.lexeme()
                ));
            }
            let ast::TypeRef::Compartment(compartment_name) = &declaration.ty else {
                return Err(format!(
                    "checked multipart reference traverses primitive `{}`",
                    declaration.name.lexeme()
                ));
            };
            let compartment = self
                .compartments
                .get(compartment_name.lexeme())
                .copied()
                .ok_or_else(|| {
                    format!(
                        "checked compartment type `{}` is unresolved",
                        compartment_name.lexeme()
                    )
                })?;
            declaration = compartment
                .entities
                .iter()
                .map(|entity| &entity.decl)
                .find(|candidate| candidate.name.lexeme() == part.name.lexeme())
                .ok_or_else(|| {
                    format!(
                        "checked compartment field `{}` is unresolved",
                        part.name.lexeme()
                    )
                })?;
            remaining = reference_shape(declaration, part)?;
        }
        Ok(remaining)
    }

    fn call_shape(
        &self,
        call: &ast::FunctionCall,
        arguments: &[TypedExpressionView<'_>],
    ) -> Result<ShapeEvidence, String> {
        if let Some(function) = self.functions.get(call.function.lexeme()) {
            let output = function
                .parameters
                .iter()
                .find(|parameter| parameter.direction == ast::Direction::Output)
                .ok_or_else(|| {
                    format!(
                        "checked expression call `{}` has no output",
                        call.function.lexeme()
                    )
                })?;
            return Ok(declaration_shape(&output.decl));
        }
        if let Some(base) = rumoca_ir_galec::builtins::find_lifted_base(call.function.lexeme()) {
            let rank = usize::from(call.function.lexeme().ends_with("2D")) + 1;
            let mut argument_shapes = arguments
                .iter()
                .map(ShapeEvidence::of_expression)
                .filter(|shape| shape.rank > 0);
            let shape = argument_shapes.next().ok_or_else(|| {
                format!(
                    "checked lifted call `{}` has no array argument",
                    call.function.lexeme()
                )
            })?;
            if shape.rank != rank {
                return Err(format!(
                    "checked lifted call `{}` has rank {}, expected {rank}",
                    call.function.lexeme(),
                    shape.rank
                ));
            }
            for argument in argument_shapes {
                require_equal_shape(shape.clone(), argument, "checked lifted-call arguments")?;
            }
            if base.outputs.len() == 1 {
                return Ok(shape);
            }
        }
        let builtin = rumoca_ir_galec::builtins::find_builtin(call.function.lexeme())
            .ok_or_else(|| format!("checked call `{}` is unresolved", call.function.lexeme()))?;
        let [output] = builtin.outputs else {
            return Err(format!(
                "checked expression call `{}` does not have one output",
                call.function.lexeme()
            ));
        };
        Ok(ShapeEvidence {
            rank: builtin_rank(output.ty),
            extents: (builtin_rank(output.ty) == 0).then(Vec::new),
            scalar: Some(builtin_scalar(output.ty)),
        })
    }
}

struct ScopeShapes<'a, 'block> {
    block: &'block BlockShapes<'a>,
    locals: HashMap<&'a str, &'a ast::VariableDeclaration>,
    iterators: HashSet<&'a str>,
    /// The names the enclosing owner keeps in its context region, each with the
    /// arm overlay it landed in. A reference names a slot and needs both facts:
    /// whether the region holds it at all, and which member of the region it is
    /// reached through.
    context_slots: HashMap<&'a str, Option<usize>>,
}

impl<'a, 'block> ScopeShapes<'a, 'block> {
    fn new(
        block: &'block BlockShapes<'a>,
        locals: &'a [ast::VariableDeclaration],
        scratch: &ScratchRegionView<'a>,
    ) -> Self {
        Self {
            block,
            locals: locals
                .iter()
                .map(|declaration| (declaration.name.lexeme(), declaration))
                .collect(),
            iterators: HashSet::new(),
            context_slots: scratch.slot_overlays.clone(),
        }
    }

    fn for_function(
        block: &'block BlockShapes<'a>,
        function: &'a ast::UserFunction,
        scratch: &ScratchRegionView<'a>,
    ) -> Self {
        Self {
            block,
            locals: function
                .parameters
                .iter()
                .map(|parameter| (parameter.decl.name.lexeme(), &parameter.decl))
                .chain(
                    function
                        .locals
                        .iter()
                        .map(|declaration| (declaration.name.lexeme(), declaration)),
                )
                .collect(),
            iterators: HashSet::new(),
            context_slots: scratch.slot_overlays.clone(),
        }
    }

    fn with_iterator(&self, iterator: Option<&'a ast::Name>) -> Self {
        let mut iterators = self.iterators.clone();
        if let Some(iterator) = iterator {
            iterators.insert(iterator.lexeme());
        }
        Self {
            block: self.block,
            locals: self.locals.clone(),
            iterators,
            context_slots: self.context_slots.clone(),
        }
    }

    fn statements(
        &self,
        statements: &'a [ast::Spanned<ast::Statement>],
        placements: &LocalPlacements<'a>,
        path: &mut ScopePath,
    ) -> Result<Vec<TypedSpannedStatement<'a>>, String> {
        statements
            .iter()
            .enumerate()
            .map(|(statement_index, statement)| {
                Ok(TypedSpannedStatement {
                    trace: self.block.traces.trace(&statement.span),
                    kernel: None,
                    node: self.statement(&statement.node, statement_index, placements, path)?,
                })
            })
            .collect()
    }

    fn statement(
        &self,
        statement: &'a ast::Statement,
        statement_index: usize,
        placements: &LocalPlacements<'a>,
        path: &mut ScopePath,
    ) -> Result<TypedStatementView<'a>, String> {
        Ok(match statement {
            ast::Statement::Assignment { target, value } => {
                let target = self.reference(target)?;
                let value = self.expression(value)?;
                require_equal_shape(
                    ShapeEvidence::of_reference(&target),
                    ShapeEvidence::of_expression(&value),
                    "checked assignment",
                )?;
                TypedStatementView::Assignment { target, value }
            }
            ast::Statement::MultiAssignment { targets, call } => {
                TypedStatementView::MultiAssignment {
                    targets: targets
                        .iter()
                        .map(|target| self.reference(target))
                        .collect::<Result<_, _>>()?,
                    call: self.call(call)?,
                }
            }
            ast::Statement::Call(call) => TypedStatementView::Call(self.call(call)?),
            ast::Statement::If(statement) => {
                // The `else` body and its frame declarations have to be built
                // together, because kernelizing the body is what decides which
                // of those declarations survive.
                let (else_c_locals, else_body) = match statement.else_body.as_deref() {
                    None => (Vec::new(), None),
                    Some(body) => {
                        path.push(ScopeStep::IfElse {
                            statement: statement_index,
                        });
                        let c_locals = frame_locals(placements, path);
                        let body = self.statements(body, placements, path);
                        path.pop();
                        let mut body = body?;
                        let absorbed = kernelize(&mut body, &c_locals);
                        (surviving_locals(c_locals, &absorbed), Some(body))
                    }
                };
                TypedStatementView::If(TypedIfStatementView {
                    branches: statement
                        .branches
                        .iter()
                        .enumerate()
                        .map(|(branch_index, branch)| {
                            path.push(ScopeStep::IfBranch {
                                statement: statement_index,
                                branch: branch_index,
                            });
                            let c_locals = frame_locals(placements, path);
                            let body = self.statements(&branch.body, placements, path);
                            path.pop();
                            let mut body = body?;
                            let absorbed = kernelize(&mut body, &c_locals);
                            Ok(TypedIfBranchView {
                                condition: self.condition(&branch.condition)?,
                                c_locals: surviving_locals(c_locals, &absorbed),
                                body,
                            })
                        })
                        .collect::<Result<_, String>>()?,
                    else_c_locals,
                    else_body,
                })
            }
            ast::Statement::For(for_loop) => {
                let body_scope = self.with_iterator(for_loop.iterator.as_ref());
                path.push(ScopeStep::ForBody {
                    statement: statement_index,
                });
                let c_locals = frame_locals(placements, path);
                let body = body_scope.statements(&for_loop.body, placements, path);
                path.pop();
                let mut body = body?;
                let absorbed = kernelize(&mut body, &c_locals);
                let c_locals = surviving_locals(c_locals, &absorbed);
                TypedStatementView::For(Box::new(TypedForView {
                    iterator: &for_loop.iterator,
                    start: self.expression(&for_loop.start)?,
                    step: for_loop
                        .step
                        .as_ref()
                        .map(|step| self.expression(step))
                        .transpose()?,
                    stop: self.expression(&for_loop.stop)?,
                    c_locals,
                    body,
                    row_contraction: for_loop.row_contraction(),
                }))
            }
            ast::Statement::Limit(targets) => TypedStatementView::Limit(
                targets
                    .iter()
                    .map(|target| match target {
                        ast::LimitTarget::SelfState => Ok(TypedLimitTargetView::SelfState),
                        ast::LimitTarget::Reference(reference) => {
                            Ok(TypedLimitTargetView::Reference(self.reference(reference)?))
                        }
                    })
                    .collect::<Result<_, String>>()?,
            ),
            ast::Statement::Signal(signals) => TypedStatementView::Signal(signals),
        })
    }

    fn condition(&self, condition: &'a ast::Condition) -> Result<TypedConditionView<'a>, String> {
        Ok(match condition {
            ast::Condition::Expression(expression) => {
                TypedConditionView::Expression(self.expression(expression)?)
            }
            ast::Condition::SignalCheck(check) => TypedConditionView::SignalCheck(check),
        })
    }

    fn call(&self, call: &'a ast::FunctionCall) -> Result<TypedCallView<'a>, String> {
        let lifted_base = rumoca_ir_galec::builtins::find_lifted_base(call.function.lexeme())
            .map(|builtin| builtin.name);
        self.block
            .semantic_uses
            .borrow_mut()
            .observe_call(lifted_base.unwrap_or_else(|| call.function.lexeme()));
        let callee = self.block.functions.get(call.function.lexeme()).copied();
        Ok(TypedCallView {
            function: &call.function,
            lifted_base,
            user_function: callee.is_some(),
            arguments: call
                .arguments
                .iter()
                .map(|argument| self.expression(argument))
                .collect::<Result<_, _>>()?,
            outputs: callee
                .map(|function| {
                    output_parameters(function)
                        .map(|parameter| &parameter.decl)
                        .collect()
                })
                .unwrap_or_default(),
        })
    }

    /// The declaration a reference ultimately indexes into: the local or state
    /// variable for a one-part reference, and the compartment field a
    /// multi-part state path ends at.
    ///
    /// `None` for a `for` iterator (an automatic scalar with no declaration
    /// here) and for anything this scope cannot resolve. It is deliberately
    /// total rather than fallible: it feeds an optional emission hint, and a
    /// reference this cannot resolve is one [`Self::reference`] is about to
    /// reject anyway.
    fn final_declaration(
        &self,
        reference: &'a ast::Reference,
    ) -> Option<&'a ast::VariableDeclaration> {
        match reference {
            ast::Reference::Local(part) if self.iterators.contains(part.name.lexeme()) => None,
            ast::Reference::Local(part) => self.locals.get(part.name.lexeme()).copied(),
            ast::Reference::State(parts) => {
                let (first, rest) = parts.split_first()?;
                let mut declaration = self.block.state.get(first.name.lexeme()).copied()?;
                for part in rest {
                    declaration = self.compartment_field(declaration, part)?;
                }
                Some(declaration)
            }
        }
    }

    fn compartment_field(
        &self,
        declaration: &'a ast::VariableDeclaration,
        part: &ast::RefPart,
    ) -> Option<&'a ast::VariableDeclaration> {
        let ast::TypeRef::Compartment(compartment) = &declaration.ty else {
            return None;
        };
        self.block
            .compartments
            .get(compartment.lexeme())?
            .entities
            .iter()
            .map(|entity| &entity.decl)
            .find(|candidate| candidate.name.lexeme() == part.name.lexeme())
    }

    fn reference(&self, reference: &'a ast::Reference) -> Result<TypedReferenceView<'a>, String> {
        let shape = match reference {
            ast::Reference::Local(part) if self.iterators.contains(part.name.lexeme()) => {
                ShapeEvidence::scalar(ast::ScalarType::Integer)
            }
            ast::Reference::Local(part) => self
                .locals
                .get(part.name.lexeme())
                .copied()
                .ok_or_else(|| {
                    format!(
                        "checked local reference `{}` is unresolved",
                        part.name.lexeme()
                    )
                })
                .and_then(|declaration| reference_shape(declaration, part))?,
            ast::Reference::State(parts) => self.block.state_reference_shape(parts)?,
        };
        let parts = match reference {
            ast::Reference::Local(part) => std::slice::from_ref(part),
            ast::Reference::State(parts) => parts.as_slice(),
        };
        let mut typed_parts = Vec::with_capacity(parts.len());
        for part in parts {
            typed_parts.push(TypedRefPartView {
                name: &part.name,
                subscripts: part
                    .subscripts
                    .iter()
                    .map(|subscript| self.expression(subscript))
                    .collect::<Result<_, _>>()?,
            });
        }
        // A `for` iterator shadows any declaration of the same name inside the
        // loop body, and an iterator is always an automatic scalar, so the
        // shadowing case has to be answered before the region is consulted.
        let slot = match reference {
            ast::Reference::Local(part) if !self.iterators.contains(part.name.lexeme()) => {
                self.context_slots.get(part.name.lexeme()).copied()
            }
            ast::Reference::Local(_) | ast::Reference::State(_) => None,
        };
        let context_resident = slot.is_some();
        if context_resident {
            *self.block.context_reads.borrow_mut() = true;
        }
        Ok(TypedReferenceView {
            rank: shape.rank,
            extents: shape.extents,
            scalar: shape.scalar,
            context_resident,
            context_overlay: slot.flatten(),
            // Arena membership is decided after every reference is projected;
            // `mark_arena_references` sets this where the placement landed.
            context_arena: false,
            declared_extents: self
                .final_declaration(reference)
                .and_then(|declaration| literal_extents(&declaration.dimensions)),
            node: match reference {
                ast::Reference::Local(_) => {
                    let Some(part) = typed_parts.pop() else {
                        return Err("checked local reference has no parts".to_owned());
                    };
                    TypedReferenceNodeView::Local(part)
                }
                ast::Reference::State(_) => TypedReferenceNodeView::State(typed_parts),
            },
        })
    }

    fn expression(
        &self,
        expression: &'a ast::Expression,
    ) -> Result<TypedExpressionView<'a>, String> {
        let (shape, node) = match expression {
            ast::Expression::Bool(value) => (
                ShapeEvidence::scalar(ast::ScalarType::Boolean),
                TypedExpressionNodeView::Bool(*value),
            ),
            ast::Expression::Integer(value) => (
                ShapeEvidence::scalar(ast::ScalarType::Integer),
                TypedExpressionNodeView::Integer(*value),
            ),
            ast::Expression::Real(value) => (
                ShapeEvidence::scalar(ast::ScalarType::Real),
                TypedExpressionNodeView::Real(*value),
            ),
            ast::Expression::Ref(reference) => {
                let reference = self.reference(reference)?;
                (
                    ShapeEvidence::of_reference(&reference),
                    TypedExpressionNodeView::Ref(reference),
                )
            }
            ast::Expression::Size { array, dimension } => (
                ShapeEvidence::scalar(ast::ScalarType::Integer),
                TypedExpressionNodeView::Size {
                    array: self.reference(array)?,
                    dimension: Box::new(self.expression(dimension)?),
                },
            ),
            ast::Expression::Call(call) => {
                let call_view = self.call(call)?;
                (
                    self.block.call_shape(call, &call_view.arguments)?,
                    TypedExpressionNodeView::Call(call_view),
                )
            }
            ast::Expression::Paren(value) => {
                let value = Box::new(self.expression(value)?);
                (
                    ShapeEvidence::of_expression(&value),
                    TypedExpressionNodeView::Paren(value),
                )
            }
            ast::Expression::If(value) => {
                if let Some(selection) = value.bounded_selection_correlation() {
                    self.bounded_selection_expression(value, selection)?
                } else {
                    self.if_expression(value)?
                }
            }
            ast::Expression::Array(values) => self.array_expression(values)?,
            ast::Expression::Neg(reference) => {
                let reference = self.reference(reference)?;
                (
                    ShapeEvidence::of_reference(&reference),
                    TypedExpressionNodeView::Neg(reference),
                )
            }
            ast::Expression::Not(value) => (
                ShapeEvidence::scalar(ast::ScalarType::Boolean),
                TypedExpressionNodeView::Not(Box::new(self.expression(value)?)),
            ),
            ast::Expression::Binary { op, lhs, rhs } => self.binary_expression(*op, lhs, rhs)?,
        };
        Ok(TypedExpressionView {
            rank: shape.rank,
            extents: shape.extents,
            scalar: shape.scalar,
            node,
        })
    }

    fn bounded_selection_expression(
        &self,
        value: &'a ast::IfExpression,
        selection: &'a ast::BoundedSelection,
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let reference = self.reference(selection.reference())?;
        let (shape, node) = self.if_expression(value)?;
        let TypedExpressionNodeView::If(galec) = node else {
            return Err("if-expression projection did not return an if node".to_owned());
        };
        self.block.semantic_uses.borrow_mut().bounded_selection = true;
        Ok((
            shape,
            TypedExpressionNodeView::BoundedSelection(Box::new(TypedBoundedSelectionView {
                reference,
                extents: selection.extents().to_vec(),
                galec,
            })),
        ))
    }

    fn if_expression(
        &self,
        value: &'a ast::IfExpression,
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let branches = value
            .branches
            .iter()
            .map(|(condition, branch)| Ok((self.expression(condition)?, self.expression(branch)?)))
            .collect::<Result<Vec<_>, String>>()?;
        let else_value = Box::new(self.expression(&value.else_value)?);
        let shape = ShapeEvidence::of_expression(&else_value);
        for (_, branch) in &branches {
            require_equal_shape(
                shape.clone(),
                ShapeEvidence::of_expression(branch),
                "checked if-expression branches",
            )?;
        }
        Ok((
            shape,
            TypedExpressionNodeView::If(TypedIfExpressionView {
                branches,
                else_value,
            }),
        ))
    }

    fn array_expression(
        &self,
        elements: &'a [ast::Expression],
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let values = elements
            .iter()
            .map(|value| self.expression(value))
            .collect::<Result<Vec<_>, _>>()?;
        let element_shape = values
            .first()
            .map(ShapeEvidence::of_expression)
            .unwrap_or_else(ShapeEvidence::unknown_scalar);
        for value in &values[1..] {
            require_equal_shape(
                element_shape.clone(),
                ShapeEvidence::of_expression(value),
                "checked array constructor",
            )?;
        }
        let mut extents = element_shape.extents.clone();
        if let Some(extents) = &mut extents {
            extents.insert(0, values.len());
        }
        Ok((
            ShapeEvidence {
                rank: element_shape.rank + 1,
                extents,
                scalar: element_shape.scalar,
            },
            TypedExpressionNodeView::Array(values),
        ))
    }

    fn binary_expression(
        &self,
        op: ast::BinaryOp,
        lhs: &'a ast::Expression,
        rhs: &'a ast::Expression,
    ) -> Result<(ShapeEvidence, TypedExpressionNodeView<'a>), String> {
        let lhs = Box::new(self.expression(lhs)?);
        let rhs = Box::new(self.expression(rhs)?);
        if lhs.scalar == Some(ast::ScalarType::Real) || rhs.scalar == Some(ast::ScalarType::Real) {
            self.block
                .semantic_uses
                .borrow_mut()
                .observe_real_comparison(op);
        }
        let shape = if matches!(
            op.precedence_class(),
            ast::PrecedenceClass::Power
                | ast::PrecedenceClass::Multiplicative
                | ast::PrecedenceClass::Additive
        ) {
            broadcast_shape(
                ShapeEvidence::of_expression(&lhs),
                ShapeEvidence::of_expression(&rhs),
            )?
        } else {
            ShapeEvidence::scalar(ast::ScalarType::Boolean)
        };
        let square_form = if op == ast::BinaryOp::Pow {
            real_square_form(&lhs, &rhs)
        } else {
            SquareForm::Power
        };
        if square_form == SquareForm::BoundMultiply {
            self.block.semantic_uses.borrow_mut().real_square = true;
        }
        Ok((
            shape,
            TypedExpressionNodeView::Binary {
                op,
                precedence_class: op.precedence_class(),
                associativity: op.precedence_class().associativity(),
                lhs,
                rhs,
                square_form,
            },
        ))
    }
}

/// Decide which statements of one list a C-family target prints as calls into
/// the shared array-kernel library, and return the accumulator locals the
/// rewrite left with no reader.
///
/// The caller owns those declarations and must drop them: a local whose only
/// assignments were absorbed into a kernel call would otherwise be declared,
/// never written, and then diagnosed under `-Werror`.
///
/// Two shapes are recognised, both decided here and printed — not detected — by
/// the C templates:
///
/// * a whole-array assignment of one repeated literal, which becomes a
///   `rumoca_galec_fill_*` call over each innermost run; and
/// * the three-statement accumulate group the GALEC projection emits for a
///   materialised tensor contraction (zero the accumulator, sum over the
///   contracted index, store), which becomes one `rumoca_galec_dot_real` call
///   when both operands turn out to be contiguous runs.
///
/// Anything else keeps the loop nest the checked Algorithm Code spells. That
/// fallback is where contraction shapes no rank-1 kernel covers — a strided
/// column walk, a computed subscript, an operand that is a whole expression
/// rather than a run — genuinely have to go.
/// Drop the frame declarations whose only assignments a kernel call absorbed.
fn surviving_locals<'a>(
    locals: Vec<TypedLocalView<'a>>,
    absorbed: &HashSet<&'a str>,
) -> Vec<TypedLocalView<'a>> {
    if absorbed.is_empty() {
        return locals;
    }
    locals
        .into_iter()
        .filter(|local| !absorbed.contains(local.decl.name.lexeme()))
        .collect()
}

fn kernelize<'a>(
    statements: &mut [TypedSpannedStatement<'a>],
    frame: &[TypedLocalView<'a>],
) -> HashSet<&'a str> {
    for statement in statements.iter_mut() {
        if let TypedStatementView::Assignment { target, value } = &statement.node
            && target.rank > 0
            && target.extents.is_some()
            && let Some(fill) = uniform_fill_literal(value)
        {
            statement.kernel = Some(KernelStatementView::Fill { value: fill });
        }
    }

    // A whole result-row loop of a matrix product. This is tried before the
    // three-statement group below because the two overlap: a reduction whose
    // operands are both contiguous runs is a better inner product than it is a
    // row accumulation, and it has already been recognised as one by the time
    // the enclosing loop is examined — `contraction_parts` refuses a group that
    // any kernel already claimed, so the better form wins by construction.
    for statement in statements.iter_mut() {
        if statement.kernel.is_some() {
            continue;
        }
        let TypedStatementView::For(row) = &statement.node else {
            continue;
        };
        let carried = carried_row_contraction(row);
        // R-1 retirement assertion: every loop the old recognizer could claim
        // must now arrive carrying the row contraction the GALEC lowering
        // attested, so the recognizer has nothing left of its own to find.
        #[cfg(debug_assertions)]
        assert!(
            carried.is_some() || matmul_row_in_place(row).is_none(),
            "a fissioned row product reached emission without the row \
             contraction its lowering owes it"
        );
        if let Some(product) = matmul_row(row).or(carried) {
            statement.kernel = Some(KernelStatementView::ScaledAdd {
                count: product.count,
                zero: product.zero,
                target: product.target,
                iterator: product.iterator,
                extent: product.extent,
                scale: Box::new(product.scale),
                source: product.source,
            });
        } else if let Some(fused) = matmul_row_fused(row) {
            statement.kernel = Some(KernelStatementView::ScaledAddFused {
                count: fused.count,
                zero: fused.zero,
                row_local: fused.row_local,
                target: fused.target,
                column: fused.column,
                iterator: fused.iterator,
                extent: fused.extent,
                scale: Box::new(fused.scale),
                source: fused.source,
                store_target: fused.store_target,
                store_value: Box::new(fused.store_value),
            });
        } else if let Some(restricted) = index_restricted_block(row) {
            statement.kernel = Some(KernelStatementView::IndexRestricted {
                statement: Box::new(restricted),
            });
        }
    }

    let mut dead = HashSet::new();
    let mut index = 0;
    while index + 3 <= statements.len() {
        let Some(core) = contraction_core(&statements[index..index + 3]) else {
            index += 1;
            continue;
        };
        // The accumulator is a compiler temporary, but nothing in this view
        // says so, and reading one that a later statement also reads would
        // delete its only assignment. Prove it instead, in two halves. The
        // name must be declared in this frame: LocalPlacements puts every
        // declaration at the common prefix of its uses, so a name placed here
        // cannot be read from an outer statement list — a mentions scan of
        // this list alone can never rule that out.
        if !frame
            .iter()
            .any(|local| local.decl.name.lexeme() == core.name)
        {
            index += 1;
            continue;
        }
        // And within this list (the scan recurses into nested bodies), the
        // group must be the whole life of the name.
        let mentions = statements
            .iter()
            .enumerate()
            .filter(|(position, statement)| {
                !(index..index + 3).contains(position) && mentions_local(&statement.node, core.name)
            })
            .count();
        if mentions > 0 {
            index += 1;
            continue;
        }
        statements[index].kernel = Some(KernelStatementView::Absorbed);
        statements[index + 1].kernel = Some(KernelStatementView::Absorbed);
        statements[index + 2].kernel = Some(KernelStatementView::Dot {
            count: core.count,
            target: core.target,
            lhs: core.lhs,
            rhs: core.rhs,
        });
        dead.insert(core.name);
        index += 3;
    }

    absorb_accumulate_pairs(statements);
    dead
}

/// The two-statement remainder of the same reduction: `acc := 0.0` and the
/// ascending accumulate loop, with no bare `target := acc` store, because
/// the accumulated value feeds a larger expression — a fused store, a pair
/// of reductions combined at the end, a guard. The pair still is the dot
/// kernel: zero-init plus ascending accumulate over two contiguous runs
/// into one single-precision object. It prints as `acc =
/// rumoca_galec_dot_real(...)`, which performs the identical products in
/// the identical order into the identical `float`, so the substitution is
/// bit-preserving. The accumulator local survives — every later reader
/// still sees the value the loop produced — so nothing is retired and no
/// liveness proof is needed; only the zero statement is absorbed into the
/// call that replaces the loop. Runs after the three-statement pass so
/// the fuller form, which also retires the local, wins where both apply.
fn absorb_accumulate_pairs<'a>(statements: &mut [TypedSpannedStatement<'a>]) {
    let mut index = 0;
    while index + 2 <= statements.len() {
        let recognized =
            accumulate_pair(&statements[index], &statements[index + 1]).and_then(|pair| {
                Some((
                    pair.count,
                    pair.accumulator.clone(),
                    contiguous_run(reference_of(pair.lhs)?, pair.iterator, pair.count)?,
                    contiguous_run(reference_of(pair.rhs)?, pair.iterator, pair.count)?,
                ))
            });
        let Some((count, target, lhs, rhs)) = recognized else {
            index += 1;
            continue;
        };
        statements[index].kernel = Some(KernelStatementView::Absorbed);
        statements[index + 1].kernel = Some(KernelStatementView::Dot {
            count,
            target,
            lhs,
            rhs,
        });
        index += 2;
    }
}

/// The literal every element of a whole-array assignment receives, if there is
/// one.
///
/// Only literals qualify. A fill kernel takes its value by value and therefore
/// evaluates the source expression once, where the loop it replaces evaluated
/// it per element; for a literal those are the same thing, and for anything
/// else — a reference the target may alias, a comparison that writes the error
/// signal status, a call — they need not be, so those keep their loop.
fn uniform_fill_literal<'a>(value: &TypedExpressionView<'a>) -> Option<TypedExpressionView<'a>> {
    match &value.node {
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_) => Some(value.clone()),
        TypedExpressionNodeView::Array(elements) => {
            let (first, rest) = elements.split_first()?;
            let fill = uniform_fill_literal(first)?;
            rest.iter()
                .all(|element| {
                    uniform_fill_literal(element)
                        .is_some_and(|other| same_literal(&fill.node, &other.node))
                })
                .then_some(fill)
        }
        _ => None,
    }
}

fn same_literal(left: &TypedExpressionNodeView<'_>, right: &TypedExpressionNodeView<'_>) -> bool {
    match (left, right) {
        (TypedExpressionNodeView::Bool(left), TypedExpressionNodeView::Bool(right)) => {
            left == right
        }
        (TypedExpressionNodeView::Integer(left), TypedExpressionNodeView::Integer(right)) => {
            left == right
        }
        // Bit equality, not numeric equality: two `Real` literals that print
        // differently must not be collapsed into one fill, and `-0.0` and `0.0`
        // are different fills.
        (TypedExpressionNodeView::Real(left), TypedExpressionNodeView::Real(right)) => {
            left.to_bits() == right.to_bits()
        }
        _ => false,
    }
}

/// One recognised tensor-contraction accumulate group.
struct ContractionCore<'a> {
    /// The accumulator local the group zeroes, sums into, and then stores.
    name: &'a str,
    /// The contracted extent, which is both the loop trip count and the length
    /// of each operand run.
    count: i64,
    target: TypedReferenceView<'a>,
    /// The operand references with their final (contracted) subscript dropped:
    /// the contiguous runs the inner product reads.
    lhs: TypedReferenceView<'a>,
    rhs: TypedReferenceView<'a>,
}

/// Recognise the accumulate group the GALEC projection emits for a materialised
/// tensor contraction, and answer whether its two operands are contiguous runs.
///
/// The group, exactly:
///
/// ```text
/// acc := 0.0;
/// for k in 1:count loop
///   acc := acc + (lhs[…][k] * rhs[…][k]);
/// end for;
/// target := acc;
/// ```
///
/// The multiplication's operand order is preserved into the kernel call, and
/// the kernel sums over ascending `k` into a single `float`. So the emitted
/// call performs the same operations on the same values in the same order as
/// the loop it replaces, which is what makes the substitution bit-preserving
/// rather than merely equal in exact arithmetic.
///
/// Each operand must be *fully subscripted* with its final subscript the bare
/// contracted iterator, and `count` must equal the operand's final declared
/// extent. Together those say that dropping the final subscript names a run of
/// exactly `count` adjacent elements — one whole row of a row-major array —
/// which is the only thing a rank-1 kernel can be handed. A column walk
/// (`rhs[k][j]`), a computed subscript, or a partial run all fail here and keep
/// the loop.
fn contraction_core<'a>(group: &[TypedSpannedStatement<'a>]) -> Option<ContractionCore<'a>> {
    let parts = contraction_parts(group)?;
    Some(ContractionCore {
        name: parts.name,
        count: parts.count,
        target: parts.target.clone(),
        lhs: contiguous_run(reference_of(parts.lhs)?, parts.iterator, parts.count)?,
        rhs: contiguous_run(reference_of(parts.rhs)?, parts.iterator, parts.count)?,
    })
}

/// The pieces of a contraction accumulate group, before any decision about
/// whether a kernel can express it.
struct ContractionParts<'a, 'view> {
    name: &'a str,
    iterator: &'a ast::Name,
    count: i64,
    zero: &'view TypedExpressionView<'a>,
    target: &'view TypedReferenceView<'a>,
    lhs: &'view TypedExpressionView<'a>,
    rhs: &'view TypedExpressionView<'a>,
}

fn contraction_parts<'a, 'view>(
    group: &'view [TypedSpannedStatement<'a>],
) -> Option<ContractionParts<'a, 'view>> {
    let [zero, loop_statement, store] = group else {
        return None;
    };
    if store.kernel.is_some() {
        return None;
    }
    let pair = accumulate_pair(zero, loop_statement)?;
    let TypedStatementView::Assignment { target, value } = &store.node else {
        return None;
    };
    if scalar_local_name(reference_of(value)?)? != pair.name
        || target.scalar != Some(ast::ScalarType::Real)
        || target.rank != 0
    {
        return None;
    }

    Some(ContractionParts {
        name: pair.name,
        iterator: pair.iterator,
        count: pair.count,
        zero: pair.zero,
        target,
        lhs: pair.lhs,
        rhs: pair.rhs,
    })
}

/// The first two statements of every contraction group — `acc := 0.0` and the
/// ascending accumulate loop — parsed on their own, so recognisers can accept
/// the group whether or not a bare `target := acc` store follows.
struct AccumulatePair<'a, 'view> {
    name: &'a str,
    accumulator: &'view TypedReferenceView<'a>,
    iterator: &'a ast::Name,
    count: i64,
    zero: &'view TypedExpressionView<'a>,
    lhs: &'view TypedExpressionView<'a>,
    rhs: &'view TypedExpressionView<'a>,
}

fn accumulate_pair<'a, 'view>(
    zero: &'view TypedSpannedStatement<'a>,
    loop_statement: &'view TypedSpannedStatement<'a>,
) -> Option<AccumulatePair<'a, 'view>> {
    // Neither statement may already belong to another kernel.
    if zero.kernel.is_some() || loop_statement.kernel.is_some() {
        return None;
    }

    let TypedStatementView::Assignment {
        target: accumulator,
        value: initial,
    } = &zero.node
    else {
        return None;
    };
    let name = scalar_local_name(accumulator)?;
    // Bit equality, like `same_literal`: the dot kernel hardcodes `0.0f`, so a
    // `-0.0` zero-init (which `==` would accept) must keep its loop — the two
    // are different bits and the substitution contract is bit-preservation.
    if accumulator.scalar != Some(ast::ScalarType::Real)
        || !matches!(initial.node,
            TypedExpressionNodeView::Real(value) if value.to_bits() == 0.0_f64.to_bits())
    {
        return None;
    }

    let TypedStatementView::For(contracted) = &loop_statement.node else {
        return None;
    };
    let iterator = contracted.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(count)) =
        (&contracted.start.node, &contracted.stop.node)
    else {
        return None;
    };
    if contracted.step.is_some() || !contracted.c_locals.is_empty() {
        return None;
    }
    let [body] = contracted.body.as_slice() else {
        return None;
    };
    let TypedStatementView::Assignment {
        target: accumulated,
        value: sum,
    } = &body.node
    else {
        return None;
    };
    if scalar_local_name(accumulated)? != name {
        return None;
    }
    let TypedExpressionNodeView::Binary {
        op: ast::BinaryOp::Add,
        lhs: carried,
        rhs: product,
        ..
    } = &unparenthesized(sum).node
    else {
        return None;
    };
    if scalar_local_name(reference_of(carried)?)? != name {
        return None;
    }
    let TypedExpressionNodeView::Binary {
        op: ast::BinaryOp::Mul,
        lhs: left,
        rhs: right,
        ..
    } = &unparenthesized(product).node
    else {
        return None;
    };

    Some(AccumulatePair {
        name,
        accumulator,
        iterator,
        count: *count,
        zero: initial,
        lhs: left,
        rhs: right,
    })
}

/// One recognised result-row loop of a matrix product.
struct MatmulRow<'a> {
    count: i64,
    zero: TypedExpressionView<'a>,
    target: TypedReferenceView<'a>,
    iterator: &'a ast::Name,
    extent: i64,
    scale: TypedExpressionView<'a>,
    source: TypedReferenceView<'a>,
}

/// Recognise `for j in 1:count loop <contraction over k> end for` as one result
/// row of a matrix product, i.e.
///
/// ```text
/// for j in 1:count loop
///   acc := 0.0;
///   for k in 1:extent loop acc := acc + (scale(k) * source[k][j]); end for;
///   target[…][j] := acc;
/// end for;
/// ```
///
/// where `scale(k)` does not depend on `j`, and both `target[…]` and
/// `source[k]` are contiguous `count`-element runs. The emission drops the `j`
/// loop and accumulates the whole row.
///
/// Two conditions beyond shape, both load-bearing:
///
/// * `scale` must not mention `j` — it is hoisted out of the row.
/// * `target` must be a different declared object from `scale`'s and `source`'s
///   bases. The row is written `extent + 1` times instead of once, so a target
///   that aliased an operand would feed partial sums back into later reads. The
///   check is on declaration identity, which is conservative: GALEC's distinct
///   declarations are distinct objects, and a shared root (`self->a.b` vs
///   `self->a.c`) is rejected rather than analysed.
fn matmul_row<'a>(row: &TypedForView<'a>) -> Option<MatmulRow<'a>> {
    let column = row.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(count)) =
        (&row.start.node, &row.stop.node)
    else {
        return None;
    };
    if row.step.is_some() {
        return None;
    }
    let parts = contraction_parts(&row.body)?;
    // The accumulator lives and dies inside this loop body, which the group is
    // the whole of, so there is nothing else in scope that could read it.
    if row.c_locals.len() != 1 || row.c_locals[0].decl.name.lexeme() != parts.name {
        return None;
    }

    let target = contiguous_run(parts.target, column, *count)?;
    let source = contiguous_run(reference_of(parts.rhs)?, column, *count)?;
    // The scale must be a pure-arithmetic `j`-free operand ([`scale_operand`]):
    // hoisting it out of the row loop must not move a comparison or a call,
    // which would change how many times an operation with an observable effect
    // runs. Fused product forms outside that whitelist keep their loops.
    let (scale, scale_roots) = scale_operand(parts.lhs)?;
    if mentions_name_in_expression(scale, column.lexeme()) {
        return None;
    }
    let target_root = reference_root(&target);
    if target_root == reference_root(&source) || scale_roots.contains(&target_root) {
        return None;
    }

    Some(MatmulRow {
        count: *count,
        zero: parts.zero.clone(),
        target,
        iterator: parts.iterator,
        extent: parts.count,
        scale: scale.clone(),
        source,
    })
}

/// Read the row contraction the GALEC lowering carried on this loop.
///
/// Nothing here decides anything. The contraction node that issued the loop
/// nest already attested the three facts an emitter cannot see in the
/// statements. The accumulator is a temporary of its own, the coefficient is
/// row-invariant pure arithmetic, and the right operand walks one run per
/// contracted value ([`ast::RowContraction`]), so this walks straight to the
/// operands the correlation names and hands them over.
///
/// Two things are still read from the printed view rather than taken on trust,
/// and both are this target's own data rather than the lowering's: the
/// declared extents behind each reference, which decide whether the run the
/// correlation names is expressible as a `count`-element C array argument
/// ([`contiguous_run`]), and whether a neighbouring kernel already claimed one
/// of these statements. Where either says no, the loop nest prints as itself.
fn carried_row_contraction<'a>(row: &TypedForView<'a>) -> Option<MatmulRow<'a>> {
    let correlation = row.row_contraction?;
    let [zero, accumulate] = row.body.as_slice() else {
        return None;
    };
    if zero.kernel.is_some() || accumulate.kernel.is_some() {
        return None;
    }
    let TypedStatementView::Assignment {
        target: accumulator,
        value: initial,
    } = &zero.node
    else {
        return None;
    };
    let TypedStatementView::For(contracted) = &accumulate.node else {
        return None;
    };
    let [body] = contracted.body.as_slice() else {
        return None;
    };
    let TypedStatementView::Assignment { value: sum, .. } = &body.node else {
        return None;
    };
    let TypedExpressionNodeView::Binary { rhs: product, .. } = &unparenthesized(sum).node else {
        return None;
    };
    let TypedExpressionNodeView::Binary {
        lhs: scale,
        rhs: source,
        ..
    } = &unparenthesized(product).node
    else {
        return None;
    };
    let column = row.iterator.as_ref()?;
    let count = i64::from(correlation.count());
    let target = contiguous_run(accumulator, column, count)?;
    // The run the printer reaches is a product of placement, overlay and arena
    // projection, so it is derived here rather than carried. Its *identity* is
    // not: lowering attested which intermediate this nest accumulates into, and
    // a derived run naming a different object would mean the printer matched a
    // nest the attestation was not about.
    debug_assert_eq!(
        reference_root(&target),
        correlation.target().lexeme(),
        "row contraction attested an accumulation into a different run"
    );
    Some(MatmulRow {
        count,
        zero: initial.clone(),
        target,
        iterator: correlation.iterator(),
        extent: i64::from(correlation.extent()),
        scale: scale.as_ref().clone(),
        source: contiguous_run(reference_of(source)?, column, count)?,
    })
}

/// The retired recognizer for the fissioned spelling of one result-row loop
/// of a matrix product, kept as the assertion that its pattern is unreachable.
///
/// The GALEC lowering now carries [`ast::RowContraction`] on the loop it
/// issues, so [`carried_row_contraction`] reads the shape instead of
/// rediscovering it. This is called under `debug_assertions` on every loop
/// that arrives without that correlation and is required to find nothing;
/// when it has ridden a release silently, it goes.
///
/// The shape it reconstructed, where the accumulator is an element of a
/// materialized tensor rather than a frame scalar:
///
/// ```text
/// for j in 1:count loop
///   acc[…][j] := 0.0;
///   for k in 1:extent loop acc[…][j] := acc[…][j] + (scale(k) * source[k][j]); end for;
/// end for;
/// ```
///
/// The contraction-fission pass produces exactly this shape for the first pass
/// of a split quadratic form: the carried scalar is widened over the free
/// column index into a context array that the second pass then reads. The
/// emitted loop nest walks `source`'s *column* per `j` (strided) and re-reads
/// the in-memory accumulator element every iteration, which is the most
/// expensive spelling of the product on an embedded target.
///
/// The rewrite is the same one [`matmul_row`] performs: drop the `j` loop,
/// zero the whole `count`-element accumulator run, and add each `scale(k)`
/// scaled source row into it. For a fixed `j` the identical products are
/// accumulated in the identical ascending-`k` order into the identical
/// `float` object, so the substitution is bit-preserving; it interleaves the
/// independent `j` sums instead of running them one after another. Unlike the
/// frame-scalar shape no declaration is retired: the accumulator array is
/// real storage a later statement reads, and the kernel call writes exactly
/// the values the loop nest wrote.
#[cfg(debug_assertions)]
fn matmul_row_in_place<'a>(row: &TypedForView<'a>) -> Option<MatmulRow<'a>> {
    let column = row.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(count)) =
        (&row.start.node, &row.stop.node)
    else {
        return None;
    };
    if row.step.is_some() || !row.c_locals.is_empty() {
        return None;
    }
    let [zero, accumulate] = row.body.as_slice() else {
        return None;
    };
    if zero.kernel.is_some() || accumulate.kernel.is_some() {
        return None;
    }
    let TypedStatementView::Assignment {
        target: accumulator,
        value: initial,
    } = &zero.node
    else {
        return None;
    };
    // Bit equality, not `==`: the fill kernel writes the zero literal it is
    // handed, and `-0.0` has different bits.
    if accumulator.scalar != Some(ast::ScalarType::Real)
        || !matches!(initial.node,
            TypedExpressionNodeView::Real(value) if value.to_bits() == 0.0_f64.to_bits())
    {
        return None;
    }
    let TypedStatementView::For(contracted) = &accumulate.node else {
        return None;
    };
    let iterator = contracted.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(extent)) =
        (&contracted.start.node, &contracted.stop.node)
    else {
        return None;
    };
    if contracted.step.is_some() || !contracted.c_locals.is_empty() {
        return None;
    }
    let [body] = contracted.body.as_slice() else {
        return None;
    };
    let TypedStatementView::Assignment {
        target: accumulated,
        value: sum,
    } = &body.node
    else {
        return None;
    };
    // All three accumulator spellings must name the same element: the zeroed
    // one, the assigned one, and the carried operand of the sum.
    if !same_reference(accumulator, accumulated) {
        return None;
    }
    let TypedExpressionNodeView::Binary {
        op: ast::BinaryOp::Add,
        lhs: carried,
        rhs: product,
        ..
    } = &unparenthesized(sum).node
    else {
        return None;
    };
    if !same_reference(reference_of(carried)?, accumulator) {
        return None;
    }
    let TypedExpressionNodeView::Binary {
        op: ast::BinaryOp::Mul,
        lhs: left,
        rhs: right,
        ..
    } = &unparenthesized(product).node
    else {
        return None;
    };
    // The accumulator run: dropping the final `j` subscript must name a
    // contiguous `count`-element run, and what remains must not move with the
    // contracted index either — the run has to be one object across the whole
    // rewritten loop.
    let target = contiguous_run(accumulator, column, *count)?;
    if mentions_name_in_reference(&target, iterator.lexeme()) {
        return None;
    }
    let source = contiguous_run(reference_of(right)?, column, *count)?;
    let (scale, scale_roots) = scale_operand(left)?;
    if mentions_name_in_expression(scale, column.lexeme()) {
        return None;
    }
    let target_root = reference_root(&target);
    if target_root == reference_root(&source) || scale_roots.contains(&target_root) {
        return None;
    }

    Some(MatmulRow {
        count: *count,
        zero: initial.clone(),
        target,
        iterator,
        extent: *extent,
        scale: scale.clone(),
        source,
    })
}

/// Recognise the dense guarded spelling of a rectangular block assignment and
/// rebuild it with the loops restricted to the guard's own rectangle; see
/// [`KernelStatementView::IndexRestricted`] for the shape and the
/// bit-preservation argument. `None` whenever any piece falls outside the
/// vocabulary: a non-literal bound, a guard that constrains anything but the
/// two loop indices, an else arm that is not the target element itself.
fn index_restricted_block<'a>(outer: &TypedForView<'a>) -> Option<TypedStatementView<'a>> {
    let row = outer.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(rows)) =
        (&outer.start.node, &outer.stop.node)
    else {
        return None;
    };
    if outer.step.is_some() || !outer.c_locals.is_empty() {
        return None;
    }
    let [inner_wrapped] = outer.body.as_slice() else {
        return None;
    };
    if inner_wrapped.kernel.is_some() {
        return None;
    }
    let TypedStatementView::For(inner) = &inner_wrapped.node else {
        return None;
    };
    let column = inner.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(columns)) =
        (&inner.start.node, &inner.stop.node)
    else {
        return None;
    };
    if inner.step.is_some() || !inner.c_locals.is_empty() {
        return None;
    }
    let [assignment] = inner.body.as_slice() else {
        return None;
    };
    if assignment.kernel.is_some() {
        return None;
    }
    let TypedStatementView::Assignment { target, value } = &assignment.node else {
        return None;
    };
    let TypedExpressionNodeView::If(selection) = &unparenthesized(value).node else {
        return None;
    };
    let [(condition, chosen)] = selection.branches.as_slice() else {
        return None;
    };
    if !same_reference(reference_of(&selection.else_value)?, target) {
        return None;
    }
    let mut bounds = [(1, *rows), (1, *columns)];
    if !conjunctive_index_bounds(condition, row, column, &mut bounds) {
        return None;
    }
    let [(row_lo, row_hi), (col_lo, col_hi)] = bounds;
    // A degenerate rectangle would mean the whole nest is identity writes; no
    // model spells that, so leave it its loop rather than emit nothing.
    if row_lo > row_hi || col_lo > col_hi {
        return None;
    }

    let store = TypedSpannedStatement {
        trace: assignment.trace.clone(),
        kernel: None,
        node: TypedStatementView::Assignment {
            target: target.clone(),
            value: chosen.clone(),
        },
    };
    let inner_for = TypedSpannedStatement {
        trace: inner_wrapped.trace.clone(),
        kernel: None,
        node: TypedStatementView::For(Box::new(TypedForView {
            iterator: inner.iterator,
            start: integer_literal(col_lo),
            step: None,
            stop: integer_literal(col_hi),
            c_locals: Vec::new(),
            body: vec![store],
            row_contraction: None,
        })),
    };
    Some(TypedStatementView::For(Box::new(TypedForView {
        iterator: outer.iterator,
        start: integer_literal(row_lo),
        step: None,
        stop: integer_literal(row_hi),
        c_locals: Vec::new(),
        body: vec![inner_for],
        row_contraction: None,
    })))
}

/// An `int32` literal expression for a rebuilt loop bound.
fn integer_literal<'a>(value: i64) -> TypedExpressionView<'a> {
    TypedExpressionView {
        rank: 0,
        extents: Some(Vec::new()),
        scalar: Some(ast::ScalarType::Integer),
        node: TypedExpressionNodeView::Integer(value),
    }
}

/// Tighten `bounds` (one `(lo, hi)` pair per index, `[row, column]`) by a
/// conjunction of literal comparisons on exactly those indices. `false` the
/// moment anything else appears — a disjunction, a third name, a computed
/// bound — so a `true` return means the conjunction is *exactly* the
/// rectangle the tightened bounds describe.
fn conjunctive_index_bounds(
    condition: &TypedExpressionView<'_>,
    row: &ast::Name,
    column: &ast::Name,
    bounds: &mut [(i64, i64); 2],
) -> bool {
    let TypedExpressionNodeView::Binary { op, lhs, rhs, .. } = &unparenthesized(condition).node
    else {
        return false;
    };
    if *op == ast::BinaryOp::And {
        return conjunctive_index_bounds(lhs, row, column, bounds)
            && conjunctive_index_bounds(rhs, row, column, bounds);
    }
    // One side the bare index, the other an integer literal; normalize to
    // `index OP literal`.
    let index_of = |value: &TypedExpressionView<'_>| -> Option<usize> {
        let name = scalar_local_name(reference_of(unparenthesized(value))?)?;
        if name == row.lexeme() {
            Some(0)
        } else if name == column.lexeme() {
            Some(1)
        } else {
            None
        }
    };
    let literal_of = |value: &TypedExpressionView<'_>| -> Option<i64> {
        match &unparenthesized(value).node {
            TypedExpressionNodeView::Integer(literal) => Some(*literal),
            _ => None,
        }
    };
    let (index, literal, op) =
        if let (Some(index), Some(literal)) = (index_of(lhs), literal_of(rhs)) {
            (index, literal, *op)
        } else if let (Some(literal), Some(index)) = (literal_of(lhs), index_of(rhs)) {
            // `literal OP index` mirrors to `index OP' literal`.
            let mirrored = match op {
                ast::BinaryOp::Lt => ast::BinaryOp::Gt,
                ast::BinaryOp::Gt => ast::BinaryOp::Lt,
                ast::BinaryOp::Le => ast::BinaryOp::Ge,
                ast::BinaryOp::Ge => ast::BinaryOp::Le,
                ast::BinaryOp::Eq => ast::BinaryOp::Eq,
                _ => return false,
            };
            (index, literal, mirrored)
        } else {
            return false;
        };
    let (lo, hi) = &mut bounds[index];
    match op {
        ast::BinaryOp::Ge => *lo = (*lo).max(literal),
        ast::BinaryOp::Gt => *lo = (*lo).max(literal.saturating_add(1)),
        ast::BinaryOp::Le => *hi = (*hi).min(literal),
        ast::BinaryOp::Lt => *hi = (*hi).min(literal.saturating_sub(1)),
        ast::BinaryOp::Eq => {
            *lo = (*lo).max(literal);
            *hi = (*hi).min(literal);
        }
        _ => return false,
    }
    true
}

/// One recognised fused-store result-row loop; see
/// [`KernelStatementView::ScaledAddFused`] for the shape and the argument.
struct MatmulRowFused<'a> {
    count: i64,
    zero: TypedExpressionView<'a>,
    row_local: &'a ast::Name,
    target: TypedReferenceView<'a>,
    column: &'a ast::Name,
    iterator: &'a ast::Name,
    extent: i64,
    scale: TypedExpressionView<'a>,
    source: TypedReferenceView<'a>,
    store_target: TypedReferenceView<'a>,
    store_value: TypedExpressionView<'a>,
}

fn matmul_row_fused<'a>(row: &TypedForView<'a>) -> Option<MatmulRowFused<'a>> {
    let column = row.iterator.as_ref()?;
    let (TypedExpressionNodeView::Integer(1), TypedExpressionNodeView::Integer(count)) =
        (&row.start.node, &row.stop.node)
    else {
        return None;
    };
    if row.step.is_some() {
        return None;
    }
    let [zero, loop_statement, store] = row.body.as_slice() else {
        return None;
    };
    if store.kernel.is_some() {
        return None;
    }
    let pair = accumulate_pair(zero, loop_statement)?;
    // The accumulator lives and dies inside this loop body: its declaration is
    // this loop's only frame local, so no sibling statement can read it.
    if row.c_locals.len() != 1 || row.c_locals[0].decl.name.lexeme() != pair.name {
        return None;
    }
    let TypedStatementView::Assignment {
        target: store_target,
        value: store_value,
    } = &store.node
    else {
        return None;
    };
    // The store runs after the whole row accumulation instead of interleaved
    // with it, so it must neither perform an observable effect nor write an
    // object the accumulation reads.
    if !effect_free_store(store_value) {
        return None;
    }
    let source = contiguous_run(reference_of(pair.rhs)?, column, *count)?;
    let (scale, scale_roots) = scale_operand(pair.lhs)?;
    if mentions_name_in_expression(scale, column.lexeme()) {
        return None;
    }
    let store_root = reference_root(store_target);
    if store_root == reference_root(&source)
        || scale_roots.contains(&store_root)
        || store_root == pair.name
    {
        return None;
    }
    // The accumulator local, respelled: the row it becomes, and its `j`-th
    // element every mention in the store expression is rewritten to.
    let row_name = &row.c_locals[0].decl.name;
    let target = TypedReferenceView {
        rank: 1,
        extents: Some(vec![usize::try_from(*count).ok()?]),
        scalar: Some(ast::ScalarType::Real),
        context_resident: false,
        context_arena: false,
        context_overlay: None,
        declared_extents: Some(vec![usize::try_from(*count).ok()?]),
        node: TypedReferenceNodeView::Local(TypedRefPartView {
            name: row_name,
            subscripts: Vec::new(),
        }),
    };
    let store_value = substitute_scalar_local(store_value, pair.name, row_name, column);

    Some(MatmulRowFused {
        count: *count,
        zero: pair.zero.clone(),
        row_local: row_name,
        target,
        column,
        iterator: pair.iterator,
        extent: pair.count,
        scale: scale.clone(),
        source,
        store_target: store_target.clone(),
        store_value,
    })
}

/// Whether a fused store expression is free of observable effects, judged by
/// the same criterion the C emission applies: a Real-operand comparison
/// signals through the error-status channel, a call is a call, and a bounded
/// selection clamps through the signalling index helper — all three refuse.
/// Everything else the expression grammar offers is pure arithmetic and
/// selection whose evaluation count and order are unobservable.
fn effect_free_store(value: &TypedExpressionView<'_>) -> bool {
    match &value.node {
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_)
        | TypedExpressionNodeView::Ref(_)
        | TypedExpressionNodeView::Neg(_) => true,
        TypedExpressionNodeView::Size { .. }
        | TypedExpressionNodeView::BoundedSelection(_)
        | TypedExpressionNodeView::Array(_) => false,
        // Of the callable vocabulary only the Integer-to-Real conversion is
        // known pure here: it prints as a C cast, not a call. Every other
        // call — a user function mutates its context region, a libm call is
        // still a call — refuses.
        TypedExpressionNodeView::Call(call) => {
            !call.user_function
                && call.lifted_base.is_none()
                && call.function.lexeme() == "real"
                && call.arguments.iter().all(effect_free_store)
        }
        TypedExpressionNodeView::Paren(inner) | TypedExpressionNodeView::Not(inner) => {
            effect_free_store(inner)
        }
        TypedExpressionNodeView::If(branches) => {
            branches
                .branches
                .iter()
                .all(|(condition, arm)| effect_free_store(condition) && effect_free_store(arm))
                && effect_free_store(&branches.else_value)
        }
        TypedExpressionNodeView::Binary { op, lhs, rhs, .. } => {
            let real_comparison = matches!(
                op,
                ast::BinaryOp::Lt
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::Le
                    | ast::BinaryOp::Ge
                    | ast::BinaryOp::Eq
                    | ast::BinaryOp::Ne
            ) && (lhs.scalar == Some(ast::ScalarType::Real)
                || rhs.scalar == Some(ast::ScalarType::Real));
            !real_comparison && effect_free_store(lhs) && effect_free_store(rhs)
        }
    }
}

/// Rebuild an expression with every mention of the bare scalar local `name`
/// respelled as `row[column]`. Only unsubscripted single-part local
/// references can mention a scalar local, so the rewrite touches exactly
/// those nodes and clones the rest.
fn substitute_scalar_local<'a>(
    value: &TypedExpressionView<'a>,
    name: &str,
    row: &'a ast::Name,
    column: &'a ast::Name,
) -> TypedExpressionView<'a> {
    let node = match &value.node {
        TypedExpressionNodeView::Ref(reference) if scalar_local_name(reference) == Some(name) => {
            TypedExpressionNodeView::Ref(row_element(reference, row, column))
        }
        TypedExpressionNodeView::Neg(reference) if scalar_local_name(reference) == Some(name) => {
            TypedExpressionNodeView::Neg(row_element(reference, row, column))
        }
        TypedExpressionNodeView::Paren(inner) => TypedExpressionNodeView::Paren(Box::new(
            substitute_scalar_local(inner, name, row, column),
        )),
        TypedExpressionNodeView::Not(inner) => TypedExpressionNodeView::Not(Box::new(
            substitute_scalar_local(inner, name, row, column),
        )),
        TypedExpressionNodeView::Call(call) => TypedExpressionNodeView::Call(TypedCallView {
            function: call.function,
            lifted_base: call.lifted_base,
            user_function: call.user_function,
            arguments: call
                .arguments
                .iter()
                .map(|argument| substitute_scalar_local(argument, name, row, column))
                .collect(),
            outputs: call.outputs.clone(),
        }),
        TypedExpressionNodeView::If(branches) => {
            TypedExpressionNodeView::If(TypedIfExpressionView {
                branches: branches
                    .branches
                    .iter()
                    .map(|(condition, arm)| {
                        (
                            substitute_scalar_local(condition, name, row, column),
                            substitute_scalar_local(arm, name, row, column),
                        )
                    })
                    .collect(),
                else_value: Box::new(substitute_scalar_local(
                    &branches.else_value,
                    name,
                    row,
                    column,
                )),
            })
        }
        TypedExpressionNodeView::Binary {
            op,
            precedence_class,
            associativity,
            lhs,
            rhs,
            square_form,
        } => TypedExpressionNodeView::Binary {
            op: *op,
            precedence_class: *precedence_class,
            associativity: *associativity,
            lhs: Box::new(substitute_scalar_local(lhs, name, row, column)),
            rhs: Box::new(substitute_scalar_local(rhs, name, row, column)),
            square_form: *square_form,
        },
        other => other.clone(),
    };
    TypedExpressionView {
        rank: value.rank,
        extents: value.extents.clone(),
        scalar: value.scalar,
        node,
    }
}

/// `row[column]`: the element the retired accumulator's mention becomes.
fn row_element<'a>(
    original: &TypedReferenceView<'a>,
    row: &'a ast::Name,
    column: &'a ast::Name,
) -> TypedReferenceView<'a> {
    let mut column_ref = TypedReferenceView {
        rank: 0,
        extents: Some(Vec::new()),
        scalar: Some(ast::ScalarType::Integer),
        context_resident: false,
        context_arena: false,
        context_overlay: None,
        declared_extents: None,
        node: TypedReferenceNodeView::Local(TypedRefPartView {
            name: column,
            subscripts: Vec::new(),
        }),
    };
    column_ref.scalar = Some(ast::ScalarType::Integer);
    TypedReferenceView {
        rank: 0,
        extents: Some(Vec::new()),
        scalar: original.scalar,
        context_resident: false,
        context_arena: false,
        context_overlay: None,
        declared_extents: None,
        node: TypedReferenceNodeView::Local(TypedRefPartView {
            name: row,
            subscripts: vec![TypedExpressionView {
                rank: 0,
                extents: Some(Vec::new()),
                scalar: Some(ast::ScalarType::Integer),
                node: TypedExpressionNodeView::Ref(column_ref),
            }],
        }),
    }
}

/// A row-product scale operand a C-family target may evaluate once per
/// contracted step instead of once per element: pure scalar Real arithmetic
/// over literals and element references — parentheses, negation, `*` and `/`
/// — and nothing that could observe how often it runs. A comparison signals
/// through the error channel and a call is a call, so both refuse; hoisting
/// this whitelist changes only *how many times* bit-identical values are
/// recomputed, never a value and never an effect.
///
/// Returns the expression together with the root object of every reference in
/// it, so the caller can prove none of them is the run the kernel writes.
fn scale_operand<'a, 'view>(
    value: &'view TypedExpressionView<'a>,
) -> Option<(&'view TypedExpressionView<'a>, Vec<&'a str>)> {
    if value.rank != 0 || value.scalar != Some(ast::ScalarType::Real) {
        return None;
    }
    let mut roots = Vec::new();
    scale_operand_roots(value, &mut roots).then_some((value, roots))
}

/// The recursive walk behind [`scale_operand`]: `true` when every node is on
/// the whitelist, collecting reference roots along the way.
fn scale_operand_roots<'a>(value: &TypedExpressionView<'a>, roots: &mut Vec<&'a str>) -> bool {
    match &value.node {
        TypedExpressionNodeView::Real(_) | TypedExpressionNodeView::Integer(_) => true,
        TypedExpressionNodeView::Ref(reference) | TypedExpressionNodeView::Neg(reference) => {
            roots.push(reference_root(reference));
            true
        }
        TypedExpressionNodeView::Paren(inner) => scale_operand_roots(inner, roots),
        TypedExpressionNodeView::Binary {
            op: ast::BinaryOp::Mul | ast::BinaryOp::Div,
            lhs,
            rhs,
            ..
        } => scale_operand_roots(lhs, roots) && scale_operand_roots(rhs, roots),
        _ => false,
    }
}

/// Structural identity of two references, decided conservatively: identical
/// storage spelling, identical part names, and pairwise-identical subscripts
/// where a subscript is a literal or a bare scalar name. Any subscript shape
/// outside that vocabulary answers `false`, which callers treat as "not the
/// same element" — the safe direction.
fn same_reference(left: &TypedReferenceView<'_>, right: &TypedReferenceView<'_>) -> bool {
    if left.context_resident != right.context_resident
        || left.context_overlay != right.context_overlay
    {
        return false;
    }
    let (left_parts, right_parts) = match (&left.node, &right.node) {
        (TypedReferenceNodeView::Local(a), TypedReferenceNodeView::Local(b)) => {
            (std::slice::from_ref(a), std::slice::from_ref(b))
        }
        (TypedReferenceNodeView::State(a), TypedReferenceNodeView::State(b)) => {
            (a.as_slice(), b.as_slice())
        }
        _ => return false,
    };
    left_parts.len() == right_parts.len()
        && left_parts.iter().zip(right_parts).all(|(a, b)| {
            a.name.lexeme() == b.name.lexeme()
                && a.subscripts.len() == b.subscripts.len()
                && a.subscripts
                    .iter()
                    .zip(&b.subscripts)
                    .all(|(x, y)| same_subscript(x, y))
        })
}

/// Identity of one subscript pair, over the two shapes loop nests actually
/// index with: integer literals and bare scalar names. Anything else is
/// "unknown", not "equal".
fn same_subscript(left: &TypedExpressionView<'_>, right: &TypedExpressionView<'_>) -> bool {
    match (&unparenthesized(left).node, &unparenthesized(right).node) {
        (TypedExpressionNodeView::Integer(a), TypedExpressionNodeView::Integer(b)) => a == b,
        (TypedExpressionNodeView::Ref(a), TypedExpressionNodeView::Ref(b)) => {
            match (scalar_local_name(a), scalar_local_name(b)) {
                (Some(a), Some(b)) => a == b,
                _ => false,
            }
        }
        _ => false,
    }
}

/// The declared object a reference indexes into, named by its root identifier.
/// A `local` reference is that name; a state path is named by its first part,
/// which is conservative — two distinct fields of one compartment share it.
fn reference_root<'a>(reference: &TypedReferenceView<'a>) -> &'a str {
    match &reference.node {
        TypedReferenceNodeView::Local(part) => part.name.lexeme(),
        TypedReferenceNodeView::State(parts) => parts.first().map_or("", |part| part.name.lexeme()),
    }
}

/// The reference a bare `Ref` expression names, ignoring parentheses.
fn reference_of<'a, 'view>(
    value: &'view TypedExpressionView<'a>,
) -> Option<&'view TypedReferenceView<'a>> {
    match &unparenthesized(value).node {
        TypedExpressionNodeView::Ref(reference) => Some(reference),
        _ => None,
    }
}

fn unparenthesized<'a, 'view>(
    value: &'view TypedExpressionView<'a>,
) -> &'view TypedExpressionView<'a> {
    match &value.node {
        TypedExpressionNodeView::Paren(inner) => unparenthesized(inner),
        _ => value,
    }
}

/// The name of an unsubscripted single-part local reference.
fn scalar_local_name<'a>(reference: &TypedReferenceView<'a>) -> Option<&'a str> {
    match &reference.node {
        TypedReferenceNodeView::Local(part) if part.subscripts.is_empty() => {
            Some(part.name.lexeme())
        }
        _ => None,
    }
}

/// The run an operand of a contraction reads, if dropping its final subscript
/// names `count` adjacent Real elements and that final subscript is exactly the
/// contracted iterator.
///
/// The returned reference is the operand with that subscript removed, which is
/// what the kernel call passes: `A[i][k]` becomes `A[i]`, a row that decays to
/// `float *` and converts to `const float *` with no cast.
fn contiguous_run<'a>(
    operand: &TypedReferenceView<'a>,
    iterator: &ast::Name,
    count: i64,
) -> Option<TypedReferenceView<'a>> {
    if operand.rank != 0 || operand.scalar != Some(ast::ScalarType::Real) {
        return None;
    }
    let declared = operand.declared_extents.as_ref()?;
    let mut run = operand.clone();
    let part = match &mut run.node {
        TypedReferenceNodeView::Local(part) => part,
        TypedReferenceNodeView::State(parts) => parts.last_mut()?,
    };
    // Only the final part may be subscripted at all, and it must be subscripted
    // to the declaration's full rank: a partially subscripted path names an
    // array whose element offsets this cannot reason about.
    if part.subscripts.len() != declared.len() {
        return None;
    }
    let last = part.subscripts.pop()?;
    if i64::try_from(*declared.last()?).ok()? != count {
        return None;
    }
    // The dropped subscript must be the bare contracted iterator, and no
    // surviving subscript may mention it — otherwise the "run" moves as `k`
    // does and is not one run at all.
    if !is_iterator_reference(&last, iterator)
        || part
            .subscripts
            .iter()
            .any(|subscript| mentions_name_in_expression(subscript, iterator.lexeme()))
    {
        return None;
    }
    run.rank = 1;
    run.extents = Some(vec![*declared.last()?]);
    Some(run)
}

/// Whether a statement mentions a local name anywhere — as a target, inside an
/// expression, inside a subscript, or anywhere in a nested body.
///
/// Deliberately syntactic and deliberately over-approximate: it does not model
/// shadowing, so a `for` iterator that reuses the name counts as a mention and
/// the kernel rewrite backs off. Over-approximating here can only cost a kernel
/// call; under-approximating would delete a live assignment.
fn mentions_local(statement: &TypedStatementView<'_>, name: &str) -> bool {
    match statement {
        TypedStatementView::Assignment { target, value } => {
            mentions_name_in_reference(target, name) || mentions_name_in_expression(value, name)
        }
        TypedStatementView::MultiAssignment { targets, call } => {
            targets
                .iter()
                .any(|target| mentions_name_in_reference(target, name))
                || mentions_name_in_call(call, name)
        }
        TypedStatementView::Call(call) => mentions_name_in_call(call, name),
        TypedStatementView::If(statement) => {
            statement.branches.iter().any(|branch| {
                (match &branch.condition {
                    TypedConditionView::Expression(condition) => {
                        mentions_name_in_expression(condition, name)
                    }
                    TypedConditionView::SignalCheck(_) => false,
                }) || branch
                    .body
                    .iter()
                    .any(|statement| mentions_local(&statement.node, name))
            }) || statement.else_body.as_ref().is_some_and(|body| {
                body.iter()
                    .any(|statement| mentions_local(&statement.node, name))
            })
        }
        TypedStatementView::For(for_loop) => {
            for_loop
                .iterator
                .as_ref()
                .is_some_and(|iterator| iterator.lexeme() == name)
                || mentions_name_in_expression(&for_loop.start, name)
                || mentions_name_in_expression(&for_loop.stop, name)
                || for_loop
                    .step
                    .as_ref()
                    .is_some_and(|step| mentions_name_in_expression(step, name))
                || for_loop
                    .body
                    .iter()
                    .any(|statement| mentions_local(&statement.node, name))
        }
        TypedStatementView::Limit(targets) => targets.iter().any(|target| match target {
            TypedLimitTargetView::SelfState => false,
            TypedLimitTargetView::Reference(reference) => {
                mentions_name_in_reference(reference, name)
            }
        }),
        TypedStatementView::Signal(_) => false,
    }
}

fn mentions_name_in_call(call: &TypedCallView<'_>, name: &str) -> bool {
    call.arguments
        .iter()
        .any(|argument| mentions_name_in_expression(argument, name))
}

fn mentions_name_in_reference(reference: &TypedReferenceView<'_>, name: &str) -> bool {
    let parts = match &reference.node {
        TypedReferenceNodeView::Local(part) => std::slice::from_ref(part),
        TypedReferenceNodeView::State(parts) => parts.as_slice(),
    };
    // A state path's own component names are not local names, so only the first
    // part of a `local` reference can be the name itself; every part's
    // subscripts are ordinary expressions and are searched in full.
    matches!(reference.node, TypedReferenceNodeView::Local(ref part) if part.name.lexeme() == name)
        || parts.iter().any(|part| {
            part.subscripts
                .iter()
                .any(|subscript| mentions_name_in_expression(subscript, name))
        })
}

fn mentions_name_in_expression(value: &TypedExpressionView<'_>, name: &str) -> bool {
    match &value.node {
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_) => false,
        TypedExpressionNodeView::Ref(reference) | TypedExpressionNodeView::Neg(reference) => {
            mentions_name_in_reference(reference, name)
        }
        TypedExpressionNodeView::Size { array, dimension } => {
            mentions_name_in_reference(array, name) || mentions_name_in_expression(dimension, name)
        }
        TypedExpressionNodeView::Call(call) => mentions_name_in_call(call, name),
        TypedExpressionNodeView::Paren(inner) | TypedExpressionNodeView::Not(inner) => {
            mentions_name_in_expression(inner, name)
        }
        TypedExpressionNodeView::If(branches) => {
            branches.branches.iter().any(|(condition, value)| {
                mentions_name_in_expression(condition, name)
                    || mentions_name_in_expression(value, name)
            }) || mentions_name_in_expression(&branches.else_value, name)
        }
        TypedExpressionNodeView::BoundedSelection(selection) => {
            mentions_name_in_reference(&selection.reference, name)
                || selection.galec.branches.iter().any(|(condition, value)| {
                    mentions_name_in_expression(condition, name)
                        || mentions_name_in_expression(value, name)
                })
                || mentions_name_in_expression(&selection.galec.else_value, name)
        }
        TypedExpressionNodeView::Array(elements) => elements
            .iter()
            .any(|element| mentions_name_in_expression(element, name)),
        TypedExpressionNodeView::Binary { lhs, rhs, .. } => {
            mentions_name_in_expression(lhs, name) || mentions_name_in_expression(rhs, name)
        }
    }
}

fn is_iterator_reference(value: &TypedExpressionView<'_>, iterator: &ast::Name) -> bool {
    reference_of(value)
        .and_then(scalar_local_name)
        .is_some_and(|name| name == iterator.lexeme())
}

/// How a C-family target prints `lhs ^ rhs`.
///
/// Two conditions decide that a multiply is available at all, and both are
/// necessary:
///
/// 1. `rhs` is exactly the literal 2, Integer `2` or Real `2.0`. GALEC allows
///    a mixed-type `^` (see `validate/types.rs`, `allow_mixed`), and `x ^ 2`
///    with an Integer literal exponent is the common Modelica spelling, so the
///    exponent's *type* is deliberately not constrained, only its value.
/// 2. The base is Real. An Integer `^` printed as `*` would change the emitted
///    C expression's type from `float` (what `powf` returns) to `int32_t`, and
///    with it the overflow behaviour, that is a semantic change, not a
///    rounding-neutral respelling.
///
/// A third question then picks between the two multiply spellings rather than
/// refusing: is the base cheap and effect-free to *print twice*?
/// [`SquareForm::Multiply`] duplicates the rendered text, so it is reserved for
/// bases where that is free. Everything else, a builtin call, a nested
/// operator, an if-expression, a bounded selection, takes
/// [`SquareForm::BoundMultiply`], where `rumoca_galec_square`'s parameter binds
/// the base so its text appears once and its cost and its effects (a bounds
/// check, a Real comparison writing `ErrorSignalStatus`) occur exactly once, as
/// they did under the `powf` call it replaces.
///
/// Under these conditions `lhs * lhs` is not an approximation of
/// `powf(lhs, 2.0f)`, it is the correctly-rounded value of `lhs²` for *every*
/// binary32 input. That was verified exhaustively over all 2^32 bit patterns
/// against an exact oracle: `(double)x * (double)x` is error-free for binary32
/// operands (24+24 significand bits fit binary64's 53, and 2·[-149, 127] fits
/// its exponent range), so rounding it once to `float` is by construction the
/// correctly-rounded square. The single multiply matched that oracle on all
/// 4_278_190_080 finite inputs, with zero exceptions.
///
/// The bound form inherits that argument unchanged: `rumoca_galec_square` takes
/// its argument by value as a `float` and returns `float`, so its body is the
/// same single binary32 multiply with the same single rounding.
///
/// It is also what the deployment target already computes: picolibc's `powf`
/// tests for `y == 2.0f` and takes a fast path whose body is a single
/// `vmul.f32 s0, s15, s15`. So on target this rewrite is bit-for-bit inert and
/// only removes the call and its ~28 instructions of dispatch.
///
/// A *host* libm need not agree, and glibc's does not: the same exhaustive
/// sweep found `powf(x, 2.0f)` off by 1 ulp from the correctly-rounded square
/// on 1_548_806 inputs. Since `tests/suite_galec_fmu/galec_equivalence.rs` compiles the
/// generated C with the host `cc -lm`, emitting the multiply is what makes
/// that host verification leg and a picolibc target build agree on the same
/// bits instead of differing on ~0.036% of squared values.
fn real_square_form(lhs: &TypedExpressionView<'_>, rhs: &TypedExpressionView<'_>) -> SquareForm {
    if lhs.scalar != Some(ast::ScalarType::Real) || !exponent_is_two(rhs) {
        return SquareForm::Power;
    }
    if duplication_safe(lhs) {
        SquareForm::Multiply
    } else {
        SquareForm::BoundMultiply
    }
}

/// Whether an exponent is exactly the literal 2.
///
/// The literal does not always arrive bare. A Modelica `x ^ 2` over a Real base
/// carries an Integer literal exponent, and the GALEC projection makes that
/// mixed-type power explicit by wrapping the exponent in the `real` conversion
/// builtin — so the checked tree holds `real(2)`, which the C template prints as
/// `((float)(2))`. Every squared base in the RDD2 models arrives in exactly that
/// shape, so unwrapping the conversion is what makes this reduction reach real
/// code rather than only hand-built fixtures.
fn exponent_is_two(value: &TypedExpressionView<'_>) -> bool {
    match &value.node {
        TypedExpressionNodeView::Integer(literal) => *literal == 2,
        // Exact bit equality: only the literal 2.0 reduces, never a value that
        // merely prints like it.
        TypedExpressionNodeView::Real(literal) => literal.to_bits() == 2.0_f64.to_bits(),
        TypedExpressionNodeView::Paren(inner) => exponent_is_two(inner),
        TypedExpressionNodeView::Call(call) => {
            !call.user_function
                && builtin_name(call) == "real"
                && call.arguments.len() == 1
                && exponent_is_two(&call.arguments[0])
        }
        _ => false,
    }
}

/// The builtin a call resolves to, matching how the C template names it.
fn builtin_name<'a>(call: &TypedCallView<'a>) -> &'a str {
    call.lifted_base.unwrap_or_else(|| call.function.lexeme())
}

/// Whether a node's rendered C text may be printed twice at no cost and with no
/// observable effect.
///
/// Literals and references qualify; a reference qualifies only if its subscripts
/// do too, which makes this an lvalue/constant test rather than a shape test.
/// Everything else — calls, nested operators, if-expressions, bounded
/// selections — is excluded, so a base that would cost something to recompute
/// or that could re-run an effect (a user function, a Real comparison writing
/// `ErrorSignalStatus`) is squared through the binding helper instead of by
/// duplication. This is therefore a choice between two multiply spellings, not
/// a gate on whether the square reduces at all; see [`real_square_form`].
///
/// Subscripts are *not* a reason to fall back on their own. An ordinary
/// reference prints its subscripts through the `subscript` macro as `[N - 1]`
/// or `[i - 1]`; the bounds-checked `rumoca_galec_bounded_index(…)` spelling
/// belongs to the separate `bounded_reference` macro, which serves
/// `BoundedSelection` nodes and is unreachable from here. This matters in
/// practice: every squared base in the RDD2 navigation estimator is a
/// constant-subscripted array element such as `q[0]` or `ctx->omega_l[2]`, and
/// binding those would spend a helper call on text that is already free to
/// repeat.
fn duplication_safe(value: &TypedExpressionView<'_>) -> bool {
    match &value.node {
        TypedExpressionNodeView::Bool(_)
        | TypedExpressionNodeView::Integer(_)
        | TypedExpressionNodeView::Real(_) => true,
        TypedExpressionNodeView::Ref(reference) | TypedExpressionNodeView::Neg(reference) => {
            reference_duplication_safe(reference)
        }
        _ => false,
    }
}

/// A reference is safe to print twice when every subscript along it is.
fn reference_duplication_safe(reference: &TypedReferenceView<'_>) -> bool {
    let parts = match &reference.node {
        TypedReferenceNodeView::Local(part) => std::slice::from_ref(part),
        TypedReferenceNodeView::State(parts) => parts.as_slice(),
    };
    parts
        .iter()
        .all(|part| part.subscripts.iter().all(duplication_safe))
}

/// Gate tests for the `x ^ 2` emission form.
///
/// These pin the *decision*, which is the part that lives in Rust; the C tokens
/// it selects are the template's and are covered by the golden/differential
/// suites. Three outcomes are distinguished, and the boundary that matters most
/// is [`SquareForm::Power`]: a case that lands there is one where no multiply is
/// value-preserving at all. The boundary between the two multiply spellings is a
/// cost question, and each case below says which of the two it is.
#[cfg(test)]
mod square_reduction_tests {
    use super::*;

    fn scalar_view<'a>(
        scalar: ast::ScalarType,
        node: TypedExpressionNodeView<'a>,
    ) -> TypedExpressionView<'a> {
        TypedExpressionView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(scalar),
            node,
        }
    }

    fn reference<'a>(
        name: &'a ast::Name,
        subscripts: Vec<TypedExpressionView<'a>>,
    ) -> TypedReferenceView<'a> {
        TypedReferenceView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(ast::ScalarType::Real),
            context_resident: false,
            context_arena: false,
            context_overlay: None,
            declared_extents: None,
            node: TypedReferenceNodeView::Local(TypedRefPartView { name, subscripts }),
        }
    }

    fn real_ref<'a>(name: &'a ast::Name) -> TypedExpressionView<'a> {
        scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(reference(name, Vec::new())),
        )
    }

    fn integer_literal<'a>(value: i64) -> TypedExpressionView<'a> {
        scalar_view(
            ast::ScalarType::Integer,
            TypedExpressionNodeView::Integer(value),
        )
    }

    fn real_literal<'a>(value: f64) -> TypedExpressionView<'a> {
        scalar_view(ast::ScalarType::Real, TypedExpressionNodeView::Real(value))
    }

    /// The common Modelica spelling: a Real variable, an Integer literal 2.
    #[test]
    fn real_reference_with_integer_two_multiplies_in_place() {
        let x = ast::Name::ident("x");
        assert_eq!(
            real_square_form(&real_ref(&x), &integer_literal(2)),
            SquareForm::Multiply
        );
    }

    #[test]
    fn real_reference_with_real_two_multiplies_in_place() {
        let x = ast::Name::ident("x");
        assert_eq!(
            real_square_form(&real_ref(&x), &real_literal(2.0)),
            SquareForm::Multiply
        );
    }

    /// Unary minus over a plain reference is still one cheap, effect-free token.
    #[test]
    fn negated_reference_multiplies_in_place() {
        let x = ast::Name::ident("x");
        let base = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Neg(reference(&x, Vec::new())),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::Multiply
        );
    }

    /// An Integer base must keep `powf`: `i * i` would be `int32_t` arithmetic
    /// where `powf(i, 2)` is `float`, changing both the type and the overflow
    /// behaviour of the emitted expression. Binding the base would not rescue
    /// it, so this is the one case where no multiply spelling is available.
    #[test]
    fn integer_base_keeps_the_power() {
        let i = ast::Name::ident("i");
        let base = scalar_view(
            ast::ScalarType::Integer,
            TypedExpressionNodeView::Ref(reference(&i, Vec::new())),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::Power
        );
    }

    /// Only the exponent 2 reduces; a cube has no single-multiply equivalent.
    #[test]
    fn exponent_three_keeps_the_power() {
        let x = ast::Name::ident("x");
        assert_eq!(
            real_square_form(&real_ref(&x), &integer_literal(3)),
            SquareForm::Power
        );
    }

    /// A value that merely rounds to 2.0 in print is not the literal 2.
    #[test]
    fn exponent_near_two_keeps_the_power() {
        let x = ast::Name::ident("x");
        assert_eq!(
            real_square_form(&real_ref(&x), &real_literal(2.000_000_1)),
            SquareForm::Power
        );
    }

    /// A constant-subscripted array element is duplicated in place. This is the
    /// shape every squared base in the RDD2 navigation estimator actually has
    /// (`q[0]`, `ctx->omega_l[2]`, …), and it prints as `[N - 1]`, a constant,
    /// free to repeat.
    #[test]
    fn literal_subscripted_base_multiplies_in_place() {
        let v = ast::Name::ident("v");
        let base = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(reference(&v, vec![integer_literal(1)])),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::Multiply
        );
    }

    /// A loop-induction subscript is duplicated too: it prints as `[i - 1]`, a
    /// plain local read with no effect and no meaningful recomputation cost.
    #[test]
    fn induction_subscripted_base_multiplies_in_place() {
        let v = ast::Name::ident("v");
        let i = ast::Name::ident("i");
        let base = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(reference(&v, vec![real_ref(&i)])),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::Multiply
        );
    }

    /// A *computed* subscript is bound instead: duplicating it would recompute
    /// the index expression on every use.
    #[test]
    fn computed_subscript_base_binds_the_operand() {
        let v = ast::Name::ident("v");
        let i = ast::Name::ident("i");
        let index = scalar_view(
            ast::ScalarType::Integer,
            TypedExpressionNodeView::Binary {
                op: ast::BinaryOp::Add,
                precedence_class: ast::PrecedenceClass::Additive,
                associativity: ast::Associativity::Left,
                lhs: Box::new(real_ref(&i)),
                rhs: Box::new(integer_literal(1)),
                square_form: SquareForm::Power,
            },
        );
        let base = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(reference(&v, vec![index])),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::BoundMultiply
        );
    }

    /// A function-call base is bound. The estimator has these:
    /// `rumoca_galec_max(theta_sq, eps) ^ 2` — and duplicating a call is both a
    /// cost and, for a user function, an effect; binding it is neither.
    #[test]
    fn call_base_binds_the_operand() {
        let f = ast::Name::ident("f");
        let base = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Call(TypedCallView {
                function: &f,
                lifted_base: None,
                user_function: true,
                arguments: Vec::new(),
                outputs: Vec::new(),
            }),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::BoundMultiply
        );
    }

    /// A compound base, `(a + b) ^ 2`, is bound: printing the sum twice would
    /// recompute it.
    #[test]
    fn parenthesized_base_binds_the_operand() {
        let a = ast::Name::ident("a");
        let base = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Paren(Box::new(real_ref(&a))),
        );
        assert_eq!(
            real_square_form(&base, &integer_literal(2)),
            SquareForm::BoundMultiply
        );
    }

    /// The shape real models actually produce: `x ^ real(2)`, the GALEC
    /// projection's explicit conversion of a Modelica Integer exponent. Every
    /// squared base in the RDD2 estimator arrives this way, so this is the case
    /// that decides whether the reduction does anything at all.
    #[test]
    fn real_conversion_wrapped_exponent_reduces() {
        let x = ast::Name::ident("x");
        let real = ast::Name::ident("real");
        let exponent = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Call(TypedCallView {
                function: &real,
                lifted_base: None,
                user_function: false,
                arguments: vec![integer_literal(2)],
                outputs: Vec::new(),
            }),
        );
        assert_eq!(
            real_square_form(&real_ref(&x), &exponent),
            SquareForm::Multiply
        );
    }

    /// The same wrapper around a different literal must not reduce.
    #[test]
    fn real_conversion_wrapped_three_keeps_the_power() {
        let x = ast::Name::ident("x");
        let real = ast::Name::ident("real");
        let exponent = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Call(TypedCallView {
                function: &real,
                lifted_base: None,
                user_function: false,
                arguments: vec![integer_literal(3)],
                outputs: Vec::new(),
            }),
        );
        assert_eq!(
            real_square_form(&real_ref(&x), &exponent),
            SquareForm::Power
        );
    }

    /// A *user* function that happens to be named `real` is not the builtin.
    #[test]
    fn user_function_named_real_keeps_the_power() {
        let x = ast::Name::ident("x");
        let real = ast::Name::ident("real");
        let exponent = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Call(TypedCallView {
                function: &real,
                lifted_base: None,
                user_function: true,
                arguments: vec![integer_literal(2)],
                outputs: Vec::new(),
            }),
        );
        assert_eq!(
            real_square_form(&real_ref(&x), &exponent),
            SquareForm::Power
        );
    }

    /// A non-literal exponent is unknown at emission time.
    #[test]
    fn variable_exponent_keeps_the_power() {
        let x = ast::Name::ident("x");
        let n = ast::Name::ident("n");
        assert_eq!(
            real_square_form(&real_ref(&x), &real_ref(&n)),
            SquareForm::Power
        );
    }
}

#[derive(Clone)]
struct ShapeEvidence {
    rank: usize,
    extents: Option<Vec<usize>>,
    scalar: Option<ast::ScalarType>,
}

impl ShapeEvidence {
    fn scalar(scalar: ast::ScalarType) -> Self {
        Self {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(scalar),
        }
    }

    fn unknown_scalar() -> Self {
        Self {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: None,
        }
    }

    fn of_reference(reference: &TypedReferenceView<'_>) -> Self {
        Self {
            rank: reference.rank,
            extents: reference.extents.clone(),
            scalar: reference.scalar,
        }
    }

    fn of_expression(expression: &TypedExpressionView<'_>) -> Self {
        Self {
            rank: expression.rank,
            extents: expression.extents.clone(),
            scalar: expression.scalar,
        }
    }
}

fn declaration_shape(declaration: &ast::VariableDeclaration) -> ShapeEvidence {
    ShapeEvidence {
        rank: declaration.dimensions.len(),
        extents: literal_extents(&declaration.dimensions),
        scalar: match declaration.ty {
            ast::TypeRef::Primitive(scalar) => Some(scalar),
            ast::TypeRef::Compartment(_) => None,
        },
    }
}

fn literal_extents(dimensions: &[ast::Dimension]) -> Option<Vec<usize>> {
    dimensions
        .iter()
        .map(|dimension| match dimension {
            ast::Dimension::Expr(ast::Expression::Integer(value)) if *value > 0 => {
                usize::try_from(*value).ok()
            }
            ast::Dimension::Derived | ast::Dimension::Expr(_) => None,
        })
        .collect()
}

const fn builtin_rank(ty: rumoca_ir_galec::builtins::BuiltinType) -> usize {
    use rumoca_ir_galec::builtins::BuiltinType;
    match ty {
        BuiltinType::Boolean | BuiltinType::Integer | BuiltinType::Real => 0,
        BuiltinType::IntegerVector | BuiltinType::RealVector => 1,
        BuiltinType::RealMatrix => 2,
        BuiltinType::RealArray3 => 3,
    }
}

fn require_equal_shape(
    expected: ShapeEvidence,
    found: ShapeEvidence,
    context: &str,
) -> Result<(), String> {
    if expected.rank != found.rank {
        return Err(format!(
            "{context} rank mismatch ({} != {})",
            expected.rank, found.rank
        ));
    }
    if matches!(
        (&expected.extents, &found.extents),
        (Some(expected), Some(found)) if expected != found
    ) {
        return Err(format!("{context} extent mismatch"));
    }
    Ok(())
}

fn broadcast_shape(lhs: ShapeEvidence, rhs: ShapeEvidence) -> Result<ShapeEvidence, String> {
    if lhs.rank == 0 {
        return Ok(rhs);
    }
    if rhs.rank == 0 {
        return Ok(lhs);
    }
    require_equal_shape(lhs.clone(), rhs, "checked binary operands")?;
    Ok(lhs)
}

fn reference_shape(
    declaration: &ast::VariableDeclaration,
    part: &ast::RefPart,
) -> Result<ShapeEvidence, String> {
    let rank = declaration
        .dimensions
        .len()
        .checked_sub(part.subscripts.len())
        .ok_or_else(|| {
            format!(
                "checked reference `{}` has too many subscripts",
                declaration.name.lexeme()
            )
        })?;
    let extents = if part.subscripts.is_empty() {
        literal_extents(&declaration.dimensions)
    } else if rank == 0 {
        Some(Vec::new())
    } else {
        None
    };
    Ok(ShapeEvidence {
        rank,
        extents,
        scalar: match declaration.ty {
            ast::TypeRef::Primitive(scalar) => Some(scalar),
            ast::TypeRef::Compartment(_) => None,
        },
    })
}

const fn builtin_scalar(ty: rumoca_ir_galec::builtins::BuiltinType) -> ast::ScalarType {
    use rumoca_ir_galec::builtins::BuiltinType;
    match ty {
        BuiltinType::Boolean => ast::ScalarType::Boolean,
        BuiltinType::Integer | BuiltinType::IntegerVector => ast::ScalarType::Integer,
        BuiltinType::Real
        | BuiltinType::RealVector
        | BuiltinType::RealMatrix
        | BuiltinType::RealArray3 => ast::ScalarType::Real,
    }
}

/// Properties of the overlaid working-memory layout ([`ScratchLayoutView`]).
///
/// These are deliberately *properties over the computed layout* rather than
/// assertions about rendered text: the one thing that can turn the overlay into
/// silent wrong code is two regions sharing storage while both are live, and
/// that is a fact about the call graph, not about C syntax.
#[cfg(test)]
mod kernelize_tests {
    use super::*;

    struct GroupNames {
        acc: ast::Name,
        target: ast::Name,
        lhs: ast::Name,
        rhs: ast::Name,
        iterator: Option<ast::Name>,
    }

    impl GroupNames {
        fn new() -> Self {
            Self {
                acc: ast::Name::ident("acc"),
                target: ast::Name::ident("t"),
                lhs: ast::Name::ident("l"),
                rhs: ast::Name::ident("r"),
                iterator: Some(ast::Name::ident("k")),
            }
        }
    }

    fn stmt<'a>(node: TypedStatementView<'a>) -> TypedSpannedStatement<'a> {
        TypedSpannedStatement {
            trace: None,
            kernel: None,
            node,
        }
    }

    fn expr<'a>(
        scalar: ast::ScalarType,
        node: TypedExpressionNodeView<'a>,
    ) -> TypedExpressionView<'a> {
        TypedExpressionView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(scalar),
            node,
        }
    }

    fn scalar_ref<'a>(name: &'a ast::Name) -> TypedReferenceView<'a> {
        TypedReferenceView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(ast::ScalarType::Real),
            context_resident: false,
            context_arena: false,
            context_overlay: None,
            declared_extents: None,
            node: TypedReferenceNodeView::Local(TypedRefPartView {
                name,
                subscripts: Vec::new(),
            }),
        }
    }

    fn ref_expr<'a>(name: &'a ast::Name) -> TypedExpressionView<'a> {
        expr(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(scalar_ref(name)),
        )
    }

    /// `name[k]`: fully subscripted element of a `Real[3]` run.
    fn run_element<'a>(name: &'a ast::Name, iterator: &'a ast::Name) -> TypedExpressionView<'a> {
        let mut iterator_ref = scalar_ref(iterator);
        iterator_ref.scalar = Some(ast::ScalarType::Integer);
        expr(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(TypedReferenceView {
                rank: 0,
                extents: Some(Vec::new()),
                scalar: Some(ast::ScalarType::Real),
                context_resident: false,
                context_arena: false,
                context_overlay: None,
                declared_extents: Some(vec![3]),
                node: TypedReferenceNodeView::Local(TypedRefPartView {
                    name,
                    subscripts: vec![expr(
                        ast::ScalarType::Integer,
                        TypedExpressionNodeView::Ref(iterator_ref),
                    )],
                }),
            }),
        )
    }

    fn binary<'a>(
        op: ast::BinaryOp,
        lhs: TypedExpressionView<'a>,
        rhs: TypedExpressionView<'a>,
    ) -> TypedExpressionView<'a> {
        expr(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Binary {
                op,
                precedence_class: op.precedence_class(),
                associativity: op.precedence_class().associativity(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                square_form: SquareForm::Power,
            },
        )
    }

    /// The canonical accumulate group: `acc := 0.0; for k in 1:3 loop
    /// acc := acc + l[k] * r[k]; end for; t := acc;`
    fn accumulate_group<'a>(names: &'a GroupNames) -> Vec<TypedSpannedStatement<'a>> {
        let iterator = names.iterator.as_ref().expect("iterator name");
        vec![
            stmt(TypedStatementView::Assignment {
                target: scalar_ref(&names.acc),
                value: expr(ast::ScalarType::Real, TypedExpressionNodeView::Real(0.0)),
            }),
            stmt(TypedStatementView::For(Box::new(TypedForView {
                iterator: &names.iterator,
                start: expr(
                    ast::ScalarType::Integer,
                    TypedExpressionNodeView::Integer(1),
                ),
                step: None,
                stop: expr(
                    ast::ScalarType::Integer,
                    TypedExpressionNodeView::Integer(3),
                ),
                c_locals: Vec::new(),
                body: vec![stmt(TypedStatementView::Assignment {
                    target: scalar_ref(&names.acc),
                    value: binary(
                        ast::BinaryOp::Add,
                        ref_expr(&names.acc),
                        binary(
                            ast::BinaryOp::Mul,
                            run_element(&names.lhs, iterator),
                            run_element(&names.rhs, iterator),
                        ),
                    ),
                })],
                row_contraction: None,
            }))),
            stmt(TypedStatementView::Assignment {
                target: scalar_ref(&names.target),
                value: ref_expr(&names.acc),
            }),
        ]
    }

    #[test]
    fn frame_declared_accumulator_is_absorbed() {
        let names = GroupNames::new();
        let declaration =
            ast::VariableDeclaration::scalar(ast::ScalarType::Real, ast::Name::ident("acc"));
        let frame = vec![TypedLocalView {
            decl: &declaration,
            needs_unused_marker: false,
        }];
        let mut statements = accumulate_group(&names);
        let dead = kernelize(&mut statements, &frame);
        assert!(dead.contains("acc"));
        assert!(matches!(
            statements[0].kernel,
            Some(KernelStatementView::Absorbed)
        ));
        assert!(matches!(
            statements[2].kernel,
            Some(KernelStatementView::Dot { count: 3, .. })
        ));
    }

    /// Frame membership alone is not license to *retire*: a sibling statement
    /// in the same list reading the accumulator after the canonical
    /// three-statement group means the group is not the whole life of the
    /// name, so the store and the declaration must both survive. The
    /// reduction itself is still the dot kernel — the two-statement pass
    /// prints `acc = rumoca_galec_dot_real(...)`, which writes the identical
    /// value the loop wrote, and every later reader (the store and the
    /// sibling) still reads it.
    #[test]
    fn sibling_reader_of_the_accumulator_keeps_the_store_and_the_local() {
        let names = GroupNames::new();
        let extra_target = ast::Name::ident("t2");
        let declaration =
            ast::VariableDeclaration::scalar(ast::ScalarType::Real, ast::Name::ident("acc"));
        let frame = vec![TypedLocalView {
            decl: &declaration,
            needs_unused_marker: false,
        }];
        let mut statements = accumulate_group(&names);
        statements.push(stmt(TypedStatementView::Assignment {
            target: scalar_ref(&extra_target),
            value: ref_expr(&names.acc),
        }));
        let dead = kernelize(&mut statements, &frame);
        // Nothing is retired: the local survives for the store and the
        // sibling reader, both of which keep their statements.
        assert!(dead.is_empty());
        assert!(matches!(
            statements[0].kernel,
            Some(KernelStatementView::Absorbed)
        ));
        assert!(matches!(
            statements[1].kernel,
            Some(KernelStatementView::Dot { count: 3, .. })
        ));
        assert!(statements[2].kernel.is_none(), "the store must survive");
        assert!(
            statements[3].kernel.is_none(),
            "the sibling reader must survive"
        );
    }

    /// The accumulator's declaration living at an *outer* frame means some
    /// scope outside this statement list also uses the name — the placement
    /// is the common prefix of all uses. Retiring the declaration is
    /// therefore off the table, but the reduction itself may still print as
    /// `acc = rumoca_galec_dot_real(...)`: the write survives with the
    /// identical value, so every outer reader is unaffected.
    #[test]
    fn outer_declared_accumulator_keeps_its_write() {
        let names = GroupNames::new();
        let mut statements = accumulate_group(&names);
        let dead = kernelize(&mut statements, &[]);
        assert!(dead.is_empty());
        assert!(matches!(
            statements[0].kernel,
            Some(KernelStatementView::Absorbed)
        ));
        assert!(matches!(
            statements[1].kernel,
            Some(KernelStatementView::Dot { count: 3, .. })
        ));
        assert!(statements[2].kernel.is_none(), "the store must survive");
    }

    /// The guard parser behind the index-restriction rewrite. Getting a bound
    /// wrong silently drops block writes, so the property pinned here is
    /// two-sided: exact rectangles for the conjunction vocabulary, and a hard
    /// `false` for anything outside it.
    #[test]
    fn conjunctive_index_bounds_are_exact_and_fail_closed() {
        let row = ast::Name::ident("i");
        let column = ast::Name::ident("j");
        fn compare<'a>(
            op: ast::BinaryOp,
            lhs: TypedExpressionView<'a>,
            rhs: TypedExpressionView<'a>,
        ) -> TypedExpressionView<'a> {
            binary(op, lhs, rhs)
        }
        let literal = |value: i64| {
            expr(
                ast::ScalarType::Integer,
                TypedExpressionNodeView::Integer(value),
            )
        };

        // ((i >= 4) && (i <= 6)) && ((j >= 7) && (j <= 9))  ->  [4,6] x [7,9]
        let condition = binary(
            ast::BinaryOp::And,
            binary(
                ast::BinaryOp::And,
                compare(ast::BinaryOp::Ge, ref_expr(&row), literal(4)),
                compare(ast::BinaryOp::Le, ref_expr(&row), literal(6)),
            ),
            binary(
                ast::BinaryOp::And,
                compare(ast::BinaryOp::Ge, ref_expr(&column), literal(7)),
                compare(ast::BinaryOp::Le, ref_expr(&column), literal(9)),
            ),
        );
        let mut bounds = [(1, 15), (1, 15)];
        assert!(conjunctive_index_bounds(
            &condition,
            &row,
            &column,
            &mut bounds
        ));
        assert_eq!(bounds, [(4, 6), (7, 9)]);

        // Mirrored and strict spellings: (3 < i) && (j == 5)  ->  [4,15] x [5,5]
        let condition = binary(
            ast::BinaryOp::And,
            compare(ast::BinaryOp::Lt, literal(3), ref_expr(&row)),
            compare(ast::BinaryOp::Eq, ref_expr(&column), literal(5)),
        );
        let mut bounds = [(1, 15), (1, 15)];
        assert!(conjunctive_index_bounds(
            &condition,
            &row,
            &column,
            &mut bounds
        ));
        assert_eq!(bounds, [(4, 15), (5, 5)]);

        // A disjunction is not a rectangle.
        let condition = binary(
            ast::BinaryOp::Or,
            compare(ast::BinaryOp::Le, ref_expr(&row), literal(3)),
            compare(ast::BinaryOp::Ge, ref_expr(&row), literal(9)),
        );
        let mut bounds = [(1, 15), (1, 15)];
        assert!(!conjunctive_index_bounds(
            &condition,
            &row,
            &column,
            &mut bounds
        ));

        // A third name is not one of the loop's indices.
        let other = ast::Name::ident("n");
        let condition = compare(ast::BinaryOp::Le, ref_expr(&row), ref_expr(&other));
        let mut bounds = [(1, 15), (1, 15)];
        assert!(!conjunctive_index_bounds(
            &condition,
            &row,
            &column,
            &mut bounds
        ));
    }
}

#[cfg(test)]
mod layout_tests {
    use super::*;

    fn ident(name: &str) -> ast::Name {
        ast::Name::ident(name)
    }

    /// A `Real[extent]` local — an array, so context-resident.
    fn array_local(name: &str, extent: i64) -> ast::VariableDeclaration {
        let mut declaration = ast::VariableDeclaration::scalar(ast::ScalarType::Real, ident(name));
        declaration.dimensions = vec![ast::Dimension::Expr(ast::Expression::Integer(extent))];
        declaration
    }

    fn parameter(direction: ast::Direction, name: &str, extent: i64) -> ast::Parameter {
        ast::Parameter {
            direction,
            decl: array_local(name, extent),
        }
    }

    /// `function <name>(input u[extent]) => (output y[extent])` whose body is
    /// `y := u;` followed by one `(t) := <callee>(u);` per callee.
    fn function(name: &str, extent: i64, callees: &[&str]) -> ast::UserFunction {
        let mut statements = vec![ast::Spanned::dummy(ast::Statement::Assignment {
            target: ast::Reference::local(ident("y")),
            value: ast::Expression::Ref(ast::Reference::local(ident("u"))),
        })];
        for callee in callees {
            statements.push(ast::Spanned::dummy(ast::Statement::MultiAssignment {
                targets: vec![ast::Reference::local(ident("t"))],
                call: ast::FunctionCall {
                    function: ident(callee),
                    arguments: vec![ast::Expression::Ref(ast::Reference::local(ident("u")))],
                },
            }));
        }
        ast::UserFunction {
            kind: ast::FunctionKind::Stateless,
            name: ident(name),
            signals: Vec::new(),
            parameters: vec![
                parameter(ast::Direction::Input, "u", extent),
                parameter(ast::Direction::Output, "y", extent),
            ],
            locals: vec![array_local("t", extent)],
            statements,
            span: rumoca_core::Span::DUMMY,
        }
    }

    /// A block whose `DoStep` calls `roots`, with `functions` declared
    /// protected.
    fn block_of(functions: Vec<ast::UserFunction>, roots: &[&str]) -> ast::Block {
        let mut block = ast::Block::new(ident("Fixture"));
        block.protected_functions = functions;
        block.do_step.statements = roots
            .iter()
            .map(|root| {
                ast::Spanned::dummy(ast::Statement::MultiAssignment {
                    targets: vec![ast::Reference::local(ident("out"))],
                    call: ast::FunctionCall {
                        function: ident(root),
                        arguments: vec![ast::Expression::Ref(ast::Reference::local(ident("out")))],
                    },
                })
            })
            .collect();
        block.do_step.locals = vec![array_local("out", 4)];
        block
    }

    fn layout<'a>(
        block: &'a ast::Block,
        sources: &'a rumoca_core::SourceMap,
    ) -> ScratchLayoutView<'a> {
        super::block(block, sources)
            .expect("fixture must project")
            .0
            .scratch_layout
    }

    /// Every owner that carries a region, paired with the statements a target
    /// prints for it: what the arena census re-derives its evidence from.
    fn owners_with_statements<'view, 'a>(
        view: &'view TypedBlockView<'a>,
    ) -> Vec<(
        &'a str,
        &'view ScratchRegionView<'a>,
        &'view [TypedSpannedStatement<'a>],
    )> {
        let methods = [
            ("startup", &view.startup),
            ("recalibrate", &view.recalibrate),
            ("dostep", &view.do_step),
        ]
        .into_iter()
        .map(|(spelling, method)| (spelling, &method.scratch, method.statements.as_slice()));
        view.protected_functions
            .iter()
            .chain(&view.public_functions)
            .map(|function| {
                (
                    function.name.lexeme(),
                    &function.scratch,
                    function.statements.as_slice(),
                )
            })
            .chain(methods)
            .collect()
    }

    /// Every unordered pair of arena slots whose byte ranges overlap.
    fn overlapping_pairs<'view, 'a>(
        slots: &'view [ArenaSlotView<'a>],
    ) -> Vec<(&'view ArenaSlotView<'a>, &'view ArenaSlotView<'a>)> {
        let bytes = |slot: &ArenaSlotView<'a>| slot.slot.bytes.expect("an arena slot is sizable");
        slots
            .iter()
            .enumerate()
            .flat_map(|(index, placed)| slots[index + 1..].iter().map(move |other| (placed, other)))
            .filter(|(placed, other)| {
                let low = placed.offset * ARENA_ALIGN;
                let other_low = other.offset * ARENA_ALIGN;
                low < other_low + bytes(other) && other_low < low + bytes(placed)
            })
            .collect()
    }

    /// THE arena soundness property, checked end to end through the finished
    /// projection: in every region, every pair of arena slots whose byte
    /// ranges overlap is a pair the relation proves can never be live at the
    /// same moment.
    ///
    /// A layout violating this is a silent miscompile: one temporary writes
    /// over another that is still holding a value. The prover's own unit tests
    /// in `algorithm_code_arena` pin the relation on hand-built use sets; this
    /// pins that the placement which SURVIVED into the view still satisfies it
    /// against evidence re-derived from the view's own statements. That is the
    /// part a unit test cannot cover, because the passes that run after the
    /// placement (bound equalization, the accounting) are free to invalidate
    /// it and this is what would notice.
    ///
    /// The fixture is deliberately awkward: sequential temporaries that must
    /// share, a temporary live across everything that must not, two arms that
    /// must share with each other and with neither, and a loop body where
    /// nothing may share.
    #[test]
    fn overlapping_arena_slots_are_pairwise_never_concurrent() {
        let block = block_of(
            vec![
                sequential_function("chained", 16),
                branching_function("branching", 8),
                looping_function("looping", 8),
            ],
            &["chained", "branching", "looping"],
        );
        let sources = rumoca_core::SourceMap::new();
        let view = super::block(&block, &sources)
            .expect("fixture must project")
            .0;

        let mut arenas_seen = 0usize;
        for (owner, region, statements) in owners_with_statements(&view) {
            let Some(arena) = region.arena.as_ref() else {
                continue;
            };
            arenas_seen += 1;
            // Re-derive the evidence from the statements this owner prints,
            // rather than trusting whatever the placement was handed.
            let uses = arena::SlotUses::observed(arena_slot_uses(statements));
            for placed in &arena.slots {
                let bytes = placed.slot.bytes.expect("an arena slot is sizable");
                assert_eq!(
                    placed.floats * ARENA_ALIGN,
                    bytes,
                    "`{owner}`.`{}` reports an extent its size disagrees with",
                    placed.slot.lexeme()
                );
                assert!(
                    placed.offset + placed.floats <= arena.floats,
                    "`{owner}`.`{}` runs past the arena it is declared in",
                    placed.slot.lexeme()
                );
            }
            for (placed, other) in overlapping_pairs(&arena.slots) {
                assert!(
                    uses.never_concurrent(placed.slot.lexeme(), other.slot.lexeme()),
                    "`{owner}`: `{}` and `{}` overlap in the arena without a proof",
                    placed.slot.lexeme(),
                    other.slot.lexeme()
                );
            }
        }
        assert!(
            arenas_seen > 0,
            "the fixture must exercise at least one arena, or this proves nothing"
        );
    }

    /// What the arena is FOR, as a layout property: two sequential temporaries
    /// land on one offset, and the temporary that is live across both does
    /// not. Without the second half the first would pass for the uninteresting
    /// reason that everything shares.
    #[test]
    fn sequential_temporaries_share_an_offset_and_a_spanning_one_does_not() {
        let block = block_of(vec![sequential_function("chained", 16)], &["chained"]);
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let region = region_of(&resolved, "chained");

        let (first, _) = arena_of(region, "first").expect("`first` is arena-placed");
        let (second, _) = arena_of(region, "second").expect("`second` is arena-placed");
        let (spanning, _) = arena_of(region, "spanning").expect("`spanning` is arena-placed");
        assert_eq!(
            first, second,
            "two temporaries used one after the other share one offset"
        );
        assert_ne!(
            spanning, first,
            "a temporary read after both must keep bytes of its own"
        );
    }

    /// NEGATIVE CONTROL. Two temporaries used in one loop body never share:
    /// statement order within an iteration says nothing across the back edge.
    #[test]
    fn two_temporaries_inside_one_loop_body_never_share_an_offset() {
        let block = block_of(vec![looping_function("looping", 8)], &["looping"]);
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let region = region_of(&resolved, "looping");

        // Refusing them the arena entirely is the same refusal, stronger, so
        // only a pair that both landed has anything to check.
        if let (Some((early, _)), Some((late, _))) =
            (arena_of(region, "early"), arena_of(region, "late"))
        {
            assert_ne!(early, late, "a loop body orders nothing across iterations");
        }
    }

    /// Every declared region, keyed by owner, with the group it landed in.
    fn groups_by_owner<'a>(layout: &ScratchLayoutView<'a>) -> BTreeMap<&'a str, usize> {
        layout
            .groups
            .iter()
            .flat_map(|group| {
                group
                    .regions
                    .iter()
                    .map(|region| (region.owner(), group.ordinal))
            })
            .collect()
    }

    /// An INDEPENDENT call-edge walk, for the oracle only.
    ///
    /// Deliberately not `collect_called_functions`. That collector is the
    /// overlay's single soundness input, and an oracle built from it shares its
    /// blind spots: an edge the collector misses is an edge the layout does not
    /// separate AND an edge the property test does not look for, so the two
    /// agree and the test passes on wrong code. This walk is written against
    /// `ast::Statement` from the other direction — it visits every node and
    /// records every `FunctionCall` it meets, with no notion of which positions
    /// a target can reach — so a hole in one is not a hole in both.
    fn every_call<'a>(
        statements: &'a [ast::Spanned<ast::Statement>],
        found: &mut BTreeSet<&'a str>,
    ) {
        fn if_expression<'a>(conditional: &'a ast::IfExpression, found: &mut BTreeSet<&'a str>) {
            for (condition, branch) in &conditional.branches {
                expression(condition, found);
                expression(branch, found);
            }
            expression(&conditional.else_value, found);
        }

        fn expression<'a>(value: &'a ast::Expression, found: &mut BTreeSet<&'a str>) {
            match value {
                ast::Expression::Call(call) => {
                    found.insert(call.function.lexeme());
                    call.arguments.iter().for_each(|a| expression(a, found));
                }
                ast::Expression::Paren(v) | ast::Expression::Not(v) => expression(v, found),
                ast::Expression::Binary { lhs, rhs, .. } => {
                    expression(lhs, found);
                    expression(rhs, found);
                }
                ast::Expression::If(v) => if_expression(v, found),
                ast::Expression::Array(values) => {
                    values.iter().for_each(|v| expression(v, found));
                }
                ast::Expression::Size { array, dimension } => {
                    reference(array, found);
                    expression(dimension, found);
                }
                ast::Expression::Ref(r) | ast::Expression::Neg(r) => reference(r, found),
                ast::Expression::Bool(_)
                | ast::Expression::Integer(_)
                | ast::Expression::Real(_) => {}
            }
        }
        fn reference<'a>(value: &'a ast::Reference, found: &mut BTreeSet<&'a str>) {
            let parts = match value {
                ast::Reference::Local(part) => std::slice::from_ref(part),
                ast::Reference::State(parts) => parts.as_slice(),
            };
            for subscript in parts.iter().flat_map(|part| &part.subscripts) {
                expression(subscript, found);
            }
        }

        fn condition<'a>(condition: &'a ast::Condition, found: &mut BTreeSet<&'a str>) {
            match condition {
                ast::Condition::Expression(value) => expression(value, found),
                ast::Condition::SignalCheck(check) => {
                    check
                        .fallback
                        .iter()
                        .for_each(|fallback| expression(fallback, found));
                }
            }
        }

        fn conditional<'a>(conditional: &'a ast::IfStatement, found: &mut BTreeSet<&'a str>) {
            for branch in &conditional.branches {
                condition(&branch.condition, found);
                every_call(&branch.body, found);
            }
            if let Some(body) = &conditional.else_body {
                every_call(body, found);
            }
        }

        fn for_loop<'a>(loop_: &'a ast::ForLoop, found: &mut BTreeSet<&'a str>) {
            expression(&loop_.start, found);
            if let Some(step) = &loop_.step {
                expression(step, found);
            }
            expression(&loop_.stop, found);
            every_call(&loop_.body, found);
        }

        fn limit_targets<'a>(targets: &'a [ast::LimitTarget], found: &mut BTreeSet<&'a str>) {
            for value in targets.iter().filter_map(|target| match target {
                ast::LimitTarget::SelfState => None,
                ast::LimitTarget::Reference(value) => Some(value),
            }) {
                reference(value, found);
            }
        }

        for statement in statements {
            match &statement.node {
                ast::Statement::Assignment { target, value } => {
                    reference(target, found);
                    expression(value, found);
                }
                ast::Statement::MultiAssignment { targets, call } => {
                    targets.iter().for_each(|t| reference(t, found));
                    found.insert(call.function.lexeme());
                    call.arguments.iter().for_each(|a| expression(a, found));
                }
                ast::Statement::Call(call) => {
                    found.insert(call.function.lexeme());
                    call.arguments.iter().for_each(|a| expression(a, found));
                }
                ast::Statement::If(value) => conditional(value, found),
                ast::Statement::For(value) => for_loop(value, found),
                ast::Statement::Limit(targets) => limit_targets(targets, found),
                ast::Statement::Signal(_) => {}
            }
        }
    }

    /// Everything reachable from `name` by any number of call edges, under the
    /// independent walk.
    fn below<'a>(
        name: &'a str,
        functions: &HashMap<&'a str, &'a ast::UserFunction>,
        reached: &mut BTreeSet<&'a str>,
    ) {
        let mut callees = BTreeSet::new();
        if let Some(function) = functions.get(name) {
            every_call(&function.statements, &mut callees);
        }
        for callee in callees {
            include_callee(callee, functions, reached);
        }
    }

    fn include_callee<'a>(
        callee: &str,
        functions: &HashMap<&'a str, &'a ast::UserFunction>,
        reached: &mut BTreeSet<&'a str>,
    ) {
        if let Some((declared, _)) = functions.get_key_value(callee)
            && reached.insert(*declared)
        {
            below(declared, functions, reached);
        }
    }

    /// Every (caller, callee) pair that lies on some common call chain,
    /// transitive pairs included, with the three methods as roots. Computed
    /// independently of the layout so that it cannot agree with it by
    /// construction.
    fn chain_pairs(block: &ast::Block) -> BTreeSet<(&str, &str)> {
        let functions: HashMap<&str, &ast::UserFunction> = block
            .protected_functions
            .iter()
            .chain(&block.public_functions)
            .map(|function| (function.name.lexeme(), function))
            .collect();
        let mut pairs = BTreeSet::new();
        for (spelling, method) in [
            ("startup", &block.startup),
            ("recalibrate", &block.recalibrate),
            ("dostep", &block.do_step),
        ] {
            let mut direct = BTreeSet::new();
            every_call(&method.statements, &mut direct);
            let mut reached = BTreeSet::new();
            for callee in direct {
                include_callee(callee, &functions, &mut reached);
            }
            for name in reached {
                pairs.insert((spelling, name));
            }
        }
        for &name in functions.keys() {
            let mut reached = BTreeSet::new();
            below(name, &functions, &mut reached);
            for callee in reached {
                pairs.insert((name, callee));
            }
        }
        pairs
    }

    /// THE soundness property, checked end to end through the projection: two
    /// regions share a group only when their owners can never be active
    /// together, which is exactly "no caller shares a group with any of its
    /// transitive callees".
    ///
    /// A layout violating this is a silent miscompile: the callee overwrites
    /// the caller's live intermediates. The prover's own unit tests in
    /// `algorithm_code_overlay` pin the relation; this pins that the projection
    /// hands the prover the whole call graph, over an awkward one.
    #[test]
    fn no_caller_shares_a_group_with_a_transitive_callee() {
        // A deliberately awkward graph: a diamond, a three-deep chain, a leaf
        // reached at two different depths, and an unreachable function.
        let block = block_of(
            vec![
                function("top", 8, &["left", "right"]),
                function("left", 8, &["shared"]),
                function("right", 8, &["deep"]),
                function("deep", 8, &["shared"]),
                function("shared", 8, &[]),
                function("orphan", 8, &[]),
            ],
            &["top"],
        );
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let groups = groups_by_owner(&resolved);
        for (caller, callee) in chain_pairs(&block) {
            let (Some(above), Some(under)) = (groups.get(caller), groups.get(callee)) else {
                continue;
            };
            assert_ne!(
                above, under,
                "`{caller}` and its transitive callee `{callee}` share overlay group {above}"
            );
        }
        // `shared` is reached through `left` and through `right -> deep`. Every
        // one of those callers has to be separated from it, not merely the
        // first path found.
        assert_ne!(groups["shared"], groups["deep"], "{groups:?}");
        assert_ne!(groups["shared"], groups["left"], "{groups:?}");
        // And what the rule buys: `left` sits one call above `deep` and neither
        // reaches the other, so they share. Grouping by call depth could not
        // say that.
        assert_eq!(groups["left"], groups["deep"], "{groups:?}");
    }

    /// The read-back hazard, stated as a layout property.
    ///
    /// A multi-output call leaves its results in the CALLEE's region and the
    /// caller copies them out after the call returns. Those copies read the
    /// callee's region and write the caller's, so the two must not be one piece
    /// of storage — the copy's own writes would clobber the source it is still
    /// reading. This is the likeliest way to turn the overlay into wrong code,
    /// so it gets its own name.
    #[test]
    fn a_read_back_never_reads_the_group_it_writes() {
        let block = block_of(
            vec![
                // `outer` calls both, one after the other. `sibling`'s region
                // legitimately shares `inner`'s: its results are read back at a
                // different point in `outer`'s sequence and never while
                // `inner`'s are live.
                function("outer", 8, &["inner", "sibling"]),
                function("inner", 8, &[]),
                function("sibling", 8, &[]),
            ],
            &["outer"],
        );
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let groups = groups_by_owner(&resolved);
        assert_ne!(
            groups["outer"], groups["inner"],
            "a caller reading `inner`'s outputs back into its own region must not \
             share storage with it"
        );
        assert_eq!(
            groups["inner"], groups["sibling"],
            "two callees that are never live together are what the overlay is for"
        );
        assert_ne!(
            groups["dostep"], groups["outer"],
            "the method reading `outer`'s outputs back must not share its storage"
        );
    }

    /// A method's group holds methods and nothing else, even against a function
    /// no method reaches: a public function is callable directly by the same
    /// consumer that calls the methods, in an order this compiler does not see,
    /// so a function region is never argued against a method region.
    #[test]
    fn a_method_never_shares_storage_with_a_function() {
        let block = block_of(
            vec![function("reached", 8, &[]), function("orphan", 8, &[])],
            &["reached"],
        );
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let groups = groups_by_owner(&resolved);
        assert_ne!(groups["dostep"], groups["orphan"], "{groups:?}");
        assert_ne!(groups["dostep"], groups["reached"], "{groups:?}");
        // The gain the reachability rule has over grouping by call depth: an
        // orphan reaches nothing and nothing reaches it, so it rides along with
        // whatever function region is largest instead of founding its own.
        assert_eq!(groups["orphan"], groups["reached"], "{groups:?}");
    }

    /// The accounting a reviewer reads: the achieved total is at least the
    /// heaviest chain, and that chain is the floor ANY sound overlay could reach,
    /// its members being pairwise caller and callee. The chain named is a
    /// real one. On this fixture the two meet, so the placement is optimal.
    #[test]
    fn the_reported_total_is_explained_by_the_reported_chain() {
        let block = block_of(
            vec![
                function("top", 4, &["heavy"]),
                function("heavy", 64, &[]),
                function("light", 2, &[]),
            ],
            &["top", "light"],
        );
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        assert_eq!(resolved.chain, vec!["dostep", "top", "heavy"]);
        assert!(
            resolved.bytes >= resolved.chain_bytes,
            "achieved {:?} must not be below the floor {:?}",
            resolved.bytes,
            resolved.chain_bytes
        );
        // Groups: {heavy, light} + {top} + {dostep}. `light` rides along with
        // the heaviest region it may share, so it costs nothing at all and the
        // total lands exactly on the chain floor.
        assert_eq!(resolved.groups.len(), 3);
        assert_eq!(resolved.bytes, resolved.chain_bytes);
        assert!(resolved.summary.contains("dostep -> top -> heavy"));
    }

    /// A cycle fails closed rather than producing a layout. The projection
    /// already rejects recursion; the layout walk re-detects it, because a
    /// region sharing a group with itself would let an inner activation
    /// overwrite the outer one's intermediates.
    #[test]
    fn a_call_cycle_fails_closed() {
        let block = block_of(
            vec![function("a", 8, &["b"]), function("b", 8, &["a"])],
            &["a"],
        );
        let sources = rumoca_core::SourceMap::new();
        let error = super::block(&block, &sources).expect_err("a cycle must not project");
        assert!(error.contains("reachable from itself"), "{error}");

        // And the overlay prover on its own, without the projection's guard.
        let functions: HashMap<&str, &ast::UserFunction> = block
            .protected_functions
            .iter()
            .map(|function| (function.name.lexeme(), function))
            .collect();
        let error = CallGraph::prove(call_edges(&block, &functions))
            .expect_err("the overlay must fail closed too");
        assert!(error.contains("acyclic"), "{error}");
    }

    /// `function <name>(input u[extent]) => (output y[extent])` whose body is a
    /// straight-line chain:
    ///
    /// ```text
    /// spanning := u; first := u; y := first;
    /// second := u; y := second; y := spanning;
    /// ```
    ///
    /// `first` dies before `second` is born, so they are what the arena is
    /// for. `spanning` is written first and read last, so it is live across
    /// both and must keep bytes of its own: the counterexample in the same
    /// fixture.
    fn sequential_function(name: &str, extent: i64) -> ast::UserFunction {
        let from_input = |slot: &str| {
            ast::Spanned::dummy(ast::Statement::Assignment {
                target: ast::Reference::local(ident(slot)),
                value: ast::Expression::Ref(ast::Reference::local(ident("u"))),
            })
        };
        let into_output = |slot: &str| {
            ast::Spanned::dummy(ast::Statement::Assignment {
                target: ast::Reference::local(ident("y")),
                value: ast::Expression::Ref(ast::Reference::local(ident(slot))),
            })
        };
        ast::UserFunction {
            kind: ast::FunctionKind::Stateless,
            name: ident(name),
            signals: Vec::new(),
            parameters: vec![
                parameter(ast::Direction::Input, "u", extent),
                parameter(ast::Direction::Output, "y", extent),
            ],
            locals: vec![
                array_local("spanning", extent),
                array_local("first", extent),
                array_local("second", extent),
            ],
            statements: vec![
                from_input("spanning"),
                from_input("first"),
                into_output("first"),
                from_input("second"),
                into_output("second"),
                into_output("spanning"),
            ],
            span: rumoca_core::Span::DUMMY,
        }
    }

    /// The same chain shut inside one `for` body, where the back edge makes
    /// statement order say nothing: `early` written late in one iteration is
    /// read by `late` early in the next, so neither may take the other's
    /// bytes.
    fn looping_function(name: &str, extent: i64) -> ast::UserFunction {
        let element = |slot: &str, source: &str| {
            let mut target = ast::RefPart::plain(ident(slot));
            target.subscripts = vec![ast::Expression::Ref(ast::Reference::local(ident("i")))];
            let mut from = ast::RefPart::plain(ident(source));
            from.subscripts = vec![ast::Expression::Ref(ast::Reference::local(ident("i")))];
            ast::Spanned::dummy(ast::Statement::Assignment {
                target: ast::Reference::Local(target),
                value: ast::Expression::Ref(ast::Reference::Local(from)),
            })
        };
        ast::UserFunction {
            kind: ast::FunctionKind::Stateless,
            name: ident(name),
            signals: Vec::new(),
            parameters: vec![
                parameter(ast::Direction::Input, "u", extent),
                parameter(ast::Direction::Output, "y", extent),
            ],
            locals: vec![array_local("early", extent), array_local("late", extent)],
            statements: vec![ast::Spanned::dummy(ast::Statement::for_loop(
                ast::ForLoop::new(
                    Some(ident("i")),
                    ast::Expression::Integer(1),
                    None,
                    ast::Expression::Integer(extent),
                    vec![
                        element("early", "u"),
                        element("late", "early"),
                        element("y", "late"),
                    ],
                ),
            ))],
            span: rumoca_core::Span::DUMMY,
        }
    }

    /// `function <name>(input u[extent]) => (output y[extent])` whose body is a
    /// two-armed conditional: `if c then a := u; y := a; else b := u; y := b;`.
    ///
    /// `a` and `b` are each named in exactly one arm; `y` is the OUTPUT
    /// parameter, named in both.
    fn branching_function(name: &str, extent: i64) -> ast::UserFunction {
        let arm = |slot: &str| {
            vec![
                ast::Spanned::dummy(ast::Statement::Assignment {
                    target: ast::Reference::local(ident(slot)),
                    value: ast::Expression::Ref(ast::Reference::local(ident("u"))),
                }),
                ast::Spanned::dummy(ast::Statement::Assignment {
                    target: ast::Reference::local(ident("y")),
                    value: ast::Expression::Ref(ast::Reference::local(ident(slot))),
                }),
            ]
        };
        ast::UserFunction {
            kind: ast::FunctionKind::Stateless,
            name: ident(name),
            signals: Vec::new(),
            parameters: vec![
                parameter(ast::Direction::Input, "u", extent),
                parameter(ast::Direction::Output, "y", extent),
            ],
            locals: vec![array_local("a", extent), array_local("b", extent)],
            statements: vec![ast::Spanned::dummy(ast::Statement::If(ast::IfStatement {
                branches: vec![ast::IfBranch {
                    condition: ast::Condition::Expression(ast::Expression::Bool(true)),
                    body: arm("a"),
                    span: rumoca_core::Span::DUMMY,
                }],
                else_body: Some(arm("b")),
            }))],
            span: rumoca_core::Span::DUMMY,
        }
    }

    /// The region one owner declared, by source name.
    fn region_of<'view, 'a>(
        layout: &'view ScratchLayoutView<'a>,
        owner: &str,
    ) -> &'view ScratchRegionView<'a> {
        layout
            .groups
            .iter()
            .flat_map(|group| &group.regions)
            .find(|region| region.owner() == owner)
            .unwrap_or_else(|| panic!("`{owner}` declares no region"))
    }

    /// The arm overlay a named slot landed in, `None` when it owns its storage.
    fn overlay_of<'a>(region: &ScratchRegionView<'a>, slot: &str) -> Option<usize> {
        region
            .members
            .iter()
            .find(|member| member.slots.iter().any(|decl| decl.name.lexeme() == slot))
            .and_then(|member| member.overlay)
    }

    /// The arena slot placement of a named slot: `(float offset, floats)`, or
    /// `None` when the slot is not arena-placed.
    fn arena_of<'a>(region: &ScratchRegionView<'a>, slot: &str) -> Option<(usize, usize)> {
        region.arena.as_ref().and_then(|arena| {
            arena
                .slots
                .iter()
                .find(|placed| placed.slot.lexeme() == slot)
                .map(|placed| (placed.offset, placed.floats))
        })
    }

    /// What sub-region sharing buys, as a layout property: two Real array
    /// temporaries each confined to one arm of one conditional become one
    /// piece of arena storage, and the region is sized for one of them rather
    /// than both. The exclusive-arms proof now lands them in the value arena
    /// at one offset; the arm union remains the vehicle for slots the arena
    /// does not take (see the Integer variant below).
    #[test]
    fn two_arm_local_slots_share_one_piece_of_region_storage() {
        let block = block_of(vec![branching_function("branching", 8)], &["branching"]);
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let region = region_of(&resolved, "branching");

        let (a_offset, a_floats) = arena_of(region, "a").expect("`a` is confined to one arm");
        let (b_offset, _) = arena_of(region, "b").expect("so is `b`, the other arm");
        assert_eq!(a_offset, b_offset, "the two arms share one offset");
        assert_eq!(a_floats, 8);
        // 8 Reals of 4 bytes each: `a` and `b` cost 32 together, not 64, and
        // `y` still costs its own 32 as a declared member the caller reads
        // back.
        assert_eq!(region.bytes, Some(64));
        assert_eq!(resolved.arena_regions, 1);
        assert_eq!(resolved.arena_slots, 2);
        assert_eq!(resolved.arena_slot_bytes, 64);
        assert_eq!(resolved.arena_bytes, 32);
    }

    /// The same two-arm shape over Integer slots: the arena is a float array
    /// and refuses them, so the arm overlay still places them in one union.
    /// This is the path that keeps the arm prover load-bearing.
    #[test]
    fn two_integer_arm_local_slots_still_share_an_arm_union() {
        let mut function = branching_function("branching", 8);
        for local in &mut function.locals {
            local.ty = ast::TypeRef::Primitive(ast::ScalarType::Integer);
        }
        for parameter in &mut function.parameters {
            parameter.decl.ty = ast::TypeRef::Primitive(ast::ScalarType::Integer);
        }
        let block = block_of(vec![function], &["branching"]);
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let region = region_of(&resolved, "branching");

        let arm = overlay_of(region, "a").expect("`a` is confined to one arm");
        assert_eq!(
            overlay_of(region, "b"),
            Some(arm),
            "so is `b`, the other arm"
        );
        assert!(
            region.arena.is_none(),
            "an Integer slot never enters the float arena"
        );
        assert_eq!(region.bytes, Some(64));
        assert_eq!(resolved.arm_overlays, 1);
        assert_eq!(resolved.arm_overlay_slots, 2);
    }

    /// NEGATIVE CONTROL. A function's OUTPUT is never given an arm overlay,
    /// however arm-local its assignments look.
    ///
    /// This is what keeps the read-back sound. The caller copies a callee's
    /// results out with `<callee region>.<output>`, and it knows only the name
    /// it called and the output's name, and it cannot know which arm inside the
    /// callee happened to write it, and there is no arm it could name that
    /// would be right on every path. The refusal is structural rather than a
    /// special case here: an output is not a body-placed local, so the
    /// placement analysis gives it no home, and no home is a refusal.
    #[test]
    fn a_function_output_is_never_given_an_arm_overlay() {
        let block = block_of(vec![branching_function("branching", 8)], &["branching"]);
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let region = region_of(&resolved, "branching");

        assert_eq!(
            overlay_of(region, "y"),
            None,
            "a callee output must stay reachable as `<region>.<output>`"
        );
        assert!(
            region.members.iter().any(|member| member.overlay.is_none()
                && member.slots.iter().any(|d| d.name.lexeme() == "y")),
            "`y` must be a member of its own"
        );
    }

    /// NEGATIVE CONTROL. A slot named in more than one arm keeps its own
    /// storage: the arms that would share it are exactly the arms that need it
    /// intact.
    #[test]
    fn a_slot_named_in_two_arms_keeps_its_own_storage() {
        let mut function = branching_function("branching", 8);
        // Make `a` the slot both arms name: the else arm now reads it too, so
        // its home rises to the whole body.
        function.statements = vec![ast::Spanned::dummy(ast::Statement::If(ast::IfStatement {
            branches: vec![ast::IfBranch {
                condition: ast::Condition::Expression(ast::Expression::Bool(true)),
                body: vec![ast::Spanned::dummy(ast::Statement::Assignment {
                    target: ast::Reference::local(ident("a")),
                    value: ast::Expression::Ref(ast::Reference::local(ident("u"))),
                })],
                span: rumoca_core::Span::DUMMY,
            }],
            else_body: Some(vec![ast::Spanned::dummy(ast::Statement::Assignment {
                target: ast::Reference::local(ident("b")),
                value: ast::Expression::Ref(ast::Reference::local(ident("a"))),
            })]),
        }))];
        let block = block_of(vec![function], &["branching"]);
        let sources = rumoca_core::SourceMap::new();
        let resolved = layout(&block, &sources);
        let region = region_of(&resolved, "branching");

        assert_eq!(overlay_of(region, "a"), None, "`a` spans both arms");
        assert_eq!(
            overlay_of(region, "b"),
            None,
            "so nothing may share with it"
        );
        assert_eq!(
            resolved.arm_overlays, 0,
            "an overlay of one member is not an overlay"
        );
    }

    /// A slot whose extent is not a literal reports as unsizable rather than
    /// as zero: the number a reviewer reads must never be a guess.
    #[test]
    fn an_unsizable_extent_reports_nothing_rather_than_zero() {
        let mut derived = array_local("t", 4);
        derived.dimensions = vec![ast::Dimension::Derived];
        assert_eq!(slot_bytes(&derived), None);
        assert_eq!(slot_bytes(&array_local("t", 4)), Some(16));
        assert_eq!(total_bytes([slot_bytes(&derived), Some(16)]), None);
        assert_eq!(widest_bytes([slot_bytes(&derived), Some(16)]), None);
    }
}

#[cfg(test)]
mod scale_operand_tests {
    use super::*;

    fn scalar_view<'a>(
        scalar: ast::ScalarType,
        node: TypedExpressionNodeView<'a>,
    ) -> TypedExpressionView<'a> {
        TypedExpressionView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(scalar),
            node,
        }
    }

    fn reference<'a>(
        name: &'a ast::Name,
        subscripts: Vec<TypedExpressionView<'a>>,
    ) -> TypedReferenceView<'a> {
        TypedReferenceView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(ast::ScalarType::Real),
            context_resident: false,
            context_arena: false,
            context_overlay: None,
            declared_extents: None,
            node: TypedReferenceNodeView::Local(TypedRefPartView { name, subscripts }),
        }
    }

    fn real_ref<'a>(name: &'a ast::Name) -> TypedExpressionView<'a> {
        scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Ref(reference(name, Vec::new())),
        )
    }

    fn binary<'a>(
        op: ast::BinaryOp,
        lhs: TypedExpressionView<'a>,
        rhs: TypedExpressionView<'a>,
    ) -> TypedExpressionView<'a> {
        scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Binary {
                op,
                precedence_class: op.precedence_class(),
                associativity: op.precedence_class().associativity(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                square_form: SquareForm::Multiply,
            },
        )
    }

    /// A scale is a scalar Real by definition: an array-valued operand is not
    /// a coefficient the kernel can hoist, whatever its node shape.
    #[test]
    fn array_valued_operand_is_not_a_scale() {
        let a = ast::Name::ident("a");
        let mut value = real_ref(&a);
        value.rank = 1;
        value.extents = Some(vec![3]);
        assert!(scale_operand(&value).is_none());
    }

    #[test]
    fn integer_operand_is_not_a_scale() {
        let value = scalar_view(
            ast::ScalarType::Integer,
            TypedExpressionNodeView::Integer(2),
        );
        assert!(scale_operand(&value).is_none());
    }

    /// The whitelist walk descends products, quotients, parentheses and
    /// negation, and reports the root object of every reference it passes so
    /// the caller can prove none of them is the run the kernel writes.
    #[test]
    fn product_reports_every_reference_root() {
        let (a, b, c) = (
            ast::Name::ident("a"),
            ast::Name::ident("b"),
            ast::Name::ident("c"),
        );
        let negated = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Neg(reference(&c, Vec::new())),
        );
        let parenthesized = scalar_view(
            ast::ScalarType::Real,
            TypedExpressionNodeView::Paren(Box::new(binary(
                ast::BinaryOp::Div,
                real_ref(&b),
                negated,
            ))),
        );
        let value = binary(ast::BinaryOp::Mul, real_ref(&a), parenthesized);

        let (reported, roots) = scale_operand(&value).expect("product of references is a scale");
        assert!(std::ptr::eq(reported, &value));
        assert_eq!(roots, vec!["a", "b", "c"]);
    }

    /// Addition is off the whitelist: hoisting it would change how many times
    /// a rounding-sensitive sum is evaluated, so the walk refuses the node
    /// rather than reporting partial roots.
    #[test]
    fn addition_is_refused() {
        let (a, b) = (ast::Name::ident("a"), ast::Name::ident("b"));
        let value = binary(ast::BinaryOp::Add, real_ref(&a), real_ref(&b));
        assert!(scale_operand(&value).is_none());

        let mut roots = Vec::new();
        assert!(!scale_operand_roots(&value, &mut roots));
    }

    #[test]
    fn local_reference_mentions_its_own_name() {
        let a = ast::Name::ident("a");
        assert!(mentions_name_in_reference(&reference(&a, Vec::new()), "a"));
        assert!(!mentions_name_in_reference(&reference(&a, Vec::new()), "b"));
    }

    #[test]
    fn subscript_mentions_are_found() {
        let (a, k) = (ast::Name::ident("a"), ast::Name::ident("k"));
        let subscripted = reference(&a, vec![real_ref(&k)]);
        assert!(mentions_name_in_reference(&subscripted, "k"));
    }

    /// A state path's component names are not local names, so a component that
    /// happens to spell the searched name is not a mention; its subscripts
    /// still are.
    #[test]
    fn state_component_name_is_not_a_local_mention() {
        let (state, k) = (ast::Name::ident("k"), ast::Name::ident("k"));
        let path = TypedReferenceView {
            rank: 0,
            extents: Some(Vec::new()),
            scalar: Some(ast::ScalarType::Real),
            context_resident: false,
            context_arena: false,
            context_overlay: None,
            declared_extents: None,
            node: TypedReferenceNodeView::State(vec![TypedRefPartView {
                name: &state,
                subscripts: Vec::new(),
            }]),
        };
        assert!(!mentions_name_in_reference(&path, "k"));

        let subscripted = TypedReferenceView {
            node: TypedReferenceNodeView::State(vec![TypedRefPartView {
                name: &state,
                subscripts: vec![real_ref(&k)],
            }]),
            ..path.clone()
        };
        assert!(mentions_name_in_reference(&subscripted, "k"));
    }
}
