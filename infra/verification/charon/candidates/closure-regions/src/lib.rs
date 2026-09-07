#![feature(rustc_private)]

extern crate rustc_borrowck;
extern crate rustc_hir;
extern crate rustc_middle;

mod positions;

use positions::match_positions;
use rustc_borrowck::consumers::BodyWithBorrowckFacts;
use rustc_hir::def_id::{DefPathHash, LocalDefId};
use rustc_middle::mir::MirSource;
use rustc_middle::ty::{self, TyCtxt};

/// A position in the full type, including phantom generic arguments.
/// Bound occurrences are not free positions and remain in their original binder.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegionPart {
    ParentArguments,
    Signature,
    Captures,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegionSlot {
    part: RegionPart,
    occurrence: usize,
}

impl RegionSlot {
    pub fn part(self) -> RegionPart {
        self.part
    }

    pub fn occurrence(self) -> usize {
        self.occurrence
    }
}

/// A path requirement issued from one body's graph, not a declared bound.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RequiredOutlives {
    longer: RegionSlot,
    shorter: RegionSlot,
}

impl RequiredOutlives {
    pub fn longer(self) -> RegionSlot {
        self.longer
    }

    pub fn shorter(self) -> RegionSlot {
        self.shorter
    }
}

/// Required paths in one borrow-checking body's region constraint graph.
/// A path records a constraint, not equality of two computed region values.
/// These are not yet callable predicates or an authenticated cross-crate artifact:
/// both require a justified declaration/caller mapping and binder projection.
#[derive(Debug)]
pub struct ClosureRegionFacts {
    owner: DefPathHash,
    slots: Vec<RegionSlot>,
    required_outlives: Vec<RequiredOutlives>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FactError {
    WrongBodyOwner,
    NotClosure,
    MissingReceiver,
    WrongReceiverOwner,
    StructuralMismatch,
    OccurrenceMismatch(RegionPart),
    UnresolvedRegion(RegionPart),
}

impl ClosureRegionFacts {
    pub fn from_body<'tcx>(
        tcx: TyCtxt<'tcx>,
        owner: LocalDefId,
        facts: &BodyWithBorrowckFacts<'tcx>,
    ) -> Result<Self, FactError> {
        if facts.body.source != MirSource::item(owner.to_def_id()) {
            return Err(FactError::WrongBodyOwner);
        }
        let raw = tcx.type_of(owner).instantiate_identity().skip_norm_wip();
        let ty::Closure(raw_owner, raw_args) = raw.kind() else {
            return Err(FactError::NotClosure);
        };
        let receiver = facts
            .body
            .args_iter()
            .next()
            .ok_or(FactError::MissingReceiver)?;
        let receiver = facts.body.local_decls[receiver].ty;
        let closure = match receiver.kind() {
            ty::Ref(_, inner, _) => *inner,
            _ => receiver,
        };
        let ty::Closure(inferred_owner, inferred_args) = closure.kind() else {
            return Err(FactError::NotClosure);
        };
        if *raw_owner != owner.to_def_id() || raw_owner != inferred_owner {
            return Err(FactError::WrongReceiverOwner);
        }
        if tcx.erase_and_anonymize_regions(raw) != tcx.erase_and_anonymize_regions(closure) {
            return Err(FactError::StructuralMismatch);
        }
        let raw_args = raw_args.as_closure();
        let inferred_args = inferred_args.as_closure();
        let mut positions = Vec::new();
        positions.extend(match_positions(
            RegionPart::ParentArguments,
            &raw_args.parent_args(),
            &inferred_args.parent_args(),
        )?);
        positions.extend(match_positions(
            RegionPart::Signature,
            &raw_args.sig(),
            &inferred_args.sig(),
        )?);
        positions.extend(match_positions(
            RegionPart::Captures,
            &raw_args.tupled_upvars_ty(),
            &inferred_args.tupled_upvars_ty(),
        )?);
        let required_outlives = required_relations(facts, &positions);
        Ok(Self {
            owner: tcx.def_path_hash(owner.to_def_id()),
            slots: positions.into_iter().map(|(slot, _)| slot).collect(),
            required_outlives,
        })
    }

    pub fn owner(&self) -> DefPathHash {
        self.owner
    }

    pub fn slots(&self) -> &[RegionSlot] {
        &self.slots
    }

    pub fn required_outlives(&self) -> &[RequiredOutlives] {
        &self.required_outlives
    }
}

fn required_relations(
    facts: &BodyWithBorrowckFacts<'_>,
    positions: &[(RegionSlot, ty::RegionVid)],
) -> Vec<RequiredOutlives> {
    let graph = facts.region_inference_context.constraint_sccs();
    let mut relations = Vec::new();
    for &(longer, longer_vid) in positions {
        let mut pending = vec![graph.scc(longer_vid)];
        let mut reachable = std::collections::BTreeSet::new();
        while let Some(node) = pending.pop() {
            if reachable.insert(node) {
                pending.extend(graph.successors(node));
            }
        }
        relations.extend(positions.iter().filter_map(|&(shorter, shorter_vid)| {
            reachable
                .contains(&graph.scc(shorter_vid))
                .then_some(RequiredOutlives { longer, shorter })
        }));
    }
    relations
}
