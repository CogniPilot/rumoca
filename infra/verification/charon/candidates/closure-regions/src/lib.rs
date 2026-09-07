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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OutlivesRelation {
    longer: RegionSlot,
    shorter: RegionSlot,
}

impl OutlivesRelation {
    pub fn longer(self) -> RegionSlot {
        self.longer
    }

    pub fn shorter(self) -> RegionSlot {
        self.shorter
    }
}

/// Owned observations of one borrow-checking body's inferred regions.
/// These are not universal signature predicates or an authenticated cross-crate
/// artifact. Promoting them to either requires a separate, justified mapping.
#[derive(Debug)]
pub struct ClosureRegionFacts {
    owner: DefPathHash,
    slots: Vec<RegionSlot>,
    outlives: Vec<OutlivesRelation>,
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
        let outlives = positions
            .iter()
            .flat_map(|&(longer, longer_vid)| {
                positions.iter().filter_map(move |&(shorter, shorter_vid)| {
                    facts
                        .region_inference_context
                        .eval_outlives(longer_vid, shorter_vid)
                        .then_some(OutlivesRelation { longer, shorter })
                })
            })
            .collect();
        Ok(Self {
            owner: tcx.def_path_hash(owner.to_def_id()),
            slots: positions.into_iter().map(|(slot, _)| slot).collect(),
            outlives,
        })
    }

    pub fn owner(&self) -> DefPathHash {
        self.owner
    }

    pub fn slots(&self) -> &[RegionSlot] {
        &self.slots
    }

    pub fn outlives(&self) -> &[OutlivesRelation] {
        &self.outlives
    }
}
