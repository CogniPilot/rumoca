use rustc_middle::ty::{self, TyCtxt, TypeSuperVisitable, TypeVisitable, TypeVisitor};

use crate::{FactError, RegionPart, RegionSlot};

/// Unlike `for_each_free_region`, this visits erased occurrences too: rustc's
/// HAS_FREE_REGIONS fast path deliberately skips types containing only erasure.
/// A structural position must not disappear just because its value was erased.
struct RegionOccurrences<'tcx> {
    outer: ty::DebruijnIndex,
    regions: Vec<ty::Region<'tcx>>,
}

impl<'tcx> TypeVisitor<TyCtxt<'tcx>> for RegionOccurrences<'tcx> {
    type Result = ();

    fn visit_binder<T: TypeVisitable<TyCtxt<'tcx>>>(&mut self, binder: &ty::Binder<'tcx, T>) {
        self.outer.shift_in(1);
        binder.super_visit_with(self);
        self.outer.shift_out(1);
    }

    fn visit_region(&mut self, region: ty::Region<'tcx>) {
        match region.kind() {
            ty::ReBound(ty::BoundVarIndexKind::Bound(depth), _) if depth < self.outer => {}
            _ => self.regions.push(region),
        }
    }
}

fn occurrences<'tcx>(value: &impl TypeVisitable<TyCtxt<'tcx>>) -> Vec<ty::Region<'tcx>> {
    let mut visitor = RegionOccurrences {
        outer: ty::INNERMOST,
        regions: Vec::new(),
    };
    value.visit_with(&mut visitor);
    visitor.regions
}

pub(crate) fn match_positions<'tcx>(
    part: RegionPart,
    raw: &impl TypeVisitable<TyCtxt<'tcx>>,
    inferred: &impl TypeVisitable<TyCtxt<'tcx>>,
) -> Result<Vec<(RegionSlot, ty::RegionVid)>, FactError> {
    let raw_regions = occurrences(raw);
    let inferred_regions = occurrences(inferred);
    if raw_regions.len() != inferred_regions.len() {
        return Err(FactError::OccurrenceMismatch(part));
    }
    inferred_regions
        .into_iter()
        .enumerate()
        .map(|(occurrence, region)| {
            let ty::ReVar(vid) = region.kind() else {
                return Err(FactError::UnresolvedRegion(part));
            };
            Ok((RegionSlot { part, occurrence }, vid))
        })
        .collect()
}
