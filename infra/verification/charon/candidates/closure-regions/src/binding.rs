//! Retain source occurrence identity while replacing erased regions.

use rustc_middle::ty::{self, TyCtxt, TypeFoldable, TypeFolder, TypeSuperFoldable};

/// The free positions of the input, expressed in the same binder as `value`.
/// Genuine bound positions remain bound and never enter this occurrence list.
#[must_use]
pub struct BoundOccurrences<'tcx, T> {
    value: ty::Binder<'tcx, T>,
    free_regions: ty::Binder<'tcx, Vec<ty::Region<'tcx>>>,
}

impl<'tcx, T> BoundOccurrences<'tcx, T> {
    /// Consume one issued pair; callers cannot construct independent binders
    /// and present them as source-correlated occurrences.
    pub fn into_parts(self) -> (ty::Binder<'tcx, T>, ty::Binder<'tcx, Vec<ty::Region<'tcx>>>) {
        (self.value, self.free_regions)
    }
}

struct Freshener<'tcx> {
    tcx: TyCtxt<'tcx>,
    depth: u32,
    bound_vars: Vec<ty::BoundVariableKind<'tcx>>,
    free_regions: Vec<ty::Region<'tcx>>,
}

impl<'tcx> TypeFolder<TyCtxt<'tcx>> for Freshener<'tcx> {
    fn cx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }

    fn fold_ty(&mut self, ty: ty::Ty<'tcx>) -> ty::Ty<'tcx> {
        ty.super_fold_with(self)
    }

    fn fold_binder<T: TypeFoldable<TyCtxt<'tcx>>>(
        &mut self,
        binder: ty::Binder<'tcx, T>,
    ) -> ty::Binder<'tcx, T> {
        self.depth += 1;
        let binder = binder.super_fold_with(self);
        self.depth -= 1;
        binder
    }

    fn fold_region(&mut self, region: ty::Region<'tcx>) -> ty::Region<'tcx> {
        // The input already has its root binder. Its bound regions and regions
        // introduced by nested binders are not free source occurrences.
        if let ty::ReBound(ty::BoundVarIndexKind::Bound(depth), _) = region.kind()
            && depth.as_u32() <= self.depth
        {
            return region;
        }
        let region = if region.is_erased() {
            let bound = ty::BoundRegion {
                var: ty::BoundVar::from_usize(self.bound_vars.len()),
                kind: ty::BoundRegionKind::Anon,
            };
            self.bound_vars
                .push(ty::BoundVariableKind::Region(bound.kind));
            ty::Region::new_bound(self.tcx, ty::DebruijnIndex::from(self.depth), bound)
        } else {
            region
        };
        let at_root = match region.kind() {
            ty::ReBound(ty::BoundVarIndexKind::Bound(depth), bound) => {
                ty::Region::new_bound(self.tcx, depth.shifted_out(self.depth), bound)
            }
            _ => region,
        };
        self.free_regions.push(at_root);
        region
    }
}

/// Allocate erased occurrences before translating type structure. The returned
/// provenance uses Rust's structural visit order, including phantom arguments.
pub fn bind_erased<'tcx, T: TypeFoldable<TyCtxt<'tcx>>>(
    tcx: TyCtxt<'tcx>,
    input: ty::Binder<'tcx, T>,
) -> BoundOccurrences<'tcx, T> {
    let mut freshener = Freshener {
        tcx,
        depth: 0,
        bound_vars: input.bound_vars().iter().collect(),
        free_regions: Vec::new(),
    };
    let value = input.skip_binder().fold_with(&mut freshener);
    let bound_vars = tcx.mk_bound_variable_kinds(&freshener.bound_vars);
    BoundOccurrences {
        value: ty::Binder::bind_with_vars(value, bound_vars),
        free_regions: ty::Binder::bind_with_vars(freshener.free_regions, bound_vars),
    }
}

/// Introduce a binder without capturing any variable belonging to the caller.
pub fn under_binder<'tcx, T: TypeFoldable<TyCtxt<'tcx>>>(
    tcx: TyCtxt<'tcx>,
    value: T,
) -> ty::Binder<'tcx, T> {
    ty::Binder::bind_with_vars(
        ty::shift_vars(tcx, value, 1),
        tcx.mk_bound_variable_kinds(&[]),
    )
}
