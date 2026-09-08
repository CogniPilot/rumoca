use closure_region_facts::binding::{bind_erased, under_binder};
use closure_region_facts::free_regions;
use rustc_middle::ty::{self, TyCtxt};

fn bound(tcx: TyCtxt<'_>, depth: u32, index: usize) -> ty::Region<'_> {
    ty::Region::new_bound(
        tcx,
        ty::DebruijnIndex::from(depth),
        ty::BoundRegion {
            var: ty::BoundVar::from_usize(index),
            kind: ty::BoundRegionKind::Anon,
        },
    )
}

pub fn check_binder_scope(tcx: TyCtxt<'_>) {
    let vars =
        tcx.mk_bound_variable_kinds(&[ty::BoundVariableKind::Region(ty::BoundRegionKind::Anon)]);
    let erased = tcx.lifetimes.re_erased;
    let static_region = tcx.lifetimes.re_static;
    let nested = ty::Binder::bind_with_vars(
        vec![bound(tcx, 0, 0), bound(tcx, 1, 0), bound(tcx, 2, 0), erased],
        vars,
    );
    let input = ty::Binder::bind_with_vars(
        (vec![erased, static_region, bound(tcx, 0, 0)], nested),
        vars,
    );
    assert_eq!(
        free_regions(&input),
        vec![erased, static_region, bound(tcx, 2, 0), erased]
    );
    let (value, free_regions) = bind_erased(tcx, input).into_parts();
    assert_eq!(value.bound_vars().len(), 3);
    assert_eq!(value.bound_vars(), free_regions.bound_vars());
    assert_eq!(
        free_regions.skip_binder(),
        vec![
            bound(tcx, 0, 1),
            static_region,
            bound(tcx, 1, 0),
            bound(tcx, 0, 2)
        ]
    );
    let (root, nested) = value.skip_binder();
    assert_eq!(
        root,
        vec![bound(tcx, 0, 1), static_region, bound(tcx, 0, 0)]
    );
    assert_eq!(nested.bound_vars(), vars);
    assert_eq!(
        nested.skip_binder(),
        vec![
            bound(tcx, 0, 0),
            bound(tcx, 1, 0),
            bound(tcx, 2, 0),
            bound(tcx, 1, 2)
        ]
    );

    let input = (
        bound(tcx, 0, 0),
        ty::Binder::bind_with_vars(vec![bound(tcx, 0, 0), bound(tcx, 1, 0)], vars),
    );
    let (value, free_regions) = bind_erased(tcx, under_binder(tcx, input)).into_parts();
    assert!(value.bound_vars().is_empty());
    assert_eq!(free_regions.skip_binder(), vec![bound(tcx, 1, 0); 2]);
    let (outside, nested) = value.skip_binder();
    assert_eq!(outside, bound(tcx, 1, 0));
    assert_eq!(
        nested.skip_binder(),
        vec![bound(tcx, 0, 0), bound(tcx, 2, 0)]
    );
    println!("CHECKED_FRESHENING_BINDER_SCOPE");
}

pub fn check_source_occurrences(tcx: TyCtxt<'_>, owner: rustc_hir::def_id::LocalDefId) {
    let raw = tcx.type_of(owner).instantiate_identity().skip_norm_wip();
    let ty::Closure(_, args) = raw.kind() else {
        panic!("caller must supply a closure");
    };
    let closure = args.as_closure();
    let signature = tcx.signature_unclosure(closure.sig(), rustc_hir::Safety::Safe);
    let captures = under_binder(tcx, closure.upvar_tys());
    let signature_positions = free_regions(&signature);
    let capture_positions = free_regions(&captures);
    let (fresh_signature, signature_regions) = bind_erased(tcx, signature).into_parts();
    let (fresh_captures, capture_regions) = bind_erased(tcx, captures).into_parts();
    let signature_regions = signature_regions.skip_binder();
    let capture_regions = capture_regions.skip_binder();
    assert_eq!(signature_positions.len(), signature_regions.len());
    assert_eq!(capture_positions.len(), capture_regions.len());
    // Erasing all regions independently must recover the identical type shape.
    // The separate synthetic control checks preservation of genuinely bound ones.
    assert_eq!(
        tcx.erase_and_anonymize_regions(tcx.instantiate_bound_regions_with_erased(signature)),
        tcx.erase_and_anonymize_regions(tcx.instantiate_bound_regions_with_erased(fresh_signature))
    );
    assert_eq!(
        tcx.erase_and_anonymize_regions(tcx.instantiate_bound_regions_with_erased(captures)),
        tcx.erase_and_anonymize_regions(tcx.instantiate_bound_regions_with_erased(fresh_captures))
    );
    for (before, after) in signature_positions
        .iter()
        .chain(&capture_positions)
        .zip(signature_regions.iter().chain(&capture_regions))
    {
        if !before.is_erased() {
            assert_eq!(before, after, "free source position must retain identity");
        }
        assert!(!after.is_erased(), "every erased occurrence must be issued");
    }
}
