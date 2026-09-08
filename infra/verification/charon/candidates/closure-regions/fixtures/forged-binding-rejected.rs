#![feature(rustc_private)]

extern crate rustc_middle;

use closure_region_facts::binding::BoundOccurrences;
use rustc_middle::ty::{Binder, Region, Ty};

// Independently supplied binders do not establish shared source provenance.
pub fn forge<'tcx>(
    value: Binder<'tcx, Ty<'tcx>>,
    free_regions: Binder<'tcx, Vec<Region<'tcx>>>,
) -> BoundOccurrences<'tcx, Ty<'tcx>> {
    BoundOccurrences {
        value,
        free_regions,
    }
}
