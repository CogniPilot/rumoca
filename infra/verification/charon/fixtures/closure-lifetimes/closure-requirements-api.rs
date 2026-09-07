#![feature(rustc_private)]

extern crate rustc_borrowck;

pub fn requirements<'facts, 'tcx>(
    facts: &'facts rustc_borrowck::consumers::BodyWithBorrowckFacts<'tcx>,
) -> &'facts Option<rustc_borrowck::ClosureRegionRequirements<'tcx>> {
    &facts.closure_requirements
}
