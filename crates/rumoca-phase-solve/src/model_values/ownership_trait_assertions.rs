//! Compiler-enforced affine ownership relations for checked Solve lowering.

use super::{C60CheckedSolveRoot, CheckedSolveRoot, LoweredSolveModel, PreparedSolveContext};
use crate::scalar_constant_derivative_refinement::CheckedDaeSolveScalarConstantDerivativeRefinement;
use crate::variable_catalog_refinement::CheckedDaeSolveVariableCatalogRefinement;

macro_rules! assert_not_implemented {
    ($ty:ty, $bound:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }
            impl<T> AmbiguousIfImplemented<()> for T {}
            struct Implements;
            impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

macro_rules! assert_affine_proof_carrier {
    ($ty:ty) => {
        assert_not_implemented!($ty, ::core::clone::Clone);
        assert_not_implemented!($ty, ::core::marker::Copy);
        assert_not_implemented!($ty, ::core::default::Default);
        assert_not_implemented!($ty, ::serde::Serialize);
        assert_not_implemented!($ty, ::serde::de::DeserializeOwned);
    };
}

assert_affine_proof_carrier!(CheckedDaeSolveVariableCatalogRefinement);
assert_affine_proof_carrier!(CheckedDaeSolveScalarConstantDerivativeRefinement);
assert_affine_proof_carrier!(PreparedSolveContext<'static>);
assert_affine_proof_carrier!(C60CheckedSolveRoot<'static>);
assert_affine_proof_carrier!(CheckedSolveRoot<'static>);
assert_affine_proof_carrier!(LoweredSolveModel<'static>);
