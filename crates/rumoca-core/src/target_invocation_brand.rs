//! Generative origin brand for one complete target invocation.

use std::marker::PhantomData;

/// Invariant origin token minted only by [`with_target_invocation_brand`].
///
/// A target invocation carries this token from its semantic input through
/// every prepared artifact. Invariance prevents a product from one invocation
/// from being substituted into another merely because their ordinary borrow
/// lifetimes happen to overlap.
#[derive(Debug, Clone, Copy)]
pub struct TargetInvocationBrand<'inv>(PhantomData<fn(&'inv mut ()) -> &'inv mut ()>);

impl<'inv> TargetInvocationBrand<'inv> {
    const fn mint(_scope: &'inv mut ()) -> Self {
        Self(PhantomData)
    }
}

/// Run one target invocation under a fresh invariant origin brand.
///
/// Brands minted by nested invocations cannot be joined:
///
/// ```compile_fail
/// use rumoca_core::{TargetInvocationBrand, with_target_invocation_brand};
///
/// fn require_same<'inv>(
///     _left: TargetInvocationBrand<'inv>,
///     _right: TargetInvocationBrand<'inv>,
/// ) {}
///
/// with_target_invocation_brand(|outer| {
///     with_target_invocation_brand(|inner| require_same(outer, inner));
/// });
/// ```
pub fn with_target_invocation_brand<R>(
    use_brand: impl for<'inv> FnOnce(TargetInvocationBrand<'inv>) -> R,
) -> R {
    fn scoped<'inv, R>(
        scope: &'inv mut (),
        use_brand: impl FnOnce(TargetInvocationBrand<'inv>) -> R,
    ) -> R {
        use_brand(TargetInvocationBrand::mint(scope))
    }

    let mut scope = ();
    scoped(&mut scope, use_brand)
}
