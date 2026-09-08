//! Exact compiler-negative capability frontier for catalogued roots.
//!
//! These assertions intentionally do not generalize one type parameter into a
//! claim about every possible trait instantiation. The source inventory owns
//! that broader direct-syntax drift boundary.

macro_rules! assert_not_bound {
    ($ty:ty: $($bound:tt)+) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }
            impl<T: ?Sized> AmbiguousIfImplemented<()> for T {}
            struct ImplementsBound;
            impl<T: ?Sized + $($bound)+> AmbiguousIfImplemented<ImplementsBound> for T {}
            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

assert_not_bound!(rumoca_ir_ast::instance::InstanceOverlay: std::ops::DerefMut);
assert_not_bound!(rumoca_ir_flat::Model: std::ops::DerefMut);
assert_not_bound!(rumoca_ir_dae::Dae: std::ops::DerefMut);
assert_not_bound!(rumoca_ir_solve::SolveProblem: std::ops::DerefMut);
assert_not_bound!(rumoca_ir_solve::SolveModel: std::ops::DerefMut);

assert_not_bound!(rumoca_ir_ast::instance::InstanceOverlay: AsMut<rumoca_ir_ast::instance::InstanceOverlay>);
assert_not_bound!(rumoca_ir_ast::instance::InstancedTree: AsMut<rumoca_ir_ast::instance::InstancedTree>);
assert_not_bound!(rumoca_ir_flat::Model: AsMut<rumoca_ir_flat::Model>);
assert_not_bound!(rumoca_ir_dae::Dae: AsMut<rumoca_ir_dae::Dae>);
assert_not_bound!(rumoca_ir_solve::SolveProblem: AsMut<rumoca_ir_solve::SolveProblem>);
assert_not_bound!(rumoca_ir_solve::SolveModel: AsMut<rumoca_ir_solve::SolveModel>);

assert_not_bound!(rumoca_ir_ast::instance::InstanceOverlay: std::ops::IndexMut<usize>);
assert_not_bound!(rumoca_ir_ast::instance::InstancedTree: std::ops::IndexMut<usize>);
assert_not_bound!(rumoca_ir_flat::Model: std::ops::IndexMut<usize>);
assert_not_bound!(rumoca_ir_dae::Dae: std::ops::IndexMut<usize>);
assert_not_bound!(rumoca_ir_solve::SolveProblem: std::ops::IndexMut<usize>);
assert_not_bound!(rumoca_ir_solve::SolveModel: std::ops::IndexMut<usize>);

assert_not_bound!(rumoca_ir_ast::instance::InstanceOverlay: serde::de::DeserializeOwned);
assert_not_bound!(rumoca_ir_ast::instance::InstancedTree: serde::de::DeserializeOwned);
assert_not_bound!(rumoca_ir_solve::SolveModel: serde::de::DeserializeOwned);

assert_not_bound!(rumoca_ir_ast::instance::InstancedTree: Default);
assert_not_bound!(rumoca_ir_dae::Dae: Default);
assert_not_bound!(rumoca_ir_solve::SolveProblem: Default);
assert_not_bound!(rumoca_ir_solve::SolveModel: Default);

// The Typecheck-minted proof artifact lives in its phase crate. Its unique
// phase capability moves exactly once, so cloning, defaulting, wire decode,
// and mutable projection are compiler-refused rather than reviewed.
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: Clone);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: Default);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: serde::Serialize);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: serde::de::DeserializeOwned);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: std::ops::Deref);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: std::ops::DerefMut);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: AsMut<rumoca_ir_ast::instance::InstanceOverlay>);
assert_not_bound!(rumoca_phase_typecheck::TypedInstancedTree: From<rumoca_phase_typecheck::TypedOverlayProjection>);
assert_not_bound!(rumoca_phase_typecheck::TypedOverlayProjection: Into<rumoca_phase_typecheck::TypedInstancedTree>);

// The cached read-only projection is Clone by design but carries no proof:
// it cannot be deserialized, defaulted, or mutably projected either.
assert_not_bound!(rumoca_phase_typecheck::TypedOverlayProjection: Default);
assert_not_bound!(rumoca_phase_typecheck::TypedOverlayProjection: serde::Serialize);
assert_not_bound!(rumoca_phase_typecheck::TypedOverlayProjection: serde::de::DeserializeOwned);
assert_not_bound!(rumoca_phase_typecheck::TypedOverlayProjection: std::ops::DerefMut);
assert_not_bound!(rumoca_phase_typecheck::TypedOverlayProjection: AsMut<rumoca_ir_ast::instance::InstanceOverlay>);

// Resolve's exact-root projection is Clone only as a read-only correlated
// shared view. It has no independent construction or extraction authority.
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: Default);
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: serde::Serialize);
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: serde::de::DeserializeOwned);
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: std::ops::Deref);
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: std::ops::DerefMut);
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: AsMut<rumoca_ir_ast::ClassTree>);
assert_not_bound!(rumoca_phase_resolve::ResolvedTreeProjection: From<rumoca_ir_ast::ClassTree>);
