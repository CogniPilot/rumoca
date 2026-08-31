//! Compiler-level negative trait assertions for the callable plan root.
//!
//! This deliberately does not parse source. A renamed import, a type alias, a
//! blanket implementation, a derive, or a macro-generated implementation all
//! change the compiler's trait relation and therefore fail the same assertion.
//! The plan is an affine proof aggregate: cloning it would duplicate a proof,
//! defaulting it would fabricate one, and serializing or deserializing it
//! would give a consumer a route around its construction authority.

macro_rules! assert_not_implemented {
    ($ty:ty, $bound:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }

            impl<T> AmbiguousIfImplemented<()> for T {}

            struct Implements;
            impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}

            // Exactly one implementation is selectable only while the bound is
            // unimplemented. Any direct, aliased, derived, blanket, or
            // macro-generated implementation makes inference ambiguous and this
            // test crate cannot compile.
            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

assert_not_implemented!(rumoca_plan_callable::CallablePlan, ::core::clone::Clone);
assert_not_implemented!(rumoca_plan_callable::CallablePlan, ::core::default::Default);
assert_not_implemented!(rumoca_plan_callable::CallablePlan, ::serde::Serialize);
assert_not_implemented!(
    rumoca_plan_callable::CallablePlan,
    ::serde::de::DeserializeOwned
);

#[test]
fn the_compiler_proves_the_plan_root_is_affine_and_has_no_wire_form() {
    // The assertions above are compile-time obligations. This named test keeps
    // the gate discoverable in filtered architecture-test output.
}
