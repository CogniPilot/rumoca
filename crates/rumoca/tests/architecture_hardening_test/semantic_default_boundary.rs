//! Compiler-enforced negative trait assertions for semantic constructors.
//!
//! This deliberately does not parse source. Renamed imports, type aliases,
//! generic construction, and macro-generated implementations all change the
//! compiler's trait relation and therefore fail the same assertion.

macro_rules! assert_not_default {
    ($ty:ty) => {
        const _: fn() = || {
            trait AmbiguousIfDefault<Marker> {
                fn probe() {}
            }

            impl<T: ?Sized> AmbiguousIfDefault<()> for T {}

            struct ImplementsDefault;
            impl<T: ?Sized + ::core::default::Default> AmbiguousIfDefault<ImplementsDefault> for T {}

            // Exactly one impl is selectable only when `$ty: !Default`. If a
            // direct, aliased, derived, or macro-generated Default impl exists,
            // inference is ambiguous and this test crate cannot compile.
            let _ = <$ty as AmbiguousIfDefault<_>>::probe;
        };
    };
}

assert_not_default!(rumoca_core::RealMatrixMultiplySemantics);
assert_not_default!(rumoca_ir_ast::Connection);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodePackageMetadata);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodePackage);
assert_not_default!(rumoca_ir_galec::package::CheckedAlgorithmBlock);
assert_not_default!(rumoca_phase_galec::GalecOptions);
// The callable proof plan is affine: a default value would be a plan whose
// coverage and acyclic receipt were never discharged.
assert_not_default!(rumoca_plan_callable::CallablePlan);
assert_not_default!(rumoca_ir_solve::SolveArithmeticProfile);
assert_not_default!(rumoca_ir_solve::SolveIntegerDomain);
assert_not_default!(rumoca_ir_solve::SolveMatrixMultiplyPlan);
assert_not_default!(rumoca_ir_solve::SolveProblem);
assert_not_default!(rumoca_ir_solve::SolveModel);
assert_not_default!(rumoca_ir_solve::SolvePureCallTable);
assert_not_default!(rumoca_ir_solve::SolveRealFormat);

#[test]
fn compiler_proves_catalogued_semantic_types_are_not_default_constructible() {
    // The assertions above are compile-time obligations. This named test keeps
    // the gate discoverable in filtered architecture-test output.
}

fn public_galec_policy_candidates(relative: &str, source: &str) -> Vec<String> {
    let syntax =
        syn::parse_file(source).unwrap_or_else(|error| panic!("parse {relative}: {error}"));
    let mut candidates = Vec::new();
    for item in syntax.items {
        let (visibility, ident) = match item {
            syn::Item::Enum(item) => (item.vis, item.ident),
            syn::Item::Struct(item) => (item.vis, item.ident),
            _ => continue,
        };
        if !matches!(visibility, syn::Visibility::Public(_)) {
            continue;
        }
        let name = ident.to_string();
        if ["Policy", "Profile", "Options"]
            .iter()
            .any(|suffix| name.ends_with(suffix))
        {
            candidates.push(format!("{relative}::{name}"));
        }
    }
    candidates
}

#[test]
fn source_inventory_flags_uncatalogued_galec_policy_candidates() {
    // This inventory is intentionally only a drift backstop. The compiler
    // assertions above prove the trait relation for the exact catalogued
    // types; parsing source cannot see macro expansion, aliases, or inferred
    // trait selection and therefore makes no such claim.
    let root = super::workspace_root();
    let mut candidates = Vec::new();
    for relative in [
        "crates/rumoca-ir-galec/src/package.rs",
        "crates/rumoca-phase-galec/src/input.rs",
    ] {
        let source = std::fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        candidates.extend(public_galec_policy_candidates(relative, &source));
    }
    candidates.sort();

    let mut expected = vec![
        "crates/rumoca-ir-galec/src/package.rs::AlgorithmCodeArithmeticProfile".to_owned(),
        "crates/rumoca-phase-galec/src/input.rs::GalecOptions".to_owned(),
    ];
    expected.sort();

    assert_eq!(
        candidates, expected,
        "a public GALEC policy/profile/options candidate changed; classify it and update the \
compiler-level negative-trait catalog in the same cutover"
    );
}

#[test]
fn source_inventory_detects_public_candidate_shape_without_claiming_private_helpers() {
    let candidates = public_galec_policy_candidates(
        "fixture.rs",
        r#"
            pub enum AddedPolicy { Explicit }
            pub struct AddedProfile;
            pub struct AddedOptions;
            enum PrivatePolicy { Internal }
            pub struct Presentation;
        "#,
    );

    assert_eq!(
        candidates,
        [
            "fixture.rs::AddedPolicy",
            "fixture.rs::AddedProfile",
            "fixture.rs::AddedOptions",
        ]
    );
}
