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

macro_rules! assert_not_clone {
    ($ty:ty) => {
        const _: fn() = || {
            trait AmbiguousIfClone<Marker> {
                fn probe() {}
            }
            impl<T: ?Sized> AmbiguousIfClone<()> for T {}
            struct ImplementsClone;
            impl<T: ?Sized + ::core::clone::Clone> AmbiguousIfClone<ImplementsClone> for T {}
            let _ = <$ty as AmbiguousIfClone<_>>::probe;
        };
    };
}

macro_rules! assert_not_serialize {
    ($ty:ty) => {
        const _: fn() = || {
            trait AmbiguousIfSerialize<Marker> {
                fn probe() {}
            }
            impl<T: ?Sized> AmbiguousIfSerialize<()> for T {}
            struct ImplementsSerialize;
            impl<T: ?Sized + serde::Serialize> AmbiguousIfSerialize<ImplementsSerialize> for T {}
            let _ = <$ty as AmbiguousIfSerialize<_>>::probe;
        };
    };
}

macro_rules! assert_not_deserialize {
    ($ty:ty) => {
        const _: fn() = || {
            trait AmbiguousIfDeserialize<Marker> {
                fn probe() {}
            }
            impl<T: ?Sized> AmbiguousIfDeserialize<()> for T {}
            struct ImplementsDeserialize;
            impl<T: ?Sized + serde::de::DeserializeOwned>
                AmbiguousIfDeserialize<ImplementsDeserialize> for T
            {
            }
            let _ = <$ty as AmbiguousIfDeserialize<_>>::probe;
        };
    };
}

assert_not_default!(rumoca_core::RealMatrixMultiplySemantics);
// The effective MLS 4.8.1 `fixed` value is decided once, from the variable's
// role, at DAE variable definition. A `Default` impl would let a containing
// struct fill it in silently and re-create the absence the type deletes.
assert_not_default!(rumoca_core::Fixity);
assert_not_default!(rumoca_ir_solve::SolveStorageColumn);
assert_not_default!(rumoca_ir_solve::SolveStorageCoordinate);
assert_not_default!(rumoca_ir_solve::SolveVariableStorageRun);
assert_not_default!(rumoca_compile::codegen::targets::TargetAlgorithmCodeArithmetic);
assert_not_default!(rumoca_ir_ast::Connection);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodeIntegerFormat);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodeRealFormat);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodePackageMetadata);
assert_not_default!(rumoca_ir_galec::package::AlgorithmCodePackage);
assert_not_default!(rumoca_ir_galec::package::CheckedAlgorithmBlock);
assert_not_default!(rumoca_phase_galec::GalecOptions);
assert_not_default!(rumoca_phase_dae::DaeConstructionProduct);
assert_not_default!(rumoca_sim::PreparedSimulation);
assert_not_clone!(rumoca_sim::PreparedSimulation);
// The callable proof plan is affine: a default value would be a plan whose
// coverage and acyclic receipt were never discharged.
assert_not_default!(rumoca_plan_callable::CallablePlan);
assert_not_default!(rumoca_ir_solve::SolveArithmeticProfile);
assert_not_default!(rumoca_ir_solve::SolveIntegerDomain);
assert_not_default!(rumoca_ir_solve::SolveMatrixMultiplyPlan);
assert_not_default!(rumoca_ir_solve::SolveArtifactInputs);
assert_not_default!(rumoca_ir_solve::ContinuousRefreshPlanInputs);
assert_not_default!(rumoca_ir_solve::ContinuousSolveSystemInputs);
assert_not_default!(rumoca_ir_solve::ContinuousRefreshOwners);
assert_not_default!(rumoca_ir_solve::RefreshPlan);
assert_not_default!(rumoca_ir_solve::RefreshRowOwnerId);
assert_not_default!(rumoca_ir_solve::RefreshRowSelection);
assert_not_default!(rumoca_ir_solve::RefreshSequenceId);
assert_not_default!(rumoca_ir_solve::IssuedRefreshPlan);
assert_not_default!(rumoca_ir_solve::ContinuousSolveSystem);
// The initialization aggregate is minted only by its correlation issuer, and
// a row role is proven per row, never assumed: a `Default` on either would
// re-create the silently benign label the issuer deletes.
assert_not_default!(rumoca_ir_solve::InitializationSolveSystem);
assert_not_default!(rumoca_ir_solve::InitializationRowRole);
assert_not_default!(rumoca_ir_solve::SolveAlgorithmBlock);
assert_not_default!(rumoca_ir_solve::SolveAlgorithmProduct<'static>);
assert_not_default!(rumoca_ir_solve::SolveProblem);
assert_not_default!(rumoca_ir_solve::SolveModel);
assert_not_default!(rumoca_ir_solve::SolvePureCallTable);
assert_not_default!(rumoca_ir_solve::SolveRealFormat);
assert_not_default!(rumoca_phase_codegen::PreparedSolveAlgorithmProduction<'static>);
assert_not_default!(rumoca_phase_codegen::SolveAlgorithmProductionProfile);

macro_rules! assert_fmi_semantic_carrier_is_nonforgeable {
    ($ty:ty) => {
        assert_not_default!($ty);
        assert_not_clone!($ty);
        assert_not_serialize!($ty);
        assert_not_deserialize!($ty);
    };
}

assert_fmi_semantic_carrier_is_nonforgeable!(rumoca_ir_solve::fmi::FmiLinkedRuntimeFacts);
assert_fmi_semantic_carrier_is_nonforgeable!(rumoca_ir_solve::fmi::FmiEventIndicatorPlan);
assert_fmi_semantic_carrier_is_nonforgeable!(rumoca_ir_solve::fmi::FmiComponent);
assert_fmi_semantic_carrier_is_nonforgeable!(rumoca_ir_solve::fmi::FmiRuntimeView);
// The scalar constant-derivative receipt and its FMI 3 carrier are proof
// carriers: the receipt is minted only by the profile checker and the carrier
// only by its admitting constructor, so neither may be defaulted, cloned, or
// reconstructed from a wire claim.
assert_fmi_semantic_carrier_is_nonforgeable!(rumoca_ir_solve::fmi::ScalarConstantDerivativeReceipt);
assert_fmi_semantic_carrier_is_nonforgeable!(
    rumoca_ir_solve::fmi::Fmi3ScalarConstantDerivativeCarrier
);
// Width is a copyable structural role, not an affine proof. Its value still
// cannot be defaulted or reconstructed from a serialized caller claim.
assert_not_default!(rumoca_ir_solve::fmi::FmiContinuousStateWidth);
assert_not_serialize!(rumoca_ir_solve::fmi::FmiContinuousStateWidth);
assert_not_deserialize!(rumoca_ir_solve::fmi::FmiContinuousStateWidth);
// A numerical plugin may clone an already-checked setup, but it cannot mint
// one from defaults or a caller-controlled wire representation.
assert_not_default!(rumoca_solver::fmi_me::MeNumericalSetup);
assert_not_serialize!(rumoca_solver::fmi_me::MeNumericalSetup);
assert_not_deserialize!(rumoca_solver::fmi_me::MeNumericalSetup);

#[test]
fn compiler_proves_catalogued_semantic_types_are_not_default_constructible() {
    // The assertions above are compile-time obligations. This named test keeps
    // the gate discoverable in filtered architecture-test output.
}

fn public_semantic_default_candidates(relative: &str, source: &str) -> Vec<String> {
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
        if ["Policy", "Profile", "Options", "Block", "Product"]
            .iter()
            .any(|suffix| name.ends_with(suffix))
            || name == "PreparedSolveAlgorithmProduction"
        {
            candidates.push(format!("{relative}::{name}"));
        }
    }
    candidates
}

#[test]
fn source_inventory_flags_uncatalogued_semantic_default_candidates() {
    // This inventory is intentionally only a drift backstop. The compiler
    // assertions above prove the trait relation for the exact catalogued
    // types; parsing source cannot see macro expansion, aliases, or inferred
    // trait selection and therefore makes no such claim.
    let root = super::workspace_root();
    let mut candidates = Vec::new();
    for relative in [
        "crates/rumoca-ir-galec/src/package.rs",
        "crates/rumoca-ir-solve/src/algorithm_block/root.rs",
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production.rs",
        "crates/rumoca-phase-dae/src/lib.rs",
        "crates/rumoca-phase-galec/src/input.rs",
    ] {
        let source = std::fs::read_to_string(root.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        candidates.extend(public_semantic_default_candidates(relative, &source));
    }
    candidates.sort();

    let mut expected = vec![
        "crates/rumoca-ir-galec/src/package.rs::AlgorithmCodeArithmeticProfile".to_owned(),
        "crates/rumoca-ir-galec/src/package.rs::CheckedAlgorithmBlock".to_owned(),
        "crates/rumoca-ir-solve/src/algorithm_block/root.rs::SolveAlgorithmBlock".to_owned(),
        "crates/rumoca-ir-solve/src/algorithm_block/root.rs::SolveAlgorithmProduct".to_owned(),
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production.rs::PreparedSolveAlgorithmProduction".to_owned(),
        // This is a value-preserving representation/configuration record, so
        // SPEC_0036 excludes it from the compiler-enforced semantic frontier.
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production.rs::ProductionCodeContainerProfile".to_owned(),
        "crates/rumoca-phase-codegen/src/views/solve_algorithm_production.rs::SolveAlgorithmProductionProfile".to_owned(),
        "crates/rumoca-phase-dae/src/lib.rs::DaeConstructionProduct".to_owned(),
        "crates/rumoca-phase-galec/src/input.rs::GalecOptions".to_owned(),
    ];
    expected.sort();

    assert_eq!(
        candidates, expected,
        "a public semantic root/product/policy/profile/options candidate changed; classify it and \
update the compiler-level negative-trait catalog when SPEC_0036 covers it"
    );
}

#[test]
fn source_inventory_detects_public_candidate_shape_without_claiming_private_helpers() {
    let candidates = public_semantic_default_candidates(
        "fixture.rs",
        r#"
            pub enum AddedPolicy { Explicit }
            pub struct AddedProfile;
            pub struct AddedOptions;
            pub struct AddedBlock;
            pub struct AddedProduct;
            pub struct PreparedSolveAlgorithmProduction;
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
            "fixture.rs::AddedBlock",
            "fixture.rs::AddedProduct",
            "fixture.rs::PreparedSolveAlgorithmProduction",
        ]
    );
}
