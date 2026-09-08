//! Structural L2 gate for the bounded scalar constant-derivative refinement.
//!
//! The semantic comparison is only a fact checker. Authority comes from the
//! affine production transition that consumes one C60-checked root, obtains
//! the profile and Solve root only through that carrier, and retains both
//! receipts with the same roots. This gate inspects Rust syntax; spelling and
//! comments are never accepted as ownership or trait evidence.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};
use syn::{Expr, FnArg, ImplItemFn, ItemFn, Pat, Token, Type};

use super::architecture_hardening_support::{production_rust_sources, workspace_root};

mod root_projection;
mod syntax_visitors;
use syntax_visitors::*;

const CHECKER_PATH: &str = "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement.rs";
const MODEL_VALUES_PATH: &str = "crates/rumoca-phase-solve/src/model_values.rs";
const TRAIT_ASSERTIONS_PATH: &str =
    "crates/rumoca-phase-solve/src/model_values/ownership_trait_assertions.rs";
const POSITIVE_WITNESS_PATH: &str = "crates/rumoca/tests/suite_core/pipeline_test.rs";
const SIM_ENTRY_PATH: &str = "crates/rumoca-sim/src/solve_lowering/entry.rs";
const SIM_FMI_PATH: &str = "crates/rumoca-sim/src/solve_lowering/fmi.rs";
const SIM_DIAGNOSTICS_PATH: &str = "crates/rumoca-sim/src/solve_lowering/diagnostics.rs";
const COMPILE_RENDERING_PATH: &str =
    "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs";

const C60_RECEIPT: &str = "CheckedDaeSolveVariableCatalogRefinement";
const C61_RECEIPT: &str = "CheckedDaeSolveScalarConstantDerivativeRefinement";
const PREPARED_CONTEXT: &str = "PreparedSolveContext";
const C60_ROOT: &str = "C60CheckedSolveRoot";
const CHECKED_ROOT: &str = "CheckedSolveRoot";
const LOWERED_ROOT: &str = "LoweredSolveModel";
const PROTECTED_TYPES: [&str; 6] = [
    C60_RECEIPT,
    C61_RECEIPT,
    PREPARED_CONTEXT,
    C60_ROOT,
    CHECKED_ROOT,
    LOWERED_ROOT,
];

#[derive(Clone)]
struct Sources {
    checker: String,
    model_values: String,
    trait_assertions: String,
    positive_witness: String,
    sim_entry: String,
    sim_fmi: String,
    sim_diagnostics: String,
    compile_rendering: String,
    additional_phase_sources: Vec<(String, String)>,
}

#[test]
fn scalar_constant_derivative_authority_is_one_affine_production_chain() {
    let sources = Sources::read();
    let violations = boundary_violations(&sources);
    assert!(
        violations.is_empty(),
        "SOLVE-C61 affine boundary drifted: {violations:#?}"
    );
}

#[test]
fn mutations_detect_foreign_roots_shadowing_and_fabricated_authority() {
    let sources = Sources::read();

    let mut second_profile_root = sources.clone();
    second_profile_root.model_values = replace_once(
        &second_profile_root.model_values,
        "let scalar_constant_derivative_profile = prepared\n            .as_dae()",
        "let scalar_constant_derivative_profile = unrelated_prepared\n            .as_dae()",
    );
    assert_violation(
        &second_profile_root,
        "profile-admission-not-direct-prepared-view",
    );

    let mut second_c60_root = sources.clone();
    second_c60_root.model_values = replace_once(
        &second_c60_root.model_values,
        "C60CheckedSolveRoot::construct(context, solve_model, vectors.catalog_transfer_map)?",
        "C60CheckedSolveRoot::construct(context, other_model, vectors.catalog_transfer_map)?",
    );
    assert_violation(&second_c60_root, "lowering-c60-root-not-direct-move");

    let mut shadowed_c60_root = sources.clone();
    shadowed_c60_root.model_values = replace_once(
        &shadowed_c60_root.model_values,
        "let checked_root =\n        C60CheckedSolveRoot::construct(context, solve_model, vectors.catalog_transfer_map)?",
        "let solve_model = other_model;\n    let checked_root =\n        C60CheckedSolveRoot::construct(context, solve_model, vectors.catalog_transfer_map)?",
    );
    assert_violation(&shadowed_c60_root, "lowering-carrier-binding-shadowed");

    let mut independent_c61_root = sources.clone();
    independent_c61_root.model_values = replace_once(
        &independent_c61_root.model_values,
        "check_scalar_constant_derivative_facts(&profile, &model)",
        "check_scalar_constant_derivative_facts(&profile, &other_model)",
    );
    assert_violation(&independent_c61_root, "c61-fact-call-not-carrier-bound");

    let mut shadowed_root = sources.clone();
    shadowed_root.model_values = replace_once(
        &shadowed_root.model_values,
        "let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {",
        "let model = other_model;\n        let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {",
    );
    assert_violation(&shadowed_root, "c61-carrier-binding-shadowed");

    let mut rebound_root = sources.clone();
    rebound_root.model_values = replace_once(
        &rebound_root.model_values,
        "let Self {\n            model,\n            context,",
        "let Self {\n            mut model,\n            context,",
    );
    rebound_root.model_values = replace_once(
        &rebound_root.model_values,
        "let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {",
        "model = other_model;\n        let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {",
    );
    assert_violation(&rebound_root, "c61-carrier-binding-shadowed");

    let mut duplicate_mint = sources.clone();
    duplicate_mint.checker.push_str(
        "\nfn forged_c61() -> CheckedDaeSolveScalarConstantDerivativeRefinement { CheckedDaeSolveScalarConstantDerivativeRefinement { _private: () } }\n",
    );
    assert_violation(&duplicate_mint, "c61-receipt-mint-census");

    let mut self_mint = sources.clone();
    self_mint.checker.push_str(
        "\nimpl CheckedDaeSolveScalarConstantDerivativeRefinement { fn forged() -> Self { Self { _private: () } } }\n",
    );
    assert_violation(&self_mint, "c61-receipt-mint-census");

    let mut raw_parts = sources.clone();
    raw_parts.model_values.push_str(
        "\nimpl C60CheckedSolveRoot<'_> { fn into_raw(self) -> rumoca_ir_solve::SolveModel { self.model } }\n",
    );
    assert_violation(&raw_parts, "private-carrier-owned-escape");

    let mut nested_raw_parts = sources.clone();
    nested_raw_parts.model_values.push_str(
        "\nmod nested_escape { impl super::C60CheckedSolveRoot<'_> { fn into_raw(self) -> rumoca_ir_solve::SolveModel { self.model } } }\n",
    );
    assert_violation(&nested_raw_parts, "private-carrier-owned-escape");

    let mut unrelated_impl_escape = sources.clone();
    unrelated_impl_escape.model_values.push_str(
        "\nstruct Unrelated;\nimpl Unrelated {\n    fn steal(root: CheckedSolveRoot<'_>) -> Result<CheckedDaeSolveScalarConstantDerivativeRefinement, ScalarConstantDerivativeUnsupported> {\n        root.scalar_constant_derivative_refinement\n    }\n}\n",
    );
    assert_violation(&unrelated_impl_escape, "private-carrier-owned-escape");

    let mut generic_impl_escape = sources.clone();
    generic_impl_escape.model_values.push_str(
        "\nstruct GenericEscape;\nimpl GenericEscape {\n    fn steal(profile: &AdmittedScalarConstantDerivativeProfile, model: &solve::SolveModel) -> impl Iterator<Item = CheckedDaeSolveScalarConstantDerivativeRefinement> {\n        std::iter::once(check_scalar_constant_derivative_facts(profile, model).expect(\"forged escape\"))\n    }\n}\n",
    );
    assert_violation(&generic_impl_escape, "private-carrier-owned-escape");
}

#[test]
fn mutations_detect_dead_or_deferred_comparison_before_mint() {
    let sources = Sources::read();

    let mut dead_check = sources.clone();
    dead_check.checker = replace_once(
        &dead_check.checker,
        "check_scalar_constant_derivative_refinement(profile, &solve_facts)\n}",
        "if std::hint::black_box(false) {\n        return check_scalar_constant_derivative_refinement(profile, &solve_facts);\n    }\n    Ok(CheckedDaeSolveScalarConstantDerivativeRefinement { _private: () })\n}",
    );
    assert_violation(&dead_check, "fact-wrapper-not-tail-check");

    let mut deferred_check = sources.clone();
    deferred_check.checker = replace_once(
        &deferred_check.checker,
        "check_scalar_constant_derivative_refinement(profile, &solve_facts)\n}",
        "let _not_executed = || {\n        check_scalar_constant_derivative_refinement(profile, &solve_facts)\n    };\n    Ok(CheckedDaeSolveScalarConstantDerivativeRefinement { _private: () })\n}",
    );
    assert_violation(&deferred_check, "fact-wrapper-not-tail-check");

    let mut shadowed_projection = sources.clone();
    shadowed_projection.checker = replace_once(
        &shadowed_projection.checker,
        "let solve_facts = project_solve_facts(model);",
        "let project_solve_facts = |_model: &solve::SolveModel| forged_solve_facts();\n    let solve_facts = project_solve_facts(model);",
    );
    assert_violation(&shadowed_projection, "fact-wrapper-shadowed");
}

#[test]
fn mutations_detect_cfg_disabled_or_unreachable_positive_witnesses() {
    let sources = Sources::read();

    let mut cfg_disabled = sources.clone();
    cfg_disabled.positive_witness = replace_once(
        &cfg_disabled.positive_witness,
        "#[test]\nfn unit_derivative_production_lowering_carries_the_equation_refinement_receipt()",
        "#[test]\n#[cfg(any())]\nfn unit_derivative_production_lowering_carries_the_equation_refinement_receipt()",
    );
    assert_violation(
        &cfg_disabled,
        "positive-production-witness-not-unconditionally-runnable",
    );

    let mut ignored = sources.clone();
    ignored.positive_witness = replace_once(
        &ignored.positive_witness,
        "#[test]\nfn unit_derivative_production_lowering_carries_the_equation_refinement_receipt()",
        "#[test]\n#[ignore]\nfn unit_derivative_production_lowering_carries_the_equation_refinement_receipt()",
    );
    assert_violation(
        &ignored,
        "positive-production-witness-not-unconditionally-runnable",
    );

    let mut deferred_receipt_read = sources.clone();
    deferred_receipt_read.positive_witness = replace_once(
        &deferred_receipt_read.positive_witness,
        "if let Err(unsupported) = lowered.scalar_constant_derivative_refinement() {",
        "let _not_executed = || lowered.scalar_constant_derivative_refinement();\n    if let Err(unsupported) = Ok(()) {",
    );
    assert_violation(
        &deferred_receipt_read,
        "positive-production-witness-receipt-not-reachable",
    );
}

#[test]
fn mutations_detect_vacuous_admission_and_missing_positive_witness() {
    let sources = Sources::read();

    let mut never_admits = sources.clone();
    never_admits.checker = replace_once(
        &never_admits.checker,
        "admit_dae_profile(view)",
        "Err(ScalarConstantDerivativeUnsupported::RuntimeOverrides)",
    );
    assert_violation(&never_admits, "profile-has-no-admission-route");

    let mut no_witness = sources.clone();
    no_witness.positive_witness = replace_once(
        &no_witness.positive_witness,
        "fn unit_derivative_production_lowering_carries_the_equation_refinement_receipt()",
        "fn unit_derivative_production_lowering_does_not_require_equation_refinement()",
    );
    assert_violation(&no_witness, "positive-production-witness-missing");

    let mut ignores_receipt = sources.clone();
    ignores_receipt.positive_witness = replace_once(
        &ignores_receipt.positive_witness,
        "lowered.scalar_constant_derivative_refinement()",
        "Ok(&placeholder_receipt)",
    );
    assert_violation(
        &ignores_receipt,
        "positive-production-witness-does-not-read-receipt",
    );
}

#[test]
fn mutations_detect_forbidden_traits_aliases_and_assertion_drift() {
    let sources = Sources::read();

    let mut cloned_receipt = sources.clone();
    cloned_receipt.checker = replace_once(
        &cloned_receipt.checker,
        "pub struct CheckedDaeSolveScalarConstantDerivativeRefinement {",
        "#[derive(Clone)]\npub struct CheckedDaeSolveScalarConstantDerivativeRefinement {",
    );
    assert_violation(&cloned_receipt, "forbidden-trait:Clone");

    let mut conditionally_cloned_receipt = sources.clone();
    conditionally_cloned_receipt.checker = replace_once(
        &conditionally_cloned_receipt.checker,
        "pub struct CheckedDaeSolveScalarConstantDerivativeRefinement {",
        "#[cfg_attr(feature = \"forge\", derive(Clone))]\npub struct CheckedDaeSolveScalarConstantDerivativeRefinement {",
    );
    assert_violation(&conditionally_cloned_receipt, "forbidden-trait:Clone");

    let mut serialized_carrier = sources.clone();
    serialized_carrier.model_values.push_str(
        "\nimpl serde::Serialize for C60CheckedSolveRoot<'_> { fn serialize<S>(&self, _: S) -> Result<S::Ok, S::Error> where S: serde::Serializer { unreachable!() } }\n",
    );
    assert_violation(&serialized_carrier, "forbidden-trait:Serialize");

    let mut nested_default = sources.clone();
    nested_default.checker.push_str(
        "\n#[cfg(not(test))]\nmod nested_default_forge {\n    macro_rules! mint { () => { super::CheckedDaeSolveScalarConstantDerivativeRefinement { _private: () } }; }\n    impl Default for super::CheckedDaeSolveScalarConstantDerivativeRefinement { fn default() -> Self { mint!() } }\n}\n",
    );
    assert_violation(&nested_default, "forbidden-trait:Default");

    let mut nested_clone = sources.clone();
    nested_clone.checker.push_str(
        "\n#[cfg(not(test))]\nmod nested_clone_forge {\n    macro_rules! mint { () => { super::CheckedDaeSolveScalarConstantDerivativeRefinement { _private: () } }; }\n    impl Clone for super::CheckedDaeSolveScalarConstantDerivativeRefinement { fn clone(&self) -> Self { mint!() } }\n}\n",
    );
    assert_violation(&nested_clone, "forbidden-trait:Clone");

    let mut generic_macro_mint = sources.clone();
    generic_macro_mint.checker.push_str(
        "\n#[cfg(not(test))]\nmod nested_macro_forge { macro_rules! mint { ($name:ident) => { $name { _private: () } }; } }\n",
    );
    assert_violation(&generic_macro_mint, "protected-type-in-macro");

    let mut attribute_macro_mint = sources.clone();
    attribute_macro_mint
        .checker
        .push_str("\n#[unreviewed_codegen]\nstruct AttributeMacroForge;\n");
    assert_violation(&attribute_macro_mint, "protected-type-in-macro");

    let mut aliased_carrier = sources.clone();
    aliased_carrier
        .model_values
        .push_str("\ntype ReusableCheckedRoot<'a> = C60CheckedSolveRoot<'a>;\n");
    assert_violation(&aliased_carrier, "protected-type-alias");

    let mut deleted_assertion = sources.clone();
    deleted_assertion.trait_assertions = replace_once(
        &deleted_assertion.trait_assertions,
        "assert_affine_proof_carrier!(C60CheckedSolveRoot<'static>);",
        "",
    );
    assert_violation(&deleted_assertion, "negative-trait-assertion-catalog");

    let mut disabled_assertion = sources.clone();
    disabled_assertion.trait_assertions = replace_once(
        &disabled_assertion.trait_assertions,
        "impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}",
        "impl<T> AmbiguousIfImplemented<Implements> for T {}",
    );
    assert_violation(&disabled_assertion, "negative-trait-assertion-mechanism");

    let mut cfg_split_assertion = sources.clone();
    cfg_split_assertion.trait_assertions = replace_once(
        &cfg_split_assertion.trait_assertions,
        "($ty:ty) => {\n        assert_not_implemented!",
        "($ty:ty) => {\n        #[cfg(test)]\n        assert_not_implemented!",
    );
    assert_violation(&cfg_split_assertion, "negative-trait-assertion-mechanism");
}

#[test]
fn mutations_detect_macro_mints_and_deferred_transition_checks() {
    let sources = Sources::read();

    let mut child_alias_macro_mint = sources.clone();
    child_alias_macro_mint
        .checker
        .push_str("\npub(crate) mod forged_receipt;\n");
    child_alias_macro_mint.additional_phase_sources.push((
        "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement/forged_receipt.rs"
            .to_owned(),
        "use super::CheckedDaeSolveScalarConstantDerivativeRefinement as R;\n\
         macro_rules! mint { () => { R { _private: () } }; }\n\
         pub(crate) fn forged() -> R { mint!() }\n"
            .to_owned(),
    ));
    assert_violation(&child_alias_macro_mint, "protected-type-in-macro");

    let mut renamed_macro_mint = sources.clone();
    renamed_macro_mint
        .checker
        .push_str("\npub(crate) mod renamed_macro_receipt;\n");
    renamed_macro_mint.additional_phase_sources.push((
        "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement/renamed_macro_receipt.rs"
            .to_owned(),
        "use super::CheckedDaeSolveScalarConstantDerivativeRefinement as R;\n\
         use external_macros::mint as format;\n\
         pub(crate) fn forged() -> R { format!(R) }\n"
            .to_owned(),
    ));
    assert_violation(&renamed_macro_mint, "protected-type-in-macro");

    let mut child_alias_direct_mint = sources.clone();
    child_alias_direct_mint
        .checker
        .push_str("\npub(crate) mod forged_direct_receipt;\n");
    child_alias_direct_mint.additional_phase_sources.push((
        "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement/forged_direct_receipt.rs"
            .to_owned(),
        "use super::CheckedDaeSolveScalarConstantDerivativeRefinement as R;\n\
         pub(crate) fn forged() -> R { R { _private: () } }\n"
            .to_owned(),
    ));
    assert_violation(&child_alias_direct_mint, "c61-receipt-mint-census");

    let mut associated_alias_mint = sources.clone();
    associated_alias_mint
        .checker
        .push_str("\npub(crate) mod associated_alias_receipt;\n");
    associated_alias_mint.additional_phase_sources.push((
        "crates/rumoca-phase-solve/src/scalar_constant_derivative_refinement/associated_alias_receipt.rs"
            .to_owned(),
        "trait ReceiptAlias { type Receipt; }\n\
         impl ReceiptAlias for () {\n\
             type Receipt = super::CheckedDaeSolveScalarConstantDerivativeRefinement;\n\
         }\n\
         pub(crate) fn forged() -> <() as ReceiptAlias>::Receipt {\n\
             <() as ReceiptAlias>::Receipt { _private: () }\n\
         }\n"
            .to_owned(),
    ));
    assert_violation(&associated_alias_mint, "protected-type-alias");

    let mut separated_transition = child_alias_macro_mint;
    separated_transition.model_values = replace_once(
        &separated_transition.model_values,
        "Ok(profile) => Ok(\n                check_scalar_constant_derivative_facts(&profile, &model).map_err(|error| {\n                    scalar_constant_derivative_refinement_error(prepared.as_dae(), error)\n                })?,\n            ),",
        "Ok(profile) => {\n                if std::hint::black_box(false) {\n                    let _discarded = check_scalar_constant_derivative_facts(&profile, &model)\n                        .map_err(|error| scalar_constant_derivative_refinement_error(prepared.as_dae(), error))?;\n                }\n                Ok(crate::scalar_constant_derivative_refinement::forged_receipt::forged())\n            },",
    );
    assert_violation(&separated_transition, "c61-refinement-not-direct-check");

    let mut shadowed_checker = sources.clone();
    shadowed_checker.model_values = replace_once(
        &shadowed_checker.model_values,
        "let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {",
        "let check_scalar_constant_derivative_facts = |_profile: &(), _model: &()| -> Result<(), ()> { Ok(()) };\n        let scalar_constant_derivative_refinement = match scalar_constant_derivative_profile {",
    );
    assert_violation(&shadowed_checker, "c61-checker-shadowed");
}

impl Sources {
    fn read() -> Self {
        Self {
            checker: read(CHECKER_PATH),
            model_values: read(MODEL_VALUES_PATH),
            trait_assertions: read(TRAIT_ASSERTIONS_PATH),
            positive_witness: read(POSITIVE_WITNESS_PATH),
            sim_entry: read(SIM_ENTRY_PATH),
            sim_fmi: read(SIM_FMI_PATH),
            sim_diagnostics: read(SIM_DIAGNOSTICS_PATH),
            compile_rendering: read(COMPILE_RENDERING_PATH),
            additional_phase_sources: Vec::new(),
        }
    }
}

fn read(relative: &str) -> String {
    let path = workspace_root().join(relative);
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

fn parse(source: &str, label: &str) -> syn::File {
    syn::parse_file(source).unwrap_or_else(|error| panic!("parse {label}: {error}"))
}

fn boundary_violations(sources: &Sources) -> BTreeSet<String> {
    let mut violations = BTreeSet::new();
    let model = parse(&sources.model_values, MODEL_VALUES_PATH);
    let checker = parse(&sources.checker, CHECKER_PATH);
    check_aggregate_shapes(&model, &checker, &mut violations);
    check_affine_transitions(&model, &mut violations);
    check_fact_checker(&checker, &mut violations);
    check_positive_witness(&sources.positive_witness, &mut violations);
    check_negative_trait_assertions(&model, &sources.trait_assertions, &mut violations);
    check_crate_wide_census(sources, &mut violations);
    check_consumers(sources, &mut violations);
    violations
}

fn check_aggregate_shapes(
    model: &syn::File,
    checker: &syn::File,
    violations: &mut BTreeSet<String>,
) {
    for (name, expected) in [
        (C61_RECEIPT, vec![("_private", None)]),
        (
            PREPARED_CONTEXT,
            vec![
                ("prepared", Some("PreparedDae")),
                ("scalar_constant_derivative_profile", Some("Result")),
            ],
        ),
        (
            C60_ROOT,
            vec![
                ("model", Some("SolveModel")),
                ("context", Some(PREPARED_CONTEXT)),
                ("variable_catalog_refinement", Some(C60_RECEIPT)),
            ],
        ),
        (
            CHECKED_ROOT,
            vec![
                ("model", Some("SolveModel")),
                ("_prepared", Some("PreparedDae")),
                ("_variable_catalog_refinement", Some(C60_RECEIPT)),
                ("scalar_constant_derivative_refinement", Some("Result")),
            ],
        ),
        (
            LOWERED_ROOT,
            vec![
                ("checked_root", Some(CHECKED_ROOT)),
                ("program_seconds", Some("f64")),
                ("runtime_value_seconds", Some("f64")),
            ],
        ),
    ] {
        let source = if name == C61_RECEIPT { checker } else { model };
        let Some(item) = find_struct(source, name) else {
            violations.insert(format!("missing-aggregate:{name}"));
            continue;
        };
        let fields = item
            .fields
            .iter()
            .filter_map(|field| Some((field.ident.as_ref()?.to_string(), &field.ty, &field.vis)))
            .collect::<Vec<_>>();
        if fields.len() != expected.len()
            || expected.iter().any(|(field_name, type_name)| {
                !fields.iter().any(|(actual, ty, visibility)| {
                    actual == field_name
                        && matches!(visibility, syn::Visibility::Inherited)
                        && type_name.map_or_else(
                            || matches!(ty, Type::Tuple(tuple) if tuple.elems.is_empty()),
                            |name| type_contains_name(ty, name),
                        )
                })
            })
        {
            violations.insert(format!("aggregate-shape:{name}"));
        }
    }
    if !find_struct(checker, C61_RECEIPT)
        .is_some_and(|item| matches!(item.vis, syn::Visibility::Public(_)))
    {
        violations.insert("c61-receipt-not-public-opaque".to_owned());
    }
    for private in [PREPARED_CONTEXT, C60_ROOT, CHECKED_ROOT] {
        if !find_struct(model, private)
            .is_some_and(|item| matches!(item.vis, syn::Visibility::Inherited))
        {
            violations.insert(format!("private-carrier-visible:{private}"));
        }
    }
}

fn check_affine_transitions(file: &syn::File, violations: &mut BTreeSet<String>) {
    check_profile_admission_transition(file, violations);
    check_c60_transition(file, violations);
    check_c61_transition(file, violations);
    check_complete_lowering_transition(file, violations);
}

fn check_profile_admission_transition(file: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(context_new) = find_method(file, PREPARED_CONTEXT, "new") else {
        violations.insert("profile-admission-constructor-missing".to_owned());
        return;
    };
    if !takes_prepared_context_inputs(&context_new.sig.inputs)
        || exact_import_count(
            file,
            &[
                "crate",
                "scalar_constant_derivative_refinement",
                "admit_scalar_constant_derivative_profile",
            ],
        ) != 1
        || block_value_definition_count(
            &context_new.block,
            "admit_scalar_constant_derivative_profile",
        ) != 0
        || !local_initializer(&context_new.block, "scalar_constant_derivative_profile")
            .is_some_and(prepared_profile_admission_is_direct)
        || binding_count(&context_new.block, "scalar_constant_derivative_profile") != 1
        || self_literal_count(
            &context_new.block,
            &["prepared", "scalar_constant_derivative_profile"],
        ) != 1
    {
        violations.insert("profile-admission-not-direct-prepared-view".to_owned());
    }
}

fn check_c60_transition(file: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(c60_construct) = find_method(file, C60_ROOT, "construct") else {
        violations.insert("c60-carrier-constructor-missing".to_owned());
        return;
    };
    if !takes_named_values(
        &c60_construct.sig.inputs,
        &[
            ("context", PREPARED_CONTEXT),
            ("model", "SolveModel"),
            ("mapping", "VariableCatalogTransferMap"),
        ],
    ) || self_literal_count(
        &c60_construct.block,
        &["model", "context", "variable_catalog_refinement"],
    ) != 1
    {
        violations.insert("c60-carrier-not-atomic".to_owned());
    }
}

fn check_c61_transition(file: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(c61) = find_method(file, C60_ROOT, "into_equation_refined") else {
        violations.insert("c61-consuming-transition-missing".to_owned());
        return;
    };
    if exact_import_count(
        file,
        &[
            "crate",
            "scalar_constant_derivative_refinement",
            "check_scalar_constant_derivative_facts",
        ],
    ) != 1
        || block_value_definition_count(&c61.block, "check_scalar_constant_derivative_facts") != 0
    {
        violations.insert("c61-checker-shadowed".to_owned());
    }
    if !takes_only_value_self(c61)
        || !return_contains(&c61.sig.output, CHECKED_ROOT)
        || !has_struct_destructure(
            &c61.block,
            "Self",
            "self",
            &["model", "context", "variable_catalog_refinement"],
        )
        || !has_struct_destructure(
            &c61.block,
            PREPARED_CONTEXT,
            "context",
            &["prepared", "scalar_constant_derivative_profile"],
        )
        || [
            "model",
            "context",
            "variable_catalog_refinement",
            "prepared",
            "scalar_constant_derivative_profile",
            "profile",
            "scalar_constant_derivative_refinement",
        ]
        .iter()
        .any(|name| binding_count(&c61.block, name) != 1)
    {
        violations.insert("c61-carrier-binding-shadowed".to_owned());
    }
    if !call_has_ident_arguments(
        &c61.block,
        "check_scalar_constant_derivative_facts",
        &["profile", "model"],
    ) {
        violations.insert("c61-fact-call-not-carrier-bound".to_owned());
    }
    if !try_contains_call(&c61.block, "check_scalar_constant_derivative_facts")
        || gate_event_order(&c61.block, "check_scalar_constant_derivative_facts") != ["check"]
    {
        violations.insert("c61-mint-not-success-gated".to_owned());
    }
    if !local_initializer(&c61.block, "scalar_constant_derivative_refinement")
        .is_some_and(c61_refinement_initializer_is_direct)
    {
        violations.insert("c61-refinement-not-direct-check".to_owned());
    }
    if explicit_literal_fields(&c61.block, CHECKED_ROOT)
        != vec![BTreeMap::from([
            ("_prepared".to_owned(), "prepared".to_owned()),
            (
                "_variable_catalog_refinement".to_owned(),
                "variable_catalog_refinement".to_owned(),
            ),
            ("model".to_owned(), "model".to_owned()),
            (
                "scalar_constant_derivative_refinement".to_owned(),
                "scalar_constant_derivative_refinement".to_owned(),
            ),
        ])]
    {
        violations.insert("c61-final-carrier-not-direct-move".to_owned());
    }
}

fn check_complete_lowering_transition(file: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(lower) = find_function(file, "lower_solve_model") else {
        violations.insert("sole-lowering-missing".to_owned());
        return;
    };
    if !local_initializer(&lower.block, "context").is_some_and(|expression| {
        qualified_call_has_idents(
            expression,
            PREPARED_CONTEXT,
            "new",
            &["prepared", "overrides"],
        )
    }) {
        violations.insert("lowering-context-not-direct-move".to_owned());
    }
    if ["prepared", "context", "solve_model", "checked_root"]
        .iter()
        .any(|name| binding_count(&lower.block, name) != 1)
    {
        violations.insert("lowering-carrier-binding-shadowed".to_owned());
    }
    if !local_initializer(&lower.block, "checked_root").is_some_and(checked_root_chain_is_direct) {
        violations.insert("lowering-c60-root-not-direct-move".to_owned());
    }
    if explicit_literal_fields(&lower.block, LOWERED_ROOT)
        != vec![BTreeMap::from([
            ("checked_root".to_owned(), "checked_root".to_owned()),
            ("program_seconds".to_owned(), "program_seconds".to_owned()),
            (
                "runtime_value_seconds".to_owned(),
                "runtime_value_seconds".to_owned(),
            ),
        ])]
    {
        violations.insert("lowered-root-not-direct-move".to_owned());
    }
}

fn check_fact_checker(file: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(admit) = find_function(file, "admit_scalar_constant_derivative_profile") else {
        violations.insert("profile-admission-missing".to_owned());
        return;
    };
    if block_value_definition_count(&admit.block, "admit_dae_profile") != 0
        || !tail_expression(&admit.block).is_some_and(|expression| {
            call_expression_has_idents(expression, "admit_dae_profile", &["view"])
        })
    {
        violations.insert("profile-has-no-admission-route".to_owned());
    }
    let Some(checker) = find_function(file, "check_scalar_constant_derivative_facts") else {
        violations.insert("pure-fact-checker-missing".to_owned());
        return;
    };
    if !takes_named_references(
        &checker.sig.inputs,
        &[
            ("profile", "AdmittedScalarConstantDerivativeProfile"),
            ("model", "SolveModel"),
        ],
    ) || !return_contains(&checker.sig.output, C61_RECEIPT)
        || call_count(&checker.block, "project_solve_facts") != 1
        || call_count(
            &checker.block,
            "check_scalar_constant_derivative_refinement",
        ) != 1
    {
        violations.insert("pure-fact-checker-shape".to_owned());
    }
    if [
        "profile",
        "model",
        "project_solve_facts",
        "check_scalar_constant_derivative_refinement",
    ]
    .iter()
    .any(|name| block_value_definition_count(&checker.block, name) != 0)
    {
        violations.insert("fact-wrapper-shadowed".to_owned());
    }
    if !local_initializer(&checker.block, "solve_facts").is_some_and(|expression| {
        call_expression_has_idents(expression, "project_solve_facts", &["model"])
    }) || !tail_expression(&checker.block).is_some_and(|expression| {
        call_expression_has_idents(
            expression,
            "check_scalar_constant_derivative_refinement",
            &["profile", "solve_facts"],
        )
    }) || explicit_struct_literal_count(&checker.block, C61_RECEIPT) != 0
    {
        violations.insert("fact-wrapper-not-tail-check".to_owned());
    }
    for forbidden in [
        "lower_solve_model",
        "lower_prepared_solve_package",
        "prepare_for_solve",
        "construct",
    ] {
        if call_count(&checker.block, forbidden) != 0 {
            violations.insert("fact-checker-relowers-or-constructs".to_owned());
        }
    }
    let Some(comparison) = find_function(file, "check_scalar_constant_derivative_refinement")
    else {
        violations.insert("total-comparison-missing".to_owned());
        return;
    };
    if !takes_named_references(
        &comparison.sig.inputs,
        &[
            ("dae", "AdmittedScalarConstantDerivativeProfile"),
            ("solve", "SolveFacts"),
        ],
    ) || !return_contains(&comparison.sig.output, C61_RECEIPT)
        || explicit_struct_literal_count(&comparison.block, C61_RECEIPT) != 1
        || !tail_expression(&comparison.block)
            .is_some_and(|expression| ok_wraps_struct_literal(expression, C61_RECEIPT))
    {
        violations.insert("total-comparison-does-not-own-sole-mint".to_owned());
    }
}

fn check_positive_witness(source: &str, violations: &mut BTreeSet<String>) {
    let file = parse(source, POSITIVE_WITNESS_PATH);
    let Some(witness) = find_function(
        &file,
        "unit_derivative_production_lowering_carries_the_equation_refinement_receipt",
    ) else {
        violations.insert("positive-production-witness-missing".to_owned());
        return;
    };
    let is_test = witness
        .attrs
        .iter()
        .any(|attribute| attribute.path().is_ident("test"));
    let disabled = witness.attrs.iter().any(|attribute| {
        attribute.path().is_ident("cfg")
            || attribute.path().is_ident("cfg_attr")
            || attribute.path().is_ident("ignore")
    });
    if !is_test || disabled {
        violations.insert("positive-production-witness-not-unconditionally-runnable".to_owned());
    }
    if witness.block.stmts.len() != 4
        || !local_initializer(&witness.block, "lowered")
            .is_some_and(witness_lowering_initializer_is_direct)
        || call_count(
            &witness.block,
            "lower_correlated_for_simulation_with_overrides",
        ) != 1
    {
        violations.insert("positive-production-witness-bypasses-production".to_owned());
    }
    if method_call_count(&witness.block, "scalar_constant_derivative_refinement") != 1 {
        violations.insert("positive-production-witness-does-not-read-receipt".to_owned());
    }
    if !witness_receipt_check_is_direct(&witness.block) {
        violations.insert("positive-production-witness-receipt-not-reachable".to_owned());
    }
}

fn check_negative_trait_assertions(
    model: &syn::File,
    source: &str,
    violations: &mut BTreeSet<String>,
) {
    let has_module = model.items.iter().any(|item| {
        matches!(item, syn::Item::Mod(module)
            if module.ident == "ownership_trait_assertions"
                && !module.attrs.iter().any(|attr| attr.path().is_ident("cfg")))
    });
    let assertions = parse(source, TRAIT_ASSERTIONS_PATH)
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Macro(item) if item.mac.path.is_ident("assert_affine_proof_carrier") => {
                syn::parse2::<Type>(item.mac.tokens.clone())
                    .ok()
                    .and_then(|ty| type_last_name(&ty))
            }
            _ => None,
        })
        .collect::<BTreeSet<_>>();
    let syntax = parse(source, TRAIT_ASSERTIONS_PATH);
    let invocations_are_unconditional = syntax.items.iter().all(|item| {
        !matches!(item, syn::Item::Macro(item)
            if item.mac.path.is_ident("assert_affine_proof_carrier")
                && item.attrs.iter().any(|attribute|
                    attribute.path().is_ident("cfg")
                        || attribute.path().is_ident("cfg_attr")))
    });
    let negative_shape = macro_identifier_counts(&syntax, "assert_not_implemented");
    let aggregate_shape = macro_identifier_counts(&syntax, "assert_affine_proof_carrier");
    let mechanism_is_active = [
        ("AmbiguousIfImplemented", 4),
        ("Marker", 1),
        ("Implements", 2),
        ("probe", 2),
        ("bound", 2),
        ("ty", 3),
    ]
    .iter()
    .all(|(name, count)| negative_shape.get(*name) == Some(count))
        && [
            ("assert_not_implemented", 5),
            ("Clone", 1),
            ("Copy", 1),
            ("Default", 1),
            ("Serialize", 1),
            ("DeserializeOwned", 1),
            ("ty", 7),
        ]
        .iter()
        .all(|(name, count)| aggregate_shape.get(*name) == Some(count))
        && !negative_shape.contains_key("cfg")
        && !negative_shape.contains_key("cfg_attr")
        && !aggregate_shape.contains_key("cfg")
        && !aggregate_shape.contains_key("cfg_attr")
        && invocations_are_unconditional;
    let expected = [
        C60_RECEIPT,
        C61_RECEIPT,
        PREPARED_CONTEXT,
        C60_ROOT,
        CHECKED_ROOT,
        LOWERED_ROOT,
    ]
    .into_iter()
    .map(ToOwned::to_owned)
    .collect::<BTreeSet<_>>();
    if !has_module || assertions != expected {
        violations.insert("negative-trait-assertion-catalog".to_owned());
    }
    if !mechanism_is_active {
        violations.insert("negative-trait-assertion-mechanism".to_owned());
    }
}

struct ProtectedSurface<'violations> {
    assertion_file: bool,
    protected_scope: bool,
    aliases: &'violations BTreeMap<String, String>,
    violations: &'violations mut BTreeSet<String>,
}

fn protected_impl_owner(
    item: &syn::ItemImpl,
    aliases: &BTreeMap<String, String>,
) -> Option<String> {
    type_last_name(&item.self_ty)
        .and_then(|name| aliases.get(&name).cloned())
        .filter(|owner| PROTECTED_TYPES.contains(&owner.as_str()))
}

fn forbidden_impl_trait(item: &syn::ItemImpl) -> Option<String> {
    let (_, path, _) = item.trait_.as_ref()?;
    let trait_name = path.segments.last()?.ident.to_string();
    [
        "Clone",
        "Copy",
        "Default",
        "Serialize",
        "Deserialize",
        "Deref",
        "AsRef",
        "Borrow",
        "Into",
        "From",
    ]
    .contains(&trait_name.as_str())
    .then_some(trait_name)
}

fn attribute_is_reviewed(attribute: &syn::Attribute) -> bool {
    if ["doc", "error", "must_use"]
        .iter()
        .any(|name| attribute.path().is_ident(name))
    {
        return true;
    }
    if !attribute.path().is_ident("derive") {
        return false;
    }
    let Ok(derives) = attribute.parse_args_with(
        syn::punctuated::Punctuated::<syn::Path, syn::Token![,]>::parse_terminated,
    ) else {
        return false;
    };
    derives.iter().all(|path| {
        path.is_ident("Debug")
            || path.is_ident("Clone")
            || path.is_ident("Copy")
            || path.is_ident("PartialEq")
            || path.is_ident("Eq")
            || path.segments.len() == 2
                && path.segments[0].ident == "thiserror"
                && path.segments[1].ident == "Error"
    })
}

fn is_allowed_affine_method(owner: Option<&str>, method: &syn::ImplItemFn) -> bool {
    owner.is_some_and(|owner| {
        matches!(
            (owner, method.sig.ident.to_string().as_str()),
            (PREPARED_CONTEXT, "new")
                | (C60_ROOT, "construct" | "into_equation_refined")
                | (LOWERED_ROOT, "into_model")
        )
    })
}

fn impl_method_escapes_affine_carrier(
    method: &syn::ImplItemFn,
    owner: Option<&str>,
    aliases: &BTreeMap<String, String>,
) -> bool {
    if is_allowed_affine_method(owner, method) {
        return false;
    }
    let owns_protected_input = method.sig.inputs.iter().any(|argument| match argument {
        FnArg::Receiver(receiver) => receiver.reference.is_none() && owner.is_some(),
        FnArg::Typed(argument) => type_owns_resolved(&argument.ty, aliases, &PROTECTED_TYPES),
    });
    let owns_protected_output = PROTECTED_TYPES
        .iter()
        .any(|target| return_owns_resolved(&method.sig.output, target, aliases))
        || (owner.is_some() && return_owns(&method.sig.output, "Self"));
    let owns_raw_root = owner.is_some()
        && ["SolveModel", "PreparedDae"]
            .iter()
            .any(|target| return_owns(&method.sig.output, target));
    owns_protected_input || owns_protected_output || owns_raw_root
}

impl Visit<'_> for ProtectedSurface<'_> {
    fn visit_attribute(&mut self, attribute: &syn::Attribute) {
        if self.protected_scope && !attribute_is_reviewed(attribute) {
            self.violations.insert("protected-type-in-macro".to_owned());
        }
        visit::visit_attribute(self, attribute);
    }

    fn visit_item_mod(&mut self, item: &syn::ItemMod) {
        if super::attributes_require_test(&item.attrs) {
            return;
        }
        visit::visit_item_mod(self, item);
    }

    fn visit_item_fn(&mut self, item: &syn::ItemFn) {
        let allowed = (item.sig.ident == "check_variable_catalog_refinement"
            && return_owns_resolved(&item.sig.output, C60_RECEIPT, self.aliases))
            || (item.sig.ident == "check_scalar_constant_derivative_facts"
                && return_owns_resolved(&item.sig.output, C61_RECEIPT, self.aliases))
            || (item.sig.ident == "check_scalar_constant_derivative_refinement"
                && return_owns_resolved(&item.sig.output, C61_RECEIPT, self.aliases))
            || (item.sig.ident == "lower_solve_model"
                && return_owns_resolved(&item.sig.output, LOWERED_ROOT, self.aliases));
        if PROTECTED_TYPES
            .iter()
            .any(|target| return_owns_resolved(&item.sig.output, target, self.aliases))
            && !allowed
        {
            self.violations
                .insert("private-carrier-owned-escape".to_owned());
        }
        visit::visit_item_fn(self, item);
    }

    fn visit_item_struct(&mut self, item: &syn::ItemStruct) {
        if PROTECTED_TYPES.contains(&item.ident.to_string().as_str()) {
            self.violations.extend(
                forbidden_derives(&item.attrs)
                    .into_iter()
                    .map(|trait_name| format!("forbidden-trait:{trait_name}")),
            );
        }
        visit::visit_item_struct(self, item);
    }

    fn visit_item_impl(&mut self, item: &syn::ItemImpl) {
        let owner = protected_impl_owner(item, self.aliases);
        if owner.is_some()
            && let Some(trait_name) = forbidden_impl_trait(item)
        {
            self.violations
                .insert(format!("forbidden-trait:{trait_name}"));
        }
        for method in item.items.iter().filter_map(|member| match member {
            syn::ImplItem::Fn(method) => Some(method),
            _ => None,
        }) {
            if impl_method_escapes_affine_carrier(method, owner.as_deref(), self.aliases) {
                self.violations
                    .insert("private-carrier-owned-escape".to_owned());
            }
        }
        visit::visit_item_impl(self, item);
    }

    fn visit_item_type(&mut self, item: &syn::ItemType) {
        if PROTECTED_TYPES
            .iter()
            .any(|target| type_contains_resolved_name(&item.ty, target, self.aliases))
        {
            self.violations.insert("protected-type-alias".to_owned());
        }
        visit::visit_item_type(self, item);
    }

    fn visit_impl_item_type(&mut self, item: &syn::ImplItemType) {
        if PROTECTED_TYPES
            .iter()
            .any(|target| type_contains_resolved_name(&item.ty, target, self.aliases))
        {
            self.violations.insert("protected-type-alias".to_owned());
        }
        visit::visit_impl_item_type(self, item);
    }

    fn visit_trait_item_type(&mut self, item: &syn::TraitItemType) {
        if item.default.as_ref().is_some_and(|(_, ty)| {
            PROTECTED_TYPES
                .iter()
                .any(|target| type_contains_resolved_name(ty, target, self.aliases))
        }) {
            self.violations.insert("protected-type-alias".to_owned());
        }
        visit::visit_trait_item_type(self, item);
    }

    fn visit_item_use(&mut self, item: &syn::ItemUse) {
        if self.protected_scope && use_may_shadow_reviewed_macro(&item.tree) {
            self.violations.insert("protected-type-in-macro".to_owned());
        }
        visit::visit_item_use(self, item);
    }

    fn visit_item_macro(&mut self, item: &syn::ItemMacro) {
        let assertion_definition = self.assertion_file
            && item.ident.as_ref().is_some_and(|identifier| {
                identifier == "assert_not_implemented"
                    || identifier == "assert_affine_proof_carrier"
            });
        if assertion_definition {
            return;
        }
        if item.ident.is_some() && self.protected_scope {
            self.violations.insert("protected-type-in-macro".to_owned());
        }
        visit::visit_item_macro(self, item);
    }

    fn visit_macro(&mut self, item: &syn::Macro) {
        let assertion = self.assertion_file && item.path.is_ident("assert_affine_proof_carrier");
        let ordinary = ["format", "matches", "vec", "write"]
            .iter()
            .any(|name| item.path.is_ident(name));
        if (!assertion && macro_contains_resolved_identifier(item, self.aliases))
            || (self.protected_scope && !ordinary && !assertion)
        {
            self.violations.insert("protected-type-in-macro".to_owned());
        }
        visit::visit_macro(self, item);
    }
}

fn check_protected_surface(
    file: &syn::File,
    assertion_file: bool,
    protected_scope: bool,
    aliases: &BTreeMap<String, String>,
    violations: &mut BTreeSet<String>,
) {
    ProtectedSurface {
        assertion_file,
        protected_scope,
        aliases,
        violations,
    }
    .visit_file(file);
}

fn resolved_type_aliases(
    sources: &[(std::path::PathBuf, syn::File)],
    roots: &[&str],
    violations: &mut BTreeSet<String>,
) -> BTreeMap<String, String> {
    struct AliasEdges {
        edges: Vec<(String, String)>,
    }

    impl Visit<'_> for AliasEdges {
        fn visit_item_mod(&mut self, item: &syn::ItemMod) {
            if super::attributes_require_test(&item.attrs) {
                return;
            }
            visit::visit_item_mod(self, item);
        }

        fn visit_item_use(&mut self, item: &syn::ItemUse) {
            collect_use_alias_edges(&item.tree, &mut self.edges);
            visit::visit_item_use(self, item);
        }

        fn visit_item_type(&mut self, item: &syn::ItemType) {
            if let Some(source) = type_last_name(&item.ty) {
                self.edges.push((source, item.ident.to_string()));
            }
            visit::visit_item_type(self, item);
        }

        fn visit_impl_item_type(&mut self, item: &syn::ImplItemType) {
            if let Some(source) = type_last_name(&item.ty) {
                self.edges.push((source, item.ident.to_string()));
            }
            visit::visit_impl_item_type(self, item);
        }

        fn visit_trait_item_type(&mut self, item: &syn::TraitItemType) {
            if let Some(source) = item.default.as_ref().and_then(|(_, ty)| type_last_name(ty)) {
                self.edges.push((source, item.ident.to_string()));
            }
            visit::visit_trait_item_type(self, item);
        }
    }

    let mut collector = AliasEdges { edges: Vec::new() };
    for (_, file) in sources {
        collector.visit_file(file);
    }
    let mut resolved = roots
        .iter()
        .copied()
        .map(|name| (name.to_owned(), name.to_owned()))
        .collect::<BTreeMap<_, _>>();
    loop {
        let mut changed = false;
        for (source, alias) in &collector.edges {
            let Some(canonical) = resolved.get(source).cloned() else {
                continue;
            };
            match resolved.get(alias) {
                Some(existing) if existing != &canonical => {
                    violations.insert("protected-alias-is-ambiguous".to_owned());
                }
                Some(_) => {}
                None => {
                    resolved.insert(alias.clone(), canonical);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    resolved
}

fn collect_use_alias_edges(tree: &syn::UseTree, edges: &mut Vec<(String, String)>) {
    match tree {
        syn::UseTree::Rename(rename) if rename.rename != "_" => {
            edges.push((rename.ident.to_string(), rename.rename.to_string()));
        }
        syn::UseTree::Path(path) => collect_use_alias_edges(&path.tree, edges),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_alias_edges(item, edges);
            }
        }
        syn::UseTree::Name(_) | syn::UseTree::Rename(_) | syn::UseTree::Glob(_) => {}
    }
}

fn protected_module_roots(sources: &[(std::path::PathBuf, syn::File)]) -> Vec<std::path::PathBuf> {
    sources
        .iter()
        .filter(|(_, file)| {
            file.items.iter().any(|item| {
                matches!(item, syn::Item::Struct(item)
                    if PROTECTED_TYPES.contains(&item.ident.to_string().as_str()))
            })
        })
        .filter_map(|(path, _)| {
            if path.file_name().is_some_and(|name| name == "mod.rs") {
                path.parent().map(Path::to_path_buf)
            } else {
                Some(path.with_extension(""))
            }
        })
        .collect()
}

fn check_crate_wide_census(sources: &Sources, violations: &mut BTreeSet<String>) {
    let root = workspace_root();
    let mut counts = BTreeMap::from([
        (C60_RECEIPT, 0usize),
        (C61_RECEIPT, 0usize),
        (PREPARED_CONTEXT, 0usize),
        (C60_ROOT, 0usize),
        (CHECKED_ROOT, 0usize),
        (LOWERED_ROOT, 0usize),
    ]);
    let mut aliases = 0usize;
    let mut source_files =
        production_rust_sources(&root.join("crates").join("rumoca-phase-solve"), &root)
            .into_iter()
            .map(|(path, disk_source)| {
                let source = if path.ends_with(Path::new(MODEL_VALUES_PATH)) {
                    sources.model_values.clone()
                } else if path.ends_with(Path::new(CHECKER_PATH)) {
                    sources.checker.clone()
                } else {
                    disk_source
                };
                (path, source)
            })
            .collect::<Vec<_>>();
    source_files.extend(
        sources
            .additional_phase_sources
            .iter()
            .map(|(path, source)| (Path::new(path).to_path_buf(), source.clone())),
    );
    let parsed = source_files
        .into_iter()
        .map(|(path, source)| {
            let file = parse(&source, &path.display().to_string());
            (path, file)
        })
        .collect::<Vec<_>>();
    let resolved_aliases = resolved_type_aliases(&parsed, &PROTECTED_TYPES, violations);
    root_projection::check(&parsed, violations);
    let protected_roots = protected_module_roots(&parsed);

    for (path, file) in &parsed {
        let owns_protected = file.items.iter().any(|item| {
            matches!(item, syn::Item::Struct(item)
                if PROTECTED_TYPES.contains(&item.ident.to_string().as_str()))
        });
        let protected_scope = owns_protected
            || protected_roots
                .iter()
                .any(|module_root| path.starts_with(module_root));
        check_protected_surface(
            file,
            path.ends_with(Path::new(TRAIT_ASSERTIONS_PATH)),
            protected_scope,
            &resolved_aliases,
            violations,
        );
        let mut census = LiteralCensus::new(PROTECTED_TYPES, &resolved_aliases);
        census.visit_file(file);
        for (name, count) in census.counts {
            *counts.get_mut(name.as_str()).expect("catalogued literal") += count;
        }
        for owner in PROTECTED_TYPES {
            *counts.get_mut(owner).expect("catalogued owner") +=
                self_literal_count_in_impls(file, owner, &resolved_aliases);
        }
        aliases += file
            .items
            .iter()
            .filter(|item| {
                matches!(item, syn::Item::Type(item)
                    if PROTECTED_TYPES
                        .iter().any(|target| type_contains_name(&item.ty, target)))
            })
            .count();
    }
    for (owner, count) in counts {
        if count != 1 {
            violations.insert(if owner == C61_RECEIPT {
                "c61-receipt-mint-census".to_owned()
            } else {
                format!("aggregate-mint-census:{owner}")
            });
        }
    }
    if aliases != 0 {
        violations.insert("protected-type-alias".to_owned());
    }
}

fn check_consumers(sources: &Sources, violations: &mut BTreeSet<String>) {
    for (label, source) in [
        (SIM_ENTRY_PATH, &sources.sim_entry),
        (SIM_FMI_PATH, &sources.sim_fmi),
        (COMPILE_RENDERING_PATH, &sources.compile_rendering),
    ] {
        let file = parse(source, label);
        if file_call_count(&file, "lower_solve_model") == 0
            || file_call_count(&file, "lower_prepared_solve_package") != 0
            || qualified_file_call_count(&file, "SolveModel", "construct") != 0
        {
            violations.insert(format!("consumer-bypasses-checked-lowering:{label}"));
        }
    }
    let diagnostics = parse(&sources.sim_diagnostics, SIM_DIAGNOSTICS_PATH);
    let typed = diagnostics.items.iter().any(|item| {
        matches!(item, syn::Item::Enum(item) if item.variants.iter().any(|variant|
            variant.ident == "ScalarConstantDerivativeRefinement"
                && variant.fields.iter().any(|field|
                    type_contains_name(&field.ty, "ScalarConstantDerivativeMismatch"))))
    });
    if !typed {
        violations.insert("simulation-refusal-not-typed".to_owned());
    }
    let entry = parse(&sources.sim_entry, SIM_ENTRY_PATH);
    if !find_function(&entry, "model_lowering_error").is_some_and(|function| {
        match_has_variant_without_wildcard(&function.block, "ScalarConstantDerivativeRefinement")
    }) {
        violations.insert("simulation-refusal-can-be-swallowed".to_owned());
    }
}

fn find_struct<'a>(file: &'a syn::File, name: &str) -> Option<&'a syn::ItemStruct> {
    file.items.iter().find_map(|item| match item {
        syn::Item::Struct(item) if item.ident == name => Some(item),
        _ => None,
    })
}

fn find_function<'a>(file: &'a syn::File, name: &str) -> Option<&'a ItemFn> {
    file.items.iter().find_map(|item| match item {
        syn::Item::Fn(item) if item.sig.ident == name => Some(item),
        _ => None,
    })
}

fn find_method<'a>(file: &'a syn::File, owner: &str, name: &str) -> Option<&'a ImplItemFn> {
    file.items.iter().find_map(|item| {
        let syn::Item::Impl(item) = item else {
            return None;
        };
        if type_last_name(&item.self_ty).as_deref() != Some(owner) {
            return None;
        }
        item.items.iter().find_map(|member| match member {
            syn::ImplItem::Fn(method) if method.sig.ident == name => Some(method),
            _ => None,
        })
    })
}

fn takes_only_value_self(method: &ImplItemFn) -> bool {
    method.sig.inputs.len() == 1
        && matches!(method.sig.inputs.first(), Some(FnArg::Receiver(receiver))
            if receiver.reference.is_none() && receiver.colon_token.is_none())
}

fn takes_named_values(inputs: &Punctuated<FnArg, Token![,]>, expected: &[(&str, &str)]) -> bool {
    inputs.len() == expected.len()
        && inputs.iter().zip(expected).all(|(argument, (name, ty))| {
            matches!(argument, FnArg::Typed(argument)
                if pattern_is_immutable_binding(&argument.pat, name)
                    && !matches!(argument.ty.as_ref(), Type::Reference(_))
                    && type_contains_name(&argument.ty, ty))
        })
}

fn takes_named_references(
    inputs: &Punctuated<FnArg, Token![,]>,
    expected: &[(&str, &str)],
) -> bool {
    inputs.len() == expected.len()
        && inputs.iter().zip(expected).all(|(argument, (name, ty))| {
            matches!(argument, FnArg::Typed(argument)
                if pattern_is_immutable_binding(&argument.pat, name)
                    && matches!(argument.ty.as_ref(), Type::Reference(reference)
                        if type_contains_name(&reference.elem, ty)))
        })
}

fn pattern_is_immutable_binding(pattern: &Pat, expected: &str) -> bool {
    matches!(pattern, Pat::Ident(binding)
        if binding.ident == expected
            && binding.by_ref.is_none()
            && binding.mutability.is_none()
            && binding.subpat.is_none())
}

fn takes_prepared_context_inputs(inputs: &Punctuated<FnArg, Token![,]>) -> bool {
    let mut inputs = inputs.iter();
    let prepared = inputs.next().is_some_and(|argument| {
        matches!(argument, FnArg::Typed(argument)
            if pattern_is_immutable_binding(&argument.pat, "prepared")
                && !matches!(argument.ty.as_ref(), Type::Reference(_))
                && type_contains_name(&argument.ty, "PreparedDae"))
    });
    let overrides = inputs.next().is_some_and(|argument| {
        matches!(argument, FnArg::Typed(argument)
            if pattern_is_immutable_binding(&argument.pat, "overrides")
                && matches!(argument.ty.as_ref(), Type::Reference(reference)
                    if type_contains_name(&reference.elem, "HashMap")))
    });
    prepared && overrides && inputs.next().is_none()
}

fn local_initializer<'a>(block: &'a syn::Block, name: &str) -> Option<&'a Expr> {
    block.stmts.iter().find_map(|statement| match statement {
        syn::Stmt::Local(local)
            if matches!(&local.pat, Pat::Ident(ident) if ident.ident == name) =>
        {
            local.init.as_ref().map(|init| init.expr.as_ref())
        }
        _ => None,
    })
}

fn tail_expression(block: &syn::Block) -> Option<&Expr> {
    match block.stmts.last()? {
        syn::Stmt::Expr(expression, None) => Some(expression),
        _ => None,
    }
}

fn call_expression_has_idents(expression: &Expr, target: &str, arguments: &[&str]) -> bool {
    let Expr::Call(call) = strip_wrappers(expression) else {
        return false;
    };
    matches!(call.func.as_ref(), Expr::Path(path)
        if path.path.segments.last().is_some_and(|segment| segment.ident == target))
        && call.args.len() == arguments.len()
        && call
            .args
            .iter()
            .zip(arguments)
            .all(|(argument, expected)| expression_ident(argument).as_deref() == Some(expected))
}

fn ok_wraps_struct_literal(expression: &Expr, target: &str) -> bool {
    let Expr::Call(call) = strip_wrappers(expression) else {
        return false;
    };
    if !matches!(call.func.as_ref(), Expr::Path(path) if path.path.is_ident("Ok"))
        || call.args.len() != 1
    {
        return false;
    }
    matches!(call.args.first(), Some(Expr::Struct(literal))
        if literal.path.segments.last().is_some_and(|segment| segment.ident == target))
}

fn macro_contains_resolved_identifier(
    item: &syn::Macro,
    aliases: &BTreeMap<String, String>,
) -> bool {
    fn contains(stream: proc_macro2::TokenStream, aliases: &BTreeMap<String, String>) -> bool {
        stream.into_iter().any(|token| match token {
            proc_macro2::TokenTree::Ident(identifier) => {
                aliases.contains_key(&identifier.to_string())
            }
            proc_macro2::TokenTree::Group(group) => contains(group.stream(), aliases),
            proc_macro2::TokenTree::Punct(_) | proc_macro2::TokenTree::Literal(_) => false,
        })
    }

    contains(item.tokens.clone(), aliases)
}

fn qualified_call_has_idents(
    expression: &Expr,
    qualifier: &str,
    function: &str,
    arguments: &[&str],
) -> bool {
    let Expr::Call(call) = strip_wrappers(expression) else {
        return false;
    };
    let Expr::Path(path) = call.func.as_ref() else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == function)
        && path
            .path
            .segments
            .iter()
            .rev()
            .nth(1)
            .is_some_and(|segment| segment.ident == qualifier)
        && call.args.len() == arguments.len()
        && call
            .args
            .iter()
            .zip(arguments)
            .all(|(argument, expected)| expression_ident(argument).as_deref() == Some(expected))
}

fn checked_root_chain_is_direct(expression: &Expr) -> bool {
    let Expr::MethodCall(method) = strip_wrappers(expression) else {
        return false;
    };
    method.method == "into_equation_refined"
        && method.args.is_empty()
        && match strip_wrappers(&method.receiver) {
            Expr::Call(call) => {
                let Expr::Path(path) = call.func.as_ref() else {
                    return false;
                };
                path.path
                    .segments
                    .last()
                    .is_some_and(|segment| segment.ident == "construct")
                    && path
                        .path
                        .segments
                        .iter()
                        .rev()
                        .nth(1)
                        .is_some_and(|segment| segment.ident == C60_ROOT)
                    && call.args.len() == 3
                    && expression_ident(&call.args[0]).as_deref() == Some("context")
                    && expression_ident(&call.args[1]).as_deref() == Some("solve_model")
            }
            _ => false,
        }
}

fn prepared_profile_admission_is_direct(expression: &Expr) -> bool {
    let Expr::MethodCall(inspect) = strip_wrappers(expression) else {
        return false;
    };
    if inspect.method != "inspect" || inspect.args.len() != 1 {
        return false;
    }
    let Expr::MethodCall(as_dae) = strip_wrappers(&inspect.receiver) else {
        return false;
    };
    if as_dae.method != "as_dae"
        || !as_dae.args.is_empty()
        || expression_ident(&as_dae.receiver).as_deref() != Some("prepared")
    {
        return false;
    }
    let Some(Expr::Closure(closure)) = inspect.args.first() else {
        return false;
    };
    closure.inputs.len() == 1
        && matches!(closure.inputs.first(), Some(Pat::Ident(binding))
            if binding.ident == "view"
                && binding.by_ref.is_none()
                && binding.mutability.is_none()
                && binding.subpat.is_none())
        && call_expression_has_idents(
            &closure.body,
            "admit_scalar_constant_derivative_profile",
            &["view", "overrides"],
        )
}

fn call_has_ident_arguments(block: &syn::Block, target: &str, expected: &[&str]) -> bool {
    let mut visitor = CallShapeVisitor {
        target,
        expected,
        matched: false,
    };
    visitor.visit_block(block);
    visitor.matched
}

struct CallShapeVisitor<'name> {
    target: &'name str,
    expected: &'name [&'name str],
    matched: bool,
}

impl<'ast> Visit<'ast> for CallShapeVisitor<'_> {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        let named = matches!(call.func.as_ref(), Expr::Path(path)
            if path.path.segments.last().is_some_and(|segment| segment.ident == self.target));
        if named
            && call.args.len() == self.expected.len()
            && call
                .args
                .iter()
                .zip(self.expected)
                .all(|(argument, expected)| expression_ident(argument).as_deref() == Some(expected))
        {
            self.matched = true;
        }
        visit::visit_expr_call(self, call);
    }
}

fn has_struct_destructure(
    block: &syn::Block,
    owner: &str,
    initializer: &str,
    expected_fields: &[&str],
) -> bool {
    block.stmts.iter().any(|statement| {
        let syn::Stmt::Local(local) = statement else {
            return false;
        };
        let Pat::Struct(pattern) = &local.pat else {
            return false;
        };
        let Some(init) = &local.init else {
            return false;
        };
        pattern
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == owner)
            && expression_ident(&init.expr).as_deref() == Some(initializer)
            && pattern.fields.len() == expected_fields.len()
            && expected_fields.iter().all(|expected| {
                pattern.fields.iter().any(|field| {
                    matches!(&field.member, syn::Member::Named(name) if name == expected)
                        && matches!(field.pat.as_ref(), Pat::Ident(ident)
                            if ident.ident == expected
                                && ident.by_ref.is_none()
                                && ident.mutability.is_none()
                                && ident.subpat.is_none())
                })
            })
    })
}

fn c61_refinement_initializer_is_direct(expression: &Expr) -> bool {
    let Expr::Match(expression) = strip_wrappers(expression) else {
        return false;
    };
    if expression_ident(&expression.expr).as_deref() != Some("scalar_constant_derivative_profile")
        || expression.arms.len() != 2
    {
        return false;
    }
    let mut admitted = false;
    let mut unsupported = false;
    for arm in &expression.arms {
        if arm.guard.is_some() || !arm.attrs.is_empty() {
            return false;
        }
        if pattern_is_single_tuple_binding(&arm.pat, "Ok", "profile") {
            admitted = direct_checked_receipt_result(&arm.body);
        } else if pattern_is_single_tuple_binding(&arm.pat, "Err", "unsupported") {
            unsupported = direct_result_binding(&arm.body, "Err", "unsupported");
        } else {
            return false;
        }
    }
    admitted && unsupported
}

fn direct_checked_receipt_result(expression: &Expr) -> bool {
    let Expr::Call(ok) = strip_wrappers(expression) else {
        return false;
    };
    if !matches!(ok.func.as_ref(), Expr::Path(path) if path.path.is_ident("Ok"))
        || ok.args.len() != 1
    {
        return false;
    }
    let Some(Expr::Try(checked)) = ok.args.first() else {
        return false;
    };
    let Expr::MethodCall(mapped) = strip_wrappers(&checked.expr) else {
        return false;
    };
    mapped.method == "map_err"
        && mapped.args.len() == 1
        && call_expression_has_idents(
            &mapped.receiver,
            "check_scalar_constant_derivative_facts",
            &["profile", "model"],
        )
}

fn direct_result_binding(expression: &Expr, constructor: &str, binding: &str) -> bool {
    let Expr::Call(call) = strip_wrappers(expression) else {
        return false;
    };
    matches!(call.func.as_ref(), Expr::Path(path) if path.path.is_ident(constructor))
        && call.args.len() == 1
        && call.args.first().and_then(expression_ident).as_deref() == Some(binding)
}

fn replace_once(source: &str, old: &str, new: &str) -> String {
    assert_eq!(source.matches(old).count(), 1, "mutation anchor `{old}`");
    source.replacen(old, new, 1)
}

fn assert_violation(sources: &Sources, expected: &str) {
    let violations = boundary_violations(sources);
    assert!(
        violations.contains(expected),
        "mutation did not trigger `{expected}`: {violations:#?}"
    );
}
