//! Enforce the DAE-C02 checked-derivative lowering boundary.

use std::collections::BTreeSet;
use std::fs;

use super::architecture_hardening_support::workspace_root;

const LOWERING_PATH: &str = "crates/rumoca-phase-dae/src/construction/expression/temporal.rs";
const ANALYSIS_PATH: &str = "crates/rumoca-phase-dae/src/construction/analysis/derivatives.rs";

#[test]
fn derivative_analysis_uses_the_complete_model_owner_traversal() {
    let source = read(ANALYSIS_PATH);
    assert!(
        source.contains("analyzer.visit_model_owners(owners)?;"),
        "DAE-C02 certificates must be issued through ModelExpressionOwnerVisitor"
    );

    let equation_only = source.replacen("visit_model_owners", "visit_equation_owners", 1);
    assert!(
        !equation_only.contains("analyzer.visit_model_owners(owners)?;"),
        "the owner-traversal mutation fixture must remove the complete traversal"
    );
}

#[test]
fn derivative_lowering_consumes_the_checked_occurrence_certificate() {
    let source = lowering_source();
    let violations = boundary_violations(&source);
    assert!(
        violations.is_empty(),
        "DAE-C02 derivative lowering boundary drifted: {:#?}",
        violations
    );
}

#[test]
fn mutations_detect_certificate_bypass_and_untyped_coordinate_lookup() {
    let source = lowering_source();

    let without_certificate = source.replacen(".derivatives", ".history_operators", 1);
    assert!(boundary_violations(&without_certificate).contains("certificate-not-consumed"));

    let without_typed_state = source.replacen(".state_occurrences", ".coordinate_instances", 1);
    assert!(boundary_violations(&without_typed_state).contains("typed-state-map-not-consumed"));

    let with_name_lookup = source.replacen(
        "let missing =",
        "let _forbidden = symbols.coordinates;\n    let missing =",
        1,
    );
    assert!(boundary_violations(&with_name_lookup).contains("name-based-coordinate-lookup"));

    let with_proof_panic = source.replacen(
        "let missing =",
        "let _forbidden = Some(()).expect(\"proof\");\n    let missing =",
        1,
    );
    assert!(boundary_violations(&with_proof_panic).contains("proof-panic"));
}

fn lowering_source() -> String {
    read(LOWERING_PATH)
}

fn read(relative: &str) -> String {
    let path = workspace_root().join(relative);
    fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

fn boundary_violations(source: &str) -> BTreeSet<&'static str> {
    let body = function_body(
        source,
        "pub(super) fn lower_derivative",
        "pub(super) fn lower_pre",
    );
    let mut violations = BTreeSet::new();
    if !body.contains(".derivatives") || !body.contains(".certificate(") {
        violations.insert("certificate-not-consumed");
    }
    if !body.contains(".state_occurrences")
        || !body.contains("dae::CoordinateInput::Derivative(state)")
    {
        violations.insert("typed-state-map-not-consumed");
    }
    if body.contains("symbols.coordinates")
        || body.contains(".coordinate_instances")
        || body.contains(".var_name()")
        || body.contains(".derivative(")
    {
        violations.insert("name-based-coordinate-lookup");
    }
    if body.contains("expect(") || body.contains("unwrap(") {
        violations.insert("proof-panic");
    }
    violations
}

fn function_body<'source>(source: &'source str, start: &str, end: &str) -> &'source str {
    let start = source
        .find(start)
        .unwrap_or_else(|| panic!("missing derivative lowering entry in {LOWERING_PATH}"));
    let tail = &source[start..];
    let end = tail
        .find(end)
        .unwrap_or_else(|| panic!("missing derivative lowering boundary in {LOWERING_PATH}"));
    &tail[..end]
}
