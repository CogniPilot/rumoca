//! Mint semantics of the `TypedInstancedTree` proof (SPEC_0036, SPEC_0029 §4):
//! success seals the finalized overlay inside one affine artifact, failure
//! exposes no overlay at all, and the read-only projection shares the exact
//! immutable payload without carrying the phase capability.

use super::*;

fn instantiate_fixture(
    source: &str,
    model_name: &str,
) -> (rumoca_phase_resolve::ResolvedTree, InstanceOverlay) {
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("fixture resolves");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    (resolved, overlay)
}

#[test]
fn successful_mint_seals_the_finalized_overlay_and_model_name() {
    let (resolved, overlay) = instantiate_fixture(
        "model Test\n  parameter Integer n = 2;\n  Real x[n];\nequation\n  der(x) = -x;\nend Test;\n",
        "Test",
    );
    let typed = crate::typecheck_instanced_tree(&resolved, overlay, "Test")
        .expect("valid model mints its proof");
    assert_eq!(typed.model_name(), "Test");
    let x = typed
        .overlay()
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "x")
        .expect("the sealed overlay retains the instanced component");
    assert_eq!(x.dims, vec![2], "the sealed overlay is the finalized one");
}

#[test]
fn successful_mint_retains_the_exact_resolve_root() {
    let (resolved_a, overlay_a) = instantiate_fixture(
        "model RootA\n  Real x;\nequation\n  der(x) = -x;\nend RootA;\n",
        "RootA",
    );
    let (resolved_b, _) = instantiate_fixture(
        "model RootB\n  Real x;\nequation\n  der(x) = -x;\nend RootB;\n",
        "RootB",
    );
    let typed = crate::typecheck_instanced_tree(&resolved_a, overlay_a, "RootA")
        .expect("valid model mints its proof");

    assert!(
        typed
            .resolved_tree()
            .definitions
            .classes
            .contains_key("RootA")
    );
    assert!(
        !typed
            .resolved_tree()
            .definitions
            .classes
            .contains_key("RootB")
    );
    assert!(resolved_b.definitions.classes.contains_key("RootB"));
    assert!(
        std::ptr::eq(typed.resolved_tree(), resolved_a.inner()),
        "the proof retains the exact shared Resolve allocation"
    );
    assert!(!std::ptr::eq(typed.resolved_tree(), resolved_b.inner()));
}

#[test]
fn projection_shares_the_exact_sealed_payload_read_only() {
    let (resolved, overlay) = instantiate_fixture(
        "model Test\n  Real x;\nequation\n  der(x) = -x;\nend Test;\n",
        "Test",
    );
    let typed = crate::typecheck_instanced_tree(&resolved, overlay, "Test")
        .expect("valid model mints its proof");
    let projection = typed.shared_projection();
    // The projection shares the exact allocation rather than a copy.
    assert!(std::ptr::eq(typed.overlay(), projection.overlay()));
    assert_eq!(projection.model_name(), typed.model_name());
    // Dropping the proof leaves the shared payload readable through the
    // projection; the phase capability is gone with the proof.
    drop(typed);
    assert_eq!(projection.model_name(), "Test");
    assert!(!projection.overlay().components.is_empty());
}

#[test]
fn refusing_mint_returns_diagnostics_and_no_artifact() {
    let (resolved, overlay) = instantiate_fixture(
        "model Test\n  Real x(startd = 1.0);\nequation\n  der(x) = -x;\nend Test;\n",
        "Test",
    );
    let diagnostics = crate::typecheck_instanced_tree(&resolved, overlay, "Test")
        .expect_err("an unknown modifier target must refuse the mint");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET001")),
        "the refusal keeps its typed diagnostic: {diagnostics:?}"
    );
}
