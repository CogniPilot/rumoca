use std::fs;

use super::architecture_hardening_support::workspace_root;

#[test]
fn canonical_dae_is_non_cloneable_and_shared_at_session_boundaries() {
    let root = workspace_root();
    let model = read(&root, "crates/rumoca-ir-dae/src/model.rs");
    let dae_declaration = declaration_prefix(&model, "pub struct Dae");
    assert!(
        !dae_declaration.contains("Clone"),
        "canonical Dae must not derive Clone; session boundaries share Arc<Dae>"
    );
    assert!(
        !model.contains("Clone for Dae"),
        "canonical Dae must not implement Clone"
    );
    assert!(
        !declaration_prefix(&model, "struct FrozenStorage").contains("Clone"),
        "frozen DAE arenas must not regain an internal deep-copy capability"
    );

    let session = read(&root, "crates/rumoca-compile/src/session.rs");
    let result = declaration_body(&session, "pub struct CompilationResult");
    assert!(
        result.contains("pub dae: Arc<dae::Dae>"),
        "full compilation results must share their one checked DAE root"
    );

    let facade = read(&root, "crates/rumoca/src/compiler.rs");
    let facade_result = declaration_body(&facade, "pub struct CompilationResult");
    assert!(
        facade_result.contains("pub dae: Arc<Dae>"),
        "the public compiler facade must expose checked DAE sharing explicitly"
    );
    let constructor = declaration_body(&facade, "pub fn new(");
    assert!(
        constructor.contains("dae: Arc<Dae>"),
        "the compiler facade constructor must accept the existing shared root"
    );
}

#[test]
fn rendering_borrows_the_checked_dae_without_copy_adapters() {
    let root = workspace_root();
    let renderer = read(
        &root,
        "crates/rumoca-phase-codegen/src/codegen/solve_renderer.rs",
    );
    assert!(
        renderer.contains("dae_model: &dae::Dae"),
        "Solve template projection must borrow the checked DAE"
    );

    for prohibited in [
        "new_owned_with_shared_dae",
        "Arc::new(dae_model)",
        "dae_model.clone()",
    ] {
        assert!(
            !renderer.contains(prohibited),
            "Solve template renderer retains obsolete DAE copy path `{prohibited}`"
        );
    }

    let compile_support = read(
        &root,
        "crates/rumoca-compile/src/session/compile_support.rs",
    );
    assert!(
        !compile_support.contains("unwrap_or_clone(artifact.dae)"),
        "session result assembly must share its cached DAE instead of copying it"
    );
}

#[test]
fn galec_issues_one_causal_definition_proof_per_entry_operation() {
    let root = workspace_root();
    let galec = root.join("crates/rumoca-phase-galec/src");
    let admissibility = read(&root, "crates/rumoca-phase-galec/src/admissibility.rs");
    let lowering = read(&root, "crates/rumoca-phase-galec/src/lower.rs");
    let derivation = "CausalDefinitions::derive(view)";
    assert_eq!(
        admissibility.matches(derivation).count(),
        1,
        "the standalone admissibility entry must issue exactly one causal-definition proof"
    );
    assert_eq!(
        lowering.matches(derivation).count(),
        1,
        "the GALEC lowering entry must issue exactly one causal-definition proof"
    );

    for relative in [
        "lower/causal_outputs.rs",
        "lower/clock_schedule.rs",
        "lower/clocked_assignments.rs",
        "lower/user_functions.rs",
    ] {
        let source = fs::read_to_string(galec.join(relative))
            .unwrap_or_else(|error| panic!("read {relative}: {error}"));
        assert!(
            !source.contains(derivation),
            "{relative} must borrow the entry-issued causal-definition proof"
        );
    }
}

#[test]
fn solve_consumes_the_structural_analysis_issued_by_preparation() {
    let root = workspace_root();
    let preparation = read(&root, "crates/rumoca-phase-structural/src/dae_transform.rs");
    assert_eq!(
        preparation.matches("sort(view)").count(),
        1,
        "structural preparation must route every whole-model sort through its one analysis issuer"
    );
    assert!(
        preparation.contains("structural: structural.bind(view)"),
        "PreparedSystem must rebrand the analysis coupled to its prepared DAE"
    );

    let solve_lowering = read(&root, "crates/rumoca-phase-solve/src/lower.rs");
    for prohibited in ["rumoca_phase_structural::sort", "structural::sort("] {
        assert!(
            !solve_lowering.contains(prohibited),
            "Solve lowering must consume PreparedSystem structural analysis, not call `{prohibited}`"
        );
    }
}

#[test]
fn tensor_address_arithmetic_has_one_foundation_owner() {
    let root = workspace_root();
    let owner = read(&root, "crates/rumoca-core/src/structured_domain.rs");
    for helper in [
        "fn row_major_coordinates(",
        "fn flatten_coordinates(",
        "fn checked_product(",
    ] {
        assert_eq!(
            owner.matches(helper).count(),
            1,
            "rumoca-core must contain exactly one `{helper}` owner"
        );
        let mut duplicates = Vec::new();
        let mut files = Vec::new();
        super::architecture_hardening_support::collect_rs_files(&root.join("crates"), &mut files);
        for path in files {
            if path.ends_with("crates/rumoca-core/src/structured_domain.rs")
                || path
                    .ends_with("crates/rumoca/tests/architecture_hardening_test/dae_ownership.rs")
            {
                continue;
            }
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            if source.contains(helper) {
                duplicates.push(path.display().to_string());
            }
        }
        assert!(
            duplicates.is_empty(),
            "tensor arithmetic helper `{helper}` is duplicated in {duplicates:?}"
        );
    }
}

fn read(root: &std::path::Path, relative: &str) -> String {
    fs::read_to_string(root.join(relative))
        .unwrap_or_else(|error| panic!("read {relative}: {error}"))
        .replace("\r\n", "\n")
}

fn declaration_prefix<'a>(source: &'a str, declaration: &str) -> &'a str {
    let end = source
        .find(declaration)
        .unwrap_or_else(|| panic!("missing declaration `{declaration}`"));
    let start = source[..end].rfind("\n\n").map_or(0, |index| index + 2);
    &source[start..end]
}

fn declaration_body<'a>(source: &'a str, declaration: &str) -> &'a str {
    let start = source
        .find(declaration)
        .unwrap_or_else(|| panic!("missing declaration `{declaration}`"));
    let end = source[start..]
        .find("\n}")
        .map_or(source.len(), |offset| start + offset);
    &source[start..end]
}
