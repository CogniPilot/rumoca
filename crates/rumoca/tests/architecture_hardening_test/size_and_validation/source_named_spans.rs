//! Source-named span fixture provenance gates.

use super::super::*;
use super::helpers::*;

/// SPEC_0008: generated `connect()` flat equations must carry the originating
/// `connect()` source span, not `Span::DUMMY`. The `ConnectionSet` type must
/// carry a `span` field and the connection-derived equation-generation
/// functions (`generate_equality_equations` and `generate_flow_equation`)
/// must pass it through. Unconnected-flow defaults (`flow = 0` for flow
/// variables never mentioned in a connect()) are true compiler-generated
/// constructs with no source span, so they are allowed to use `Span::DUMMY`
/// per SPEC_0008's "compiler-generated constructs" exception.
#[test]
fn test_connection_equations_carry_real_spans() {
    let root = workspace_root();
    let connections_mod =
        fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/connections/mod.rs"))
            .expect("read connections/mod.rs");
    assert!(
        connections_mod.contains("span: rumoca_core::Span"),
        "ConnectionSet must carry a `span` field so generated equations can \
preserve the originating connect() span (SPEC_0008)"
    );

    let eq_gen = fs::read_to_string(
        root.join("crates/rumoca-phase-flatten/src/connections/equation_generation.rs"),
    )
    .expect("read equation_generation.rs");

    // Scope the gate to the two connection-derived generator functions.
    let mut offenders = Vec::new();
    let mut current_fn: Option<&str> = None;
    let connection_derived_fns = ["generate_equality_equations", "generate_flow_equation"];
    for (idx, line) in eq_gen.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed
            .strip_prefix("pub(super) fn ")
            .or_else(|| trimmed.strip_prefix("fn "))
            .or_else(|| trimmed.strip_prefix("pub fn "))
        {
            let name = fn_name.split('(').next().unwrap_or("").trim();
            current_fn = connection_derived_fns
                .iter()
                .copied()
                .find(|&candidate| candidate == name);
        }
        if let Some(fn_name) = current_fn
            && !trimmed.starts_with("//")
            && line.contains("Equation::new_array")
            && line.contains("Span::DUMMY")
        {
            offenders.push(format!(
                "{}:{} in fn {fn_name}",
                "equation_generation.rs",
                idx + 1
            ));
        }
    }
    assert!(
        offenders.is_empty(),
        "found Equation::new_array calls passing Span::DUMMY in connection-derived \
generators: {offenders:#?}. SPEC_0008: pass the connection set's span instead."
    );
}

#[test]
fn test_core_semantic_phase_diagnostics_do_not_reconstruct_source_zero() {
    let root = workspace_root();
    let phase_src_dirs = [
        "crates/rumoca-phase-typecheck/src",
        "crates/rumoca-phase-flatten/src",
        "crates/rumoca-phase-dae/src",
        "crates/rumoca-phase-solve/src",
    ];
    let banned = [
        "Span::from_offsets(SourceId(0)",
        "Span::from_offsets(rumoca_core::SourceId(0)",
        "Span::new(SourceId(0)",
        "Span::new(rumoca_core::SourceId(0)",
    ];

    let offenders = phase_src_dirs
        .iter()
        .flat_map(|phase_src_dir| source_zero_span_offenders(&root.join(phase_src_dir), &banned))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "semantic phase diagnostics must preserve source identity instead of reconstructing spans with SourceId(0): {offenders:?}"
    );
}

#[test]
fn test_phase_dae_fixtures_use_source_named_spans() {
    let root = workspace_root().join("crates/rumoca-phase-dae/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .flat_map(|path| numeric_source_id_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "phase-DAE tests and fixtures must use SourceId::from_source_name so \
span provenance remains traceable. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_phase_solve_fixtures_use_source_named_spans() {
    let root = workspace_root().join("crates/rumoca-phase-solve/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .flat_map(|path| numeric_source_id_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "phase-solve tests and fixtures must use SourceId::from_source_name \
so span provenance remains traceable. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_ir_solve_fixtures_use_source_named_spans() {
    let root = workspace_root().join("crates/rumoca-ir-solve/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .flat_map(|path| numeric_source_id_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "IR-solve tests and fixtures must use SourceId::from_source_name so \
span provenance remains traceable. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_solve_runtime_fixtures_use_source_named_spans() {
    let root = workspace_root();
    let source_dirs = [
        "crates/rumoca-eval-solve/src",
        "crates/rumoca-solver/src",
        "crates/rumoca-sim/src",
        "crates/rumoca-exec-cranelift/src",
    ];

    let offenders = source_dirs
        .iter()
        .flat_map(|source_dir| {
            let mut rs_files = Vec::new();
            collect_rs_files(&root.join(source_dir), &mut rs_files);
            rs_files
                .into_iter()
                .flat_map(|path| numeric_source_id_locations(&path))
        })
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "solve runtime/eval tests and fixtures must use SourceId::from_source_name \
so span provenance remains traceable. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_early_semantic_eval_fixtures_use_source_named_spans() {
    let root = workspace_root();
    let source_dirs = [
        "crates/rumoca-phase-flatten/src",
        "crates/rumoca-phase-instantiate/src",
        "crates/rumoca-eval-flat/src",
        "crates/rumoca-eval-dae/src",
    ];

    let offenders = source_dirs
        .iter()
        .flat_map(|source_dir| {
            let mut rs_files = Vec::new();
            collect_rs_files(&root.join(source_dir), &mut rs_files);
            rs_files
                .into_iter()
                .flat_map(|path| numeric_source_id_locations(&path))
        })
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "early semantic/eval tests and fixtures must use SourceId::from_source_name \
so span provenance remains traceable. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_structural_fixtures_use_source_named_spans() {
    let root = workspace_root().join("crates/rumoca-phase-structural/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .flat_map(|path| numeric_source_id_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "phase-structural tests and fixtures must use SourceId::from_source_name \
so span provenance remains traceable. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_repository_fixtures_use_source_named_spans() {
    let root = workspace_root().join("crates");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .flat_map(|path| numeric_source_id_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "tests and fixtures must use SourceId::from_source_name so span provenance \
remains traceable. Offenders: {offenders:#?}"
    );
}
