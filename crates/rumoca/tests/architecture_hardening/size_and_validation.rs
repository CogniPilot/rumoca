use super::*;

#[test]
fn test_rs_files_stay_under_spec_0021_hard_limit() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders: Vec<String> = rs_files
        .into_iter()
        .filter(|path| is_line_count_checked_source_file(path))
        .filter_map(|path| {
            let content = fs::read_to_string(&path).ok()?;
            let line_count = content.lines().count();
            let has_exception = content.contains("SPEC_0021")
                && content.contains("file-size")
                && content.contains("split plan");
            (line_count > 2000 && !has_exception)
                .then(|| format!("{} ({line_count} lines)", path.display()))
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "Rust files over the SPEC_0021 hard action threshold of 2,000 lines need \
an explicit SPEC_0021 file-size exception and split plan: {offenders:#?}"
    );
}

fn is_line_count_checked_source_file(path: &Path) -> bool {
    let rel = path
        .strip_prefix(workspace_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    !rel.contains("/generated/")
}

#[test]
fn test_ir_crates_have_no_public_scalarize_functions() {
    // SPEC_0007 keeps scalarization out of IR crates; backend/evaluator
    // fallback helpers live in rumoca-eval-solve.
    let root = workspace_root();
    let mut offenders = Vec::new();

    for path in collect_ir_crate_rs_files(&root) {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        offenders.extend(content.lines().enumerate().filter_map(|(line_idx, line)| {
            public_scalarize_function_location(&path, line_idx, line)
        }));
    }

    assert!(
        offenders.is_empty(),
        "found public scalarise/to_scalar functions in rumoca-ir-* crates (SPEC_0007 violation). \
Move scalarization logic to rumoca-eval-solve or an execution adapter crate: {offenders:#?}"
    );
}

#[test]
fn test_ast_visitor_helpers_stay_split_by_behavior_shape() {
    let root = workspace_root();
    let visitor_root = root.join("crates/rumoca-ir-ast/src/visitor");
    let facade = root.join("crates/rumoca-ir-ast/src/visitor.rs");
    let facade_content = fs::read_to_string(&facade).expect("read AST visitor facade");

    for required in ["read_only.rs", "rewrite.rs", "query.rs"] {
        assert!(
            visitor_root.join(required).exists(),
            "rumoca-ir-ast visitor helpers must stay split by behavior shape; missing {required}"
        );
    }

    assert!(
        facade_content.contains("mod read_only;")
            && facade_content.contains("mod rewrite;")
            && facade_content.contains("mod query;"),
        "AST visitor facade must route through read_only/rewrite/query modules"
    );

    let read_only = fs::read_to_string(visitor_root.join("read_only.rs"))
        .expect("read AST read-only visitor module");
    let rewrite =
        fs::read_to_string(visitor_root.join("rewrite.rs")).expect("read AST rewrite module");
    assert!(
        read_only.contains("pub trait Visitor") && !read_only.contains("transform_expression"),
        "read_only.rs must contain traversal only, without rewrite-shape transforms"
    );
    assert!(
        rewrite.contains("pub trait ExpressionTransformer")
            && !rewrite.contains("pub trait Visitor"),
        "rewrite.rs must contain rewrite-shape transforms without owning read-only traversal"
    );
}

#[test]
fn test_ast_visitor_helpers_do_not_import_phase_or_eval_semantics() {
    let root = workspace_root();
    let visitor_root = root.join("crates/rumoca-ir-ast/src/visitor");
    let mut offenders = Vec::new();

    for path in [
        root.join("crates/rumoca-ir-ast/src/visitor.rs"),
        visitor_root.join("read_only.rs"),
        visitor_root.join("rewrite.rs"),
        visitor_root.join("query.rs"),
    ] {
        let content = fs::read_to_string(&path).expect("read AST visitor source");
        for (line_idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("use rumoca_phase_") || trimmed.starts_with("use rumoca_eval_") {
                offenders.push(format!("{}:{}", path.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "AST visitor helpers are allowed to traverse/query/rewrite AST shape, \
but must not import phase or evaluator semantics: {offenders:#?}"
    );
}

#[test]
fn test_session_root_facade_exports_are_minimal() {
    let session_lib = workspace_root().join("crates/rumoca-compile/src/lib.rs");
    let content = fs::read_to_string(&session_lib).expect("read rumoca-compile lib.rs");
    let root_pub_uses = collect_root_pub_use_statements(&content);

    let expected = vec!["pub use compile::{Session, SessionConfig};".to_string()];

    assert_eq!(
        root_pub_uses, expected,
        "unexpected rumoca-compile root exports. \
Author reminder: SPEC_0029_CRATE_BOUNDARIES.md §9 requires `rumoca-compile` \
root exports to stay minimal (`Session`, `SessionConfig`) and to keep other APIs namespaced."
    );
}

/// SPEC_0029 §3a: `rumoca-core` is the sole Tier-1 foundation. No
/// `rumoca-ir-core` crate exists. Enforce that the directory has not been
/// re-created and that no Cargo.toml declares a `rumoca-ir-core` dependency.
#[test]
fn test_no_separate_ir_core_foundation_crate() {
    let root = workspace_root();

    assert!(
        !root.join("crates/rumoca-ir-core").exists(),
        "crates/rumoca-ir-core was dissolved into rumoca-core per SPEC_0029 §3a; \
do not re-create it"
    );

    let mut offenders = Vec::new();
    for entry in fs::read_dir(root.join("crates")).expect("read crates dir") {
        let entry = entry.expect("crates entry");
        let cargo = entry.path().join("Cargo.toml");
        let Ok(content) = fs::read_to_string(&cargo) else {
            continue;
        };
        for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
            if section_contains_dependency(&content, section, "rumoca-ir-core") {
                offenders.push(format!("{} [{section}]", cargo.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "found Cargo.toml entries declaring removed `rumoca-ir-core` dep: {offenders:#?}; \
SPEC_0029 §3a: depend on rumoca-core instead"
    );

    let workspace_cargo =
        fs::read_to_string(root.join("Cargo.toml")).expect("read workspace Cargo.toml");
    assert!(
        !workspace_cargo.contains("\"crates/rumoca-ir-core\""),
        "workspace Cargo.toml still references crates/rumoca-ir-core in [members]"
    );
    assert!(
        !workspace_cargo.contains("rumoca-ir-core ="),
        "workspace Cargo.toml still declares rumoca-ir-core in [workspace.dependencies]"
    );
}

/// SPEC_0029 §12 + SPEC_0007 §Structural Transformation Scope: DAE structural
/// transformations (index reduction, state demotion, BLT, tearing) live in
/// `rumoca-phase-structural`. `rumoca-phase-solve` only lowers a finalized
/// DAE to Solve-IR; it does not mutate DAE mathematical structure.
#[test]
fn test_dae_structural_transforms_live_in_phase_structural() {
    let root = workspace_root();

    let banned_symbols = [
        "index_reduce_missing_state_derivatives",
        "demote_states_without_derivative_refs",
        "demote_states_without_assignable_derivative_rows",
        "demote_orphan_states_without_equation_refs",
        "demote_states_without_retained_derivative_rows",
    ];

    let mut offenders = Vec::new();
    let phase_solve_src = root.join("crates/rumoca-phase-solve/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&phase_solve_src, &mut rs_files);
    for path in rs_files {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (line_idx, line) in content.lines().enumerate() {
            // Only flag definition sites (fn name) or pub uses; calls to these
            // symbols (if any) are legitimate downstream usage from
            // phase-structural via a public API.
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            if let Some(banned) = banned_symbols.iter().find(|name| {
                trimmed.starts_with(&format!("pub fn {name}"))
                    || trimmed.starts_with(&format!("fn {name}"))
                    || trimmed.contains("pub use ") && line.contains(*name)
            }) {
                offenders.push(format!(
                    "{}:{} defines {banned}",
                    path.display(),
                    line_idx + 1
                ));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "DAE structural transformations must live in rumoca-phase-structural \
(SPEC_0029 §12, SPEC_0007 §Structural Transformation Scope), not phase-solve: \
{offenders:#?}"
    );

    assert!(
        root.join("crates/rumoca-phase-structural/src/dae_prepare")
            .exists(),
        "phase-structural must own the dae_prepare module after relocation"
    );
}

/// SPEC_0007 Stage 3 Contract: source temporal operators are eliminated in every DAE partition.
/// The `phase-dae::appendix_b_validation::validate_no_source_temporal_operator_survives`
/// gate must exist as a positive runtime check (not just absent from compile).
#[test]
fn test_source_temporal_operator_validation_gate_exists() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/appendix_b_validation.rs");
    let content = fs::read_to_string(&path).expect("read appendix_b_validation.rs");

    assert!(
        content.contains("validate_no_source_temporal_operator_survives"),
        "phase-dae::appendix_b_validation must define validate_no_source_temporal_operator_survives \
to enforce SPEC_0007 Stage 3 Contract (no pre/edge/change/sample/previous in solver-facing DAE-IR)"
    );

    let error_path = workspace_root().join("crates/rumoca-phase-dae/src/errors.rs");
    let error_content = fs::read_to_string(&error_path).expect("read errors.rs");
    assert!(
        error_content.contains("SourceTemporalOperatorSurvivedDaeBoundary"),
        "ToDaeError must include SourceTemporalOperatorSurvivedDaeBoundary so the gate \
produces a structured diagnostic per SPEC_0008"
    );

    let pre_lowering_path = workspace_root().join("crates/rumoca-phase-dae/src/pre_lowering.rs");
    let pre_lowering_content =
        fs::read_to_string(&pre_lowering_path).expect("read pre_lowering.rs");
    for partition in [
        "dae.continuous.equations",
        "dae.discrete.real_updates",
        "dae.discrete.valued_updates",
        "dae.conditions.equations",
    ] {
        assert!(
            pre_lowering_content.contains(partition),
            "phase-dae::pre_lowering must process every DAE partition; missing {partition}"
        );
    }
}

/// SPEC_0007 Stage 4 Contract: Solve-IR is the register-machine form of
/// Appendix B and must have a positive validation gate at the lowering boundary.
#[test]
fn test_solve_ir_appendix_b_validation_gate_exists() {
    let root = workspace_root();
    let path = root.join("crates/rumoca-phase-solve/src/appendix_b_validation.rs");
    let content = fs::read_to_string(&path).expect("read phase-solve appendix_b_validation.rs");

    for required in [
        "validate_solve_input_appendix_b_invariants",
        "validate_solve_problem_appendix_b_invariants",
        "validate_solve_artifacts_appendix_b_invariants",
        "find_source_temporal_operator",
        "validate_function_calls_resolve",
        "validate_row_ops",
        "validate_compute_node",
        "SeedUse::Forbidden",
        "SeedUse::Allowed",
    ] {
        assert!(
            content.contains(required),
            "phase-solve Appendix-B validation must define `{required}`"
        );
    }

    let lib_path = root.join("crates/rumoca-phase-solve/src/lib.rs");
    let lib_content = fs::read_to_string(&lib_path).expect("read phase-solve lib.rs");
    assert!(
        lib_content.contains("mod appendix_b_validation;")
            && lib_content.contains("validate_solve_input_appendix_b_invariants(dae_model)")
            && lib_content.contains("validate_solve_problem_appendix_b_invariants(&problem)")
            && lib_content.contains("validate_solve_artifacts_appendix_b_invariants(&artifacts)"),
        "lower_solve_problem must call the Solve-IR Appendix-B validation gate \
at the DAE input, SolveProblem output, and SolveArtifacts output boundaries"
    );
    assert!(
        !lib_content.contains("pub fn lower_continuous_solve_artifacts"),
        "continuous artifact helper must stay private so public callers use \
the validated SolveArtifacts lowering path"
    );

    let solve_model_path = root.join("crates/rumoca-phase-solve/src/solve_model.rs");
    let solve_model_content =
        fs::read_to_string(&solve_model_path).expect("read phase-solve solve_model.rs");
    let problem_lowering = solve_model_content
        .find("crate::lower_solve_problem_with_solver_len(&dae_model, solver_len)?")
        .expect(
            "lower_dae_to_solve_model_inner must lower through the validated SolveProblem path",
        );
    let artifact_lowering = solve_model_content
        .find("crate::lower_solve_artifacts_with_mass_matrix(&problem, mass_matrix)?")
        .expect("lower_dae_to_solve_model_inner must derive runtime artifacts from SolveProblem");
    assert!(
        problem_lowering < artifact_lowering,
        "lower_dae_to_solve_model_inner must run validated Solve-IR lowering before artifact lowering"
    );
}

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

fn source_zero_span_offenders(phase_src_dir: &Path, banned: &[&str]) -> Vec<String> {
    let mut files = Vec::new();
    collect_rs_files(phase_src_dir, &mut files);
    files
        .into_iter()
        .flat_map(|path| source_zero_span_offenders_in_file(&path, banned))
        .collect()
}

fn source_zero_span_offenders_in_file(path: &Path, banned: &[&str]) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read phase source file");
    let production_content = strip_cfg_test_modules(&content);
    production_content
        .lines()
        .enumerate()
        .filter(|(_, line)| banned.iter().any(|needle| line.contains(needle)))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn strip_cfg_test_modules(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    let mut pending_cfg_test = false;
    let mut skipping_depth: Option<usize> = None;

    for line in content.lines() {
        let trimmed = line.trim_start();
        if let Some(depth) = skipping_depth.as_mut() {
            *depth = depth.saturating_add(line.matches('{').count());
            *depth = depth.saturating_sub(line.matches('}').count());
            if *depth == 0 {
                skipping_depth = None;
            }
            out.push('\n');
            continue;
        }

        if trimmed.starts_with("#[cfg(test)]") {
            pending_cfg_test = true;
            out.push('\n');
            continue;
        }

        if pending_cfg_test && trimmed.starts_with("mod tests") {
            let depth = line
                .matches('{')
                .count()
                .saturating_sub(line.matches('}').count());
            if depth > 0 {
                skipping_depth = Some(depth);
            }
            pending_cfg_test = false;
            out.push('\n');
            continue;
        }

        pending_cfg_test = false;
        out.push_str(line);
        out.push('\n');
    }

    out
}

/// SPEC_0029 §12 + WASM optionality: `rumoca-bind-wasm` must not pull in
/// `diffsol` (or any concrete solver backend) transitively in its default
/// feature set. Confirmed via `cargo metadata --no-deps` + a feature-aware
/// dependency walk.
#[test]
fn test_bind_wasm_default_graph_does_not_include_diffsol() {
    use std::process::Command;
    let output = Command::new(env!("CARGO"))
        .args([
            "tree",
            "-p",
            "rumoca-bind-wasm",
            "--edges",
            "normal",
            "--prefix",
            "depth",
        ])
        .current_dir(workspace_root())
        .output()
        .expect("run cargo tree");
    assert!(
        output.status.success(),
        "cargo tree -p rumoca-bind-wasm failed: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let banned: Vec<_> = stdout
        .lines()
        .filter(|l| {
            l.contains("diffsol")
                || l.contains("rumoca-sim")
                || l.contains("rumoca-solver-diffsol")
                || l.contains("rumoca-solver-rk45")
        })
        .map(str::to_owned)
        .collect();
    assert!(
        banned.is_empty(),
        "rumoca-bind-wasm default transitive graph still includes solver \
crates (SPEC_0029 §12 + WASM optionality):\n{}\nUse default-features=false on \
rumoca-tool-lsp to drop the simulation tail.",
        banned.join("\n"),
    );
}
