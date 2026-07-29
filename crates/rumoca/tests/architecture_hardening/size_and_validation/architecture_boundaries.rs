//! Crate/module architecture boundary gates.

use super::super::*;

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
}

#[test]
fn test_checked_dae_is_the_only_production_dae_representation() {
    let root = workspace_root();
    let dae_src = root.join("crates/rumoca-ir-dae/src");
    for removed in ["checked", "types.rs", "visitor.rs", "clock_schedule.rs"] {
        assert!(
            !dae_src.join(removed).exists(),
            "removed DAE representation path must not coexist with the canonical checked DAE: \
             {removed}"
        );
    }

    let mut offenders = Vec::new();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);
    for path in rs_files {
        if path
            .components()
            .any(|component| component.as_os_str() == "tests")
            || path
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name == "tests.rs" || name.ends_with("_tests.rs"))
        {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for forbidden in [
            "rumoca_ir_dae::checked",
            "rumoca_ir_dae::DaeDraft",
            "Dae::build(",
            "deserialize_v10",
            "schema_version = 10",
        ] {
            if content.contains(forbidden) {
                offenders.push(format!("{}:{forbidden}", path.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "production code still references a removed DAE representation or wire path: \
         {offenders:#?}"
    );
}

#[test]
fn test_todae_uses_constructor_enforced_invariants_without_validation_passes() {
    let root = workspace_root();
    let phase_dae = root.join("crates/rumoca-phase-dae/src");
    let construction =
        fs::read_to_string(phase_dae.join("construction.rs")).expect("read DAE construction");
    assert!(
        construction.contains("Dae::construct("),
        "ToDAE must enter the canonical Dae::construct boundary"
    );

    for removed in [
        "appendix_b_validation.rs",
        "reference_validation.rs",
        "temporal_finalization.rs",
    ] {
        assert!(
            !phase_dae.join(removed).exists(),
            "constructor-enforced invariants replace obsolete validation pass {removed}"
        );
    }

    let dae_model =
        fs::read_to_string(root.join("crates/rumoca-ir-dae/src/model.rs")).expect("read DAE model");
    assert!(
        !dae_model.contains("pub fn validate(") && !dae_model.contains("pub fn insert_unchecked"),
        "canonical DAE must not expose validation or unchecked insertion escape hatches"
    );
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
