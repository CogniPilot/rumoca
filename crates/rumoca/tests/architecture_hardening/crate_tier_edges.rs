//! Crate dependency-tier edge enforcement for the solve/runtime stack.
//!
//! Policy: SPEC_0029 §"Dependency Tiers" ("dependencies flow downward") and
//! §3b ("Runtime Policy Ownership"). `rumoca-eval-solve` is a Tier 3 crate: it
//! is a pure Solve-IR row evaluator / scalarizer. The runtime state machine
//! (`SolveRuntime`) and the backend-neutral simulation driver are Tier 5 and
//! live in `rumoca-solver::runtime`.
//!
//! Before this module existed, `rumoca-eval-solve` depended *upward* on
//! `rumoca-solver` and hosted the driver, which even hard-coded the diffsol
//! backend's tracing namespace. These tests pin the corrected layering so the
//! edge cannot silently come back.

use super::*;
use std::path::PathBuf;

/// The complete, exact `[dependencies]` set of `rumoca-eval-solve`.
///
/// Pinned (not a "must not contain rumoca-solver" check) so that *any* new
/// dependency on a Tier 4/5 crate is a deliberate, reviewable change rather
/// than an accident.
const EVAL_SOLVE_DEPENDENCIES: &[&str] = &["indexmap", "rumoca-core", "rumoca-ir-solve", "tracing"];

fn read_manifest(crate_name: &str) -> String {
    let path = workspace_root()
        .join("crates")
        .join(crate_name)
        .join("Cargo.toml");
    fs::read_to_string(&path).unwrap_or_else(|error| {
        panic!("read {} manifest: {error}", path.display());
    })
}

#[test]
fn test_eval_solve_dependency_set_is_pinned() {
    let content = read_manifest("rumoca-eval-solve");
    let mut names = section_dependency_names(&content, "dependencies");
    names.sort();

    let expected: Vec<String> = EVAL_SOLVE_DEPENDENCIES
        .iter()
        .map(|name| (*name).to_string())
        .collect();

    assert_eq!(
        names, expected,
        "rumoca-eval-solve [dependencies] must stay exactly {expected:?} \
(SPEC_0029 Dependency Tiers: a Tier 3 evaluation crate may not depend on a \
Tier 4/5 crate such as rumoca-solver). Found {names:?}."
    );
}

#[test]
fn test_eval_solve_has_no_runtime_or_driver_sources() {
    let eval_solve_src = workspace_root().join("crates/rumoca-eval-solve/src");
    for relocated in [
        "sim_driver.rs",
        "runtime.rs",
        "runtime_events.rs",
        "jacobian.rs",
        "eval_at.rs",
        "delay.rs",
        "runtime",
    ] {
        let path = eval_solve_src.join(relocated);
        assert!(
            !path.exists(),
            "{} must not exist: the runtime state machine and simulation driver \
belong to rumoca-solver::runtime (SPEC_0029 §3b), not to the Tier 3 row evaluator",
            path.display()
        );
    }

    let solver_runtime = workspace_root().join("crates/rumoca-solver/src/runtime");
    for relocated in ["driver.rs", "solve_runtime.rs", "solve_events.rs"] {
        let path = solver_runtime.join(relocated);
        assert!(
            path.exists(),
            "{} must exist: rumoca-solver owns the backend-neutral driver and \
runtime state machine (SPEC_0029 §3b)",
            path.display()
        );
    }
}

#[test]
fn test_no_backend_namespace_in_neutral_crates() {
    let mut files: Vec<PathBuf> = Vec::new();
    for crate_name in ["rumoca-eval-solve", "rumoca-solver"] {
        collect_rs_files(
            &workspace_root().join("crates").join(crate_name).join("src"),
            &mut files,
        );
    }

    let offenders: Vec<String> = files
        .iter()
        .filter(|path| {
            let content = fs::read_to_string(path).expect("read backend-neutral source");
            content.contains("rumoca_solver_diffsol")
        })
        .map(|path| path.display().to_string())
        .collect();

    assert!(
        offenders.is_empty(),
        "backend-neutral crates must not name a concrete backend's tracing \
namespace (SPEC_0029 §3b); the shared driver traces under \
rumoca_solver::driver. Offenders: {offenders:#?}"
    );

    let report = workspace_root().join("crates/rumoca-solver/src/runtime/report.rs");
    let report_content = fs::read_to_string(&report).expect("read runtime report module");
    assert!(
        report_content.contains("\"rumoca_solver::driver\""),
        "{} must define the backend-neutral DRIVER_TRACE_TARGET",
        report.display()
    );
}

#[test]
fn test_solver_contract_crate_consumes_row_evaluator_only() {
    let content = read_manifest("rumoca-solver");

    assert!(
        section_contains_dependency(&content, "dependencies", "rumoca-eval-solve"),
        "rumoca-solver must depend on rumoca-eval-solve: its runtime state \
machine evaluates prepared Solve-IR rows through the Tier 3 evaluator"
    );

    for banned in [
        "rumoca-ir-dae",
        "rumoca-eval-dae",
        "rumoca-phase-dae",
        "rumoca-phase-structural",
        "rumoca-phase-solve",
        "diffsol",
        "rumoca-solver-diffsol",
        "rumoca-solver-rk45",
    ] {
        assert!(
            !section_contains_dependency(&content, "dependencies", banned),
            "rumoca-solver must not depend on {banned}: DAE/phase preparation \
stays upstream and concrete backends stay downstream of the runtime-contract crate"
        );
    }
}
