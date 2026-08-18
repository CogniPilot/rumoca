//! Crate dependency-tier edge enforcement for the solve/runtime stack.
//!
//! Policy: SPEC_0029 §"Dependency Tiers" ("dependencies flow downward") and
//! §3b ("Runtime Policy Ownership"). `rumoca-eval-solve` is a Tier 3 crate: it
//! is a pure Solve-IR row evaluator / scalarizer. Component-private evaluation
//! (`SolveRuntime`) lives in `rumoca-solver::runtime`; the sole master driver
//! and session live in `rumoca-solver::fmi_me`.
//!
//! Before this module existed, `rumoca-eval-solve` depended *upward* on
//! `rumoca-solver` and hosted the driver, which even hard-coded the diffsol
//! backend's tracing namespace. These tests pin the corrected layering so the
//! edge cannot silently come back.

use super::*;
use std::collections::BTreeSet;
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

    let solver_root = workspace_root().join("crates/rumoca-solver/src");
    for relocated in [
        "runtime/solve_runtime.rs",
        "runtime/solve_events.rs",
        "fmi_me/driver.rs",
        "fmi_me/session.rs",
    ] {
        let path = solver_root.join(relocated);
        assert!(
            path.exists(),
            "{} must exist: rumoca-solver owns component-private evaluation and the sole FMI \
ME master algorithm (SPEC_0029 §3b, SPEC_0044 §6-7)",
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

// ---------------------------------------------------------------------------
// Phase-to-phase dependency policy
// ---------------------------------------------------------------------------

/// Production phase-to-phase dependencies, each with the reason it is allowed.
///
/// Phases lower one IR into the next, so they should compose through the IR and
/// eval crates rather than through each other: a phase that reaches into
/// another drags its neighbour's internals into its own contract and makes
/// either one harder to replace, reorder, or drop. The exceptions below are
/// shared *analysis* phases, which several later phases legitimately consume as
/// a prerequisite, rather than one lowering stage calling another.
///
/// Adding an entry here is the intended escape hatch, but it must be a
/// deliberate one with a stated reason. Removing an edge requires removing its
/// entry too: the gate rejects stale exceptions so this list cannot rot into a
/// record of dependencies that no longer exist.
///
/// Scope is `[dependencies]` only. Test fixtures legitimately build their
/// inputs by running upstream phases, so `[dev-dependencies]` edges are not
/// restricted.
const ALLOWED_PHASE_TO_PHASE_DEPENDENCIES: &[(&str, &str, &str)] = &[
    (
        "rumoca-phase-instantiate",
        "rumoca-phase-resolve",
        "name resolution is a shared prerequisite analysis consumed by later phases, not a lowering stage",
    ),
    (
        "rumoca-phase-typecheck",
        "rumoca-phase-resolve",
        "name resolution is a shared prerequisite analysis consumed by later phases, not a lowering stage",
    ),
    (
        "rumoca-phase-galec",
        "rumoca-phase-structural",
        "structural analysis is a shared prerequisite consumed by both lowering targets",
    ),
    (
        "rumoca-phase-solve",
        "rumoca-phase-structural",
        "structural analysis is a shared prerequisite consumed by both lowering targets",
    ),
];

#[test]
fn test_phase_crates_depend_on_each_other_only_by_recorded_exception() {
    let root = workspace_root();
    let mut observed: BTreeSet<(String, String)> = BTreeSet::new();

    for dir in workspace_crate_dirs(&root) {
        let Some(crate_name) = dir.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if !crate_name.starts_with("rumoca-phase-") {
            continue;
        }
        let Ok(content) = fs::read_to_string(dir.join("Cargo.toml")) else {
            continue;
        };
        for dependency in section_dependency_names(&content, "dependencies") {
            if dependency.starts_with("rumoca-phase-") && dependency != crate_name {
                observed.insert((crate_name.to_string(), dependency));
            }
        }
    }

    let allowed: BTreeSet<(String, String)> = ALLOWED_PHASE_TO_PHASE_DEPENDENCIES
        .iter()
        .map(|(from, to, _)| ((*from).to_string(), (*to).to_string()))
        .collect();

    let unrecorded: Vec<String> = observed
        .difference(&allowed)
        .map(|(from, to)| format!("{from} -> {to}"))
        .collect();
    assert!(
        unrecorded.is_empty(),
        "phase crates must not depend on other phase crates; compose through the IR/eval crates \
instead. If the edge is genuinely a shared prerequisite analysis, add it to \
ALLOWED_PHASE_TO_PHASE_DEPENDENCIES with a reason. Unrecorded: {unrecorded:?}"
    );

    let stale: Vec<String> = allowed
        .difference(&observed)
        .map(|(from, to)| format!("{from} -> {to}"))
        .collect();
    assert!(
        stale.is_empty(),
        "ALLOWED_PHASE_TO_PHASE_DEPENDENCIES records edges that no longer exist; delete them so \
the exception list stays a live record rather than history: {stale:?}"
    );
}
