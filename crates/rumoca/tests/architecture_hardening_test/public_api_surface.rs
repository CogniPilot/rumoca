//! Dead public-surface gate.
//!
//! Rust's `dead_code` lint deliberately treats externally reachable `pub` items
//! as potential downstream API, so an unused public item never warns. This gate
//! recovers that signal: [`engine`] resolves every workspace reference to the
//! declaration it names, and a public declaration nothing resolves to must be
//! narrowed, deleted, or written down as a documented contract. DO-178C treats
//! deactivated code as an objective in its own right, so the disposition has to
//! be explicit either way.
//!
//! Prose is not a reference, so `spec/` and `docs/` are outside the usage
//! corpus; a name is only kept alive by code that could actually call it.

mod engine;

use crate::architecture_hardening_support::{collect_rs_files, workspace_root};
use engine::{Corpus, crate_name};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Declaration-only surfaces removed by the workspace API audit.
///
/// These path/name pairs were separately proved to have no workspace caller or
/// documented external contract. Pinning them here prevents a compatibility
/// shim or copied abstraction from silently restoring the dead surface.
///
/// A removed item can only be pinned when its name is unique within its file,
/// since the key is `(path, name)`. `SimExecutionPolicy::from_external_name`
/// was removed without a pin because `SimSolverMode::from_external_name` lives
/// in the same file and is live.
const REMOVED_DECLARATION_ONLY_SURFACES: &[(&str, &str)] = &[
    (
        "crates/rumoca-compile/src/session.rs",
        "prewarm_document_read_queries",
    ),
    (
        "crates/rumoca-contracts/src/test_support.rs",
        "expect_simulation_failure",
    ),
    (
        "crates/rumoca-core/src/lib.rs",
        "CLOCK_FACTOR_INTEGER_TOLERANCE",
    ),
    (
        "crates/rumoca-core/src/lib.rs",
        "INTERNAL_SAMPLE_FUNCTION_NAME",
    ),
    (
        "crates/rumoca-eval-solve/src/lib.rs",
        "build_root_refresh_plan",
    ),
    (
        "crates/rumoca-eval-solve/src/lib.rs",
        "report_state_jacobian",
    ),
    (
        "crates/rumoca-eval-solve/src/refresh_plan.rs",
        "root_condition_dependencies",
    ),
    (
        "crates/rumoca-exec-cranelift/src/lib.rs",
        "compile_residual_scalar_program_block",
    ),
    ("crates/rumoca-ir-ast/src/instance.rs", "symbol_def_ids"),
    ("crates/rumoca-ir-flat/src/connections.rs", "add_set"),
    ("crates/rumoca-ir-flat/src/connections.rs", "num_nodes"),
    (
        "crates/rumoca-ir-solve/src/model.rs",
        "set_integrator_history_effect",
    ),
    (
        "crates/rumoca-phase-resolve/src/lib.rs",
        "resolve_with_stats",
    ),
    (
        "crates/rumoca-phase-resolve/src/semantic_checks.rs",
        "check_chained_relationals",
    ),
    (
        "crates/rumoca-phase-resolve/src/semantic_checks.rs",
        "check_der_in_functions",
    ),
    (
        "crates/rumoca-phase-resolve/src/semantic_checks.rs",
        "check_semantics",
    ),
    (
        "crates/rumoca-phase-solve/src/lib.rs",
        "lower_solve_artifacts_with_mass_matrix",
    ),
    (
        "crates/rumoca-phase-structural/src/lib.rs",
        "maximum_regular_subsystem",
    ),
    ("crates/rumoca-sim/src/lib.rs", "compare_trace_files"),
    ("crates/rumoca-sim/src/lib.rs", "clear_parameter_overrides"),
    ("crates/rumoca-sim/src/lib.rs", "set_parameter_values"),
    ("crates/rumoca-solver/src/session.rs", "initial_point"),
    (
        "crates/rumoca-solver/src/runtime/solve_ops.rs",
        "filter_scheduled_root_crossings",
    ),
    (
        "crates/rumoca-solver/src/runtime/solve_runtime.rs",
        "root_condition_is_search_active",
    ),
    (
        "crates/rumoca-solver/src/runtime/solve_runtime/plans.rs",
        "root_search_is_uniformly_inactive",
    ),
    (
        "crates/rumoca-tool-lsp/src/util.rs",
        "token_to_range_in_source",
    ),
    (
        "crates/rumoca-compile/src/session/session_impl.rs",
        "all_class_names_cached",
    ),
    (
        "crates/rumoca-compile/src/session/session_snapshot.rs",
        "all_class_names_cached",
    ),
    ("crates/rumoca-compile/src/session.rs", "is_success"),
    ("crates/rumoca-compile/src/session.rs", "needs_inner"),
    ("crates/rumoca-ir-solve/src/model.rs", "empty_with_span"),
    ("crates/rumoca-opt/src/model.rs", "set_parameter_value"),
    (
        "crates/rumoca-phase-instantiate/src/errors.rs",
        "is_success",
    ),
    (
        "crates/rumoca-phase-instantiate/src/errors.rs",
        "needs_inner",
    ),
    ("crates/rumoca-phase-parse/src/lib.rs", "parse_file"),
    ("crates/rumoca-phase-typecheck/src/lib.rs", "type_mismatch"),
    ("crates/rumoca-sim/src/diffsol.rs", "set_parameter_value"),
    (
        "crates/rumoca-sim/src/lib.rs",
        "runtime_defined_unknown_names",
    ),
    (
        "crates/rumoca-sim/src/lib.rs",
        "runtime_defined_continuous_unknown_names",
    ),
    ("crates/rumoca-sim/src/rk45.rs", "set_inputs"),
    ("crates/rumoca-sim/src/simulation_session.rs", "set_inputs"),
    (
        "crates/rumoca-solver/src/runtime/solve_runtime/relation_memory.rs",
        "eval_scalar_program_block",
    ),
    ("crates/rumoca-transport-udp/src/lib.rs", "send_addr"),
];

/// Public items a specification or an out-of-corpus consumer names as an owned
/// contract, so their disposition is "kept on purpose" rather than "no caller
/// found".
///
/// Each entry is `(path, name, owning document)`, and that document must still
/// name the item: the exemption dies with the row that grants it.
const DOCUMENTED_CONTRACT_SURFACES: &[(&str, &str, &str)] = &[
    (
        "crates/rumoca-core/src/lib.rs",
        "source_temporal_function_short_name",
        "spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md",
    ),
    (
        "crates/rumoca-core/src/lib.rs",
        "source_dae_forbidden_builtin_name",
        "spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md",
    ),
];

/// Workspace directories whose sources can reference a public item.
///
/// `spec/` and `docs/` are absent on purpose: a mnemonic in a specification
/// table or a name in a guide is prose about the code, not a caller of it.
const USAGE_CORPUS_ROOTS: &[&str] = &["crates", "packages", "infra", "examples"];

/// Non-Rust source extensions that can hold a real call site (binding
/// consumers, build tooling, examples).
const NON_RUST_CALLER_EXTENSIONS: &[&str] = &["mjs", "js", "py", "ts"];

fn is_generated(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "generated")
}

/// This gate's own sources, whose mention of a name is a record about the item
/// rather than a use of it.
fn is_audit_source(path: &Path) -> bool {
    path.ends_with("crates/rumoca/tests/architecture_hardening_test/public_api_surface.rs")
        || path.ends_with(
            "crates/rumoca/tests/architecture_hardening_test/public_api_surface/engine.rs",
        )
}

fn collect_audited_text_files(dir: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().is_some_and(|name| {
                matches!(
                    name.to_str(),
                    Some("target" | ".git" | "generated" | "vendor" | "node_modules" | "dist")
                )
            }) {
                continue;
            }
            collect_audited_text_files(&path, files);
            continue;
        }
        if is_audit_source(&path) {
            continue;
        }
        let extension = path.extension().and_then(|extension| extension.to_str());
        if extension.is_some_and(|extension| {
            extension == "rs" || NON_RUST_CALLER_EXTENSIONS.contains(&extension)
        }) {
            files.push(path);
        }
    }
}

fn workspace_corpus(root: &Path) -> Corpus {
    let mut audited_files = Vec::new();
    for relative in USAGE_CORPUS_ROOTS {
        collect_audited_text_files(&root.join(relative), &mut audited_files);
    }
    audited_files.sort();
    audited_files.dedup();
    audited_files.retain(|path| !is_generated(path));

    let mut corpus = Corpus::default();
    for path in audited_files {
        let source = fs::read_to_string(&path).expect("read source for the API audit");
        if path.extension().is_some_and(|extension| extension == "rs") {
            corpus.add_rust_source(crate_name(&path).as_deref(), &path, &source);
        } else {
            corpus.add_text_source(&source);
        }
    }
    corpus
}

fn has_documented_contract(root: &Path, path: &Path, name: &str) -> bool {
    DOCUMENTED_CONTRACT_SURFACES
        .iter()
        .any(|(owner_path, owner_name, _)| *owner_name == name && root.join(owner_path) == path)
}

#[test]
fn documented_contract_surfaces_are_named_by_the_document_that_owns_them() {
    let root = workspace_root();
    let unnamed: Vec<String> = DOCUMENTED_CONTRACT_SURFACES
        .iter()
        .filter(|(_, name, owner)| {
            let text = fs::read_to_string(root.join(owner))
                .unwrap_or_else(|error| panic!("read {owner}: {error}"));
            !text.contains(name)
        })
        .map(|(path, name, owner)| format!("{path}: {name} (claims {owner})"))
        .collect();
    assert!(
        unnamed.is_empty(),
        "a public surface is only exempt while the document still names it as owned: {unnamed:#?}"
    );
}

#[test]
fn new_declaration_only_public_surfaces_require_explicit_disposition() {
    let root = workspace_root();
    let mut candidates = workspace_corpus(&root).unreferenced_public_declarations();
    candidates.retain(|(path, name)| !has_documented_contract(&root, path, name));
    assert!(
        candidates.is_empty(),
        "public declarations no workspace reference resolves to must be narrowed to pub(crate); dead_code = deny will then prove whether to delete them:\n  {}",
        candidates
            .iter()
            .map(|(path, name)| format!(
                "{}: {name}",
                path.strip_prefix(&root).unwrap_or(path).display()
            ))
            .collect::<Vec<_>>()
            .join("\n  ")
    );
}

#[test]
fn removed_declaration_only_public_items_do_not_return() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files.retain(|path| !is_generated(path));
    files.sort();

    let mut corpus = Corpus::default();
    for path in files {
        let source = fs::read_to_string(&path).expect("read Rust source for public API audit");
        corpus.add_rust_source(crate_name(&path).as_deref(), &path, &source);
    }

    let inventory: BTreeSet<(PathBuf, String)> = corpus
        .declarations
        .iter()
        .map(|declaration| (declaration.path.clone(), declaration.name.clone()))
        .collect();
    let restored = REMOVED_DECLARATION_ONLY_SURFACES
        .iter()
        .filter(|(path, name)| inventory.contains(&(root.join(path), (*name).to_owned())))
        .map(|(path, name)| format!("{path}: {name}"))
        .collect::<Vec<_>>();
    assert!(
        restored.is_empty(),
        "audited declaration-only public surfaces were restored:\n{}",
        restored.join("\n")
    );
}

#[test]
fn prose_directories_stay_out_of_the_usage_corpus() {
    assert!(
        !USAGE_CORPUS_ROOTS.contains(&"spec") && !USAGE_CORPUS_ROOTS.contains(&"docs"),
        "a name in a specification table or a guide is prose about the code, not a caller of \
it, and a text caller is credited to every declaration of the name: {USAGE_CORPUS_ROOTS:?}"
    );
}

#[test]
fn audited_facades_do_not_restore_forwarded_or_private_surfaces() {
    let root = workspace_root();
    let compile_facade = fs::read_to_string(root.join("crates/rumoca-compile/src/lib.rs"))
        .expect("read compile facade");
    for forbidden in [
        "pub use rumoca_core as",
        "AstCausality",
        "AstComponent",
        "AstExpression",
        "AstToken",
        "AstVariability",
    ] {
        assert!(
            !compile_facade.contains(forbidden),
            "rumoca-compile restored audited forwarding surface `{forbidden}`"
        );
    }

    let solver_root = fs::read_to_string(root.join("crates/rumoca-solver/src/lib.rs"))
        .expect("read solver facade");
    assert!(
        !solver_root.contains("pub use runtime::projection"),
        "rumoca-solver restored component-private projection re-exports"
    );
    let runtime_root = fs::read_to_string(root.join("crates/rumoca-solver/src/runtime/mod.rs"))
        .expect("read solver runtime modules");
    assert!(
        !runtime_root.contains("pub mod projection;"),
        "rumoca-solver restored public access to component-private projection policy"
    );
}
