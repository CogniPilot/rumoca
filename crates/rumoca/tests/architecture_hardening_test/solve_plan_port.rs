//! Solve IR plan facts ported from the in-process target stay tied to it.

use super::*;
use std::path::PathBuf;

const DECLARATION: &str = "const MAX_STATIC_MATRIX_WORK: usize = ";

/// The in-process rule files the Solve IR storage plan was ported from. The
/// target now reads the issued plan, so none of them may return.
const RETIRED_RULE_FILES: &[&str] = &[
    "crates/rumoca-exec-cranelift/src/emit/typed_program/fusion.rs",
    "crates/rumoca-exec-cranelift/src/emit/typed_program/aliasing.rs",
    "crates/rumoca-exec-cranelift/src/emit/typed_program/storage.rs",
];

/// The decisions those files made. Each now lives only under
/// `rumoca-ir-solve/src/typed_program/program/storage_plan/`, and a map's
/// repeated capture on its region.
const RETIRED_DECISIONS: &[&str] = &[
    "fn register_flow(",
    "fn alias_read_only_loads(",
    "fn alias_final_output_stores(",
    "fn alias_consumed_functional_updates(",
    "fn alias_consumed_fold_carries(",
    "fn compact_register_tape(",
    "fn single_use_consumers(",
    "fn recompute_cheap_producers(",
    "fn fuse_product_transposes(",
    "fn sign_definition(",
];

/// The in-process target lays typed programs out from the issued storage
/// plan; the fusion, alias and frame-cell decisions it was ported from stay
/// retired.
#[test]
fn the_ported_storage_rules_stay_retired_from_the_in_process_target() {
    let root = workspace_root();
    for file in RETIRED_RULE_FILES {
        assert!(
            !root.join(file).exists(),
            "{file} returned; the Solve IR storage plan issues its decisions"
        );
    }
    let mut sources = Vec::new();
    rust_sources(&root.join("crates/rumoca-exec-cranelift/src"), &mut sources);
    assert!(
        !sources.is_empty(),
        "no in-process target sources were read"
    );
    let offenders = sources
        .iter()
        .flat_map(|path| {
            let source = fs::read_to_string(path).expect("read an in-process source");
            retired_definitions(&source)
                .into_iter()
                .map(move |decision| format!("{}: {decision}", path.display()))
        })
        .collect::<Vec<_>>();
    assert!(
        offenders.is_empty(),
        "retired storage decisions are defined in the in-process target again: {offenders:?}"
    );
}

/// The scan reports a retired definition and ignores a mention of one.
#[test]
fn the_retired_decision_scan_finds_a_definition() {
    assert_eq!(
        retired_definitions("pub(super) fn compact_register_tape(\n"),
        ["fn compact_register_tape("]
    );
    assert!(retired_definitions("// compact_register_tape was retired\n").is_empty());
}

/// The retired decisions `source` defines.
fn retired_definitions(source: &str) -> Vec<&'static str> {
    RETIRED_DECISIONS
        .iter()
        .copied()
        .filter(|decision| source.contains(decision))
        .collect()
}

/// Every `.rs` file under `dir`.
fn rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for path in entries.flatten().map(|entry| entry.path()) {
        if path.is_dir() {
            rust_sources(&path, out);
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            out.push(path);
        }
    }
}

/// Solve IR issues which transposes a host-kernel product reads in place
/// from `SOLVE_STATIC_MATRIX_WORK`; the in-process target still picks its
/// product lowering from its own `MAX_STATIC_MATRIX_WORK`. Until a target
/// declares the bound, the two must name the same number.
#[test]
fn solve_ir_and_the_in_process_target_share_the_static_matrix_bound() {
    let emit = workspace_root().join("crates/rumoca-exec-cranelift/src/emit.rs");
    let source = fs::read_to_string(&emit).expect("read the in-process emitter");
    assert_eq!(
        declared_bound(&source),
        Ok(rumoca_ir_solve::SOLVE_STATIC_MATRIX_WORK),
        "the in-process MAX_STATIC_MATRIX_WORK in {} and Solve IR's SOLVE_STATIC_MATRIX_WORK differ",
        emit.display()
    );
}

/// The guard refuses a missing, repeated or non-literal declaration rather
/// than passing on it.
#[test]
fn the_static_matrix_bound_guard_refuses_what_it_cannot_read() {
    assert_eq!(
        declared_bound("const MAX_STATIC_MATRIX_WORK: usize = 64;"),
        Ok(64)
    );
    assert_eq!(
        declared_bound("const MAX_STATIC_MATRIX_WORK: usize = 65;"),
        Ok(65)
    );
    assert!(declared_bound("const OTHER: usize = 64;").is_err());
    assert!(
        declared_bound(
            "const MAX_STATIC_MATRIX_WORK: usize = 64;\nconst MAX_STATIC_MATRIX_WORK: usize = 64;"
        )
        .is_err()
    );
    assert!(declared_bound("const MAX_STATIC_MATRIX_WORK: usize = LIMIT;").is_err());
}

/// The one `MAX_STATIC_MATRIX_WORK` integer literal declared in `source`.
fn declared_bound(source: &str) -> Result<u64, String> {
    let declared = source
        .lines()
        .filter_map(|line| line.trim().strip_prefix(DECLARATION)?.strip_suffix(';'))
        .collect::<Vec<_>>();
    let [value] = declared.as_slice() else {
        return Err(format!(
            "expected exactly one `{DECLARATION}<n>;`, found {}",
            declared.len()
        ));
    };
    value
        .trim()
        .parse()
        .map_err(|error| format!("MAX_STATIC_MATRIX_WORK is not an integer literal: {error}"))
}
