//! Solve IR plan facts ported from the in-process target stay tied to it.

use super::*;
use sha2::{Digest, Sha256};

const DECLARATION: &str = "const MAX_STATIC_MATRIX_WORK: usize = ";

/// Each in-process rule file a Solve IR storage-plan table was ported from,
/// the sha256 of that file at the port, and the port that must be re-reviewed
/// when it changes.
const PORTED_RULES: &[(&str, &str, &str)] = &[
    (
        "crates/rumoca-exec-cranelift/src/emit/typed_program/fusion.rs",
        "e4ee13326293787d418ed11111872ef8ad8d49a09345732345206085cdc6fe0c",
        "storage_plan/fusion.rs",
    ),
    (
        "crates/rumoca-exec-cranelift/src/emit/typed_program/aliasing.rs",
        "f790b2bd9066efa7ec58f4c3c0d7b3cec443527fb9ca472098864b49c09a4384",
        "storage_plan/aliases.rs",
    ),
    (
        "crates/rumoca-exec-cranelift/src/emit/typed_program/storage.rs",
        "74a9486768c023837e54d9427ab11d8e45b8fb03c16f23f0275571277c82471a",
        "storage_plan/aliases.rs (fold carries)",
    ),
];

/// The storage-plan tables claim the in-process rules unchanged. A change to
/// a rule file must re-review its port under
/// `rumoca-ir-solve/src/typed_program/program/`.
#[test]
fn the_ported_storage_rules_are_unchanged_since_the_port() {
    for (rules, pinned, port) in PORTED_RULES {
        let source = fs::read(workspace_root().join(rules)).expect("read an in-process rule file");
        let digest = Sha256::digest(&source)
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();
        assert_eq!(
            &digest, pinned,
            "{rules} changed since the Solve IR storage plan was ported from it; \
re-review {port} against it, then record the new sha256 here"
        );
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
