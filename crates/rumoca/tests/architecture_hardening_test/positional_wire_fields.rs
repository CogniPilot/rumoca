use crate::architecture_hardening_support::workspace_root;
use std::fs;

/// Source files whose serde types are decoded from a positional binary cache:
/// parsed `StoredDefinition` artifacts, persisted semantic summaries, and
/// source maps. A positional reader cannot detect a field omitted by value, so
/// these types never use `skip_serializing_if` (SPEC_0041 §3).
const POSITIONALLY_DECODED_FILES: &[&str] = &[
    "crates/rumoca-ir-ast/src/lib.rs",
    "crates/rumoca-ir-ast/src/nodes.rs",
    "crates/rumoca-ir-ast/src/scope.rs",
    "crates/rumoca-ir-ast/src/state_machines.rs",
    "crates/rumoca-ir-ast/src/types.rs",
    "crates/rumoca-core/src/source_map.rs",
    "crates/rumoca-compile/src/parsed_artifact_cache.rs",
    "crates/rumoca-compile/src/session/declaration_index.rs",
    "crates/rumoca-compile/src/session/file_summary.rs",
    "crates/rumoca-compile/src/session/package_def_map.rs",
    "crates/rumoca-compile/src/session/semantic_summary_cache.rs",
];

#[test]
fn positionally_decoded_types_never_omit_fields_by_value() {
    let root = workspace_root();
    let violations: Vec<String> = POSITIONALLY_DECODED_FILES
        .iter()
        .flat_map(|relative| {
            let text = fs::read_to_string(root.join(relative))
                .unwrap_or_else(|error| panic!("read {relative}: {error}"));
            text.lines()
                .enumerate()
                .filter(|(_, line)| line.contains("skip_serializing_if"))
                .map(|(index, _)| format!("{relative}:{}", index + 1))
                .collect::<Vec<_>>()
        })
        .collect();
    assert!(
        violations.is_empty(),
        "fields omitted by value in positionally decoded types:\n{}",
        violations.join("\n")
    );
}
