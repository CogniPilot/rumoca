//! Cargo dependency-identity mutation fixtures.

use super::*;

#[test]
fn mutation_gate_resolves_cargo_dependency_aliases_to_semantic_crates() {
    let temporary = tempfile::tempdir().expect("temporary Rust crate");
    let src = temporary.path().join("src");
    fs::create_dir_all(&src).expect("create fixture src");
    fs::write(
        temporary.path().join("Cargo.toml"),
        r#"
            [package]
            name = "dependency-alias-fixture"
            version = "0.0.0"
            edition = "2024"

            [dependencies]
            semantic_backend = { package = "rumoca-ir-solve", version = "0.0.0" }
            rumoca-ir-dae = { package = "numeric", version = "0.0.0" }
        "#,
    )
    .expect("write fixture manifest");
    fs::write(
        src.join("lib.rs"),
        r#"
            use semantic_backend::LinearOp as Operation;
            fn lower_alias(operation: &Operation) -> bool {
                matches!(operation, Operation::Add { .. })
            }
            fn unrelated_alias(value: &rumoca_ir_dae::Expression) -> bool {
                value.is_finite()
            }
        "#,
    )
    .expect("write aliased semantic consumer");

    let contexts = production_rust_source_contexts(temporary.path(), temporary.path());
    assert!(
        contexts.iter().all(|context| {
            context.crate_aliases.get("semantic_backend") == Some(&"rumoca_ir_solve".to_string())
                && context.crate_aliases.get("rumoca_ir_dae") == Some(&"numeric".to_string())
        }),
        "Cargo package rename was not retained as semantic provenance: {contexts:#?}"
    );
    let findings = analyze_source_contexts(&contexts);
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("lower_alias:semantic-consumer")),
        "Cargo-renamed semantic crate escaped carrier analysis: {findings:#?}"
    );
    assert!(
        !findings
            .iter()
            .any(|finding| finding.contains("unrelated_alias:semantic-consumer")),
        "a nonsemantic Cargo package using a semantic-looking alias was tainted: {findings:#?}"
    );
}
