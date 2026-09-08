//! Schema and template-context mutation fixtures.

use super::*;

#[test]
fn mutation_gate_fingerprints_generated_schema_and_template_context() {
    let findings = analyze_sources(&[(
        PathBuf::from("crates/rumoca-phase-codegen/src/schema.rs"),
        r#"
            fn schema() -> serde_json::Value {
                serde_json::json!({ "operator": "+", "precedence": 6 })
            }
            fn template_context() -> minijinja::Value {
                minijinja::context! { operator => "+", precedence => 6 }
            }
        "#
        .to_string(),
    )]);
    for expected in [
        "schema:generated-schema:macro-json:",
        "template_context:template-context:macro-context:",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "generated schema/context escaped owner fingerprinting: {expected}: {findings:#?}"
        );
    }
}
