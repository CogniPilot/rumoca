use std::fs;

use rumoca::Compiler;
use tempfile::tempdir;

#[test]
fn basic_usage_flow_compiles_and_serializes_json() {
    let source = r#"
model Integrator
    Real x(start=0.0);
equation
    der(x) = 1.0;
end Integrator;
"#;

    let result = Compiler::new()
        .model("Integrator")
        .compile_str(source, "Integrator.mo")
        .expect("basic usage example compile should succeed");

    assert_eq!(result.dae.states.len(), 1);
    assert!(!result.dae.f_x.is_empty());
    let json = result.to_json().expect("json serialization should succeed");
    assert!(json.contains("\"f_x\""));
}

#[test]
fn file_compilation_flow_compiles_from_disk() {
    let dir = tempdir().expect("tempdir should be creatable");
    let model_file = dir.path().join("file_example.mo");
    fs::write(
        &model_file,
        r#"
model FileExample
    Real x(start=0.0);
equation
    der(x) = 2.0;
end FileExample;
"#,
    )
    .expect("model file should be writable");

    let result = Compiler::new()
        .model("FileExample")
        .compile_file(
            model_file
                .to_str()
                .expect("temp model path should be utf8 for this test"),
        )
        .expect("file compilation example should compile");

    assert_eq!(result.dae.states.len(), 1);
    assert_eq!(result.dae.f_x.len(), 1);
}

#[test]
fn protected_flow_marks_protected_components_in_flat_ir() {
    let source = r#"
model ProtectedDemo
    parameter Real public_gain = 2;
protected
    parameter Real protected_gain = 3;
    Real hidden(start = 0);
equation
    hidden = public_gain + protected_gain;
end ProtectedDemo;
"#;

    let result = Compiler::new()
        .model("ProtectedDemo")
        .compile_str(source, "<protected_demo>")
        .expect("protected example should compile");

    let find_var = |name: &str| {
        result
            .flat
            .variables
            .iter()
            .find(|(var_name, _)| var_name.as_str() == name)
            .map(|(_, var)| var)
            .unwrap_or_else(|| panic!("variable '{name}' should exist"))
    };

    let public = find_var("public_gain");
    assert!(
        !public.is_protected,
        "public variable should not be marked protected"
    );

    let protected = find_var("protected_gain");
    assert!(
        protected.is_protected,
        "protected variable should be marked protected"
    );

    let hidden = find_var("hidden");
    assert!(
        hidden.is_protected,
        "variables declared in protected section should be marked protected"
    );
}
