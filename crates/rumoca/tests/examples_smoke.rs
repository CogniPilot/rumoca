use std::fs;
use std::path::PathBuf;

use rumoca::Compiler;
use tempfile::tempdir;

fn example_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples")
}

fn example_template_path(name: &str) -> PathBuf {
    example_root().join("templates").join(name)
}

fn compile_ball_example() -> rumoca::CompilationResult {
    let model_path = example_root().join("Ball.mo");
    assert!(
        model_path.is_file(),
        "expected example model at {}",
        model_path.display()
    );

    Compiler::new()
        .model("Ball")
        .compile_file(
            model_path
                .to_str()
                .expect("example path should be utf8 for this test"),
        )
        .expect("Ball example should compile")
}

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

#[test]
fn ball_example_file_compiles_from_examples_directory() {
    let result = compile_ball_example();

    assert_eq!(result.dae.states.len(), 1);
    assert_eq!(result.dae.f_x.len(), 1);
}

#[test]
fn ball_example_renders_javascript_template() {
    let result = compile_ball_example();
    let template_path = example_template_path("javascript.jinja");
    assert!(
        template_path.is_file(),
        "expected JS example template at {}",
        template_path.display()
    );

    let rendered = result
        .render_template(template_path.to_string_lossy().as_ref())
        .expect("Ball example should render the JavaScript template");

    assert!(
        rendered.contains("function Model()"),
        "expected JS example template to emit a model factory"
    );
    assert!(
        rendered.contains("residual,") && rendered.contains("applyResets"),
        "expected JS example template to emit the residual-model runtime hooks"
    );
}

#[test]
fn ball_example_renders_standalone_html_template() {
    let result = compile_ball_example();
    let template_path = example_template_path("standalone_html.jinja");
    assert!(
        template_path.is_file(),
        "expected standalone HTML template at {}",
        template_path.display()
    );

    let rendered = result
        .render_template_prepared(template_path.to_string_lossy().as_ref(), true)
        .expect("Ball example should render the standalone HTML template");

    assert!(
        rendered.contains("<!doctype html>"),
        "expected standalone template to emit an HTML document"
    );
    assert!(
        rendered.contains("Run simulation"),
        "expected standalone template to include the simulation UI"
    );
}
