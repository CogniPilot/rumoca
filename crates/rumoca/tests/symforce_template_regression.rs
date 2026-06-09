use rumoca::Compiler;
use rumoca_phase_codegen::templates;

fn render_template(source: &str, model_name: &str, file_name: &str) -> String {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, file_name)
        .expect("compile test model");

    rumoca_phase_codegen::render_template_with_name(
        &compiled.dae,
        templates::builtin_template_source("symforce", "symforce.py.jinja").unwrap(),
        model_name,
    )
    .expect("render symforce template")
}

fn render_ball_template() -> String {
    let source = r#"
model Ball
  Real x(start = 1);
  parameter Real k = 2;
equation
  der(x) = -k * x;
end Ball;
"#;

    render_template(source, "Ball", "Ball.mo")
}

fn render_array_template() -> String {
    let source = r#"
model Arr
  Real x[2](start = {1, 2});
equation
  der(x[1]) = -x[1];
  der(x[2]) = -x[2];
end Arr;
"#;

    render_template(source, "Arr", "Arr.mo")
}

fn render_reprojection_template() -> String {
    let source = r#"
model ReprojectionResidual
  input Real pointCam[3];
  input Real observed[2];
  parameter Real focal = 1;
  Real residual[2];
equation
  residual[1] = focal * pointCam[1] / pointCam[3] - observed[1];
  residual[2] = focal * pointCam[2] / pointCam[3] - observed[2];
end ReprojectionResidual;
"#;

    render_template(source, "ReprojectionResidual", "ReprojectionResidual.mo")
}

#[test]
fn symforce_template_renders_symbolic_dae_codegen_surface() {
    let rendered = render_ball_template();

    assert!(
        rendered.contains("import symforce.symbolic as sf"),
        "expected SymForce symbolic import, got:\n{rendered}"
    );
    assert!(
        rendered.contains("from symforce.values import Values"),
        "expected SymForce Values import, got:\n{rendered}"
    );
    assert!(
        rendered.contains("if not getattr(symforce, \"_have_used_epsilon\", False):"),
        "expected idempotent SymForce epsilon setup, got:\n{rendered}"
    );
    assert!(
        rendered.contains("def create_model()"),
        "expected create_model entry point, got:\n{rendered}"
    );
    assert!(
        rendered.contains("def make_residual_codegen("),
        "expected residual Codegen helper, got:\n{rendered}"
    );
    assert!(
        rendered.contains("return codegen.Codegen("),
        "expected Codegen construction, got:\n{rendered}"
    );
    assert!(
        rendered.contains("outputs=Values(f_x=model[\"f_x\"]),"),
        "expected residual Codegen to exclude empty auxiliary outputs, got:\n{rendered}"
    );
    assert!(
        rendered.contains("x = _symbol(\"x\")"),
        "expected state symbol declaration, got:\n{rendered}"
    );
    assert!(
        rendered.contains("der_x = _symbol(\"der(x)\")"),
        "expected derivative symbol declaration, got:\n{rendered}"
    );
    assert!(
        rendered.contains("f_x = _column(["),
        "expected DAE residual vector, got:\n{rendered}"
    );
    assert!(
        rendered.contains("der(x)") && rendered.contains("k * x"),
        "expected rendered DAE residual to reference der(x), k, and x, got:\n{rendered}"
    );
}

#[test]
fn symforce_template_scalarizes_fixed_size_arrays_readably() {
    let rendered = render_array_template();

    assert!(
        rendered.contains("_symbol(\"x[1]\")") && rendered.contains("_symbol(\"x[2]\")"),
        "expected scalar array element symbols, got:\n{rendered}"
    );
    assert!(
        rendered.contains("x = _matrix_from_elements([2], x_elements)"),
        "expected array base matrix construction, got:\n{rendered}"
    );
    assert!(
        rendered.contains("der_x_1 = der_x_elements[0]")
            && rendered.contains("der_x_2 = der_x_elements[1]"),
        "expected derivative aliases for array state elements, got:\n{rendered}"
    );
    assert!(
        rendered.contains("der(x_1)") && rendered.contains("der(x_2)"),
        "expected residuals to reference derivative aliases, got:\n{rendered}"
    );
}

#[test]
fn symforce_template_can_express_reprojection_residuals_for_ba_style_models() {
    let rendered = render_reprojection_template();

    assert!(
        rendered.contains("pointCam_1")
            && rendered.contains("pointCam_2")
            && rendered.contains("pointCam_3"),
        "expected point array aliases for reprojection residual, got:\n{rendered}"
    );
    assert!(
        rendered.contains("observed_1") && rendered.contains("observed_2"),
        "expected observation array aliases for reprojection residual, got:\n{rendered}"
    );
    assert!(
        rendered.contains("residual_1") && rendered.contains("residual_2"),
        "expected residual array aliases, got:\n{rendered}"
    );
    assert!(
        rendered.contains("focal") && rendered.contains("pointCam_3"),
        "expected rendered residual expression to include focal projection denominator, got:\n{rendered}"
    );
    assert!(
        rendered.contains("with inputs.scope(\"external_inputs\")"),
        "expected external input Values scope for observations/points, got:\n{rendered}"
    );
}
