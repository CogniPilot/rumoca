//! A non-FMI Solve target can request an inlined scalar residual (issue #547).

use rumoca::Compiler;

const SOURCE: &str = r#"
model ScalarResidualLoop
  function f
    input Real v[2];
    output Real y;
  algorithm
    y := v[1] + 0.1*sin(v[2]);
  end f;
  Real x(start=1, fixed=true);
  Real z;
equation
  der(x) = -f({z, x});
  f({z, x}) + z^3 = x;
end ScalarResidualLoop;
"#;

#[test]
fn custom_solve_target_inlines_the_residual_without_changing_its_output_slots() {
    let model = Compiler::new()
        .model("ScalarResidualLoop")
        .compile_str(SOURCE, "ScalarResidualLoop.mo")
        .expect("the implicit array-call model compiles");
    let template = r#"
{%- for name in ['residual', 'residual_scalar', 'derivative_rhs_scalar'] %}
{{ name }}:{{ solve_blocks.continuous[name].output_count }}:{% for p in solve_blocks.continuous[name].scalar_plan.programs %}{% for op in p.ops %}{{ op.kind }}{% if op.kind == 'StoreOutput' %}[{{ op.output_index }}]{% endif %},{% endfor %}{% endfor %}
{%- endfor %}
"#;
    let rendered = model
        .render_solve_template_str_without_dae(template, "ResidualTarget")
        .expect("the custom target retains the model's pure-call owner");
    let raw = rendered
        .lines()
        .find(|line| line.starts_with("residual:"))
        .unwrap();
    let scalar = rendered
        .lines()
        .find(|line| line.starts_with("residual_scalar:"))
        .unwrap();
    assert!(
        raw.contains("PureCall"),
        "the reproduction must retain its original call: {raw}"
    );
    assert!(!scalar.contains("PureCall"), "{scalar}");
    for tensor in ["Tensor", "MatrixMultiply", "DotProduct", "FunctionFold"] {
        assert!(!scalar.contains(tensor), "unlowered {tensor}: {scalar}");
    }
    let outputs = |line: &str| {
        line.split(',')
            .filter_map(|op| op.strip_prefix("StoreOutput"))
            .map(str::to_owned)
            .collect::<Vec<_>>()
    };
    assert!(
        !outputs(raw).is_empty(),
        "the residual must store its algebraic rows"
    );
    assert_eq!(
        outputs(raw),
        outputs(scalar),
        "scalar projection keeps derivative holes"
    );
    assert_eq!(raw.split(':').nth(1), scalar.split(':').nth(1));
    let repeated = model
        .render_solve_template_str_without_dae(template, "ResidualTarget")
        .unwrap();
    assert_eq!(rendered, repeated);

    let target = tempfile::tempdir().unwrap();
    std::fs::write(target.path().join("op_kinds.jinja"), template).unwrap();
    std::fs::write(
        target.path().join("target.toml"),
        r#"version = 1
ir = "solve"
name = "scalar-residual-fixture"
description = "Render a scalar residual with retained pure-call owners"
readiness_level = 1
execution_mode = "symbolic"
deployment_class = "symbolic"
[capabilities]
continuous_states = true
residual_equations = true
scalar_fallback = true
[[files]]
path = "op_kinds.txt"
template = "op_kinds.jinja"
"#,
    )
    .unwrap();
    let files = rumoca::render_target_files(
        &model,
        "ResidualTarget",
        target.path().to_str().unwrap(),
        None,
    )
    .expect("a non-FMI directory target renders the same scalar residual");
    let file = files
        .iter()
        .find(|file| file.path == "op_kinds.txt")
        .unwrap();
    assert_eq!(file.content, rendered);
}
