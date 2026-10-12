//! The public no-DAE renderer retains the pure-call owners it lowers.

use rumoca::Compiler;

const SOURCE: &str = r#"
function scale
  input Real x;
  output Real y;
algorithm
  y := 2*x;
end scale;
model RendererWithoutDae
  Real x(start=1);
equation
  der(x) = -scale(x);
end RendererWithoutDae;
"#;

#[test]
fn public_solve_renderer_without_dae_projects_checked_function_owner() {
    let compiled = Compiler::new()
        .model("RendererWithoutDae")
        .compile_str(SOURCE, "RendererWithoutDae.mo")
        .expect("the function-call model compiles");
    let template =
        "{{ model_name }}|{{ solve_blocks.continuous.derivative_rhs.scalar_plan.output_count }}";
    for name in ["FirstRender", "SecondRender"] {
        let rendered = compiled
            .render_solve_template_str_without_dae(template, name)
            .expect("the public renderer retains the checked scalar projection");
        assert_eq!(rendered, format!("{name}|1"));
    }
    let error = compiled
        .render_solve_template_str_without_dae("{{ dae.states | length }}", "NoDae")
        .expect_err("this public context cannot invent a DAE entry");
    assert!(error.to_string().contains("undefined"), "{error}");
    assert_eq!(
        compiled
            .render_solve_template_str_without_dae(template, "AfterRefusal")
            .expect("a template refusal must not corrupt the retained checked context"),
        "AfterRefusal|1"
    );
}
