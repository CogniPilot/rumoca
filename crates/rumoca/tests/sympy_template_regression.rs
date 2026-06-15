#[cfg(feature = "template-runtime-tests")]
use std::{fs, process::Command};

use rumoca::Compiler;
use rumoca_phase_codegen::templates;
#[cfg(feature = "template-runtime-tests")]
use tempfile::Builder;

#[cfg(feature = "template-runtime-tests")]
fn python_command() -> &'static str {
    for candidate in ["python3", "python"] {
        if Command::new(candidate)
            .args(["-c", "import sympy"])
            .output()
            .is_ok_and(|output| output.status.success())
        {
            return candidate;
        }
    }
    panic!("expected python3 or python with sympy installed for SymPy template regression tests");
}

fn render_template(source: &str, model_name: &str, file_name: &str) -> String {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, file_name)
        .expect("compile test model");

    rumoca_phase_codegen::render_template_with_name(
        &compiled.dae,
        templates::builtin_template_source("sympy", "sympy.py.jinja").unwrap(),
        model_name,
    )
    .expect("render sympy template")
}

fn render_dae_template_json(source: &str, model_name: &str, file_name: &str) -> serde_json::Value {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, file_name)
        .expect("compile test model");

    rumoca_phase_codegen::dae_template_json(&compiled.dae).expect("render DAE template JSON")
}

#[cfg(feature = "template-runtime-tests")]
fn assert_python_executes(rendered: &str, assertion_script: &str) {
    let temp = Builder::new()
        .suffix(".py")
        .tempfile()
        .expect("create temporary Python module");
    fs::write(temp.path(), rendered).expect("write rendered Python module");

    let module_path = serde_json::to_string(temp.path().to_string_lossy().as_ref())
        .expect("serialize module path for Python");
    let script = format!(
        r#"
import importlib.util

spec = importlib.util.spec_from_file_location("generated_model", {module_path})
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
model = module.Model()
solution = model.solve_explicit()
{assertion_script}
"#
    );
    let output = Command::new(python_command())
        .arg("-c")
        .arg(script)
        .output()
        .expect("run generated SymPy module through Python");

    assert!(
        output.status.success(),
        "expected generated Python to run successfully\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn render_ball_template() -> String {
    let source = r#"
model Ball
  Real x(start=0);
equation
  der(x) = -x;
end Ball;
"#;

    render_template(source, "Ball", "Ball.mo")
}

fn render_array_template() -> String {
    let source = r#"
model Arr
  Real x[2](start={1, 2});
equation
  der(x[1]) = -x[1];
  der(x[2]) = -x[2];
end Arr;
"#;

    render_template(source, "Arr", "Arr.mo")
}

fn render_unicycle_template() -> String {
    render_template(unicycle_source(), "Unicycle", "Unicycle.mo")
}

fn unicycle_source() -> &'static str {
    r#"
model Unicycle
  Real x(start=0);
  Real y(start=0);
  Real theta(start=0);
  Real omega_ref;
  Real tau;
  parameter Real v_nom = 2.0;
  parameter Real omega_turn = 0.4;
  parameter Real r_fig8 = 5.0;
  parameter Real dt = 0.1;
  parameter Real pi = 3.14159;
  parameter Real T_fig8 = 10 * pi;
  parameter Real T_lobe = 2 * pi * r_fig8 / v_nom;
algorithm
  tau := time - T_fig8 * floor(time / T_fig8);
  omega_ref := if tau <= T_lobe then omega_turn else -omega_turn;
equation
  der(x) = v_nom * cos(theta);
  der(y) = v_nom * sin(theta);
  der(theta) = omega_ref;
end Unicycle;
"#
}

#[test]
fn sympy_template_renders_against_current_dae_context() {
    let rendered = render_ball_template();

    assert!(
        rendered.contains("class Model:"),
        "expected generated SymPy model class, got:\n{rendered}"
    );
    assert!(
        rendered.contains("self.f_x = _column(["),
        "expected residual matrix in rendered output, got:\n{rendered}"
    );
    assert!(
        rendered.contains("(der(x) - (-x))") || rendered.contains("(der(x)-(-x))"),
        "expected Ball residual in rendered output, got:\n{rendered}"
    );
    assert!(
        rendered.contains("self.explicit_targets = ["),
        "expected explicit solve targets in rendered output, got:\n{rendered}"
    );
}

#[test]
fn sympy_template_uses_indexed_symbols_for_array_states() {
    let rendered = render_array_template();

    assert!(
        rendered.contains("x = sp.IndexedBase(\"x\")"),
        "expected array states to use IndexedBase, got:\n{rendered}"
    );
    assert!(
        rendered.contains("der(x[1])")
            || rendered.contains("der(x[0])")
            || rendered.contains("der(x_1)")
            || rendered.contains("der(x_1_)"),
        "expected explicit derivative targets for array state equations, got:\n{rendered}"
    );
}

#[test]
fn sympy_template_dae_context_exposes_condition_aliases() {
    let value = render_dae_template_json(unicycle_source(), "Unicycle", "Unicycle.mo");
    let aliases = value
        .get("condition_aliases")
        .and_then(serde_json::Value::as_array)
        .expect("condition aliases should be present");

    assert_eq!(aliases.len(), 1);
    let condition_name = &aliases[0]["condition"]["VarRef"]["name"];
    assert_eq!(condition_name["name"], "c[1]");
    assert!(condition_name["component_ref"].is_object());
    assert!(aliases[0]["relation"]["Binary"].is_object());
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn sympy_template_executes_against_current_dae_context() {
    let rendered = render_ball_template();

    assert_python_executes(
        &rendered,
        r#"
assert str(model.x[0]) == "x"
assert str(model.explicit_targets[0]) == "x_dot"
assert str(solution[model.explicit_targets[0]]) == "-x"
"#,
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn sympy_template_executes_with_indexed_array_states() {
    let rendered = render_array_template();

    assert_python_executes(
        &rendered,
        r#"
assert str(model.x[0]) == "x"
targets = [str(target) for target in model.explicit_targets]
assert targets == ["x_dot[1]", "x_dot[2]"] or targets == ["x_1_dot", "x_2_dot"] or targets == ["x_1__dot", "x_2__dot"]
assert str(solution[model.explicit_targets[0]]) in {"-x[1]", "-x_1", "-x_1_"}
assert str(solution[model.explicit_targets[1]]) in {"-x[2]", "-x_2", "-x_2_"}
"#,
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn sympy_template_solves_piecewise_explicit_rows() {
    let rendered = render_unicycle_template();

    assert_python_executes(
        &rendered,
        r#"
targets = [str(target) for target in model.explicit_targets]
assert targets == ["x_dot", "y_dot", "theta_dot", "tau", "omega_ref"], targets
assert len(solution) == len(model.explicit_targets), solution
rhs = [str(value) for value in model.explicit_rhs]
assert "Piecewise" in rhs[2], rhs
assert "floor" in rhs[3], rhs
assert "Piecewise" in rhs[4], rhs
raw_piecewise = module.if_else(
    module.sp.Symbol("tau") <= module.sp.Symbol("T_lobe"),
    module.sp.Symbol("omega_turn"),
    -module.sp.Symbol("omega_turn"),
)
assert raw_piecewise.has(module.sp.Symbol("tau")), raw_piecewise
assert raw_piecewise.subs({
    module.sp.Symbol("tau"): 1,
    module.sp.Symbol("T_lobe"): 2,
    module.sp.Symbol("omega_turn"): 0.4,
}) == 0.4
assert raw_piecewise.subs({
    module.sp.Symbol("tau"): 3,
    module.sp.Symbol("T_lobe"): 2,
    module.sp.Symbol("omega_turn"): 0.4,
}) == -0.4
piecewise_rhs = solution[model.explicit_targets[4]]
condition_memory = module.sp.Symbol("c[1]", boolean=True)
assert piecewise_rhs.has(condition_memory), piecewise_rhs
assert not piecewise_rhs.has(module.sp.Symbol("T_lobe")), piecewise_rhs
assert not piecewise_rhs.has(module.sp.Symbol("time")), piecewise_rhs
assert piecewise_rhs.subs({
    condition_memory: True,
    module.sp.Symbol("omega_turn"): 0.4,
}) == 0.4
assert piecewise_rhs.subs({
    condition_memory: False,
    module.sp.Symbol("omega_turn"): 0.4,
}) == -0.4
"#,
    );
}
