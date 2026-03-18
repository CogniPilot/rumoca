use std::{fs, process::Command};

use rumoca::Compiler;
use rumoca_phase_codegen::templates;
use tempfile::Builder;

fn python_command() -> &'static str {
    for candidate in ["python3", "python"] {
        if Command::new(candidate).arg("--version").output().is_ok() {
            return candidate;
        }
    }
    panic!("expected python3 or python to be available for SymPy template regression tests");
}

fn render_template(source: &str, model_name: &str, file_name: &str) -> String {
    let compiled = Compiler::new()
        .model(model_name)
        .compile_str(source, file_name)
        .expect("compile test model");

    rumoca_phase_codegen::render_template_with_name(&compiled.dae, templates::SYMPY, model_name)
        .expect("render sympy template")
}

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
            || rendered.contains("der(x_1)"),
        "expected explicit derivative targets for array state equations, got:\n{rendered}"
    );
}

#[test]
#[ignore = "requires Python runtime with sympy; run via `rum verify template-runtimes`"]
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
#[ignore = "requires Python runtime with sympy; run via `rum verify template-runtimes`"]
fn sympy_template_executes_with_indexed_array_states() {
    let rendered = render_array_template();

    assert_python_executes(
        &rendered,
        r#"
assert str(model.x[0]) == "x"
targets = [str(target) for target in model.explicit_targets]
assert targets == ["x_dot[1]", "x_dot[2]"] or targets == ["x_1_dot", "x_2_dot"]
assert str(solution[model.explicit_targets[0]]) in {"-x[1]", "-x_1"}
assert str(solution[model.explicit_targets[1]]) in {"-x[2]", "-x_2"}
"#,
    );
}
