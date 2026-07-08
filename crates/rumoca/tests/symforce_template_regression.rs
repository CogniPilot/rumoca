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

// ============================================================================
// Runtime checks: execute the generated module under an installed symforce.
// Run via `cargo xtask verify template-runtimes`; skipped with a notice when
// the package is missing locally.
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
fn python_command() -> &'static str {
    for candidate in ["python3", "python"] {
        if std::process::Command::new(candidate)
            .arg("--version")
            .output()
            .is_ok()
        {
            return candidate;
        }
    }
    panic!("expected python3 or python to be available");
}

#[cfg(feature = "template-runtime-tests")]
fn python_has_symforce() -> bool {
    std::process::Command::new(python_command())
        .args(["-c", "import symforce"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[cfg(feature = "template-runtime-tests")]
fn strict_runtime_dependencies() -> bool {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/template-runtimes/strict")
        .is_file()
}

#[cfg(feature = "template-runtime-tests")]
fn symforce_available_or_skip() -> bool {
    if python_has_symforce() {
        return true;
    }
    if strict_runtime_dependencies() {
        panic!("symforce not available; strict template runtime checks require it");
    }
    eprintln!("SKIP: symforce not available");
    false
}

#[cfg(feature = "template-runtime-tests")]
fn run_python(rendered: &str, driver: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix("rumoca_symforce_runtime_")
        .tempdir()
        .expect("create temp dir");
    let model_path = dir.path().join("model.py");
    let driver_path = dir.path().join("driver.py");
    std::fs::write(&model_path, rendered).expect("write model.py");
    std::fs::write(&driver_path, driver).expect("write driver.py");

    let output = std::process::Command::new(python_command())
        .arg(driver_path.to_str().unwrap())
        .output()
        .expect("run Python driver");

    assert!(
        output.status.success(),
        "Python execution failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8(output.stdout).expect("stdout is utf8")
}

/// The generated module must build a symbolic DAE under real symforce and the
/// residual must vanish numerically at a consistent point of
/// `der(x) = -k*x` (x=1, k=2, der(x)=-2).
#[cfg(feature = "template-runtime-tests")]
const SYMFORCE_EVAL_DRIVER: &str = r#"
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import model

m = model.create_model()
inputs = m["inputs"]
f_x = m["f_x"]
assert f_x.shape == (1, 1), f"expected one residual row, got {f_x.shape}"

# derivatives Values are keyed by state name; the symbol itself renders
# as der(<state>).
der_x = inputs["derivatives"]["x"]
subs = {
    inputs["states"]["x"]: 1.0,
    inputs["parameters"]["k"]: 2.0,
    der_x: -2.0,
}
residual = float(f_x.subs(subs)[0, 0])
print(f"RESIDUAL={residual}")

off = float(f_x.subs({**subs, der_x: 0.0})[0, 0])
print(f"OFF_RESIDUAL={off}")
"#;

/// The symforce Codegen path (the EKF/controller deployment use case) must
/// produce a compilable C++ residual-with-jacobians function.
#[cfg(feature = "template-runtime-tests")]
const SYMFORCE_CODEGEN_DRIVER: &str = r#"
import os
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import model

out = tempfile.mkdtemp(prefix="rumoca_symforce_codegen_")
model.make_residual_jacobian_codegen().generate_function(
    output_dir=out, namespace="sym"
)
headers = []
for root, _, files in os.walk(out):
    for name in files:
        if name.endswith(".h"):
            headers.append(name)
print(f"HEADERS={sorted(headers)}")
"#;

#[cfg(feature = "template-runtime-tests")]
#[test]
fn symforce_runtime_evaluates_generated_residual() {
    if !symforce_available_or_skip() {
        return;
    }
    let rendered = render_ball_template();
    let stdout = run_python(&rendered, SYMFORCE_EVAL_DRIVER);

    let residual: f64 = stdout
        .lines()
        .find_map(|line| line.strip_prefix("RESIDUAL="))
        .expect("driver prints RESIDUAL=")
        .parse()
        .expect("residual parses as f64");
    assert!(
        residual.abs() < 1.0e-12,
        "residual at the consistent point should vanish, got {residual}"
    );

    let off: f64 = stdout
        .lines()
        .find_map(|line| line.strip_prefix("OFF_RESIDUAL="))
        .expect("driver prints OFF_RESIDUAL=")
        .parse()
        .expect("off residual parses as f64");
    assert!(
        (off - 2.0).abs() < 1.0e-12,
        "residual with der(x)=0 should equal k*x = 2, got {off}"
    );
}

#[cfg(feature = "template-runtime-tests")]
#[test]
fn symforce_runtime_generates_cpp_residual_jacobians() {
    if !symforce_available_or_skip() {
        return;
    }
    let rendered = render_ball_template();
    let stdout = run_python(&rendered, SYMFORCE_CODEGEN_DRIVER);

    let headers = stdout
        .lines()
        .find_map(|line| line.strip_prefix("HEADERS="))
        .expect("driver prints HEADERS=");
    assert!(
        headers.contains("Ball_dae_residual_jacobians.h"),
        "symforce Codegen should emit the residual-with-jacobians header, got {headers}"
    );
}
