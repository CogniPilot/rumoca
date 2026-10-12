//! Execute the shared selected-region scalar view through its real consumers.

use super::*;

const SOURCE: &str = r#"
function vectorRoot
  input Real u[2];
  input Boolean positive;
  output Real y[2];
algorithm
  for k in 1:2 loop
    if positive then
      if u[k] > 0 then
        y[k] := sqrt(u[k]);
      else
        y[k] := sqrt(-u[k]);
      end if;
    else
      y[k] := 2 * u[k];
    end if;
  end for;
end vectorRoot;
model Probe
  Real x(start=1,fixed=true);
equation
  der(x)=sum(vectorRoot({x,4*x},noEvent(x < 0 or x > 0)))/3;
end Probe;
"#;

fn render(target: &str) -> (tempfile::TempDir, Vec<RenderedTargetFile>) {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(SOURCE, "selected_vector.mo")
        .expect("the original checked function compiles");
    for (x, slope) in [(-1.0, -0.5), (1.0, 0.5)] {
        let overrides = [("x".to_owned(), x)];
        let native = rumoca_sim::eval_dae_at(&compiled.dae, &Default::default(), &overrides, 0.0)
            .expect("native selected regions prepare")
            .report;
        assert!(native.error.is_none(), "{:?}", native.error);
        assert_eq!(native.derivatives.len(), 1);
        assert_eq!(native.derivatives[0].value, 1.0);
        let jacobian =
            rumoca_sim::jacobian_for_dae(&compiled.dae, &Default::default(), &overrides, 0.0)
                .expect("native checked AD prepares")
                .report;
        assert!(jacobian.error.is_none(), "{:?}", jacobian.error);
        assert_eq!(jacobian.matrix, vec![vec![slope]]);
    }
    let files = rumoca::render_target_files(&compiled, "Probe", target, None)
        .expect("the target preserves checked nested selected regions");
    let directory = tempfile::tempdir().expect("temporary generated source directory");
    for file in &files {
        fs::write(directory.path().join(&file.path), &file.content).unwrap();
    }
    (directory, files)
}

#[test]
fn c_ode_selected_regions_keep_inactive_domain_faults_unexecuted() {
    let (directory, _) = render("c-ode");
    let harness = directory.path().join("probe.c");
    fs::write(
        &harness,
        r#"#include "Probe_ode.h"
#include <fenv.h>
#include <math.h>
int main(void) {
    double y[RUMOCA_PROBE_Y_LEN] = {0};
    double out[RUMOCA_PROBE_DERIVATIVE_LEN];
    for (int sign = -1; sign <= 1; sign += 2) {
        y[0] = (double)sign;
        if (feclearexcept(FE_ALL_EXCEPT) != 0) return 1;
        if (Probe_derivative_rhs(0, y, NULL, out) != 0) return 2;
        if (out[0] != 1.0 || fetestexcept(FE_INVALID)) return 3;
    }
    return 0;
}

"#,
    )
    .unwrap();
    let executable = directory.path().join("selected-regions");
    run_checked(
        Command::new("cc")
            .args(["-std=c99", "-Wall", "-Wextra", "-Wpedantic", "-Werror"])
            .arg(&harness)
            .arg(directory.path().join("Probe_ode.c"))
            .arg("-lm")
            .arg("-o")
            .arg(&executable),
        "compile selected C regions",
    );
    run_checked(&mut Command::new(executable), "execute selected C regions");
}

#[test]
fn c_ode_selected_regions_reject_real_equality_outside_functions() {
    // Retain the original illegal trigger as a language-admission control.
    let source = SOURCE.replace("noEvent(x < 0 or x > 0)", "x <> 0");
    let failure = Compiler::new()
        .model("Probe")
        .compile_str(&source, "selected_vector_real_equality.mo")
        .expect_err("MLS §3.5 forbids Real equality outside functions");
    let rumoca::CompilerError::CompileDiagnosticsError { failures, .. } = failure else {
        panic!("Real equality must retain a typed phase diagnostic: {failure:?}");
    };
    assert!(
        failures
            .iter()
            .any(|failure| failure.error_code.as_deref() == Some("ER029")),
        "{failures:?}"
    );
}

#[test]
fn rust_ode_selected_regions_preserve_typed_captures() {
    check_rust("rust-ode", false);
}

#[test]
fn rust_fixed_ode_selected_regions_preserve_typed_captures() {
    check_rust("rust-fixed-ode", true);
}

fn check_rust(target: &str, fixed: bool) {
    let (directory, files) = render(target);
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".rs"))
        .unwrap();
    fs::copy(
        directory.path().join(&module.path),
        directory.path().join("generated.rs"),
    )
    .unwrap();
    let call = if fixed {
        "let out = generated::derivative_rhs(0.0, &[x], &[]).unwrap();"
    } else {
        "let mut out = [0.0]; generated::derivative_rhs(0.0, &[x], &[], &mut out).unwrap();"
    };
    let harness = directory.path().join("main.rs");
    fs::write(
        &harness,
        format!(
            "mod generated; fn main() {{ for x in [-1.0,1.0] {{ {call} assert_eq!(out, [1.0]); }} }}"
        ),
    )
    .unwrap();
    let executable = directory.path().join("selected-regions");
    run_checked(
        Command::new("rustc")
            .args(["--edition=2024", "-Dwarnings"])
            .arg(&harness)
            .arg("-o")
            .arg(&executable),
        "compile selected Rust regions",
    );
    run_checked(
        &mut Command::new(executable),
        "execute selected Rust regions",
    );
}

#[test]
fn casadi_selected_regions_preserve_values_and_jacobians() {
    check_python("casadi-ode", CASADI_DRIVER);
}

#[test]
fn jax_selected_regions_preserve_vector_boolean_captures_and_scalar_ad() {
    check_python("jax-ode", JAX_DRIVER);
}

fn check_python(target: &str, driver: &str) {
    let (directory, files) = render(target);
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".py"))
        .unwrap();
    run_checked(
        Command::new("python")
            .args(["-c", driver])
            .arg(directory.path().join(&module.path)),
        "execute selected symbolic regions",
    );
}

const CASADI_DRIVER: &str = r#"
import importlib.util, sys, numpy as np
import casadi as ca
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
generated = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generated)
p = generated.init_params(generated.DEFAULT_P)
x = ca.MX.sym('x', 1)
value = generated.rhs(0, x, [], p)
jacobian = ca.Function('jacobian', [x], [ca.jacobian(value, x)])
for point, slope in [(-1.0,-0.5),(1.0,0.5)]:
    np.testing.assert_allclose(np.asarray(generated.rhs(0, [point], [], p)), 1.0, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(np.asarray(jacobian([point])), slope, rtol=1e-12, atol=1e-12)
"#;

const JAX_DRIVER: &str = r#"
import importlib.util, sys, numpy as np
import jax, jax.numpy as jnp
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
generated = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generated)
p = jnp.asarray(generated.init_params(generated.DEFAULT_P))
f = lambda x: generated.rhs(0, x, jnp.zeros(generated.N_U), p)
for point, slope in [(-1.0,-0.5),(1.0,0.5)]:
    x = jnp.asarray([point])
    np.testing.assert_allclose(f(x), 1.0, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(jax.jit(f)(x), 1.0, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(jax.jacfwd(f)(x), slope, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(jax.jacrev(f)(x), slope, rtol=1e-12, atol=1e-12)
    np.testing.assert_allclose(jax.grad(lambda x: jnp.sum(f(x)))(x), slope, rtol=1e-12, atol=1e-12)
"#;
