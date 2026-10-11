//! Runtime function conditionals must preserve selected-branch differentiation.

use super::*;

const SOURCE: &str = r#"
function selectedRoot
  input Real u;
  output Real y;
algorithm
  if u > 0 then
    y := sqrt(u);
  else
    y := sqrt(-u);
  end if;
end selectedRoot;
model Probe
  Real x(start=1,fixed=true);
equation
  der(x)=selectedRoot(x);
end Probe;
"#;

#[test]
fn jax_runtime_selected_sqrt_regions_match_values_and_both_ad_modes() {
    check_source(SOURCE, &[(-1.0, -0.5), (1.0, 0.5)]);
}

#[test]
fn jax_constant_selected_sqrt_region_remains_supported() {
    check_source(&SOURCE.replace("u > 0", "true"), &[(1.0, 0.5)]);
}

fn check_source(source: &str, probes: &[(f64, f64)]) {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "selected_sqrt.mo")
        .expect("public function compiles");
    for &(x, slope) in probes {
        let overrides = [("x".to_owned(), x)];
        let value = rumoca_sim::eval_dae_at(&compiled.dae, &Default::default(), &overrides, 0.0)
            .expect("native selected branch prepares")
            .report;
        assert!(value.error.is_none(), "{:?}", value.error);
        assert_eq!(value.derivatives.len(), 1);
        assert_eq!(value.derivatives[0].value, 1.0);
        let jacobian =
            rumoca_sim::jacobian_for_dae(&compiled.dae, &Default::default(), &overrides, 0.0)
                .expect("native checked directional derivative prepares")
                .report;
        assert!(jacobian.error.is_none(), "{:?}", jacobian.error);
        assert_eq!(jacobian.matrix, vec![vec![slope]]);
    }
    let files = rumoca::render_target_files(&compiled, "Probe", "jax-ode", None)
        .expect("a faithful selected-branch target renders");
    let module = files
        .iter()
        .find(|file| file.path.ends_with(".py"))
        .unwrap();
    let work = tempfile::tempdir().unwrap();
    let path = work.path().join(&module.path);
    fs::write(&path, &module.content).unwrap();
    let script = format!(
        "probes = {}\n{DRIVER}",
        serde_json::to_string(probes).unwrap()
    );
    let output = Command::new("python")
        .args(["-c", &script])
        .arg(&path)
        .output()
        .expect("start exported JAX conditional probe");
    println!("{}", String::from_utf8_lossy(&output.stdout));
    assert!(
        output.status.success(),
        "exported JAX conditional probe failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

const DRIVER: &str = r#"
import importlib.util, sys, numpy as np
import jax, jax.numpy as jnp
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
generated = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generated)
parameters = jnp.asarray(generated.init_params(generated.DEFAULT_P))
inputs = jnp.zeros(generated.N_U)
f = lambda x: generated.rhs(0.0, x, inputs, parameters)
checks = [
    ('rhs', f, False),
    ('jit(rhs)', jax.jit(f), False),
    ('jacfwd', jax.jacfwd(f), True),
    ('jacrev', jax.jacrev(f), True),
    ('grad', jax.grad(lambda x: jnp.sum(f(x))), True),
]
failures = []
for x, slope in probes:
    for name, check, derivative in checks:
        actual = np.asarray(check(jnp.asarray([x]))).reshape(-1)
        expected = slope if derivative else 1.0
        print(f'{name} at x={x}: {actual.tolist()}, expected {expected}', flush=True)
        if actual.size != 1 or not np.isfinite(actual).all() or not np.allclose(actual, expected, rtol=1e-12, atol=1e-12):
            failures.append(f'{name} at x={x}: {actual.tolist()} != {expected}')
assert not failures, '\n'.join(failures)
"#;
