use rumoca::Compiler;
use std::{fs, process::Command};

fn native_rhs(source: &str, x: f64) -> Result<f64, String> {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .expect("the public compiler accepts the function");
    let probe = rumoca_sim::eval_dae_at(
        &compiled.dae,
        &rumoca_sim::SimOptions::default(),
        &[("x".to_owned(), x)],
        0.0,
    )
    .expect("the canonical runtime prepares the model");
    if let Some(error) = probe.report.error {
        return Err(error);
    }
    assert_eq!(probe.report.derivatives.len(), 1);
    Ok(probe.report.derivatives[0].value)
}

fn run_rhs(source: &str, probes: &str) {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .expect("the public compiler accepts the function");
    let files = rumoca::render_target_files(&compiled, "Probe", "c-ode", None)
        .expect("the scalar target preserves the function");
    let content = |path: &str| {
        files
            .iter()
            .find(|file| file.path == path)
            .unwrap()
            .content
            .as_str()
    };
    let header = content("Probe_ode.h");
    let implementation = content("Probe_ode.c").replace("#include \"Probe_ode.h\"", "");
    let driver = format!("{header}\n{implementation}\nint main(void) {{\n{probes}\n}}\n");
    let work = tempfile::tempdir().unwrap();
    let input = work.path().join("probe.c");
    let binary = work.path().join("probe");
    fs::write(&input, driver).unwrap();
    let compilation = Command::new("cc")
        .args(["-std=c99", "-Wall", "-Wextra", "-Wpedantic", "-Werror"])
        .arg(&input)
        .arg("-lm")
        .arg("-o")
        .arg(&binary)
        .output()
        .unwrap();
    assert!(
        compilation.status.success(),
        "{}",
        String::from_utf8_lossy(&compilation.stderr)
    );
    let execution = Command::new(binary).output().unwrap();
    assert!(
        execution.status.success(),
        "RHS semantic probe returned {:?}",
        execution.status.code()
    );
}

#[test]
fn exact_integer_arithmetic_survives_a_runtime_real_call() {
    run_rhs(
        r#"
function exactOffset
  input Real u;
  output Real y;
protected
  Integer k;
algorithm
  k := 9007199254740993;
  y := u + mod(k, 7);
end exactOffset;
model Probe
  Real x(start = 1, fixed = true);
equation
  der(x) = exactOffset(x);
end Probe;
"#,
        r#"
double y[RUMOCA_PROBE_Y_LEN] = {1.0};
double out[RUMOCA_PROBE_DERIVATIVE_LEN] = {0};
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 1;
return out[0] == 6.0 ? 0 : 2;
"#,
    );
}

#[test]
fn a_guarded_invalid_array_write_cannot_become_a_successful_rhs() {
    let source = r#"
function guardedWrite
  input Real u;
  output Real y;
protected
  Real a[1];
algorithm
  a[1] := u;
  for i in 1:2 loop
    if i == 1 or u > 0 then
      a[i] := u + 1;
    end if;
  end for;
  y := a[1];
end guardedWrite;
model Probe
  Real x(start = -1, fixed = true);
equation
  der(x) = guardedWrite(x);
end Probe;
"#;
    assert_eq!(native_rhs(source, -1.0).unwrap(), 0.0);
    assert!(
        native_rhs(source, 1.0).is_err(),
        "the active write must fault"
    );
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .expect("the compiler retains the runtime-guarded write");
    for target in [
        "c-ode",
        "rust-ode",
        "rust-fixed-ode",
        "jax-ode",
        "casadi-ode",
        "cuda-ode",
    ] {
        let error = rumoca::render_target_files(&compiled, "Probe", target, None)
            .expect_err("scalar expansion cannot erase the active out-of-range write");
        assert!(
            error.to_string().contains("unsupported-feature:"),
            "{target}: {error:#}"
        );
    }
}

#[test]
fn runtime_integer_conversions_retain_the_checked_domain_failure() {
    let source = r#"
function checkedInteger
  input Real u;
  output Real y;
protected
  Integer k;
algorithm
  k := integer(u);
  y := k;
end checkedInteger;
model Probe
  Real x(start = 1, fixed = true);
equation
  der(x) = checkedInteger(x);
end Probe;
"#;
    assert_eq!(native_rhs(source, -1.3).unwrap(), -2.0);
    assert!(
        native_rhs(source, 1.0e20).is_err(),
        "the i64 domain remains checked"
    );
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .expect("the compiler represents the checked conversion");
    for target in [
        "c-ode",
        "rust-ode",
        "rust-fixed-ode",
        "jax-ode",
        "casadi-ode",
        "cuda-ode",
    ] {
        let error = rumoca::render_target_files(&compiled, "Probe", target, None)
            .expect_err("a scalar target cannot turn a checked conversion into an unbounded Real");
        assert!(
            error.to_string().contains("unsupported-feature:"),
            "{target}: {error:#}"
        );
    }
}

#[test]
fn a_statically_inactive_invalid_write_is_not_expanded() {
    run_rhs(
        r#"
function staticGuard
  input Real u;
  output Real y;
protected
  Real a[1];
algorithm
  a[1] := u;
  for i in 1:2 loop
    if i == 1 then
      a[i] := u + 1;
    end if;
  end for;
  y := a[1];
end staticGuard;
model Probe
  Real x(start = -1, fixed = true);
equation
  der(x) = staticGuard(x);
end Probe;
"#,
        r#"
double y[RUMOCA_PROBE_Y_LEN] = {-1.0};
double out[RUMOCA_PROBE_DERIVATIVE_LEN] = {123.0};
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 1;
if (out[0] != 0.0) return 2;
y[0] = 1.0;
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 3;
return out[0] == 2.0 ? 0 : 4;
"#,
    );
}

#[test]
fn constant_only_function_loops_are_bounded_before_scalar_expansion() {
    let source = r#"
function constantLoop
  input Real u;
  output Real y;
protected
  Integer n;
algorithm
  n := 0;
  for i in 1:4000001 loop
    n := n + 1;
  end for;
  y := u + n;
end constantLoop;
model Probe
  Real x(start = 1, fixed = true);
equation
  der(x) = constantLoop(x);
end Probe;
"#;
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(source, "Probe.mo")
        .expect("canonical typed function loops remain compact");
    let error = rumoca::render_target_files(&compiled, "Probe", "c-ode", None)
        .expect_err("constant work must obey the scalar expansion budget");
    assert!(
        error.to_string().contains("unsupported-feature:"),
        "{error:#}"
    );
}

#[test]
fn tensor_call_views_and_products_preserve_row_major_values() {
    let source = r#"
function tensorViews
  input Real u;
  output Real y;
protected
  Real t[3,2];
  Real p[3];
  Real c[3];
  Real v[6];
  Real w[3];
  Real d[3,3];
  Real e[3,3];
algorithm
  t := transpose({{u,2,3},{4,5,6}});
  p := t*{1,2};
  c := cross({u,2,3},{4,5,6});
  v := cat(1,p,c);
  w := t[:,2];
  d := diagonal({u,2,3});
  e := identity(3);
  y := sum(v) + sum(w) + sum(d*{1,2,3}) + sum(e*{u,2,3});
end tensorViews;
model Probe
  Real x(start = 1, fixed = true);
equation
  der(x) = tensorViews(x);
end Probe;
"#;
    // Independently: sum(p)=u+35, sum(c)=1-u, sum(w)=15,
    // sum(d*{1,2,3})=u+13, sum(e*{u,2,3})=u+5.
    assert_eq!(native_rhs(source, 1.0).unwrap(), 71.0);
    assert_eq!(native_rhs(source, -2.0).unwrap(), 65.0);
    run_rhs(
        source,
        r#"
double y[RUMOCA_PROBE_Y_LEN] = {1.0};
double out[RUMOCA_PROBE_DERIVATIVE_LEN] = {0};
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 1;
if (out[0] != 71.0) return 2;
y[0] = -2.0;
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 3;
return out[0] == 65.0 ? 0 : 4;
"#,
    );
}

#[test]
fn finite_fold_break_and_runtime_branch_keep_the_carried_values() {
    let source = r#"
function boundedSum
  input Real u;
  output Real y;
protected
  Real a[5];
  Real s;
algorithm
  for i in 1:5 loop
    a[i] := u + i;
  end for;
  s := 0;
  for i in 1:5 loop
    if i == 4 then
      break;
    end if;
    s := s + a[i];
  end for;
  if u > 0 then
    y := s;
  else
    y := -s;
  end if;
end boundedSum;
model Probe
  Real x(start = 1, fixed = true);
equation
  der(x) = boundedSum(x);
end Probe;
"#;
    // Only indices 1,2,3 contribute, so s=3*u+6. The branch is strict.
    assert_eq!(native_rhs(source, 1.0).unwrap(), 9.0);
    assert_eq!(native_rhs(source, -3.0).unwrap(), 3.0);
    assert_eq!(native_rhs(source, 0.0).unwrap(), -6.0);
    run_rhs(
        source,
        r#"
double y[RUMOCA_PROBE_Y_LEN] = {1.0};
double out[RUMOCA_PROBE_DERIVATIVE_LEN] = {0};
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 1;
if (out[0] != 9.0) return 2;
y[0] = -3.0;
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 3;
if (out[0] != 3.0) return 4;
y[0] = 0.0;
if (Probe_derivative_rhs(0.0, y, NULL, out) != 0) return 5;
return out[0] == -6.0 ? 0 : 6;
"#,
    );
}
