use rumoca::Compiler;
use std::{fs, process::Command};

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
