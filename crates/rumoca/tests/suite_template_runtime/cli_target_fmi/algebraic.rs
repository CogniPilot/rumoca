//! Exact assignment consumers keep ME/CS outputs current (SPEC_0044 §2).

use super::*;

#[test]
fn packaged_fmi2_and_fmi3_refresh_algebraic_chains_after_input_time_and_state_changes() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("algebraic FMI work directory");
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(
            r#"
model FmiTensorDecay
  output Real x[2](each start=1);
  input Real u=0;
  Real a;
  output Real y;
  output Real z;
equation
  der(x)={-0.5*x[1], -x[2]};
  y=a+time;
  a=2*x[1]+u;
  z=2*x[2];
end FmiTensorDecay;
"#,
            "FmiTensorDecay.mo",
        )
        .expect("compile the exact algebraic chain");
    let driver = work.path().join("algebraic.py");
    fs::write(&driver, DRIVER).expect("write independent importer driver");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_fmu(work.path(), &compiled, target);
        validate_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("execute {target} algebraic ME/CS traces and direct input refresh"),
        );
    }
}

const DRIVER: &str = r#"
import math
import sys
import numpy as np
from fmpy import simulate_fmu

for interface in ['ModelExchange', 'CoSimulation']:
    for input_value in [0.0, 2.0, None]:
        signal = None if input_value is not None else np.array(
            [(0.0, 0.0), (1.0, 2.0)], dtype=[('time', np.float64), ('u', np.float64)])
        trace = simulate_fmu(sys.argv[1], fmi_type=interface, start_time=0.0,
            stop_time=1.0, output_interval=0.1,
            start_values={'u': 0.0 if input_value is None else input_value}, input=signal,
            output=['u', 'y', 'z'], relative_tolerance=1e-8)
        assert len(trace) == 11, (interface, len(trace))
        if input_value is None:
            assert trace[-1]['u'] > trace[0]['u'], 'the importer must change the input'
        for row in trace:
            time = float(row['time'])
            expected_y = 2.0 * math.exp(-0.5*time) + float(row['u']) + time
            expected_z = 2.0 * math.exp(-time)
            assert abs(row['y'] - expected_y) < 1e-5, (interface, time, row, expected_y)
            assert abs(row['z'] - expected_z) < 1e-5, (interface, time, row, expected_z)
"#;

/// A for-family of algebraic equations is one compact call owner whose body
/// is a typed map (SPEC_0043 §6c); the C targets render that map as one loop
/// nest and every element stays a current exact output.
#[test]
fn packaged_fmi_renders_compact_algebraic_families_as_loops() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("family FMI work directory");
    let compiled = rumoca::Compiler::new()
        .model("FmiAlgebraicFamily")
        .compile_str(
            r#"
model FmiAlgebraicFamily
  output Real x(start=1, fixed=true);
  Real g[6];
  Real d[4];
  output Real s;
equation
  der(x) = -x;
  for i in 1:6 loop
    g[i] = x*i;
  end for;
  for i in 1:4 loop
    d[i] = (g[i+2] - g[i])/2.0;
  end for;
  s = d[1] + d[4];
end FmiAlgebraicFamily;
"#,
            "FmiAlgebraicFamily.mo",
        )
        .expect("compile the algebraic families");
    let driver = work.path().join("family.py");
    fs::write(&driver, FAMILY_DRIVER).expect("write independent importer driver");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "FmiAlgebraicFamily");
        validate_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("execute {target} compact algebraic families"),
        );
    }
}

const FAMILY_DRIVER: &str = r#"
import math
import sys
from fmpy import simulate_fmu

for interface in ['ModelExchange', 'CoSimulation']:
    trace = simulate_fmu(sys.argv[1], fmi_type=interface, start_time=0.0,
        stop_time=1.0, output_interval=0.25, output=['x', 's'], relative_tolerance=1e-8)
    for row in trace:
        expected = math.exp(-float(row['time']))
        assert abs(row['x'] - expected) < 1e-5, (interface, row)
        assert abs(row['s'] - 2.0*row['x']) < 1e-12, (interface, row)
"#;
