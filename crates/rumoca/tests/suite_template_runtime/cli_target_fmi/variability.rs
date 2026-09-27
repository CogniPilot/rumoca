//! Parameter variability through an independent importer (SPEC_0044
//! ME-PARAM-001, MLS §4.5, §8.6): a set of an ordinary parameter a state
//! start reads takes effect, and a `final` parameter refuses a set.

use super::*;

const VARIABILITY_MODEL: &str = "StartParameter";

#[test]
fn packaged_fmi2_and_fmi3_honor_a_start_parameter_and_refuse_a_final_set() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("variability FMI work directory");
    let compiled = rumoca::Compiler::new()
        .model(VARIABILITY_MODEL)
        .compile_str(
            r#"
model StartParameter
  parameter Real x0 = 1;
  final parameter Real k = 1;
  output Real x(start = x0, fixed = true);
equation
  der(x) = -k * x;
end StartParameter;
"#,
            "StartParameter.mo",
        )
        .expect("compile the start-parameter model");
    let driver = work.path().join("variability.py");
    fs::write(&driver, DRIVER).expect("write independent importer driver");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, VARIABILITY_MODEL);
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("execute {target} start-parameter sets"),
        );
    }
}

const DRIVER: &str = r#"
import math
import sys
from fmpy import simulate_fmu

for interface in ['ModelExchange', 'CoSimulation']:
    for x0 in [1.0, 3.0]:
        trace = simulate_fmu(sys.argv[1], fmi_type=interface, start_time=0.0,
            stop_time=1.0, output_interval=0.1, start_values={'x0': x0},
            output=['x'], relative_tolerance=1e-8)
        for row in trace:
            expected = x0 * math.exp(-float(row['time']))
            assert abs(row['x'] - expected) < 1e-5, (interface, x0, row, expected)
    try:
        simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=0.1,
            start_values={'k': 2.0})
    except Exception:
        pass
    else:
        raise AssertionError('a final parameter must refuse a set: ' + interface)
"#;
