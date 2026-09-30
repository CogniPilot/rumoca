//! Issue #364: a public `parameter Integer` sizing an array exports as an
//! Integer variable of the FMI 2 and FMI 3 components, and the component runs.

use super::*;

#[test]
fn packaged_fmi_exports_an_integer_array_size_parameter() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("integer-parameter FMI work directory");
    let driver = work.path().join("integer_parameter.py");
    fs::write(&driver, DRIVER).expect("write independent importer driver");
    let compiled = rumoca::Compiler::new()
        .model("IntN")
        .compile_str(SOURCE, "IntN.mo")
        .expect("compile the reported Integer-sized model");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "IntN");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("execute {target} IntN through ME and CS"),
        );
    }
}

const SOURCE: &str = r#"
model IntN
  parameter Integer n = 3;
  Real x[n](each start = 1, each fixed = true);
equation
  der(x) = -x;
end IntN;
"#;

const DRIVER: &str = r#"
import math
import sys
import numpy as np
from fmpy import read_model_description, simulate_fmu

description = read_model_description(sys.argv[1])
n = [v for v in description.modelVariables if v.name == 'n']
assert len(n) == 1 and n[0].type in ('Integer', 'Int32'), [(v.name, v.type) for v in n]
names = ['x'] if description.fmiVersion.startswith('3') else ['x[1]', 'x[2]', 'x[3]']
for interface in ['ModelExchange', 'CoSimulation']:
    trace = simulate_fmu(sys.argv[1], fmi_type=interface, stop_time=1.0,
        output_interval=0.1, output=names + ['n'], relative_tolerance=1e-8)
    for row in trace:
        assert int(row['n']) == 3, (interface, row)
        actual = np.concatenate([np.atleast_1d(np.asarray(row[name], dtype=float)) for name in names])
        assert actual.shape == (3,), actual
        assert np.max(np.abs(actual - math.exp(-row['time']))) < 1e-6, (interface, row)
"#;
