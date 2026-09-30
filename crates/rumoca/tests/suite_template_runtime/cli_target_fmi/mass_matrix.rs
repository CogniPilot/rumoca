//! Issue #363: a derivative inside a matrix product, `J * der(w) = f`, lowers
//! to a Solve IR `LinSolve` that the FMI 2 and FMI 3 C kernels evaluate with
//! the linked kernel's dense elimination. The traces of both interfaces match
//! the matrix exponential of `-inv(J)`, including an inertia tensor whose
//! leading entry is zero so the elimination must pivot.

use super::*;

#[test]
fn packaged_fmi_mass_matrix_derivatives_match_the_matrix_exponential() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("mass-matrix FMI work directory");
    let driver = work.path().join("mass_matrix.py");
    fs::write(&driver, DRIVER).expect("write independent importer driver");
    for (model, source, inertia) in [
        ("Inertia", INERTIA, "2,0;0,4"),
        ("PivotedInertia", PIVOTED_INERTIA, "0,2,0;1,0,0;0,0,4"),
    ] {
        let compiled = rumoca::Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .expect("compile a mass-matrix derivative equation");
        for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
            let fmu = build_named_fmu(work.path(), &compiled, target, model);
            validate_source_package(&fmu, standard);
            checked_output(
                Command::new("python3")
                    .arg(&driver)
                    .arg(&fmu.archive)
                    .arg(inertia),
                &format!("execute {target} {model} through ME and CS"),
            );
        }
    }
}

const INERTIA: &str = r#"
model Inertia
  parameter Real J[2,2] = [2, 0; 0, 4];
  Real w[2](each start = 1, each fixed = true);
equation
  J * der(w) = -w;
end Inertia;
"#;

const PIVOTED_INERTIA: &str = r#"
model PivotedInertia
  parameter Real J[3,3] = [0, 2, 0; 1, 0, 0; 0, 0, 4];
  Real w[3](start = {1, -0.5, 2}, each fixed = true);
equation
  J * der(w) = -w;
end PivotedInertia;
"#;

const DRIVER: &str = r#"
import sys
import numpy as np
from fmpy import read_model_description, simulate_fmu

inertia = np.asarray([[float(v) for v in row.split(',')] for row in sys.argv[2].split(';')])
n = inertia.shape[0]
w0 = np.ones(n) if n == 2 else np.asarray([1.0, -0.5, 2.0])
rate = -np.linalg.inv(inertia)
values, vectors = np.linalg.eig(rate)
description = read_model_description(sys.argv[1])
# FMI 3 exports `w` as one array variable, FMI 2 as scalars `w[i]`.
names = ['w'] if description.fmiVersion.startswith('3') else ['w[%d]' % (i + 1) for i in range(n)]
for interface in ['ModelExchange', 'CoSimulation']:
    trace = simulate_fmu(sys.argv[1], fmi_type=interface, start_time=0.0,
        stop_time=1.0, output_interval=0.01, output=names, relative_tolerance=1e-8)
    assert len(trace) == 101, (interface, len(trace))
    for row in trace:
        t = float(row['time'])
        expected = np.real(vectors @ np.diag(np.exp(values * t)) @ np.linalg.solve(vectors, w0))
        actual = np.concatenate([np.atleast_1d(np.asarray(row[name], dtype=float)) for name in names])
        assert np.max(np.abs(actual - expected)) < 1e-6, (interface, t, actual, expected)
"#;
