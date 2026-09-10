//! Issue #346: explicit output equations remain executable in source FMUs.

use super::*;

#[test]
fn packaged_fmi2_and_fmi3_execute_reported_output_equation_and_pid() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("reported-model FMI work directory");
    let driver = work.path().join("reported_models.py");
    fs::write(&driver, DRIVER).expect("write independent importer driver");
    for (model, source) in [
        ("B", REPORTED_MODEL),
        ("PID", PID_MODEL),
        ("Feedback", FEEDBACK_MODEL),
    ] {
        let compiled = rumoca::Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .expect("compile reported explicit output equations");
        for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
            let fmu = build_named_fmu(work.path(), &compiled, target, model);
            validate_source_package(&fmu, standard);
            checked_output(
                Command::new("python3")
                    .arg(&driver)
                    .arg(&fmu.archive)
                    .arg(model),
                &format!("execute {target} {model} through ME and CS"),
            );
        }
    }
}

const REPORTED_MODEL: &str = r#"
model B
  input Real u;
  output Real y;
  Real x(start = 1.0, fixed = true);
equation
  der(x) = -x + u;
  y = 2.0 * x;
end B;
"#;

const PID_MODEL: &str = r#"
block PID
  input Real u;
  output Real y;
  parameter Real Kp = 2;
  parameter Real Ki = 0.5;
  parameter Real Kd = 0.25;
  parameter Real Tf = 0.5;
  Real xi(start = 0, fixed = true);
  Real xd(start = 0, fixed = true);
equation
  der(xi) = u;
  der(xd) = (u - xd) / Tf;
  y = Kp * u + Ki * xi + Kd * (u - xd) / Tf;
end PID;
"#;

const FEEDBACK_MODEL: &str = r#"
model Feedback
  input Real u;
  output Real y;
  Real x(start = 1, fixed = true);
equation
  der(x) = -y + u;
  y = 2*x;
end Feedback;
"#;

const DRIVER: &str = r#"
import math
import sys
from fmpy import simulate_fmu

model = sys.argv[2]
for interface in ['ModelExchange', 'CoSimulation']:
    for u in [0.0, 2.0, -1.0]:
        outputs = ['u', 'xi', 'xd', 'y'] if model == 'PID' else ['u', 'x', 'y']
        trace = simulate_fmu(sys.argv[1], fmi_type=interface, start_time=0.0,
            stop_time=1.0, output_interval=0.01, start_values={'u': u},
            output=outputs, relative_tolerance=1e-8)
        assert len(trace) == 101, (model, interface, len(trace))
        for row in trace:
            t = float(row['time'])
            if model == 'B':
                x = u + (1.0 - u) * math.exp(-t)
                expected = {'u': u, 'x': x, 'y': 2.0*x}
            elif model == 'Feedback':
                x = u/2.0 + (1.0 - u/2.0) * math.exp(-2.0*t)
                expected = {'u': u, 'x': x, 'y': 2.0*x}
            else:
                xi = u*t
                xd = u*(1.0 - math.exp(-2.0*t))
                expected = {'u': u, 'xi': xi, 'xd': xd,
                    'y': 2.0*u + 0.5*xi + 0.5*(u-xd)}
            for name, value in expected.items():
                assert abs(row[name] - value) < 1e-5, (
                    model, interface, u, t, name, row[name], value)
"#;
