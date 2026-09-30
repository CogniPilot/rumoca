//! Issues #359 and #361: a Co-Simulation communication step is integrated by
//! the component's error-controlled step rule, so a stiff block stays accurate
//! at communication steps far beyond an explicit method's stability limit. The
//! reported PID has a filtered-derivative pole at -100; one classical RK4 step
//! per communication step diverged from `h = 0.03` on while `fmi2DoStep`
//! still returned `fmi2OK`.

use super::*;

#[test]
fn packaged_fmi_co_simulation_steps_a_stiff_pid_within_tolerance() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("co-simulation step work directory");
    let driver = work.path().join("co_simulation_step.py");
    fs::write(&driver, DRIVER).expect("write independent importer driver");
    let compiled = rumoca::Compiler::new()
        .model("PID")
        .compile_str(PID_MODEL, "PID.mo")
        .expect("compile the reported stiff PID");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, "PID");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("step {target} PID through coarse communication steps"),
        );
    }
}

const PID_MODEL: &str = r#"
block PID
  parameter Real Kp = 1;
  parameter Real Ki = 0.1;
  parameter Real Kd = 0.01;
  parameter Real Tf = 0.01;
  input Real u;
  output Real y;
  Real xi(start = 0, fixed = true);
  Real xd(start = 0, fixed = true);
equation
  der(xi) = u;
  der(xd) = (u - xd) / Tf;
  y = Kp * u + Ki * xi + Kd * (u - xd) / Tf;
end PID;
"#;

const DRIVER: &str = r#"
import math
import sys
from fmpy import simulate_fmu

# |h * lambda| from 1 to 50: RK4 alone is stable only up to about 2.785.
for h in [0.01, 0.02, 0.03, 0.05, 0.1, 0.25, 0.5]:
    trace = simulate_fmu(sys.argv[1], fmi_type='CoSimulation', start_time=0.0,
        stop_time=1.0, output_interval=h, start_values={'u': 1.0}, output=['y'])
    assert trace[-1]['time'] >= 1.0 - 1e-9, (h, trace[-1]['time'])
    for row in trace:
        t = float(row['time'])
        exact = 1.0 + 0.1 * t + math.exp(-100.0 * t)
        assert abs(row['y'] - exact) < 1e-4, (h, t, row['y'], exact)
"#;
