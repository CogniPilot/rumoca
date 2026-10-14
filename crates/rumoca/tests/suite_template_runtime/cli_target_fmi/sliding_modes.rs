//! SPEC_0044 ME-EVENT-010: a packaged component whose state events chatter in
//! a sliding mode completes in Co-Simulation, its locator applying each
//! crossing at the scan coordinate instead of refining it to the location
//! tolerance. Model Exchange leaves location to the importer, so it is not
//! driven here.

use super::*;

/// Below the limit `i` is pushed up, above it down: it slides along `i = -1`.
const SOURCE: &str = r#"
model Slide
  Real i(start = -0.9, fixed = true);
equation
  der(i) = if i < -1 then 1 else -1;
end Slide;
"#;

/// A Co-Simulation step overshoots the surface by at most one communication
/// interval at `|der(i)| = 1`.
const DRIVER: &str = r#"
import sys
from fmpy import simulate_fmu
archive = sys.argv[1]
r = simulate_fmu(archive, fmi_type="CoSimulation", stop_time=2.0, output_interval=0.01, output=["i"])
assert r[-1][0] == 2.0, r[-1]
worst = max(abs(i + 1.0) for t, i in r if t > 0.2)
assert worst < 0.05, worst
"#;

#[test]
fn packaged_fmi_sliding_modes_complete_on_their_surface() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("sliding-mode FMI work directory");
    let driver = work.path().join("sliding_modes.py");
    fs::write(&driver, DRIVER).expect("write the sliding-mode driver");
    let compiled = rumoca::Compiler::new()
        .model("Slide")
        .compile_str(SOURCE, "Slide.mo")
        .expect("compile the sliding-mode model");
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(&work.path().join(target), &compiled, target, "Slide");
        validate_source_package(&fmu, standard);
        checked_output(
            Command::new("python3").arg(&driver).arg(&fmu.archive),
            &format!("{target} sliding-mode traces"),
        );
    }
}
