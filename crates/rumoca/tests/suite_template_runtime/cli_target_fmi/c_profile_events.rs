//! SPEC_0044 ME-EVENT-002: the packaged FMI 2 and 3 components execute the
//! events `rumoca sim` executes, and their Model Exchange and Co-Simulation
//! traces agree with the simulation's at every output point.
//!
//! Each case is one Modelica model, the simulation's trace of it, and the
//! trace FMPy records from the packaged FMU in both interfaces. A grid point
//! away from every event instant compares to the integrator tolerance; the
//! value at an event instant is its right limit in both runs.

use super::*;

const SOURCE: &str = r#"
model WhenRelation
  Real x(start = 0, fixed = true);
  Integer n(start = 0, fixed = true);
equation
  der(x) = 1;
  when x > 0.5 then
    n = pre(n) + 1;
  end when;
end WhenRelation;
"#;

/// FMPy's rows of one interface, `interface,time,value...` per line.
const DRIVER: &str = r#"
import sys
from fmpy import simulate_fmu
archive, stop, interval, names = sys.argv[1], float(sys.argv[2]), float(sys.argv[3]), sys.argv[4].split(",")
for interface in ["ModelExchange", "CoSimulation"]:
    rows = simulate_fmu(archive, fmi_type=interface, stop_time=stop, output_interval=interval, output=names)
    for row in rows:
        print(interface + "," + ",".join(repr(float(value)) for value in row))
"#;

struct Case {
    model: &'static str,
    variables: &'static [&'static str],
    stop: f64,
    interval: f64,
}

type Rows = Vec<(f64, Vec<f64>)>;

fn simulated(compiled: &rumoca::CompilationResult, case: &Case) -> Rows {
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: case.stop,
            dt: Some(case.interval),
            ..rumoca_sim::SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{} simulates: {error}", case.model));
    let columns: Vec<&Vec<f64>> = case
        .variables
        .iter()
        .map(|name| {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("{name} is recorded by the simulation"));
            &result.data[index]
        })
        .collect();
    result
        .times
        .iter()
        .enumerate()
        .map(|(row, time)| (*time, columns.iter().map(|column| column[row]).collect()))
        .collect()
}

fn fmu_rows(driver: &Path, archive: &Path, case: &Case) -> [Rows; 2] {
    let output = checked_output(
        Command::new("python3")
            .arg(driver)
            .arg(archive)
            .arg(case.stop.to_string())
            .arg(case.interval.to_string())
            .arg(case.variables.join(",")),
        &format!("{} FMPy trace", case.model),
    );
    let mut interfaces: [Rows; 2] = [Vec::new(), Vec::new()];
    for line in String::from_utf8_lossy(&output.stdout).lines() {
        let (interface, values) = line.split_once(',').expect("an interface-tagged row");
        let values: Vec<f64> = values
            .split(',')
            .map(|value| value.parse().expect("a numeric FMPy value"))
            .collect();
        let slot = usize::from(interface == "CoSimulation");
        interfaces[slot].push((values[0], values[1..].to_vec()));
    }
    interfaces
}

/// The last row recorded at output point `time`: an event instant is recorded
/// before and after its event, and the later row is the right limit.
fn last_row_at(rows: &Rows, time: f64) -> Option<&Vec<f64>> {
    rows.iter()
        .rev()
        .find(|(candidate, _)| (candidate - time).abs() <= 1.0e-9)
        .map(|(_, values)| values)
}

fn assert_trace_agrees(case: &Case, label: &str, expected: &Rows, actual: &Rows) {
    let points = (case.stop / case.interval).round() as usize;
    for point in 0..=points {
        let time = point as f64 * case.interval;
        let want = last_row_at(expected, time)
            .unwrap_or_else(|| panic!("{} simulation lacks t = {time}", case.model));
        let got = last_row_at(actual, time)
            .unwrap_or_else(|| panic!("{} {label} lacks t = {time}", case.model));
        for ((name, want), got) in case.variables.iter().zip(want).zip(got) {
            assert!(
                (want - got).abs() <= 1.0e-6,
                "{} {label} {name} at t = {time}: simulation {want}, FMU {got}",
                case.model
            );
        }
    }
}

fn assert_cases_agree(source: &str, cases: &[Case]) {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("event FMI work directory");
    let driver = work.path().join("events.py");
    fs::write(&driver, DRIVER).expect("write the FMPy event driver");
    for case in cases {
        let compiled = rumoca::Compiler::new()
            .model(case.model)
            .compile_str(source, "Events.mo")
            .unwrap_or_else(|error| panic!("compile {}: {error:?}", case.model));
        let expected = simulated(&compiled, case);
        for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
            let fmu = build_named_fmu(&work.path().join(case.model), &compiled, target, case.model);
            validate_source_package(&fmu, standard);
            let [exchange, co_simulation] = fmu_rows(&driver, &fmu.archive, case);
            assert_trace_agrees(
                case,
                &format!("{target} ModelExchange"),
                &expected,
                &exchange,
            );
            assert_trace_agrees(
                case,
                &format!("{target} CoSimulation"),
                &expected,
                &co_simulation,
            );
        }
    }
}

#[test]
fn packaged_fmi_when_clauses_on_state_relations_match_the_simulation() {
    assert_cases_agree(
        SOURCE,
        &[Case {
            model: "WhenRelation",
            variables: &["x", "n"],
            stop: 1.0,
            interval: 0.2,
        }],
    );
}
