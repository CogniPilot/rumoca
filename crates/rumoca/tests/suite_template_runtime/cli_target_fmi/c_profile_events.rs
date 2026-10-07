//! SPEC_0044 ME-EVENT-002: the packaged FMI 2 and 3 components execute the
//! events `rumoca sim` executes, and their Model Exchange and Co-Simulation
//! traces agree with the reference at every output point.
//!
//! Each case is one Modelica model, a reference trace, and the trace FMPy
//! records from the packaged FMU in both interfaces. The reference is the
//! simulation's own trace or, for a model driven by an importer-supplied
//! input, its closed form. The value at an event instant is its right limit
//! in every trace.

use super::*;

const SOURCE: &str = r#"
block RelationOnInput
  input Real u = 0;
  output Real y;
equation
  y = if u > 0 then u else 0;
end RelationOnInput;

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
import numpy as np
from fmpy import simulate_fmu
archive, stop, interval, names = sys.argv[1], float(sys.argv[2]), float(sys.argv[3]), sys.argv[4].split(",")
signal = None
if len(sys.argv) > 5 and sys.argv[5]:
    points = [tuple(float(v) for v in point.split(":")) for point in sys.argv[5].split(",")]
    signal = np.array(points, dtype=[("time", np.float64), ("u", np.float64)])
for interface in ["ModelExchange", "CoSimulation"]:
    rows = simulate_fmu(archive, fmi_type=interface, stop_time=stop, output_interval=interval, output=names, input=signal)
    for row in rows:
        print(interface + "," + ",".join(repr(float(value)) for value in row))
"#;

/// The value of every compared variable at one output point.
type Point = Vec<f64>;
/// `(time, values)` rows in recording order.
type Rows = Vec<(f64, Point)>;

/// What the traces of a case are compared with.
enum Reference {
    /// The trace `rumoca sim` records.
    Simulation,
    /// The closed form of a model driven by the input signal: the values at
    /// an output point in Model Exchange and, in Co-Simulation, whose steps
    /// hold the input set at their start, the values of a step starting at
    /// `start`.
    ClosedForm {
        at: fn(time: f64) -> Point,
        held: fn(start: f64) -> Point,
    },
}

struct Case {
    model: &'static str,
    variables: &'static [&'static str],
    stop: f64,
    interval: f64,
    /// Points `(time, value)` of the input `u` the importer applies.
    input: &'static [(f64, f64)],
    reference: Reference,
}

impl Case {
    fn input_argument(&self) -> String {
        self.input
            .iter()
            .map(|(time, value)| format!("{time}:{value}"))
            .collect::<Vec<_>>()
            .join(",")
    }

    fn output_times(&self) -> Vec<f64> {
        (0..=(self.stop / self.interval).round() as usize)
            .map(|point| point as f64 * self.interval)
            .collect()
    }
}

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
            .arg(case.variables.join(","))
            .arg(case.input_argument()),
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
fn last_row_at(rows: &Rows, time: f64) -> &Point {
    rows.iter()
        .rev()
        .find(|(candidate, _)| (candidate - time).abs() <= 1.0e-9)
        .map(|(_, values)| values)
        .unwrap_or_else(|| panic!("no row is recorded at t = {time}"))
}

fn assert_trace_agrees(case: &Case, label: &str, expected: impl Fn(f64) -> Point, actual: &Rows) {
    for time in case.output_times() {
        let (want, got) = (expected(time), last_row_at(actual, time));
        for ((name, want), got) in case.variables.iter().zip(&want).zip(got) {
            assert!(
                (want - got).abs() <= 1.0e-6,
                "{} {label} {name} at t = {time}: reference {want}, FMU {got}",
                case.model
            );
        }
    }
}

fn assert_cases_agree(cases: &[Case]) {
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
            .compile_str(SOURCE, "Events.mo")
            .unwrap_or_else(|error| panic!("compile {}: {error:?}", case.model));
        let simulation = match case.reference {
            Reference::Simulation => simulated(&compiled, case),
            Reference::ClosedForm { .. } => Vec::new(),
        };
        for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
            let fmu = build_named_fmu(&work.path().join(case.model), &compiled, target, case.model);
            validate_source_package(&fmu, standard);
            let [exchange, co_simulation] = fmu_rows(&driver, &fmu.archive, case);
            let label = |interface| format!("{target} {interface}");
            match case.reference {
                Reference::ClosedForm { at, held } => {
                    assert_trace_agrees(case, &label("ModelExchange"), at, &exchange);
                    let interval = case.interval;
                    assert_trace_agrees(
                        case,
                        &label("CoSimulation"),
                        |time| held((time - interval).max(0.0)),
                        &co_simulation,
                    );
                }
                Reference::Simulation => {
                    let reference = |time| last_row_at(&simulation, time).clone();
                    assert_trace_agrees(case, &label("ModelExchange"), reference, &exchange);
                    assert_trace_agrees(case, &label("CoSimulation"), reference, &co_simulation);
                }
            }
        }
    }
}

#[test]
fn packaged_fmi_when_clauses_on_state_relations_match_the_simulation() {
    assert_cases_agree(&[Case {
        model: "WhenRelation",
        variables: &["x", "n"],
        stop: 1.0,
        interval: 0.2,
        input: &[],
        reference: Reference::Simulation,
    }]);
}

/// The input steps from -1 to 2 at t = 0.5, and `y` is the input when positive.
fn input_and_relation(time: f64) -> Point {
    let u = if time >= 0.5 { 2.0 } else { -1.0 };
    vec![u, u.max(0.0)]
}

#[test]
fn packaged_fmi_relations_on_inputs_follow_the_importer_supplied_signal() {
    assert_cases_agree(&[Case {
        model: "RelationOnInput",
        variables: &["u", "y"],
        stop: 1.0,
        interval: 0.25,
        input: &[(0.0, -1.0), (0.5, -1.0), (0.5, 2.0), (1.0, 2.0)],
        reference: Reference::ClosedForm {
            at: input_and_relation,
            held: input_and_relation,
        },
    }]);
}
