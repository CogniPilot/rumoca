//! The generated C kernel switches reduced charts exactly as the linked ME
//! kernel does (SPEC_0040 STRUCT-T07 constraint-fold chart rows).
//!
//! `SplitCircleChart` integrates `y` and reconstructs `x` from `x*x + y*y = 1`;
//! that chart folds at every quarter turn, where the exchange integrating `x`
//! is regular. Each packaged FMU must request a basis change at an accepted
//! step before each fold, transfer in Event Mode with changed state values,
//! and stay on the physical branch `x = cos t`, `y = sin t` for a full turn.
//! The fixed-step Model Exchange drive requests each switch at the same
//! accepted step, between the same charts, and with the same conditionings as
//! the linked kernel under the same drive (`fmi_me::fixed_step`), and its
//! states agree with the linked kernel's at every step. It also restores an
//! FMU state taken before a switch and replays the same steps bit for bit, so
//! the active chart travels with the state. A C harness forces a failed basis
//! transfer and checks that it changes nothing.

use super::projection::compile_packaged_sources;
use super::*;

const MODEL: &str = "SplitCircleChart";
const SOURCE: &str = include_str!("../../fixtures/index_reduction/SplitCircleChart.mo");
/// The linked kernel's own bound on the switched trajectory.
const BRANCH_TOLERANCE: f64 = 1.0e-4;
/// Step of the fixed-step drives.
const STEP: f64 = 1.0e-3;
/// Agreement of the generated and linked conditionings and states.
const CONDITIONING_TOLERANCE: f64 = 1.0e-12;
const STATE_TOLERANCE: f64 = 1.0e-9;

/// The linked kernel under the drive the generated component receives.
fn linked_fixed_step(
    compiled: &rumoca::CompilationResult,
) -> rumoca_solver::fmi_me::fixed_step::FixedStepRun {
    let component = rumoca_sim::lower_fmi_component(&compiled.dae)
        .unwrap_or_else(|error| panic!("lower {MODEL}: {error:?}"));
    let artifact = rumoca_solver::fmi_me::MeModelArtifact::new(component);
    let steps = ((2.0 * std::f64::consts::PI + 0.5) / STEP).round();
    rumoca_solver::fmi_me::fixed_step::fixed_step_rk4(&artifact, STEP, steps * STEP)
        .unwrap_or_else(|error| panic!("linked fixed-step drive: {error:?}"))
}

/// One `key=value` field of a switch log line.
fn field(line: &str, key: &str) -> f64 {
    line.split_whitespace()
        .find_map(|part| part.strip_prefix(key)?.strip_prefix('='))
        .and_then(|value| value.parse().ok())
        .unwrap_or_else(|| panic!("switch line lacks {key}: {line}"))
}

fn close(actual: f64, expected: f64, tolerance: f64) -> bool {
    (actual - expected).abs() <= tolerance * expected.abs().max(1.0)
}

/// The generated drive's switches and states against the linked kernel's.
fn assert_linked(stdout: &str, linked: &rumoca_solver::fmi_me::fixed_step::FixedStepRun) {
    let switches = stdout
        .lines()
        .filter_map(|line| line.strip_prefix("SWITCH "))
        .collect::<Vec<_>>();
    assert_eq!(
        switches.len(),
        linked.switches.len(),
        "switch count\n{stdout}"
    );
    for (line, expected) in switches.iter().zip(&linked.switches) {
        let step = line
            .split_whitespace()
            .next()
            .and_then(|step| step.parse::<usize>().ok());
        assert_eq!(step, Some(expected.step), "switch step: {line}");
        assert_eq!(field(line, "from") as usize, expected.from, "{line}");
        assert_eq!(field(line, "to") as usize, expected.to, "{line}");
        for (key, value) in [
            ("sigma_active", expected.sigma_active),
            ("sigma_target", expected.sigma_target),
        ] {
            assert!(
                close(field(line, key), value, CONDITIONING_TOLERANCE),
                "{key}: {line} vs {value:e}"
            );
        }
    }
    let mut compared = 0;
    for line in stdout
        .lines()
        .filter_map(|line| line.strip_prefix("STATE "))
    {
        let values = line
            .split_whitespace()
            .map(|value| value.parse::<f64>().expect("numeric state"))
            .collect::<Vec<_>>();
        let expected = &linked.states[values[0] as usize - 1];
        for (actual, expected) in values[1..].iter().zip(expected) {
            assert!(
                close(*actual, *expected, STATE_TOLERANCE),
                "state at step {}: {actual:e} vs {expected:e}",
                values[0]
            );
        }
        compared += 1;
    }
    assert_eq!(
        compared,
        linked.states.len(),
        "every step's states are compared"
    );
}

#[test]
fn packaged_fmi_switches_reduced_charts_like_the_linked_kernel() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("chart switching work directory");
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, &format!("{MODEL}.mo"))
        .unwrap_or_else(|error| panic!("compile {MODEL}: {error:?}"));
    let linked = linked_fixed_step(&compiled);
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, MODEL);
        let source =
            fs::read_to_string(fmu.root.join("sources/model.c")).expect("read generated model");
        assert!(
            source.contains("#define RMC_NCHARTS 2"),
            "{target}: the primary and its exchange are both executable charts"
        );
        validate_source_package(&fmu, standard);
        compile_packaged_sources(&fmu);
        let drives = [
            ("rk4", ME_DRIVER),
            ("ModelExchange", SIMULATE_DRIVER),
            ("CoSimulation", SIMULATE_DRIVER),
        ];
        for (drive, text) in drives {
            if drive == "rk4" && target != "fmi3" {
                continue;
            }
            let driver = work.path().join(format!("{target}_{drive}.py"));
            fs::write(&driver, text).expect("write chart driver");
            let output = checked_output(
                Command::new("python3")
                    .arg(&driver)
                    .arg(&fmu.archive)
                    .arg(drive),
                &format!("{MODEL} {target} {drive} chart drive"),
            );
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_branch(drive, &stdout);
            if drive == "rk4" {
                assert_linked(&stdout, &linked);
            }
        }
    }
}

/// Every reported row stays on the physical branch, the drive switched at
/// least once per quarter turn, and a restored state replays identically.
fn assert_branch(drive: &str, stdout: &str) {
    let mut worst = 0.0_f64;
    for line in stdout.lines().filter(|line| line.starts_with("ROW ")) {
        let values = line[4..]
            .split(',')
            .map(|value| value.parse::<f64>().expect("numeric row"))
            .collect::<Vec<_>>();
        let (time, x, y) = (values[0], values[1], values[2]);
        worst = worst
            .max((x - time.cos()).abs())
            .max((y - time.sin()).abs());
    }
    assert!(
        worst < BRANCH_TOLERANCE,
        "{drive}: the switched trajectory leaves the physical branch by {worst}\n{stdout}"
    );
    if drive == "rk4" {
        let switches = stdout
            .lines()
            .find_map(|line| line.strip_prefix("SWITCHES "))
            .and_then(|count| count.parse::<usize>().ok())
            .expect("the drive reports its switch count");
        assert_eq!(switches, 4, "one switch per quarter turn\n{stdout}");
        assert!(
            stdout.contains("REPLAY identical"),
            "a restored FMU state replays the switch identically\n{stdout}"
        );
    }
}

/// A fixed-step RK4 Model Exchange drive: every accepted step is completed,
/// and a requested event runs the discrete update and re-reads the states.
/// An FMU state taken at step 1000 on the primary chart is restored after the
/// first switch and the same steps replay to the same bits.
const ME_DRIVER: &str = r#"
import math, shutil, sys, tempfile
from ctypes import c_double
from fmpy import extract, read_model_description
from fmpy.fmi3 import FMU3Model

archive = sys.argv[1]
md = read_model_description(archive)
unzipped = extract(archive, unzipdir=tempfile.mkdtemp())
fmu = FMU3Model(guid=md.instantiationToken, unzipDirectory=unzipped,
    modelIdentifier=md.modelExchange.modelIdentifier, instanceName='charts')
current = [0]
recording = [True]

def log(environment, status, category, message):
    if recording[0] and category == b'chart':
        print('SWITCH %d %s' % (current[0], message.decode()))

fmu.instantiate(loggingOn=True, logMessage=log)
fmu.enterInitializationMode()
fmu.exitInitializationMode()
while fmu.updateDiscreteStates()[0]:
    pass
fmu.enterContinuousTimeMode()
nx = 2
h = 1e-3
x = (c_double * nx)()
fmu.getContinuousStates(x, nx)
refs = {v.name: v.valueReference for v in md.modelVariables}

def derivative(t, state):
    fmu.setTime(t)
    fmu.setContinuousStates((c_double * nx)(*state), nx)
    out = (c_double * nx)()
    fmu.getContinuousStateDerivatives(out, nx)
    return list(out)

def step(s, state):
    current[0] = s
    t0 = (s - 1) * h
    k1 = derivative(t0, state)
    k2 = derivative(t0 + 0.5 * h, [a + 0.5 * h * b for a, b in zip(state, k1)])
    k3 = derivative(t0 + 0.5 * h, [a + 0.5 * h * b for a, b in zip(state, k2)])
    k4 = derivative(t0 + h, [a + h * b for a, b in zip(state, k3)])
    state = [a + h / 6.0 * (b + 2.0 * c + 2.0 * d + e) for a, b, c, d, e in zip(state, k1, k2, k3, k4)]
    fmu.setTime(s * h)
    fmu.setContinuousStates((c_double * nx)(*state), nx)
    event, _ = fmu.completedIntegratorStep()
    if not event:
        return state, 0
    fmu.enterEventMode()
    while fmu.updateDiscreteStates()[0]:
        pass
    fmu.enterContinuousTimeMode()
    out = (c_double * nx)()
    fmu.getContinuousStates(out, nx)
    return list(out), 1

state = list(x)
switches = 0
saved = None
steps = int(round((2.0 * math.pi + 0.5) / h))
for s in range(1, steps + 1):
    if s == 1000:
        saved = (fmu.getFMUState(), list(state))
    state, switched = step(s, state)
    switches += switched
    print('STATE %d %r %r' % (s, state[0], state[1]))
    if s == 2000:
        reached = list(state)
    if s % 100 == 0:
        xy = fmu.getFloat64([refs['x'], refs['y']])
        print('ROW %r,%r,%r' % (s * h, xy[0], xy[1]))
print('SWITCHES', switches)
recording[0] = False
fmu.setFMUState(saved[0])
state = saved[1]
for s in range(1000, 2001):
    state, _ = step(s, state)
print('REPLAY identical' if state == reached else 'REPLAY differs %r %r' % (state, reached))
fmu.freeFMUState(saved[0])
fmu.terminate()
fmu.freeInstance()
shutil.rmtree(unzipped, ignore_errors=True)
"#;

/// FMPy's own drive over a full turn: its Model Exchange solver completes
/// steps and services the requested events; each Co-Simulation step
/// integrates and switches inside the component.
const SIMULATE_DRIVER: &str = r#"
import math, sys
from fmpy import simulate_fmu

trace = simulate_fmu(sys.argv[1], fmi_type=sys.argv[2], start_time=0.0,
    stop_time=2.0 * math.pi + 0.5, output_interval=1e-3, output=['x', 'y'],
    relative_tolerance=1e-10)
for row in trace:
    print('ROW %r,%r,%r' % (float(row['time']), float(row['x']), float(row['y'])))
"#;

/// C harness over the generated `SplitCircleChart` sources: a latched change
/// whose transfer cannot establish the target chart fails as a whole.
const TRANSFER_HARNESS: &str = r#"#include "model.c"
#include <stdio.h>
#define CHECK(condition, what) do { if (!(condition)) { printf("FAILED: %s\n", what); return 1; } } while (0)
int main(void) {
    ModelInstance* m = fmi3InstantiateModelExchange("transfer", HARNESS_TOKEN, "", fmi3False, fmi3False, NULL, NULL);
    CHECK(m != NULL, "instantiate");
    CHECK(fmi3EnterInitializationMode(m, fmi3False, 0.0, 0.0, fmi3False, 0.0) == fmi3OK, "enter initialization");
    CHECK(fmi3ExitInitializationMode(m) == fmi3OK, "exit initialization");
    fmi3Boolean need, terminate, nominals, values, defined; double next;
    CHECK(fmi3UpdateDiscreteStates(m, &need, &terminate, &nominals, &values, &defined, &next) == fmi3OK, "initial event");
    double before[Y_LEN]; memcpy(before, m->y, sizeof(before));
    double reference = m->rmc_chart_reference;
    m->rmc_chart_pending = true; m->rmc_chart_target = 1; m->rmc_chart_target_reference = 0.25;
    for (size_t k = 0; k < Y_LEN; ++k) m->rmc_chart_y[k] = NAN;
    CHECK(fmi3UpdateDiscreteStates(m, &need, &terminate, &nominals, &values, &defined, &next) == fmi3Error, "a transfer that cannot establish the target fails");
    CHECK(m->rmc_chart == 0, "the active chart is kept");
    CHECK(!m->rmc_chart_pending, "the latched change is consumed");
    CHECK(m->rmc_chart_reference == reference, "the keep reference is kept");
    CHECK(memcmp(before, m->y, sizeof(before)) == 0, "the coordinate is restored");
    fmi3FreeInstance(m);
    printf("ok\n");
    return 0;
}
"#;

#[test]
fn generated_chart_transfer_failure_changes_nothing() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let headers = standard_roots().1.root.join("headers");
    if !headers.join("fmi3Functions.h").is_file() {
        return;
    }
    let work = tempdir().expect("chart transfer harness work directory");
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, &format!("{MODEL}.mo"))
        .unwrap_or_else(|error| panic!("compile {MODEL}: {error:?}"));
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", MODEL);
    let description =
        fs::read_to_string(fmu.root.join("modelDescription.xml")).expect("read model description");
    let token = description
        .split("instantiationToken=\"")
        .nth(1)
        .and_then(|rest| rest.split('"').next())
        .expect("instantiation token");
    let sources = fmu.root.join("sources");
    let harness = work.path().join("chart_transfer_harness.c");
    fs::write(&harness, TRANSFER_HARNESS).expect("write the chart transfer harness");
    let binary = work.path().join("chart_transfer_harness");
    checked_output(
        Command::new("cc")
            .arg("-std=c11")
            .arg("-ffp-contract=off")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Werror")
            .arg(format!("-DHARNESS_TOKEN=\"{token}\""))
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(&harness)
            .args(
                [
                    "rmc_assign.c",
                    "rmc_rows.c",
                    "rmc_jacobian.c",
                    "rmc_isolators.c",
                    "rmc_functions.c",
                ]
                .map(|unit| sources.join(unit)),
            )
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the chart transfer harness",
    );
    let output = checked_output(&mut Command::new(&binary), "run the chart transfer harness");
    assert!(
        String::from_utf8_lossy(&output.stdout).starts_with("ok"),
        "{}",
        String::from_utf8_lossy(&output.stdout)
    );
}
