//! The generated C kernel switches reduced charts exactly as the linked ME
//! kernel does (SPEC_0040 STRUCT-T07 constraint-fold chart rows).
//!
//! `SplitCircleChart` integrates `y` and reconstructs `x` from `x*x + y*y = 1`;
//! that chart folds at every quarter turn, where the exchange integrating `x`
//! is regular. Each packaged FMU must request a basis change at an accepted
//! step before each fold, transfer in Event Mode with changed state values,
//! and stay on the physical branch `x = cos t`, `y = sin t` for a full turn.
//! The Model Exchange drive also restores an FMU state taken before a switch
//! and replays the same steps bit for bit, so the active chart travels with
//! the state.

use super::projection::compile_packaged_sources;
use super::*;

const MODEL: &str = "SplitCircleChart";
const SOURCE: &str = include_str!("../../fixtures/index_reduction/SplitCircleChart.mo");
/// The linked kernel's own bound on the switched trajectory.
const BRANCH_TOLERANCE: f64 = 1.0e-4;

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
            assert_branch(drive, &String::from_utf8_lossy(&output.stdout));
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
fmu.instantiate()
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
    if s == 2000:
        reached = list(state)
    if s % 100 == 0:
        xy = fmu.getFloat64([refs['x'], refs['y']])
        print('ROW %r,%r,%r' % (s * h, xy[0], xy[1]))
print('SWITCHES', switches)
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
