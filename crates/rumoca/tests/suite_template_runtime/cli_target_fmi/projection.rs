//! Generated components settle coupled algebraic blocks with the linked ME
//! projection (SPEC_0044 §2 trace parity; SPEC_0038 one kernel semantics).
//!
//! Each fixture drives one projection path of the shared kernel and compares
//! the packaged FMI 2 and FMI 3 Model Exchange and Co-Simulation traces with
//! the in-process ME kernel at the same output grid:
//!
//! - `TornLoop`: torn reduced Newton over a three-unknown nonlinear loop.
//! - `ImplicitScalar`: untorn singleton without an isolator (dense Newton).
//! - `ImplicitSquare`: untorn singleton starting on a vanished column, which
//!   the dense path advances off zero (the singular-seed nudge).
//! - `AffineLoop`: small affine block solved densely.
//! - `AffineChain`: 18-unknown affine chain solved through the torn affine
//!   elimination.
//! - `DenseSeeded`: a mixed loop (its derivative row drops the tearing) solved
//!   by the dense block Newton from a start where `log(u)` is undefined, so
//!   the first refresh seeds the block through its row isolations.
//! - `SingularSeed`: a projection stage whose warm-start seed isolates an
//!   unknown through a coefficient that is identically zero, so every refresh
//!   restores the seed targets and block unknowns and projects that block
//!   alone, keeping the complete plan only for a failed block projection.
//!
//! `FallbackFails` then drives that complete-plan fallback itself.

use super::*;

const TOLERANCE: f64 = 1.0e-6;

struct Fixture {
    model: &'static str,
    source: String,
    outputs: &'static [&'static str],
    /// Text the generated FMI 3 model must contain for this path.
    marker: Option<&'static str>,
}

fn affine_chain() -> String {
    let mut source = String::from("model AffineChain\n  output Real x(start=1, fixed=true);\n");
    for k in 1..=20 {
        source.push_str(&format!("  output Real v{k};\n"));
    }
    source.push_str("equation\n  der(x) = -0.1*x - 0.01*v10;\n  v1 = 1 + x;\n");
    for k in 2..=19 {
        source.push_str(&format!(
            "  v{} - (2 + x*x)*v{k} + v{} = 0.1*x;\n",
            k - 1,
            k + 1
        ));
    }
    source.push_str("  v20 + 0.5*v1 = x;\nend AffineChain;\n");
    source
}

fn fixtures() -> Vec<Fixture> {
    vec![
        Fixture {
            model: "TornLoop",
            source: "model TornLoop
  output Real x(start=1, fixed=true);
  output Real a(start=1);
  output Real b(start=1);
  output Real c(start=1);
equation
  der(x) = -0.5*a;
  a = x + 0.2*sin(c);
  b = a*a + 0.1*a;
  c = 1 + b - 0.3*cos(b) + 0.1*sin(c);
end TornLoop;"
                .to_string(),
            outputs: &["x", "a", "b", "c"],
            marker: Some("rmc_project_stage"),
        },
        Fixture {
            model: "ImplicitScalar",
            source: "model ImplicitScalar
  output Real x(start=1, fixed=true);
  output Real u(start=0.5);
equation
  der(x) = -u;
  u + 0.5*sin(u) + 0.1*u*u*u = x;
end ImplicitScalar;"
                .to_string(),
            outputs: &["x", "u"],
            marker: Some("rmc_project_stage"),
        },
        Fixture {
            model: "ImplicitSquare",
            source: "model ImplicitSquare
  output Real x(start=1, fixed=true);
  output Real u(start=0);
equation
  der(x) = -0.5*u;
  u*u = 1 + x*x;
end ImplicitSquare;"
                .to_string(),
            outputs: &["x", "u"],
            marker: Some("rmc_project_stage"),
        },
        Fixture {
            model: "AffineLoop",
            source: "model AffineLoop
  output Real x(start=1, fixed=true);
  output Real i1;
  output Real i2;
equation
  der(x) = -i1 - 0.5*i2;
  (2 + x*x)*i1 + i2 = 1 + x;
  i1 - (3 + x*x)*i2 = x;
end AffineLoop;"
                .to_string(),
            outputs: &["x", "i1", "i2"],
            marker: Some("rmc_project_stage"),
        },
        Fixture {
            model: "AffineChain",
            source: affine_chain(),
            outputs: &["x", "v1", "v5", "v10", "v18"],
            marker: Some("rmc_eliminate_solve"),
        },
        Fixture {
            model: "DenseSeeded",
            source: "model DenseSeeded
  output Real x(start=1, fixed=true);
  output Real u(start=-1);
  output Real v(start=0.5);
equation
  der(x) = -u - v;
  u + 0.1*der(x) = 0.2*sin(v) + x;
  log(u) + v*v*v + v = x;
end DenseSeeded;"
                .to_string(),
            outputs: &["x", "u", "v"],
            marker: Some("rmc_seed_assignments(m, b)"),
        },
        Fixture {
            model: "SingularSeed",
            source: "model SingularSeed
  Real x(start=0, fixed=true);
  output Real y(start=1, fixed=true);
  output Real t(start=1);
  output Real s(start=1);
  output Real u(start=1);
equation
  der(x) = 0;
  der(y) = -0.2*t - 0.1*y;
  sin(x)*t + s = 1 + 0.5*y;
  t + u*u*u = 3;
  s - u*u = 0.5;
end SingularSeed;"
                .to_string(),
            outputs: &["y", "t", "s", "u"],
            marker: Some("rescue_0:"),
        },
    ]
}

#[test]
fn packaged_fmi_projection_paths_match_the_linked_me_kernel() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("projection FMI work directory");
    let driver = work.path().join("projection_trace.py");
    fs::write(&driver, DRIVER).expect("write projection trace driver");
    for fixture in fixtures() {
        let compiled = rumoca::Compiler::new()
            .model(fixture.model)
            .compile_str(&fixture.source, &format!("{}.mo", fixture.model))
            .unwrap_or_else(|error| panic!("compile {}: {error:?}", fixture.model));
        let reference = in_process_trace(&compiled, fixture.outputs);
        for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
            let fmu = build_named_fmu(work.path(), &compiled, target, fixture.model);
            if let Some(marker) = fixture.marker {
                let source = fs::read_to_string(fmu.root.join("sources/model.c"))
                    .expect("read generated model source");
                assert!(
                    source.contains(marker),
                    "{} {target} does not emit the projection path `{marker}`",
                    fixture.model
                );
            }
            validate_source_package(&fmu, standard);
            for interface in ["ModelExchange", "CoSimulation"] {
                let csv = work
                    .path()
                    .join(format!("{}-{target}-{interface}.csv", fixture.model));
                checked_output(
                    Command::new("python3")
                        .arg(&driver)
                        .arg(&fmu.archive)
                        .arg(interface)
                        .arg(fixture.outputs.join(","))
                        .arg(&csv),
                    &format!("{} {target} {interface} trace", fixture.model),
                );
                assert_projection_trace(fixture.model, &csv, &reference);
            }
        }
    }
}

/// In-process ME-kernel trace on the FMU output grid, one row per time.
fn in_process_trace(compiled: &rumoca::CompilationResult, outputs: &[&str]) -> Vec<Vec<f64>> {
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            dt: Some(0.01),
            rtol: 1.0e-10,
            atol: 1.0e-12,
            ..Default::default()
        },
    )
    .expect("in-process simulation");
    let columns = outputs
        .iter()
        .map(|name| {
            result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("in-process trace lacks {name}"))
        })
        .collect::<Vec<_>>();
    result
        .times
        .iter()
        .enumerate()
        .map(|(row, &time)| {
            std::iter::once(time)
                .chain(columns.iter().map(|&column| result.data[column][row]))
                .collect()
        })
        .collect()
}

fn assert_projection_trace(model: &str, csv: &Path, reference: &[Vec<f64>]) {
    let text = fs::read_to_string(csv).expect("read projection trace");
    let rows = text
        .lines()
        .skip(1)
        .map(|line| {
            line.split(',')
                .map(|value| value.parse::<f64>().expect("numeric trace value"))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    assert_eq!(rows.len(), reference.len(), "{model}: trace length");
    for (row, expected) in rows.iter().zip(reference) {
        assert_close(row[0], expected[0], 1.0e-9);
        for (actual, expected) in row.iter().zip(expected).skip(1) {
            assert!(
                (actual - expected).abs() <= TOLERANCE,
                "{model} at t={}: FMU {actual:.12e} vs in-process {expected:.12e}",
                row[0]
            );
        }
    }
}

const DRIVER: &str = r#"
import sys
from fmpy import simulate_fmu

fmu, interface, names, out = sys.argv[1], sys.argv[2], sys.argv[3].split(','), sys.argv[4]
trace = simulate_fmu(fmu, fmi_type=interface, start_time=0.0, stop_time=1.0,
    output_interval=0.01, output=names, relative_tolerance=1e-10)
with open(out, 'w') as handle:
    handle.write(','.join(['time'] + names) + '\n')
    for row in trace:
        handle.write(','.join(repr(float(row[name])) for name in ['time'] + names) + '\n')
"#;

/// A projection stage whose seed is unavailable and whose block has no real
/// solution once `y < -1` (`s = 1 + 0.5*y` and `s = u*u + 0.5`): the rescue
/// projects the block alone, fails, restores the incoming coordinate, and runs
/// the complete-plan fallback, which fails too. The linked ME kernel fails the
/// same trajectory, and the packaged FMU reports the fallback's own failure.
const FALLBACK_FAILS: &str = "model FallbackFails
  Real x(start=0, fixed=true);
  output Real y(start=1, fixed=true);
  output Real t(start=1);
  output Real s(start=1);
  output Real u(start=1);
equation
  der(x) = 0;
  der(y) = -1;
  sin(x)*t + s = 1 + 0.5*y;
  t + u*u*u = 3;
  s - u*u = 0.5;
end FallbackFails;";

const FALLBACK_MESSAGE: &str =
    "complete algebraic projection did not establish coordinate convergence";

#[test]
fn packaged_fmi_runs_the_complete_plan_after_a_failed_rescue() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let compiled = rumoca::Compiler::new()
        .model("FallbackFails")
        .compile_str(FALLBACK_FAILS, "FallbackFails.mo")
        .expect("compile FallbackFails");
    let linked = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 3.0,
            dt: Some(0.01),
            ..Default::default()
        },
    );
    assert!(
        linked.is_err(),
        "the linked ME kernel fails once the block has no real solution"
    );

    let work = tempdir().expect("fallback FMI work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", "FallbackFails");
    compile_packaged_sources(&fmu);
    let source =
        fs::read_to_string(fmu.root.join("sources/model.c")).expect("read generated model source");
    assert!(
        source.contains("rescue_0:"),
        "the seeded stage rescues its block"
    );
    assert_eq!(
        source.matches("rmc_project_complete(m, RMC_P(").count(),
        source.matches("\nfallback:\n").count(),
        "the staged plans project the complete plan only in their fallback"
    );
    let driver = work.path().join("fallback_trace.py");
    fs::write(&driver, FALLBACK_DRIVER).expect("write fallback driver");
    let output = Command::new("python3")
        .arg(&driver)
        .arg(&fmu.archive)
        .output()
        .expect("run the fallback driver");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success() && stdout.contains("FAILED") && stdout.contains(FALLBACK_MESSAGE),
        "the FMU must fail through its complete-plan fallback:\n{stdout}\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

const FALLBACK_DRIVER: &str = r#"
import sys
from fmpy import simulate_fmu

messages = []
def logger(environment, status, category, message):
    messages.append(message.decode() if isinstance(message, bytes) else str(message))

try:
    simulate_fmu(sys.argv[1], fmi_type='ModelExchange', start_time=0.0, stop_time=3.0,
        output_interval=0.01, logger=logger)
    print('COMPLETED')
except Exception as error:
    print('FAILED', type(error).__name__)
for message in messages:
    print(message)
"#;

/// Build a packaged source FMU's platform binary with warnings as errors.
fn compile_packaged_sources(fmu: &BuiltFmu) {
    checked_output(
        Command::new("fmpy")
            .arg("compile")
            .arg(&fmu.archive)
            .arg("--all-warnings")
            .arg("--warning-as-error"),
        &format!("build {} packaged sources with FMPy", fmu.version),
    );
}

/// The refreshed algebraic values at fixed states, not only trajectories: the
/// generated refresh and the linked ME kernel's `eval_at` settle the same
/// coordinates at each state, for FMI 2 and FMI 3 Model Exchange. Every
/// fixture here has a unique real solution per state, so the two warm starts
/// (the FMU's previous coordinate and the model start values) converge to the
/// same point within the refresh tolerance.
#[test]
fn packaged_fmi_refresh_matches_the_linked_kernel_at_fixed_states() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let work = tempdir().expect("fixed-state FMI work directory");
    let driver = work.path().join("fixed_state_values.py");
    fs::write(&driver, FIXED_STATE_DRIVER).expect("write fixed-state driver");
    for fixture in fixtures().into_iter().filter(|fixture| {
        [
            "TornLoop",
            "ImplicitScalar",
            "AffineLoop",
            "AffineChain",
            "DenseSeeded",
        ]
        .contains(&fixture.model)
    }) {
        let compiled = rumoca::Compiler::new()
            .model(fixture.model)
            .compile_str(&fixture.source, &format!("{}.mo", fixture.model))
            .unwrap_or_else(|error| panic!("compile {}: {error:?}", fixture.model));
        let algebraics = &fixture.outputs[1..];
        let linked = linked_fixed_state_values(&compiled, fixture.model, algebraics);
        for target in ["fmi2", "fmi3"] {
            let fmu = build_named_fmu(work.path(), &compiled, target, fixture.model);
            compile_packaged_sources(&fmu);
            let generated = generated_fixed_state_values(&driver, &fmu, algebraics);
            assert_fixed_state_values(
                &format!("{} {target}", fixture.model),
                algebraics,
                &generated,
                &linked,
            );
        }
    }
}

const FIXED_STATES: [f64; 4] = [0.25, 0.6, 1.0, 1.7];

/// The linked ME kernel's refreshed values of `names` at each fixed state.
fn linked_fixed_state_values(
    compiled: &rumoca::CompilationResult,
    model: &str,
    names: &[&str],
) -> Vec<Vec<f64>> {
    FIXED_STATES
        .iter()
        .map(|&state| {
            let probe = rumoca_sim::eval_dae_at(
                &compiled.dae,
                &rumoca_sim::SimOptions::default(),
                &[("x".to_string(), state)],
                0.0,
            )
            .unwrap_or_else(|error| panic!("{model} eval at x={state}: {error:?}"));
            names
                .iter()
                .map(|name| {
                    probe
                        .report
                        .solver_y
                        .iter()
                        .find(|slot| slot.name == *name)
                        .unwrap_or_else(|| panic!("linked eval lacks {name}"))
                        .value
                })
                .collect()
        })
        .collect()
}

/// The packaged FMU's refreshed values of `names` at each fixed state.
fn generated_fixed_state_values(driver: &Path, fmu: &BuiltFmu, names: &[&str]) -> Vec<Vec<f64>> {
    let states = FIXED_STATES.map(|value| value.to_string()).join(",");
    let output = checked_output(
        Command::new("python3")
            .arg(driver)
            .arg(&fmu.archive)
            .arg(fmu.version)
            .arg(&states)
            .arg(names.join(",")),
        &format!("{} fixed-state values", fmu.version),
    );
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(|line| {
            line.split(',')
                .map(|value| value.parse::<f64>().expect("numeric refresh value"))
                .collect()
        })
        .collect()
}

fn assert_fixed_state_values(
    label: &str,
    names: &[&str],
    generated: &[Vec<f64>],
    linked: &[Vec<f64>],
) {
    const VALUE_TOLERANCE: f64 = 1.0e-8;
    assert_eq!(generated.len(), FIXED_STATES.len(), "{label}");
    for ((state, generated), linked) in FIXED_STATES.iter().zip(generated).zip(linked) {
        for ((name, actual), expected) in names.iter().zip(generated).zip(linked) {
            assert!(
                (actual - expected).abs() <= VALUE_TOLERANCE,
                "{label} {name} at x={state}: FMU {actual:.15e} vs linked {expected:.15e}"
            );
        }
    }
}

const FIXED_STATE_DRIVER: &str = r#"
import ctypes, sys
from fmpy import extract, read_model_description

fmu, version, states, names = sys.argv[1], sys.argv[2], sys.argv[3].split(','), sys.argv[4].split(',')
description = read_model_description(fmu)
directory = extract(fmu)
references = {variable.name: variable.valueReference for variable in description.modelVariables}
refs = [references[name] for name in names]
identifier = description.modelExchange.modelIdentifier
if version == 'fmi3':
    from fmpy.fmi3 import FMU3Model
    model = FMU3Model(guid=description.guid, unzipDirectory=directory,
        modelIdentifier=identifier, instanceName='fixed')
    model.instantiate()
    model.enterInitializationMode()
    model.exitInitializationMode()
    model.enterContinuousTimeMode()
    read = model.getFloat64
else:
    from fmpy.fmi2 import FMU2Model
    model = FMU2Model(guid=description.guid, unzipDirectory=directory,
        modelIdentifier=identifier, instanceName='fixed')
    model.instantiate()
    model.setupExperiment()
    model.enterInitializationMode()
    model.exitInitializationMode()
    model.enterContinuousTimeMode()
    read = model.getReal
for state in states:
    value = (ctypes.c_double * 1)(float(state))
    model.setContinuousStates(value, 1)
    print(','.join(repr(float(v)) for v in read(refs)))
model.terminate()
model.freeInstance()
"#;
