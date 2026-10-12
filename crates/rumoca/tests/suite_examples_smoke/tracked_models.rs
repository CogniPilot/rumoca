//! Actual shared examples run in the existing library-staged example CI lane.

use std::{collections::BTreeSet, fs, path::PathBuf};

use rumoca::{CompilationResult, Compiler};
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, SimulationSession, simulate_dae};
use rumoca_solver::SimExecutionPolicy;

// File, standalone demo entrypoint, required MSL. Circuit's component models
// are exercised by Circuit.Test; they are not standalone connected circuits.
const EXAMPLES: &[(&str, &str, bool)] = &[
    ("Ball.mo", "Ball", false),
    ("Circuit.mo", "Circuit.Test", false),
    ("FmiTensorDecay.mo", "FmiTensorDecay", false),
    ("GalecCounter.mo", "GalecCounter", false),
    ("KalmanFilterStepTest.mo", "KalmanFilterStepTest", true),
    ("NeuralLatentOscillator.mo", "NeuralLatentOscillator", false),
    ("NeuralODEBackprop.mo", "NeuralODEBackprop", false),
    ("NeuralODEBackprop.mo", "NeuralODEBackprop1k", false),
    ("NeuralODETensor.mo", "NeuralODETensor", false),
    ("NeuralODETensor.mo", "NeuralODETensor100k", false),
    ("NeuralPredatorPrey.mo", "NeuralPredatorPrey", false),
    ("PIDMSL.mo", "PIDMSL", true),
    ("SwitchedRLC.mo", "SwitchedRLC", false),
    ("SwitchedRLC_MSL.mo", "SwitchedRLC_MSL", true),
    ("SympyDecay.mo", "SympyDecay", false),
];

fn repository_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn required_msl_root() -> PathBuf {
    let root = repository_root();
    let text = fs::read_to_string(root.join("examples/modelica_dependencies.toml"))
        .expect("tracked library dependency manifest");
    let manifest: toml::Value = toml::from_str(&text).expect("library dependency TOML");
    let msl = &manifest["libraries"]["msl"];
    let cache = root.join(msl["cache_dir"].as_str().expect("MSL cache directory"));
    let required = msl["required_files"]
        .as_array()
        .expect("MSL required files");
    assert!(!required.is_empty(), "MSL has explicit required files");
    for file in required {
        let path = cache.join(file.as_str().expect("required MSL file path"));
        assert!(
            path.is_file(),
            "required pinned MSL fixture is missing: {}; run cargo xtask repo modelica-deps ensure",
            path.display()
        );
    }
    cache
}

fn compile(name: &str) -> Result<CompilationResult, String> {
    let &(file, _, needs_msl) = EXAMPLES
        .iter()
        .find(|(_, model, _)| *model == name)
        .expect("the requested model belongs to the explicit tracked roster");
    let mut compiler = Compiler::new().model(name);
    if needs_msl {
        let msl = required_msl_root();
        compiler = compiler.source_root(msl.to_str().expect("MSL path is UTF-8"));
    }
    let path = repository_root().join("examples/models").join(file);
    assert!(path.is_file(), "required tracked source {}", path.display());
    compiler
        .compile_file(path.to_str().expect("tracked model path is UTF-8"))
        .map_err(|error| format!("{name}: {error}"))
}

#[test]
fn every_tracked_shared_model_demo_compiles_with_its_authored_defaults() {
    let expected: BTreeSet<_> = EXAMPLES
        .iter()
        .map(|(file, _, _)| (*file).to_owned())
        .collect();
    let actual: BTreeSet<_> = fs::read_dir(repository_root().join("examples/models"))
        .expect("tracked models directory")
        .map(|entry| entry.expect("tracked model entry").file_name())
        .map(|name| {
            name.into_string()
                .expect("tracked model filenames are UTF-8")
        })
        .filter(|name| name.ends_with(".mo"))
        .collect();
    assert_eq!(
        actual, expected,
        "new files require explicit demo expectations"
    );
    assert_eq!(expected.len(), 13);
    assert_eq!(EXAMPLES.len(), 15);
    let mut failures = Vec::new();
    for &(_, name, _) in EXAMPLES {
        match compile(name) {
            Ok(compiled) if compiled.is_balanced() => {
                eprintln!("[tracked-example] {name}: checked balanced compilation passed");
            }
            Ok(compiled) => failures.push(format!("{name}: balance {}", compiled.balance())),
            Err(error) => failures.push(error),
        }
    }
    assert!(
        failures.is_empty(),
        "tracked examples failed:\n{}",
        failures.join("\n")
    );
}

fn simulate(name: &str, end: f64) -> (SimResult, SimOptions) {
    let compiled = compile(name).unwrap_or_else(|error| panic!("{error}"));
    let options = SimOptions {
        execution_policy: SimExecutionPolicy::Interpreter,
        solver_mode: SimSolverMode::RkLike,
        t_end: end,
        dt: Some(0.01),
        rtol: 1e-9,
        atol: 1e-9,
        ..SimOptions::default()
    };
    let result = simulate_dae(&compiled.dae, &options)
        .unwrap_or_else(|error| panic!("actual {name} simulates: {error:?}"));
    assert!(result.termination.is_none(), "{name} ended early");
    assert!(!result.times.is_empty());
    assert_eq!(result.times[0], 0.0);
    assert_eq!(*result.times.last().unwrap(), end);
    assert!(result.times.iter().all(|time| time.is_finite()));
    assert!(result.times.windows(2).all(|pair| pair[0] <= pair[1]));
    (result, options)
}

fn assert_curve(
    result: &SimResult,
    options: &SimOptions,
    name: &str,
    expected: impl Fn(f64) -> f64,
) {
    let index = result
        .names
        .iter()
        .position(|column| column == name)
        .unwrap_or_else(|| panic!("actual trace must contain {name}: {:?}", result.names));
    let values = &result.data[index];
    assert_eq!(
        values.len(),
        result.times.len(),
        "rectangular trace for {name}"
    );
    for (&time, &actual) in result.times.iter().zip(values) {
        let expected = expected(time);
        let bound = 20.0 * (options.atol + options.rtol * expected.abs());
        assert!(
            actual.is_finite() && expected.is_finite() && (actual - expected).abs() <= bound,
            "{name}({time})={actual}, expected {expected}, bound {bound}"
        );
    }
}

#[test]
fn actual_decay_examples_follow_independent_scalar_and_tensor_exponentials() {
    let (scalar, options) = simulate("SympyDecay", 0.1);
    assert_curve(&scalar, &options, "x", |time| (-0.5 * time).exp());
    let (tensor, options) = simulate("FmiTensorDecay", 0.1);
    assert_curve(&tensor, &options, "x[1]", |time| (-0.5 * time).exp());
    assert_curve(&tensor, &options, "x[2]", |time| (-time).exp());
}

#[test]
fn actual_ball_follows_free_fall_before_its_first_impact() {
    let (result, options) = simulate("Ball", 0.1);
    assert_curve(&result, &options, "x", |time| {
        10.0 + time - 0.5 * 9.81 * time * time
    });
    assert_curve(&result, &options, "v", |time| 1.0 - 9.81 * time);
}

#[test]
fn actual_circuit_follows_its_independent_rc_charging_curve() {
    let (result, options) = simulate("Circuit.Test", 0.1);
    assert_curve(&result, &options, "cap.v", |time| {
        5.0 * (1.0 - (-40.0 * time).exp())
    });
}

#[test]
fn actual_local_switched_rlc_follows_the_independent_voltage_step_response() {
    let (result, options) = simulate("SwitchedRLC", 0.6);
    // Authored L=1, C=0.001 and R=100 give V''+10V'+1000V=1000Vs.
    assert_curve(&result, &options, "V", |time| {
        if time <= 0.5 {
            0.0
        } else {
            let elapsed = time - 0.5;
            let frequency = 975.0_f64.sqrt();
            24.0 * (1.0
                - (-5.0 * elapsed).exp()
                    * ((frequency * elapsed).cos() + 5.0 / frequency * (frequency * elapsed).sin()))
        }
    });
}

#[test]
fn actual_kalman_example_executes_its_authored_numeric_initialization_assertions() {
    // The unchanged source asserts estimate/covariance against its independent
    // 1x1 reference numbers at tolerance 1e-10 before the ODE starts.
    let compiled = compile("KalmanFilterStepTest").unwrap_or_else(|error| panic!("{error}"));
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let options = SimOptions {
            execution_policy: policy,
            solver_mode: SimSolverMode::RkLike,
            t_end: 0.1,
            dt: Some(0.01),
            rtol: 1e-9,
            atol: 1e-9,
            ..SimOptions::default()
        };
        let mut session = SimulationSession::new(&compiled.dae, options.clone())
            .unwrap_or_else(|error| panic!("actual Kalman {policy:?} initializes: {error}"));
        for sample in 0..=10 {
            let time = f64::from(sample) / 100.0;
            if sample != 0 {
                session.advance_to(time).expect("actual Kalman advances");
            }
            let state = session.state().expect("actual Kalman exposes its state");
            assert_eq!(state.time, time);
            let actual = state.values["x"];
            let bound = 20.0 * (options.atol + options.rtol * time.abs());
            assert!(
                actual.is_finite() && (actual - time).abs() <= bound,
                "{policy:?} x({time})={actual}"
            );
            eprintln!(
                "[tracked-example] Kalman policy={policy:?} sample={sample} receipt={:?}",
                session.execution_receipt()
            );
        }
    }
}
