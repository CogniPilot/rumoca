use std::env;
use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::simulate_dae_with_diagnostics;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode};

fn example_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples")
        .join(name)
}

fn cached_msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

fn compiler_with_msl() -> Option<Compiler> {
    if let Some(raw) = env::var_os("MODELICAPATH") {
        return Some(
            env::split_paths(&raw).fold(Compiler::new(), |compiler, path| {
                compiler.source_root(path.to_string_lossy().as_ref())
            }),
        );
    }
    if let Some(msl_root) = cached_msl_root() {
        return Some(Compiler::new().source_root(msl_root.to_string_lossy().as_ref()));
    }
    None
}

fn require_msl_compiler() -> Compiler {
    compiler_with_msl().expect(
        "MSL simulation regression tests require MODELICAPATH or cached MSL at \
         target/msl/ModelicaStandardLibrary-4.1.0; run without the msl-sim-tests \
         feature when MSL is not available",
    )
}

fn result_series<'a>(result: &'a SimResult, names: &[&str]) -> &'a [f64] {
    let idx = result
        .names
        .iter()
        .position(|candidate| names.iter().any(|name| candidate == name))
        .unwrap_or_else(|| panic!("simulation result missing columns {:?}", names));
    result
        .data
        .get(idx)
        .map(Vec::as_slice)
        .unwrap_or_else(|| panic!("simulation result missing data series for {:?}", names))
}

fn final_series_value(result: &SimResult, names: &[&str]) -> f64 {
    *result_series(result, names)
        .last()
        .unwrap_or_else(|| panic!("simulation result missing final sample for {:?}", names))
}

fn max_abs_column_value(result: &SimResult, names: &[&str]) -> f64 {
    result_series(result, names)
        .iter()
        .copied()
        .map(f64::abs)
        .fold(0.0, f64::max)
}

fn max_abs_series_delta(left: &[f64], right: &[f64]) -> f64 {
    assert_eq!(
        left.len(),
        right.len(),
        "series length mismatch: left={} right={}",
        left.len(),
        right.len()
    );
    left.iter()
        .zip(right)
        .map(|(a, b)| (a - b).abs())
        .fold(0.0, f64::max)
}

fn series_value_at(result: &SimResult, name: &str, time: f64) -> f64 {
    let sample_idx = result
        .times
        .iter()
        .position(|candidate| (*candidate - time).abs() <= 1.0e-12)
        .unwrap_or_else(|| panic!("simulation result missing t={time}: {:?}", result.times));
    result_series(result, &[name])[sample_idx]
}

fn variable_is_state(result: &SimResult, name: &str) -> bool {
    result
        .variable_meta
        .iter()
        .find(|meta| meta.name == name)
        .is_some_and(|meta| meta.is_state)
}

#[test]
fn switched_rlc_msl_retains_storage_states_through_step() {
    let msl_compiler = require_msl_compiler();
    let simple = Compiler::new()
        .model("SwitchedRLC")
        .compile_file(
            example_path("models/SwitchedRLC.mo")
                .to_string_lossy()
                .as_ref(),
        )
        .expect("handwritten switched RLC example should compile");
    let msl = msl_compiler
        .model("SwitchedRLC_MSL")
        .compile_file(
            example_path("models/SwitchedRLC_MSL.mo")
                .to_string_lossy()
                .as_ref(),
        )
        .expect("MSL switched RLC example should compile");

    let opts = SimOptions {
        t_end: 0.75,
        solver_mode: SimSolverMode::RkLike,
        ..SimOptions::default()
    };

    let simple_result = simulate_dae_with_diagnostics(&simple.dae, &opts)
        .expect("handwritten switched RLC example should simulate");
    let msl_result = simulate_dae_with_diagnostics(&msl.dae, &opts)
        .expect("MSL switched RLC example should simulate");

    // MLS Appendix B / SPEC_0003: variables appearing differentiated remain
    // states. The MSL capacitor voltage and inductor current are both physical
    // storage states and must survive simulator preparation.
    assert_eq!(simple_result.n_states, 2);
    assert_eq!(
        msl_result.n_states, 2,
        "expected SwitchedRLC_MSL to retain both storage states"
    );
    assert!(
        variable_is_state(&msl_result, "capacitor.v"),
        "expected capacitor.v to remain a reported state"
    );
    assert!(
        variable_is_state(&msl_result, "inductor.i"),
        "expected inductor.i to remain a reported state"
    );

    let simple_v_series = result_series(&simple_result, &["V"]);
    let msl_v_series = result_series(&msl_result, &["capacitor.v", "capacitor.p.v"]);
    let simple_i_series = result_series(&simple_result, &["i_L"]);
    let msl_i_series = result_series(&msl_result, &["inductor.i"]);
    let max_v_delta = max_abs_series_delta(simple_v_series, msl_v_series);
    let max_i_delta = max_abs_series_delta(simple_i_series, msl_i_series);

    assert!(
        max_v_delta <= 1.0e-9,
        "expected capacitor voltage trace through the switch event to match handwritten example: max delta={max_v_delta}"
    );
    assert!(
        max_i_delta <= 1.0e-9,
        "expected inductor current trace through the switch event to match handwritten example: max delta={max_i_delta}"
    );
}

#[test]
fn pid_msl_responds_to_step_error() {
    let msl_compiler = require_msl_compiler();
    let pid = msl_compiler
        .model("PIDMSL")
        .compile_file(example_path("models/PIDMSL.mo").to_string_lossy().as_ref())
        .expect("PIDMSL example should compile");

    let opts = SimOptions {
        t_end: 1.0,
        dt: Some(0.02),
        ..SimOptions::default()
    };

    let result =
        simulate_dae_with_diagnostics(&pid.dae, &opts).expect("PIDMSL example should simulate");

    // MLS Appendix B B.1a: continuous equations are simultaneous and unordered.
    // The forcing equation `pid.u = 1 - x` must not be overwritten by later
    // connection aliases that share the same residual target.
    let x_final = final_series_value(&result, &["x"]);
    let pid_y_max = max_abs_column_value(&result, &["pid.y"]);
    assert!(
        x_final.abs() > 0.1,
        "expected PIDMSL state to respond to the step input, got final x={x_final}"
    );
    assert!(
        pid_y_max > 1.0,
        "expected PIDMSL controller output to become nonzero, max |pid.y|={pid_y_max}"
    );
}

#[test]
fn exactly_clocked_drive_refreshes_inferred_sample_and_controller_on_same_tick() {
    let msl_compiler = require_msl_compiler();
    let model_path = cached_msl_root()
        .expect("cached MSL root is required for this concrete example")
        .join("Modelica 4.1.0/Clocked/Examples/SimpleControlledDrive/ExactlyClockedWithDiscreteController.mo");
    let compiled = msl_compiler
        .model(
            "Modelica.Clocked.Examples.SimpleControlledDrive.ExactlyClockedWithDiscreteController",
        )
        .compile_file(model_path.to_string_lossy().as_ref())
        .expect("ExactlyClockedWithDiscreteController should compile");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            // Run the model's full experiment horizon. A short run through the
            // second clock tick misses later continuous/event instability.
            t_end: 5.0,
            max_wall_seconds: Some(10.0),
            ..SimOptions::default()
        },
    )
    .expect("ExactlyClockedWithDiscreteController should simulate");

    let sampled_speed = series_value_at(&result, "sample1.y", 0.2);
    let speed = series_value_at(&result, "speed.w", 0.2);
    assert!(
        (sampled_speed - speed).abs() <= 1.0e-9 && sampled_speed > 0.1,
        "the inferred-clock sample must read the projected speed source on the second tick: \
         sample1.y={sampled_speed}, speed.w={speed}"
    );
    let pi_y = series_value_at(&result, "PI.y", 0.2);
    let held = series_value_at(&result, "hold1.y", 0.2);
    assert!(
        (pi_y - 3.3).abs() <= 1.0e-8 && (held - pi_y).abs() <= 1.0e-9,
        "the PI and hold chain must consume the refreshed sample on the same tick: \
         PI.y={pi_y}, hold1.y={held}"
    );
}
