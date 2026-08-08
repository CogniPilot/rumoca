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

    // MLS Appendix B / SPEC_0022: variables appearing differentiated remain
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

/// A translational spring between a support and a mass, with the initial
/// condition stated where the library states relative positions:
/// `spring1.s_rel(start = 1, fixed = true)`.
///
/// MSL writes `s_rel = flange_b.s - flange_a.s` and `flange_a.s = s - L/2`, so
/// the pinned quantity reaches the integrated position `m1.s` through a
/// connector chain, a support held at a parameter, and the body's own
/// half-length displacement. MLS 3.6 §8.6 makes the pin an initialization
/// equation, so `m1.s` starts where the pin puts it and not at its own guess.
const PINNED_SPRING_MASS: &str = r#"
model PinnedSpringMass
  Modelica.Mechanics.Translational.Components.Fixed fixed1(s0 = 0);
  Modelica.Mechanics.Translational.Components.Spring spring1(
    c = 100, s_rel(start = 1, fixed = true));
  Modelica.Mechanics.Translational.Components.Mass m1(
    m = 1, L = 0.5, s(start = 1.5), v(start = 0, fixed = true));
equation
  connect(fixed1.flange, spring1.flange_a);
  connect(spring1.flange_b, m1.flange_a);
end PinnedSpringMass;
"#;

/// OMC 4.1.0 on the same source: `spring1.s_rel(0) = 1`, `m1.s(0) = 1.25`.
#[test]
fn pinned_relative_position_initializes_the_mass_it_holds() {
    let msl_compiler = require_msl_compiler();
    let compiled = msl_compiler
        .model("PinnedSpringMass")
        .compile_str(PINNED_SPRING_MASS, "PinnedSpringMass.mo")
        .expect("pinned spring/mass model should compile");

    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.05),
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .expect("pinned spring/mass model should simulate");

    let s_rel = result_series(&result, &["spring1.s_rel"])[0];
    let position = result_series(&result, &["m1.s"])[0];
    assert!(
        (s_rel - 1.0).abs() <= 1.0e-9,
        "spring1.s_rel started at {s_rel}, expected the stated 1.0"
    );
    assert!(
        (position - 1.25).abs() <= 1.0e-9,
        "m1.s started at {position}, expected 1.25 = s_rel + L/2 + s0"
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

/// `Modelica.Magnetic.FluxTubes.Examples.BasicExamples.SaturatedInductor` at its own
/// `experiment` settings (`StopTime = 0.1`, `Tolerance = 1e-7`).
///
/// The example drives a closed ferromagnetic core past the permeability peak of
/// M350-50A twice per mains period, so every output sample past `t ~ 0.022 s` asks
/// the algebraic projection to move `r_mFe.B` across the saturation knee, where
/// `mu_r` falls by an order of magnitude over a tenth of a tesla. Reference values
/// are OMC 4.1.0 on the same source.
///
/// The output interval is pinned at `2e-4 s` deliberately. On a coarser grid whether
/// the projection is handed a distant warm start depends on the step sequence, and
/// that shifted when `time_event_instant` stopped scheduling a stop at the start
/// instant (`expression_events.rs`, `instant <= 0.0`, correct per MLS §8.5) and so
/// stopped restarting the integrator there. At this interval the stall reproduces
/// regardless, so the guard tests the projection rather than the schedule that
/// happens to reach it.
const SATURATED_INDUCTOR: &str = r#"
model SaturatedInductorRun
  extends Modelica.Magnetic.FluxTubes.Examples.BasicExamples.SaturatedInductor;
end SaturatedInductorRun;
"#;

#[test]
fn saturated_inductor_projects_across_the_saturation_knee() {
    let msl_compiler = require_msl_compiler();
    let compiled = msl_compiler
        .model("SaturatedInductorRun")
        .compile_str(SATURATED_INDUCTOR, "SaturatedInductorRun.mo")
        .expect("MSL saturated inductor example should compile");

    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(2.0e-4),
            atol: 1.0e-7,
            rtol: 1.0e-7,
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .expect("saturated inductor should simulate to its experiment stop time");

    // A relative permeability below 1 is outside the range the MSL approximation
    // `1 + non-negative/positive` can take, and a non-positive reluctance is not a
    // reluctance: either means the projection settled off the physical branch.
    let permeability = result_series(&result, &["r_mFe.mu_r"]);
    let worst_permeability = permeability.iter().copied().fold(f64::INFINITY, f64::min);
    assert!(
        worst_permeability >= 1.0,
        "r_mFe.mu_r reached {worst_permeability}, which is not a physical relative permeability"
    );
    let worst_reluctance = result_series(&result, &["r_mFe.R_m"])
        .iter()
        .copied()
        .fold(f64::INFINITY, f64::min);
    assert!(
        worst_reluctance > 0.0,
        "r_mFe.R_m reached {worst_reluctance}, which is not a physical reluctance"
    );

    // OMC 4.1.0: min(r_mFe.mu_r) = 413.11 at the flux-density peak
    // max|r_mFe.B| = 1.5868 T, max|coil.i| = 1.5350 A.
    assert!(
        (worst_permeability - 413.11).abs() <= 413.11 * 0.02,
        "saturation depth drifted: min r_mFe.mu_r = {worst_permeability}, OMC reports 413.11"
    );
    let peak_flux_density = max_abs_column_value(&result, &["r_mFe.B"]);
    assert!(
        (peak_flux_density - 1.5868).abs() <= 1.5868 * 0.02,
        "peak r_mFe.B = {peak_flux_density}, OMC reports 1.5868 T"
    );
    let peak_current = max_abs_column_value(&result, &["coil.i"]);
    assert!(
        (peak_current - 1.5350).abs() <= 1.5350 * 0.02,
        "peak coil.i = {peak_current}, OMC reports 1.5350 A"
    );
}
