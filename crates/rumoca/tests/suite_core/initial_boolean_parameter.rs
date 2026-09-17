//! MLS §8.6: an initialization equation determines a fixed=false parameter.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"model BooleanInitialProbe
  parameter Boolean branch(start=true, fixed=false);
  Real q(start=5, fixed=false);
  Real y(start=0, fixed=true);
initial equation
  q = -2;
  branch = q > 0;
equation
  der(q) = 0;
  der(y) = if branch then 1 else -1;
end BooleanInitialProbe;
"#;

// A fixed=false Boolean parameter whose initial equation reads an algebraic
// determined by a nonlinear continuous equation. Unlike `q` above (a state the
// projection owns), `a` here is reconstructed by the continuous algebraic solve
// and never appears as an explicit assignment, so its initialization-consistent
// value is available only after the algebraics are refreshed. MLS §8.6 solves
// the continuous equations and the parameter together, so the branch must be
// selected at the solved geometry (a = 2, so a > 1), not the start seed (a = 0).
const NONLINEAR_ALGEBRAIC_SOURCE: &str = r#"model NonlinearBranchProbe
  parameter Boolean pos(fixed=false);
  Real a;
  Real x(start=2.0, fixed=true);
  Real d;
initial equation
  pos = a > 1.0;
equation
  sin(a) + a = sin(x) + x;
  der(x) = 0;
  d = if pos then 100.0 else -100.0;
end NonlinearBranchProbe;
"#;

const DEPENDENT_SOURCE: &str = r#"model InitialParameterChain
  function positive
    input Real x;
    output Boolean yes;
  algorithm
    yes := x > 0;
  end positive;
  parameter Boolean branch(start=true, fixed=false);
  parameter Boolean inverted(start=false, fixed=false);
  parameter Integer direction(start=7, fixed=false);
  parameter Real gain = direction + (if branch then 1 else -1);
  Real q(start=5, fixed=false);
  Real y(start=0, fixed=false);
initial equation
  y = gain;
  direction = if inverted then -2 else 2;
  not branch = inverted;
  branch = positive(q);
  q = -2;
equation
  der(q) = 30;
  der(y) = gain;
end InitialParameterChain;
"#;

#[test]
fn initial_parameter_dependencies_settle_and_remain_constant_after_initialization() {
    let compiled = Compiler::new()
        .model("InitialParameterChain")
        .compile_str(DEPENDENT_SOURCE, "initial_parameter_chain.mo")
        .expect("initial definitions preserve types, dependencies, and equation orientation");
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                solver_mode,
                ..Default::default()
            },
        )
        .expect("dependent parameters must use the solved initialization point");
        assert_affine_trace(
            &result,
            &[("q", 30.0, -2.0), ("y", -3.0, -3.0)],
            solver_mode,
        );
    }
}

#[test]
fn cyclic_initial_parameter_definitions_require_a_supported_coupled_solve() {
    let source = r#"model InitialParameterCycle
      parameter Boolean first(fixed=false);
      parameter Boolean second(fixed=false);
      Real y(start=0, fixed=true);
    initial equation
      first = not second;
      second = not first;
    equation
      der(y) = if first then 1 else -1;
    end InitialParameterCycle;"#;
    let compiled = Compiler::new()
        .model("InitialParameterCycle")
        .compile_str(source, "initial_parameter_cycle.mo")
        .unwrap();
    let error = simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default())
        .expect_err("a cyclic definition cannot use declaration order as a solution");
    assert!(
        error
            .to_string()
            .contains("cyclic initialization parameter definitions"),
        "{error}"
    );
}

#[test]
fn boolean_initial_parameter_uses_solved_state_instead_of_start_guesses() {
    for positive in [false, true] {
        let source = if positive {
            SOURCE
                .replace("start=true", "start=false")
                .replace("start=5", "start=-5")
                .replace("q = -2", "q = 2")
        } else {
            SOURCE.to_owned()
        };
        let compiled = Compiler::new()
            .model("BooleanInitialProbe")
            .compile_str(&source, "initial_boolean_parameter.mo")
            .expect("a Boolean initialization equation has a typed initialization owner");
        for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
            let result = simulate_dae_with_diagnostics(
                &compiled.dae,
                &SimOptions {
                    t_end: 0.2,
                    dt: Some(0.05),
                    solver_mode,
                    ..Default::default()
                },
            )
            .expect("initialization must settle the state before selecting its Boolean branch");
            let sign = if positive { 1.0 } else { -1.0 };
            assert_affine_trace(
                &result,
                &[("q", 0.0, sign * 2.0), ("y", sign, 0.0)],
                solver_mode,
            );
        }
    }
}

#[test]
fn boolean_initial_parameter_uses_nonlinear_algebraic_branch() {
    let compiled = Compiler::new()
        .model("NonlinearBranchProbe")
        .compile_str(NONLINEAR_ALGEBRAIC_SOURCE, "nonlinear_branch_probe.mo")
        .expect("a Boolean initialization equation reading an algebraic has a typed owner");
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                solver_mode,
                ..Default::default()
            },
        )
        .expect("initialization must reconstruct the algebraic before selecting the branch");
        // a solves to 2 (the unique root of sin(a) + a = sin(2) + 2), so pos is
        // true and d holds 100. Reading a at its start seed of 0 would select
        // the false branch and hold -100.
        assert_affine_trace(&result, &[("x", 0.0, 2.0), ("d", 0.0, 100.0)], solver_mode);
    }
}

fn assert_affine_trace(result: &SimResult, expected: &[(&str, f64, f64)], solver: SimSolverMode) {
    assert!(!result.times.is_empty());
    for &(name, slope, offset) in expected {
        let channel = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        for (&time, &actual) in result.times.iter().zip(&result.data[channel]) {
            assert!(
                (actual - (offset + slope * time)).abs() < 1e-7,
                "{solver:?}: {name}({time})={actual}, slope={slope}, offset={offset}"
            );
        }
    }
}
