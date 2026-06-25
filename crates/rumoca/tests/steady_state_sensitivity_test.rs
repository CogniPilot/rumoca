//! Roadmap Track 0.2: the steady-state forward parameter sensitivity
//! `∂y/∂p = -(∂f/∂y)⁻¹·∂f/∂p` (implicit-function theorem) must match the
//! closed-form steady-state sensitivities of a small model with a known
//! analytic steady state.

use rumoca::Compiler;
use rumoca_sim::SimOptions;

// Steady state (a=2, b=3, c=1.5, d=0.5):
//   der(w) = c - d*w        => w* = c/d = 3
//   der(x) = -a*x + b*w     => x* = b*c/(a*d) = 4.5
const SOURCE: &str = r#"
model SteadyModel
  parameter Real a = 2.0;
  parameter Real b = 3.0;
  parameter Real c = 1.5;
  parameter Real d = 0.5;
  Real x(start = 3.0);
  Real w(start = 3.0);
  // `z` is a genuine *nonlinear* solver-y algebraic (z* = sqrt(b*x*)); it stays
  // in the solver vector rather than being alias-eliminated, so it exercises the
  // algebraic-objective sensitivity-projection path on a nonlinear residual that
  // must be iterated to convergence (cf. nonlinear pressure/drag relations).
  Real z(start = 3.0);
equation
  der(w) = c - d * w;
  der(x) = -a * x + b * w;
  z * z = b * x;
end SteadyModel;
"#;

#[test]
fn steady_state_sensitivity_matches_analytic() {
    let result = Compiler::new()
        .model("SteadyModel")
        .compile_str(SOURCE, "SteadyModel.mo")
        .expect("SteadyModel should compile");

    // Evaluate at the analytic steady state x*=4.5, w*=3.
    let probe = rumoca_sim::steady_state_parameter_sensitivity_for_dae(
        &result.dae,
        &SimOptions::default(),
        &[("x".to_string(), 4.5), ("w".to_string(), 3.0)],
        0.0,
    )
    .expect("steady-state sensitivity should evaluate");
    let report = &probe.report;
    assert!(
        report.error.is_none(),
        "steady sensitivity error: {:?}",
        report.error
    );

    let val = |state: &str, param: &str| -> f64 {
        let r = report
            .state_labels
            .iter()
            .position(|s| s == state)
            .unwrap_or_else(|| panic!("missing state `{state}`"));
        let c = report
            .param_labels
            .iter()
            .position(|p| p == param)
            .unwrap_or_else(|| panic!("missing parameter `{param}`"));
        report.sensitivity[r][c]
    };
    let close = |got: f64, want: f64, what: &str| {
        assert!(
            (got - want).abs() < 1.0e-6,
            "{what}: got {got}, want {want}"
        );
    };

    // w* = c/d: depends only on c, d.
    close(val("w", "c"), 2.0, "∂w*/∂c = 1/d");
    close(val("w", "d"), -6.0, "∂w*/∂d = -c/d^2");
    close(val("w", "a"), 0.0, "∂w*/∂a");
    close(val("w", "b"), 0.0, "∂w*/∂b");
    // x* = b*c/(a*d).
    close(val("x", "a"), -2.25, "∂x*/∂a = -b*c/(a^2*d)");
    close(val("x", "b"), 1.5, "∂x*/∂b = c/(a*d)");
    close(val("x", "c"), 3.0, "∂x*/∂c = b/(a*d)");
    close(val("x", "d"), -9.0, "∂x*/∂d = -b*c/(a*d^2)");
}

/// Roadmap Track 0.2 (M2): the steady-state objective gradient `d(obj)/dp` must
/// match analytic values for **both** a state objective (where it equals the
/// `∂y/∂p` row) and a genuine nonlinear algebraic objective `z` with `z² = b*x`
/// (where the total derivative folds the algebraic projection's own dependence on
/// the parameters together with the chained state sensitivities).
#[test]
fn steady_state_objective_gradient_matches_analytic() {
    let result = Compiler::new()
        .model("SteadyModel")
        .compile_str(SOURCE, "SteadyModel.mo")
        .expect("SteadyModel should compile");

    let overrides = [("x".to_string(), 4.5), ("w".to_string(), 3.0)];
    let grad = |objective: &str| {
        let probe = rumoca_sim::steady_state_objective_gradient_for_dae(
            &result.dae,
            &SimOptions::default(),
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("objective `{objective}` gradient should evaluate: {e:?}"));
        let report = probe.report;
        assert!(
            report.error.is_none(),
            "objective gradient error: {:?}",
            report.error
        );
        move |param: &str| -> f64 {
            let c = report
                .param_labels
                .iter()
                .position(|p| p == param)
                .unwrap_or_else(|| panic!("missing parameter `{param}`"));
            report.gradient[c]
        }
    };
    let close = |got: f64, want: f64, what: &str| {
        assert!(
            (got - want).abs() < 1.0e-6,
            "{what}: got {got}, want {want}"
        );
    };

    // State objective: d(x*)/dp equals the ∂x/∂p sensitivity row.
    let dx = grad("x");
    close(dx("a"), -2.25, "d(x*)/da");
    close(dx("b"), 1.5, "d(x*)/db");
    close(dx("c"), 3.0, "d(x*)/dc");
    close(dx("d"), -9.0, "d(x*)/dd");

    // Nonlinear algebraic objective z with z² = b*x at steady state (x*=4.5, b=3
    // → z*=√13.5). Implicit differentiation: 2z·dz/dp = x*·∂b/∂p + b·∂x/∂p, so
    //   dz/dp = (x*·δ_{p,b} + b·∂x/∂p) / (2z*).
    let x_star = 4.5_f64;
    let b = 3.0_f64;
    let z_star = (b * x_star).sqrt();
    let dz_expected = |dx_dp: f64, direct_b: f64| (direct_b * x_star + b * dx_dp) / (2.0 * z_star);
    let dz = grad("z");
    close(dz("a"), dz_expected(-2.25, 0.0), "d(z)/da");
    close(dz("b"), dz_expected(1.5, 1.0), "d(z)/db"); // direct ∂(z²)/∂b = x*
    close(dz("c"), dz_expected(3.0, 0.0), "d(z)/dc");
    close(dz("d"), dz_expected(-9.0, 0.0), "d(z)/dd");
}

/// Regression for the nonlinear-algebraic refresh-convergence bug: a standalone
/// nonlinear solver-y algebraic `z*z = b*x` (no dependency cycle) must iterate to
/// its true root `z* = sqrt(b*x)`. The refresh-plan iteration classifier formerly
/// keyed only on dependency cycles, leaving such a row non-iterative — a single
/// secant step then stopped at `51/14 ≈ 3.6429` instead of `√13.5 ≈ 3.6742`.
#[test]
fn nonlinear_algebraic_refresh_converges_to_true_root() {
    let result = Compiler::new()
        .model("SteadyModel")
        .compile_str(SOURCE, "SteadyModel.mo")
        .expect("SteadyModel should compile");
    let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &SimOptions::default())
        .expect("lowering should succeed");
    let runtime = rumoca_eval_solve::SolveRuntime::new(&solve_model).expect("runtime should build");

    // Settle the algebraics from the steady state x*=4.5, w*=3 (z is the third,
    // nonlinear, solver-y slot).
    let names = &solve_model.problem.solve_layout.solver_maps.names;
    let z_index = names
        .iter()
        .position(|n| n == "z")
        .expect("z is a solver variable");
    let solver_y = runtime
        .full_solver_y(0.0, &[4.5, 3.0], &solve_model.parameters, 1.0e-12, 200)
        .expect("algebraic settle should converge");

    let z_true = (3.0_f64 * 4.5).sqrt();
    assert!(
        (solver_y[z_index] - z_true).abs() < 1.0e-9,
        "nonlinear algebraic z should settle to sqrt(13.5)={z_true}, got {}",
        solver_y[z_index]
    );
}

/// A declared `output` whose defining expression is non-trivial (here the
/// nonlinear `obj = b*x²`) is kept as a solver-y slot rather than alias-eliminated,
/// so it can serve directly as a steady objective. This is the shape of the
/// airfoil demo's drag/lift proxy (a reduction over the field), so the steady
/// objective gradient must handle it without any output-reconstruction step.
const OUTPUT_OBJECTIVE_SOURCE: &str = r#"
model OutputObjectiveModel
  parameter Real a = 2.0;
  parameter Real b = 3.0;
  parameter Real c = 1.5;
  parameter Real d = 0.5;
  Real x(start = 3.0);
  Real w(start = 3.0);
  output Real obj;
equation
  der(w) = c - d * w;
  der(x) = -a * x + b * w;
  obj = b * x * x;
end OutputObjectiveModel;
"#;

#[test]
fn steady_state_objective_gradient_supports_output_objective() {
    let result = Compiler::new()
        .model("OutputObjectiveModel")
        .compile_str(OUTPUT_OBJECTIVE_SOURCE, "OutputObjectiveModel.mo")
        .expect("OutputObjectiveModel should compile");

    let probe = rumoca_sim::steady_state_objective_gradient_for_dae(
        &result.dae,
        &SimOptions::default(),
        &[("x".to_string(), 4.5), ("w".to_string(), 3.0)],
        "obj",
        0.0,
    )
    .expect("output objective gradient should evaluate");
    let report = probe.report;
    assert!(
        report.error.is_none(),
        "output objective error: {:?}",
        report.error
    );

    let grad = |param: &str| -> f64 {
        let c = report
            .param_labels
            .iter()
            .position(|p| p == param)
            .unwrap_or_else(|| panic!("missing parameter `{param}`"));
        report.gradient[c]
    };
    let close = |got: f64, want: f64, what: &str| {
        assert!(
            (got - want).abs() < 1.0e-6,
            "{what}: got {got}, want {want}"
        );
    };

    // obj = b*x² at steady state (x*=4.5): d(obj)/dp = x*²·∂b/∂p + 2*b*x*·∂x/∂p.
    // With 2*b*x* = 27 and x*² = 20.25, ∂x/∂{a,b,c,d} = {-2.25, 1.5, 3, -9}:
    close(grad("a"), 27.0 * -2.25, "d(obj)/da");
    close(grad("b"), 20.25 + 27.0 * 1.5, "d(obj)/db"); // direct ∂(b*x²)/∂b = x*²
    close(grad("c"), 27.0 * 3.0, "d(obj)/dc");
    close(grad("d"), 27.0 * -9.0, "d(obj)/dd");
}
