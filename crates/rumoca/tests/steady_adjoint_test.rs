//! Roadmap Track B (steady-state adjoint): the reverse-mode adjoint objective
//! gradient `dJ/dp = -(∂f/∂p)ᵀ λ` with `(∂f/∂y)ᵀ λ = (∂J/∂y)ᵀ` must equal the
//! forward implicit-function gradient (M2) and the closed-form analytic values.
//! Pure-ODE model so `solver_y == states` and the bare reverse VJP is exact.

use rumoca::Compiler;
use rumoca_eval_solve::ObjectiveGradientReport;
use rumoca_sim::SimOptions;

// Steady state (a=2, b=3, c=1.5, d=0.5):
//   w* = c/d = 3,  x* = b*c/(a*d) = 4.5
const SOURCE: &str = r#"
model AdjModel
  parameter Real a = 2.0;
  parameter Real b = 3.0;
  parameter Real c = 1.5;
  parameter Real d = 0.5;
  Real x(start = 3.0);
  Real w(start = 3.0);
equation
  der(w) = c - d * w;
  der(x) = -a * x + b * w;
end AdjModel;
"#;

fn param(report: &ObjectiveGradientReport, name: &str) -> f64 {
    let idx = report
        .param_labels
        .iter()
        .position(|p| p == name)
        .unwrap_or_else(|| panic!("missing parameter `{name}` in {:?}", report.param_labels));
    report.gradient[idx]
}

#[test]
fn steady_state_adjoint_matches_forward_and_analytic() {
    let result = Compiler::new()
        .model("AdjModel")
        .compile_str(SOURCE, "AdjModel.mo")
        .expect("AdjModel should compile");
    let overrides = [("x".to_string(), 4.5), ("w".to_string(), 3.0)];

    let adjoint = |objective: &str| {
        let probe = rumoca_sim::steady_state_adjoint_objective_gradient_for_dae(
            &result.dae,
            &SimOptions::default(),
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("adjoint `{objective}` should evaluate: {e:?}"));
        assert!(
            probe.report.error.is_none(),
            "adjoint error: {:?}",
            probe.report.error
        );
        probe.report
    };
    let forward = |objective: &str| {
        let probe = rumoca_sim::steady_state_objective_gradient_for_dae(
            &result.dae,
            &SimOptions::default(),
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("forward `{objective}` should evaluate: {e:?}"));
        assert!(
            probe.report.error.is_none(),
            "forward error: {:?}",
            probe.report.error
        );
        probe.report
    };

    let close = |got: f64, want: f64, what: &str| {
        assert!(
            (got - want).abs() < 1.0e-6,
            "{what}: got {got}, want {want}"
        );
    };

    // d(x*)/dp = [-2.25, 1.5, 3, -9]; d(w*)/dp = [0, 0, 2, -6].
    for (objective, analytic) in [
        ("x", [("a", -2.25), ("b", 1.5), ("c", 3.0), ("d", -9.0)]),
        ("w", [("a", 0.0), ("b", 0.0), ("c", 2.0), ("d", -6.0)]),
    ] {
        let adj = adjoint(objective);
        let fwd = forward(objective);
        for (name, want) in analytic {
            close(
                param(&adj, name),
                want,
                &format!("adjoint d({objective})/d{name}"),
            );
            // The adjoint must agree with the forward implicit-function gradient.
            close(
                param(&adj, name),
                param(&fwd, name),
                &format!("adjoint vs forward d({objective})/d{name}"),
            );
        }
    }
}

// A 5-state linear chain — large enough to exercise the matrix-free GMRES adjoint
// solve over several Krylov iterations (the 2-state model converges in ≤2).
const CHAIN_SOURCE: &str = r#"
model Chain5
  parameter Real a = 2.0;
  parameter Real b = 1.0;
  parameter Real c = 4.0;
  Real x1(start = 2.0);
  Real x2(start = 1.0);
  Real x3(start = 0.5);
  Real x4(start = 0.25);
  Real x5(start = 0.125);
equation
  der(x1) = c - a * x1;
  der(x2) = b * x1 - a * x2;
  der(x3) = b * x2 - a * x3;
  der(x4) = b * x3 - a * x4;
  der(x5) = b * x4 - a * x5;
end Chain5;
"#;

/// The matrix-free GMRES adjoint must match the forward implicit-function
/// gradient on a larger (5-state) system, where GMRES runs several iterations.
/// Forward and adjoint are transposes of the same linearization, so they must
/// agree at the evaluation point regardless of its proximity to steady state.
#[test]
fn steady_state_adjoint_matches_forward_on_chain() {
    let result = Compiler::new()
        .model("Chain5")
        .compile_str(CHAIN_SOURCE, "Chain5.mo")
        .expect("Chain5 should compile");
    // Steady state (a=2, b=1, c=4): x_i = c / a^i.
    let overrides = [
        ("x1".to_string(), 2.0),
        ("x2".to_string(), 1.0),
        ("x3".to_string(), 0.5),
        ("x4".to_string(), 0.25),
        ("x5".to_string(), 0.125),
    ];
    let opts = SimOptions::default();

    for objective in ["x1", "x2", "x3", "x4", "x5"] {
        let adjoint = rumoca_sim::steady_state_adjoint_objective_gradient_for_dae(
            &result.dae,
            &opts,
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("adjoint `{objective}`: {e:?}"));
        let forward = rumoca_sim::steady_state_objective_gradient_for_dae(
            &result.dae,
            &opts,
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("forward `{objective}`: {e:?}"));
        assert!(
            adjoint.report.error.is_none(),
            "adjoint error: {:?}",
            adjoint.report.error
        );
        assert!(
            forward.report.error.is_none(),
            "forward error: {:?}",
            forward.report.error
        );
        for name in ["a", "b", "c"] {
            let a = param(&adjoint.report, name);
            let f = param(&forward.report, name);
            assert!(
                (a - f).abs() < 1.0e-7,
                "adjoint vs forward d({objective})/d{name}: {a} vs {f}"
            );
        }
    }
}

// Model with a genuine nonlinear solver algebraic `z` (z* = sqrt(b*x*)). The
// full-DAE-residual adjoint must handle it — both a state objective and the
// algebraic objective `z` itself.
const ALG_SOURCE: &str = r#"
model AdjAlgModel
  parameter Real a = 2.0;
  parameter Real b = 3.0;
  parameter Real c = 1.5;
  parameter Real d = 0.5;
  Real x(start = 3.0);
  Real w(start = 3.0);
  Real z(start = 3.0);
equation
  der(w) = c - d * w;
  der(x) = -a * x + b * w;
  z * z = b * x;
end AdjAlgModel;
"#;

/// The full-DAE-residual adjoint handles solver algebraics and an algebraic
/// objective: `d(z)/dp` (and `d(x)/dp`) must match the forward gradient (M2) and
/// the closed-form analytic values. Steady state x*=4.5, z*=√(b·x*)=√13.5;
/// implicit differentiation of `z² = b*x` gives `dz/dp = (x*·δ_{p,b} + b·∂x/∂p)/(2z*)`.
#[test]
fn steady_state_adjoint_handles_algebraic_objective() {
    let result = Compiler::new()
        .model("AdjAlgModel")
        .compile_str(ALG_SOURCE, "AdjAlgModel.mo")
        .expect("AdjAlgModel should compile");
    let overrides = [("x".to_string(), 4.5), ("w".to_string(), 3.0)];
    let opts = SimOptions::default();
    let close = |got: f64, want: f64, what: &str| {
        assert!(
            (got - want).abs() < 1.0e-6,
            "{what}: got {got}, want {want}"
        );
    };

    let adjoint = |objective: &str| {
        let probe = rumoca_sim::steady_state_adjoint_objective_gradient_for_dae(
            &result.dae,
            &opts,
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("adjoint `{objective}`: {e:?}"));
        assert!(
            probe.report.error.is_none(),
            "adjoint error: {:?}",
            probe.report.error
        );
        probe.report
    };
    let forward = |objective: &str| {
        let probe = rumoca_sim::steady_state_objective_gradient_for_dae(
            &result.dae,
            &opts,
            &overrides,
            objective,
            0.0,
        )
        .unwrap_or_else(|e| panic!("forward `{objective}`: {e:?}"));
        assert!(
            probe.report.error.is_none(),
            "forward error: {:?}",
            probe.report.error
        );
        probe.report
    };

    // State objective x: d(x*)/dp = [-2.25, 1.5, 3, -9].
    let adj_x = adjoint("x");
    let fwd_x = forward("x");
    for (name, want) in [("a", -2.25), ("b", 1.5), ("c", 3.0), ("d", -9.0)] {
        close(param(&adj_x, name), want, &format!("adjoint d(x)/d{name}"));
        close(
            param(&adj_x, name),
            param(&fwd_x, name),
            &format!("adjoint vs forward d(x)/d{name}"),
        );
    }

    // Algebraic objective z: dz/dp = (x*·δ_{p,b} + b·∂x/∂p)/(2z*).
    let x_star = 4.5_f64;
    let b = 3.0_f64;
    let z_star = (b * x_star).sqrt();
    let dz = |dx_dp: f64, direct_b: f64| (direct_b * x_star + b * dx_dp) / (2.0 * z_star);
    let adj_z = adjoint("z");
    let fwd_z = forward("z");
    for (name, want) in [
        ("a", dz(-2.25, 0.0)),
        ("b", dz(1.5, 1.0)),
        ("c", dz(3.0, 0.0)),
        ("d", dz(-9.0, 0.0)),
    ] {
        close(param(&adj_z, name), want, &format!("adjoint d(z)/d{name}"));
        close(
            param(&adj_z, name),
            param(&fwd_z, name),
            &format!("adjoint vs forward d(z)/d{name}"),
        );
    }
}
