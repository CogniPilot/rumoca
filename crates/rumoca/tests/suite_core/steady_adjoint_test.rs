//! The reverse-mode steady-state adjoint objective
//! gradient `dJ/dp = -(∂f/∂p)ᵀ λ` with `(∂f/∂y)ᵀ λ = (∂J/∂y)ᵀ` must equal the
//! forward implicit-function gradient (M2) and the closed-form analytic values.
//! Pure-ODE model so `solver_y == states` and the bare reverse VJP is exact.

use rumoca::Compiler;
use rumoca_sim::SimOptions;
use rumoca_solver::ObjectiveGradientReport;

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

/// A smooth mesh coordinate changes both discrete diffusion fluxes and the
/// gradient used for wall traction. The manufactured steady velocity x(1-x)
/// is quadratic, so the nonuniform three-point stencil is exact. The measured
/// one-sided wall shear is 1-h; recovering its slope needs the state response.
#[test]
fn adjoint_follows_a_moving_grid_line_through_flow_and_wall_shear() {
    let model = r#"
model MovingGrid
  function flux
    input Real u[2];
    input Real dx;
    output Real q;
  algorithm
    q := (u[2]-u[1])/dx;
    annotation(Inline=false);
  end flux;
  parameter Real h=0.25;
  parameter Real b=0.65;
  Real u(start=0.1875), v(start=0.2275), wallShear;
equation
  der(u) = 2/b*(flux({u,v},b-h)-flux({0,u},h))+2;
  der(v) = 2/(1-h)*(flux({v,0},1-b)-flux({u,v},b-h))+2;
  wallShear = flux({0,u},h);
end MovingGrid;
"#;
    let compiled = Compiler::new()
        .model("MovingGrid")
        .compile_str(model, "MovingGrid.mo")
        .unwrap();
    let point = [("u".to_string(), 0.1875), ("v".to_string(), 0.2275)];
    let probe = rumoca_sim::steady_state_adjoint_objective_gradient_for_dae(
        &compiled.dae,
        &SimOptions::default(),
        &point,
        "wallShear",
        0.0,
    )
    .unwrap();
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    assert!((param(&probe.report, "h") + 1.0).abs() < 1e-9);
    assert!(param(&probe.report, "b").abs() < 1e-9);
}

#[test]
fn nested_loop_branch_objective_has_the_same_forward_and_reverse_sensitivity() {
    let source = r#"
model NestedBranchAdjoint
  function loadHistory
    input Real gain;
    input Real bias;
    output Real force;
  algorithm
    force := bias;
    for i in 1:2 loop
      for j in 1:2 loop
        if force > 0 then
          force := gain*force + bias;
        else
          force := force - gain;
        end if;
      end for;
    end for;
  end loadHistory;
  parameter Real gain=2;
  parameter Real bias=1;
  Real x(start=31);
equation
  der(x)=loadHistory(gain,bias)-x;
end NestedBranchAdjoint;
"#;
    let result = Compiler::new()
        .model("NestedBranchAdjoint")
        .compile_str(source, "NestedBranchAdjoint.mo")
        .unwrap();
    let state = [("x".to_string(), 31.)];
    let options = SimOptions::default();
    let reverse = rumoca_sim::steady_state_adjoint_objective_gradient_for_dae(
        &result.dae,
        &options,
        &state,
        "x",
        0.,
    )
    .unwrap();
    let forward =
        rumoca_sim::steady_state_objective_gradient_for_dae(&result.dae, &options, &state, "x", 0.)
            .unwrap();
    // At this point x* = bias*(1+g+g²+g³+g⁴).
    for report in [&reverse.report, &forward.report] {
        assert!(report.error.is_none(), "{:?}", report.error);
        assert!((param(report, "gain") - 49.).abs() < 1e-9);
        assert!((param(report, "bias") - 31.).abs() < 1e-9);
    }
}
