use crate::{
    DifferentiableModel, GradientDescent, GradientMode, GradientStrategy, OptError,
    RhsMseObjective, TrainableSet, rhs_mse_value_and_gradient,
};
use rumoca::Compiler;
use rumoca_solver::{SimOptions, SimSolverMode};

const LINEAR_FIT: &str = r#"
model LinearFit
  parameter Real a = 0.0;
  parameter Real b = 0.0;
  Real x(start = 2.0, fixed = true);
equation
  der(x) = a * x + b;
end LinearFit;
"#;

const OVERFLOW_GRADIENT: &str = r#"
model OverflowGradient
  parameter Real p = 1.0;
  Real x(start = 0.0, fixed = true);
equation
  der(x) = 1.0e155 * (p - 1.0);
end OverflowGradient;
"#;

fn linear_model() -> DifferentiableModel {
    linear_model_with_options(&SimOptions::default())
}

fn linear_model_with_options(options: &SimOptions) -> DifferentiableModel {
    let result = Compiler::new()
        .model("LinearFit")
        .compile_str(LINEAR_FIT, "LinearFit.mo")
        .expect("LinearFit should compile");
    DifferentiableModel::from_dae_default(&result.dae, options)
        .expect("LinearFit should prepare for optimization")
}

#[test]
fn reverse_rhs_mse_gradient_matches_analytic_linear_model() {
    let model = linear_model();
    let trainables = TrainableSet::by_names(&model, &["a", "b"]).expect("known trainables");
    let objective = RhsMseObjective::new(0.0, vec![5.0]);

    let report = rhs_mse_value_and_gradient(&model, &objective, &trainables, GradientMode::Reverse)
        .expect("reverse gradient should evaluate");

    assert_eq!(report.strategy, GradientStrategy::ReverseVjp);
    assert!((report.loss - 12.5).abs() < 1.0e-12);
    assert_eq!(report.trainable_names, ["a", "b"]);
    assert!((report.gradients[0] + 10.0).abs() < 1.0e-12);
    assert!((report.gradients[1] + 5.0).abs() < 1.0e-12);
}

#[test]
fn forward_and_reverse_rhs_mse_gradients_agree() {
    let model = linear_model();
    let trainables = TrainableSet::by_names(&model, &["a", "b"]).expect("known trainables");
    let objective = RhsMseObjective::new(0.0, vec![5.0]);

    let reverse =
        rhs_mse_value_and_gradient(&model, &objective, &trainables, GradientMode::Reverse)
            .expect("reverse gradient should evaluate");
    let forward =
        rhs_mse_value_and_gradient(&model, &objective, &trainables, GradientMode::Forward)
            .expect("forward gradient should evaluate");

    assert_eq!(forward.strategy, GradientStrategy::ForwardJacobian);
    assert_eq!(reverse.gradients.len(), forward.gradients.len());
    for (reverse, forward) in reverse.gradients.iter().zip(&forward.gradients) {
        assert!((reverse - forward).abs() < 1.0e-12);
    }
}

#[test]
fn forward_gradient_keeps_artifacts_when_simulation_mode_is_rk_like() {
    let options = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        ..SimOptions::default()
    };
    let model = linear_model_with_options(&options);
    let trainables = TrainableSet::by_names(&model, &["a", "b"]).expect("known trainables");
    let objective = RhsMseObjective::new(0.0, vec![5.0]);

    let report = rhs_mse_value_and_gradient(&model, &objective, &trainables, GradientMode::Forward)
        .expect("forward gradient should evaluate");

    assert_eq!(report.strategy, GradientStrategy::ForwardJacobian);
    assert!((report.gradients[0] + 10.0).abs() < 1.0e-12);
    assert!((report.gradients[1] + 5.0).abs() < 1.0e-12);
}

#[test]
fn reverse_rhs_mse_rejects_non_finite_gradient() {
    let result = Compiler::new()
        .model("OverflowGradient")
        .compile_str(OVERFLOW_GRADIENT, "OverflowGradient.mo")
        .expect("OverflowGradient should compile");
    let model = DifferentiableModel::from_dae_default(&result.dae, &SimOptions::default())
        .expect("OverflowGradient should prepare for optimization");
    let trainables = TrainableSet::by_names(&model, &["p"]).expect("known trainable");
    let objective = RhsMseObjective::new(0.0, vec![-1.0e154]);

    let error = rhs_mse_value_and_gradient(&model, &objective, &trainables, GradientMode::Reverse)
        .expect_err("reverse gradient should reject non-finite values");

    assert!(matches!(
        error,
        OptError::NonFinite {
            what: "gradient",
            ..
        }
    ));
}

#[test]
fn gradient_descent_reduces_rhs_mse_loss() {
    let mut model = linear_model();
    let trainables = TrainableSet::by_names(&model, &["a", "b"]).expect("known trainables");
    let objective = RhsMseObjective::new(0.0, vec![5.0]);

    let report = GradientDescent::new(0.05, 20)
        .with_gradient_mode(GradientMode::Reverse)
        .optimize_rhs_mse(&mut model, &objective, &trainables)
        .expect("gradient descent should run");

    let initial = report.initial_loss().expect("initial loss");
    let final_loss = report.final_loss().expect("final loss");
    assert!(
        final_loss < initial * 0.05,
        "initial={initial}, final={final_loss}"
    );
    assert!(
        (model.parameter_value("a").expect("a") * 2.0 + model.parameter_value("b").expect("b")
            - 5.0)
            .abs()
            < 0.8
    );
}
