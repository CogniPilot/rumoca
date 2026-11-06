use super::*;
use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_ir_solve::SolveModel;

#[test]
fn declared_native_target_selects_the_checked_scalar_profile() {
    let profile = selected_profile().unwrap();
    assert_eq!(profile.name(), "in-process-cranelift");
    assert!(!profile.select().admits_nested_map_frames());
    let model = model(NONLINEAR, "OwnedJacobian");
    let portable = ModelStateJacobian::construct(&model).unwrap();
    let selected = ModelStateJacobian::construct_for_target(&model, &profile).unwrap();
    assert_eq!(
        serde_json::to_value(portable.table()).unwrap(),
        serde_json::to_value(selected.table()).unwrap()
    );
    assert_eq!(
        portable.specialization().program(),
        selected.specialization().program()
    );
    assert_ne!(
        portable.specialization().profile(),
        selected.specialization().profile()
    );
}

#[test]
fn invalid_native_target_declarations_refuse_instead_of_using_host_defaults() {
    let source = include_str!("../../../targets/cranelift/target.toml");
    for changed in [
        source.replacen("version = 1", "version = 2", 1),
        source.replacen("ir = \"solve\"", "ir = \"dae\"", 1),
        source.replacen(
            "name = \"in-process-cranelift\"",
            "name = \"other-host\"",
            1,
        ),
        source.replace("admits_nested_map_frames = false", ""),
        source.replace(
            "max_unrolled_operations = 1024",
            "max_unrolled_operations = 1025",
        ),
        source.replace("[specialization]", "unknown = true\n[specialization]"),
        format!("{source}\nwcet_budget = 1\n"),
    ] {
        assert!(profile_from_target(&changed).is_err(), "{changed}");
    }
}

thread_local! {
    static CALLS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static PREPARATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

pub(super) fn record_preparation() {
    PREPARATIONS.with(|calls| calls.set(calls.get() + 1));
}

pub(super) fn record_call() {
    CALLS.with(|calls| calls.set(calls.get() + 1));
}

#[cfg(any(feature = "solver-diffsol", feature = "solver-velimplicit"))]
fn actual_solver_uses_model_jacobian(mode: rumoca_solver::SimSolverMode) {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("implicit_jacobian.mo", NONLINEAR)
        .unwrap();
    let compiled = session.compile_model("OwnedJacobian").unwrap();
    let before = CALLS.with(std::cell::Cell::get);
    crate::simulate_with_diagnostics(
        &compiled.dae,
        &crate::SimOptions {
            solver_mode: mode,
            t_end: 0.02,
            dt: Some(0.01),
            max_wall_seconds: Some(30.0),
            ..crate::SimOptions::default()
        },
    )
    .unwrap();
    assert!(
        CALLS.with(std::cell::Cell::get) > before,
        "the actual implicit solver must call the prepared model product"
    );
}

#[cfg(feature = "solver-diffsol")]
#[test]
fn actual_bdf_callbacks_reach_the_retained_model_jacobian() {
    actual_solver_uses_model_jacobian(rumoca_solver::SimSolverMode::Bdf);
}

#[cfg(feature = "solver-velimplicit")]
#[test]
fn actual_velimplicit_callbacks_reach_the_retained_model_jacobian() {
    actual_solver_uses_model_jacobian(rumoca_solver::SimSolverMode::VelImplicit);
}

#[cfg(feature = "solver-rk45")]
#[test]
fn explicit_rk_does_not_prepare_an_unused_full_jacobian() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("explicit_jacobian.mo", NONLINEAR)
        .unwrap();
    let compiled = session.compile_model("OwnedJacobian").unwrap();
    let before = PREPARATIONS.with(std::cell::Cell::get);
    crate::simulate_with_diagnostics(
        &compiled.dae,
        &crate::SimOptions {
            solver_mode: rumoca_solver::SimSolverMode::RkLike,
            t_end: 0.02,
            dt: Some(0.01),
            max_wall_seconds: Some(30.0),
            ..crate::SimOptions::default()
        },
    )
    .unwrap();
    assert_eq!(PREPARATIONS.with(std::cell::Cell::get), before);
}

fn model(text: &str, name: &str) -> SolveModel {
    let mut session = Session::new(SessionConfig::default());
    session.add_document("owned_jacobian.mo", text).unwrap();
    let compiled = session.compile_model(name).unwrap();
    crate::lower_dae_for_simulation(&compiled.dae, &crate::SimOptions::default()).unwrap()
}

const NONLINEAR: &str = "model OwnedJacobian
  parameter Real p=2;
  Real x(start=1, fixed=true);
  Real y(start=2, fixed=true);
equation
  der(x)=p*x*x+sin(y);
  der(y)=x*y-time;
end OwnedJacobian;";

fn compiled(source: SolveExecutionSource) -> Rc<dyn CompiledSolveModelStateJacobian> {
    match prepare(source).unwrap() {
        ModelStateJacobianPreparation::Compiled(code) => code,
        ModelStateJacobianPreparation::Unsupported { reason } => {
            panic!("unexpected decline: {reason}")
        }
    }
}

#[test]
fn retained_native_model_jacobian_outlives_the_external_source_handle() {
    let source = SolveExecutionSource::new(model(NONLINEAR, "OwnedJacobian"));
    let parameters = source.model().parameters.clone();
    let code = compiled(source.clone());
    drop(source);
    let mut out = [f64::NAN; 4];
    code.call(&[1.0, 2.0], &parameters, 0.5, &mut out).unwrap();
    assert!(out.iter().all(|value| value.is_finite()));
    code.call(&[2.0, 3.0], &parameters, 1.0, &mut out).unwrap();
    assert!(out.iter().all(|value| value.is_finite()));
}

#[test]
fn retained_native_model_jacobian_matches_the_independent_directional_runtime() {
    let model = model(NONLINEAR, "OwnedJacobian");
    let reference = rumoca_solver::SolveRuntime::new(&model).unwrap();
    assert!(reference.model_state_jacobian_fallback_reason().is_some());
    let code = compiled(SolveExecutionSource::new(model.clone()));
    for (time, state) in [(0.0, [1.0, 2.0]), (0.5, [-0.25, 3.0]), (2.0, [4.0, -1.0])] {
        let report = reference.eval_state_jacobian(
            time,
            &state,
            &model.parameters,
            rumoca_solver::AlgebraicSettle {
                tol: 1e-12,
                max_iters: 64,
            },
        );
        assert!(report.error.is_none(), "{:?}", report.error);
        let mut actual = [f64::NAN; 4];
        code.call(&state, &model.parameters, time, &mut actual)
            .unwrap();
        for (got, expected) in actual.iter().zip(report.matrix.iter().flatten()) {
            assert!((got - expected).abs() <= 1e-12 * (1.0 + expected.abs()));
        }
    }
}

#[test]
fn unsupported_projection_retains_a_specific_capability_reason() {
    let model = model(
        "model ProjectedJacobian
      Real x(start=1, fixed=true); Real a;
    equation der(x)=a; a+a*a=x; end ProjectedJacobian;",
        "ProjectedJacobian",
    );
    match prepare(SolveExecutionSource::new(model)).unwrap() {
        ModelStateJacobianPreparation::Unsupported { reason } => {
            assert!(
                reason.contains("solver coordinates") || reason.contains("algebraic"),
                "{reason}"
            );
        }
        ModelStateJacobianPreparation::Compiled(_) => {
            panic!("projection must keep its directional base")
        }
    }
}

#[test]
fn finite_model_guard_failure_keeps_the_complete_caller_output() {
    let model = model(
        "model ConstantJacobian
      Real x(start=1, fixed=true); Real y(start=2, fixed=true);
    equation der(x)=1; der(y)=2; end ConstantJacobian;",
        "ConstantJacobian",
    );
    let parameters = model.parameters.clone();
    let code = compiled(SolveExecutionSource::new(model));
    let mut out = [41.0, 42.0, 43.0, 44.0];
    assert!(
        code.call(&[f64::NAN, 2.0], &parameters, 0.0, &mut out)
            .is_err()
    );
    assert_eq!(out, [41.0, 42.0, 43.0, 44.0]);
    code.call(&[1.0, 2.0], &parameters, 0.0, &mut out).unwrap();
    assert!(out.iter().all(|value| *value == 0.0));
}

#[cfg(feature = "solver-rk45")]
#[test]
fn explicit_rk_session_has_no_unrequested_model_jacobian_decline() {
    let mut compiler = Session::new(SessionConfig::default());
    compiler
        .add_document("explicit_receipt.mo", NONLINEAR)
        .unwrap();
    let compiled = compiler.compile_model("OwnedJacobian").unwrap();
    let before = PREPARATIONS.with(std::cell::Cell::get);
    let mut session = crate::SimulationSession::new(
        &compiled.dae,
        crate::SimOptions {
            solver_mode: rumoca_solver::SimSolverMode::RkLike,
            t_end: 0.02,
            dt: Some(0.01),
            max_wall_seconds: Some(30.0),
            ..Default::default()
        },
    )
    .unwrap();
    session.advance_to(0.01).unwrap();
    assert!(session.execution_receipt().declined.is_empty());
    session.reset(0.0).unwrap();
    session.advance_to(0.01).unwrap();
    assert!(session.execution_receipt().declined.is_empty());
    assert_eq!(PREPARATIONS.with(std::cell::Cell::get), before);
}
