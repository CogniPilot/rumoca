use super::*;
use crate::{SimExecutionPolicy, SimSolverMode};
use rumoca_compile::compile::{Session, SessionConfig};

fn dae() -> Arc<rumoca_ir_dae::Dae> {
    let mut compiler = Session::new(SessionConfig::default());
    compiler
        .add_document(
            "cached.mo",
            "model Cached input Real u; Real x(start=1, fixed=true); \
             equation der(x)=u; end Cached;",
        )
        .unwrap();
    compiler.compile_model("Cached").unwrap().dae
}

fn options() -> SimOptions {
    SimOptions {
        solver_mode: if cfg!(feature = "solver-rk45") {
            SimSolverMode::RkLike
        } else {
            SimSolverMode::Bdf
        },
        execution_policy: SimExecutionPolicy::Interpreter,
        dt: Some(0.01),
        initial_inputs: vec![("u".into(), 0.0)],
        ..Default::default()
    }
}

fn value(lease: &mut SimulationSessionLease, name: &str) -> f64 {
    match lease.apply(SessionCommand::Get { name: name.into() }) {
        SessionEvent::Value { value, .. } => value.unwrap(),
        other => panic!("expected a value, got {other:?}"),
    }
}

fn drive(lease: &mut SimulationSessionLease) -> f64 {
    assert!(matches!(
        lease.apply(SessionCommand::SetInput {
            name: "u".into(),
            value: 2.0,
        }),
        SessionEvent::Ok { .. }
    ));
    assert!(matches!(
        lease.apply(SessionCommand::AdvanceTo { time: 0.25 }),
        SessionEvent::Ok { .. }
    ));
    value(lease, "x")
}

#[test]
fn released_session_resets_defaults_and_replays_its_trajectory() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    let mut first = cache.acquire(Arc::clone(&model), options()).unwrap();
    assert!(!first.preparation_reused());
    assert_eq!(value(&mut first, "x"), 1.0);
    let expected = drive(&mut first);
    assert!((expected - 1.5).abs() < 1e-10);
    drop(first);

    let mut repeat = cache.acquire(model, options()).unwrap();
    assert!(repeat.preparation_reused());
    assert_eq!(repeat.time().to_bits(), 0.0_f64.to_bits());
    assert_eq!(value(&mut repeat, "x"), 1.0);
    assert_eq!(value(&mut repeat, "u").to_bits(), 0.0_f64.to_bits());
    assert_eq!(drive(&mut repeat).to_bits(), expected.to_bits());
}

#[test]
fn active_leases_never_share_mutable_state() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    let mut first = cache.acquire(Arc::clone(&model), options()).unwrap();
    let mut second = cache.acquire(Arc::clone(&model), options()).unwrap();
    assert!(!second.preparation_reused());
    drive(&mut first);
    assert_eq!(second.time(), 0.0);
    assert_eq!(value(&mut second, "x"), 1.0);
    drop(first);
    let mut third = cache.acquire(model, options()).unwrap();
    assert!(third.preparation_reused());
    drive(&mut third);
    assert_eq!(second.time(), 0.0);
    assert_eq!(value(&mut second, "u"), 0.0);
}

#[test]
fn different_dae_owner_or_exact_options_cannot_reuse_a_session() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    drop(cache.acquire(Arc::clone(&model), options()).unwrap());
    // Identical source bytes do not make two compiler-owned DAEs identical.
    let other = cache.acquire(dae(), options()).unwrap();
    assert!(!other.preparation_reused());
    drop(other);
    drop(cache.acquire(Arc::clone(&model), options()).unwrap());
    let mut changed = options();
    changed.dt = Some(0.02);
    let different = cache.acquire(Arc::clone(&model), changed).unwrap();
    assert!(!different.preparation_reused());
    drop(different);
    drop(cache.acquire(Arc::clone(&model), options()).unwrap());
    let mut negative_zero = options();
    negative_zero.initial_inputs[0].1 = -0.0;
    assert!(
        !cache
            .acquire(model, negative_zero)
            .unwrap()
            .preparation_reused()
    );
}

#[test]
fn rejected_commands_and_cache_clear_prevent_reuse() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    let mut failed = cache.acquire(Arc::clone(&model), options()).unwrap();
    assert!(matches!(
        failed.apply(SessionCommand::SetInput {
            name: "missing".into(),
            value: 1.0,
        }),
        SessionEvent::Error { .. }
    ));
    drop(failed);
    let active = cache.acquire(Arc::clone(&model), options()).unwrap();
    assert!(!active.preparation_reused());
    cache.clear();
    drop(active);
    let next = cache.acquire(Arc::clone(&model), options()).unwrap();
    assert!(!next.preparation_reused());
    drop(next);
    cache.clear();
    assert!(
        !cache
            .acquire(model, options())
            .unwrap()
            .preparation_reused()
    );
}

#[test]
fn active_session_survives_the_cache_owner() {
    let cache = SimulationSessionCache::default();
    let mut active = cache.acquire(dae(), options()).unwrap();
    drop(cache);
    assert!((drive(&mut active) - 1.5).abs() < 1e-10);
    drop(active);
}

#[cfg(feature = "solver-rk45")]
#[test]
fn invalid_typed_payload_refuses_without_consuming_the_idle_session() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    drop(cache.acquire(Arc::clone(&model), options()).unwrap());
    let mut invalid = options();
    invalid.backend_options = Some(crate::BackendOptions::new(
        crate::NumericalBackend::Rk45,
        7_u32,
    ));
    assert!(matches!(
        cache.acquire(Arc::clone(&model), invalid),
        Err(SimulationDiagnosticError::BackendOptions(
            crate::BackendOptionsError::ConcreteType {
                backend: crate::NumericalBackend::Rk45
            }
        ))
    ));
    let mut reused = cache.acquire(model, options()).unwrap();
    assert!(reused.preparation_reused());
    assert_eq!(value(&mut reused, "x"), 1.0);
    assert!((drive(&mut reused) - 1.5).abs() < 1e-10);
}

#[cfg(feature = "solver-rk45")]
#[test]
fn explicit_backend_default_is_consumed_and_has_its_own_cache_identity() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    let mut absent = cache.acquire(Arc::clone(&model), options()).unwrap();
    let expected = drive(&mut absent);
    drop(absent);
    let mut explicit = options();
    explicit.backend_options = Some(crate::BackendOptions::new(
        crate::NumericalBackend::Rk45,
        rumoca_solver_rk45::Rk45Options,
    ));
    let mut configured = cache.acquire(Arc::clone(&model), explicit.clone()).unwrap();
    assert!(!configured.preparation_reused());
    assert_eq!(drive(&mut configured).to_bits(), expected.to_bits());
    drop(configured);
    let mut reused = cache.acquire(model, explicit).unwrap();
    assert!(reused.preparation_reused());
    assert_eq!(drive(&mut reused).to_bits(), expected.to_bits());
}

#[cfg(feature = "solver-velimplicit")]
fn velimplicit_options(config: rumoca_solver_velimplicit::VelImplicitConfig) -> SimOptions {
    SimOptions {
        solver_mode: SimSolverMode::VelImplicit,
        backend_options: Some(crate::BackendOptions::new(
            crate::NumericalBackend::VelImplicit,
            config,
        )),
        dt: Some(0.25),
        ..options()
    }
}

#[cfg(feature = "solver-velimplicit")]
fn drive_with_step_count(lease: &mut SimulationSessionLease) -> (u64, f64) {
    rumoca_solver::reset_step_counts();
    let result = drive(lease);
    (rumoca_solver::step_counts().solver_steps, result)
}

#[cfg(feature = "solver-velimplicit")]
#[test]
fn configured_step_changes_real_integration_and_exact_cache_replay() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    let coarse = rumoca_solver_velimplicit::VelImplicitConfig {
        step: 0.03125,
        ..Default::default()
    };
    let fine = rumoca_solver_velimplicit::VelImplicitConfig {
        step: 0.015625,
        ..coarse
    };
    let mut first = cache
        .acquire(Arc::clone(&model), velimplicit_options(coarse))
        .unwrap();
    assert!(!first.preparation_reused());
    let (coarse_steps, expected) = drive_with_step_count(&mut first);
    assert!(coarse_steps > 0);
    assert!((expected - 1.5).abs() < 1e-10);
    drop(first);

    let mut changed = cache
        .acquire(Arc::clone(&model), velimplicit_options(fine))
        .unwrap();
    assert!(!changed.preparation_reused());
    let (fine_steps, actual) = drive_with_step_count(&mut changed);
    assert!(fine_steps > coarse_steps);
    assert!((actual - expected).abs() < 1e-10);
    drop(changed);

    let mut replay = cache.acquire(model, velimplicit_options(fine)).unwrap();
    assert!(replay.preparation_reused());
    assert_eq!(value(&mut replay, "x"), 1.0);
    let (replayed_steps, replayed_value) = drive_with_step_count(&mut replay);
    assert_eq!(replayed_steps, fine_steps);
    assert_eq!(replayed_value.to_bits(), actual.to_bits());
}

#[cfg(feature = "solver-velimplicit")]
#[test]
fn invalid_custom_value_preserves_the_existing_prepared_session() {
    let cache = SimulationSessionCache::default();
    let model = dae();
    let valid = rumoca_solver_velimplicit::VelImplicitConfig::default();
    drop(
        cache
            .acquire(Arc::clone(&model), velimplicit_options(valid))
            .unwrap(),
    );
    let invalid = rumoca_solver_velimplicit::VelImplicitConfig {
        step: f64::NAN,
        ..valid
    };
    assert!(matches!(
        cache.acquire(Arc::clone(&model), velimplicit_options(invalid)),
        Err(SimulationDiagnosticError::BackendOptions(
            crate::BackendOptionsError::InvalidValue {
                backend: crate::NumericalBackend::VelImplicit,
                field: "step",
            }
        ))
    ));
    let mut reused = cache.acquire(model, velimplicit_options(valid)).unwrap();
    assert!(reused.preparation_reused());
    assert!((drive(&mut reused) - 1.5).abs() < 1e-10);
}
