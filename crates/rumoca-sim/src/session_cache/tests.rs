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
        solver_mode: SimSolverMode::RkLike,
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
