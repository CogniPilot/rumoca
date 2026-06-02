use std::{
    sync::atomic::{AtomicU64, Ordering},
    time::Instant,
};

static RK_DERIVATIVE_CALLS: AtomicU64 = AtomicU64::new(0);
static RK_DERIVATIVE_NANOS: AtomicU64 = AtomicU64::new(0);
static RK_ROOT_CALLS: AtomicU64 = AtomicU64::new(0);
static RK_ROOT_NANOS: AtomicU64 = AtomicU64::new(0);

pub(super) fn rk_eval_trace_enabled() -> bool {
    tracing::enabled!(target: "rumoca_solver_rk45::eval", tracing::Level::DEBUG)
}

pub(super) fn reset_rk_eval_trace() {
    if !rk_eval_trace_enabled() {
        return;
    }
    for counter in [
        &RK_DERIVATIVE_CALLS,
        &RK_DERIVATIVE_NANOS,
        &RK_ROOT_CALLS,
        &RK_ROOT_NANOS,
    ] {
        counter.store(0, Ordering::Relaxed);
    }
}

pub(super) fn record_derivative_eval_trace(start: Instant) {
    RK_DERIVATIVE_CALLS.fetch_add(1, Ordering::Relaxed);
    RK_DERIVATIVE_NANOS.fetch_add(elapsed_nanos_u64(start), Ordering::Relaxed);
}

pub(super) fn record_root_eval_trace(start: Instant) {
    RK_ROOT_CALLS.fetch_add(1, Ordering::Relaxed);
    RK_ROOT_NANOS.fetch_add(elapsed_nanos_u64(start), Ordering::Relaxed);
}

pub(super) fn trace_rk_eval_snapshot(label: &str) {
    if !rk_eval_trace_enabled() {
        return;
    }
    let derivative_calls = RK_DERIVATIVE_CALLS.load(Ordering::Relaxed);
    let derivative_nanos = RK_DERIVATIVE_NANOS.load(Ordering::Relaxed);
    let root_calls = RK_ROOT_CALLS.load(Ordering::Relaxed);
    let root_nanos = RK_ROOT_NANOS.load(Ordering::Relaxed);
    tracing::debug!(
        target: "rumoca_solver_rk45::eval",
        "{label}: derivatives={} ({:.3}ms) roots={} ({:.3}ms) total_eval={:.3}ms",
        derivative_calls,
        nanos_to_ms(derivative_nanos),
        root_calls,
        nanos_to_ms(root_nanos),
        nanos_to_ms(derivative_nanos.saturating_add(root_nanos))
    );
}

fn elapsed_nanos_u64(start: Instant) -> u64 {
    start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64
}

fn nanos_to_ms(nanos: u64) -> f64 {
    nanos as f64 / 1.0e6
}
