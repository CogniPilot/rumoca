use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct HotpathStatsSnapshot {
    pub solver_steps: u64,
    pub root_hits: u64,
    pub no_state_eval_points: u64,
    pub no_state_settles: u64,
    pub clock_edge_evals: u64,
    pub sample_active_checks: u64,
    pub sample_active_true: u64,
    pub held_value_reads: u64,
    pub left_limit_reads: u64,
    pub explicit_clock_inference: u64,
    pub clock_alias_source_scans: u64,
}

static ENABLED: OnceLock<bool> = OnceLock::new();
static SOLVER_STEPS: AtomicU64 = AtomicU64::new(0);
static ROOT_HITS: AtomicU64 = AtomicU64::new(0);
static NO_STATE_EVAL_POINTS: AtomicU64 = AtomicU64::new(0);
static NO_STATE_SETTLES: AtomicU64 = AtomicU64::new(0);
static CLOCK_EDGE_EVALS: AtomicU64 = AtomicU64::new(0);
static SAMPLE_ACTIVE_CHECKS: AtomicU64 = AtomicU64::new(0);
static SAMPLE_ACTIVE_TRUE: AtomicU64 = AtomicU64::new(0);
static HELD_VALUE_READS: AtomicU64 = AtomicU64::new(0);
static LEFT_LIMIT_READS: AtomicU64 = AtomicU64::new(0);
static EXPLICIT_CLOCK_INFERENCE: AtomicU64 = AtomicU64::new(0);
static CLOCK_ALIAS_SOURCE_SCANS: AtomicU64 = AtomicU64::new(0);

fn enabled() -> bool {
    *ENABLED.get_or_init(|| std::env::var("RUMOCA_SIM_HOTPATH_STATS").is_ok())
}

fn reset_counter(counter: &AtomicU64) {
    counter.store(0, Ordering::Relaxed);
}

fn bump(counter: &AtomicU64) {
    if enabled() {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn reset() {
    if !enabled() {
        return;
    }
    for counter in [
        &SOLVER_STEPS,
        &ROOT_HITS,
        &NO_STATE_EVAL_POINTS,
        &NO_STATE_SETTLES,
        &CLOCK_EDGE_EVALS,
        &SAMPLE_ACTIVE_CHECKS,
        &SAMPLE_ACTIVE_TRUE,
        &HELD_VALUE_READS,
        &LEFT_LIMIT_READS,
        &EXPLICIT_CLOCK_INFERENCE,
        &CLOCK_ALIAS_SOURCE_SCANS,
    ] {
        reset_counter(counter);
    }
}

pub fn snapshot() -> Option<HotpathStatsSnapshot> {
    if !enabled() {
        return None;
    }
    Some(HotpathStatsSnapshot {
        solver_steps: SOLVER_STEPS.load(Ordering::Relaxed),
        root_hits: ROOT_HITS.load(Ordering::Relaxed),
        no_state_eval_points: NO_STATE_EVAL_POINTS.load(Ordering::Relaxed),
        no_state_settles: NO_STATE_SETTLES.load(Ordering::Relaxed),
        clock_edge_evals: CLOCK_EDGE_EVALS.load(Ordering::Relaxed),
        sample_active_checks: SAMPLE_ACTIVE_CHECKS.load(Ordering::Relaxed),
        sample_active_true: SAMPLE_ACTIVE_TRUE.load(Ordering::Relaxed),
        held_value_reads: HELD_VALUE_READS.load(Ordering::Relaxed),
        left_limit_reads: LEFT_LIMIT_READS.load(Ordering::Relaxed),
        explicit_clock_inference: EXPLICIT_CLOCK_INFERENCE.load(Ordering::Relaxed),
        clock_alias_source_scans: CLOCK_ALIAS_SOURCE_SCANS.load(Ordering::Relaxed),
    })
}

pub(crate) fn inc_solver_step() {
    bump(&SOLVER_STEPS);
}

pub(crate) fn inc_root_hit() {
    bump(&ROOT_HITS);
}

pub(crate) fn inc_no_state_eval_point() {
    bump(&NO_STATE_EVAL_POINTS);
}

pub(crate) fn inc_no_state_settle() {
    bump(&NO_STATE_SETTLES);
}

pub(crate) fn inc_clock_edge_eval() {
    bump(&CLOCK_EDGE_EVALS);
}

pub(crate) fn inc_sample_active_check(active: bool) {
    bump(&SAMPLE_ACTIVE_CHECKS);
    if active {
        bump(&SAMPLE_ACTIVE_TRUE);
    }
}

pub(crate) fn inc_held_value_read() {
    bump(&HELD_VALUE_READS);
}

pub(crate) fn inc_left_limit_read() {
    bump(&LEFT_LIMIT_READS);
}

pub(crate) fn inc_explicit_clock_inference() {
    bump(&EXPLICIT_CLOCK_INFERENCE);
}

pub(crate) fn inc_clock_alias_source_scan() {
    bump(&CLOCK_ALIAS_SOURCE_SCANS);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_disabled_without_env() {
        if std::env::var("RUMOCA_SIM_HOTPATH_STATS").is_ok() {
            return;
        }
        reset();
        assert_eq!(snapshot(), None);
    }
}
