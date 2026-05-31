use super::*;
use std::cell::RefCell;
use std::sync::atomic::{AtomicU64, Ordering};

/// Aggregate timing for a compilation phase across all model compiles.
#[derive(Debug, Clone, Copy, Default)]
pub struct CompilePhaseTimingStat {
    /// Number of times this phase executed.
    pub calls: u64,
    /// Total wall-clock time spent in this phase.
    pub total_nanos: u64,
}

impl CompilePhaseTimingStat {
    pub fn total_seconds(self) -> f64 {
        self.total_nanos as f64 / 1_000_000_000.0
    }
}

/// Snapshot of compile phase timing accumulators.
#[derive(Debug, Clone, Copy, Default)]
pub struct CompilePhaseTimingSnapshot {
    pub instantiate: CompilePhaseTimingStat,
    pub typecheck: CompilePhaseTimingStat,
    pub flatten: CompilePhaseTimingStat,
    pub todae: CompilePhaseTimingStat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilePhaseEvent {
    Started,
    Completed,
}

type CompilePhaseObserver = Box<dyn FnMut(FailedPhase, CompilePhaseEvent)>;

thread_local! {
    static COMPILE_PHASE_OBSERVER: RefCell<Option<CompilePhaseObserver>> = RefCell::new(None);
}

pub struct CompilePhaseObserverGuard {
    previous: Option<CompilePhaseObserver>,
}

impl Drop for CompilePhaseObserverGuard {
    fn drop(&mut self) {
        COMPILE_PHASE_OBSERVER.with(|observer| {
            *observer.borrow_mut() = self.previous.take();
        });
    }
}

pub fn install_compile_phase_observer(
    observer: impl FnMut(FailedPhase, CompilePhaseEvent) + 'static,
) -> CompilePhaseObserverGuard {
    let previous =
        COMPILE_PHASE_OBSERVER.with(|slot| slot.borrow_mut().replace(Box::new(observer)));
    CompilePhaseObserverGuard { previous }
}

#[inline]
pub(super) fn notify_compile_phase(phase: FailedPhase, event: CompilePhaseEvent) {
    COMPILE_PHASE_OBSERVER.with(|observer| {
        if let Some(observer) = observer.borrow_mut().as_mut() {
            observer(phase, event);
        }
    });
}

static INSTANTIATE_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static INSTANTIATE_CALLS: AtomicU64 = AtomicU64::new(0);
static TYPECHECK_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static TYPECHECK_CALLS: AtomicU64 = AtomicU64::new(0);
static FLATTEN_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static FLATTEN_CALLS: AtomicU64 = AtomicU64::new(0);
static TODAE_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static TODAE_CALLS: AtomicU64 = AtomicU64::new(0);

fn duration_to_nanos(duration: Duration) -> u64 {
    let nanos = duration.as_nanos();
    if nanos > u128::from(u64::MAX) {
        u64::MAX
    } else {
        nanos as u64
    }
}

fn phase_stat(total_nanos: &AtomicU64, calls: &AtomicU64) -> CompilePhaseTimingStat {
    CompilePhaseTimingStat {
        calls: calls.load(Ordering::Relaxed),
        total_nanos: total_nanos.load(Ordering::Relaxed),
    }
}

fn record_compile_phase_timing(phase: FailedPhase, elapsed: Duration) {
    let nanos = duration_to_nanos(elapsed);
    match phase {
        FailedPhase::Instantiate => {
            INSTANTIATE_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            INSTANTIATE_CALLS.fetch_add(1, Ordering::Relaxed);
        }
        FailedPhase::Typecheck => {
            TYPECHECK_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            TYPECHECK_CALLS.fetch_add(1, Ordering::Relaxed);
        }
        FailedPhase::Flatten => {
            FLATTEN_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            FLATTEN_CALLS.fetch_add(1, Ordering::Relaxed);
        }
        FailedPhase::ToDae => {
            TODAE_TOTAL_NANOS.fetch_add(nanos, Ordering::Relaxed);
            TODAE_CALLS.fetch_add(1, Ordering::Relaxed);
        }
    }
}

#[inline]
pub(super) fn maybe_record_compile_phase_timing(phase: FailedPhase, start: OptionalTimer) {
    if let Some(elapsed) = maybe_elapsed_duration(start) {
        record_compile_phase_timing(phase, elapsed);
    }
}

/// Reset global compile phase timing accumulators.
pub fn reset_compile_phase_timing_stats() {
    INSTANTIATE_TOTAL_NANOS.store(0, Ordering::Relaxed);
    INSTANTIATE_CALLS.store(0, Ordering::Relaxed);
    TYPECHECK_TOTAL_NANOS.store(0, Ordering::Relaxed);
    TYPECHECK_CALLS.store(0, Ordering::Relaxed);
    FLATTEN_TOTAL_NANOS.store(0, Ordering::Relaxed);
    FLATTEN_CALLS.store(0, Ordering::Relaxed);
    TODAE_TOTAL_NANOS.store(0, Ordering::Relaxed);
    TODAE_CALLS.store(0, Ordering::Relaxed);
}

/// Snapshot global compile phase timing accumulators.
pub fn compile_phase_timing_stats() -> CompilePhaseTimingSnapshot {
    CompilePhaseTimingSnapshot {
        instantiate: phase_stat(&INSTANTIATE_TOTAL_NANOS, &INSTANTIATE_CALLS),
        typecheck: phase_stat(&TYPECHECK_TOTAL_NANOS, &TYPECHECK_CALLS),
        flatten: phase_stat(&FLATTEN_TOTAL_NANOS, &FLATTEN_CALLS),
        todae: phase_stat(&TODAE_TOTAL_NANOS, &TODAE_CALLS),
    }
}
