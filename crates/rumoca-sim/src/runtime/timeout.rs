use std::any::Any;
use std::cell::Cell;
#[cfg(not(target_arch = "wasm32"))]
use std::time::Duration;

#[cfg(target_arch = "wasm32")]
pub(crate) type WallClockInstant = f64;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) type WallClockInstant = std::time::Instant;

#[cfg(target_arch = "wasm32")]
#[inline]
pub(crate) fn wall_clock_now() -> WallClockInstant {
    js_sys::Date::now() / 1_000.0
}

#[cfg(not(target_arch = "wasm32"))]
#[inline]
pub(crate) fn wall_clock_now() -> WallClockInstant {
    std::time::Instant::now()
}

#[cfg(target_arch = "wasm32")]
#[inline]
pub(crate) fn wall_clock_elapsed_seconds(started_at: WallClockInstant) -> f64 {
    (wall_clock_now() - started_at).max(0.0)
}

#[cfg(not(target_arch = "wasm32"))]
#[inline]
pub(crate) fn wall_clock_elapsed_seconds(started_at: WallClockInstant) -> f64 {
    started_at.elapsed().as_secs_f64()
}

#[cfg(target_arch = "wasm32")]
#[inline]
fn wall_clock_deadline_after(seconds: f64) -> WallClockInstant {
    wall_clock_now() + seconds
}

#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn wall_clock_deadline_after(seconds: f64) -> WallClockInstant {
    wall_clock_now() + Duration::from_secs_f64(seconds)
}

#[cfg(target_arch = "wasm32")]
#[inline]
fn wall_clock_expired(deadline: WallClockInstant) -> bool {
    wall_clock_now() >= deadline
}

#[cfg(not(target_arch = "wasm32"))]
#[inline]
fn wall_clock_expired(deadline: WallClockInstant) -> bool {
    wall_clock_now() >= deadline
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TimeoutBudget {
    deadline: Option<WallClockInstant>,
    seconds: Option<f64>,
}

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("timeout after {seconds:.3}s")]
pub struct TimeoutExceeded {
    pub seconds: f64,
}

impl TimeoutBudget {
    pub fn new(max_wall_seconds: Option<f64>) -> Self {
        let seconds = max_wall_seconds.filter(|s| s.is_finite() && *s > 0.0);
        let deadline = seconds.map(wall_clock_deadline_after);
        Self { deadline, seconds }
    }

    #[inline]
    pub fn check(&self) -> Result<(), TimeoutExceeded> {
        if self.deadline.is_some_and(wall_clock_expired) {
            return Err(self.timeout_error());
        }
        Ok(())
    }

    #[inline]
    pub fn timeout_error(&self) -> TimeoutExceeded {
        TimeoutExceeded {
            seconds: self.seconds.unwrap_or(0.0),
        }
    }

    #[inline]
    pub(crate) fn deadline(&self) -> Option<WallClockInstant> {
        self.deadline
    }
}

#[derive(Debug)]
pub struct SolverTimeoutPanic;

thread_local! {
    static SOLVER_DEADLINE: Cell<Option<WallClockInstant>> = const { Cell::new(None) };
}

pub struct SolverDeadlineGuard {
    prev: Option<WallClockInstant>,
}

impl SolverDeadlineGuard {
    pub fn install(deadline: Option<WallClockInstant>) -> Self {
        let prev = SOLVER_DEADLINE.with(|cell| {
            let prev = cell.get();
            cell.set(deadline);
            prev
        });
        Self { prev }
    }
}

impl Drop for SolverDeadlineGuard {
    fn drop(&mut self) {
        SOLVER_DEADLINE.with(|cell| cell.set(self.prev));
    }
}

#[inline]
pub fn panic_on_expired_solver_deadline() {
    let expired = SOLVER_DEADLINE.with(|cell| cell.get().is_some_and(wall_clock_expired));
    if expired {
        std::panic::panic_any(SolverTimeoutPanic);
    }
}

#[inline]
pub fn is_solver_timeout_panic(payload: &(dyn Any + Send + 'static)) -> bool {
    payload.is::<SolverTimeoutPanic>()
}

pub fn run_timeout_step<E, F>(budget: &TimeoutBudget, step: F) -> Result<(), E>
where
    E: From<TimeoutExceeded>,
    F: FnOnce(),
{
    budget.check().map_err(E::from)?;
    step();
    budget.check().map_err(E::from)
}

pub fn run_timeout_step_result<E, F>(budget: &TimeoutBudget, step: F) -> Result<(), E>
where
    E: From<TimeoutExceeded>,
    F: FnOnce() -> Result<(), E>,
{
    budget.check().map_err(E::from)?;
    step()?;
    budget.check().map_err(E::from)
}

pub fn run_timeout_result<T, E, F>(budget: &TimeoutBudget, step: F) -> Result<T, E>
where
    E: From<TimeoutExceeded>,
    F: FnOnce() -> Result<T, E>,
{
    budget.check().map_err(E::from)?;
    let value = step()?;
    budget.check().map_err(E::from)?;
    Ok(value)
}
