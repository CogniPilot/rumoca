#[cfg(target_arch = "wasm32")]
use instant::Instant;
use std::any::Any;
use std::cell::Cell;
use std::time::Duration;
#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

pub type WallClockInstant = Instant;

#[inline]
pub fn wall_clock_now() -> WallClockInstant {
    Instant::now()
}

#[inline]
pub fn wall_clock_elapsed_seconds(started_at: WallClockInstant) -> f64 {
    started_at.elapsed().as_secs_f64()
}

#[inline]
fn wall_clock_deadline_after(seconds: f64) -> Option<WallClockInstant> {
    let duration = Duration::try_from_secs_f64(seconds).ok()?;
    wall_clock_now().checked_add(duration)
}

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
        let requested = max_wall_seconds.filter(|s| s.is_finite() && *s > 0.0);
        let deadline = requested.and_then(wall_clock_deadline_after);
        let seconds = requested;
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
    pub fn deadline(&self) -> Option<WallClockInstant> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn oversized_timeout_does_not_panic() {
        let budget = TimeoutBudget::new(Some(f64::MAX));
        assert_eq!(budget.deadline(), None);
        assert_eq!(budget.timeout_error().seconds, f64::MAX);
        assert_eq!(budget.check(), Ok(()));
    }
}
