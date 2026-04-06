use std::any::Any;
use std::cell::Cell;
#[cfg(not(target_arch = "wasm32"))]
use std::time::Duration;
use std::time::Instant;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TimeoutBudget {
    deadline: Option<Instant>,
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
        #[cfg(target_arch = "wasm32")]
        let deadline = None;
        #[cfg(not(target_arch = "wasm32"))]
        let deadline = seconds.map(|s| Instant::now() + Duration::from_secs_f64(s));
        Self { deadline, seconds }
    }

    #[inline]
    pub fn check(&self) -> Result<(), TimeoutExceeded> {
        #[cfg(not(target_arch = "wasm32"))]
        if self.deadline.is_some_and(|d| Instant::now() >= d) {
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
    pub fn deadline(&self) -> Option<Instant> {
        self.deadline
    }
}

#[derive(Debug)]
pub struct SolverTimeoutPanic;

thread_local! {
    static SOLVER_DEADLINE: Cell<Option<Instant>> = const { Cell::new(None) };
}

pub struct SolverDeadlineGuard {
    prev: Option<Instant>,
}

impl SolverDeadlineGuard {
    pub fn install(deadline: Option<Instant>) -> Self {
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
    #[cfg(not(target_arch = "wasm32"))]
    {
        let expired = SOLVER_DEADLINE.with(|cell| cell.get().is_some_and(|d| Instant::now() >= d));
        if expired {
            std::panic::panic_any(SolverTimeoutPanic);
        }
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
