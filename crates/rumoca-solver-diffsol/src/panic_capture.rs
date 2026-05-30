//! Panic capture for solver calls into diffsol/faer.
//!
//! diffsol and faer signal some unrecoverable conditions (e.g. a structurally
//! singular Newton sparsity) by panicking rather than returning an error. We
//! catch those panics so a failed model degrades gracefully to a
//! `sim_solver_fail` instead of aborting the worker, and we record the panic
//! `Location` so the resulting message points at the originating source line
//! (e.g. faer's `sparse_lu.rs`).

use std::any::Any;
use std::fmt::Display;
use std::sync::Mutex;

use crate::SimError;

/// Serializes installation of the panic hook, which is process-global.
static SOLVER_PANIC_HOOK_LOCK: Mutex<()> = Mutex::new(());

pub(crate) fn solver_call<T, E, F>(context: &str, f: F) -> Result<T, SimError>
where
    E: Display,
    F: FnOnce() -> Result<T, E>,
{
    match catch_solver_panic(f) {
        Ok(Ok(value)) => Ok(value),
        Ok(Err(error)) => Err(SimError::SolverError(format!("{context}: {error}"))),
        Err(message) => Err(SimError::SolverError(format!(
            "{context} panicked: {message}"
        ))),
    }
}

pub(crate) fn catch_solver_panic<T, F>(f: F) -> Result<T, String>
where
    F: FnOnce() -> T,
{
    let _guard = SOLVER_PANIC_HOOK_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let previous_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|info| {
        if let Some(location) = info.location() {
            PANIC_LOCATION.with(|slot| {
                *slot.borrow_mut() = Some(format!(
                    "{}:{}:{}",
                    location.file(),
                    location.line(),
                    location.column()
                ));
            });
        }
    }));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    std::panic::set_hook(previous_hook);
    result.map_err(panic_message)
}

thread_local! {
    static PANIC_LOCATION: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

fn panic_message(panic_info: Box<dyn Any + Send>) -> String {
    let location = PANIC_LOCATION.with(|slot| slot.borrow_mut().take());
    let base = if let Some(message) = panic_info.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = panic_info.downcast_ref::<String>() {
        message.clone()
    } else {
        "unknown panic".to_string()
    };
    match location {
        Some(loc) => format!("{base} (at {loc})"),
        None => base,
    }
}
