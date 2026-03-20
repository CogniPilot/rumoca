//! Shared optional timing helpers for host-only instrumentation.
//!
//! These helpers centralize the common pattern used across compiler/runtime
//! crates: on native targets, start a wall-clock timer; on `wasm32`, return
//! `None` so instrumentation stays portable and does not call unsupported
//! `std::time::Instant` APIs.

use std::time::{Duration, Instant};

/// Optional wall-clock timer used by host-only instrumentation.
pub type OptionalTimer = Option<Instant>;

#[inline]
/// Start a wall-clock timer when the current target supports `Instant`.
pub fn maybe_start_timer() -> OptionalTimer {
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[inline]
/// Start a wall-clock timer only when `enabled` is true.
pub fn maybe_start_timer_if(enabled: bool) -> OptionalTimer {
    if enabled { maybe_start_timer() } else { None }
}

#[inline]
/// Compute the elapsed duration for an optional timer.
pub fn maybe_elapsed_duration(start: OptionalTimer) -> Option<Duration> {
    start.map(|t0| t0.elapsed())
}

#[inline]
/// Compute elapsed milliseconds for an optional timer, or `0` when disabled.
pub fn maybe_elapsed_ms(start: OptionalTimer) -> u128 {
    maybe_elapsed_duration(start).map_or(0, |elapsed| elapsed.as_millis())
}

#[inline]
/// Compute elapsed seconds for an optional timer, or `0.0` when disabled.
pub fn maybe_elapsed_seconds(start: OptionalTimer) -> f64 {
    maybe_elapsed_duration(start).map_or(0.0, |elapsed| elapsed.as_secs_f64())
}

#[cfg(test)]
mod tests {
    use super::{maybe_elapsed_ms, maybe_elapsed_seconds, maybe_start_timer, maybe_start_timer_if};

    #[test]
    fn disabled_timer_returns_zero_elapsed() {
        assert_eq!(maybe_elapsed_ms(None), 0);
        assert_eq!(maybe_elapsed_seconds(None), 0.0);
    }

    #[test]
    fn conditional_timer_respects_enable_flag() {
        assert!(maybe_start_timer_if(false).is_none());
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn host_timer_starts_when_enabled() {
        assert!(maybe_start_timer().is_some());
        assert!(maybe_start_timer_if(true).is_some());
    }

    #[cfg(target_arch = "wasm32")]
    #[test]
    fn wasm_timer_is_disabled() {
        assert!(maybe_start_timer().is_none());
        assert!(maybe_start_timer_if(true).is_none());
    }
}
