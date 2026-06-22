//! Stage timing primitives and lowering-trace logging hooks shared across the
//! solve-lowering stages. The timers compile to no-ops on `wasm32` (no
//! `std::time::Instant`), so the lowering stages stay portable.

#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

#[cfg(not(target_arch = "wasm32"))]
pub(super) type StageTimer = Instant;

#[cfg(target_arch = "wasm32")]
pub(super) type StageTimer = ();

#[cfg(not(target_arch = "wasm32"))]
pub(super) fn stage_timer_start() -> StageTimer {
    Instant::now()
}

#[cfg(target_arch = "wasm32")]
pub(super) fn stage_timer_start() -> StageTimer {}

#[cfg(not(target_arch = "wasm32"))]
pub(super) fn stage_timer_elapsed_seconds(start: StageTimer) -> f64 {
    start.elapsed().as_secs_f64()
}

#[cfg(target_arch = "wasm32")]
pub(super) fn stage_timer_elapsed_seconds(_start: StageTimer) -> f64 {
    0.0
}

pub(super) fn log_solve_lowering_start(_label: &str) {}

pub(super) fn log_solve_lowering_done(_label: &str, _start: StageTimer) {}
