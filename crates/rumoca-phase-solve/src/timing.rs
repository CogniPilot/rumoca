#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

#[cfg(not(target_arch = "wasm32"))]
pub(crate) type StageTimer = Instant;

#[cfg(target_arch = "wasm32")]
pub(crate) type StageTimer = ();

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn stage_start() -> StageTimer {
    Instant::now()
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn stage_start() -> StageTimer {}

#[cfg(not(target_arch = "wasm32"))]
fn stage_elapsed_seconds(start: StageTimer) -> f64 {
    start.elapsed().as_secs_f64()
}

#[cfg(target_arch = "wasm32")]
fn stage_elapsed_seconds(_start: StageTimer) -> f64 {
    0.0
}

pub(crate) fn log_stage(label: &'static str, start: StageTimer) {
    tracing::debug!(
        target: "rumoca_phase_solve::timing",
        phase = label,
        elapsed_seconds = stage_elapsed_seconds(start),
        "solve model lowering stage"
    );
}
