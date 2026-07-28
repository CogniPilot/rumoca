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

/// [`log_stage`] plus the row/node counts the stage consumed and produced, so a
/// slow stage can be read as "many rows" versus "superlinear in rows".
pub(crate) fn log_stage_with_shape(
    label: &'static str,
    start: StageTimer,
    rows: usize,
    nodes: usize,
) {
    tracing::debug!(
        target: "rumoca_phase_solve::timing",
        phase = label,
        elapsed_seconds = stage_elapsed_seconds(start),
        rows,
        nodes,
        "solve model lowering stage"
    );
}
