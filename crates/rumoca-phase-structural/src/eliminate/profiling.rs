#[cfg(feature = "tracing")]
pub(super) fn eliminate_profile_enabled() -> bool {
    tracing::enabled!(target: "rumoca_phase_structural::eliminate", tracing::Level::DEBUG)
}

#[cfg(not(feature = "tracing"))]
pub(super) fn eliminate_profile_enabled() -> bool {
    false
}

#[cfg(feature = "tracing")]
pub(super) fn log_eliminate_profile(
    enabled: bool,
    phase: &'static str,
    start: rumoca_core::OptionalTimer,
    count: usize,
) {
    if !enabled {
        return;
    }
    tracing::debug!(
        target: "rumoca_phase_structural::eliminate",
        phase,
        elapsed_seconds = rumoca_core::maybe_elapsed_seconds(start),
        count,
        "eliminate_trivial stage"
    );
}

#[cfg(feature = "tracing")]
pub(super) fn log_blt_profile(enabled: bool, n_scalar: usize, n_loops: usize, max_loop: usize) {
    if enabled {
        tracing::debug!(
            target: "rumoca_phase_structural::eliminate",
            n_scalar,
            n_loops,
            max_loop,
            "eliminate_trivial BLT shape"
        );
    }
}

#[cfg(not(feature = "tracing"))]
pub(super) fn log_blt_profile(_enabled: bool, _n_scalar: usize, _n_loops: usize, _max_loop: usize) {
}

#[cfg(not(feature = "tracing"))]
pub(super) fn log_eliminate_profile(
    _enabled: bool,
    _phase: &'static str,
    _start: rumoca_core::OptionalTimer,
    _count: usize,
) {
}
