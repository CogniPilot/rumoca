//! Retry policy for cached OMC reference failures.
//!
//! A cached `error`/`timeout` used to be reusable unconditionally, so any
//! transient failure (host hiccup, OOM kill, loaded machine) became permanent:
//! the model was never re-run and silently dropped out of the parity comparison
//! forever. A failure now has to reproduce before it is believed.

use super::SimModelResult;

/// Consecutive reproductions before a cached OMC failure is treated as real.
///
/// Without this every transient failure (a host hiccup, an OOM kill, a machine
/// under load) became permanent: `cached_omc_result_is_reusable` returned `true`
/// for `error`/`timeout` unconditionally, so the model was never re-run and it
/// silently dropped out of the parity comparison forever.
pub(super) const OMC_FAILURE_RETRY_ATTEMPTS: u32 = 2;

/// Larger budget for failures whose text points at the environment rather than
/// the model — those are the ones most likely to clear on a retry.
pub(super) const OMC_TRANSIENT_FAILURE_RETRY_ATTEMPTS: u32 = 4;

/// Failure texts that indicate a host/environment problem, not a model defect.
pub(super) const OMC_TRANSIENT_FAILURE_MARKERS: &[&str] = &[
    "timed out",
    "timeout",
    "killed",
    "out of memory",
    "cannot allocate",
    "resource temporarily unavailable",
    "signal 9",
    "no space left",
    "session io error",
    "session spawn failed",
];

pub(super) fn omc_failure_looks_transient(result: &SimModelResult) -> bool {
    if result.status == "timeout" {
        return true;
    }
    let Some(error) = result.error.as_deref() else {
        return false;
    };
    let error = error.to_ascii_lowercase();
    OMC_TRANSIENT_FAILURE_MARKERS
        .iter()
        .any(|marker| error.contains(marker))
}

pub(super) fn omc_failure_retry_budget(result: &SimModelResult) -> u32 {
    if omc_failure_looks_transient(result) {
        OMC_TRANSIENT_FAILURE_RETRY_ATTEMPTS
    } else {
        OMC_FAILURE_RETRY_ATTEMPTS
    }
}

pub(super) fn omc_result_is_failure(result: &SimModelResult) -> bool {
    matches!(result.status.as_str(), "error" | "timeout")
}

/// Carry the cached attempt counter into a freshly produced result.
///
/// The counter must persist into the cached JSON or the retry loop never
/// terminates: each run would see `failed_attempts == 0` and re-run forever.
pub(super) fn carry_failed_attempts(fresh: &mut SimModelResult, prior: Option<&SimModelResult>) {
    if !omc_result_is_failure(fresh) {
        fresh.failed_attempts = 0;
        return;
    }
    let prior_attempts = prior
        .filter(|prior| omc_result_is_failure(prior))
        .map_or(0, |prior| prior.failed_attempts);
    fresh.failed_attempts = prior_attempts.saturating_add(1);
}
