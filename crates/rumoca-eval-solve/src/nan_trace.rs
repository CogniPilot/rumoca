//! Opt-in NaN / non-finite value tracing for runtime evaluation.
//!
//! Tracing is toggled programmatically via [`set_nan_trace`] so it can be driven
//! by a first-class CLI flag (e.g. `--nan-trace`) rather than an environment
//! variable, and switched on for any already-built binary without a recompile.
//!
//! It is *effectively* zero-cost when off: [`report_nonfinite`] is called at
//! coarse evaluation boundaries (once per state-derivative / residual eval, not
//! per scalar op), and when disabled it does nothing beyond a single relaxed
//! atomic load and a branch that is reliably predicted not-taken — negligible
//! against the hundreds of scalar operations each eval already performs. The
//! expensive per-element finiteness scan and the index→name resolution only run
//! when tracing is enabled.
//!
//! When enabled, each offending entry is named via the caller-provided
//! `name_of` closure (mapping a slot index back to its solver-variable name) so
//! a `NaN`/`inf` can be traced to the specific model variable that produced or
//! consumed it.

use std::sync::atomic::{AtomicBool, Ordering};

use rumoca_ir_solve as solve;

static NAN_TRACE_ENABLED: AtomicBool = AtomicBool::new(false);

/// Enable or disable NaN tracing process-wide. Wire this to a CLI flag.
pub fn set_nan_trace(enabled: bool) {
    NAN_TRACE_ENABLED.store(enabled, Ordering::Relaxed);
}

/// Whether NaN tracing is currently switched on. Hot-path guard is a single
/// relaxed atomic load.
#[must_use]
#[inline]
pub fn nan_trace_enabled() -> bool {
    NAN_TRACE_ENABLED.load(Ordering::Relaxed)
}

/// Heuristic: does this solver/diagnostic error message suggest a non-finite
/// (`NaN`/`inf`) value was involved? Used to decide whether to automatically
/// re-run a failed simulation with NaN tracing enabled.
#[must_use]
pub fn error_suggests_nonfinite(message: &str) -> bool {
    const NEEDLES: &[&str] = &[
        "NaN",
        "non-finite",
        "nonfinite",
        "Step size is too small",
        "step size too small",
        "did not converge",
        "SymbolicSingular",
        "Failed to factorise",
        "infinite",
    ];
    NEEDLES.iter().any(|needle| message.contains(needle))
}

/// Report any non-finite entries in `values`, naming each via `name_of`.
///
/// Returns `true` if at least one non-finite value was found. When tracing is
/// disabled this returns `false` after only the cached-flag check, so it is safe
/// — and effectively free — to call unconditionally on hot paths. The
/// `name_of` closure is only invoked for entries that are actually non-finite.
#[inline]
pub fn report_nonfinite<F: Fn(usize) -> String>(
    context: &str,
    t: f64,
    values: &[f64],
    name_of: F,
) -> bool {
    if !nan_trace_enabled() {
        return false;
    }
    report_nonfinite_cold(context, t, values, &name_of)
}

/// Report non-finite state-derivative inputs (the projected `solver_y`) and
/// outputs (`der(state)`), each named by solver variable. Effectively free when
/// tracing is off.
pub fn report_state_derivative(
    model: &solve::SolveModel,
    t: f64,
    solver_y: &[f64],
    derivatives: &[f64],
) {
    if !nan_trace_enabled() {
        return;
    }
    let names = &model.problem.solve_layout.solver_maps.names;
    report_nonfinite(
        "state-derivative inputs (solver_y after projection)",
        t,
        solver_y,
        |index| {
            names
                .get(index)
                .cloned()
                .unwrap_or_else(|| format!("y[{index}]"))
        },
    );
    report_nonfinite("state-derivative output", t, derivatives, |index| {
        names.get(index).map_or_else(
            || format!("der(state[{index}])"),
            |name| format!("der({name})"),
        )
    });
}

/// Out-of-line slow path so the enabled-only work stays out of the caller's hot
/// path (keeps the inlined fast path to just the flag check + branch).
#[cold]
fn report_nonfinite_cold(
    context: &str,
    t: f64,
    values: &[f64],
    name_of: &dyn Fn(usize) -> String,
) -> bool {
    let mut found = false;
    for (index, value) in values.iter().enumerate() {
        if !value.is_finite() {
            found = true;
            let kind = if value.is_nan() { "NaN" } else { "inf" };
            eprintln!(
                "[nan-trace] {context} @ t={t}: `{name}` (slot {index}) = {kind}",
                name = name_of(index),
            );
        }
    }
    found
}
