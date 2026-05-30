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

/// Report non-finite Jacobian entries with full directional context: for each
/// non-finite output row `i`, names the differentiated row variable and each
/// seeded column variable (`∂(row)/∂(col) = NaN`). `row_name`/`col_name` should
/// include source spans so the offending pair is traceable to the model.
///
/// Seed vectors containing non-finite entries are skipped: those are the
/// solver's NaN-propagation sparsity probes, where a non-finite output is
/// expected and not a defect. Effectively free when tracing is disabled.
#[inline]
pub fn report_nonfinite_jacobian<R, C>(
    context: &str,
    t: f64,
    seed: &[f64],
    out: &[f64],
    row_name: R,
    col_name: C,
) -> bool
where
    R: Fn(usize) -> String,
    C: Fn(usize) -> String,
{
    if !nan_trace_enabled() {
        return false;
    }
    report_nonfinite_jacobian_cold(context, t, seed, out, &row_name, &col_name)
}

#[cold]
fn report_nonfinite_jacobian_cold(
    context: &str,
    t: f64,
    seed: &[f64],
    out: &[f64],
    row_name: &dyn Fn(usize) -> String,
    col_name: &dyn Fn(usize) -> String,
) -> bool {
    if out.iter().all(|value| value.is_finite()) {
        return false;
    }
    // A non-finite seed is the solver's NaN-propagation sparsity probe: a
    // non-finite output there is expected (it reveals the dependency structure
    // rather than a defect), so tag it distinctly.
    let probe = seed.iter().any(|value| !value.is_finite());
    let tag = if probe { " [sparsity probe]" } else { "" };
    // Seeded columns are the perturbation directions (non-zero, or the probed
    // NaN column) — i.e. the variables being differentiated against.
    let seeded_columns = seed
        .iter()
        .enumerate()
        .filter(|(_, value)| **value != 0.0 || !value.is_finite())
        .map(|(index, _)| col_name(index))
        .collect::<Vec<_>>()
        .join(", ");
    let columns = if seeded_columns.is_empty() {
        "<no seeded column>".to_string()
    } else {
        seeded_columns
    };
    let mut found = false;
    for (index, value) in out.iter().enumerate() {
        if !value.is_finite() {
            found = true;
            let kind = if value.is_nan() { "NaN" } else { "inf" };
            eprintln!(
                "[nan-trace] {context}{tag} @ t={t}: \u{2202}({row}) / \u{2202}({columns}) = {kind}",
                row = row_name(index),
            );
        }
    }
    found
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

/// Report non-finite state-Jacobian entries with directional context, naming the
/// differentiated state derivative and the seeded column(s) with their source
/// spans. Effectively free when tracing is off.
pub fn report_state_jacobian(model: &solve::SolveModel, t: f64, seed: &[f64], jacobian_v: &[f64]) {
    if !nan_trace_enabled() {
        return;
    }
    report_nonfinite_jacobian(
        "state-derivative Jacobian (finite difference)",
        t,
        seed,
        jacobian_v,
        |row| format!("der({})", solver_var_label(model, row)),
        |col| solver_var_label(model, col),
    );
}

/// Human-readable label for a solver slot: variable name plus source span
/// (looked up from `variable_meta`) so traces are traceable back to source.
fn solver_var_label(model: &solve::SolveModel, index: usize) -> String {
    let names = &model.problem.solve_layout.solver_maps.names;
    let Some(name) = names.get(index) else {
        return format!("y[{index}]");
    };
    match model.variable_meta.iter().find(|meta| &meta.name == name) {
        Some(meta) => format!("{name} @ {:?}", meta.source_span),
        None => name.clone(),
    }
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
