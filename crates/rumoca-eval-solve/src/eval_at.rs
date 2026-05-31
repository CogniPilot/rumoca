//! One-shot model evaluation probe used by `rumoca sim --inspect eval`.
//!
//! Evaluates a [`SolveRuntime`] at a chosen `(state, t)` and reports every
//! solver value and state derivative by name, so a non-finite (`NaN`/`inf`)
//! can be attributed to a specific model variable in a single command instead
//! of repeated instrumented rebuilds.

use crate::SolveRuntime;

/// One named value in an [`EvalAtReport`].
#[derive(Debug, Clone)]
pub struct EvalAtSlot {
    /// Solver-variable name (or `der(<state>)` for a derivative row).
    pub name: String,
    /// Evaluated value at the probed point.
    pub value: f64,
}

impl EvalAtSlot {
    /// Whether the value is finite (not `NaN`/`inf`).
    #[must_use]
    pub fn is_finite(&self) -> bool {
        self.value.is_finite()
    }

    /// `"NaN"`/`"inf"` tag for a non-finite value, else `None`.
    #[must_use]
    pub fn nonfinite_kind(&self) -> Option<&'static str> {
        if self.value.is_finite() {
            None
        } else if self.value.is_nan() {
            Some("NaN")
        } else {
            Some("inf")
        }
    }
}

/// Snapshot of a model evaluated at a chosen `(state, t)`.
#[derive(Debug, Clone)]
pub struct EvalAtReport {
    /// Time the model was evaluated at.
    pub t: f64,
    /// Number of leading [`solver_y`](Self::solver_y) entries that are states
    /// (the rest are algebraics/outputs).
    pub state_count: usize,
    /// Full solver vector after algebraic/output refresh: states first, then
    /// algebraics/outputs, each named by its solver variable.
    pub solver_y: Vec<EvalAtSlot>,
    /// State derivatives `der(state)` produced at the point.
    pub derivatives: Vec<EvalAtSlot>,
    /// Runtime error, if `solver_y` refresh or derivative evaluation failed. The
    /// partially-computed values are still reported so the offending variable
    /// can be named.
    pub error: Option<String>,
}

impl EvalAtReport {
    /// All non-finite slots across `solver_y` and `derivatives`, tagged with the
    /// section they came from (`"solver_y"` or `"derivative"`).
    pub fn nonfinite(&self) -> impl Iterator<Item = (&'static str, &EvalAtSlot)> {
        self.solver_y
            .iter()
            .map(|slot| ("solver_y", slot))
            .chain(self.derivatives.iter().map(|slot| ("derivative", slot)))
            .filter(|(_, slot)| !slot.is_finite())
    }

    /// Whether any solver value or derivative is non-finite.
    #[must_use]
    pub fn has_nonfinite(&self) -> bool {
        self.nonfinite().next().is_some()
    }
}

impl SolveRuntime {
    /// Evaluate the model at `(state, t)` and report every solver value and
    /// state derivative by name, flagging non-finite entries. Backs the
    /// `rumoca sim --inspect eval` debugging probe.
    ///
    /// `state` shorter than the model's state count is padded with the model's
    /// initial state; extra entries are ignored. Evaluation failures are
    /// captured in [`EvalAtReport::error`] with the partially-computed values
    /// still reported, so the variable that produced a `NaN`/`inf` is named.
    pub fn eval_at(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> EvalAtReport {
        let names = &self.model.problem.solve_layout.solver_maps.names;
        let mut error = None;

        // `full_solver_y_into` seeds from the model's initial solver vector and
        // overlays the provided state prefix, then refreshes algebraics/outputs.
        // On a non-finite refresh it returns Err but leaves the partially
        // refreshed values in `solver_y`, which is exactly what we want to name.
        let mut solver_y_values = Vec::new();
        if let Err(err) =
            self.full_solver_y_into(t, state, params, tol, max_iters, &mut solver_y_values)
        {
            error = Some(err.to_string());
        }
        let solver_y = solver_y_values
            .iter()
            .enumerate()
            .map(|(index, &value)| EvalAtSlot {
                name: names
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| format!("y[{index}]")),
                value,
            })
            .collect();

        // The derivative evaluation fills `out` and *then* rejects non-finite
        // values (see `eval_state_derivatives_with_solver_y`), so a failure
        // still leaves the offending `der(state)` values in place to name.
        let mut derivative_values = vec![f64::NAN; self.state_count];
        if let Err(err) = self.eval_state_derivatives_into(
            t,
            state,
            params,
            tol,
            max_iters,
            &mut derivative_values,
        ) && error.is_none()
        {
            error = Some(err.to_string());
        }
        let derivatives = derivative_values
            .iter()
            .enumerate()
            .map(|(index, &value)| EvalAtSlot {
                name: names.get(index).map_or_else(
                    || format!("der(state[{index}])"),
                    |name| format!("der({name})"),
                ),
                value,
            })
            .collect();

        EvalAtReport {
            t,
            state_count: self.state_count,
            solver_y,
            derivatives,
            error,
        }
    }
}
