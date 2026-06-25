//! State-Jacobian inspector for the `rumoca sim --inspect jacobian` debug dump.
//!
//! Assembles the dense state-derivative Jacobian `J = ∂(der(state))/∂(state)`
//! at a chosen `(state, t)` by the exact forward-mode AD JVP (one unit-seed
//! column at a time), naming every row and column
//! by its qualified model variable name, and proactively flags
//! structurally-singular columns (no derivative depends on that state) and zero
//! pivots (`∂der(x)/∂x = 0`). This complements the reactive `--inspect eval` NaN
//! trace with a proactive view of the linearization the solver sees.

use crate::{
    AlgebraicLinearization, AlgebraicSettle, SolveRuntime,
    inspect_alloc::{filled_f64_values, vec_with_capacity},
    iterative_solve::{GmresConfig, gmres},
    linear_solve::{AugmentedMatrix, gaussian_eliminate},
};
use rumoca_ir_solve::ScalarSlot;

/// Write `column[row]` into `matrix[row][col]` for every row.
fn write_column(matrix: &mut [Vec<f64>], col: usize, column: &[f64]) {
    for (row, value) in column.iter().copied().enumerate() {
        matrix[row][col] = value;
    }
}

/// Fill column `col` of `matrix` with a single `value` (used for failed columns).
fn write_column_value(matrix: &mut [Vec<f64>], col: usize, value: f64) {
    for row in matrix.iter_mut() {
        row[col] = value;
    }
}

/// Magnitude below which a Jacobian entry is treated as a structural zero. A
/// state a derivative truly does not depend on yields an exact `0` from the AD
/// JVP, so this only needs to absorb round-off.
const STRUCTURAL_ZERO: f64 = 1.0e-12;

/// Dense state-Jacobian evaluated at a point, with named rows/columns.
#[derive(Debug, Clone)]
pub struct JacobianReport {
    /// Time the Jacobian was evaluated at.
    pub t: f64,
    /// State labels (the qualified state name) for both rows (`der(name)`) and
    /// columns (`name`), in solver state order.
    pub state_labels: Vec<String>,
    /// Dense `n x n` matrix: `matrix[row][col] = ∂(der(state_row))/∂(state_col)`.
    pub matrix: Vec<Vec<f64>>,
    /// Runtime error, if a column evaluation failed; the affected column is
    /// filled with `NaN` and reporting continues.
    pub error: Option<String>,
}

impl JacobianReport {
    /// Dimension `n` of the (square) state Jacobian.
    #[must_use]
    pub fn dim(&self) -> usize {
        self.state_labels.len()
    }

    /// Column indices that are structurally singular: no state derivative
    /// depends on that state (an all-zero column), so the column is unconstrained
    /// in the linearization.
    #[must_use]
    pub fn singular_columns(&self) -> Vec<usize> {
        if !self.has_square_matrix() {
            return Vec::new();
        }
        // `matrix` is a dense square `dim x dim`, so every `row[col]` is in range.
        (0..self.dim())
            .filter(|&col| {
                self.matrix
                    .iter()
                    .all(|row| row[col].abs() <= STRUCTURAL_ZERO)
            })
            .collect()
    }

    /// Diagonal indices whose pivot `∂der(x)/∂x` is (near) zero.
    #[must_use]
    pub fn zero_pivots(&self) -> Vec<usize> {
        if !self.has_square_matrix() {
            return Vec::new();
        }
        (0..self.dim())
            .filter(|&i| self.matrix[i][i].abs() <= STRUCTURAL_ZERO)
            .collect()
    }

    /// Non-zero entries as `(row, col, value)`, in row-major order.
    pub fn nonzero_entries(&self) -> impl Iterator<Item = (usize, usize, f64)> + '_ {
        self.matrix.iter().enumerate().flat_map(|(row, columns)| {
            columns
                .iter()
                .copied()
                .enumerate()
                .filter(|(_, value)| value.abs() > STRUCTURAL_ZERO)
                .map(move |(col, value)| (row, col, value))
        })
    }

    fn has_square_matrix(&self) -> bool {
        self.matrix.len() == self.dim() && self.matrix.iter().all(|row| row.len() == self.dim())
    }
}

impl SolveRuntime {
    /// Assemble the dense state Jacobian `∂(der(state))/∂(state)` at `(state, t)`
    /// by the exact forward-mode AD JVP (one unit-seed column at a time), naming
    /// each row and column by its qualified model variable name. Backs `rumoca
    /// sim --inspect jacobian`. State entries beyond the model's state count are
    /// ignored.
    pub fn eval_state_jacobian(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        settle: AlgebraicSettle,
    ) -> JacobianReport {
        let n = self.state_count;
        let state_labels = (0..n).map(|index| self.state_label(index)).collect();

        let (mut matrix, mut seed, mut column) = match jacobian_work_buffers(n) {
            Ok(buffers) => buffers,
            Err(error) => {
                return JacobianReport {
                    t,
                    state_labels,
                    matrix: Vec::new(),
                    error: Some(error),
                };
            }
        };
        let mut error = None;
        for col in 0..n {
            seed[col] = 1.0;
            let result = self.eval_state_jacobian_v_ad_into(
                AlgebraicLinearization { t, params, settle },
                state,
                &seed,
                &mut column,
            );
            seed[col] = 0.0;
            match result {
                Ok(()) => write_column(&mut matrix, col, &column),
                Err(err) => {
                    error.get_or_insert_with(|| err.to_string());
                    write_column_value(&mut matrix, col, f64::NAN);
                }
            }
        }

        JacobianReport {
            t,
            state_labels,
            matrix,
            error,
        }
    }

    // (column writers are free functions below)

    /// Qualified-name label for state slot `index`, looked up from the solver
    /// names. The fully-qualified name uniquely identifies the state; raw source
    /// byte spans are omitted as they aren't human-actionable in this dump.
    fn state_label(&self, index: usize) -> String {
        let names = &self.model.problem.solve_layout.solver_maps.names;
        match names.get(index) {
            Some(name) => name.clone(),
            None => format!("y[{index}]"),
        }
    }

    /// Assemble the dense parameter sensitivity `∂(der(state))/∂p` at `(state, t)`
    /// by the exact forward-mode AD JVP, seeding one parameter unit vector at a
    /// time. Rows are `der(state)`; columns are the model parameters. The seed
    /// spans `[solver-y | parameter]`, so parameter `p` is seeded at offset
    /// `y_scalars + p`; the path through algebraics is included because the
    /// projection JVP is lowered with parameter seeds too. Backs roadmap Track
    /// 0.3 (`--inspect jacobian` parameter block).
    pub fn eval_parameter_jacobian(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        settle: AlgebraicSettle,
    ) -> ParameterJacobianReport {
        let layout = &self.model.problem.layout;
        let n_state = self.state_count;
        let y_scalars = layout.y_scalars();
        let row_labels = (0..n_state).map(|index| self.state_label(index)).collect();
        // One column per model parameter (rumoca-internal `__`-prefixed slots are
        // excluded); each carries its P-slot so callers can map a column back to
        // the parameter vector.
        let model_params = self.model_parameter_slots();
        let (param_slots, param_labels): (Vec<usize>, Vec<String>) =
            model_params.into_iter().unzip();
        let n_param = param_slots.len();

        // The seed must reach the largest P-slot index, not just `n_param`, since
        // model and internal parameter slots may be interleaved.
        let seed_len = y_scalars.saturating_add(layout.p_scalars());
        let buffers = rect_jacobian_buffers(n_state, n_param, seed_len);
        let (mut matrix, mut seed, mut column) = match buffers {
            Ok(buffers) => buffers,
            Err(error) => {
                return ParameterJacobianReport {
                    t,
                    row_labels,
                    param_labels,
                    param_slots,
                    matrix: Vec::new(),
                    error: Some(error),
                };
            }
        };
        let mut error = None;
        for (col, slot) in param_slots.iter().copied().enumerate() {
            seed[y_scalars + slot] = 1.0;
            let result = self.eval_full_jacobian_v_ad_into(
                AlgebraicLinearization { t, params, settle },
                state,
                &seed,
                &mut column,
            );
            seed[y_scalars + slot] = 0.0;
            match result {
                Ok(()) => write_column(&mut matrix, col, &column),
                Err(err) => {
                    error.get_or_insert_with(|| err.to_string());
                    write_column_value(&mut matrix, col, f64::NAN);
                }
            }
        }

        ParameterJacobianReport {
            t,
            row_labels,
            param_labels,
            param_slots,
            matrix,
            error,
        }
    }

    /// Model-parameter P-slots as `(slot, name)` in slot order, excluding
    /// rumoca-internal `__`-prefixed slots (event/pre memory etc.). The first
    /// name bound to a slot wins.
    fn model_parameter_slots(&self) -> Vec<(usize, String)> {
        let n = self.model.problem.layout.p_scalars();
        let mut by_slot: Vec<Option<String>> = vec![None; n];
        for (name, slot) in self.model.problem.layout.bindings() {
            if let ScalarSlot::P { index, .. } = slot
                && *index < n
                && !name.starts_with("__")
                && by_slot[*index].is_none()
            {
                by_slot[*index] = Some(name.clone());
            }
        }
        by_slot
            .into_iter()
            .enumerate()
            .filter_map(|(slot, name)| name.map(|name| (slot, name)))
            .collect()
    }

    /// Steady-state forward parameter sensitivity `∂y/∂p` at a settled point,
    /// by the implicit-function theorem: at `f(y, p) = 0`,
    /// `∂y/∂p = -(∂f/∂y)⁻¹ · ∂f/∂p`. Both Jacobians come from the exact
    /// forward-mode AD, and each parameter column is one dense solve of the
    /// state Jacobian. This is the natural derivative for design objectives that
    /// are functions of the *converged* state (roadmap §6); it assumes the
    /// linearization point is at (or near) steady state.
    ///
    /// Dense — appropriate for small models / small parameter counts; the
    /// state-Jacobian is re-eliminated per column. Grid-scale models want a
    /// sparse factorization (factor once) or the steady adjoint (reverse mode,
    /// Track C), which this does not attempt.
    pub fn steady_state_parameter_sensitivity(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        settle: AlgebraicSettle,
    ) -> SteadyStateSensitivityReport {
        let state_jac = self.eval_state_jacobian(t, state, params, settle);
        let param_jac = self.eval_parameter_jacobian(t, state, params, settle);
        let state_labels = state_jac.state_labels.clone();
        let param_labels = param_jac.param_labels.clone();
        let param_slots = param_jac.param_slots.clone();

        // Surface any Jacobian-evaluation failure before attempting the solve.
        if let Some(error) = state_jac.error.clone().or_else(|| param_jac.error.clone()) {
            return SteadyStateSensitivityReport {
                t,
                state_labels,
                param_labels,
                param_slots,
                sensitivity: Vec::new(),
                error: Some(error),
            };
        }

        let n = state_labels.len();
        let n_param = param_labels.len();
        match solve_steady_state_sensitivity(&state_jac.matrix, &param_jac.matrix, n, n_param) {
            Ok(sensitivity) => SteadyStateSensitivityReport {
                t,
                state_labels,
                param_labels,
                param_slots,
                sensitivity,
                error: None,
            },
            Err(error) => SteadyStateSensitivityReport {
                t,
                state_labels,
                param_labels,
                param_slots,
                sensitivity: Vec::new(),
                error: Some(error),
            },
        }
    }

    /// Steady-state gradient `d(objective)/dp`, where `objective` is any solver
    /// variable (a state or an output/algebraic). Computes the state sensitivity
    /// `∂state/∂p` (implicit-function theorem), projects each column to the full
    /// solver-y sensitivity (so algebraic outputs are covered), and selects the
    /// objective's row. Parameter-named. Track 0.2.
    pub fn steady_state_objective_gradient(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        objective: &str,
        settle: AlgebraicSettle,
    ) -> ObjectiveGradientReport {
        let steady = self.steady_state_parameter_sensitivity(t, state, params, settle);
        let param_labels = steady.param_labels.clone();
        let param_slots = steady.param_slots.clone();
        let make_error = |error: String| ObjectiveGradientReport {
            t,
            objective: objective.to_string(),
            param_labels: param_labels.clone(),
            param_slots: param_slots.clone(),
            gradient: Vec::new(),
            error: Some(error),
        };
        if let Some(error) = steady.error {
            return make_error(error);
        }
        let Some(objective_index) = self.solver_variable_index(objective) else {
            return make_error(format!(
                "objective `{objective}` is not a solver-y variable (a state or a solver \
                 algebraic). The steady-state gradient currently projects onto the solver vector, \
                 so the objective must be one of its variables. Alias-eliminated result-only \
                 outputs (reconstructed after the solve) are not yet supported as objectives; \
                 reference the underlying state or solver algebraic instead"
            ));
        };

        let n_state = steady.state_labels.len();
        let mut gradient = vec![0.0; param_slots.len()];
        let mut solver_column = vec![0.0; self.solver_count];
        let mut state_column = vec![0.0; n_state];
        for (col, &slot) in param_slots.iter().enumerate() {
            for (row, value) in state_column.iter_mut().enumerate() {
                *value = steady.sensitivity[row][col];
            }
            if let Err(error) = self.project_state_sensitivity_to_solver_y(
                AlgebraicLinearization { t, params, settle },
                state,
                &state_column,
                slot,
                &mut solver_column,
            ) {
                return make_error(error.to_string());
            }
            // `objective_index` is a position in `solver_maps.names` (length
            // `solver_count`), and `solver_column` is sized `solver_count`, so the
            // index is in bounds by construction.
            gradient[col] = solver_column[objective_index];
        }

        ObjectiveGradientReport {
            t,
            objective: objective.to_string(),
            param_labels,
            param_slots,
            gradient,
            error: None,
        }
    }

    /// Steady-state objective gradient via the **reverse-mode adjoint** — the same
    /// `d(objective)/dp` as [`Self::steady_state_objective_gradient`], computed
    /// from the transposed linearization:
    ///
    /// ```text
    ///   (∂f/∂y)ᵀ λ = (∂J/∂y)ᵀ           (adjoint solve)
    ///   dJ/dp = ∂J/∂p − (∂f/∂p)ᵀ λ
    /// ```
    ///
    /// The transposed operators `(∂f/∂y)ᵀ` and `(∂f/∂p)ᵀ` are supplied by the
    /// scalar reverse VJP ([`SolveRuntime::reverse_state_derivative_vjp`]): the
    /// adjoint system is solved **matrix-free** with GMRES using `v ↦ (∂f/∂y)ᵀ v`,
    /// so the dense Jacobian is never formed; one more application with the solved
    /// `λ` yields `(∂f/∂p)ᵀ λ`.
    ///
    /// Cost is **independent of the parameter count**: the adjoint solve is a
    /// handful of VJP matvecs (≤ `n_state`, and far fewer when the transposed
    /// linearization is sparse — e.g. a PDE stencil) plus one VJP for the
    /// parameter gradient, versus the forward path's one dense solve *per
    /// parameter*. So the adjoint wins once parameters outnumber states
    /// (`n_param ≳ n_state`) and scales to grid-sized state vectors that the dense
    /// forward sensitivity cannot.
    ///
    /// Scope: any **solver-y** objective (state OR algebraic), via the full DAE
    /// steady residual `R = [der; g]`. The objective must be a solver-y variable;
    /// alias-eliminated result-only outputs are not (reference the underlying
    /// state/algebraic). The result must equal the forward gradient
    /// ([`Self::steady_state_objective_gradient`]) at the same steady point.
    pub fn steady_state_adjoint_objective_gradient(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        objective: &str,
        settle: AlgebraicSettle,
    ) -> ObjectiveGradientReport {
        let slots = self.model_parameter_slots();
        let param_labels: Vec<String> = slots.iter().map(|(_, name)| name.clone()).collect();
        let param_slots: Vec<usize> = slots.iter().map(|(slot, _)| *slot).collect();
        let make_error = |error: String| ObjectiveGradientReport {
            t,
            objective: objective.to_string(),
            param_labels: param_labels.clone(),
            param_slots: param_slots.clone(),
            gradient: Vec::new(),
            error: Some(error),
        };

        let Some(objective_index) = self.solver_variable_index(objective) else {
            return make_error(format!(
                "objective `{objective}` is not a solver-y variable (a state or solver algebraic); \
                 alias-eliminated result-only outputs are not supported — reference the underlying \
                 state or solver algebraic"
            ));
        };

        let p_scalars = self.model.problem.layout.p_scalars();
        // Settle the full solver-y point (states + algebraics consistent, g = 0):
        // the adjoint operator and right-hand side linearize there.
        let solver_y = match self.full_solver_y(t, state, params, settle.tol, settle.max_iters) {
            Ok(solver_y) => solver_y,
            Err(error) => return make_error(error.to_string()),
        };

        // Adjoint right-hand side ∂J/∂solver_y = e_objective (solver-y objective).
        let mut rhs = vec![0.0; self.solver_count];
        rhs[objective_index] = 1.0;

        // Solve (∂R/∂solver_y)ᵀ λ = rhs matrix-free: `apply_steady_residual_transpose`
        // supplies `v ↦ (∂R/∂solver_y)ᵀ v` (full DAE residual, der + algebraic
        // constraints) from the two reverse VJPs, so no Jacobian is formed.
        let mut transpose = vec![0.0; self.solver_count + p_scalars];
        let apply = |v: &[f64], out: &mut [f64]| -> Result<(), rumoca_solver::RuntimeSolveError> {
            self.apply_steady_residual_transpose(t, &solver_y, params, v, &mut transpose)?;
            out.copy_from_slice(&transpose[..self.solver_count]);
            Ok(())
        };
        // GMRES wraps an operator failure as `GmresError::Operator`, so its
        // `Display` surfaces the real message.
        let lambda = match gmres(apply, &rhs, GmresConfig::default()) {
            Ok(lambda) => lambda,
            Err(error) => return make_error(error.to_string()),
        };

        // dJ/dp = −(∂R/∂p)ᵀ λ  (∂J/∂p = 0 for a solver-y objective).
        if let Err(error) =
            self.apply_steady_residual_transpose(t, &solver_y, params, &lambda, &mut transpose)
        {
            return make_error(error.to_string());
        }
        let gradient: Vec<f64> = param_slots
            .iter()
            .map(|&slot| -transpose[self.solver_count + slot])
            .collect();

        ObjectiveGradientReport {
            t,
            objective: objective.to_string(),
            param_labels,
            param_slots,
            gradient,
            error: None,
        }
    }
}

/// Steady-state gradient of a scalar objective w.r.t. the model parameters.
#[derive(Debug, Clone)]
pub struct ObjectiveGradientReport {
    /// Linearization time.
    pub t: f64,
    /// Objective variable name.
    pub objective: String,
    /// Parameter names (P-slot order).
    pub param_labels: Vec<String>,
    /// Parameter-vector P-slot index for each entry (`params[param_slots[i]]`).
    pub param_slots: Vec<usize>,
    /// `gradient[i] = d(objective)/d(param_i)` at steady state.
    pub gradient: Vec<f64>,
    /// Set when the sensitivity failed or the objective is not a solver variable.
    pub error: Option<String>,
}

/// Solve `J_y · X = -J_p` for the steady-state sensitivity `X = ∂y/∂p`, one
/// parameter column at a time (Gauss-Jordan with partial pivoting).
fn solve_steady_state_sensitivity(
    state_jacobian: &[Vec<f64>],
    parameter_jacobian: &[Vec<f64>],
    n: usize,
    n_param: usize,
) -> Result<Vec<Vec<f64>>, String> {
    let mut sensitivity = vec![vec![0.0; n_param]; n];
    for col in 0..n_param {
        let mut matrix = AugmentedMatrix::zeroed(n).map_err(|error| error.to_string())?;
        for row in 0..n {
            for (column, &value) in state_jacobian[row].iter().enumerate() {
                matrix.set(row, column, value);
            }
            matrix.set(row, n, -parameter_jacobian[row][col]);
        }
        if gaussian_eliminate(&mut matrix).is_none() {
            return Err(
                "steady-state Jacobian ∂(der)/∂(state) is singular; the forward parameter \
                 sensitivity is undefined at this point"
                    .to_string(),
            );
        }
        for (row, sensitivity_row) in sensitivity.iter_mut().enumerate() {
            sensitivity_row[col] = matrix.get(row, n);
        }
    }
    Ok(sensitivity)
}

/// Dense `∂y/∂p` at a settled point, with named rows (states) and columns
/// (model parameters). `sensitivity[state][param] = ∂y_steady/∂p`.
#[derive(Debug, Clone)]
pub struct SteadyStateSensitivityReport {
    /// Linearization time.
    pub t: f64,
    /// Row labels (state names).
    pub state_labels: Vec<String>,
    /// Column labels (model-parameter names, in P-slot order).
    pub param_labels: Vec<String>,
    /// Parameter-vector P-slot index for each column (`params[param_slots[col]]`).
    pub param_slots: Vec<usize>,
    /// `sensitivity[row][col] = ∂(state_row)/∂(param_col)` at steady state.
    pub sensitivity: Vec<Vec<f64>>,
    /// Set when a Jacobian eval failed or the state Jacobian was singular.
    pub error: Option<String>,
}

impl SteadyStateSensitivityReport {
    /// Number of states (rows).
    #[must_use]
    pub fn rows(&self) -> usize {
        self.state_labels.len()
    }

    /// Number of model parameters (columns).
    #[must_use]
    pub fn cols(&self) -> usize {
        self.param_labels.len()
    }
}

/// Dense `∂(der)/∂p` evaluated at a point, with named rows (`der(state)`) and
/// columns (parameters). Rectangular: `n_state x n_param`.
#[derive(Debug, Clone)]
pub struct ParameterJacobianReport {
    /// Time the sensitivity was evaluated at.
    pub t: f64,
    /// Row labels (the qualified state name; row `i` is `der(state_i)`).
    pub row_labels: Vec<String>,
    /// Column labels (model-parameter names, in P-slot order).
    pub param_labels: Vec<String>,
    /// Parameter-vector P-slot index for each column, so a column maps back to
    /// the `params` slice (`params[param_slots[col]]`).
    pub param_slots: Vec<usize>,
    /// `matrix[row][col] = ∂(der(state_row))/∂(param_col)`.
    pub matrix: Vec<Vec<f64>>,
    /// Runtime error, if a column evaluation failed; that column is `NaN`.
    pub error: Option<String>,
}

impl ParameterJacobianReport {
    /// Number of rows (state derivatives).
    #[must_use]
    pub fn rows(&self) -> usize {
        self.row_labels.len()
    }

    /// Number of columns (parameters).
    #[must_use]
    pub fn cols(&self) -> usize {
        self.param_labels.len()
    }

    /// Non-zero entries as `(row, col, value)`, in row-major order.
    pub fn nonzero_entries(&self) -> impl Iterator<Item = (usize, usize, f64)> + '_ {
        self.matrix.iter().enumerate().flat_map(|(row, columns)| {
            columns
                .iter()
                .copied()
                .enumerate()
                .filter(|(_, value)| value.abs() > STRUCTURAL_ZERO)
                .map(move |(col, value)| (row, col, value))
        })
    }
}

type RectJacobianBuffers = (Vec<Vec<f64>>, Vec<f64>, Vec<f64>);

fn rect_jacobian_buffers(
    rows: usize,
    cols: usize,
    seed_len: usize,
) -> Result<RectJacobianBuffers, String> {
    let mut matrix = vec_with_capacity(rows, "parameter jacobian matrix row count")?;
    for _ in 0..rows {
        matrix.push(filled_f64_values(
            cols,
            0.0,
            "parameter jacobian matrix row values",
        )?);
    }
    // Seed spans `[solver-y | parameter]`; one parameter slot is set in turn.
    let seed = filled_f64_values(seed_len, 0.0, "parameter jacobian seed values")?;
    let column = filled_f64_values(rows, 0.0, "parameter jacobian column values")?;
    Ok((matrix, seed, column))
}

type JacobianWorkBuffers = (Vec<Vec<f64>>, Vec<f64>, Vec<f64>);

fn jacobian_work_buffers(n: usize) -> Result<JacobianWorkBuffers, String> {
    Ok((
        zero_jacobian_matrix(n)?,
        filled_f64_values(n, 0.0, "jacobian seed values")?,
        filled_f64_values(n, 0.0, "jacobian column values")?,
    ))
}

fn zero_jacobian_matrix(n: usize) -> Result<Vec<Vec<f64>>, String> {
    let mut matrix = vec_with_capacity(n, "jacobian matrix row count")?;
    for _ in 0..n {
        matrix.push(filled_f64_values(n, 0.0, "jacobian matrix row values")?);
    }
    Ok(matrix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jacobian_matrix_rejects_impossible_dimension() {
        let err = zero_jacobian_matrix(usize::MAX)
            .expect_err("impossible jacobian matrix row count should fail");

        assert!(err.contains("jacobian matrix row count capacity overflows"));
    }

    #[test]
    fn jacobian_report_handles_missing_matrix_defensively() {
        let report = JacobianReport {
            t: 0.0,
            state_labels: vec!["x".to_string(), "y".to_string()],
            matrix: Vec::new(),
            error: Some("jacobian matrix row count capacity overflows".to_string()),
        };

        assert_eq!(report.dim(), 2);
        assert!(report.singular_columns().is_empty());
        assert!(report.zero_pivots().is_empty());
        assert_eq!(report.nonzero_entries().count(), 0);
    }
}
