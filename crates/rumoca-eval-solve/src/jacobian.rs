//! State-Jacobian inspector for the `rumoca sim --inspect jacobian` debug dump.
//!
//! Assembles the dense state-derivative Jacobian `J = ∂(der(state))/∂(state)`
//! at a chosen `(state, t)` by the exact forward-mode AD JVP (one unit-seed
//! column at a time), naming every row and column
//! by its qualified model variable name, and proactively flags
//! structurally-singular columns (no derivative depends on that state) and zero
//! pivots (`∂der(x)/∂x = 0`). This complements the reactive `--inspect eval` NaN
//! trace with a proactive view of the linearization the solver sees.

use crate::{AlgebraicSettle, SolveRuntime};

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

        let mut matrix = vec![vec![0.0; n]; n];
        let mut error = None;
        let mut seed = vec![0.0; n];
        let mut column = vec![0.0; n];
        for col in 0..n {
            seed[col] = 1.0;
            let result =
                self.eval_state_jacobian_v_ad_into(t, state, params, &seed, settle, &mut column);
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
}
