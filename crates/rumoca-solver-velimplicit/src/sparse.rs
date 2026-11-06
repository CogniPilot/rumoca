//! Sparse LU of the iteration matrix `I - h J` on the structural pattern.
//!
//! The structure is the state-Jacobian pattern plus the diagonal, fixed for the
//! life of one `initialize`, so `rumoca_solver`'s pattern-keyed LU computes the
//! symbolic factorization once and refactors numerically only when `I - h J`
//! changes (a Jacobian refresh or a new step).

use rumoca_solver::sparse_lu::SparseLuSystem;

pub(crate) struct SparseIterationLu {
    system: SparseLuSystem,
    /// Jacobian entry of each stored value of `system`; zero on a structural
    /// diagonal that is not in the pattern.
    jacobian: Box<[f64]>,
    /// Storage index of each pattern entry, per state column, in the pattern's
    /// row order.
    slots: Box<[Box<[usize]>]>,
    factored: bool,
}

impl SparseIterationLu {
    /// Prepare `I - h J` for `pattern` (structural rows of each state column).
    /// `None` when the structure is rejected; the caller keeps the dense LU.
    pub(crate) fn new(width: usize, pattern: &[Vec<usize>]) -> Option<Self> {
        let entries = pattern
            .iter()
            .enumerate()
            .flat_map(|(column, rows)| rows.iter().map(move |&row| (row, column)));
        let diagonal = (0..width).map(|index| (index, index));
        let system = SparseLuSystem::new(width, entries.chain(diagonal))?;
        let slots = pattern
            .iter()
            .enumerate()
            .map(|(column, rows)| {
                rows.iter()
                    .map(|&row| system.position(row, column))
                    .collect::<Option<Box<[usize]>>>()
            })
            .collect::<Option<Box<[_]>>>()?;
        Some(Self {
            jacobian: vec![0.0; system.coordinates().len()].into_boxed_slice(),
            system,
            slots,
            factored: false,
        })
    }

    /// Clear the Jacobian before a refresh.
    pub(crate) fn clear(&mut self) {
        self.jacobian.fill(0.0);
        self.factored = false;
    }

    /// Write the structural rows of one Jacobian column from a directional
    /// derivative, as `scatter_column` does for the dense matrix.
    pub(crate) fn scatter_column(&mut self, state_column: usize, derivative: &[f64]) {
        let coordinates = self.system.coordinates();
        for &slot in &self.slots[state_column] {
            self.jacobian[slot] = derivative[coordinates[slot].0];
        }
        self.factored = false;
    }

    /// Factor `I - h J`. Returns `false` when the numeric factorization fails.
    pub(crate) fn factor(&mut self, step: f64) -> bool {
        let jacobian = &self.jacobian;
        self.system.fill(|index, row, column| {
            let identity = if row == column { 1.0 } else { 0.0 };
            identity - step * jacobian[index]
        });
        self.factored = self.system.refactor_if_changed();
        self.factored
    }

    /// Solve `(I - h J) x = rhs` with the last successful factors; every entry
    /// of `out` is NaN when there are none.
    pub(crate) fn solve_into(&mut self, rhs: &[f64], out: &mut [f64]) {
        out.copy_from_slice(rhs);
        if !(self.factored && self.system.solve_in_place(out)) {
            out.fill(f64::NAN);
        }
    }

    /// Stored entries of `I - h J`: the pattern plus the diagonal.
    #[cfg(test)]
    pub(crate) fn stored_entries(&self) -> usize {
        self.system.coordinates().len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linear::DenseLu;

    /// A banded pattern with two columns lacking their diagonal.
    fn pattern() -> Vec<Vec<usize>> {
        vec![vec![0, 1], vec![0, 2], vec![1, 2, 3], vec![2]]
    }

    fn jacobian_column(column: usize) -> Vec<f64> {
        let mut out = vec![0.0; 4];
        for &row in &pattern()[column] {
            let diagonal = if row == column { 3.0 } else { 0.0 };
            out[row] = 1.0 + f64::from(u8::try_from(row * 4 + column).unwrap()) * 0.37 - diagonal;
        }
        out
    }

    #[test]
    fn matches_the_dense_factorization_on_a_structural_pattern() {
        let width = 4;
        let step = 0.25;
        let mut sparse = SparseIterationLu::new(width, &pattern()).expect("structure is accepted");
        assert_eq!(sparse.stored_entries(), 10);
        sparse.clear();
        let mut dense = vec![0.0; width * width];
        for column in 0..width {
            let derivative = jacobian_column(column);
            sparse.scatter_column(column, &derivative);
            for &row in &pattern()[column] {
                dense[row * width + column] = -step * derivative[row];
            }
        }
        for diagonal in 0..width {
            dense[diagonal * width + diagonal] += 1.0;
        }
        assert!(sparse.factor(step));
        let mut lu = DenseLu::default();
        assert!(lu.factor(&dense, width));
        let rhs = [1.0, -2.0, 0.5, 3.0];
        let (mut got, mut want) = ([0.0; 4], [0.0; 4]);
        sparse.solve_into(&rhs, &mut got);
        lu.solve_into(&rhs, &mut want);
        for (got, want) in got.iter().zip(want) {
            assert!(
                (got - want).abs() <= 1.0e-13 * want.abs().max(1.0),
                "{got} vs {want}"
            );
        }
    }

    #[test]
    fn an_unfactored_solve_is_not_finite() {
        let mut sparse = SparseIterationLu::new(2, &[vec![0], vec![1]]).expect("structure");
        let mut out = [0.0; 2];
        sparse.solve_into(&[1.0, 1.0], &mut out);
        assert!(out.iter().all(|value| value.is_nan()));
    }

    #[test]
    fn a_singular_iteration_matrix_is_not_solved() {
        // J = I / h makes I - h J the zero matrix. faer reports a singular
        // factorization as an error, or a zero pivot surfaces as a non-finite
        // solve, which the step rejects as a non-finite state.
        let mut sparse = SparseIterationLu::new(2, &[vec![0], vec![1]]).expect("structure");
        sparse.clear();
        sparse.scatter_column(0, &[4.0, 0.0]);
        sparse.scatter_column(1, &[0.0, 4.0]);
        let factored = sparse.factor(0.25);
        let mut out = [0.0; 2];
        sparse.solve_into(&[1.0, 1.0], &mut out);
        assert!(!factored || out.iter().any(|value| !value.is_finite()));
    }
}
