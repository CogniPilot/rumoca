use nalgebra::{DMatrix, DVector, Dyn, linalg::LU};
use rumoca_ir_solve as solve;

use super::solve_ops::RuntimeSolveError;

#[derive(Debug, Clone)]
pub struct PreparedMassMatrix {
    state_count: usize,
    kind: MassMatrixKind,
}

#[derive(Debug, Clone)]
enum MassMatrixKind {
    Identity,
    Diagonal(Vec<f64>),
    Sparse {
        entries: Vec<solve::MassMatrixEntry>,
        factorization: LU<f64, Dyn, Dyn>,
    },
}

impl PreparedMassMatrix {
    pub fn new(mass: &solve::MassMatrix, state_count: usize) -> Result<Self, RuntimeSolveError> {
        let kind = match mass {
            solve::MassMatrix::Identity => MassMatrixKind::Identity,
            solve::MassMatrix::Diagonal { values } => {
                validate_diagonal(values, state_count)?;
                MassMatrixKind::Diagonal(values.clone())
            }
            solve::MassMatrix::Sparse { entries } => {
                let factorization = factor_sparse_matrix(entries, state_count)?;
                MassMatrixKind::Sparse {
                    entries: entries.clone(),
                    factorization,
                }
            }
        };
        Ok(Self { state_count, kind })
    }

    pub fn solve(&self, rhs: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
        if rhs.len() < self.state_count {
            return Err(invalid_mass_matrix(format!(
                "state mass-matrix solve requires {} RHS entries, got {}",
                self.state_count,
                rhs.len()
            )));
        }
        let rhs = &rhs[..self.state_count];
        match &self.kind {
            MassMatrixKind::Identity => Ok(rhs.to_vec()),
            MassMatrixKind::Diagonal(diagonal) => diagonal
                .iter()
                .zip(rhs.iter().copied())
                .map(|(coefficient, value)| solve_diagonal_entry(*coefficient, value))
                .collect(),
            MassMatrixKind::Sparse { factorization, .. } => {
                let solution = factorization
                    .solve(&DVector::from_column_slice(rhs))
                    .ok_or_else(|| invalid_mass_matrix("singular state mass matrix"))?;
                if solution.iter().all(|value| value.is_finite()) {
                    Ok(solution.as_slice().to_vec())
                } else {
                    Err(invalid_mass_matrix(
                        "state mass-matrix solve produced a non-finite value",
                    ))
                }
            }
        }
    }

    pub fn apply_solver_mass_with_beta(
        &self,
        v: &[f64],
        beta: f64,
        out: &mut [f64],
        solver_count: usize,
    ) {
        let n = self.state_count.min(v.len()).min(out.len());
        match &self.kind {
            MassMatrixKind::Identity => {
                for (slot, value) in out.iter_mut().take(n).zip(v.iter().copied()) {
                    *slot = value + beta * *slot;
                }
            }
            MassMatrixKind::Diagonal(diagonal) => {
                for ((slot, coeff), value) in out
                    .iter_mut()
                    .take(n)
                    .zip(diagonal.iter().copied())
                    .zip(v.iter().copied())
                {
                    *slot = coeff * value + beta * *slot;
                }
            }
            MassMatrixKind::Sparse { entries, .. } => {
                for slot in out.iter_mut().take(n) {
                    *slot *= beta;
                }
                for entry in entries
                    .iter()
                    .filter(|entry| entry.row < n && entry.column < n)
                {
                    out[entry.row] += entry.value * v[entry.column];
                }
            }
        }
        for slot in out.iter_mut().take(solver_count).skip(n) {
            *slot *= beta;
        }
    }
}

pub fn solve_mass_matrix(
    mass: &solve::MassMatrix,
    rhs: &[f64],
) -> Result<Vec<f64>, RuntimeSolveError> {
    PreparedMassMatrix::new(mass, rhs.len())?.solve(rhs)
}

fn solve_diagonal_entry(coefficient: f64, rhs: f64) -> Result<f64, RuntimeSolveError> {
    if coefficient == 0.0 {
        return Err(invalid_mass_matrix("singular state mass matrix"));
    }
    let quotient = rhs / coefficient;
    if !quotient.is_finite() {
        return Err(invalid_mass_matrix(
            "state mass-matrix solve produced a non-finite value",
        ));
    }
    Ok(quotient)
}

fn validate_diagonal(values: &[f64], state_count: usize) -> Result<(), RuntimeSolveError> {
    if values.len() != state_count {
        return Err(invalid_mass_matrix(format!(
            "diagonal state mass matrix has {} entries for {state_count} states",
            values.len()
        )));
    }
    if values.iter().any(|value| !value.is_finite()) {
        return Err(invalid_mass_matrix(
            "diagonal state mass matrix contains a non-finite coefficient",
        ));
    }
    Ok(())
}

fn factor_sparse_matrix(
    entries: &[solve::MassMatrixEntry],
    state_count: usize,
) -> Result<LU<f64, Dyn, Dyn>, RuntimeSolveError> {
    let mut matrix = DMatrix::<f64>::zeros(state_count, state_count);
    for entry in entries {
        if entry.row >= state_count || entry.column >= state_count {
            return Err(invalid_mass_matrix(format!(
                "state mass-matrix entry ({}, {}) is outside {state_count}x{state_count}",
                entry.row, entry.column
            )));
        }
        if !entry.value.is_finite() {
            return Err(invalid_mass_matrix(
                "sparse state mass matrix contains a non-finite coefficient",
            ));
        }
        let coefficient = &mut matrix[(entry.row, entry.column)];
        *coefficient += entry.value;
        if !coefficient.is_finite() {
            return Err(invalid_mass_matrix(
                "duplicate sparse state mass-matrix entries accumulate to a non-finite coefficient",
            ));
        }
    }
    let factorization = matrix.lu();
    if !factorization.is_invertible() {
        return Err(invalid_mass_matrix("singular state mass matrix"));
    }
    Ok(factorization)
}

fn invalid_mass_matrix(reason: impl Into<String>) -> RuntimeSolveError {
    RuntimeSolveError::UnsupportedModel {
        reason: reason.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagonal_mass_matrix_solves_and_applies_without_dense_expansion() {
        let mass = solve::MassMatrix::Diagonal {
            values: vec![2.0, 4.0],
        };
        let prepared = PreparedMassMatrix::new(&mass, 2).expect("diagonal should prepare");

        assert_eq!(
            prepared.solve(&[6.0, 8.0]).expect("diagonal should solve"),
            [3.0, 2.0]
        );

        let mut out = [10.0, 20.0, 30.0];
        prepared.apply_solver_mass_with_beta(&[1.0, 2.0, 3.0], 0.5, &mut out, 3);
        assert_eq!(out, [7.0, 18.0, 15.0]);
    }

    #[test]
    fn diagonal_mass_matrix_accepts_scaled_nonsingular_coefficients() {
        let mass = solve::MassMatrix::Diagonal {
            values: vec![1.0e-20],
        };
        let prepared = PreparedMassMatrix::new(&mass, 1).expect("scaled matrix should prepare");

        assert_eq!(
            prepared
                .solve(&[2.0e-20])
                .expect("scaled matrix should solve"),
            [2.0]
        );
        assert!(
            prepared.solve(&[f64::MAX]).is_err(),
            "a non-finite quotient must fail rather than escape the mass solve"
        );
    }

    #[test]
    fn diagonal_mass_matrix_rejects_exact_zero() {
        let mass = solve::MassMatrix::Diagonal { values: vec![0.0] };
        let prepared = PreparedMassMatrix::new(&mass, 1).expect("shape should prepare");
        assert!(prepared.solve(&[1.0]).is_err());
    }

    #[test]
    fn sparse_mass_matrix_reuses_lu_for_solve_and_entries_for_apply() {
        let mass = solve::MassMatrix::Sparse {
            entries: vec![
                solve::MassMatrixEntry {
                    row: 0,
                    column: 0,
                    value: 4.0,
                },
                solve::MassMatrixEntry {
                    row: 0,
                    column: 1,
                    value: 1.0,
                },
                solve::MassMatrixEntry {
                    row: 1,
                    column: 0,
                    value: 2.0,
                },
                solve::MassMatrixEntry {
                    row: 1,
                    column: 1,
                    value: 3.0,
                },
            ],
        };
        let prepared = PreparedMassMatrix::new(&mass, 2).expect("matrix should factor");

        let solution = prepared.solve(&[9.0, 8.0]).expect("matrix should solve");
        assert!((solution[0] - 1.9).abs() < 1.0e-12);
        assert!((solution[1] - 1.4).abs() < 1.0e-12);

        let mut out = [10.0, 20.0, 30.0];
        prepared.apply_solver_mass_with_beta(&[1.0, 2.0, 3.0], 0.5, &mut out, 3);
        assert_eq!(out, [11.0, 18.0, 15.0]);
    }

    #[test]
    fn mass_matrix_rejects_invalid_compact_payloads() {
        let wrong_diagonal = solve::MassMatrix::Diagonal { values: vec![1.0] };
        assert!(PreparedMassMatrix::new(&wrong_diagonal, 2).is_err());

        let out_of_bounds = solve::MassMatrix::Sparse {
            entries: vec![solve::MassMatrixEntry {
                row: 2,
                column: 0,
                value: 1.0,
            }],
        };
        assert!(PreparedMassMatrix::new(&out_of_bounds, 2).is_err());

        let overflowing_duplicates = solve::MassMatrix::Sparse {
            entries: vec![
                solve::MassMatrixEntry {
                    row: 0,
                    column: 0,
                    value: f64::MAX,
                },
                solve::MassMatrixEntry {
                    row: 0,
                    column: 0,
                    value: f64::MAX,
                },
            ],
        };
        assert!(PreparedMassMatrix::new(&overflowing_duplicates, 1).is_err());
    }
}
