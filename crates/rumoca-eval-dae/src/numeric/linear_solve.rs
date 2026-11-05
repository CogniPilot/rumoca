use rumoca_core::linear_solve::{AugmentedMatrix, gaussian_eliminate};

use super::{
    NumericEvaluationError, NumericEvaluationErrorKind as Kind, NumericEvaluator, Scalar, failure,
};
use rumoca_core::Span;
use rumoca_ir_dae as dae;

impl<'dae, F> NumericEvaluator<'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    pub(super) fn linear_solve(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        matrix_values: Vec<Scalar>,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let rhs = arguments.get(1).ok_or_else(|| {
            failure(
                Kind::ShapeMismatch,
                "linear solve requires a right-hand side",
                span,
            )
        })?;
        let rhs = self.source_expression(rhs)?;
        let n = rhs.len();
        if n.checked_mul(n) != Some(matrix_values.len()) {
            return Err(failure(
                Kind::ShapeMismatch,
                "linear solve requires a square matrix matching the right-hand side",
                span,
            ));
        }
        if matrix_values
            .iter()
            .chain(&rhs)
            .any(|value| !value.is_finite())
        {
            return Err(failure(
                Kind::InvalidValue,
                "linear solve requires finite coefficients and right-hand side",
                span,
            ));
        }
        let mut matrix = AugmentedMatrix::zeroed(n)
            .map_err(|error| failure(Kind::Overflow, error.to_string(), span))?;
        for (row, value) in rhs.iter().enumerate() {
            for column in 0..n {
                matrix.set(row, column, matrix_values[row * n + column].real());
            }
            matrix.set(row, n, value.real());
        }
        gaussian_eliminate(&mut matrix).ok_or_else(|| {
            failure(
                Kind::InvalidValue,
                "linear solve has a singular matrix or non-finite solution",
                span,
            )
        })?;
        Ok((0..n)
            .map(|index| Scalar::Real(matrix.solution_component(index)))
            .collect())
    }
}
