//! Whole-tensor adapter to the shared numerical linear-solve kernel.

use super::*;

impl EvalFrame<'_, '_> {
    pub(super) fn eval_linear_solve(
        &mut self,
        destination: SolveRegisterId,
        matrix: SolveRegisterId,
        rhs: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let matrix = self.read(matrix, provenance)?;
        let rhs = self.read(rhs, provenance)?;
        let rhs_start = u32::try_from(matrix.elements.len())
            .map_err(|_| invalid_error("linear solve matrix size", provenance))?;
        let inputs = matrix
            .elements
            .iter()
            .chain(rhs.elements.iter())
            .map(|&element| match element {
                SolveValueKind::Real64(bits) => Ok(f64::from_bits(bits)),
                _ => Err(invalid_error("linear solve Real64 input", provenance)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut solution = vec![0.0; rhs.elements.len()];
        crate::linear_solve::solve_all_unchecked(
            &inputs,
            0,
            rhs_start,
            solution.len(),
            crate::tensor_policy::LinearSolveKernel::Dense,
            None,
            &mut solution,
        )
        .map_err(|error| TypedProgramEvalError::LinearSolve {
            reason: error.to_string(),
            provenance,
        })?;
        if solution.iter().any(|value| !value.is_finite()) {
            return Err(TypedProgramEvalError::LinearSolve {
                reason: "non-finite solution".to_owned(),
                provenance,
            });
        }
        let value = TypedValue::checked(
            self.destination_type(destination, provenance)?.clone(),
            solution
                .into_iter()
                .map(|value| SolveValueKind::Real64(value.to_bits()))
                .collect(),
            provenance,
        )?;
        self.write(destination, value, provenance)
    }
}
