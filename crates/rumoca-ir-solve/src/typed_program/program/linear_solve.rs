//! Checked signature for an aggregate linear solve.

use super::*;
use crate::SolveRealFormat;

impl<'program> TypedProgramBuilder<'program> {
    pub fn linear_solve(
        &mut self,
        matrix: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let matrix_type = self.register_type(matrix, provenance)?;
        let rhs_type = self.register_type(rhs, provenance)?;
        let [rows, columns] = matrix_type.dimensions() else {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        };
        if rows != columns
            || rhs_type.dimensions() != [*rows]
            || matrix_type.element_type() != rhs_type.element_type()
            || matrix_type.element_type()
                != (SolveScalarType::Real {
                    format: SolveRealFormat::Binary64,
                })
        {
            return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
        }
        let destination = self.issue_register(rhs_type.clone(), provenance)?;
        self.push(
            SolveOperation::LinearSolve {
                destination: destination.id,
                matrix: matrix.id,
                rhs: rhs.id,
            },
            provenance,
        );
        Ok(destination)
    }
}
