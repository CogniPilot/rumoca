use super::*;

impl<'primal, 'program> DirectionalBuilder<'primal, 'program> {
    pub(super) fn derive_linear_solve(
        &mut self,
        destination: SolveRegisterId,
        matrix: SolveRegisterId,
        rhs: SolveRegisterId,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let matrix_type = self.primal.register_types()[matrix.index()].clone();
        let rhs_type = self.primal.register_types()[rhs.index()].clone();
        let matrix = self.get(matrix, provenance)?;
        let rhs = self.get(rhs, provenance)?;
        let primal = self
            .builder
            .linear_solve(matrix.primal, rhs.primal, provenance)?;
        let matrix_tangent = self.tangent_or_zero(matrix, &matrix_type, provenance)?;
        let rhs_tangent = self.tangent_or_zero(rhs, &rhs_type, provenance)?;
        let product = self
            .builder
            .matrix_multiply(matrix_tangent, primal, provenance)?;
        let tangent_rhs = self.builder.binary(
            SolveBinaryOperator::Subtract,
            rhs_tangent,
            product,
            provenance,
        )?;
        let tangent = self
            .builder
            .linear_solve(matrix.primal, tangent_rhs, provenance)?;
        self.bind(
            destination,
            Directional {
                primal,
                tangent: Some(tangent),
            },
            provenance,
        )
    }
}
