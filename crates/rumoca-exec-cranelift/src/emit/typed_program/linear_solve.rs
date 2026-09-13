//! Final native adapter for the compact checked linear-solve operation.

use super::*;

#[cfg(test)]
mod tests;

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_linear_solve(
        &mut self,
        destination: solve::SolveRegisterId,
        matrix: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let matrix = self.register(matrix)?.clone();
        let rhs = self.register(rhs)?.clone();
        let mut matrix = self.linear_solve_elements(&matrix)?;
        let mut rhs = self.linear_solve_elements(&rhs)?;
        let n = rhs.len();
        let solution =
            super::super::emit_dense_linear_solve(self.builder, &mut matrix, &mut rhs, n)?;
        self.require_finite_linear_solve_values(&solution);
        for (index, value) in solution.into_iter().enumerate() {
            let index = self.builder.ins().iconst(types::I64, index as i64);
            self.store_scalar(&destination, index, value)?;
        }
        Ok(())
    }

    fn linear_solve_elements(&mut self, value: &ValueLocation) -> Result<Vec<Value>, CompileError> {
        let elements = (0..value.value_type.scalar_count())
            .map(|index| {
                let index = self.builder.ins().iconst(types::I64, i64::from(index));
                self.load_scalar(value, index)
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.require_finite_linear_solve_values(&elements);
        Ok(elements)
    }

    fn require_finite_linear_solve_values(&mut self, elements: &[Value]) {
        let maximum = self.builder.ins().f64const(f64::MAX);
        let mut valid = self.builder.ins().iconst(types::I8, 1);
        for &value in elements {
            let absolute = self.builder.ins().fabs(value);
            let finite = self
                .builder
                .ins()
                .fcmp(FloatCC::LessThanOrEqual, absolute, maximum);
            valid = self.builder.ins().band(valid, finite);
        }
        status::require_linear_solve(self.builder, valid);
    }
}
