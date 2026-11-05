//! Explicit opt-in finite-domain checks; ordinary Real operations stay IEEE.
#[cfg(test)]
mod tests;

use super::*;

impl<'program> TypedProgramBuilder<'program> {
    /// Guard this exact whole Real value, preserving canonical element order.
    /// No predicate output, detached validation flag or scalar expansion exists.
    pub fn check_finite(
        &mut self,
        operand: ProgramRegister<'program>,
        check: SolveFiniteCheck,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !matches!(
            self.value_type_of(operand, provenance)?.element_type(),
            SolveScalarType::Real { .. }
        ) {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        self.push(
            SolveOperation::CheckFinite {
                operand: operand.id,
                check,
            },
            provenance,
        );
        Ok(())
    }
}
