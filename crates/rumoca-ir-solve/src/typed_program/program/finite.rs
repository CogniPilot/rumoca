//! Explicit opt-in finite-domain checks; ordinary Real operations stay IEEE.
#[cfg(test)]
mod tests;

use super::*;

impl SolveFiniteCheck {
    /// The identity reported for a failure, given the value of the mapped
    /// point when the check is mapped. A `MappedPrimal` check reports
    /// `Primal { program: program_base + point, operation }`, where `point`
    /// is the row-major ordinal of the failing map point. The enclosing map
    /// proves `point` lies in `0..point_count`, so the reported identity is
    /// exactly the one the scalarized programs would report.
    #[must_use]
    pub fn reported(self, point: Option<u64>) -> Option<Self> {
        match (self, point) {
            (
                Self::MappedPrimal {
                    program_base,
                    point_count,
                    operation,
                },
                Some(point),
            ) if point < u64::from(point_count) => {
                let program = u32::try_from(u64::from(program_base) + point).ok()?;
                Some(Self::Primal { program, operation })
            }
            (Self::MappedPrimal { .. }, _) | (_, Some(_)) => None,
            (check, None) => Some(check),
        }
    }
}

impl<'program> TypedProgramBuilder<'program> {
    /// Guard this exact whole Real value, preserving canonical element order.
    /// No predicate output, detached validation flag or scalar expansion exists.
    pub fn check_finite(
        &mut self,
        operand: ProgramRegister<'program>,
        check: SolveFiniteCheck,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        if matches!(check, SolveFiniteCheck::MappedPrimal { .. }) {
            // A mapped identity needs its point register; see `check_finite_mapped`.
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        self.push_finite_check(operand, check, None, provenance)
    }

    /// Guard a primal value issued once in a compact map body for
    /// `point_count` scalar programs starting at `program_base`. `point` is
    /// the body's integer row-major map ordinal; only a map body may issue
    /// one, and the enclosing map refuses unless it proves `point` lies in
    /// `0..point_count`.
    pub fn check_finite_mapped(
        &mut self,
        operand: ProgramRegister<'program>,
        program_base: u32,
        point_count: u32,
        operation: u32,
        point: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let point_type = self.value_type_of(point, provenance)?;
        let fits = point_count > 0 && program_base.checked_add(point_count - 1).is_some();
        if !self.map_body
            || !fits
            || !point_type.dimensions().is_empty()
            || !matches!(point_type.element_type(), SolveScalarType::Integer(_))
        {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        let check = SolveFiniteCheck::MappedPrimal {
            program_base,
            point_count,
            operation,
        };
        self.push_finite_check(operand, check, Some(point.id), provenance)
    }

    fn push_finite_check(
        &mut self,
        operand: ProgramRegister<'program>,
        check: SolveFiniteCheck,
        point: Option<SolveRegisterId>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !matches!(
            self.value_type_of(operand, provenance)?.element_type(),
            SolveScalarType::Real { .. }
        ) {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        // Preserve input/publication checks and mapped admission obligations.
        // Only a validated ordinary primal check can use this builder's facts;
        // replay keeps every recorded operation and child regions start fresh.
        if self.numbered_values
            && matches!(check, SolveFiniteCheck::Primal { .. })
            && self.arithmetic_facts.is_finite(operand.id)
        {
            return Ok(());
        }
        self.push(
            SolveOperation::CheckFinite {
                operand: operand.id,
                check,
                point,
            },
            provenance,
        );
        Ok(())
    }
}
