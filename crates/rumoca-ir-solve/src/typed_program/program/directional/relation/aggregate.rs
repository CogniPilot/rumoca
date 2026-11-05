//! Compact aggregate source relations use the same canonical lifting owner.

mod columns;
#[cfg(test)]
mod tests;

use super::super::pointwise::{tangent_of_binary_source, tangent_of_unary_source};
pub use columns::SolveAggregateDirectionalColumns;

use super::*;

#[derive(Clone, Copy, Debug)]
pub struct SolveAggregateUnaryDirectionalSource<'program> {
    operator: SolveUnaryOperator,
    operand: ProgramRegister<'program>,
    primal: ProgramRegister<'program>,
    provenance: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct SolveAggregateBinaryDirectionalSource<'program> {
    operator: SolveBinaryOperator,
    operands: [ProgramRegister<'program>; 2],
    primal: ProgramRegister<'program>,
    provenance: Span,
}

impl<'program> SolveAggregateUnaryDirectionalSource<'program> {
    pub fn primal(self) -> ProgramRegister<'program> {
        self.primal
    }
}

impl<'program> SolveAggregateBinaryDirectionalSource<'program> {
    pub fn primal(self) -> ProgramRegister<'program> {
        self.primal
    }
}

impl<'program> TypedProgramBuilder<'program> {
    /// Bind the original aggregate operation without enumerating coordinates.
    pub fn aggregate_unary_directional_source(
        &mut self,
        operator: SolveUnaryOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<SolveAggregateUnaryDirectionalSource<'program>, SolveProgramConstructionError> {
        require_aggregate(self, operand, provenance)?;
        let primal = self.unary(operator, operand, provenance)?;
        Ok(SolveAggregateUnaryDirectionalSource {
            operator,
            operand,
            primal,
            provenance,
        })
    }

    /// Retain one exact same-shape elementwise binary source occurrence.
    pub fn aggregate_binary_directional_source(
        &mut self,
        operator: SolveBinaryOperator,
        operands: [ProgramRegister<'program>; 2],
        provenance: Span,
    ) -> Result<SolveAggregateBinaryDirectionalSource<'program>, SolveProgramConstructionError>
    {
        for operand in operands {
            require_aggregate(self, operand, provenance)?;
        }
        let primal = self.binary(operator, operands[0], operands[1], provenance)?;
        Ok(SolveAggregateBinaryDirectionalSource {
            operator,
            operands,
            primal,
            provenance,
        })
    }

    /// Use the canonical compact typed directional relation. Its absent
    /// tangent role remains distinct from a present aggregate of zeros.
    pub fn aggregate_unary_directional_tangent(
        &mut self,
        source: SolveAggregateUnaryDirectionalSource<'program>,
        tangent: Option<ProgramRegister<'program>>,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let operand = checked_direction(self, source.operand, tangent, source.provenance)?;
        let ty = self
            .value_type_of(source.primal, source.provenance)?
            .clone();
        tangent_of_unary_source(
            self,
            source.operator,
            operand,
            source.primal,
            &ty,
            source.provenance,
        )
    }

    /// Lift directions through one checked body, retaining the original
    /// primal in each coordinate. No per-element operation graph is created.
    pub fn aggregate_binary_directional_tangent(
        &mut self,
        source: SolveAggregateBinaryDirectionalSource<'program>,
        tangents: [Option<ProgramRegister<'program>>; 2],
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let lhs = checked_direction(self, source.operands[0], tangents[0], source.provenance)?;
        let rhs = checked_direction(self, source.operands[1], tangents[1], source.provenance)?;
        let ty = self
            .value_type_of(source.primal, source.provenance)?
            .clone();
        tangent_of_binary_source(
            self,
            source.operator,
            [lhs, rhs],
            source.primal,
            &ty,
            source.provenance,
        )
    }
}

fn require_aggregate<'program>(
    builder: &TypedProgramBuilder<'program>,
    register: ProgramRegister<'program>,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    if builder
        .value_type_of(register, provenance)?
        .dimensions()
        .is_empty()
    {
        return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
    }
    Ok(())
}
