//! Canonical scalar tangents of an already-issued exact primal occurrence.

#[cfg(test)]
mod tests;

mod aggregate;
pub use aggregate::{SolveAggregateBinaryDirectionalSource, SolveAggregateUnaryDirectionalSource};

use super::*;

/// The exact scalar unary occurrence emitted by its constructor. No caller
/// can substitute an unrelated same-typed primal for its operator/operand.
///
/// ```compile_fail,E0451
/// use rumoca_ir_solve::{ProgramRegister, SolveUnaryDirectionalSource, SolveUnaryOperator};
/// fn forge<'p>(value: ProgramRegister<'p>, span: rumoca_core::Span) {
///     let _ = SolveUnaryDirectionalSource {
///         operator: SolveUnaryOperator::Sin, operand: value, primal: value, provenance: span,
///     };
/// }
/// ```
#[derive(Clone, Copy, Debug)]
pub struct SolveUnaryDirectionalSource<'program> {
    operator: SolveUnaryOperator,
    operand: ProgramRegister<'program>,
    primal: ProgramRegister<'program>,
    provenance: Span,
}

/// The exact scalar binary occurrence emitted by its constructor. Directions
/// remain distinct from structural absence, including present signed zeros.
#[derive(Clone, Copy, Debug)]
pub struct SolveBinaryDirectionalSource<'program> {
    operator: SolveBinaryOperator,
    operands: [ProgramRegister<'program>; 2],
    primal: ProgramRegister<'program>,
    provenance: Span,
}

impl<'program> SolveUnaryDirectionalSource<'program> {
    pub fn primal(self) -> ProgramRegister<'program> {
        self.primal
    }
}

impl<'program> SolveBinaryDirectionalSource<'program> {
    pub fn primal(self) -> ProgramRegister<'program> {
        self.primal
    }
}

impl<'program> TypedProgramBuilder<'program> {
    /// Emit one scalar primal and bind the original operation to its result.
    /// Aggregate lifting is a separate compact constructor, never expansion.
    pub fn scalar_unary_directional_source(
        &mut self,
        operator: SolveUnaryOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<SolveUnaryDirectionalSource<'program>, SolveProgramConstructionError> {
        require_scalar(self, operand, provenance)?;
        let primal = self.unary(operator, operand, provenance)?;
        Ok(SolveUnaryDirectionalSource {
            operator,
            operand,
            primal,
            provenance,
        })
    }

    /// Emit one scalar primal with its exact ordered operand pair.
    pub fn scalar_binary_directional_source(
        &mut self,
        operator: SolveBinaryOperator,
        operands: [ProgramRegister<'program>; 2],
        provenance: Span,
    ) -> Result<SolveBinaryDirectionalSource<'program>, SolveProgramConstructionError> {
        for operand in operands {
            require_scalar(self, operand, provenance)?;
        }
        let primal = self.binary(operator, operands[0], operands[1], provenance)?;
        Ok(SolveBinaryDirectionalSource {
            operator,
            operands,
            primal,
            provenance,
        })
    }

    /// Reuse the sole canonical directional arithmetic relation, retaining
    /// the source occurrence and without re-emitting its original primal.
    pub fn scalar_unary_directional_tangent(
        &mut self,
        source: SolveUnaryDirectionalSource<'program>,
        tangent: Option<ProgramRegister<'program>>,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let operand = checked_direction(self, source.operand, tangent, source.provenance)?;
        let ty = self
            .value_type_of(source.primal, source.provenance)?
            .clone();
        DirectionalArithmetic { builder: self }
            .derive_unary_from_primal(
                source.operator,
                operand,
                source.primal,
                &ty,
                source.provenance,
            )
            .map(|value| value.tangent)
    }

    /// Both directions belong to the issued ordered operand pair. This API
    /// accepts neither a replacement primal nor an alternate operator.
    pub fn scalar_binary_directional_tangent(
        &mut self,
        source: SolveBinaryDirectionalSource<'program>,
        tangents: [Option<ProgramRegister<'program>>; 2],
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let lhs = checked_direction(self, source.operands[0], tangents[0], source.provenance)?;
        let rhs = checked_direction(self, source.operands[1], tangents[1], source.provenance)?;
        let ty = self
            .value_type_of(source.primal, source.provenance)?
            .clone();
        DirectionalArithmetic { builder: self }
            .derive_binary_from_primal(
                source.operator,
                lhs,
                rhs,
                source.primal,
                &ty,
                source.provenance,
            )
            .map(|value| value.tangent)
    }
}

fn require_scalar<'program>(
    builder: &TypedProgramBuilder<'program>,
    register: ProgramRegister<'program>,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    if !builder
        .value_type_of(register, provenance)?
        .dimensions()
        .is_empty()
    {
        return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
    }
    Ok(())
}

fn checked_direction<'program>(
    builder: &TypedProgramBuilder<'program>,
    primal: ProgramRegister<'program>,
    tangent: Option<ProgramRegister<'program>>,
    provenance: Span,
) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let ty = builder.value_type_of(primal, provenance)?;
    if let Some(tangent) = tangent
        && (!is_real(ty) || builder.value_type_of(tangent, provenance)? != ty)
    {
        return Err(SolveProgramConstructionError::TypeMismatch { provenance });
    }
    Ok(Directional { primal, tangent })
}
