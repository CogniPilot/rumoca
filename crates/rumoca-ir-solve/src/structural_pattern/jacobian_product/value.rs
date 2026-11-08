//! Sealed directions reuse the canonical IR-local directional arithmetic.
use super::StateJacobianProductConstruction;
use crate::{
    ProgramRegister, SolveBinaryOperator, SolveFiniteCheck, SolveProgramConstructionError,
    SolveUnaryOperator,
};
use rumoca_core::Span;

/// A caller cannot erase directions or substitute a same-shaped vector.
/// ```compile_fail
/// use rumoca_ir_solve::StateJacobianProductValue;
/// fn erase<'p>(value: StateJacobianProductValue<'p>) -> StateJacobianProductValue<'p> {
///     StateJacobianProductValue { tangents: Vec::new(), ..value }
/// }
/// ```
#[derive(Clone, Debug)]
pub struct StateJacobianProductValue<'program> {
    pub(super) primal: ProgramRegister<'program>,
    pub(super) tangents: Vec<Option<ProgramRegister<'program>>>,
}

impl<'program> StateJacobianProductConstruction<'program, '_> {
    pub fn unary(
        &mut self,
        operator: SolveUnaryOperator,
        value: &StateJacobianProductValue<'program>,
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<StateJacobianProductValue<'program>, SolveProgramConstructionError> {
        let source = self
            .builder
            .scalar_unary_directional_source(operator, value.primal, span)?;
        self.builder.check_finite(source.primal(), check, span)?;
        let tangents = value
            .tangents
            .iter()
            .map(|direction| {
                self.builder
                    .scalar_unary_structural_tangent(source, *direction)
            })
            .collect::<Result<_, _>>()?;
        Ok(StateJacobianProductValue {
            primal: source.primal(),
            tangents,
        })
    }

    pub fn binary(
        &mut self,
        operator: SolveBinaryOperator,
        values: [&StateJacobianProductValue<'program>; 2],
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<StateJacobianProductValue<'program>, SolveProgramConstructionError> {
        let source = self.builder.scalar_binary_directional_source(
            operator,
            [values[0].primal, values[1].primal],
            span,
        )?;
        self.builder.check_finite(source.primal(), check, span)?;
        let tangents = values[0]
            .tangents
            .iter()
            .zip(&values[1].tangents)
            .map(|(left, right)| {
                self.builder
                    .scalar_binary_structural_tangent(source, [*left, *right])
            })
            .collect::<Result<_, _>>()?;
        Ok(StateJacobianProductValue {
            primal: source.primal(),
            tangents,
        })
    }
}
