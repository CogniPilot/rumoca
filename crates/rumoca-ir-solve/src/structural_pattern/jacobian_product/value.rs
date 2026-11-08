//! Sealed directions reuse the canonical IR-local directional arithmetic.
use super::StateJacobianProductConstruction;
use crate::{
    ProgramRegister, SolveBinaryOperator, SolveFiniteCheck, SolveProgramConstructionError,
    SolveUnaryOperator, TypedProgramBuilder,
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
        unary(self.builder, operator, value, (span, Guard::Plain(check)))
    }

    pub fn binary(
        &mut self,
        operator: SolveBinaryOperator,
        values: [&StateJacobianProductValue<'program>; 2],
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<StateJacobianProductValue<'program>, SolveProgramConstructionError> {
        binary(self.builder, operator, values, (span, Guard::Plain(check)))
    }
}

#[derive(Clone, Copy)]
pub(super) enum Guard<'p> {
    Plain(SolveFiniteCheck),
    Mapped {
        base: u32,
        count: u32,
        operation: u32,
        point: ProgramRegister<'p>,
    },
}

impl<'p> Guard<'p> {
    pub(super) fn check(
        self,
        builder: &mut TypedProgramBuilder<'p>,
        value: ProgramRegister<'p>,
        span: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        match self {
            Self::Plain(check) => builder.check_finite(value, check, span),
            Self::Mapped {
                base,
                count,
                operation,
                point,
            } => builder.check_finite_mapped(value, base, count, operation, point, span),
        }
    }
}

pub(super) fn unary<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    operator: SolveUnaryOperator,
    value: &StateJacobianProductValue<'p>,
    (span, guard): (Span, Guard<'p>),
) -> Result<StateJacobianProductValue<'p>, SolveProgramConstructionError> {
    let source = builder.scalar_unary_directional_source(operator, value.primal, span)?;
    guard.check(builder, source.primal(), span)?;
    let tangents = value
        .tangents
        .iter()
        .map(|direction| builder.scalar_unary_structural_tangent(source, *direction))
        .collect::<Result<_, _>>()?;
    Ok(StateJacobianProductValue {
        primal: source.primal(),
        tangents,
    })
}

pub(super) fn binary<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    operator: SolveBinaryOperator,
    values: [&StateJacobianProductValue<'p>; 2],
    (span, guard): (Span, Guard<'p>),
) -> Result<StateJacobianProductValue<'p>, SolveProgramConstructionError> {
    let source = builder.scalar_binary_directional_source(
        operator,
        [values[0].primal, values[1].primal],
        span,
    )?;
    guard.check(builder, source.primal(), span)?;
    if values[0].tangents.len() != values[1].tangents.len() {
        return Err(super::invalid(span));
    }
    let tangents = values[0]
        .tangents
        .iter()
        .zip(&values[1].tangents)
        .map(|(left, right)| builder.scalar_binary_structural_tangent(source, [*left, *right]))
        .collect::<Result<_, _>>()?;
    Ok(StateJacobianProductValue {
        primal: source.primal(),
        tangents,
    })
}
