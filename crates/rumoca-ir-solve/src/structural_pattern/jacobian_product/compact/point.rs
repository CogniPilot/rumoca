//! Branded point operations share scalar directional arithmetic and mapped guards.
use super::super::{
    StateJacobianProductValue, invalid,
    value::{self, Guard},
};
use crate::{
    CompactJacobianPlacement, ProgramRegister, ProgramSlot, SolveArithmeticProfile,
    SolveBinaryOperator, SolveProgramConstructionError, SolveUnaryOperator, SolveValue,
    TypedProgramBuilder,
};
use rumoca_core::Span;
type Error = SolveProgramConstructionError;

#[derive(Clone, Copy, Debug)]
pub enum StateJacobianCompactInput {
    Seed(usize),
    Parameter(usize),
    Time,
}

/// A fresh map body's only direction producer. Seed positions refer to the
/// issued placement's checked affine columns, never to arbitrary registers.
/// ```compile_fail,E0616
/// use rumoca_ir_solve::StateJacobianCompactConstruction;
/// fn raw_builder<'b,'s>(body: StateJacobianCompactConstruction<'b,'s>) {
///     let _ = body.builder;
/// }
/// ```
pub struct StateJacobianCompactConstruction<'body, 'scope> {
    builder: &'scope mut TypedProgramBuilder<'body>,
    placement: &'scope CompactJacobianPlacement,
    vector: ProgramRegister<'body>,
    ordinals: Vec<ProgramRegister<'body>>,
    point: ProgramRegister<'body>,
    zero: ProgramRegister<'body>,
    one: ProgramRegister<'body>,
    parameters: usize,
    base: u32,
    count: u32,
    profile: SolveArithmeticProfile,
}

impl<'body, 'scope> StateJacobianCompactConstruction<'body, 'scope> {
    pub(super) fn new(
        builder: &'scope mut TypedProgramBuilder<'body>,
        (vector, binders): (ProgramSlot<'body>, &[ProgramSlot<'body>]),
        placement: &'scope CompactJacobianPlacement,
        (parameters, base, profile, span): (usize, u32, SolveArithmeticProfile, Span),
    ) -> Result<Self, Error> {
        let vector = builder.load(vector, span)?;
        let one_index = integer(builder, profile, 1, span)?;
        let mut ordinals = Vec::with_capacity(binders.len());
        for binder in binders {
            let b = builder.load(*binder, span)?;
            ordinals.push(builder.binary(SolveBinaryOperator::Subtract, b, one_index, span)?);
        }
        let mut point = *ordinals.first().ok_or_else(|| invalid(span))?;
        for (&ordinal, &extent) in ordinals.iter().zip(&placement.extents).skip(1) {
            let e = integer(builder, profile, i64::from(extent), span)?;
            point = builder.binary(SolveBinaryOperator::Multiply, point, e, span)?;
            point = builder.binary(SolveBinaryOperator::Add, point, ordinal, span)?;
        }
        let count = placement
            .extents
            .iter()
            .try_fold(1u32, |n, &e| n.checked_mul(e))
            .ok_or_else(|| invalid(span))?;
        let zero = builder.constant(SolveValue::real(profile, 0.0), span)?;
        let one = builder.constant(SolveValue::real(profile, 1.0), span)?;
        Ok(Self {
            builder,
            placement,
            vector,
            ordinals,
            point,
            zero,
            one,
            parameters,
            base,
            count,
            profile,
        })
    }

    fn guard(&self, operation: u32) -> Guard<'body> {
        Guard::Mapped {
            base: self.base,
            count: self.count,
            operation,
            point: self.point,
        }
    }

    pub fn read(
        &mut self,
        input: StateJacobianCompactInput,
        (span, operation): (Span, u32),
    ) -> Result<StateJacobianProductValue<'body>, Error> {
        let (primal, seed) = match input {
            StateJacobianCompactInput::Seed(seed) => (self.load_seed(seed, span)?, Some(seed)),
            StateJacobianCompactInput::Parameter(p) if p < self.parameters => {
                (self.project(self.placement.states + p, span)?, None)
            }
            StateJacobianCompactInput::Time => (
                self.project(self.placement.states + self.parameters, span)?,
                None,
            ),
            _ => return Err(invalid(span)),
        };
        self.guard(operation).check(self.builder, primal, span)?;
        let tangents = (0..self.placement.columns.len())
            .map(|i| (seed == Some(i)).then_some(self.one))
            .collect();
        Ok(StateJacobianProductValue { primal, tangents })
    }

    fn load_seed(&mut self, seed: usize, span: Span) -> Result<ProgramRegister<'body>, Error> {
        let key = self
            .placement
            .columns
            .get(seed)
            .ok_or_else(|| invalid(span))?;
        let mut index = integer(
            self.builder,
            self.profile,
            key.base.checked_add(1).ok_or_else(|| invalid(span))?,
            span,
        )?;
        for (&stride, &ordinal) in key.strides.iter().zip(&self.ordinals) {
            let s = integer(self.builder, self.profile, stride, span)?;
            let term = self
                .builder
                .binary(SolveBinaryOperator::Multiply, s, ordinal, span)?;
            index = self
                .builder
                .binary(SolveBinaryOperator::Add, index, term, span)?;
        }
        self.builder
            .project_element_dynamic(self.vector, &[index], span)
    }

    fn project(&mut self, index: usize, span: Span) -> Result<ProgramRegister<'body>, Error> {
        self.builder.project_element(
            self.vector,
            vec![u32::try_from(index).map_err(|_| invalid(span))?],
            span,
        )
    }

    pub fn constant(
        &mut self,
        value: f64,
        (span, operation): (Span, u32),
    ) -> Result<StateJacobianProductValue<'body>, Error> {
        let primal = self
            .builder
            .constant(SolveValue::real(self.profile, value), span)?;
        self.guard(operation).check(self.builder, primal, span)?;
        Ok(StateJacobianProductValue {
            primal,
            tangents: vec![None; self.placement.columns.len()],
        })
    }

    pub fn copy(
        &mut self,
        value: &StateJacobianProductValue<'body>,
        (span, operation): (Span, u32),
    ) -> Result<StateJacobianProductValue<'body>, Error> {
        self.guard(operation)
            .check(self.builder, value.primal, span)?;
        Ok(value.clone())
    }

    pub fn unary(
        &mut self,
        operator: SolveUnaryOperator,
        value: &StateJacobianProductValue<'body>,
        (span, operation): (Span, u32),
    ) -> Result<StateJacobianProductValue<'body>, Error> {
        let guard = self.guard(operation);
        value::unary(self.builder, operator, value, (span, guard))
    }

    pub fn binary(
        &mut self,
        operator: SolveBinaryOperator,
        values: [&StateJacobianProductValue<'body>; 2],
        (span, operation): (Span, u32),
    ) -> Result<StateJacobianProductValue<'body>, Error> {
        let guard = self.guard(operation);
        value::binary(self.builder, operator, values, (span, guard))
    }

    pub(super) fn finish_output(
        self,
        value: StateJacobianProductValue<'body>,
        output: ProgramSlot<'body>,
        span: Span,
    ) -> Result<Vec<bool>, Error> {
        if value.tangents.len() != self.placement.columns.len() {
            return Err(invalid(span));
        }
        let present = value.tangents.iter().map(Option::is_some).collect();
        let stored = if value.tangents.is_empty() {
            self.zero
        } else {
            let values = value
                .tangents
                .iter()
                .map(|v| v.unwrap_or(self.zero))
                .collect::<Vec<_>>();
            self.builder.construct_aggregate(
                &values,
                vec![u32::try_from(values.len()).map_err(|_| invalid(span))?],
                span,
            )?
        };
        self.builder.store(output, stored, span)?;
        Ok(present)
    }
}

fn integer<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    profile: SolveArithmeticProfile,
    value: i64,
    span: Span,
) -> Result<ProgramRegister<'p>, Error> {
    let value = SolveValue::integer(profile, value).map_err(|_| invalid(span))?;
    builder.constant(value, span)
}
