//! Checked finite state inputs and complete original-store row placement.
use super::{
    StateJacobianProductValue, StructuralPattern, StructuralPatternError, invalid,
    issue_dependencies,
};
use crate::{
    ProgramRegister, ProgramSlot, SolveArithmeticProfile, SolveFiniteCheck,
    SolveProgramConstructionError, SolveValue, StoredStateJacobianOutputOrder, TypedProgramBuilder,
};
use rumoca_core::Span;

type Error = SolveProgramConstructionError;

#[derive(Clone, Copy, Debug)]
pub enum StateJacobianProductInput {
    State(usize),
    Parameter(usize),
    Time,
}

/// Opaque direction construction for one branded typed program. The producer
/// cannot substitute a raw tangent vector or clear an active direction.
pub struct StateJacobianProductConstruction<'program, 'scope> {
    pub(super) builder: &'scope mut TypedProgramBuilder<'program>,
    inputs: Vec<ProgramRegister<'program>>,
    matrix: Vec<Option<ProgramRegister<'program>>>,
    claimed: Vec<bool>,
    rows_written: usize,
    pub(super) states: usize,
    parameters: usize,
    pub(super) profile: SolveArithmeticProfile,
    zero: ProgramRegister<'program>,
    one: ProgramRegister<'program>,
    output: ProgramSlot<'program>,
}

impl<'program, 'scope> StateJacobianProductConstruction<'program, 'scope> {
    pub(super) fn new(
        builder: &'scope mut TypedProgramBuilder<'program>,
        inputs: &[ProgramSlot<'program>],
        output: ProgramSlot<'program>,
        (states, parameters, profile): (usize, usize, SolveArithmeticProfile),
        span: Span,
    ) -> Result<Self, Error> {
        if states == 0 {
            return Err(invalid(span));
        }
        let cells = states.checked_mul(states).ok_or_else(|| invalid(span))?;
        let inputs = inputs
            .iter()
            .enumerate()
            .map(|(index, slot)| {
                let value = builder.load(*slot, span)?;
                let coordinate = u32::try_from(index).map_err(|_| invalid(span))?;
                builder.check_finite(value, SolveFiniteCheck::Input { coordinate }, span)?;
                Ok(value)
            })
            .collect::<Result<Vec<_>, Error>>()?;
        let zero = builder.constant(SolveValue::real(profile, 0.0), span)?;
        let one = builder.constant(SolveValue::real(profile, 1.0), span)?;
        Ok(Self {
            builder,
            inputs,
            matrix: vec![None; cells],
            claimed: vec![false; states],
            rows_written: 0,
            states,
            parameters,
            profile,
            zero,
            one,
            output,
        })
    }

    pub fn read(
        &mut self,
        input: StateJacobianProductInput,
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<StateJacobianProductValue<'program>, Error> {
        let (index, state) = match input {
            StateJacobianProductInput::State(index) if index < self.states => (index, true),
            StateJacobianProductInput::Parameter(index) if index < self.parameters => {
                (self.states + index, false)
            }
            StateJacobianProductInput::Time => (self.inputs.len() - 1, false),
            _ => return Err(invalid(span)),
        };
        let primal = self.inputs[index];
        self.builder.check_finite(primal, check, span)?;
        let tangents = (0..self.states)
            .map(|column| (state && index == column).then_some(self.one))
            .collect();
        Ok(StateJacobianProductValue { primal, tangents })
    }

    pub fn constant(
        &mut self,
        value: f64,
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<StateJacobianProductValue<'program>, Error> {
        let primal = self
            .builder
            .constant(SolveValue::real(self.profile, value), span)?;
        self.builder.check_finite(primal, check, span)?;
        Ok(StateJacobianProductValue {
            primal,
            tangents: vec![None; self.states],
        })
    }

    pub fn copy(
        &mut self,
        value: &StateJacobianProductValue<'program>,
        (span, check): (Span, SolveFiniteCheck),
    ) -> Result<StateJacobianProductValue<'program>, Error> {
        self.builder.check_finite(value.primal, check, span)?;
        Ok(value.clone())
    }

    pub fn store_row(
        &mut self,
        row: usize,
        value: &StateJacobianProductValue<'program>,
        span: Span,
    ) -> Result<(), Error> {
        if row >= self.states || self.claimed[row] || value.tangents.len() != self.states {
            return Err(invalid(span));
        }
        self.matrix[row * self.states..(row + 1) * self.states].copy_from_slice(&value.tangents);
        self.claimed[row] = true;
        self.rows_written += 1;
        Ok(())
    }

    pub(super) fn finish(
        self,
        order: &StoredStateJacobianOutputOrder<'_>,
        span: Span,
    ) -> Result<Result<StructuralPattern, StructuralPatternError>, Error> {
        if self.rows_written != self.states {
            return Err(invalid(span));
        }
        let dependencies = issue_dependencies(self.states, &self.matrix, span);
        let mut values = Vec::with_capacity(self.matrix.len());
        for row in order.row_coordinates() {
            for column in order.column_coordinates() {
                values.push(self.matrix[row * self.states + column].unwrap_or(self.zero));
            }
        }
        let width = u32::try_from(self.states).map_err(|_| invalid(span))?;
        let value = self
            .builder
            .construct_aggregate(&values, vec![width, width], span)?;
        self.builder
            .check_finite(value, SolveFiniteCheck::Derivative, span)?;
        self.builder.store(self.output, value, span)?;
        Ok(dependencies)
    }
}
