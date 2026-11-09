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
    pub(super) inputs: Vec<ProgramRegister<'program>>,
    matrix: Vec<Option<ProgramRegister<'program>>>,
    claimed: Vec<bool>,
    rows_written: usize,
    pub(super) states: usize,
    pub(super) parameters: usize,
    pub(super) compact: Vec<super::compact::BodyReceipt<'program>>,
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
            compact: Vec::new(),
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
        target: &crate::SolveTargetSelection<'_>,
        span: Span,
    ) -> Result<Result<StructuralPattern, StructuralPatternError>, Error> {
        let written = self
            .compact
            .iter()
            .try_fold(self.rows_written, |n, c| n.checked_add(c.points));
        if written != Some(self.states) {
            return Err(invalid(span));
        }
        let dependencies = self.dependencies(span);
        // Dense expansion is requested only here by the chosen target. Both
        // projections come from the same retained construction-issued receipt.
        let compact = self
            .compact
            .iter()
            .map(|c| (&c.placement, c.value))
            .collect::<Vec<_>>();
        let value = target
            .materialize_model_jacobian(
                self.builder,
                &compact,
                &self.matrix,
                self.zero,
                order,
                span,
            )?
            .value;
        self.builder
            .check_finite(value, SolveFiniteCheck::Derivative, span)?;
        self.builder.store(self.output, value, span)?;
        Ok(dependencies)
    }

    fn dependencies(&self, span: Span) -> Result<StructuralPattern, StructuralPatternError> {
        use super::super::{
            PatternDerivation, PatternProvenance, PatternRepresentation,
            stacked::{PatternSegment, SegmentRows},
        };
        if self.compact.is_empty() {
            return issue_dependencies(self.states, &self.matrix, span);
        }
        let provenance =
            PatternProvenance::derived(PatternDerivation::DependencyPropagation, span)?;
        let mut segments = Vec::new();
        for (row, &claimed) in self.claimed.iter().enumerate() {
            if !claimed {
                continue;
            }
            let columns = self.matrix[row * self.states..(row + 1) * self.states]
                .iter()
                .enumerate()
                .filter_map(|(i, v)| v.is_some().then_some(i))
                .collect();
            let pattern = StructuralPattern::from_checked_row_dependencies(
                1,
                self.states,
                &[columns],
                provenance,
            )?;
            segments.push(PatternSegment {
                rows: SegmentRows::Contiguous { start: row as u32 },
                pattern,
            });
        }
        for receipt in &self.compact {
            if let PatternRepresentation::Stacked { segments: children } =
                &receipt.relation.owner.representation
            {
                segments.extend(children.iter().cloned());
            }
        }
        StructuralPattern::checked_stacked(self.states, self.states, segments, provenance)
    }
}
