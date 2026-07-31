//! Rebuild the clocked, previous-value, terminal and delay coordinates.
//!
//! These coordinates share one property that separates them from ordinary
//! expressions: each carries an identity the construction interner must issue
//! before the expression referring to it can be built. Clocks are rebuilt with
//! their ownership, previous values and terminals with their clock, and a
//! delay seeds its own coordinate expression so the general expression
//! rebuilder never has to invent one.

use rumoca_ir_dae as dae;

use super::DirectStateConstraint;
use super::expressions::{ExpressionRebuilder, RebuiltIdentities};
use super::variables::{ReservedVariable, TargetVariable};

#[derive(Clone, Copy)]
pub(super) enum RebuiltClock<'dae> {
    Periodic(dae::PeriodicClockId<'dae>),
    Triggered(dae::ClockId<'dae>),
}

impl<'dae> RebuiltClock<'dae> {
    pub(super) fn clock_id(self) -> dae::ClockId<'dae> {
        match self {
            Self::Periodic(clock) => clock.into(),
            Self::Triggered(clock) => clock,
        }
    }

    pub(super) fn periodic(self) -> dae::PeriodicClockId<'dae> {
        match self {
            Self::Periodic(clock) => clock,
            Self::Triggered(_) => {
                unreachable!("source PeriodicClockId preserves periodic capability")
            }
        }
    }
}

pub(super) type RebuiltClocks<'dae> = Vec<RebuiltClock<'dae>>;

pub(super) fn rebuild_clocks<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<RebuiltClocks<'target>, dae::DaeConstructionError> {
    let mut clocks = Vec::with_capacity(source.clock_count());
    for index in 0..source.clock_count() {
        let id = source
            .clock_id(index)
            .expect("finalized clock ordinal resolves");
        let clock = source.clock(id).expect("finalized clock identity resolves");
        let rebuilt = target.clocks(|target| match clock.operation() {
            dae::ClockOperation::Periodic(lattice) => target
                .periodic(*lattice, clock.provenance())
                .map(RebuiltClock::Periodic),
            dae::ClockOperation::Triggered(condition) => target
                .triggered(conditions[condition.index() as usize], clock.provenance())
                .map(RebuiltClock::Triggered),
        })?;
        clocks.push(rebuilt);
    }
    for index in 0..source.clock_ownership_count() {
        rebuild_clock_ownership(source, target, variables, &clocks, index)?;
    }
    Ok(clocks)
}

fn rebuild_clock_ownership<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    clocks: &RebuiltClocks<'target>,
    index: usize,
) -> Result<(), dae::DaeConstructionError> {
    let id = source
        .clock_ownership_id(index)
        .expect("finalized clock ownership ordinal resolves");
    let ownership = source
        .clock_ownership(id)
        .expect("finalized clock ownership identity resolves");
    let clock = clocks[ownership.clock().index() as usize].clock_id();
    target.clocks(|target| match ownership.kind() {
        dae::ClockedVariableKind::DiscreteReal => {
            let TargetVariable::DiscreteReal(variable) =
                variables[ownership.variable().index() as usize].identity
            else {
                unreachable!("clock ownership retains its discrete-real role")
            };
            target.own_discrete_real(clock, variable, ownership.provenance())
        }
        dae::ClockedVariableKind::DiscreteValue => {
            let TargetVariable::DiscreteValue(variable) =
                variables[ownership.variable().index() as usize].identity
            else {
                unreachable!("clock ownership retains its discrete-value role")
            };
            target.own_discrete_value(clock, variable, ownership.provenance())
        }
    })?;
    Ok(())
}

pub(super) struct RebuiltTemporal<'dae> {
    pub(super) previous: Vec<dae::PreviousId<'dae>>,
    pub(super) terminals: Vec<dae::TerminalId<'dae>>,
}

enum RebuiltDelay<'source, 'target> {
    Parameter {
        source: dae::ExprId<'target>,
        delay_time: dae::ExprId<'target>,
        evidence: dae::PositiveParameterView<'source>,
    },
    Bounded {
        source: dae::ExprId<'target>,
        delay_time: dae::ExprId<'target>,
        delay_max: dae::ExprId<'target>,
        evidence: dae::PositiveParameterView<'source>,
    },
}

pub(super) fn rebuild_temporal_coordinates<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    variables: &[ReservedVariable<'target>],
    clocks: &RebuiltClocks<'target>,
) -> Result<RebuiltTemporal<'target>, dae::DaeConstructionError> {
    let mut previous = Vec::with_capacity(source.previous_value_count());
    for index in 0..source.previous_value_count() {
        let id = source
            .previous_id(index)
            .expect("finalized previous-value ordinal resolves");
        let entry = source
            .previous(id)
            .expect("finalized previous-value identity resolves");
        let clock = clocks[entry.clock().index() as usize].clock_id();
        let rebuilt = target.temporal(|target| {
            match variables[entry.variable().index() as usize].identity {
                TargetVariable::DiscreteReal(variable) => {
                    target.previous_discrete_real(clock, variable, entry.provenance())
                }
                TargetVariable::DiscreteValue(variable) => {
                    target.previous_discrete_value(clock, variable, entry.provenance())
                }
                _ => unreachable!("previous coordinate retains its discrete variable role"),
            }
        })?;
        previous.push(rebuilt);
    }
    let terminals = (0..source.terminal_count())
        .map(|index| {
            let id = source
                .terminal_id(index)
                .expect("finalized terminal ordinal resolves");
            let entry = source
                .terminal(id)
                .expect("finalized terminal identity resolves");
            target.temporal(|target| target.terminal(entry.provenance()))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(RebuiltTemporal {
        previous,
        terminals,
    })
}

pub(super) fn rebuild_delay_coordinates<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    identities: RebuiltIdentities<'_, 'target>,
    derivative_definitions: &[Option<u32>],
    candidate: Option<DirectStateConstraint>,
    rebuilt: &mut [Option<dae::ExprId<'target>>],
) -> Result<(), dae::DaeConstructionError> {
    let mut coordinate_indices = vec![None; source.delay_count()];
    for index in 0..source.expression_count() {
        let expression_id = source
            .expression_id(index)
            .expect("finalized expression ordinal resolves");
        let expression = source
            .expression(expression_id)
            .expect("finalized expression identity resolves");
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Delay(delay)) =
            expression.operation()
        {
            coordinate_indices[delay.index() as usize] = Some(index);
        }
    }
    for (index, coordinate_index) in coordinate_indices.into_iter().enumerate() {
        let source_id = source
            .delay_id(index)
            .expect("finalized delay ordinal resolves");
        let delay = source
            .delay(source_id)
            .expect("finalized delay identity resolves");
        let coordinate_index =
            coordinate_index.expect("checked delay has exactly one coordinate expression");
        let coordinate_id = source
            .expression_id(coordinate_index)
            .expect("delay coordinate expression resolves");
        let coordinate_provenance = source
            .expression(coordinate_id)
            .expect("delay coordinate expression identity resolves")
            .provenance();
        let rebuilt_delay = target.expressions(|expressions| {
            let mut rebuilder = ExpressionRebuilder::new(
                source,
                expressions,
                identities,
                derivative_definitions,
                candidate,
                rebuilt,
            );
            let source = rebuilder.rebuild(delay.source())?;
            match delay.operation() {
                dae::DelayOperation::ParameterDelay { delay_time } => Ok(RebuiltDelay::Parameter {
                    source,
                    delay_time: rebuilder.rebuild(delay_time.expression())?,
                    evidence: delay_time,
                }),
                dae::DelayOperation::BoundedDelay {
                    delay_time,
                    delay_max,
                } => Ok(RebuiltDelay::Bounded {
                    source,
                    delay_time: rebuilder.rebuild(delay_time)?,
                    delay_max: rebuilder.rebuild(delay_max.expression())?,
                    evidence: delay_max,
                }),
            }
        })?;
        let coordinate = construct_rebuilt_delay_coordinate(
            target,
            rebuilt_delay,
            delay.provenance(),
            coordinate_provenance,
        )?;
        if coordinate.id().index() as usize != index {
            return Err(dae::DaeConstructionError::ShapeMismatch {
                span: delay.provenance().span(),
            });
        }
        rebuilt[coordinate_index] = Some(coordinate.expression());
    }
    Ok(())
}

fn construct_rebuilt_delay_coordinate<'target>(
    target: &mut dae::DaeConstruction<'target>,
    delay: RebuiltDelay<'_, 'target>,
    owner: dae::DaeProvenance,
    coordinate_provenance: dae::DaeProvenance,
) -> Result<dae::DelayCoordinate<'target>, dae::DaeConstructionError> {
    match delay {
        RebuiltDelay::Parameter {
            source,
            delay_time,
            evidence,
        } => {
            let positive = target.temporal(|temporal| {
                temporal.positive_parameter(delay_time, evidence.value(), evidence.provenance())
            })?;
            target.expressions(|expressions| {
                expressions
                    .at(coordinate_provenance)
                    .delay(source, positive, owner)
            })
        }
        RebuiltDelay::Bounded {
            source,
            delay_time,
            delay_max,
            evidence,
        } => {
            let maximum = target.temporal(|temporal| {
                temporal.positive_parameter(delay_max, evidence.value(), evidence.provenance())
            })?;
            target.expressions(|expressions| {
                expressions
                    .at(coordinate_provenance)
                    .bounded_delay(source, delay_time, maximum, owner)
            })
        }
    }
}
