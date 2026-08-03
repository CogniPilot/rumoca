use std::marker::PhantomData;

use rumoca_core::ClockRational;

use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ConditionId, DaeConstructionError, DaeProvenance, EventActionId, ExprId, ScalarType, StateId,
    TimeEventId,
};

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(untagged)]
pub(crate) enum TimeEventKind {
    Static { instant: ClockRational },
    Dynamic { deadline: u32 },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TimeEventEntry {
    #[serde(flatten)]
    pub(crate) kind: TimeEventKind,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum EventActionKind {
    Assert { message: u32, level: Option<u32> },
    Terminate { message: u32 },
    Reinitialize { state: u32, value: u32 },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct EventActionEntry {
    pub(crate) trigger: u32,
    pub(crate) guard: u32,
    pub(crate) kind: EventActionKind,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub struct TimeEventView<'dae> {
    pub(crate) operation: TimeEventOperation<'dae>,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub enum TimeEventOperation<'dae> {
    Static(&'dae ClockRational),
    Dynamic(ExprId<'dae>),
}

impl<'dae> TimeEventView<'dae> {
    pub const fn operation(self) -> TimeEventOperation<'dae> {
        self.operation
    }

    pub const fn instant(self) -> Option<&'dae ClockRational> {
        match self.operation {
            TimeEventOperation::Static(instant) => Some(instant),
            TimeEventOperation::Dynamic(_) => None,
        }
    }

    pub const fn deadline(self) -> Option<ExprId<'dae>> {
        match self.operation {
            TimeEventOperation::Static(_) => None,
            TimeEventOperation::Dynamic(deadline) => Some(deadline),
        }
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EventActionOperation<'dae> {
    Assert {
        message: ExprId<'dae>,
        level: Option<ExprId<'dae>>,
    },
    Terminate {
        message: ExprId<'dae>,
    },
    Reinitialize {
        state: StateId<'dae>,
        value: ExprId<'dae>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct EventActionView<'dae> {
    pub(crate) trigger: ConditionId<'dae>,
    pub(crate) guard: ConditionId<'dae>,
    pub(crate) operation: EventActionOperation<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> EventActionView<'dae> {
    pub const fn trigger(self) -> ConditionId<'dae> {
        self.trigger
    }

    pub const fn guard(self) -> ConditionId<'dae> {
        self.guard
    }

    pub const fn operation(self) -> EventActionOperation<'dae> {
        self.operation
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

pub struct Events<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Events<'_, 'dae> {
    pub fn time_event(
        &mut self,
        instant: ClockRational,
        provenance: DaeProvenance,
    ) -> Result<TimeEventId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let instant =
            ClockRational::new(instant.numerator(), instant.denominator()).map_err(|source| {
                DaeConstructionError::InvalidClockLattice {
                    source,
                    span: provenance.span(),
                }
            })?;
        let raw = checked_u32(
            self.storage.time_events.len(),
            "time event arena",
            provenance,
        )?;
        self.storage.time_events.push(TimeEventEntry {
            kind: TimeEventKind::Static { instant },
            provenance,
        });
        Ok(TimeEventId::from_raw(raw))
    }

    /// Own a time-event deadline that is fixed during continuous integration
    /// and re-evaluated after each event boundary.
    pub fn dynamic_time_event(
        &mut self,
        deadline: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<TimeEventId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage
            .expect_closed_expression(deadline, provenance)?;
        let ty = self.storage.expr_type(deadline, provenance)?;
        let variability = self.storage.expr_variability(deadline, provenance)?;
        if !ty.is_scalar()
            || ty.scalar_type() != ScalarType::Real
            || variability > crate::ExpressionVariability::Discrete
        {
            return Err(DaeConstructionError::InvalidDynamicTimeEventDeadline {
                span: provenance.span(),
            });
        }
        let raw = checked_u32(
            self.storage.time_events.len(),
            "time event arena",
            provenance,
        )?;
        self.storage.time_events.push(TimeEventEntry {
            kind: TimeEventKind::Dynamic {
                deadline: deadline.index(),
            },
            provenance,
        });
        Ok(TimeEventId::from_raw(raw))
    }

    pub fn assert(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        message: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        self.assert_with_level(trigger, guard, message, None, provenance)
    }

    pub fn assert_with_level(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        message: ExprId<'dae>,
        level: Option<ExprId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        if let Some(level) = level {
            self.storage.expect_closed_expression(level, provenance)?;
            let ty = self.storage.expr_type(level, provenance)?;
            if !ty.is_scalar() || !ty.scalar_type().is_numeric() {
                return Err(DaeConstructionError::ExpectedNumeric {
                    found: ty.scalar_type(),
                    span: provenance.span(),
                });
            }
        }
        self.message_action(
            trigger,
            guard,
            message,
            provenance,
            EventActionKind::Assert {
                message: message.index(),
                level: level.map(ExprId::index),
            },
        )
    }

    pub fn terminate(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        message: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        self.message_action(
            trigger,
            guard,
            message,
            provenance,
            EventActionKind::Terminate {
                message: message.index(),
            },
        )
    }

    pub fn reinitialize(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        state: StateId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.expect_guard(trigger, provenance)?;
        self.expect_guard(guard, provenance)?;
        self.storage.expect_state_update(state, value, provenance)?;
        self.insert_action(
            trigger,
            guard,
            EventActionKind::Reinitialize {
                state: state.index(),
                value: value.index(),
            },
            provenance,
        )
    }

    fn message_action(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        message: ExprId<'dae>,
        provenance: DaeProvenance,
        kind: EventActionKind,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.expect_guard(trigger, provenance)?;
        self.expect_guard(guard, provenance)?;
        self.storage.expect_closed_expression(message, provenance)?;
        let ty = self.storage.expr_type(message, provenance)?;
        if !ty.is_scalar() || ty.scalar_type() != ScalarType::String {
            return Err(DaeConstructionError::TypeMismatch {
                expected: ScalarType::String,
                found: ty.scalar_type(),
                span: provenance.span(),
            });
        }
        self.insert_action(trigger, guard, kind, provenance)
    }

    fn expect_guard(
        &self,
        guard: ConditionId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let condition = self
            .storage
            .conditions
            .get(guard.index() as usize)
            .ok_or_else(|| unknown("condition", guard.index(), provenance))?;
        if condition.node.is_none() {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "event guard condition",
                index: guard.index(),
                span: provenance.span(),
            });
        }
        Ok(())
    }

    fn insert_action(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        kind: EventActionKind,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        let raw = checked_u32(
            self.storage.event_actions.len(),
            "event action arena",
            provenance,
        )?;
        self.storage.event_actions.push(EventActionEntry {
            trigger: trigger.index(),
            guard: guard.index(),
            kind,
            provenance,
        });
        Ok(EventActionId::from_raw(raw))
    }
}
