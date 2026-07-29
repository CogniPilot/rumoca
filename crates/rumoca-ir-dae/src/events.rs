use std::marker::PhantomData;

use rumoca_core::ClockRational;

use crate::model::{Storage, check_provenance, checked_u32, unknown};
use crate::{
    ConditionId, DaeConstructionError, DaeProvenance, DiscreteRealId, DiscreteValueId,
    EventActionId, ExprId, ScalarType, StateId, TimeEventId,
};

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TimeEventEntry {
    pub(crate) instant: ClockRational,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum EventActionKind {
    Assert { message: u32, level: Option<u32> },
    Terminate { message: u32 },
    Reinitialize { state: u32, value: u32 },
    AssignDiscreteReal { target: u32, value: u32 },
    AssignDiscreteValue { target: u32, value: u32 },
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
    pub(crate) instant: &'dae ClockRational,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> TimeEventView<'dae> {
    pub const fn instant(self) -> &'dae ClockRational {
        self.instant
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
    AssignDiscreteReal {
        target: DiscreteRealId<'dae>,
        value: ExprId<'dae>,
    },
    AssignDiscreteValue {
        target: DiscreteValueId<'dae>,
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
            instant,
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

    pub fn assign_discrete_real(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        target: DiscreteRealId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.expect_guard(trigger, provenance)?;
        self.expect_guard(guard, provenance)?;
        self.storage
            .expect_discrete_real_target(target, value, provenance)?;
        self.expect_clock_ownership(guard, target.index(), provenance)?;
        self.insert_action(
            trigger,
            guard,
            EventActionKind::AssignDiscreteReal {
                target: target.index(),
                value: value.index(),
            },
            provenance,
        )
    }

    pub fn assign_discrete_value(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        target: DiscreteValueId<'dae>,
        value: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<EventActionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.expect_guard(trigger, provenance)?;
        self.expect_guard(guard, provenance)?;
        self.storage
            .expect_discrete_value_target(target, value, provenance)?;
        self.expect_clock_ownership(guard, target.index(), provenance)?;
        let already_defined = self
            .storage
            .discrete_assignments
            .iter()
            .any(|entry| entry.target == target.index())
            || self.storage.event_actions.iter().any(|entry| {
                matches!(
                    entry.kind,
                    EventActionKind::AssignDiscreteValue {
                        target: found,
                        ..
                    } if found == target.index()
                )
            });
        if self
            .storage
            .discrete_assignments
            .iter()
            .any(|entry| entry.target == target.index())
        {
            return Err(DaeConstructionError::DuplicateKey {
                kind: "B.1c target",
                key: self
                    .storage
                    .variable_name(target.index(), provenance)?
                    .to_string(),
                span: provenance.span(),
            });
        }
        let action = self.insert_action(
            trigger,
            guard,
            EventActionKind::AssignDiscreteValue {
                target: target.index(),
                value: value.index(),
            },
            provenance,
        )?;
        if !already_defined {
            self.storage.unassigned_discrete_values -= 1;
        }
        Ok(action)
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

    fn expect_clock_ownership(
        &self,
        guard: ConditionId<'dae>,
        variable: u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let Some(clock) = condition_owner_clock(self.storage, guard.index()) else {
            return Ok(());
        };
        if self
            .storage
            .clock_ownerships
            .iter()
            .any(|ownership| ownership.variable == variable && ownership.clock == clock)
        {
            return Ok(());
        }
        Err(DaeConstructionError::MissingClockOwnership {
            variable,
            clock,
            span: provenance.span(),
        })
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

fn condition_owner_clock(storage: &Storage, condition: u32) -> Option<u32> {
    let node = storage.conditions.get(condition as usize)?.node?;
    match node {
        crate::conditions::ConditionNode::Initial => None,
        crate::conditions::ConditionNode::Clock(clock) => Some(clock),
        crate::conditions::ConditionNode::And { lhs, rhs } => merge_owner_clocks(
            condition_owner_clock(storage, lhs),
            condition_owner_clock(storage, rhs),
            false,
        ),
        crate::conditions::ConditionNode::Or { lhs, rhs } => merge_owner_clocks(
            condition_owner_clock(storage, lhs),
            condition_owner_clock(storage, rhs),
            true,
        ),
        crate::conditions::ConditionNode::Relation(_)
        | crate::conditions::ConditionNode::Discrete(_)
        | crate::conditions::ConditionNode::Not(_) => None,
    }
}

fn merge_owner_clocks(lhs: Option<u32>, rhs: Option<u32>, disjunction: bool) -> Option<u32> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Some(clock),
        _ => None,
    }
}
