//! Rebuild checked condition, root, time-event, and event-action owners.
//!
//! These owners form one activation boundary: relations are rebuilt first,
//! reserved conditions are defined against them, roots consume those defined
//! conditions, and event actions retain their typed trigger and branch guard.

use rumoca_ir_dae as dae;

use super::variables::{ReservedVariable, TargetVariable};

pub(super) fn rebuild_relations<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
) -> Result<Vec<dae::RelationId<'target>>, dae::DaeConstructionError> {
    (0..source.relation_count())
        .map(|index| {
            let id = source
                .relation_id(index)
                .expect("finalized relation ordinal resolves");
            let relation = source
                .relation(id)
                .expect("finalized relation identity resolves");
            target.conditions(|conditions| {
                conditions.relation(
                    expressions[relation.expression().index() as usize],
                    relation.provenance(),
                )
            })
        })
        .collect()
}

pub(super) fn define_conditions<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    conditions: &[dae::ConditionId<'target>],
    relations: &[dae::RelationId<'target>],
    clocks: &[super::temporal::RebuiltClock<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for (index, target_id) in conditions.iter().copied().enumerate() {
        let source_id = source
            .condition_id(index)
            .expect("finalized condition ordinal resolves");
        let condition = source
            .condition(source_id)
            .expect("finalized condition identity resolves");
        let input = match condition.operation() {
            dae::ConditionOperation::Initial => dae::ConditionInput::Initial,
            dae::ConditionOperation::Always => dae::ConditionInput::Always,
            dae::ConditionOperation::Relation(id) => {
                dae::ConditionInput::Relation(relations[id.index() as usize])
            }
            dae::ConditionOperation::Discrete(expression) => {
                dae::ConditionInput::Discrete(expressions[expression.index() as usize])
            }
            dae::ConditionOperation::Not(id) => {
                dae::ConditionInput::Not(conditions[id.index() as usize])
            }
            dae::ConditionOperation::And(lhs, rhs) => dae::ConditionInput::And(
                conditions[lhs.index() as usize],
                conditions[rhs.index() as usize],
            ),
            dae::ConditionOperation::Or(lhs, rhs) => dae::ConditionInput::Or(
                conditions[lhs.index() as usize],
                conditions[rhs.index() as usize],
            ),
            dae::ConditionOperation::AnyRise(lhs, rhs) => dae::ConditionInput::AnyRise(
                conditions[lhs.index() as usize],
                conditions[rhs.index() as usize],
            ),
            dae::ConditionOperation::Clock(id) => {
                dae::ConditionInput::Clock(clocks[id.index() as usize].clock_id())
            }
        };
        target
            .conditions(|conditions| conditions.define(target_id, input, condition.provenance()))?;
    }
    Ok(())
}

pub(super) fn rebuild_roots<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    conditions: &[dae::ConditionId<'target>],
    relations: &[dae::RelationId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for index in 0..source.root_count() {
        let id = source
            .root_id(index)
            .expect("finalized root ordinal resolves");
        let root = source.root(id).expect("finalized root identity resolves");
        target.conditions(|target| {
            target.root(
                relations[root.relation().index() as usize],
                conditions[root.activation().index() as usize],
                root.provenance(),
            )
        })?;
    }
    Ok(())
}

pub(super) fn rebuild_events<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
) -> Result<(), dae::DaeConstructionError> {
    for index in 0..source.time_event_count() {
        let id = source
            .time_event_id(index)
            .expect("finalized time-event ordinal resolves");
        let event = source
            .time_event(id)
            .expect("finalized time-event identity resolves");
        target.events(|events| events.time_event(*event.instant(), event.provenance()))?;
    }
    for index in 0..source.event_action_count() {
        rebuild_event_action(source, target, expressions, variables, conditions, index)?;
    }
    Ok(())
}

fn rebuild_event_action<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &[ReservedVariable<'target>],
    conditions: &[dae::ConditionId<'target>],
    index: usize,
) -> Result<(), dae::DaeConstructionError> {
    let id = source
        .event_action_id(index)
        .expect("finalized event-action ordinal resolves");
    let action = source
        .event_action(id)
        .expect("finalized event-action identity resolves");
    let trigger = conditions[action.trigger().index() as usize];
    let guard = conditions[action.guard().index() as usize];
    target.events(|events| match action.operation() {
        dae::EventActionOperation::Assert { message, level } => events.assert_with_level(
            trigger,
            guard,
            expressions[message.index() as usize],
            level.map(|level| expressions[level.index() as usize]),
            action.provenance(),
        ),
        dae::EventActionOperation::Terminate { message } => events.terminate(
            trigger,
            guard,
            expressions[message.index() as usize],
            action.provenance(),
        ),
        dae::EventActionOperation::Reinitialize { state, value } => {
            let TargetVariable::State(state) = variables[state.index() as usize].identity else {
                unreachable!("event reinitialization target retains its state role")
            };
            events.reinitialize(
                trigger,
                guard,
                state,
                expressions[value.index() as usize],
                action.provenance(),
            )
        }
    })?;
    Ok(())
}
