//! Edge buffers of event conditions (MLS §8.5).

use super::*;

pub(super) fn lower_condition_memory<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    let vector_elements = unowned_vector_elements(view);
    for (condition, condition_view) in view.conditions() {
        // A clock-owned condition is the activation of its partition and needs
        // no edge buffer, unless it is an element `bi` of a vector `when` that
        // no partition owns: §8.3.5.1 activates that `when` on `edge(bi)`, so
        // each element keeps its own buffer.
        if condition_clock_owner(view, condition).is_some() && !vector_elements.contains(&condition)
        {
            continue;
        }
        // An `Always` activation is not a `when`: MLS §8.5 gives an internal
        // buffer to an *event generating expression*, and a section that runs
        // because the section runs generates no event. With no buffer its slot
        // stays zero and `edge(c) = c and not pre(c)` reads the level, which is
        // what an unguarded algorithm section and a section-level `assert`
        // mean. The test is the node, not the shape of an expression: a source
        // `when true then` is a real `when` and keeps its buffer, so §8.3.5.1
        // starts that buffer true and it never has an edge to run on.
        if matches!(condition_view.operation(), dae::ConditionOperation::Always) {
            continue;
        }
        let span = condition_view.provenance().span();
        let memory = condition_memory(layout, condition, span)?;
        let target = solve::scalar_slot_p(memory);
        // A condition built from clocked relations is only meaningful on its clock's
        // ticks, and its operands only resolve while that schedule is active.
        let clock = condition_operand_clock(view, clocks, condition, span)?;
        let program = match clock {
            Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                .clocked_condition_program(clock, condition)?,
            None => ScalarCompiler::new(view, layout, None).condition_program(condition)?,
        };
        let row = rows.targets.len();
        rows.push(
            program,
            span,
            target,
            condition_memory_role(view, condition),
            solve::DiscreteEventPreMode::FollowCurrent,
            clock.and_then(|(_, solve)| solve),
        );
        if let Some((_, Some(clock_owner))) = clock {
            // A clocked activation buffer targets no DAE variable; it joins
            // the issued order purely as a consumer so it observes this tick's
            // settled operand values.
            rows.record_clocked_producer(
                PendingClockedStep::ScalarRows {
                    start_row: row,
                    count: 1,
                },
                clock_owner,
                Vec::new(),
                Vec::new(),
                vec![condition],
                span,
            );
        }
    }
    Ok(())
}

/// The elements `bi` of every vector activation `{b1, ..., bn}` that no clock
/// partition owns (the leaves of each such `AnyRise` tree).
fn unowned_vector_elements<'dae>(view: dae::DaeView<'dae>) -> BTreeSet<dae::ConditionId<'dae>> {
    let mut elements = BTreeSet::new();
    for (condition, condition_view) in view.conditions() {
        if !matches!(
            condition_view.operation(),
            dae::ConditionOperation::AnyRise(..)
        ) || condition_clock_owner(view, condition).is_some()
        {
            continue;
        }
        let mut pending = vec![condition];
        while let Some(current) = pending.pop() {
            match view
                .condition(current)
                .expect("checked condition identity resolves")
                .operation()
            {
                dae::ConditionOperation::AnyRise(lhs, rhs) => {
                    pending.push(lhs);
                    pending.push(rhs);
                }
                _ => {
                    elements.insert(current);
                }
            }
        }
    }
    elements
}

/// The role of the edge buffer of `condition`.
///
/// A condition that reads a periodic clock activation lane is a pulse: MLS
/// §3.7.5 makes `sample(start, interval)` true only at its tick instants, so
/// its buffer is a [`solve::DiscreteRowRole::PulseConditionMemory`] whose left
/// limit at the next instant is the condition with every lane cleared.
fn condition_memory_role<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
) -> solve::DiscreteRowRole {
    let mut pending = vec![condition];
    while let Some(current) = pending.pop() {
        match view
            .condition(current)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Clock(_) => {
                return solve::DiscreteRowRole::PulseConditionMemory;
            }
            dae::ConditionOperation::Not(operand) => pending.push(operand),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.push(lhs);
                pending.push(rhs);
            }
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Relation(_)
            | dae::ConditionOperation::Discrete(_) => {}
        }
    }
    solve::DiscreteRowRole::ConditionMemory
}

/// The clock whose partition owns every relation reachable from `condition`.
///
/// Returns `None` for a continuous-time condition. Two different owning clocks would
/// make the condition unschedulable, so that is rejected rather than resolved by
/// picking one.
fn condition_operand_clock<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    condition: dae::ConditionId<'dae>,
    span: Span,
) -> Result<Option<(dae::ClockId<'dae>, Option<solve::PeriodicClockId>)>, LowerError> {
    let mut owner: Option<(dae::ClockId<'dae>, Option<solve::PeriodicClockId>)> = None;
    let mut conflict = false;
    let mut visit = |found: (dae::ClockId<'dae>, Option<solve::PeriodicClockId>)| match owner {
        Some((clock, _)) if clock != found.0 => conflict = true,
        Some(_) => {}
        None => owner = Some(found),
    };
    let mut pending = vec![condition];
    while let Some(current) = pending.pop() {
        let node = view
            .condition(current)
            .expect("checked condition identity resolves");
        match node.operation() {
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked condition relation resolves")
                    .expression();
                if let Some(found) = expression_clock_owner(view, clocks, expression) {
                    visit(found);
                }
            }
            dae::ConditionOperation::Discrete(expression) => {
                if let Some(found) = expression_clock_owner(view, clocks, expression) {
                    visit(found);
                }
            }
            dae::ConditionOperation::Not(operand) => pending.push(operand),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.push(lhs);
                pending.push(rhs);
            }
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => {}
        }
    }
    if conflict {
        return Err(LowerError::non_computable(
            "condition mixes relations from different clock partitions",
            span,
        ));
    }
    Ok(owner)
}

pub(super) fn condition_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
) -> Option<dae::ClockId<'dae>> {
    let condition = view
        .condition(condition)
        .expect("checked condition identity resolves");
    match condition.operation() {
        dae::ConditionOperation::Initial => None,
        dae::ConditionOperation::Clock(clock) => Some(clock),
        dae::ConditionOperation::And(lhs, rhs) => merge_condition_clocks(
            condition_clock_owner(view, lhs),
            condition_clock_owner(view, rhs),
            false,
        ),
        dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
            merge_condition_clocks(
                condition_clock_owner(view, lhs),
                condition_clock_owner(view, rhs),
                true,
            )
        }
        dae::ConditionOperation::Always
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_) => None,
    }
}

fn merge_condition_clocks<'dae>(
    lhs: Option<dae::ClockId<'dae>>,
    rhs: Option<dae::ClockId<'dae>>,
    disjunction: bool,
) -> Option<dae::ClockId<'dae>> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Some(clock),
        _ => None,
    }
}

pub(in crate::lower) fn condition_memory(
    layout: &LoweredLayout<'_>,
    condition: dae::ConditionId<'_>,
    span: Span,
) -> Result<usize, LowerError> {
    layout
        .condition_memory
        .get(condition.index() as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("condition has no Solve memory slot", span))
}
