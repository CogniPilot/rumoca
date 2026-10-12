//! Edge buffers of event conditions (MLS §8.5).

use super::*;

pub(super) fn lower_condition_memory<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows<'dae>,
) -> Result<(), LowerError> {
    let edge_conditions = unowned_edge_conditions(view, &rows.ordinary_trigger_edges);
    let mut internal_constants = BTreeMap::new();
    for (condition, condition_view) in view.conditions() {
        // A clock-owned condition is the activation of its partition and needs
        // no edge buffer, unless an issued ordinary branch consumes its edge
        // or it is an element `bi` of an unowned vector `when`. MLS §8.3.5.1
        // requires the buffer in both cases.
        if condition_clock_owner(view, condition).is_some() && !edge_conditions.contains(&condition)
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
        // A negated internal Always is false at every instant. Its zeroed
        // memory slot already represents that truth, so no refresh row is
        // needed. Source literal conditions are Discrete nodes and retain
        // their initialization/edge buffers, including source `when true`.
        if internal_false_condition(view, condition, &mut internal_constants) {
            continue;
        }
        let span = condition_view.provenance().span();
        let memory = condition_memory(layout, condition, span)?;
        let target = solve::scalar_slot_p(memory);
        // A condition built from clocked relations is only meaningful on its clock's
        // ticks, and its operands only resolve while that schedule is active.
        let clock = condition_operand_clock(view, clocks, condition, span)?;
        // A condition no clock owns is a step of its clock's partition only
        // when it reads a `previous` coordinate, which resolves on that clock's
        // ticks alone. Otherwise (a `when` on a variable a clock updates) the
        // buffer is refreshed with the other unowned buffers, after the `when`
        // that reads it has seen the edge: a buffer refreshed in the partition,
        // right after the variable it reads, would hide the edge from that
        // `when`.
        let clock = clock.filter(|_| {
            condition_clock_owner(view, condition).is_some()
                || condition_reads_previous(view, condition)
        });
        let program = match clock {
            Some((clock, _)) => layout
                .scalar_compiler(view, None)
                .clocked_condition_program(clock, condition)?,
            None => layout
                .scalar_compiler(view, None)
                .condition_program(condition)?,
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

/// Prove constant false from internal Always/Not nodes alone. Reading no
/// expression, clock or initial flag preserves every real activation owner.
fn internal_false_condition<'dae>(
    view: dae::DaeView<'dae>,
    mut condition: dae::ConditionId<'dae>,
    constants: &mut BTreeMap<dae::ConditionId<'dae>, Option<bool>>,
) -> bool {
    let mut pending = Vec::new();
    let mut value = loop {
        if let Some(value) = constants.get(&condition) {
            break *value;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Not(operand) => {
                pending.push(condition);
                condition = operand;
            }
            dae::ConditionOperation::Always => break Some(true),
            _ => break None,
        }
    };
    constants.insert(condition, value);
    while let Some(condition) = pending.pop() {
        value = value.map(|value| !value);
        constants.insert(condition, value);
    }
    value == Some(false)
}

/// Whether a relation or discrete operand of `condition` reads a `previous`
/// coordinate.
fn condition_reads_previous<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
) -> bool {
    let mut reads = false;
    let mut pending = vec![condition];
    while let Some(current) = pending.pop() {
        let Some(node) = view.condition(current) else {
            continue;
        };
        let expression = match node.operation() {
            dae::ConditionOperation::Relation(relation) => view
                .relation(relation)
                .map(|relation| relation.expression()),
            dae::ConditionOperation::Discrete(expression) => Some(expression),
            dae::ConditionOperation::Not(operand) => {
                pending.push(operand);
                None
            }
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.push(lhs);
                pending.push(rhs);
                None
            }
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => None,
        };
        if let Some(expression) = expression {
            super::super::clock_ownership::for_each_dependency(view, expression, |node| {
                reads |= matches!(
                    node.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Previous(_))
                );
            });
        }
    }
    reads
}

/// Exact issued ordinary trigger edges and the leaves of unowned vector
/// activations. A selected Boolean task does not remove an ordinary edge.
fn unowned_edge_conditions<'dae>(
    view: dae::DaeView<'dae>,
    issued: &BTreeSet<dae::ConditionId<'dae>>,
) -> BTreeSet<dae::ConditionId<'dae>> {
    let mut elements = issued.clone();
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
                super::super::clock_ownership::for_each_expression_clock_owner(
                    view,
                    expression,
                    |variable| clocks.variable_clock(variable),
                    |clock| visit((clock, clocks.clock(clock).ok())),
                );
            }
            dae::ConditionOperation::Discrete(expression) => {
                super::super::clock_ownership::for_each_expression_clock_owner(
                    view,
                    expression,
                    |variable| clocks.variable_clock(variable),
                    |clock| visit((clock, clocks.clock(clock).ok())),
                );
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
    checked_condition_clock_owner(view, condition)
}

pub(super) fn checked_condition_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionView<'dae>,
) -> Option<dae::ClockId<'dae>> {
    condition.synchronous_clock_owner().or_else(|| {
        let clock = condition.projected_task_clock()?;
        matches!(
            view.clock(clock)?.operation(),
            dae::ClockOperation::Periodic(_)
        )
        .then_some(clock)
    })
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

#[cfg(test)]
mod internal_constant_tests {
    use super::*;

    fn define_negation_chain(
        conditions: &mut dae::Conditions<'_, '_>,
        internal: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        let ids = (0..=1024)
            .map(|_| conditions.reserve(internal))
            .collect::<Result<Vec<_>, _>>()?;
        conditions.define(ids[1024], dae::ConditionInput::Always, internal)?;
        for index in (0..1024).rev() {
            conditions.define(
                ids[index],
                dae::ConditionInput::Not(ids[index + 1]),
                internal,
            )?;
        }
        Ok(())
    }

    #[test]
    fn shared_negation_chain_is_memoized_without_index_order_assumptions() {
        let mut source_map = rumoca_core::SourceMap::new();
        let source = source_map.add("constant.mo", "true");
        let authored = dae::DaeProvenance::source(Span::from_offsets(source, 0, 4)).unwrap();
        let internal =
            dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, authored.span())
                .unwrap();
        let model = dae::Dae::construct(source_map, |model| {
            let literal = model.expressions(|expressions| {
                expressions
                    .at(authored)
                    .literal(dae::DaeLiteral::Boolean(true))
            })?;
            model.conditions(|conditions| {
                define_negation_chain(conditions, internal)?;
                let unknown = conditions.reserve(authored)?;
                conditions.define(unknown, dae::ConditionInput::Discrete(literal), authored)
            })
        })
        .expect("defined operands permit reserved IDs in reverse order");
        model.inspect(|view| {
            let mut constants = BTreeMap::new();
            let conditions = view.conditions().map(|(id, _)| id).collect::<Vec<_>>();
            assert!(!internal_false_condition(
                view,
                conditions[0],
                &mut constants
            ));
            assert_eq!(
                constants.len(),
                1025,
                "each shared chain node is issued once"
            );
            for (index, condition) in conditions[..1025].iter().copied().enumerate() {
                assert_eq!(
                    internal_false_condition(view, condition, &mut constants),
                    index % 2 == 1
                );
                assert_eq!(constants.len(), 1025, "queries reuse the chain proof");
            }
            assert!(!internal_false_condition(
                view,
                conditions[1025],
                &mut constants
            ));
            assert_eq!(constants.get(&conditions[1025]), Some(&None));
        });
    }
}
