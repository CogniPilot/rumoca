//! Continuous event roots and their checked relation ownership.

use super::*;

pub(super) struct LoweredRoots {
    pub(super) programs: solve::ScalarProgramBlock,
    pub(super) zero_domains: Vec<solve::RootZeroDomain>,
    pub(super) relation_memory_targets: Vec<Option<solve::ScalarSlot>>,
}

pub(super) fn lower_roots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<LoweredRoots, LowerError> {
    let mut rows = ScalarRows::default();
    let mut zero_domains = Vec::with_capacity(view.root_count());
    let mut relation_memory_targets = Vec::with_capacity(view.root_count());
    let mut owner_relations = Vec::new();
    let mut owner_span = None;
    for (_, root) in view.roots() {
        let relation = view
            .relation(root.relation())
            .expect("checked root relation resolves");
        if super::super::clock_ownership::relation_clock_owner(
            view,
            relation.expression(),
            |variable| clocks.variable_clock(variable),
        )
        .is_some()
        {
            continue;
        }
        let span = root.provenance().span();
        if owner_span.is_some_and(|owner| owner != span) {
            flush_root_owner(view, layout, &mut rows, &mut owner_relations, owner_span)?;
        }
        owner_span = Some(span);
        owner_relations.push((
            root.relation(),
            view.model_activation_guard(relation.expression()),
        ));
        zero_domains.push(root_zero_domain(view, relation.expression()));
        relation_memory_targets.push(layout.buffered_relations.target(root.relation()));
    }
    flush_root_owner(view, layout, &mut rows, &mut owner_relations, owner_span)?;
    lower_structured_roots(
        view,
        layout,
        clocks,
        &mut rows,
        &mut zero_domains,
        &mut relation_memory_targets,
    )?;
    Ok(LoweredRoots {
        programs: rows.into_scalar_block()?,
        zero_domains,
        relation_memory_targets,
    })
}

fn flush_root_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    rows: &mut ScalarRows,
    relations: &mut Vec<(dae::RelationId<'dae>, Option<dae::ExprId<'dae>>)>,
    span: Option<Span>,
) -> Result<(), LowerError> {
    if relations.is_empty() {
        return Ok(());
    }
    let span = span.expect("a nonempty root owner group has provenance");
    let output_start = rows.stored_output_count(span)?;
    let output_end = output_start
        .checked_add(relations.len())
        .ok_or_else(|| LowerError::contract("root output ordinal overflow", span))?;
    let program = layout
        .scalar_compiler(view, None)
        .root_program_outputs(relations.iter().copied())?;
    rows.push_output_range(program, span, output_start..output_end);
    relations.clear();
    Ok(())
}

fn lower_structured_roots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &EventCompilationContext<'_, 'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut ScalarRows,
    zero_domains: &mut Vec<solve::RootZeroDomain>,
    relation_memory_targets: &mut Vec<Option<solve::ScalarSlot>>,
) -> Result<(), LowerError> {
    for (_, root) in view.structured_roots() {
        if expression_clock_owner(view, clocks, root.expression()).is_some() {
            return Err(LowerError::non_computable(
                "clocked structured root families are not continuously monitored",
                root.provenance().span(),
            ));
        }
        let domain = view
            .domain(root.domain())
            .expect("checked structured-root domain resolves")
            .structured();
        let points = domain.index_tuples().map_err(|error| {
            LowerError::contract(
                format!("structured root domain is invalid: {error}"),
                root.provenance().span(),
            )
        })?;
        let memory = layout.buffered_quotients.entries().iter().find(|memory| {
            memory.domain == Some(root.domain()) && memory.root_index == zero_domains.len()
        });
        for point in points {
            let row = zero_domains.len();
            let program = if let Some(&memory) = memory {
                layout.scalar_compiler(view, None).quotient_root_program(
                    memory,
                    &point,
                    root.expression(),
                    root.activation_guard(),
                    root.provenance().span(),
                )?
            } else {
                layout
                    .scalar_compiler(view, Some((root.domain(), &point)))
                    .root_expression_program(
                        root.expression(),
                        root.activation_guard(),
                        root.provenance().span(),
                    )?
            };
            rows.push(program, root.provenance().span(), row);
            zero_domains.push(root_zero_domain(view, root.expression()));
            relation_memory_targets.push(None);
        }
    }
    Ok(())
}

/// The clock whose partition an expression's operands belong to, if any.
///
/// MLS §16.5 confines `previous(x)` and every clock-owned declaration to their owning
/// partition, so an operand that names one is decisive: the expression is evaluated on
/// that clock's ticks and nowhere else. That is what keeps a clocked relation out of the
/// continuous root set (MLS §16.8.1 raises no state event for it — the tick already is
/// the event) and what tells the condition-memory row which schedule to compile under.
///
/// The first owner found wins; a clocked partition cannot mix clocks, and the callers
/// that must reject a mix check for it explicitly.
fn expression_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<(dae::ClockId<'dae>, Option<solve::PeriodicClockId>)> {
    let owner =
        super::super::clock_ownership::expression_clock_owner(view, expression, |variable| {
            clocks.variable_clock(variable)
        })?;
    // An MLS §16.3 event clock owns a partition but has no periodic schedule.
    Some((owner, clocks.clock(owner).ok()))
}

fn root_zero_domain<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> solve::RootZeroDomain {
    let operation = view
        .expression(expression)
        .expect("checked relation expression resolves")
        .operation();
    match operation {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::LessEqual | dae::BinaryOperator::GreaterEqual,
            ..
        } => solve::RootZeroDomain::NonPositive,
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Less | dae::BinaryOperator::Greater,
            ..
        } => solve::RootZeroDomain::Positive,
        _ => solve::RootZeroDomain::Previous,
    }
}
