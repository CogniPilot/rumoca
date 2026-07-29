//! Checked B.1c and clocked discrete-Real projection into GALEC `DoStep`.

use super::*;

struct PendingAssignment<'dae> {
    targets: Vec<dae::VariableId<'dae>>,
    reads: HashSet<u32>,
    statements: Vec<gast::Spanned<gast::Statement>>,
    span: Span,
}

pub(super) fn lower_clocked_assignments<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let mut pending = Vec::new();
    lower_discrete_value_owners(view, clock, by_id, pre_names, &mut pending)?;
    lower_discrete_real_actions(view, clock, by_id, pre_names, &mut pending)?;
    order_assignments(pending)
}

fn lower_discrete_real_actions<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    pending: &mut Vec<PendingAssignment<'dae>>,
) -> Result<(), GalecTargetError> {
    let mut owners: HashMap<u32, usize> = HashMap::new();
    for index in 0..view.event_action_count() {
        let action = view
            .event_action(
                view.event_action_id(index)
                    .expect("dense checked action identity"),
            )
            .expect("checked action resolves");
        let (target, value) = match action.operation() {
            dae::EventActionOperation::AssignDiscreteReal { target, value } => {
                (dae::VariableId::from(target), value)
            }
            operation => {
                return Err(unsupported(
                    "event-action",
                    format!(
                        "event action `{}` cannot be represented in GALEC DoStep",
                        event_name(operation)
                    ),
                    action.provenance().span(),
                ));
            }
        };
        let classified = by_id.get(&target.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", target.index()),
                span: Some(action.provenance().span()),
            }
        })?;
        let mut lowerer = ExpressionLowerer::new(view, by_id, pre_names);
        require_periodic_trigger(view, action.trigger(), clock, action.provenance().span())?;
        let guard = lower_action_guard(
            view,
            action.guard(),
            clock,
            &mut lowerer,
            action.provenance().span(),
        )?;
        let mut assignments = Vec::with_capacity(classified.variable.scalar_count());
        for indices in row_major_indices(classified.variable.value_type().dimensions()) {
            let lowered = lowerer.lower_element(value, &indices)?;
            let value = coerce(lowered, classified.scalar_type, action.provenance().span())?;
            assignments.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: state_reference_indexed(
                        classified.name.clone(),
                        &indices,
                        action.provenance().span(),
                    ),
                    value,
                },
                action.provenance().span(),
            ));
        }
        let statements = match guard {
            Some(condition) => vec![gast::Spanned::new(
                gast::Statement::If(gast::IfStatement {
                    branches: vec![gast::IfBranch {
                        condition: gast::Condition::Expression(condition),
                        body: assignments,
                        span: action.provenance().span(),
                    }],
                    else_body: None,
                }),
                action.provenance().span(),
            )],
            None => assignments,
        };
        let mut reads = HashSet::new();
        collect_current_reads(view, value, &mut reads);
        collect_condition_current_reads(view, action.trigger(), &mut reads);
        collect_condition_current_reads(view, action.guard(), &mut reads);
        let assignment = PendingAssignment {
            targets: vec![target],
            reads,
            statements,
            span: action.provenance().span(),
        };
        if let Some(&owner) = owners.get(&target.index()) {
            pending[owner].reads.extend(assignment.reads);
            pending[owner].statements.extend(assignment.statements);
        } else {
            owners.insert(target.index(), pending.len());
            pending.push(assignment);
        }
    }
    Ok(())
}

fn lower_discrete_value_owners<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    pending: &mut Vec<PendingAssignment<'dae>>,
) -> Result<(), GalecTargetError> {
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        pending.push(lower_discrete_value_owner(
            view, clock, by_id, pre_names, owner,
        )?);
    }
    Ok(())
}

fn lower_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    clock: dae::ClockId<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<PendingAssignment<'dae>, GalecTargetError> {
    let span = owner.provenance().span();
    let targets = owner
        .targets()
        .iter()
        .map(dae::VariableId::from)
        .collect::<Vec<_>>();
    let classified = targets
        .iter()
        .map(|target| {
            by_id
                .get(&target.index())
                .ok_or_else(|| GalecTargetError::UnknownVariableReference {
                    name: format!("#{}", target.index()),
                    span: Some(span),
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut lowerer = ExpressionLowerer::new(view, by_id, pre_names);
    let mut reads = HashSet::new();
    let mut conditional = Vec::new();
    let mut unconditional = None;
    for branch in owner.branches().iter() {
        let branch_span = branch.provenance().span();
        let assignments = lower_discrete_value_branch(&mut lowerer, &classified, branch)?;
        match branch.activation() {
            dae::DiscreteBranchActivation::Always => unconditional = Some(assignments),
            dae::DiscreteBranchActivation::When { trigger, guard } => {
                require_periodic_trigger(view, trigger, clock, branch_span)?;
                collect_condition_current_reads(view, trigger, &mut reads);
                collect_condition_current_reads(view, guard, &mut reads);
                let condition = lower_action_guard(view, guard, clock, &mut lowerer, branch_span)?
                    .unwrap_or(gast::Expression::Bool(true));
                conditional.push(gast::IfBranch {
                    condition: gast::Condition::Expression(condition),
                    body: assignments,
                    span: branch_span,
                });
            }
        }
        for (value, _) in branch.values().iter() {
            collect_current_reads(view, value, &mut reads);
        }
    }
    let statements = if let Some(assignments) = unconditional {
        assignments
    } else {
        vec![gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: conditional,
                else_body: None,
            }),
            span,
        )]
    };
    Ok(PendingAssignment {
        targets,
        reads,
        statements,
        span,
    })
}

fn lower_discrete_value_branch<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    targets: &[&ClassifiedVariable<'dae>],
    branch: dae::DiscreteValueBranchView<'dae>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let mut assignments = Vec::new();
    for (classified, (value, provenance)) in targets.iter().zip(branch.values().iter()) {
        let span = provenance.span();
        for indices in row_major_indices(classified.variable.value_type().dimensions()) {
            let lowered = lowerer.lower_element(value, &indices)?;
            let value = coerce(lowered, classified.scalar_type, span)?;
            assignments.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: state_reference_indexed(classified.name.clone(), &indices, span),
                    value,
                },
                span,
            ));
        }
    }
    Ok(assignments)
}

fn collect_current_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    reads: &mut HashSet<u32>,
) {
    dae::for_each_expression(view, expression, |_, node| {
        let id = match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Input(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(id)) => {
                Some(dae::VariableId::from(id))
            }
            _ => None,
        };
        if let Some(id) = id {
            reads.insert(id.index());
        }
    });
}

fn collect_condition_current_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    reads: &mut HashSet<u32>,
) {
    let mut pending = vec![root];
    let mut seen = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Initial | dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                collect_current_reads(view, expression, reads);
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_current_reads(view, expression, reads);
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs) | dae::ConditionOperation::Or(lhs, rhs) => {
                pending.extend([lhs, rhs]);
            }
        }
    }
}

fn require_periodic_trigger<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    span: Span,
) -> Result<(), GalecTargetError> {
    let mut seen = HashSet::new();
    if condition_requires_clock(view, root, expected, &mut seen) {
        return Ok(());
    }
    Err(unsupported(
        "runtime-event-trigger",
        "a conditional assignment is not owned by the admitted periodic DoStep clock".to_owned(),
        span,
    ))
}

fn condition_requires_clock<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    seen: &mut HashSet<u32>,
) -> bool {
    if !seen.insert(condition.index()) {
        return false;
    }
    match view
        .condition(condition)
        .expect("checked condition identity resolves")
        .operation()
    {
        dae::ConditionOperation::Clock(found) => found == expected,
        dae::ConditionOperation::And(lhs, rhs) => {
            let mut lhs_seen = seen.clone();
            let mut rhs_seen = seen.clone();
            condition_requires_clock(view, lhs, expected, &mut lhs_seen)
                || condition_requires_clock(view, rhs, expected, &mut rhs_seen)
        }
        dae::ConditionOperation::Or(lhs, rhs) => {
            let mut lhs_seen = seen.clone();
            let mut rhs_seen = seen.clone();
            condition_requires_clock(view, lhs, expected, &mut lhs_seen)
                && condition_requires_clock(view, rhs, expected, &mut rhs_seen)
        }
        dae::ConditionOperation::Initial
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_) => false,
    }
}

fn order_assignments<'dae>(
    pending: Vec<PendingAssignment<'dae>>,
) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    let targets = pending
        .iter()
        .enumerate()
        .flat_map(|(index, assignment)| {
            assignment
                .targets
                .iter()
                .map(move |target| (target.index(), index))
        })
        .collect::<HashMap<_, _>>();
    let mut emitted = vec![false; pending.len()];
    let mut emitted_owners = 0usize;
    let mut ordered = Vec::with_capacity(pending.len());
    while emitted_owners < pending.len() {
        let Some(index) = pending.iter().enumerate().position(|(index, assignment)| {
            !emitted[index]
                && assignment.reads.iter().all(|read| {
                    targets
                        .get(read)
                        .is_none_or(|dependency| *dependency == index || emitted[*dependency])
                })
        }) else {
            let span = pending
                .iter()
                .enumerate()
                .find(|(index, _)| !emitted[*index])
                .expect("unfinished ordering has one unemitted owner")
                .1
                .span;
            return Err(unsupported(
                "discrete-algebraic-loop",
                "clocked assignments contain a current-tick dependency cycle".to_owned(),
                span,
            ));
        };
        emitted[index] = true;
        emitted_owners += 1;
        ordered.extend(pending[index].statements.iter().cloned());
    }
    Ok(ordered)
}

#[cfg(test)]
mod tests {
    use rumoca_core::{ClockLattice, ClockRational, SourceId, SourceMap, Span, TypeId, VarName};

    use super::*;

    fn at(source: SourceId, text: &str, needle: &str) -> dae::DaeProvenance {
        let start = text.find(needle).expect("test source contains snippet");
        dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
            .expect("test provenance is exact")
    }

    fn enclosed(source: SourceId, text: &str, first: &str, last: &str) -> dae::DaeProvenance {
        let start = text.find(first).expect("test source contains owner start");
        let end = text.find(last).expect("test source contains owner end") + last.len();
        dae::DaeProvenance::source(Span::from_offsets(source, start, end))
            .expect("test owner provenance is exact")
    }

    fn project(model: &dae::Dae) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
        model.inspect(|view| {
            let classified = classify_variables(view).expect("test variables are classifiable");
            let by_id = classified
                .iter()
                .map(|variable| (variable.id.index(), variable.clone()))
                .collect::<HashMap<_, _>>();
            let clock = admitted_clock_id(view).expect("test has one periodic clock");
            lower_clocked_assignments(view, clock, &by_id, &HashMap::new())
        })
    }

    fn periodic_clock<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ClockId<'dae>, dae::DaeConstructionError> {
        dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO)
                    .expect("test lattice is valid"),
                provenance,
            )
        })
    }

    fn assignment_target(statement: &gast::Spanned<gast::Statement>) -> &str {
        let gast::Statement::Assignment {
            target: gast::Reference::State(parts),
            ..
        } = &statement.node
        else {
            panic!("expected one state assignment")
        };
        parts
            .first()
            .expect("checked state reference is nonempty")
            .name
            .lexeme()
    }

    #[derive(Clone, Copy)]
    struct AtomicOwnerSpans {
        m_declaration: dae::DaeProvenance,
        n_declaration: dae::DaeProvenance,
        owner: dae::DaeProvenance,
        first_branch: dae::DaeProvenance,
        second_branch: dae::DaeProvenance,
        m_true: dae::DaeProvenance,
        n_false: dae::DaeProvenance,
        m_false: dae::DaeProvenance,
        n_true: dae::DaeProvenance,
    }

    struct AtomicOwnerValues<'dae> {
        first_condition: dae::ExprId<'dae>,
        second_condition: dae::ExprId<'dae>,
        first_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
        second_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
    }

    struct AtomicOwnerDefinition<'dae> {
        targets: [dae::DiscreteValueId<'dae>; 2],
        first: dae::ConditionId<'dae>,
        second: dae::ConditionId<'dae>,
        first_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
        second_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
        spans: AtomicOwnerSpans,
    }

    fn atomic_owner_spans(source: SourceId, text: &str) -> AtomicOwnerSpans {
        AtomicOwnerSpans {
            m_declaration: at(source, text, "discrete Boolean m"),
            n_declaration: at(source, text, "discrete Boolean n"),
            owner: enclosed(source, text, "when sample(0, 1)", "end when"),
            first_branch: at(source, text, "sample(0, 1) and a"),
            second_branch: at(source, text, "sample(0, 1) and b"),
            m_true: at(source, text, "m = true"),
            n_false: at(source, text, "n = false"),
            m_false: at(source, text, "m = false"),
            n_true: at(source, text, "n = true"),
        }
    }

    fn atomic_owner_values<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        spans: AtomicOwnerSpans,
    ) -> Result<AtomicOwnerValues<'dae>, dae::DaeConstructionError> {
        dae.expressions(|expressions| {
            Ok(AtomicOwnerValues {
                first_condition: expressions
                    .at(spans.first_branch)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                second_condition: expressions
                    .at(spans.second_branch)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                first_values: [
                    (
                        expressions
                            .at(spans.m_true)
                            .literal(dae::DaeLiteral::Boolean(true))?,
                        spans.m_true,
                    ),
                    (
                        expressions
                            .at(spans.n_false)
                            .literal(dae::DaeLiteral::Boolean(false))?,
                        spans.n_false,
                    ),
                ],
                second_values: [
                    (
                        expressions
                            .at(spans.m_false)
                            .literal(dae::DaeLiteral::Boolean(false))?,
                        spans.m_false,
                    ),
                    (
                        expressions
                            .at(spans.n_true)
                            .literal(dae::DaeLiteral::Boolean(true))?,
                        spans.n_true,
                    ),
                ],
            })
        })
    }

    fn atomic_owner_conditions<'dae>(
        dae: &mut dae::DaeConstruction<'dae>,
        clock: dae::ClockId<'dae>,
        values: &AtomicOwnerValues<'dae>,
        spans: AtomicOwnerSpans,
    ) -> Result<(dae::ConditionId<'dae>, dae::ConditionId<'dae>), dae::DaeConstructionError> {
        dae.conditions(|conditions| {
            let tick = conditions.reserve(spans.first_branch)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), spans.first_branch)?;
            let first_condition = conditions.reserve(spans.first_branch)?;
            conditions.define(
                first_condition,
                dae::ConditionInput::Discrete(values.first_condition),
                spans.first_branch,
            )?;
            let second_condition = conditions.reserve(spans.second_branch)?;
            conditions.define(
                second_condition,
                dae::ConditionInput::Discrete(values.second_condition),
                spans.second_branch,
            )?;
            let first = conditions.reserve(spans.first_branch)?;
            conditions.define(
                first,
                dae::ConditionInput::And(tick, first_condition),
                spans.first_branch,
            )?;
            let second = conditions.reserve(spans.second_branch)?;
            conditions.define(
                second,
                dae::ConditionInput::And(tick, second_condition),
                spans.second_branch,
            )?;
            Ok((first, second))
        })
    }

    fn define_atomic_owner<'dae>(
        topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
        definition: AtomicOwnerDefinition<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        topology.owner(definition.spans.owner, definition.targets, |owner| {
            owner.when(
                definition.first,
                definition.first,
                definition.spans.first_branch,
                definition.first_values,
            )?;
            owner.when(
                definition.second,
                definition.second,
                definition.spans.second_branch,
                definition.second_values,
            )
        })?;
        Ok(())
    }

    fn atomic_owner_model(
        sources: SourceMap,
        spans: AtomicOwnerSpans,
    ) -> Result<dae::Dae, dae::DaeConstructionError> {
        dae::Dae::construct(sources, |dae| {
            let boolean = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    spans.m_declaration,
                )
            })?;
            let (m, n) = dae.variables(|variables| {
                Ok((
                    variables.discrete_value(
                        VarName::new("m"),
                        boolean,
                        spans.m_declaration,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.discrete_value(
                        VarName::new("n"),
                        boolean,
                        spans.n_declaration,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let values = atomic_owner_values(dae, spans)?;
            let clock = periodic_clock(dae, spans.first_branch)?;
            let (first, second) = atomic_owner_conditions(dae, clock, &values, spans)?;
            dae.clocks(|clocks| {
                clocks.own_discrete_value(clock, m, spans.owner)?;
                clocks.own_discrete_value(clock, n, spans.owner)?;
                Ok(())
            })?;
            dae.b1c([m, n], |topology| {
                define_atomic_owner(
                    topology,
                    AtomicOwnerDefinition {
                        targets: [m, n],
                        first,
                        second,
                        first_values: values.first_values,
                        second_values: values.second_values,
                        spans,
                    },
                )
            })
        })
    }

    fn assert_atomic_owner_projection(
        statements: &[gast::Spanned<gast::Statement>],
        spans: AtomicOwnerSpans,
    ) {
        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].span, spans.owner.span());
        let gast::Statement::If(conditional) = &statements[0].node else {
            panic!("conditional B.1c owner must remain atomic")
        };
        assert_eq!(conditional.branches.len(), 2);
        assert_eq!(conditional.branches[0].span, spans.first_branch.span());
        assert_eq!(conditional.branches[1].span, spans.second_branch.span());
        assert_eq!(
            conditional.branches[0]
                .body
                .iter()
                .map(assignment_target)
                .collect::<Vec<_>>(),
            ["m", "n"]
        );
        assert_eq!(
            conditional.branches[0]
                .body
                .iter()
                .map(|statement| statement.span)
                .collect::<Vec<_>>(),
            [spans.m_true.span(), spans.n_false.span()]
        );
        assert_eq!(
            conditional.branches[1]
                .body
                .iter()
                .map(|statement| statement.span)
                .collect::<Vec<_>>(),
            [spans.m_false.span(), spans.n_true.span()]
        );
    }

    fn define_always_owner<'dae>(
        topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
        target: dae::DiscreteValueId<'dae>,
        value: dae::ExprId<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        topology.owner(provenance, [target], |owner| {
            owner.always(provenance, [(value, provenance)])
        })?;
        Ok(())
    }

    fn define_when_owner<'dae>(
        topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
        target: dae::DiscreteValueId<'dae>,
        condition: dae::ConditionId<'dae>,
        branch: dae::DaeProvenance,
        value: dae::ExprId<'dae>,
        action: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        topology.owner(branch, [target], |owner| {
            owner.when(condition, condition, branch, [(value, action)])
        })?;
        Ok(())
    }

    #[test]
    fn unconditional_owner_lowers_directly_with_action_provenance() {
        let text = "discrete Boolean m; m = true; sample(0, 1);";
        let mut sources = SourceMap::new();
        let source = sources.add("unconditional.mo", text);
        let declaration = at(source, text, "discrete Boolean m");
        let action = at(source, text, "m = true");
        let clock_at = at(source, text, "sample(0, 1)");
        let model = dae::Dae::construct(sources, |dae| {
            let boolean = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    declaration,
                )
            })?;
            let m = dae.variables(|variables| {
                variables.discrete_value(
                    VarName::new("m"),
                    boolean,
                    declaration,
                    dae::VariableAttributes::default(),
                )
            })?;
            let value = dae.expressions(|expressions| {
                expressions
                    .at(action)
                    .literal(dae::DaeLiteral::Boolean(true))
            })?;
            periodic_clock(dae, clock_at)?;
            dae.b1c([m], |topology| {
                topology.owner(action, [m], |owner| owner.always(action, [(value, action)]))?;
                Ok(())
            })
        })
        .expect("checked unconditional B.1c fixture");

        let statements = project(&model).expect("unconditional owner projects");
        assert_eq!(statements.len(), 1);
        assert_eq!(statements[0].span, action.span());
        assert_eq!(assignment_target(&statements[0]), "m");
        assert!(matches!(
            &statements[0].node,
            gast::Statement::Assignment {
                value: gast::Expression::Bool(true),
                ..
            }
        ));
    }

    #[test]
    fn ordered_multi_target_owner_is_atomic_and_wire_stable() {
        let text = "discrete Boolean m; discrete Boolean n; when sample(0, 1) and a then m = true; n = false; elsewhen sample(0, 1) and b then m = false; n = true; end when;";
        let mut sources = SourceMap::new();
        let source = sources.add("elsewhen.mo", text);
        let spans = atomic_owner_spans(source, text);
        let model = atomic_owner_model(sources, spans).expect("checked multi-target B.1c fixture");

        let statements = project(&model).expect("multi-target owner projects");
        assert_atomic_owner_projection(&statements, spans);
        let encoded = serde_json::to_string(&model).expect("wire encoding succeeds");
        let decoded: dae::Dae = serde_json::from_str(&encoded).expect("wire decoding succeeds");
        assert_eq!(
            statements,
            project(&decoded).expect("wire-decoded owner projects identically")
        );
    }

    #[test]
    fn current_discrete_value_dependencies_keep_topological_owner_order() {
        let text = "discrete Boolean a; discrete Boolean b; a = true; b = a; sample(0, 1);";
        let mut sources = SourceMap::new();
        let source = sources.add("topology.mo", text);
        let a_declaration = at(source, text, "discrete Boolean a");
        let b_declaration = at(source, text, "discrete Boolean b");
        let a_action = at(source, text, "a = true");
        let b_action = at(source, text, "b = a");
        let clock_at = at(source, text, "sample(0, 1)");
        let model = dae::Dae::construct(sources, |dae| {
            let boolean = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    a_declaration,
                )
            })?;
            let (a, b) = dae.variables(|variables| {
                Ok((
                    variables.discrete_value(
                        VarName::new("a"),
                        boolean,
                        a_declaration,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.discrete_value(
                        VarName::new("b"),
                        boolean,
                        b_declaration,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let (true_value, a_value) = dae.expressions(|expressions| {
                Ok((
                    expressions
                        .at(a_action)
                        .literal(dae::DaeLiteral::Boolean(true))?,
                    expressions
                        .at(b_action)
                        .coordinate(dae::CoordinateInput::DiscreteValue(a))?,
                ))
            })?;
            periodic_clock(dae, clock_at)?;
            dae.b1c([a, b], |topology| {
                define_always_owner(topology, a, true_value, a_action)?;
                define_always_owner(topology, b, a_value, b_action)?;
                Ok(())
            })
        })
        .expect("checked topological B.1c fixture");

        let statements = project(&model).expect("topological owners project");
        assert_eq!(
            statements.iter().map(assignment_target).collect::<Vec<_>>(),
            ["a", "b"]
        );
        assert_eq!(
            statements
                .iter()
                .map(|statement| statement.span)
                .collect::<Vec<_>>(),
            [a_action.span(), b_action.span()]
        );
    }

    #[test]
    fn unclocked_conditional_owner_is_rejected_at_its_source_span() {
        let text = "discrete Boolean m; when a then m = true; end when; sample(0, 1);";
        let mut sources = SourceMap::new();
        let source = sources.add("unclocked.mo", text);
        let declaration = at(source, text, "discrete Boolean m");
        let branch = at(source, text, "when a");
        let action = at(source, text, "m = true");
        let clock_at = at(source, text, "sample(0, 1)");
        let model = dae::Dae::construct(sources, |dae| {
            let boolean = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    declaration,
                )
            })?;
            let m = dae.variables(|variables| {
                variables.discrete_value(
                    VarName::new("m"),
                    boolean,
                    declaration,
                    dae::VariableAttributes::default(),
                )
            })?;
            let (condition_value, assigned_value) = dae.expressions(|expressions| {
                Ok((
                    expressions
                        .at(branch)
                        .literal(dae::DaeLiteral::Boolean(true))?,
                    expressions
                        .at(action)
                        .literal(dae::DaeLiteral::Boolean(true))?,
                ))
            })?;
            let condition = dae.conditions(|conditions| {
                let condition = conditions.reserve(branch)?;
                conditions.define(
                    condition,
                    dae::ConditionInput::Discrete(condition_value),
                    branch,
                )?;
                Ok(condition)
            })?;
            periodic_clock(dae, clock_at)?;
            dae.b1c([m], |topology| {
                define_when_owner(topology, m, condition, branch, assigned_value, action)?;
                Ok(())
            })
        })
        .expect("checked unclocked B.1c fixture");

        let error = project(&model).expect_err("unclocked condition is outside DoStep");
        assert!(matches!(
            error,
            GalecTargetError::UnsupportedFeature {
                feature,
                span: Some(span),
                ..
            } if feature == "runtime-event-trigger" && span == branch.span()
        ));
    }
}
