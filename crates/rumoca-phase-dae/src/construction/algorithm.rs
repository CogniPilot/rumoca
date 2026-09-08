use super::analysis::StructuredAssignmentPlan;
use super::*;

#[derive(Clone, Copy)]
pub(super) struct AlgorithmStatementContext<'scope, 'shape, 'dae> {
    pub(super) coordinates: &'scope ModelCoordinates<'dae>,
    pub(super) functions: &'scope FunctionRegistry<'shape, 'dae>,
    pub(super) values: &'scope HashMap<VarName, dae::ExprId<'dae>>,
    pub(super) parent: Option<EventGuard<'dae>>,
    pub(super) owner_span: Span,
}

pub(super) struct AlgorithmFunctionCall<'source> {
    pub(super) component: &'source rumoca_core::Reference,
    pub(super) arguments: &'source [Expression],
    pub(super) span: Span,
    pub(super) plan: &'source ModelEventFunctionCallPlan,
}

pub(super) struct AlgorithmAssignment<'source> {
    pub(super) component: &'source rumoca_core::ComponentReference,
    pub(super) value: &'source Expression,
    pub(super) span: Span,
    pub(super) structured_plan: Option<&'source StructuredAssignmentPlan>,
}

struct AlgorithmCallTarget<'scope, 'shape, 'dae> {
    context: AlgorithmStatementContext<'scope, 'shape, 'dae>,
    guard: EventGuard<'dae>,
    target: &'scope VarName,
    value: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
    projection: &'scope [usize],
}

fn statement_guard<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
) -> Result<EventGuard<'dae>, dae::DaeConstructionError> {
    match context.parent {
        Some(guard) => Ok(guard),
        None => {
            let always = always_condition(construction, context.owner_span)?;
            Ok(EventGuard {
                trigger: always,
                condition: always,
                owner_clock: None,
                branch_provenance: dae::DaeProvenance::generated(
                    dae::DaeGeneration::AlgorithmEquation,
                    context.owner_span,
                )?,
                always: true,
                parent_activation: None,
            })
        }
    }
}

/// The expression one field of a structured assignment reads.
///
/// Split out so the sample a field reads is decided in one place: a
/// `Previous` source must reach the previous-sample coordinate, never
/// `context.values`, which holds what this activation has already assigned.
fn structured_source_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: &AlgorithmStatementContext<'_, '_, 'dae>,
    structured_source: &StructuredSource,
    owner_clock: Option<dae::PeriodicClockId<'dae>>,
    provenance: dae::DaeProvenance,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let source_leaf = structured_source.name();
    match structured_source {
        StructuredSource::Previous(leaf) => {
            let coordinate =
                context
                    .coordinates
                    .event_occurrence(leaf.identity, source_leaf, span)?;
            let owner_clock =
                owner_clock.ok_or(dae::DaeConstructionError::MissingPreviousClockOwner { span })?;
            let previous = construction.temporal(|temporal| match coordinate {
                EventCoordinate::Real(variable) => {
                    temporal.previous_discrete_real(owner_clock.into(), variable, provenance)
                }
                EventCoordinate::Value(variable) => {
                    temporal.previous_discrete_value(owner_clock.into(), variable, provenance)
                }
            })?;
            construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Previous(previous))
            })
        }
        StructuredSource::Current(leaf) => {
            let coordinate =
                context
                    .coordinates
                    .readable_occurrence(leaf.identity, source_leaf, span)?;
            construction.expressions(
                |expressions| match context.values.get(source_leaf).copied() {
                    Some(value) => Ok(value),
                    None => expressions.at(provenance).coordinate(coordinate.current()),
                },
            )
        }
    }
}

pub(super) fn lower_algorithm_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    assignment: AlgorithmAssignment<'_>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let AlgorithmAssignment {
        component,
        value,
        span,
        structured_plan,
    } = assignment;
    let guard = statement_guard(construction, context)?;
    let target = rumoca_core::component_ref_to_base_reference(component)
        .var_name()
        .clone();
    let provenance = dae::DaeProvenance::source(span)?;
    if context.coordinates.get(&target).is_some() {
        let target_coordinate = context.coordinates.event(&target, span)?;
        let value = lower_algorithm_expression(construction, context, value)?;
        let subscripts = component
            .parts()
            .iter()
            .flat_map(|part| part.subs.iter())
            .cloned()
            .collect::<Vec<_>>();
        let value = if subscripts.is_empty() {
            value
        } else {
            let base = algorithm_assignment_base(
                construction,
                context,
                &target,
                target_coordinate,
                provenance,
            )?;
            let symbols = LoweringSymbols {
                coordinates: context.coordinates,
                record_staging: None,
                functions: context.functions,
                shapes: context.functions.shapes.model_values(),
                function_body: None,
                values: Some(context.values),
                owner_clock: guard.owner_clock,
            };
            lower_array_update(
                construction,
                symbols,
                &HashMap::new(),
                base,
                &subscripts,
                value,
                provenance,
            )?
        };
        lower_when_assignment(
            construction,
            discrete_values,
            discrete_owner,
            target_coordinate.coordinate(),
            guard,
            value,
            provenance,
        )?;
        return Ok(vec![(target, value)]);
    }
    let plan = structured_plan.ok_or(dae::DaeConstructionError::InvalidExpressionForm { span })?;
    let value_span = value
        .span()
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span })?;
    let mut updates = Vec::with_capacity(plan.pairs.len());
    for (target_leaf, structured_source) in &plan.pairs {
        let source_provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::DiscreteUpdate, value_span)?;
        let source = structured_source_expression(
            construction,
            &context,
            structured_source,
            guard.owner_clock,
            source_provenance,
            value_span,
        )?;
        updates.push((target_leaf.name.clone(), source));
        let target_coordinate =
            context
                .coordinates
                .event_occurrence(target_leaf.identity, &target_leaf.name, span)?;
        lower_when_assignment(
            construction,
            discrete_values,
            discrete_owner,
            target_coordinate.coordinate(),
            guard,
            source,
            provenance,
        )?;
    }
    Ok(updates)
}

fn algorithm_assignment_base<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    target: &VarName,
    coordinate: EventCoordinate<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(value) = context.values.get(target).copied() {
        return Ok(value);
    }
    let previous = coordinate.previous();
    construction.expressions(|expressions| expressions.at(provenance).coordinate(previous))
}

pub(super) fn lower_algorithm_function_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    call: AlgorithmFunctionCall<'_>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let guard = statement_guard(construction, context)?;
    let function = context.functions.select(
        call.component,
        call.arguments,
        context.functions.shapes.model_values(),
        call.span,
    )?;
    let arguments = call
        .arguments
        .iter()
        .map(|argument| lower_algorithm_expression(construction, context, argument))
        .collect::<Result<Vec<_>, _>>()?;
    let provenance = dae::DaeProvenance::source(call.span)?;
    let mut updates = Vec::new();
    let selected = call
        .plan
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, plan)| plan.as_ref().map(|plan| (ordinal, plan)))
        .collect::<Vec<_>>();
    let values = construction.expressions(|expressions| {
        expressions.at(provenance).call_results(
            function,
            selected.iter().map(|(ordinal, _)| *ordinal),
            arguments.iter().copied(),
        )
    })?;
    for ((_, plan), value) in selected.into_iter().zip(values) {
        match plan {
            ModelEventFunctionOutputPlan::Coordinate(target) => {
                lower_algorithm_call_target(
                    construction,
                    discrete_values,
                    discrete_owner,
                    AlgorithmCallTarget {
                        context,
                        guard,
                        target,
                        value,
                        provenance,
                        projection: &[],
                    },
                    &mut updates,
                )?;
            }
            ModelEventFunctionOutputPlan::Record(fields) => {
                for field in fields {
                    lower_algorithm_call_target(
                        construction,
                        discrete_values,
                        discrete_owner,
                        AlgorithmCallTarget {
                            context,
                            guard,
                            target: &field.target,
                            value,
                            provenance,
                            projection: &field.projection,
                        },
                        &mut updates,
                    )?;
                }
            }
        }
    }
    Ok(updates)
}

fn lower_algorithm_call_target<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    target: AlgorithmCallTarget<'_, '_, 'dae>,
    updates: &mut Vec<(VarName, dae::ExprId<'dae>)>,
) -> Result<(), dae::DaeConstructionError> {
    let mut value = target.value;
    for ordinal in target.projection {
        value = construction
            .expressions(|expressions| expressions.at(target.provenance).field(value, *ordinal))?;
    }
    lower_when_assignment(
        construction,
        discrete_values,
        discrete_owner,
        target.context.coordinates[target.target],
        target.guard,
        value,
        target.provenance,
    )?;
    updates.push((target.target.clone(), value));
    Ok(())
}

pub(super) fn lower_algorithm_tensor_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    plan: &ModelEventTensorLoopPlan<'_>,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let span = plan.span;
    let owner = dae::DaeProvenance::source(span)?;
    let domain = construction.domains(|domains| domains.structured(plan.domain.clone(), owner))?;
    let mut binders = HashMap::with_capacity(plan.binder_spans.len());
    for (ordinal, (binder, binder_span)) in plan
        .domain
        .binders
        .iter()
        .zip(&plan.binder_spans)
        .enumerate()
    {
        let provenance = dae::DaeProvenance::source(*binder_span)?;
        let id = construction.domains(|domains| domains.binder(domain, ordinal, provenance))?;
        binders.insert(VarName::new(&binder.display_name), id);
    }
    let guard = statement_guard(construction, context)?;
    let mut updates = Vec::with_capacity(plan.assignments.len());
    let mut loop_values = context.values.clone();
    for assignment in &plan.assignments {
        let body = lower_scoped_model_algorithm_expression(
            construction,
            context.coordinates,
            context.functions,
            &loop_values,
            guard.owner_clock,
            &binders,
            assignment.value,
        )?;
        let value_span =
            assignment
                .value
                .span()
                .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                    span: assignment.span,
                })?;
        let value_provenance = dae::DaeProvenance::source(value_span)?;
        let tensor = construction.expressions(|expressions| {
            expressions.at(value_provenance).comprehension(domain, body)
        })?;
        lower_when_assignment(
            construction,
            discrete_values,
            discrete_owner,
            context
                .coordinates
                .event(&assignment.target, assignment.span)?
                .coordinate(),
            guard,
            tensor,
            dae::DaeProvenance::source(assignment.span)?,
        )?;
        loop_values.insert(assignment.target.clone(), tensor);
        updates.push((assignment.target.clone(), tensor));
    }
    Ok(updates)
}

fn lower_algorithm_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match context.parent.and_then(|guard| guard.owner_clock) {
        Some(clock) => lower_clocked_model_algorithm_expression(
            construction,
            context.coordinates,
            context.functions,
            context.values,
            clock,
            expression,
        ),
        None => lower_model_algorithm_expression(
            construction,
            context.coordinates,
            context.functions,
            context.values,
            expression,
        ),
    }
}

pub(super) fn own_clocked_algorithm_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &ModelCoordinates<'dae>,
    clock: dae::ClockId<'dae>,
    statements: &[EventStatementPlan<'_>],
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        match statement {
            EventStatementPlan::Assignment {
                component,
                span,
                route,
                ..
            } => {
                if let EventAssignmentRoute::FunctionCall { plan, .. } = route {
                    own_clocked_function_outputs(
                        construction,
                        coordinates,
                        clock,
                        &plan.outputs,
                        *span,
                    )?;
                    continue;
                }
                let target = rumoca_core::component_ref_to_base_reference(component)
                    .var_name()
                    .clone();
                match route {
                    EventAssignmentRoute::Coordinate => {
                        let coordinate = coordinates.event(&target, *span)?;
                        own_clocked_coordinate(construction, clock, coordinate, *span)?;
                    }
                    EventAssignmentRoute::Structured(plan) => {
                        own_clocked_structured_targets(
                            construction,
                            coordinates,
                            clock,
                            plan,
                            *span,
                        )?;
                    }
                    EventAssignmentRoute::FunctionCall { .. } => {}
                }
            }
            EventStatementPlan::FunctionCall { span, plan, .. } => {
                own_clocked_function_outputs(
                    construction,
                    coordinates,
                    clock,
                    &plan.outputs,
                    *span,
                )?;
            }
            EventStatementPlan::If {
                blocks,
                else_product,
                ..
            } => {
                for block in blocks {
                    own_clocked_algorithm_targets(
                        construction,
                        coordinates,
                        clock,
                        &block.statements,
                    )?;
                }
                if let EventElseProduct::Statements(statements) = else_product {
                    own_clocked_algorithm_targets(construction, coordinates, clock, statements)?;
                }
            }
            EventStatementPlan::When { blocks, .. } => {
                for block in blocks {
                    own_clocked_algorithm_targets(
                        construction,
                        coordinates,
                        clock,
                        &block.statements,
                    )?;
                }
            }
            EventStatementPlan::TensorLoop(plan) => {
                for assignment in &plan.assignments {
                    let coordinate = coordinates.event(&assignment.target, assignment.span)?;
                    own_clocked_coordinate(construction, clock, coordinate, assignment.span)?;
                }
            }
            EventStatementPlan::Assert { .. } => {}
        }
    }
    Ok(())
}

fn own_clocked_structured_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &ModelCoordinates<'dae>,
    clock: dae::ClockId<'dae>,
    plan: &StructuredAssignmentPlan,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    for (target, _) in &plan.pairs {
        let coordinate = coordinates.event_occurrence(target.identity, &target.name, span)?;
        own_clocked_coordinate(construction, clock, coordinate, span)?;
    }
    Ok(())
}

fn own_clocked_function_outputs<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &ModelCoordinates<'dae>,
    clock: dae::ClockId<'dae>,
    outputs: &[Option<ModelEventFunctionOutputPlan>],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    for output in outputs.iter().flatten() {
        own_clocked_function_output(construction, coordinates, clock, output, span)?;
    }
    Ok(())
}

fn own_clocked_function_output<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &ModelCoordinates<'dae>,
    clock: dae::ClockId<'dae>,
    output: &ModelEventFunctionOutputPlan,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    match output {
        ModelEventFunctionOutputPlan::Coordinate(target) => {
            let coordinate = coordinates.event(target, span)?;
            own_clocked_coordinate(construction, clock, coordinate, span)
        }
        ModelEventFunctionOutputPlan::Record(fields) => {
            for field in fields {
                let coordinate = coordinates.event(&field.target, span)?;
                own_clocked_coordinate(construction, clock, coordinate, span)?;
            }
            Ok(())
        }
    }
}

fn own_clocked_coordinate<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    clock: dae::ClockId<'dae>,
    coordinate: EventCoordinate<'dae>,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(span)?;
    construction.clocks(|clocks| match coordinate {
        EventCoordinate::Real(variable) => {
            clocks.own_discrete_real(clock, variable, provenance)?;
            Ok(())
        }
        EventCoordinate::Value(variable) => {
            clocks.own_discrete_value(clock, variable, provenance)?;
            Ok(())
        }
    })
}
