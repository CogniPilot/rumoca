use super::*;

#[derive(Clone, Copy)]
pub(super) struct AlgorithmStatementContext<'scope, 'shape, 'dae> {
    pub(super) coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    pub(super) functions: &'scope FunctionRegistry<'shape, 'dae>,
    pub(super) values: &'scope HashMap<VarName, dae::ExprId<'dae>>,
    pub(super) parent: Option<EventGuard<'dae>>,
    pub(super) owner_span: Span,
}

pub(super) struct AlgorithmFunctionCall<'source> {
    pub(super) component: &'source rumoca_core::Reference,
    pub(super) arguments: &'source [Expression],
    pub(super) outputs: &'source [Option<rumoca_core::ComponentReference>],
    pub(super) span: Span,
    pub(super) plan: &'source ModelEventFunctionCallPlan,
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

pub(super) fn lower_algorithm_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    discrete_values: &mut DiscreteValueStaging<'dae>,
    discrete_owner: Option<DiscreteValueOwnerHandle>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    component: &rumoca_core::ComponentReference,
    value: &Expression,
    span: Span,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    let guard = statement_guard(construction, context)?;
    let target = rumoca_core::component_ref_to_base_reference(component)
        .var_name()
        .clone();
    let provenance = dae::DaeProvenance::source(span)?;
    if let Some(&target_coordinate) = context.coordinates.get(&target) {
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
                span,
            )?;
            let symbols = LoweringSymbols {
                coordinates: context.coordinates,
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
            target_coordinate,
            guard,
            value,
            provenance,
        )?;
        return Ok(vec![(target, value)]);
    }
    let pairs = structured_assignment_names(&target, value, context.coordinates.keys())
        .expect("algorithm analysis proves structured assignment leaves");
    let value_span = value
        .span()
        .expect("algorithm analysis proves assignment-value provenance");
    let mut updates = Vec::with_capacity(pairs.len());
    for (target_leaf, source_leaf) in pairs {
        let source_provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::DiscreteUpdate, value_span)?;
        let source = construction.expressions(|expressions| {
            match context.values.get(&source_leaf).copied() {
                Some(value) => Ok(value),
                None => expressions
                    .at(source_provenance)
                    .coordinate(context.coordinates[&source_leaf].current()),
            }
        })?;
        updates.push((target_leaf.clone(), source));
        lower_when_assignment(
            construction,
            discrete_values,
            discrete_owner,
            context.coordinates[&target_leaf],
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
    coordinate: Coordinate<'dae>,
    provenance: dae::DaeProvenance,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(value) = context.values.get(target).copied() {
        return Ok(value);
    }
    let previous = coordinate
        .previous(span)
        .expect("event analysis accepts only historical discrete tensor targets");
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
    for (ordinal, (output, plan)) in call.outputs.iter().zip(&call.plan.outputs).enumerate() {
        let (Some(_), Some(plan)) = (output, plan) else {
            continue;
        };
        let value = construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .call(function, ordinal, arguments.iter().copied())
        })?;
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
    plan: &ModelEventTensorLoopPlan,
    statements: &[rumoca_core::Statement],
    span: Span,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
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
    let mut updates = Vec::with_capacity(plan.targets.len());
    let mut loop_values = context.values.clone();
    for (target, statement) in plan.targets.iter().zip(statements) {
        let rumoca_core::Statement::Assignment {
            value,
            span: assignment_span,
            ..
        } = statement
        else {
            unreachable!("event analysis proves tensor-loop assignments")
        };
        let body = lower_scoped_model_algorithm_expression(
            construction,
            context.coordinates,
            context.functions,
            &loop_values,
            guard.owner_clock,
            &binders,
            value,
        )?;
        let value_span = value
            .span()
            .expect("event analysis proves tensor-loop value provenance");
        let value_provenance = dae::DaeProvenance::source(value_span)?;
        let tensor = construction.expressions(|expressions| {
            expressions.at(value_provenance).comprehension(domain, body)
        })?;
        lower_when_assignment(
            construction,
            discrete_values,
            discrete_owner,
            context.coordinates[target],
            guard,
            tensor,
            dae::DaeProvenance::source(*assignment_span)?,
        )?;
        loop_values.insert(target.clone(), tensor);
        updates.push((target.clone(), tensor));
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
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clock: dae::ClockId<'dae>,
    function_calls: &HashMap<Span, ModelEventFunctionCallPlan>,
    statements: &[rumoca_core::Statement],
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                let target = rumoca_core::component_ref_to_base_reference(comp)
                    .var_name()
                    .clone();
                if let Some(&coordinate) = coordinates.get(&target) {
                    own_clocked_coordinate(construction, clock, coordinate, *span)?;
                    continue;
                }
                let targets = structured_assignment_names(&target, value, coordinates.keys())
                    .expect("algorithm analysis proves structured assignment leaves");
                for (target, _) in targets {
                    own_clocked_coordinate(construction, clock, coordinates[&target], *span)?;
                }
            }
            rumoca_core::Statement::FunctionCall { outputs, span, .. } => {
                let plan = &function_calls[span];
                debug_assert_eq!(outputs.len(), plan.outputs.len());
                for output in plan.outputs.iter().flatten() {
                    own_clocked_function_output(construction, coordinates, clock, output, *span)?;
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    own_clocked_algorithm_targets(
                        construction,
                        coordinates,
                        clock,
                        function_calls,
                        &block.stmts,
                    )?;
                }
                if let Some(statements) = else_block {
                    own_clocked_algorithm_targets(
                        construction,
                        coordinates,
                        clock,
                        function_calls,
                        statements,
                    )?;
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    own_clocked_algorithm_targets(
                        construction,
                        coordinates,
                        clock,
                        function_calls,
                        &block.stmts,
                    )?;
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                own_clocked_algorithm_targets(
                    construction,
                    coordinates,
                    clock,
                    function_calls,
                    equations,
                )?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn own_clocked_function_output<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clock: dae::ClockId<'dae>,
    output: &ModelEventFunctionOutputPlan,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    match output {
        ModelEventFunctionOutputPlan::Coordinate(target) => {
            own_clocked_coordinate(construction, clock, coordinates[target], span)
        }
        ModelEventFunctionOutputPlan::Record(fields) => {
            for field in fields {
                own_clocked_coordinate(construction, clock, coordinates[&field.target], span)?;
            }
            Ok(())
        }
    }
}

fn own_clocked_coordinate<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    clock: dae::ClockId<'dae>,
    coordinate: Coordinate<'dae>,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(span)?;
    construction.clocks(|clocks| match coordinate {
        Coordinate::DiscreteReal(variable) => {
            clocks.own_discrete_real(clock, variable, provenance)?;
            Ok(())
        }
        Coordinate::DiscreteValue(variable) => {
            clocks.own_discrete_value(clock, variable, provenance)?;
            Ok(())
        }
        _ => unreachable!("algorithm analysis accepts only discrete event targets"),
    })
}
