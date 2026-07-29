use super::*;

#[derive(Clone, Copy)]
pub(super) struct AlgorithmStatementContext<'scope, 'shape, 'dae> {
    pub(super) coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    pub(super) functions: &'scope FunctionRegistry<'shape, 'dae>,
    pub(super) parent: Option<EventGuard<'dae>>,
    pub(super) owner_span: Span,
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
            })
        }
    }
}

pub(super) fn lower_algorithm_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    component: &rumoca_core::ComponentReference,
    value: &Expression,
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let guard = statement_guard(construction, context)?;
    let target = component.to_var_name();
    let provenance = dae::DaeProvenance::source(span)?;
    if let Some(&target_coordinate) = context.coordinates.get(&target) {
        let value = lower_algorithm_expression(construction, context, value)?;
        return lower_when_assignment(construction, target_coordinate, guard, value, provenance);
    }
    let pairs = structured_assignment_names(&target, value, context.coordinates.keys())
        .expect("algorithm analysis proves structured assignment leaves");
    let value_span = value
        .span()
        .expect("algorithm analysis proves assignment-value provenance");
    for (target_leaf, source_leaf) in pairs {
        let source_provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::DiscreteUpdate, value_span)?;
        let source = construction.expressions(|expressions| {
            expressions
                .at(source_provenance)
                .coordinate(context.coordinates[&source_leaf].current())
        })?;
        lower_when_assignment(
            construction,
            context.coordinates[&target_leaf],
            guard,
            source,
            provenance,
        )?;
    }
    Ok(())
}

pub(super) fn lower_algorithm_function_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    component: &rumoca_core::ComponentReference,
    arguments: &[Expression],
    outputs: &[Option<rumoca_core::ComponentReference>],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let guard = statement_guard(construction, context)?;
    let function_reference = rumoca_core::Reference::from_component_reference(component.clone());
    let function = context.functions.select(
        &function_reference,
        arguments,
        context.functions.shapes.model_values(),
        span,
    );
    let arguments = arguments
        .iter()
        .map(|argument| lower_algorithm_expression(construction, context, argument))
        .collect::<Result<Vec<_>, _>>()?;
    let provenance = dae::DaeProvenance::source(span)?;
    for (ordinal, output) in outputs.iter().enumerate() {
        let Some(output) = output else {
            continue;
        };
        let value = construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .call(function, ordinal, arguments.iter().copied())
        })?;
        lower_when_assignment(
            construction,
            context.coordinates[&output.to_var_name()],
            guard,
            value,
            provenance,
        )?;
    }
    Ok(())
}

fn lower_algorithm_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: AlgorithmStatementContext<'_, '_, 'dae>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match context.parent.and_then(|guard| guard.owner_clock) {
        Some(clock) => lower_clocked_expression(
            construction,
            context.coordinates,
            context.functions,
            clock,
            expression,
        ),
        None => lower_expression(
            construction,
            context.coordinates,
            context.functions,
            expression,
            None,
        ),
    }
}

pub(super) fn own_clocked_algorithm_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clock: dae::ClockId<'dae>,
    statements: &[rumoca_core::Statement],
) -> Result<(), dae::DaeConstructionError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                let target = comp.to_var_name();
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
                for output in outputs.iter().flatten() {
                    own_clocked_coordinate(
                        construction,
                        clock,
                        coordinates[&output.to_var_name()],
                        *span,
                    )?;
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    own_clocked_algorithm_targets(construction, coordinates, clock, &block.stmts)?;
                }
                if let Some(statements) = else_block {
                    own_clocked_algorithm_targets(construction, coordinates, clock, statements)?;
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    own_clocked_algorithm_targets(construction, coordinates, clock, &block.stmts)?;
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                own_clocked_algorithm_targets(construction, coordinates, clock, equations)?;
            }
            _ => {}
        }
    }
    Ok(())
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
