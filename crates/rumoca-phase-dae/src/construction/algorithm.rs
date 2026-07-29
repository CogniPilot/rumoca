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
        let value = lower_expression(
            construction,
            context.coordinates,
            context.functions,
            value,
            None,
        )?;
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
        .map(|argument| {
            lower_expression(
                construction,
                context.coordinates,
                context.functions,
                argument,
                None,
            )
        })
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
