use super::*;

pub(super) fn reserve_variables<'flat, 'dae>(
    flat: &'flat flat::Model,
    analysis: &Analysis,
    construction: &mut dae::DaeConstruction<'dae>,
    value_types: &HashMap<VarName, dae::ValueTypeId<'dae>>,
) -> Result<
    (
        HashMap<VarName, Coordinate<'dae>>,
        Vec<ReservedVariable<'flat, 'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let mut coordinates = HashMap::new();
    let mut reserved = Vec::with_capacity(flat.variables.len());
    for (name, variable) in &flat.variables {
        let role = analysis.roles[name];
        if matches!(role, PlannedRole::Clock) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(variable.source_span)?;
        let value_type = value_types[name];
        let (coordinate, definition) = construction.variables(|variables| match role {
            PlannedRole::Parameter => {
                let (id, definition) =
                    variables.reserve_parameter(name.clone(), value_type, provenance)?;
                Ok((Coordinate::Parameter(id), definition))
            }
            PlannedRole::Constant => {
                let (id, definition) =
                    variables.reserve_constant(name.clone(), value_type, provenance)?;
                Ok((Coordinate::Parameter(id), definition))
            }
            PlannedRole::Input => {
                let (id, definition) = variables.reserve_input(
                    name.clone(),
                    value_type,
                    planned_input_variability(variable),
                    provenance,
                )?;
                Ok((Coordinate::Input(id), definition))
            }
            PlannedRole::State => {
                let (id, definition) =
                    variables.reserve_state(name.clone(), value_type, provenance)?;
                Ok((Coordinate::State(id), definition))
            }
            PlannedRole::Algebraic => {
                let (id, definition) =
                    variables.reserve_algebraic(name.clone(), value_type, provenance)?;
                Ok((Coordinate::Algebraic(id), definition))
            }
            PlannedRole::Output => {
                let (id, definition) =
                    variables.reserve_output(name.clone(), value_type, provenance)?;
                Ok((Coordinate::Algebraic(id), definition))
            }
            PlannedRole::DiscreteReal => {
                let (id, definition) =
                    variables.reserve_discrete_real(name.clone(), value_type, provenance)?;
                Ok((Coordinate::DiscreteReal(id), definition))
            }
            PlannedRole::DiscreteValue => {
                let (id, definition) =
                    variables.reserve_discrete_value(name.clone(), value_type, provenance)?;
                Ok((Coordinate::DiscreteValue(id), definition))
            }
            PlannedRole::EnumerationLiteral | PlannedRole::Aggregate => {
                unreachable!("expression-only roles are never reserved as variables")
            }
            PlannedRole::Clock => unreachable!("clock variables live in the clock arena"),
        })?;
        coordinates.insert(name.clone(), coordinate);
        reserved.push(ReservedVariable {
            flat: variable,
            role,
            scalar_type: effective_variable_scalar_type(&flat.variable_type_names[name], variable)
                .expect("analysis accepts only primitive value types"),
            coordinate,
            definition,
        });
    }
    Ok((coordinates, reserved))
}

pub(super) fn define_variables<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    assigned_discrete_targets: &HashSet<VarName>,
    derived_parameters: &HashMap<VarName, DerivedParameterPlan>,
    reserved: Vec<ReservedVariable<'_, 'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let context = VariableDefinitionContext {
        coordinates,
        functions,
        assigned_discrete_targets,
        derived_parameters,
    };
    for reserved in reserved {
        define_variable(construction, context, reserved)?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct VariableDefinitionContext<'scope, 'dae> {
    coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    functions: &'scope FunctionRegistry<'scope, 'dae>,
    assigned_discrete_targets: &'scope HashSet<VarName>,
    derived_parameters: &'scope HashMap<VarName, DerivedParameterPlan>,
}

fn define_variable<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    reserved: ReservedVariable<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let declaration = dae::DaeProvenance::source(reserved.flat.source_span)?;
    let binding = lower_variable_binding(construction, context, &reserved)?;
    let start = match reserved.flat.start.as_ref() {
        Some(start) => Some(lower_attribute_expression(
            construction,
            context.coordinates,
            context.functions,
            start,
        )?),
        None if matches!(
            reserved.role,
            PlannedRole::State
                | PlannedRole::Algebraic
                | PlannedRole::Output
                | PlannedRole::DiscreteReal
                | PlannedRole::DiscreteValue
        ) =>
        {
            Some(default_start_expression(
                construction,
                reserved.scalar_type,
                reserved.flat.source_span,
            )?)
        }
        None => None,
    };
    let min = lower_optional_attribute_expression(
        construction,
        context.coordinates,
        context.functions,
        reserved.flat.min.as_ref(),
    )?;
    let max = lower_optional_attribute_expression(
        construction,
        context.coordinates,
        context.functions,
        reserved.flat.max.as_ref(),
    )?;
    let nominal = lower_optional_attribute_expression(
        construction,
        context.coordinates,
        context.functions,
        reserved.flat.nominal.as_ref(),
    )?;
    let derived_parameter = context.derived_parameters.contains_key(&reserved.flat.name);
    let causality = if derived_parameter {
        dae::VariableCausality::CalculatedParameter
    } else {
        variable_causality(reserved.flat, reserved.role)
    };
    let attributes = dae::VariableAttributes {
        component_ref: reserved.flat.component_ref.clone(),
        binding,
        start,
        fixed: reserved.flat.fixed,
        min,
        max,
        nominal,
        unit: reserved.flat.unit.clone(),
        state_select: reserved.flat.state_select,
        description: reserved.flat.description.clone(),
        causality,
        is_tunable: matches!(reserved.role, PlannedRole::Parameter)
            && !derived_parameter
            && !reserved.flat.evaluate,
        is_held: matches!(
            reserved.role,
            PlannedRole::DiscreteReal | PlannedRole::DiscreteValue
        ) && reserved.flat.binding.is_none()
            && !context
                .assigned_discrete_targets
                .contains(&reserved.flat.name),
        origin: dae::VariableOrigin::Source,
    };
    construction
        .variables(|variables| variables.define(reserved.definition, attributes, declaration))?;
    let _ = reserved.coordinate;
    Ok(())
}

fn lower_variable_binding<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    reserved: &ReservedVariable<'_, 'dae>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if let Some(plan) = context.derived_parameters.get(&reserved.flat.name) {
        return lower_derived_parameter_binding(
            construction,
            context.coordinates,
            context.functions,
            plan,
        )
        .map(Some);
    }
    if !matches!(
        reserved.role,
        PlannedRole::Parameter | PlannedRole::Constant
    ) {
        return Ok(None);
    }
    lower_optional_attribute_expression(
        construction,
        context.coordinates,
        context.functions,
        reserved.flat.binding.as_ref(),
    )
}

fn lower_derived_parameter_binding<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    plan: &DerivedParameterPlan,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let owner = dae::DaeProvenance::source(plan.owner)?;
    let domain = construction.domains(|domains| domains.structured(plan.domain.clone(), owner))?;
    let mut binders = HashMap::with_capacity(plan.domain.binders.len());
    for (ordinal, binder) in plan.domain.binders.iter().enumerate() {
        let id = construction.domains(|domains| domains.binder(domain, ordinal, owner))?;
        binders.insert(VarName::new(&binder.display_name), id);
    }
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let body = lower_expression_scoped(construction, symbols, &binders, &plan.body, None)?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::DerivedParameterLowering, plan.owner)?;
    construction.expressions(|expressions| expressions.at(generated).comprehension(domain, body))
}

fn variable_causality(variable: &flat::Variable, role: PlannedRole) -> dae::VariableCausality {
    let top_level_port = variable
        .component_ref
        .as_ref()
        .is_some_and(|reference| reference.parts.len() == 1);
    match (&variable.causality, role, top_level_port) {
        (Causality::Input(_), PlannedRole::Input, true) => dae::VariableCausality::Input,
        (Causality::Output(_), _, true) => dae::VariableCausality::Output,
        (_, PlannedRole::Parameter, _) => dae::VariableCausality::Parameter,
        _ => dae::VariableCausality::Local,
    }
}

fn default_start_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    scalar_type: dae::ScalarType,
    owner_span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::DefaultStart, owner_span)?;
    let literal = match scalar_type {
        dae::ScalarType::Real => dae::DaeLiteral::Real(0.0),
        dae::ScalarType::Integer => dae::DaeLiteral::Integer(0),
        dae::ScalarType::Boolean => dae::DaeLiteral::Boolean(false),
        dae::ScalarType::String => dae::DaeLiteral::String(String::new()),
        dae::ScalarType::Record => {
            return Err(dae::DaeConstructionError::ShapeMismatch { span: owner_span });
        }
    };
    construction.expressions(|expressions| expressions.at(provenance).literal(literal))
}
