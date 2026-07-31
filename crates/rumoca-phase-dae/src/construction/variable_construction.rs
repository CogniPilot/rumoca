mod plan;

use super::*;

pub(super) use plan::{VariableConstructionPlan, plan_variable_construction};

pub(super) struct VariableIdentityPass<'flat, 'dae> {
    pub(super) coordinates: ModelCoordinates<'dae>,
    pub(super) reserved: Vec<Option<ReservedVariable<'flat, 'dae>>>,
}

pub(super) fn insert_variable_identities<'flat, 'dae>(
    flat: &'flat flat::Model,
    analysis: &Analysis,
    construction: &mut dae::DaeConstruction<'dae>,
    value_types: &HashMap<VarName, dae::ValueTypeId<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    plan: &VariableConstructionPlan,
) -> Result<VariableIdentityPass<'flat, 'dae>, dae::DaeConstructionError> {
    let mut coordinates = ModelCoordinates::new();
    let mut reserved = (0..flat.variables.len()).map(|_| None).collect::<Vec<_>>();
    for (source_ordinal, (name, variable)) in flat.variables.iter().enumerate() {
        let role = analysis.roles[name];
        if matches!(role, PlannedRole::Clock) {
            continue;
        }
        if matches!(
            role,
            PlannedRole::EnumerationLiteral | PlannedRole::Aggregate
        ) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(variable.source_span)?;
        let value_type = value_types[name];
        let scalar_type = effective_variable_scalar_type(flat, variable)
            .expect("analysis accepts only primitive value types");
        if !plan
            .variable(source_ordinal)
            .requires_reservation(source_ordinal)
        {
            let coordinate = insert_complete_variable(
                construction,
                VariableDefinitionContext {
                    coordinates: &coordinates,
                    functions,
                    assigned_discrete_targets: &analysis.assigned_discrete_targets,
                    derived_parameters: &analysis.derived_parameters,
                    initial_parameters: &analysis.initial_parameters,
                },
                VariableSpec {
                    flat: variable,
                    role,
                    scalar_type,
                    value_type,
                },
            )?;
            coordinates.insert(variable, coordinate);
            continue;
        }
        let (coordinate, definition) =
            reserve_variable_identity(construction, variable, role, value_type, provenance)?;
        coordinates.insert(variable, coordinate);
        reserved[source_ordinal] = Some(ReservedVariable {
            flat: variable,
            role,
            scalar_type,
            value_type,
            definition,
        });
    }
    Ok(VariableIdentityPass {
        coordinates,
        reserved,
    })
}

fn reserve_variable_identity<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    variable: &flat::Variable,
    role: PlannedRole,
    value_type: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(Coordinate<'dae>, dae::VariableReservation<'dae>), dae::DaeConstructionError> {
    construction.variables(|variables| match role {
        PlannedRole::Parameter => {
            let (id, definition) =
                variables.reserve_parameter(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::Parameter(id), definition))
        }
        PlannedRole::Constant => {
            let (id, definition) =
                variables.reserve_constant(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::Parameter(id), definition))
        }
        PlannedRole::Input => {
            let (id, definition) = variables.reserve_input(
                variable.name.clone(),
                value_type,
                planned_input_variability(variable),
                provenance,
            )?;
            Ok((Coordinate::Input(id), definition))
        }
        PlannedRole::State => {
            let (id, definition) =
                variables.reserve_state(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::State(id), definition))
        }
        PlannedRole::Algebraic => {
            let (id, definition) =
                variables.reserve_algebraic(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::Algebraic(id), definition))
        }
        PlannedRole::Output => {
            let (id, definition) =
                variables.reserve_output(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::Algebraic(id), definition))
        }
        PlannedRole::DiscreteReal => {
            let (id, definition) =
                variables.reserve_discrete_real(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::DiscreteReal(id), definition))
        }
        PlannedRole::DiscreteValue => {
            let (id, definition) =
                variables.reserve_discrete_value(variable.name.clone(), value_type, provenance)?;
            Ok((Coordinate::DiscreteValue(id), definition))
        }
        PlannedRole::EnumerationLiteral | PlannedRole::Aggregate => {
            unreachable!("expression-only roles are never reserved as variables")
        }
        PlannedRole::Clock => unreachable!("clock variables live in the clock arena"),
    })
}

pub(super) fn define_reserved_variables<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    plan: &VariableConstructionPlan,
    mut reserved: Vec<Option<ReservedVariable<'_, 'dae>>>,
) -> Result<(), dae::DaeConstructionError> {
    for component in plan.definition_components() {
        for &source_ordinal in &component.members {
            let Some(reserved) = reserved[source_ordinal].take() else {
                continue;
            };
            define_reserved_variable(construction, context, reserved)?;
        }
    }
    debug_assert!(reserved.iter().all(Option::is_none));
    Ok(())
}

/// Everything a variable definition reads besides the variable itself.
#[derive(Clone, Copy)]
pub(super) struct VariableDefinitionContext<'scope, 'dae> {
    pub(super) coordinates: &'scope HashMap<VarName, Coordinate<'dae>>,
    pub(super) functions: &'scope FunctionRegistry<'scope, 'dae>,
    pub(super) assigned_discrete_targets: &'scope HashSet<VarName>,
    pub(super) derived_parameters: &'scope HashMap<VarName, DerivedParameterPlan>,
    /// `fixed = false` parameters an initial algorithm determines (MLS §8.6).
    pub(super) initial_parameters: &'scope HashMap<VarName, Expression>,
}

#[derive(Clone, Copy)]
struct VariableSpec<'flat, 'dae> {
    flat: &'flat flat::Variable,
    role: PlannedRole,
    scalar_type: dae::ScalarType,
    value_type: dae::ValueTypeId<'dae>,
}

fn define_reserved_variable<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    reserved: ReservedVariable<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let declaration = dae::DaeProvenance::source(reserved.flat.source_span)?;
    let attributes = lower_variable_attributes(
        construction,
        context,
        VariableSpec {
            flat: reserved.flat,
            role: reserved.role,
            scalar_type: reserved.scalar_type,
            value_type: reserved.value_type,
        },
    )?;
    construction
        .variables(|variables| variables.define(reserved.definition, attributes, declaration))
}

fn insert_complete_variable<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    variable: VariableSpec<'_, 'dae>,
) -> Result<Coordinate<'dae>, dae::DaeConstructionError> {
    let declaration = dae::DaeProvenance::source(variable.flat.source_span)?;
    let attributes = lower_variable_attributes(construction, context, variable)?;
    construction.variables(|variables| match variable.role {
        PlannedRole::Parameter => variables
            .parameter(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Parameter),
        PlannedRole::Constant => variables
            .constant(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Parameter),
        PlannedRole::Input => variables
            .input(
                variable.flat.name.clone(),
                variable.value_type,
                planned_input_variability(variable.flat),
                declaration,
                attributes,
            )
            .map(Coordinate::Input),
        PlannedRole::State => variables
            .state(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::State),
        PlannedRole::Algebraic => variables
            .algebraic(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Algebraic),
        PlannedRole::Output => variables
            .output(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Algebraic),
        PlannedRole::DiscreteReal => variables
            .discrete_real(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::DiscreteReal),
        PlannedRole::DiscreteValue => variables
            .discrete_value(
                variable.flat.name.clone(),
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::DiscreteValue),
        PlannedRole::Clock | PlannedRole::EnumerationLiteral | PlannedRole::Aggregate => {
            unreachable!("expression-only roles are never inserted as variables")
        }
    })
}

fn lower_variable_attributes<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    variable: VariableSpec<'_, 'dae>,
) -> Result<dae::VariableAttributes<'dae>, dae::DaeConstructionError> {
    let binding = lower_variable_binding(construction, context, variable)?;
    let start = match variable.flat.start.as_ref() {
        Some(start) => Some(lower_variable_attribute_expression(
            construction,
            context,
            variable,
            start,
        )?),
        None if matches!(
            variable.role,
            PlannedRole::State
                | PlannedRole::Algebraic
                | PlannedRole::Output
                | PlannedRole::DiscreteReal
                | PlannedRole::DiscreteValue
        ) =>
        {
            Some(default_start_expression(
                construction,
                variable.scalar_type,
                variable.flat.source_span,
            )?)
        }
        None => None,
    };
    let min = lower_optional_variable_attribute(
        construction,
        context,
        variable,
        variable.flat.min.as_ref(),
    )?;
    let max = lower_optional_variable_attribute(
        construction,
        context,
        variable,
        variable.flat.max.as_ref(),
    )?;
    let nominal = lower_optional_variable_attribute(
        construction,
        context,
        variable,
        variable.flat.nominal.as_ref(),
    )?;
    let derived_parameter = context.derived_parameters.contains_key(&variable.flat.name)
        || context.initial_parameters.contains_key(&variable.flat.name);
    let causality = if derived_parameter {
        dae::VariableCausality::CalculatedParameter
    } else {
        variable_causality(variable.flat, variable.role)
    };
    Ok(dae::VariableAttributes {
        component_ref: variable.flat.component_ref.clone(),
        binding,
        start,
        fixed: variable.flat.fixed,
        min,
        max,
        nominal,
        unit: variable.flat.unit.clone(),
        state_select: variable.flat.state_select,
        description: variable.flat.description.clone(),
        causality,
        is_tunable: matches!(variable.role, PlannedRole::Parameter)
            && !derived_parameter
            && !variable.flat.evaluate,
        is_held: matches!(
            variable.role,
            PlannedRole::DiscreteReal | PlannedRole::DiscreteValue
        ) && variable.flat.binding.is_none()
            && !context
                .assigned_discrete_targets
                .contains(&variable.flat.name),
        origin: dae::VariableOrigin::Source,
    })
}

fn lower_variable_binding<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    variable: VariableSpec<'_, 'dae>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if let Some(plan) = context.derived_parameters.get(&variable.flat.name) {
        return lower_derived_parameter_binding(
            construction,
            context.coordinates,
            context.functions,
            plan,
        )
        .map(Some);
    }
    if let Some(value) = context.initial_parameters.get(&variable.flat.name) {
        return lower_variable_attribute_expression(construction, context, variable, value)
            .map(Some);
    }
    if !matches!(
        variable.role,
        PlannedRole::Parameter | PlannedRole::Constant | PlannedRole::Input
    ) {
        return Ok(None);
    }
    lower_optional_variable_attribute(
        construction,
        context,
        variable,
        variable.flat.binding.as_ref(),
    )
}

fn lower_optional_variable_attribute<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    variable: VariableSpec<'_, 'dae>,
    expression: Option<&Expression>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    expression
        .map(|expression| {
            lower_variable_attribute_expression(construction, context, variable, expression)
        })
        .transpose()
}

/// Lower one declared attribute of `variable`.
///
/// An empty array literal owns no element expression, so its checked value type
/// comes from the declaration it is bound to (MLS §10.4); every other attribute
/// expression derives its own type from its operands.
fn lower_variable_attribute_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    variable: VariableSpec<'_, 'dae>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(span) = empty_array_bound_to_declaration(variable.flat, expression) {
        let provenance = dae::DaeProvenance::source(span)?;
        return construction.expressions(|expressions| {
            expressions.at(provenance).empty_array(variable.value_type)
        });
    }
    lower_attribute_expression(
        construction,
        context.coordinates,
        context.functions,
        expression,
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
        .is_some_and(|reference| reference.parts().len() == 1);
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
    if scalar_type == dae::ScalarType::Enumeration {
        return construction
            .expressions(|expressions| expressions.at(provenance).enumeration_literal(1));
    }
    let literal = match scalar_type {
        dae::ScalarType::Real => dae::DaeLiteral::Real(0.0),
        dae::ScalarType::Integer => dae::DaeLiteral::Integer(0),
        dae::ScalarType::Enumeration => unreachable!("enumeration default handled above"),
        dae::ScalarType::Boolean => dae::DaeLiteral::Boolean(false),
        dae::ScalarType::String => dae::DaeLiteral::String(String::new()),
        dae::ScalarType::Record => {
            return Err(dae::DaeConstructionError::ShapeMismatch { span: owner_span });
        }
    };
    construction.expressions(|expressions| expressions.at(provenance).literal(literal))
}
