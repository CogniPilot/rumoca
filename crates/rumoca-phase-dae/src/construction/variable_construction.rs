mod plan;

use super::*;

use plan::{IssuedReservations, ReservationRequirement};
pub(super) use plan::{VariableConstructionPlan, plan_variable_construction};

pub(super) struct VariableIdentityPass<'flat, 'dae> {
    pub(super) coordinates: ModelCoordinates<'dae>,
    pub(super) reserved: IssuedReservations<ReservedVariable<'flat, 'dae>>,
}

pub(super) fn insert_variable_identities<'flat, 'dae>(
    flat: &'flat flat::Model,
    analysis: &Analysis<'_>,
    construction: &mut dae::DaeConstruction<'dae>,
    value_types: &HashMap<VarName, dae::ValueTypeId<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    plan: VariableConstructionPlan,
) -> Result<VariableIdentityPass<'flat, 'dae>, dae::DaeConstructionError> {
    let mut coordinates = ModelCoordinates::new();
    let mut reservations = plan.into_reservation_issuer();
    for (source_ordinal, (name, variable)) in flat.variables.iter().enumerate() {
        let reservation_requirement =
            reservations.next_source(source_ordinal, variable.source_span)?;
        let Some(planned_role) = analysis.roles.get(name) else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: variable.source_span,
            });
        };
        let Some(role) = planned_role.runtime() else {
            if reservation_requirement == ReservationRequirement::Reserve {
                return Err(dae::DaeConstructionError::InvalidExpressionForm {
                    span: variable.source_span,
                });
            }
            continue;
        };
        let provenance = dae::DaeProvenance::source(variable.source_span)?;
        let value_type = value_types.get(name).copied().ok_or(
            dae::DaeConstructionError::InvalidExpressionForm {
                span: variable.source_span,
            },
        )?;
        let scalar_type = effective_variable_scalar_type(flat, variable).ok_or(
            dae::DaeConstructionError::InvalidExpressionForm {
                span: variable.source_span,
            },
        )?;
        if reservation_requirement == ReservationRequirement::Complete {
            let coordinate = insert_complete_variable(
                construction,
                VariableDefinitionContext {
                    flat,
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
            coordinates.insert(variable, coordinate)?;
            continue;
        }
        let (coordinate, definition) =
            reserve_variable_identity(construction, variable, role, value_type, provenance)?;
        coordinates.insert(variable, coordinate)?;
        reservations.issue(
            ReservedVariable {
                flat: variable,
                role,
                scalar_type,
                value_type,
                definition,
            },
            variable.source_span,
        )?;
    }
    Ok(VariableIdentityPass {
        coordinates,
        reserved: reservations.finish()?,
    })
}

fn reserve_variable_identity<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    variable: &flat::Variable,
    role: RuntimeVariableRole,
    value_type: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(Coordinate<'dae>, dae::VariableReservation<'dae>), dae::DaeConstructionError> {
    construction.variables(|variables| match role {
        RuntimeVariableRole::Parameter => {
            let (id, definition) = variables.reserve_parameter(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::Parameter(id), definition))
        }
        RuntimeVariableRole::Constant => {
            let (id, definition) = variables.reserve_constant(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::Parameter(id), definition))
        }
        RuntimeVariableRole::Input => {
            let (id, definition) = variables.reserve_input(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                planned_input_variability(variable),
                provenance,
            )?;
            Ok((Coordinate::Input(id), definition))
        }
        RuntimeVariableRole::State => {
            let (id, definition) = variables.reserve_state(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::State(id), definition))
        }
        RuntimeVariableRole::Algebraic => {
            let (id, definition) = variables.reserve_algebraic(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::Algebraic(id), definition))
        }
        RuntimeVariableRole::Output => {
            let (id, definition) = variables.reserve_output(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::Algebraic(id), definition))
        }
        RuntimeVariableRole::DiscreteReal => {
            let (id, definition) = variables.reserve_discrete_real(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::DiscreteReal(id), definition))
        }
        RuntimeVariableRole::DiscreteValue => {
            let (id, definition) = variables.reserve_discrete_value(
                variable.name.clone(),
                variable.instance_id,
                value_type,
                provenance,
            )?;
            Ok((Coordinate::DiscreteValue(id), definition))
        }
    })
}

pub(super) fn define_reserved_variables<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    context: VariableDefinitionContext<'_, 'dae>,
    reserved: IssuedReservations<ReservedVariable<'_, 'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for reservation in reserved.into_definitions() {
        define_reserved_variable(construction, context, reservation)?;
    }
    Ok(())
}

/// Everything a variable definition reads besides the variable itself.
#[derive(Clone, Copy)]
pub(super) struct VariableDefinitionContext<'scope, 'dae> {
    pub(super) flat: &'scope flat::Model,
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
    role: RuntimeVariableRole,
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
        RuntimeVariableRole::Parameter => variables
            .parameter(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Parameter),
        RuntimeVariableRole::Constant => variables
            .constant(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Parameter),
        RuntimeVariableRole::Input => variables
            .input(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                planned_input_variability(variable.flat),
                declaration,
                attributes,
            )
            .map(Coordinate::Input),
        RuntimeVariableRole::State => variables
            .state(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::State),
        RuntimeVariableRole::Algebraic => variables
            .algebraic(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Algebraic),
        RuntimeVariableRole::Output => variables
            .output(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::Algebraic),
        RuntimeVariableRole::DiscreteReal => variables
            .discrete_real(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::DiscreteReal),
        RuntimeVariableRole::DiscreteValue => variables
            .discrete_value(
                variable.flat.name.clone(),
                variable.flat.instance_id,
                variable.value_type,
                declaration,
                attributes,
            )
            .map(Coordinate::DiscreteValue),
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
        None if needs_default_start(variable) => Some(default_start_expression(
            construction,
            variable.scalar_type,
            variable.flat.source_span,
        )?),
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
        variable_causality(variable.flat, variable.role, context.flat)
    };
    Ok(dae::VariableAttributes {
        component_ref: variable.flat.component_ref.clone(),
        binding,
        start,
        // Only the explicit source spelling crosses this boundary. An
        // omitted `fixed` stays absent here; the DAE variable definition
        // decides the MLS 3.6 section 4.8.1 role default exactly once.
        fixed: variable.flat.fixed.map(rumoca_core::Fixity::from),
        min,
        max,
        nominal,
        unit: variable.flat.unit.clone(),
        state_select: variable.flat.state_select,
        description: variable.flat.description.clone(),
        causality,
        is_tunable: matches!(variable.role, RuntimeVariableRole::Parameter)
            && !derived_parameter
            && !variable.flat.evaluate,
        is_held: matches!(
            variable.role,
            RuntimeVariableRole::DiscreteReal | RuntimeVariableRole::DiscreteValue
        ) && variable.flat.binding.is_none()
            && !context
                .assigned_discrete_targets
                .contains(&variable.flat.name),
        origin: dae::VariableOrigin::Source,
    })
}

/// Whether DAE construction must materialize the predefined type's `start`.
///
/// Runtime variables always need their checked initialization guess. A
/// `fixed = false` parameter needs the same guess because MLS §8.6 makes it an
/// initialization unknown; the initialization projection remains the owner of
/// its value. Parameters with the default `fixed = true` stay out of this arm:
/// a default `start` is not a declaration binding and must not make an unbound
/// fixed parameter look evaluable.
fn needs_default_start(variable: VariableSpec<'_, '_>) -> bool {
    matches!(
        variable.role,
        RuntimeVariableRole::State
            | RuntimeVariableRole::Algebraic
            | RuntimeVariableRole::Output
            | RuntimeVariableRole::DiscreteReal
            | RuntimeVariableRole::DiscreteValue
    ) || (matches!(variable.role, RuntimeVariableRole::Parameter)
        && variable.flat.fixed == Some(false))
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
        RuntimeVariableRole::Parameter | RuntimeVariableRole::Constant | RuntimeVariableRole::Input
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
        record_staging: None,
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

fn variable_causality(
    variable: &flat::Variable,
    role: RuntimeVariableRole,
    model: &flat::Model,
) -> dae::VariableCausality {
    let top_level_port = variable.component_ref.as_ref().is_some_and(|reference| {
        reference.parts().len() == 1
            || reference
                .parts()
                .first()
                .is_some_and(|root| model.top_level_connectors.contains(&root.ident))
    });
    match (&variable.causality, role, top_level_port) {
        (Causality::Input(_), RuntimeVariableRole::Input, true) => dae::VariableCausality::Input,
        (Causality::Output(_), _, true) => dae::VariableCausality::Output,
        (_, RuntimeVariableRole::Parameter, _) => dae::VariableCausality::Parameter,
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
        dae::ScalarType::Enumeration => {
            return construction
                .expressions(|expressions| expressions.at(provenance).enumeration_literal(1));
        }
        dae::ScalarType::Boolean => dae::DaeLiteral::Boolean(false),
        dae::ScalarType::String => dae::DaeLiteral::String(String::new()),
        dae::ScalarType::Record => {
            return Err(dae::DaeConstructionError::ShapeMismatch { span: owner_span });
        }
    };
    construction.expressions(|expressions| expressions.at(provenance).literal(literal))
}
