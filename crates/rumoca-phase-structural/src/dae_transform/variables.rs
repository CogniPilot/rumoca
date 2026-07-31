//! Reserve and define the rebuilt variables, carrying the demotion decision.
//!
//! Reservation is where a state demotion actually happens: the selected state
//! is reserved as an algebraic variable while every other variable keeps its
//! source role. [`TargetVariable`] records the role each source variable
//! landed in so later stages can translate a coordinate without re-deriving
//! the decision, and definition replays the full attribute set once the
//! rebuilt expressions that back the attributes exist.

use rumoca_ir_dae as dae;

pub(super) enum TargetVariable<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
}

impl Copy for TargetVariable<'_> {}

impl Clone for TargetVariable<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

pub(super) struct ReservedVariable<'dae> {
    pub(super) identity: TargetVariable<'dae>,
    reservation: Option<dae::VariableReservation<'dae>>,
}

pub(super) fn reserve_variables<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    types: &[dae::ValueTypeId<'target>],
    demoted: Option<u32>,
) -> Result<Vec<ReservedVariable<'target>>, dae::DaeConstructionError> {
    target.variables(|variables| {
        source
            .variables()
            .map(|(_, variable)| {
                reserve_variable(
                    variables,
                    variable,
                    types[variable.value_type_id().index() as usize],
                    demoted,
                )
            })
            .collect()
    })
}

fn reserve_variable<'target>(
    variables: &mut dae::Variables<'_, 'target>,
    variable: dae::VariableView<'_>,
    value_type: dae::ValueTypeId<'target>,
    demoted: Option<u32>,
) -> Result<ReservedVariable<'target>, dae::DaeConstructionError> {
    let name = variable.name().clone();
    let declaration = variable.declaration();
    let (identity, reservation) = match variable.role() {
        dae::VariableRole::Parameter => {
            let (id, reservation) = variables.reserve_parameter(name, value_type, declaration)?;
            (TargetVariable::Parameter(id), reservation)
        }
        dae::VariableRole::Constant => {
            let (id, reservation) = variables.reserve_constant(name, value_type, declaration)?;
            (TargetVariable::Parameter(id), reservation)
        }
        dae::VariableRole::Input => {
            let variability = input_variability(variable.variability());
            let (id, reservation) =
                variables.reserve_input(name, value_type, variability, declaration)?;
            (TargetVariable::Input(id), reservation)
        }
        dae::VariableRole::State if Some(variable.id().index()) == demoted => {
            let (id, reservation) = variables.reserve_algebraic(name, value_type, declaration)?;
            (TargetVariable::Algebraic(id), reservation)
        }
        dae::VariableRole::State => {
            let (id, reservation) = variables.reserve_state(name, value_type, declaration)?;
            (TargetVariable::State(id), reservation)
        }
        dae::VariableRole::Algebraic => {
            let (id, reservation) = variables.reserve_algebraic(name, value_type, declaration)?;
            (TargetVariable::Algebraic(id), reservation)
        }
        dae::VariableRole::Output => {
            let (id, reservation) = variables.reserve_output(name, value_type, declaration)?;
            (TargetVariable::Algebraic(id), reservation)
        }
        dae::VariableRole::DiscreteReal => {
            let (id, reservation) =
                variables.reserve_discrete_real(name, value_type, declaration)?;
            (TargetVariable::DiscreteReal(id), reservation)
        }
        dae::VariableRole::DiscreteValue => {
            let (id, reservation) =
                variables.reserve_discrete_value(name, value_type, declaration)?;
            (TargetVariable::DiscreteValue(id), reservation)
        }
    };
    Ok(ReservedVariable {
        identity,
        reservation: Some(reservation),
    })
}

fn input_variability(variability: dae::ExpressionVariability) -> dae::InputVariability {
    if variability == dae::ExpressionVariability::Continuous {
        dae::InputVariability::Continuous
    } else {
        dae::InputVariability::Discrete
    }
}

pub(super) fn define_variables<'target>(
    source: dae::DaeView<'_>,
    target: &mut dae::DaeConstruction<'target>,
    expressions: &[dae::ExprId<'target>],
    variables: &mut [ReservedVariable<'target>],
) -> Result<(), dae::DaeConstructionError> {
    target.variables(|target| {
        for ((_, source), reserved) in source.variables().zip(variables) {
            let expression = |id: dae::ExprId<'_>| expressions[id.index() as usize];
            let attributes = dae::VariableAttributes {
                component_ref: source.component_reference().cloned(),
                binding: source.binding().map(expression),
                start: source.start().map(expression),
                fixed: source.fixed(),
                min: source.minimum().map(expression),
                max: source.maximum().map(expression),
                nominal: source.nominal().map(expression),
                unit: source.unit().map(str::to_owned),
                state_select: source.state_select(),
                description: source.description().map(str::to_owned),
                causality: source.causality(),
                is_tunable: source.is_tunable(),
                is_held: source.is_held(),
                origin: source.origin(),
            };
            target.define(
                reserved
                    .reservation
                    .take()
                    .expect("each variable reservation is consumed exactly once"),
                attributes,
                source.declaration(),
            )?;
        }
        Ok(())
    })
}
