use rumoca_core::{SourceMap, Span, VarName};
use rumoca_ir_dae::{
    AlgebraicId, Dae, DaeConstructionError, DaeLiteral, DaeProvenance, DiscreteRealId,
    DiscreteValueId, ExprId, ExpressionVariability, InputId, InputVariability, ParameterId,
    ScalarType, StateId, ValueType, ValueTypeId, VariableAttributes, VariableRole, Variables,
};

macro_rules! complete_fixed_roles {
    ($variables:ident, $value_type:ident, $held_start:ident, $at:ident;
     $($method:ident => $id:ident, $name:literal, $attributes:expr);+ $(;)?) => {
        $(let _: $id<'_> = $variables.$method(
            VarName::new($name),
            $value_type,
            $at,
            $attributes,
        )?;)+
    };
}

macro_rules! reserve_fixed_roles {
    ($variables:ident, $value_type:ident, $held_start:ident, $at:ident;
     $($method:ident => $id:ident, $name:literal, $attributes:expr);+ $(;)?) => {
        $(let (_, reservation): ($id<'_>, _) = $variables.$method(
            VarName::new($name),
            $value_type,
            $at,
        )?;
        $variables.define(reservation, $attributes, $at)?;)+
    };
}

fn held<'dae>(start: ExprId<'dae>) -> VariableAttributes<'dae> {
    VariableAttributes {
        start: Some(start),
        is_held: true,
        ..VariableAttributes::default()
    }
}

fn add_complete_roles<'dae>(
    variables: &mut Variables<'_, 'dae>,
    real: ValueTypeId<'dae>,
    boolean: ValueTypeId<'dae>,
    held_start: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<DiscreteValueId<'dae>, DaeConstructionError> {
    complete_fixed_roles! {
        variables, real, held_start, at;
        parameter => ParameterId, "complete_parameter", VariableAttributes::default();
        constant => ParameterId, "complete_constant", VariableAttributes::default();
        state => StateId, "complete_state", VariableAttributes::default();
        algebraic => AlgebraicId, "complete_algebraic", VariableAttributes::default();
        output => AlgebraicId, "complete_output", VariableAttributes::default();
        discrete_real => DiscreteRealId, "complete_discrete_real", VariableAttributes::default();
    }
    let _: InputId<'_> = variables.input(
        VarName::new("complete_input"),
        real,
        InputVariability::Discrete,
        at,
        VariableAttributes::default(),
    )?;
    let discrete_value = variables.discrete_value(
        VarName::new("complete_discrete_value"),
        boolean,
        at,
        held(held_start),
    )?;
    Ok(discrete_value)
}

fn add_reserved_roles<'dae>(
    variables: &mut Variables<'_, 'dae>,
    real: ValueTypeId<'dae>,
    boolean: ValueTypeId<'dae>,
    held_start: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<DiscreteValueId<'dae>, DaeConstructionError> {
    reserve_fixed_roles! {
        variables, real, held_start, at;
        reserve_parameter => ParameterId, "reserved_parameter", VariableAttributes::default();
        reserve_constant => ParameterId, "reserved_constant", VariableAttributes::default();
        reserve_state => StateId, "reserved_state", VariableAttributes::default();
        reserve_algebraic => AlgebraicId, "reserved_algebraic", VariableAttributes::default();
        reserve_output => AlgebraicId, "reserved_output", VariableAttributes::default();
        reserve_discrete_real => DiscreteRealId, "reserved_discrete_real", VariableAttributes::default();
    }
    let (_, input): (InputId<'_>, _) = variables.reserve_input(
        VarName::new("reserved_input"),
        real,
        InputVariability::Continuous,
        at,
    )?;
    variables.define(input, VariableAttributes::default(), at)?;
    let (id, discrete_value): (DiscreteValueId<'_>, _) =
        variables.reserve_discrete_value(VarName::new("reserved_discrete_value"), boolean, at)?;
    variables.define(discrete_value, held(held_start), at)?;
    Ok(id)
}

#[test]
fn complete_and_reserved_role_tables_preserve_typed_semantics() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("variable_roles.mo", "variable role declarations");
    let at = DaeProvenance::source(Span::from_offsets(source, 0, 8)).expect("real source span");
    let dae = Dae::construct(source_map, |dae| {
        let (real, boolean) = dae.types(|types| {
            Ok((
                types.derived(ValueType::scalar(ScalarType::Real), at)?,
                types.derived(ValueType::scalar(ScalarType::Boolean), at)?,
            ))
        })?;
        let held_start =
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Boolean(false)))?;
        let (complete, reserved) = dae.variables(|variables| {
            Ok((
                add_complete_roles(variables, real, boolean, held_start, at)?,
                add_reserved_roles(variables, real, boolean, held_start, at)?,
            ))
        })?;
        let (complete_pre, reserved_pre) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(at)
                    .coordinate(rumoca_ir_dae::CoordinateInput::PreDiscreteValue(complete))?,
                expressions
                    .at(at)
                    .coordinate(rumoca_ir_dae::CoordinateInput::PreDiscreteValue(reserved))?,
            ))
        })?;
        dae.b1c([complete, reserved], |topology| {
            topology.owner(at, [complete], |owner| {
                owner.always(at, [(complete_pre, at)])
            })?;
            topology.owner(at, [reserved], |owner| {
                owner.always(at, [(reserved_pre, at)])
            })?;
            Ok(())
        })
    })
    .expect("all complete and reserved variable roles construct");

    let expected = [
        (VariableRole::Parameter, ExpressionVariability::Parameter),
        (VariableRole::Constant, ExpressionVariability::Constant),
        (VariableRole::State, ExpressionVariability::Continuous),
        (VariableRole::Algebraic, ExpressionVariability::Continuous),
        (VariableRole::Output, ExpressionVariability::Continuous),
        (VariableRole::DiscreteReal, ExpressionVariability::Discrete),
        (VariableRole::Input, ExpressionVariability::Discrete),
        (VariableRole::DiscreteValue, ExpressionVariability::Discrete),
        (VariableRole::Parameter, ExpressionVariability::Parameter),
        (VariableRole::Constant, ExpressionVariability::Constant),
        (VariableRole::State, ExpressionVariability::Continuous),
        (VariableRole::Algebraic, ExpressionVariability::Continuous),
        (VariableRole::Output, ExpressionVariability::Continuous),
        (VariableRole::DiscreteReal, ExpressionVariability::Discrete),
        (VariableRole::Input, ExpressionVariability::Continuous),
        (VariableRole::DiscreteValue, ExpressionVariability::Discrete),
    ];
    dae.inspect(|view| {
        let found = view
            .variables()
            .map(|(_, variable)| (variable.role(), variable.variability()))
            .collect::<Vec<_>>();
        assert_eq!(found, expected);
        assert!(
            view.variables()
                .all(|(_, variable)| variable.declaration() == at)
        );
    });
}
