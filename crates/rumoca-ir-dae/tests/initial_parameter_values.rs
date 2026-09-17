//! Initialization parameter definitions retain type, ownership, and state reads.

use rumoca_core::{SourceMap, Span, VarName};
use rumoca_ir_dae::{
    BinaryOperator, CoordinateInput, Dae, DaeConstructionError, DaeLiteral, DaeProvenance,
    ScalarType, ValueType, VariableAttributes,
};

fn construct(
    fixed: Option<bool>,
    bound: bool,
    duplicate: bool,
) -> Result<Dae, DaeConstructionError> {
    let mut sources = SourceMap::new();
    let source = sources.add("initial_parameter.mo", "initial equation branch = q > 0;");
    let owner = DaeProvenance::source(Span::from_offsets(source, 17, 31)).unwrap();
    Dae::construct(sources, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), owner))?;
        let seed = dae
            .expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Boolean(true)))?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("q"),
                real,
                owner,
                VariableAttributes::default(),
            )
        })?;
        let branch = dae.variables(|variables| {
            variables.parameter(
                VarName::new("branch"),
                boolean,
                owner,
                VariableAttributes {
                    fixed: fixed.map(|value| vec![value]),
                    binding: bound.then_some(seed),
                    ..Default::default()
                },
            )
        })?;
        let value = dae.expressions(|expressions| {
            let q = expressions
                .at(owner)
                .coordinate(CoordinateInput::State(state))?;
            let zero = expressions.at(owner).literal(DaeLiteral::Real(0.0))?;
            expressions
                .at(owner)
                .binary(BinaryOperator::Greater, q, zero)
        })?;
        dae.initialization(|initialization| {
            initialization.parameter_initial_value(branch, value, owner)?;
            if duplicate {
                initialization.parameter_initial_value(branch, value, owner)?;
            }
            Ok(())
        })
    })
}

#[test]
fn initialization_parameter_keeps_its_state_read_and_checked_wire_owner() {
    let dae = construct(Some(false), false, false).unwrap();
    dae.inspect(|view| {
        let definition = view.initial_parameter_values().next().unwrap();
        assert_eq!(
            view.variable(definition.target()).unwrap().name().as_str(),
            "branch"
        );
        assert_eq!(view.initial_parameter_values().len(), 1);
    });
    let wire = serde_json::to_value(&dae).unwrap();
    let decoded: Dae = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(serde_json::to_value(decoded).unwrap(), wire);
}

#[test]
fn initialization_parameter_rejects_competing_fixed_and_binding_owners() {
    for (fixed, bound) in [(None, false), (Some(true), false), (Some(false), true)] {
        assert!(matches!(
            construct(fixed, bound, false),
            Err(DaeConstructionError::InvalidInitialParameter { .. })
        ));
    }
    assert!(construct(Some(false), false, true).is_err());
}

#[test]
fn initialization_parameter_requires_exact_shape_and_a_non_real_target() {
    for scalar in [false, true] {
        let mut sources = SourceMap::new();
        let source = sources.add("initial_parameter.mo", "initial equation p = true;");
        let owner = DaeProvenance::source(Span::from_offsets(source, 0, 26)).unwrap();
        let result = Dae::construct(sources, |dae| {
            let declared = if scalar {
                ValueType::scalar(ScalarType::Real)
            } else {
                ValueType::array(ScalarType::Boolean, [2])
            };
            let ty = dae.types(|types| types.derived(declared, owner))?;
            let parameter = dae.variables(|variables| {
                variables.parameter(
                    VarName::new("p"),
                    ty,
                    owner,
                    VariableAttributes {
                        fixed: Some(vec![false]),
                        ..Default::default()
                    },
                )
            })?;
            let value = dae.expressions(|expressions| {
                expressions.at(owner).literal(DaeLiteral::Boolean(true))
            })?;
            dae.initialization(|initialization| {
                initialization.parameter_initial_value(parameter, value, owner)?;
                Ok(())
            })
        });
        if scalar {
            assert!(
                matches!(
                    result,
                    Err(DaeConstructionError::InvalidInitialParameter { .. })
                ),
                "{result:?}"
            );
        } else {
            assert!(
                matches!(result, Err(DaeConstructionError::ShapeMismatch { .. })),
                "{result:?}"
            );
        }
    }
}

#[test]
fn initialization_parameter_wire_rejects_forged_owners() {
    let dae = construct(Some(false), false, false).unwrap();
    let wire = serde_json::to_value(&dae).unwrap();
    let mut missing = wire.clone();
    missing["storage"]
        .as_object_mut()
        .unwrap()
        .remove("initial_parameter_values");
    assert!(serde_json::from_value::<Dae>(missing).is_err());
    let mut duplicated = wire.clone();
    let definitions = duplicated["storage"]["initial_parameter_values"]
        .as_array_mut()
        .unwrap();
    definitions.push(definitions[0].clone());
    assert!(serde_json::from_value::<Dae>(duplicated).is_err());
    let state = dae.inspect(|view| {
        view.variables()
            .find(|(_, variable)| variable.name().as_str() == "q")
            .unwrap()
            .0
            .index()
    });
    let mut wrong_target = wire.clone();
    wrong_target["storage"]["initial_parameter_values"][0]["target"] = state.into();
    assert!(serde_json::from_value::<Dae>(wrong_target).is_err());
    let real = dae.inspect(|view| {
        let mut state_read = None;
        let definition = view.initial_parameter_values().next().unwrap();
        rumoca_ir_dae::for_each_expression(view, definition.value(), |id, expression| {
            if matches!(
                expression.operation(),
                rumoca_ir_dae::ExpressionOperation::Coordinate(
                    rumoca_ir_dae::CoordinateView::State(_)
                )
            ) {
                state_read = Some(id.index());
            }
        });
        state_read.expect("the defining expression reads the initialization state")
    });
    let mut wrong_value = wire;
    wrong_value["storage"]["initial_parameter_values"][0]["value"] = real.into();
    assert!(serde_json::from_value::<Dae>(wrong_value).is_err());
}
