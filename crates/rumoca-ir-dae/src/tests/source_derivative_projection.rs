use super::*;

#[derive(Clone, Copy)]
enum Selection {
    Literal,
    Parameter,
    Discrete,
    Whole,
    Nested,
    State,
}

fn model(extent: u32, selection: Selection) -> Dae {
    let source = TestSource::new("der(x)[i]");
    let owner = source.source("der(x)[i]", 0);
    Dae::construct(source.map, |dae| {
        let (array, integer) = dae.types(|types| {
            let array = types.intern(
                TypeId::new(0),
                ValueType::array(ScalarType::Real, vec![extent]),
                owner,
            )?;
            let integer = types.intern(
                TypeId::new(1),
                ValueType::scalar(ScalarType::Integer),
                owner,
            )?;
            Ok((array, integer))
        })?;
        let (state, parameter, discrete) = dae.variables(|variables| {
            let state = variables.state(
                VarName::new("x"),
                array,
                owner,
                VariableAttributes::default(),
            )?;
            let parameter = variables.parameter(
                VarName::new("p"),
                integer,
                owner,
                VariableAttributes::default(),
            )?;
            let discrete = variables.discrete_value(
                VarName::new("i"),
                integer,
                owner,
                VariableAttributes::default(),
            )?;
            Ok((state, parameter, discrete))
        })?;
        let discrete_value = dae.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(CoordinateInput::Derivative(state))?;
            let state_value = expressions
                .at(owner)
                .coordinate(CoordinateInput::State(state))?;
            let literal = expressions.at(owner).literal(DaeLiteral::Integer(1))?;
            let parameter = expressions
                .at(owner)
                .coordinate(CoordinateInput::Parameter(parameter))?;
            let discrete = expressions
                .at(owner)
                .coordinate(CoordinateInput::DiscreteValue(discrete))?;
            let index = match selection {
                Selection::Parameter => parameter,
                Selection::Discrete => discrete,
                _ => literal,
            };
            let base = if matches!(selection, Selection::State) {
                state_value
            } else {
                derivative
            };
            let subscript = if matches!(selection, Selection::Whole | Selection::Nested) {
                Subscript::Whole { provenance: owner }
            } else {
                Subscript::Index {
                    expression: index,
                    provenance: owner,
                }
            };
            let selected = expressions.at(owner).index(base, [subscript])?;
            if matches!(selection, Selection::Nested) {
                expressions.at(owner).index(
                    selected,
                    [Subscript::Index {
                        expression: literal,
                        provenance: owner,
                    }],
                )?;
            }
            Ok(literal)
        })?;
        dae.b1c([discrete], |topology| {
            topology.owner(owner, [discrete], |definition| {
                definition.always(owner, [(discrete_value, owner)])
            })?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn source_derivative_projection_uses_only_constant_parameter_or_whole_subscripts() {
    for selection in [Selection::Literal, Selection::Parameter, Selection::Whole] {
        model(4, selection).inspect(|view| {
            let root = view.expression_id(view.expression_count() - 1).unwrap();
            let projection = view
                .source_derivative_projection(root)
                .expect("invariant selection");
            assert_eq!(projection.source(), root);
            assert_eq!(projection.state().index(), 0);
            assert_eq!(projection.selections().len(), 1);
            assert_eq!(projection.selections()[0].subscripts().len(), 1);
            assert!(!projection.selections()[0].parenthesize_base());
            assert_eq!(
                projection.provenance(),
                view.expression(root).unwrap().provenance()
            );
        });
    }
}

#[test]
fn source_derivative_projection_preserves_nested_source_order_and_grouping() {
    model(4, Selection::Nested).inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let projection = view.source_derivative_projection(root).unwrap();
        let [outer, inner] = projection.selections() else {
            panic!("two selections")
        };
        assert!(matches!(
            outer.subscripts().get(0),
            Some(SubscriptView::Whole { .. })
        ));
        assert!(matches!(
            inner.subscripts().get(0),
            Some(SubscriptView::Index { .. })
        ));
        assert!(!outer.parenthesize_base());
        assert!(inner.parenthesize_base());
    });
}

#[test]
fn source_derivative_projection_never_moves_a_time_varying_index_or_plain_state() {
    for selection in [Selection::Discrete, Selection::State] {
        model(4, selection).inspect(|view| {
            let root = view.expression_id(view.expression_count() - 1).unwrap();
            assert!(view.source_derivative_projection(root).is_none());
        });
    }
}

#[test]
fn source_derivative_projection_stays_compact_and_does_not_modify_wire() {
    for extent in [4, 2_000_000] {
        let dae = model(extent, Selection::Literal);
        let before = serde_json::to_value(&dae).unwrap();
        dae.inspect(|view| {
            let root = view.expression_id(view.expression_count() - 1).unwrap();
            let projection = view.source_derivative_projection(root).unwrap();
            assert_eq!(projection.selections().len(), 1);
            assert!(
                view.source_derivative_projection(view.expression_id(0).unwrap())
                    .unwrap()
                    .selections()
                    .is_empty()
            );
        });
        assert_eq!(serde_json::to_value(&dae).unwrap(), before);
    }
}

#[test]
fn source_derivative_projection_does_not_invent_a_record_state_capability() {
    let source = TestSource::new("record state");
    let owner = source.source("record state", 0);
    Dae::construct(source.map, |dae| {
        let record = dae.types(|types| {
            let real = types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), owner)?;
            types.record(VarName::new("R"), [(VarName::new("x"), real)], owner)
        })?;
        let refused = dae.variables(|variables| {
            variables.state(
                VarName::new("r"),
                record,
                owner,
                VariableAttributes::default(),
            )
        });
        assert!(matches!(
            refused,
            Err(DaeConstructionError::InvalidVariableType { .. })
        ));
        Ok(())
    })
    .unwrap();
}
