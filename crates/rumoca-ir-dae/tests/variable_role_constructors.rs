use rumoca_core::{SourceId, SourceMap, Span, VarName};
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

#[derive(Clone, Copy)]
enum InvalidRole {
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
    ContinuousInput,
}

impl InvalidRole {
    const fn role(self) -> VariableRole {
        match self {
            Self::State => VariableRole::State,
            Self::Algebraic => VariableRole::Algebraic,
            Self::Output => VariableRole::Output,
            Self::DiscreteReal => VariableRole::DiscreteReal,
            Self::DiscreteValue => VariableRole::DiscreteValue,
            Self::ContinuousInput => VariableRole::Input,
        }
    }
}

fn invalid_role_type(role: InvalidRole, scalar: ScalarType) -> DaeConstructionError {
    let mut source_map = SourceMap::new();
    let source = source_map.add("invalid_role.mo", "forbidden coordinate declaration");
    let declaration =
        DaeProvenance::source(Span::from_offsets(source, 0, 22)).expect("exact declaration span");
    Dae::construct(source_map, |dae| {
        let value_type =
            dae.types(|types| types.derived(ValueType::scalar(scalar), declaration))?;
        dae.variables(|variables| {
            match role {
                InvalidRole::State => {
                    variables.state(
                        VarName::new("x"),
                        value_type,
                        declaration,
                        VariableAttributes::default(),
                    )?;
                }
                InvalidRole::Algebraic => {
                    variables.algebraic(
                        VarName::new("y"),
                        value_type,
                        declaration,
                        VariableAttributes::default(),
                    )?;
                }
                InvalidRole::Output => {
                    variables.output(
                        VarName::new("output"),
                        value_type,
                        declaration,
                        VariableAttributes::default(),
                    )?;
                }
                InvalidRole::DiscreteReal => {
                    variables.discrete_real(
                        VarName::new("z"),
                        value_type,
                        declaration,
                        VariableAttributes::default(),
                    )?;
                }
                InvalidRole::DiscreteValue => {
                    variables.discrete_value(
                        VarName::new("m"),
                        value_type,
                        declaration,
                        VariableAttributes::default(),
                    )?;
                }
                InvalidRole::ContinuousInput => {
                    variables.input(
                        VarName::new("u"),
                        value_type,
                        InputVariability::Continuous,
                        declaration,
                        VariableAttributes::default(),
                    )?;
                }
            }
            Ok(())
        })
    })
    .expect_err("an illegal Appendix-B role/type pair cannot finalize")
}

#[test]
fn real_and_discrete_coordinate_roles_reject_wrong_primitive_types_at_declaration() {
    let expected_span = Span::from_offsets(SourceId::from_source_name("invalid_role.mo"), 0, 22);
    for (role, scalar) in [
        (InvalidRole::State, ScalarType::Boolean),
        (InvalidRole::Algebraic, ScalarType::Integer),
        (InvalidRole::Output, ScalarType::String),
        (InvalidRole::DiscreteReal, ScalarType::Boolean),
        (InvalidRole::DiscreteValue, ScalarType::Real),
        (InvalidRole::ContinuousInput, ScalarType::Integer),
    ] {
        let error = invalid_role_type(role, scalar);
        assert!(matches!(
            error,
            DaeConstructionError::InvalidVariableType {
                role: found_role,
                found,
                span,
                ..
            } if found_role == role.role()
                && found == scalar
                && span == expected_span
        ));
    }
}

#[test]
fn enumeration_coordinate_constructs_as_discrete_value_with_b1c_owner() {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "enumeration_coordinate.mo",
        "type Mode = enumeration(off, on); Mode mode(start = Mode.off);",
    );
    let declaration =
        DaeProvenance::source(Span::from_offsets(source, 35, 62)).expect("enum declaration");
    let dae = Dae::construct(source_map, |dae| {
        let enumeration = dae.types(|types| {
            types.derived(ValueType::scalar(ScalarType::Enumeration), declaration)
        })?;
        let start =
            dae.expressions(|expressions| expressions.at(declaration).enumeration_literal(1))?;
        let mode = dae.variables(|variables| {
            variables.discrete_value(VarName::new("mode"), enumeration, declaration, held(start))
        })?;
        let previous = dae.expressions(|expressions| {
            expressions
                .at(declaration)
                .coordinate(rumoca_ir_dae::CoordinateInput::PreDiscreteValue(mode))
        })?;
        dae.b1c([mode], |topology| {
            topology.owner(declaration, [mode], |owner| {
                owner.always(declaration, [(previous, declaration)])
            })?;
            Ok(())
        })
    })
    .expect("an exact enumeration coordinate is a valid B.1c discrete value");

    dae.inspect(|view| {
        let variable = view
            .variables()
            .find_map(|(_, variable)| (variable.name().as_str() == "mode").then_some(variable))
            .expect("enumeration coordinate");
        assert_eq!(variable.role(), VariableRole::DiscreteValue);
        assert_eq!(variable.value_type().scalar_type(), ScalarType::Enumeration);
    });
}

#[test]
fn enumeration_coordinate_is_an_exact_ordinal_array_index() {
    let text = "type Mode = enumeration(off, on); Mode choices[2]; input Mode mode; Mode selected; choices[mode]";
    let mut source_map = SourceMap::new();
    let source = source_map.add("enumeration_index.mo", text);
    let provenance = |needle: &str, exact_len: usize| {
        let start = text.find(needle).expect("readable fixture occurrence");
        DaeProvenance::source(Span::from_offsets(source, start, start + exact_len))
            .expect("exact source occurrence")
    };
    let selector_declaration = provenance("input Mode mode", "input Mode mode".len());
    let selected_declaration = provenance("Mode selected", "Mode selected".len());
    let subscript = provenance("mode]", "mode".len());
    let index_owner = provenance("choices[mode]", "choices[mode]".len());
    let dae = Dae::construct(source_map, |dae| {
        let enumeration = dae.types(|types| {
            types.derived(
                ValueType::scalar(ScalarType::Enumeration),
                selector_declaration,
            )
        })?;
        let (off, choices) = dae.expressions(|expressions| {
            let off = expressions
                .at(provenance("off,", "off".len()))
                .enumeration_literal(1)?;
            let on = expressions
                .at(provenance("on);", "on".len()))
                .enumeration_literal(2)?;
            let choices = expressions
                .at(provenance("choices[2]", "choices[2]".len()))
                .array([off, on])?;
            Ok((off, choices))
        })?;
        let (mode, selected) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("mode"),
                    enumeration,
                    InputVariability::Discrete,
                    selector_declaration,
                    VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("selected"),
                    enumeration,
                    selected_declaration,
                    held(off),
                )?,
            ))
        })?;
        let indexed = dae.expressions(|expressions| {
            let ordinal = expressions
                .at(subscript)
                .coordinate(rumoca_ir_dae::CoordinateInput::Input(mode))?;
            expressions.at(index_owner).index(
                choices,
                [rumoca_ir_dae::Subscript::Value {
                    expression: ordinal,
                    provenance: subscript,
                }],
            )
        })?;
        dae.b1c([selected], |topology| {
            topology.owner(selected_declaration, [selected], |owner| {
                owner.always(selected_declaration, [(indexed, index_owner)])
            })?;
            Ok(())
        })
    })
    .expect("an exact enumeration coordinate constructs as an ordinal index");

    assert_exact_enumeration_index(&dae, index_owner, subscript);
}

fn assert_exact_enumeration_index(dae: &Dae, index_owner: DaeProvenance, subscript: DaeProvenance) {
    dae.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).expect("one owner"))
            .expect("checked discrete-value owner");
        let (indexed, value_provenance) = owner
            .branches()
            .get(0)
            .expect("always branch")
            .values()
            .get(0)
            .expect("one assigned value");
        assert_eq!(value_provenance, index_owner);
        let indexed = view
            .expression(indexed)
            .expect("checked indexed expression");
        assert_eq!(indexed.provenance(), index_owner);
        let rumoca_ir_dae::ExpressionOperation::Index { subscripts, .. } = indexed.operation()
        else {
            panic!("owner occurrence must remain an index expression");
        };
        assert!(matches!(
            subscripts.get(0),
            Some(rumoca_ir_dae::SubscriptView::Index {
                expression,
                provenance,
            }) if provenance == subscript
                && view
                    .expression(expression)
                    .is_some_and(|coordinate| coordinate.provenance() == subscript)
        ));
        assert_eq!(view.source_text(subscript), Some("mode"));
        assert_eq!(indexed.value_type().scalar_type(), ScalarType::Enumeration);
    });
}

#[test]
fn primitive_parameter_input_and_discrete_arrays_preserve_rectangular_capacity() {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "primitive_coordinates.mo",
        "parameter String labels[2]; input Boolean enabled[3]; discrete String modes[2]; \
         discrete Integer enum_code;",
    );
    let parameter_at =
        DaeProvenance::source(Span::from_offsets(source, 0, 27)).expect("parameter declaration");
    let input_at =
        DaeProvenance::source(Span::from_offsets(source, 28, 53)).expect("input declaration");
    let discrete_at =
        DaeProvenance::source(Span::from_offsets(source, 54, 79)).expect("discrete declaration");
    let integer_at =
        DaeProvenance::source(Span::from_offsets(source, 80, 107)).expect("integer declaration");
    let dae = Dae::construct(source_map, |dae| {
        let (strings, booleans, integers) = dae.types(|types| {
            Ok((
                types.derived(ValueType::array(ScalarType::String, [2]), parameter_at)?,
                types.derived(ValueType::array(ScalarType::Boolean, [3]), input_at)?,
                types.derived(ValueType::scalar(ScalarType::Integer), integer_at)?,
            ))
        })?;
        dae.variables(|variables| {
            variables.parameter(
                VarName::new("labels"),
                strings,
                parameter_at,
                VariableAttributes::default(),
            )?;
            variables.input(
                VarName::new("enabled"),
                booleans,
                InputVariability::Discrete,
                input_at,
                VariableAttributes::default(),
            )?;
            variables.discrete_value(
                VarName::new("modes"),
                strings,
                discrete_at,
                VariableAttributes {
                    causality: rumoca_ir_dae::VariableCausality::Input,
                    ..VariableAttributes::default()
                },
            )?;
            variables.discrete_value(
                VarName::new("enum_code"),
                integers,
                integer_at,
                VariableAttributes {
                    causality: rumoca_ir_dae::VariableCausality::Input,
                    ..VariableAttributes::default()
                },
            )?;
            Ok(())
        })
    })
    .expect("primitive rectangular p/input/m coordinates construct");

    dae.inspect(|view| {
        let counts = view
            .variables()
            .map(|(_, variable)| variable.scalar_count())
            .collect::<Vec<_>>();
        assert_eq!(counts, [2, 3, 2, 1]);
    });
    let encoded = serde_json::to_string(&dae).expect("checked primitive coordinates serialize");
    let decoded: Dae =
        serde_json::from_str(&encoded).expect("wire replays String discrete-value construction");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);
}

#[test]
fn record_aggregate_cannot_be_inserted_as_a_model_coordinate() {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "record_coordinate.mo",
        "record Pair Real x; end Pair; parameter Pair p;",
    );
    let record_at =
        DaeProvenance::source(Span::from_offsets(source, 0, 29)).expect("record declaration");
    let parameter_at =
        DaeProvenance::source(Span::from_offsets(source, 30, 47)).expect("parameter declaration");
    let error = Dae::construct(source_map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), record_at))?;
        let pair = dae.types(|types| {
            types.record(VarName::new("Pair"), [(VarName::new("x"), real)], record_at)
        })?;
        dae.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                pair,
                parameter_at,
                VariableAttributes::default(),
            )
        })?;
        Ok(())
    })
    .expect_err("record model coordinates must be flattened into primitive fields");
    let expected_span =
        Span::from_offsets(SourceId::from_source_name("record_coordinate.mo"), 30, 47);

    assert!(matches!(
        error,
        DaeConstructionError::InvalidVariableType {
            role: VariableRole::Parameter,
            found: ScalarType::Record,
            span,
            ..
        } if span == expected_span
    ));
}
