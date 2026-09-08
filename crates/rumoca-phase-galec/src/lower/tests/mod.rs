use rumoca_core::{SourceMap, Span, VarName};

use super::*;

mod conditional_records;
mod correlated_guards;
mod dynamic_index_bounds;
mod indexed_updates;
mod structural_locals;

fn define_scalar_passthrough_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    name: &'static str,
    scalar: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new(name), [scalar], [scalar], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, provenance)
            })?;
            let input = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            dae.functions(|functions| {
                functions.assign(&mut body, output, input, provenance)?;
                functions.define(body, provenance)
            })
        },
    )?;
    Ok(function)
}

#[test]
fn identity_element_is_integer_and_diagonal_by_index_equality() {
    let diagonal =
        lower_identity_element(&[gast::Expression::Integer(2), gast::Expression::Integer(2)]);
    assert_eq!(diagonal.scalar_type, gast::ScalarType::Integer);
    assert_eq!(diagonal.expression, gast::Expression::Integer(1));

    let off_diagonal =
        lower_identity_element(&[gast::Expression::Integer(1), gast::Expression::Integer(2)]);
    assert_eq!(off_diagonal.expression, gast::Expression::Integer(0));
}

/// The dependent-parameter binding fixture: `downstream` depends on
/// `derived`, which depends on `gain`. Split out for length; every
/// declaration and binding is unchanged.
fn dependent_parameter_binding_model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "parameter Real gain = 2; parameter Real downstream = derived + 1; parameter Real derived = 3 * gain;";
    let source = sources.add("dependent-parameters.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let at = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (
            (gain, gain_reservation),
            (_downstream, downstream_reservation),
            (derived, derived_reservation),
        ) = model.variables(|variables| {
            Ok((
                variables.reserve_parameter(
                    VarName::new("gain"),
                    rumoca_core::InstanceId::new(1),
                    real,
                    at,
                )?,
                variables.reserve_parameter(
                    VarName::new("downstream"),
                    rumoca_core::InstanceId::new(2),
                    real,
                    at,
                )?,
                variables.reserve_parameter(
                    VarName::new("derived"),
                    rumoca_core::InstanceId::new(3),
                    real,
                    at,
                )?,
            ))
        })?;
        let (gain_default, downstream_binding, derived_binding) =
            model.expressions(|expressions| {
                let gain_default = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                let gain_value = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(gain))?;
                let three = expressions.at(at).literal(dae::DaeLiteral::Real(3.0))?;
                let derived_binding =
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, three, gain_value)?;
                let derived_value = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(derived))?;
                let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                let downstream_binding =
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, derived_value, one)?;
                Ok((gain_default, downstream_binding, derived_binding))
            })?;
        model.variables(|variables| {
            let attributes = |binding| dae::VariableAttributes {
                binding: Some(binding),
                is_tunable: true,
                ..Default::default()
            };
            variables.define(gain_reservation, attributes(gain_default), at)?;
            variables.define(downstream_reservation, attributes(downstream_binding), at)?;
            variables.define(derived_reservation, attributes(derived_binding), at)
        })
    })
    .unwrap()
}

#[test]
fn binding_dependencies_issue_dependent_parameters_in_topological_order() {
    let model = dependent_parameter_binding_model();

    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let classified = classify_variables(view, &definitions).unwrap();
        let classes: HashMap<_, _> = classified
            .iter()
            .map(|variable| (variable.variable.name().as_str(), variable.class))
            .collect();
        assert_eq!(classes["gain"], VariableClass::TunableParameter);
        assert_eq!(classes["derived"], VariableClass::DependentParameter);
        assert_eq!(classes["downstream"], VariableClass::DependentParameter);

        let ordered_names: Vec<_> = classified
            .dependent_parameter_order
            .iter()
            .map(|id| {
                view.variable(view.variable_id(*id as usize).unwrap())
                    .unwrap()
                    .name()
                    .as_str()
            })
            .collect();
        assert_eq!(ordered_names, ["derived", "downstream"]);
        let derived = classified
            .iter()
            .find(|variable| variable.variable.name().as_str() == "derived")
            .expect("derived is classified");
        let downstream = classified
            .iter()
            .find(|variable| variable.variable.name().as_str() == "downstream")
            .expect("downstream is classified");
        assert_ne!(derived.id, downstream.id);
        assert_ne!(
            TemporaryNamespace::Dependent(derived.id).to_string(),
            TemporaryNamespace::Dependent(downstream.id).to_string(),
            "independent dependent-parameter lowerers cannot mint colliding temporaries"
        );
    });
}

/// Rank-2 dependent parameter over a rank-2 parameter, used to check that the
/// dependent binding lowers to a single checked whole-array move.
fn rank_two_dependent_parameter_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "parameter Real route[2,3]; parameter Real guidanceRoute[2,3] = route;";
    let source = sources.add("whole-array-modifier.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let at = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |model| {
        let matrix_type = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 3]), at)
        })?;
        let ((route, route_reservation), (_, guidance_reservation)) =
            model.variables(|variables| {
                Ok((
                    variables.reserve_parameter(
                        VarName::new("route"),
                        rumoca_core::InstanceId::new(4),
                        matrix_type,
                        at,
                    )?,
                    variables.reserve_parameter(
                        VarName::new("guidanceRoute"),
                        rumoca_core::InstanceId::new(5),
                        matrix_type,
                        at,
                    )?,
                ))
            })?;
        let (route_values, route_reference) = model.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(dae::DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let first = expressions.at(at).array(values[..3].iter().copied())?;
            let second = expressions.at(at).array(values[3..].iter().copied())?;
            let matrix = expressions.at(at).array([first, second])?;
            let reference = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(route))?;
            Ok((matrix, reference))
        })?;
        model.variables(|variables| {
            let attributes = |binding| dae::VariableAttributes {
                binding: Some(binding),
                is_tunable: true,
                ..Default::default()
            };
            variables.define(route_reservation, attributes(route_values), at)?;
            variables.define(guidance_reservation, attributes(route_reference), at)
        })
    })
    .unwrap()
}

#[test]
fn rank_two_dependent_parameter_preserves_one_checked_whole_array_move() {
    let model = rank_two_dependent_parameter_fixture();

    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let classified = classify_variables(view, &definitions).unwrap();
        let by_id = classified
            .iter()
            .map(|variable| (variable.id.index(), variable.clone()))
            .collect::<HashMap<_, _>>();
        let guidance = classified
            .iter()
            .find(|variable| variable.variable.name().as_str() == "guidanceRoute")
            .expect("dependent parameter is classified");
        assert_eq!(guidance.class, VariableClass::DependentParameter);
        assert_eq!(guidance.variable.value_type().dimensions(), [2, 3]);
        let binding = guidance.variable.binding().unwrap();
        assert_eq!(
            NumericEvaluator::new(view).expression(binding).unwrap(),
            [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
        );

        let previous = HashMap::new();
        let mut lowered = dependent_folding::dependent_assignment(
            BlockLowering {
                view,
                definitions: &definitions,
                by_id: &by_id,
                pre_names: &previous,
                arithmetic: positive_zero_arithmetic(),
            },
            guidance,
        )
        .unwrap();
        assert!(
            lowered.locals.is_empty(),
            "a whole-array copy needs no local of its own"
        );
        assert_eq!(
            lowered.statements.len(),
            1,
            "dependent parameter without assertions or materialized calls is one assignment"
        );
        let statement = lowered.statements.pop().expect("one statement");
        let gast::Statement::Assignment { target, value } = statement.node else {
            panic!("dependent parameter must lower to one assignment")
        };
        assert!(matches!(
            target,
            gast::Reference::State(parts)
                if parts.len() == 1
                    && parts[0].name.lexeme() == "guidanceRoute"
                    && parts[0].subscripts.is_empty()
        ));
        assert!(matches!(
            value,
            gast::Expression::Ref(gast::Reference::State(parts))
                if parts.len() == 1
                    && parts[0].name.lexeme() == "route"
                    && parts[0].subscripts.is_empty()
        ));

        let mut lowerer = ExpressionLowerer::new(
            view,
            &definitions,
            &by_id,
            &previous,
            positive_zero_arithmetic(),
        );
        for indices in [
            Vec::new(),
            vec![gast::Expression::Integer(1)],
            vec![gast::Expression::Integer(1); 3],
        ] {
            let error = lowerer
                .lower_at(binding, &indices)
                .err()
                .expect("partial and over-indexed scalar projections stay rejected");
            assert_eq!(error.code(), "EGT017");
        }
    });
}

#[test]
fn vector_projection_preserves_the_unique_non_unit_dimension() {
    let index = gast::Expression::Integer(2);
    assert_eq!(
        vector_operand_projection(&[1, 3, 1], std::slice::from_ref(&index)),
        [
            gast::Expression::Integer(1),
            index,
            gast::Expression::Integer(1)
        ]
    );
    assert!(vector_operand_projection(&[], &[gast::Expression::Integer(1)]).is_empty());
}

#[test]
fn transpose_projection_exchanges_only_the_first_two_axes() {
    let mut projection = [
        gast::Expression::Integer(2),
        gast::Expression::Integer(3),
        gast::Expression::Integer(4),
    ];
    projection.swap(0, 1);
    assert_eq!(
        projection,
        [
            gast::Expression::Integer(3),
            gast::Expression::Integer(2),
            gast::Expression::Integer(4),
        ]
    );
}

#[test]
fn static_integer_index_arithmetic_folds_without_overflow() {
    let expression = gast::Expression::binary(
        gast::BinaryOp::Add,
        gast::Expression::Integer(1),
        gast::Expression::binary(
            gast::BinaryOp::Mul,
            gast::Expression::Integer(2),
            gast::Expression::Integer(3),
        ),
    );
    assert_eq!(constant_integer(&expression), Some(7));
    assert_eq!(
        constant_integer(&gast::Expression::binary(
            gast::BinaryOp::Add,
            gast::Expression::Integer(i64::MAX),
            gast::Expression::Integer(1),
        )),
        None
    );
}

#[test]
fn dynamic_function_local_index_is_checked_and_exhaustively_projected() {
    let model = dae::Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let index = gast::Expression::Ref(gast::Reference::local(gast::Name::ident("segment")));
        let selected = lowerer
            .lower_local_reference(gast::Name::ident("waypoint"), &[3], &[index], Span::DUMMY)
            .unwrap();
        let prefix = lowerer.take_prefix_statements();

        assert!(matches!(
            selected,
            gast::Expression::If(ref value)
                if value.bounded_selection_correlation().is_some()
        ));
        assert_eq!(prefix.len(), 1);
        let gast::Statement::If(bounds) = &prefix[0].node else {
            panic!("a dynamic local index must own one runtime bounds check")
        };
        assert!(matches!(
            bounds.branches[0].body[0].node,
            gast::Statement::Signal(_)
        ));
    });
}

#[test]
fn whole_array_function_arguments_preserve_checked_references() {
    let mut sources = SourceMap::new();
    let text = "input Real samples[3];";
    let source = sources.add("direct-array-argument.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                provenance,
            )
        })?;
        let input = dae.variables(|variables| {
            variables.input(
                VarName::new("samples"),
                rumoca_core::InstanceId::new(6),
                vector,
                dae::InputVariability::Continuous,
                provenance,
                dae::VariableAttributes::default(),
            )
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Input(input))?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let variable = view.variable_id(0).unwrap();
        let mut variables = HashMap::new();
        variables.insert(
            variable.index(),
            ClassifiedVariable {
                id: variable,
                variable: view.variable(variable).unwrap(),
                class: VariableClass::Input,
                scalar_type: gast::ScalarType::Real,
                name: gast::Name::ident("samples"),
            },
        );
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let argument = view.expression_id(0).unwrap();
        let direct = lowerer.direct_whole_aggregate_reference(argument).unwrap();
        assert!(matches!(
            direct,
            Some(gast::Expression::Ref(gast::Reference::State(parts)))
                if parts.len() == 1
                    && parts[0].name.lexeme() == "samples"
                    && parts[0].subscripts.is_empty()
                    && parts[0].span == span
        ));
    });
}

#[test]
fn whole_array_function_value_arguments_preserve_the_proven_current_storage() {
    let mut sources = SourceMap::new();
    let text = "function current input Real u[3]; output Real y[3]; algorithm y := u; end current;";
    let source = sources.add("direct-function-value-argument.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                provenance,
            )
        })?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("current"), [vector], [vector], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, input, provenance))?;
                let read = dae.functions(|functions| functions.read(&body, output, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))?;
                Ok(read)
            },
        )?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let argument = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::FunctionValue { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        lowerer.function_scope = Some(function.id());
        let direct = lowerer.direct_whole_aggregate_reference(argument).unwrap();
        assert!(matches!(
            direct,
            Some(gast::Expression::Ref(gast::Reference::Local(part)))
                if part.name.lexeme() == "y"
                    && part.subscripts.is_empty()
                    && part.span == span
        ));
    });
}

fn direct_aggregate_call_fixture() -> (dae::Dae, Span) {
    let mut sources = SourceMap::new();
    let text = "function copy input Real u[3]; output Real y[3]; algorithm y := u; end copy; function caller input Real u[3]; output Real y[3]; algorithm y := copy(u); end caller;";
    let source = sources.add("direct-aggregate-call-result.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                provenance,
            )
        })?;
        let (copy, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("copy"), [vector], [vector], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let value = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("caller"), [vector], [vector], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let value = dae
                    .expressions(|expressions| expressions.at(provenance).call(copy, 0, [input]))?;
                dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        Ok(())
    })
    .unwrap();
    (model, span)
}

#[test]
fn protected_function_aggregate_call_uses_an_immutable_result_temporary() {
    let (model, _) = direct_aggregate_call_fixture();
    model.inspect(|view| {
        let caller = view.function_id(1).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([caller.index()]),
            positive_zero_arithmetic(),
        )
        .unwrap();
        let caller = lowered
            .iter()
            .find(|function| function.name.lexeme() == "caller")
            .expect("caller remains reachable");
        let gast::Statement::MultiAssignment { targets, .. } = &caller.statements[0].node else {
            panic!("aggregate call first materializes into immutable temporary storage")
        };
        assert!(matches!(
            targets.as_slice(),
            [gast::Reference::Local(target)] if target.name.lexeme() != "y"
        ));
    });
}

#[test]
fn one_aggregate_call_reused_in_a_function_group_executes_once() {
    let mut sources = SourceMap::new();
    let text = "function copy input Real u[3]; output Real y[3]; algorithm y := u; end copy; \
                function caller input Real u[3]; output Real a[3]; output Real b[3]; \
                algorithm (a, b) := (copy(u), copy(u)); end caller;";
    let source = sources.add("shared-direct-aggregate-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                provenance,
            )
        })?;
        let (copy, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("copy"), [vector], [vector], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, input, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let _ = dae.function(
            dae::FunctionSignature::new(
                VarName::new("caller"),
                [vector],
                [vector, vector],
                provenance,
            ),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("a"), 0, provenance)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("b"), 1, provenance)
                })?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let call = dae
                    .expressions(|expressions| expressions.at(provenance).call(copy, 0, [input]))?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                dae.functions(|functions| {
                    functions.assign_all(
                        &mut body,
                        &[(first, call), (second, call)],
                        provenance,
                    )?;
                    functions.define(body, provenance)
                })
            },
        )?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let caller = view.function_id(1).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([caller.index()]),
            positive_zero_arithmetic(),
        )
        .unwrap();
        let caller = lowered
            .iter()
            .find(|function| function.name.lexeme() == "caller")
            .expect("caller remains reachable");
        assert_eq!(
            count_named_multi_calls(&caller.statements, "copy"),
            1,
            "one issued aggregate call reused in an assignment group executes once"
        );
    });
}

#[test]
fn one_aggregate_call_reused_across_function_statements_executes_once() {
    let mut sources = SourceMap::new();
    let text = "function copy input Real u[3]; output Real y[3]; algorithm y := u; end copy; \
                function caller input Real u[3]; output Real a[3]; output Real b[3]; \
                algorithm a := copy(u); b := copy(u); end caller;";
    let source = sources.add("shared-cross-statement-aggregate-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                provenance,
            )
        })?;
        let (copy, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("copy"), [vector], [vector], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, input, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let _ = dae.function(
            dae::FunctionSignature::new(
                VarName::new("caller"),
                [vector],
                [vector, vector],
                provenance,
            ),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("a"), 0, provenance)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("b"), 1, provenance)
                })?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let call = dae
                    .expressions(|expressions| expressions.at(provenance).call(copy, 0, [input]))?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, first, call, provenance)?;
                    functions.assign(&mut body, second, call, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let caller = view.function_id(1).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([caller.index()]),
            positive_zero_arithmetic(),
        )
        .unwrap();
        let caller = lowered
            .iter()
            .find(|function| function.name.lexeme() == "caller")
            .expect("caller remains reachable");
        assert_eq!(
            count_named_multi_calls(&caller.statements, "copy"),
            1,
            "one issued aggregate call reused across statements executes once"
        );
    });
}

#[test]
fn nested_function_calls_preserve_distinct_outer_call_paths() {
    let mut sources = SourceMap::new();
    let text = "function leaf input Real u; output Real y; algorithm y := u; end leaf; \
                function wrapper input Real u; output Real y; algorithm y := leaf(u); end wrapper; \
                function caller input Real u; output Real a; output Real b; \
                algorithm a := wrapper(u); b := wrapper(u); end caller;";
    let source = sources.add("nested-function-call-path.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let leaf = define_scalar_passthrough_function(dae, "leaf", real, provenance)?;
        let (wrapper, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("wrapper"), [real], [real], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let call = dae
                    .expressions(|expressions| expressions.at(provenance).call(leaf, 0, [input]))?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, call, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("caller"), [real], [real, real], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("a"), 0, provenance)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("b"), 1, provenance)
                })?;
                let input = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let calls = dae.expressions(|expressions| {
                    Ok([
                        expressions.at(provenance).call(wrapper, 0, [input])?,
                        expressions.at(provenance).call(wrapper, 0, [input])?,
                    ])
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, first, calls[0], provenance)?;
                    functions.assign(&mut body, second, calls[1], provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let caller = view.function_id(2).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([caller.index()]),
            positive_zero_arithmetic(),
        )
        .expect("distinct outer paths keep their nested call owners distinct");
    });
}

#[test]
fn causally_defined_output_remains_an_interface_and_gets_an_assignment() {
    let mut sources = SourceMap::new();
    let text = "input Real u; output Real y; equation y = u;";
    let source = sources.add("output-definition.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (input, output) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("u"),
                    rumoca_core::InstanceId::new(7),
                    real,
                    dae::InputVariability::Continuous,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Input,
                        ..Default::default()
                    },
                )?,
                variables.output(
                    VarName::new("y"),
                    rumoca_core::InstanceId::new(8),
                    real,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Output,
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        let residual = dae.expressions(|expressions| {
            let input = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Input(input))?;
            let output = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Algebraic(output))?;
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, output, input)
        })?;
        dae.continuous(|continuous| {
            continuous.equation(provenance, |equation| equation.residual(residual))?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let classified = classify_variables(view, &definitions).unwrap();
        assert!(classified.iter().any(|variable| {
            variable.class == VariableClass::Output && variable.variable.name().as_str() == "y"
        }));
        let by_id = classified
            .iter()
            .map(|variable| (variable.id.index(), variable.clone()))
            .collect::<HashMap<_, _>>();
        let pre_names = HashMap::new();
        let plan =
            causal_outputs::CausalAssignmentsPlan::construct(&definitions, classified.as_slice())
                .unwrap();
        let mut retained_calls = RetainedCallResults::default();
        let prepared = causal_outputs::prepare_causal_assignments(
            BlockLowering {
                view,
                definitions: &definitions,
                by_id: &by_id,
                pre_names: &pre_names,
                arithmetic: positive_zero_arithmetic(),
            },
            &plan,
            &mut retained_calls,
        )
        .unwrap();
        let statements = prepared.statements;
        assert_eq!(statements.len(), 1);
        assert!(matches!(
            &statements[0].node,
            gast::Statement::Assignment { target, value }
                if matches!(target, gast::Reference::State(parts)
                    if parts.len() == 1 && parts[0].name.lexeme() == "y")
                    && matches!(value, gast::Expression::Ref(gast::Reference::State(parts))
                        if parts.len() == 1 && parts[0].name.lexeme() == "u")
        ));
    });
}

#[test]
fn function_assertion_is_detected_before_expression_inlining() {
    let mut sources = SourceMap::new();
    let text =
        "function f output Real y; algorithm assert(false, \"invalid\"); y := 0.0; end f; f();";
    let source = sources.add("assertion.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (function, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [real], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Boolean(false))
                })?;
                let message = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::String("invalid".to_owned()))
                })?;
                dae.functions(|functions| {
                    functions.assertion(&mut body, condition, message, provenance)
                })?;
                let zero = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        dae.expressions(|expressions| expressions.at(provenance).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(first_function_assertion(function.statements()), Some(span));
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .unwrap();
        assert!(user_functions::expression_calls_asserting_function(
            view, call
        ));
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let Err(rejected) = ExpressionLowerer::new(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        )
        .lower(call) else {
            panic!("an assertion needs an explicit call-scoped action sink")
        };
        assert!(matches!(
            rejected,
            GalecTargetError::UnsupportedFeature { feature, .. }
                if feature == "function-assertion"
        ));
        let mut lowerer = ExpressionLowerer::with_assertions(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        assert_eq!(
            lowerer.lower(call).unwrap().expression,
            gast::Expression::Real(0.0)
        );
        let assertions = lowerer.take_prefix_statements();
        assert_eq!(assertions.len(), 1);
        let gast::Statement::If(assertion) = &assertions[0].node else {
            panic!("call-scoped assertion lowers to a guarded signal")
        };
        assert!(matches!(
            assertion.branches[0].body[0].node,
            gast::Statement::Signal(ref signals)
                if signals[0].as_str() == gast::PredefinedSignal::InvalidArgument.name()
        ));
    });
}

#[test]
fn prefix_boundary_rematerializes_function_calls_for_reorder_safety() {
    let mut sources = SourceMap::new();
    let text = "function f output Real y; algorithm y := 1.0; end f; f();";
    let source = sources.add("materialized-prefix.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (function, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [real], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let one = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(1.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, one, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        dae.expressions(|expressions| expressions.at(provenance).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );

        let first = lowerer.lower(call).unwrap().expression;
        let first_prefix = lowerer.take_prefix_statements();
        let second = lowerer.lower(call).unwrap().expression;
        let second_prefix = lowerer.take_prefix_statements();

        assert_eq!(first_prefix.len(), 1);
        assert_eq!(second_prefix.len(), 1);
        assert_ne!(first, second);
        assert!(matches!(
            (&first_prefix[0].node, &second_prefix[0].node),
            (
                gast::Statement::MultiAssignment { targets: first, .. },
                gast::Statement::MultiAssignment { targets: second, .. }
            ) if matches!(
                (&first[0], &second[0]),
                (
                    gast::Reference::Local(first_target),
                    gast::Reference::Local(second_target)
                ) if first_target.name.lexeme() != second_target.name.lexeme()
            )
        ));
    });
}

#[test]
fn tensor_prefix_dependency_finds_outer_indices_inside_nested_loops() {
    let outer = gast::Name::ident("outer");
    let accumulator = gast::Name::ident("sum");
    let reset = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(accumulator.clone()),
        value: gast::Expression::Real(0.0),
    });
    let dependent = gast::Spanned::dummy(gast::Statement::for_loop(gast::ForLoop::new(
        Some(gast::Name::ident("inner")),
        gast::Expression::Integer(1),
        None,
        gast::Expression::Integer(3),
        vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(accumulator),
            value: gast::Expression::Ref(gast::Reference::local(outer.clone())),
        })],
    )));
    let independent = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(gast::Name::ident("argument")),
        value: gast::Expression::Real(1.0),
    });

    assert!(user_functions::statement_depends_on(
        &dependent,
        std::slice::from_ref(&outer)
    ));
    assert!(!user_functions::statement_depends_on(
        &independent,
        std::slice::from_ref(&outer)
    ));

    let (before, body) = user_functions::partition_tensor_prefixes(
        vec![reset, dependent, independent.clone()],
        std::slice::from_ref(&outer),
    );
    assert_eq!(before, vec![independent]);
    assert_eq!(body.len(), 2);
    assert!(matches!(body[0].node, gast::Statement::Assignment { .. }));
    assert!(matches!(body[1].node, gast::Statement::For(_)));
}

#[test]
fn tensor_prefix_partition_hoists_work_from_an_invariant_guard() {
    let outer = gast::Name::ident("outer");
    let accepted = gast::Name::ident("accepted");
    let ready = gast::Name::ident("ready");
    let shared = gast::Name::ident("posterior");
    let independent = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(shared.clone()),
        value: gast::Expression::Real(1.0),
    });
    let dependent = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::Local(gast::RefPart {
            name: gast::Name::ident("covariance"),
            subscripts: vec![gast::Expression::Ref(gast::Reference::local(outer.clone()))],
            span: Span::DUMMY,
        }),
        value: gast::Expression::Ref(gast::Reference::local(shared)),
    });
    let guarded = gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
        branches: vec![gast::IfBranch {
            condition: gast::Condition::Expression(gast::Expression::binary(
                gast::BinaryOp::And,
                gast::Expression::Ref(gast::Reference::local(accepted)),
                gast::Expression::Ref(gast::Reference::local(ready)),
            )),
            body: vec![independent, dependent],
            span: Span::DUMMY,
        }],
        else_body: None,
    }));

    let (before, body) =
        user_functions::partition_tensor_prefixes(vec![guarded], std::slice::from_ref(&outer));

    assert_eq!(before.len(), 1);
    assert_eq!(body.len(), 1);
    let gast::Statement::If(before_guard) = &before[0].node else {
        panic!("the hoisted prefix must retain its runtime guard")
    };
    assert!(matches!(
        before_guard.branches[0].body[0].node,
        gast::Statement::Assignment { .. }
    ));
    let gast::Statement::If(body_guard) = &body[0].node else {
        panic!("the indexed projection must retain its runtime guard")
    };
    let gast::Statement::Assignment { target, .. } = &body_guard.branches[0].body[0].node else {
        panic!("the indexed projection must remain inside the tensor loop")
    };
    assert!(matches!(
        target,
        gast::Reference::Local(part) if !part.subscripts.is_empty()
    ));
}

/// A guard whose branches assign one target with a loop-invariant value on one
/// side and a loop-dependent value on the other must survive whole.
///
/// Splitting it would put the two assignments in two conditionals that repeat
/// the same test, and the target would then be assigned on every path without
/// any one statement showing it. The generated C is what pays: a compiler
/// cannot assume the second test repeats the first across the statements
/// between them, so it reports a variable that may be used uninitialized, and
/// the assurance preflight compiles with `-Werror`.
#[test]
fn tensor_prefix_partition_keeps_a_guard_that_splits_one_target() {
    let outer = gast::Name::ident("outer");
    let selected = gast::Name::ident("selected");
    let dependent = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(selected.clone()),
        value: gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
            name: gast::Name::ident("correction"),
            subscripts: vec![gast::Expression::Ref(gast::Reference::local(outer.clone()))],
            span: Span::DUMMY,
        })),
    });
    let invariant = gast::Spanned::dummy(gast::Statement::Assignment {
        target: gast::Reference::local(selected.clone()),
        value: gast::Expression::Real(0.0),
    });
    let guarded = gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
        branches: vec![gast::IfBranch {
            condition: gast::Condition::Expression(gast::Expression::Ref(gast::Reference::local(
                gast::Name::ident("engaged"),
            ))),
            body: vec![dependent],
            span: Span::DUMMY,
        }],
        else_body: Some(vec![invariant]),
    }));

    let (before, body) = user_functions::partition_tensor_prefixes(
        vec![guarded.clone()],
        std::slice::from_ref(&outer),
    );

    assert!(
        before.is_empty(),
        "a guard that assigns one target on both sides must not be split"
    );
    assert_eq!(body, vec![guarded]);
    let gast::Statement::If(kept) = &body[0].node else {
        panic!("the guard must survive as one conditional")
    };
    assert!(matches!(
        kept.branches[0].body[0].node,
        gast::Statement::Assignment { .. }
    ));
    assert!(matches!(
        kept.else_body.as_ref().expect("the fallback arm survives")[0].node,
        gast::Statement::Assignment { .. }
    ));
}

fn guarded_tensor_fixture() -> (
    Vec<gast::Spanned<gast::Statement>>,
    Vec<gast::Spanned<gast::Statement>>,
) {
    let enabled = gast::Expression::Ref(gast::Reference::local(gast::Name::ident("enabled")));
    let ready = gast::Expression::Ref(gast::Reference::local(gast::Name::ident("ready")));
    let shared = gast::Name::ident("shared");
    let selected = gast::Name::ident("selected");
    let before = vec![gast::Spanned::dummy(gast::Statement::If(
        gast::IfStatement {
            branches: vec![
                gast::IfBranch {
                    condition: gast::Condition::Expression(enabled.clone()),
                    body: vec![gast::Spanned::dummy(gast::Statement::Assignment {
                        target: gast::Reference::local(shared.clone()),
                        value: gast::Expression::Real(1.0),
                    })],
                    span: Span::DUMMY,
                },
                gast::IfBranch {
                    condition: gast::Condition::Expression(ready.clone()),
                    body: vec![gast::Spanned::dummy(gast::Statement::Assignment {
                        target: gast::Reference::local(shared.clone()),
                        value: gast::Expression::Real(0.5),
                    })],
                    span: Span::DUMMY,
                },
            ],
            else_body: Some(vec![gast::Spanned::dummy(gast::Statement::Assignment {
                target: gast::Reference::local(shared.clone()),
                value: gast::Expression::Real(0.0),
            })]),
        },
    ))];
    let body = vec![
        gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
            branches: vec![gast::IfBranch {
                condition: gast::Condition::Expression(enabled),
                body: vec![gast::Spanned::dummy(gast::Statement::Assignment {
                    target: gast::Reference::local(selected.clone()),
                    value: gast::Expression::Ref(gast::Reference::local(shared)),
                })],
                span: Span::DUMMY,
            }],
            else_body: Some(vec![
                gast::Spanned::dummy(gast::Statement::If(gast::IfStatement {
                    branches: vec![gast::IfBranch {
                        condition: gast::Condition::Expression(ready),
                        body: vec![gast::Spanned::dummy(gast::Statement::Assignment {
                            target: gast::Reference::local(selected.clone()),
                            value: gast::Expression::Real(0.5),
                        })],
                        span: Span::DUMMY,
                    }],
                    else_body: Some(vec![gast::Spanned::dummy(gast::Statement::Assignment {
                        target: gast::Reference::local(selected.clone()),
                        value: gast::Expression::Real(0.0),
                    })]),
                })),
                gast::Spanned::dummy(gast::Statement::Assignment {
                    target: gast::Reference::local(gast::Name::ident("suffix")),
                    value: gast::Expression::Real(1.0),
                }),
            ]),
        })),
        gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(gast::Name::ident("target")),
            value: gast::Expression::Ref(gast::Reference::local(selected)),
        }),
    ];
    (before, body)
}

#[test]
fn guarded_tensor_loop_keeps_invariant_initializer_and_projection_together() {
    let (before, body) = guarded_tensor_fixture();
    let second_body = body.clone();
    let mut fused = user_functions::fuse_guarded_tensor_loop(
        before,
        body,
        &[gast::Name::ident("i")],
        &[3],
        Span::DUMMY,
    )
    .expect("matching total guards fuse");

    let gast::Statement::If(guard) = &fused[0].node else {
        panic!("the fused tensor remains one total guard")
    };
    assert_eq!(guard.branches.len(), 2);
    assert!(matches!(
        (
            &guard.branches[0].body[0].node,
            &guard.branches[0].body[1].node
        ),
        (gast::Statement::Assignment { .. }, gast::Statement::For(_))
    ));
    assert!(matches!(
        &guard.else_body.as_ref().unwrap()[1].node,
        gast::Statement::For(_)
    ));

    let second_loop = gast::Spanned::dummy(gast::Statement::for_loop(gast::ForLoop::new(
        Some(gast::Name::ident("j")),
        gast::Expression::Integer(1),
        None,
        gast::Expression::Integer(3),
        second_body,
    )));
    let merged =
        user_functions::merge_guarded_tensor_loops(vec![fused.pop().unwrap(), second_loop]);
    assert_eq!(merged.len(), 1);
    let gast::Statement::If(guard) = &merged[0].node else {
        panic!("a later tensor field merges into the total guard")
    };
    assert_eq!(
        guard.branches[0]
            .body
            .iter()
            .filter(|statement| matches!(statement.node, gast::Statement::For(_)))
            .count(),
        2
    );
}

fn count_named_multi_calls(statements: &[gast::Spanned<gast::Statement>], function: &str) -> usize {
    statements
        .iter()
        .map(|statement| match &statement.node {
            gast::Statement::MultiAssignment { call, .. } if call.function.lexeme() == function => {
                1
            }
            gast::Statement::If(statement) => {
                statement
                    .branches
                    .iter()
                    .map(|branch| count_named_multi_calls(&branch.body, function))
                    .sum::<usize>()
                    + statement
                        .else_body
                        .as_deref()
                        .map(|body| count_named_multi_calls(body, function))
                        .unwrap_or_default()
            }
            gast::Statement::For(statement) => count_named_multi_calls(&statement.body, function),
            _ => 0,
        })
        .sum()
}

fn assert_atomic_multi_output_group(model: &dae::Dae) {
    model.inspect(|view| {
        let caller = view.function_id(1).unwrap();
        let statements = view
            .function(caller)
            .unwrap()
            .statements()
            .collect::<Vec<_>>();
        assert!(matches!(
            &statements[0],
            dae::FunctionStatementView::AssignmentGroup { definitions, .. }
                if definitions.len() == 2
        ));
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([caller.index()]),
            positive_zero_arithmetic(),
        )
        .expect("the atomic multi-output group lowers");
        let caller = lowered
            .iter()
            .find(|function| function.name.lexeme() == "caller")
            .unwrap();
        assert_eq!(count_named_multi_calls(&caller.statements, "pair"), 1);
    });
}

#[test]
fn lazy_tensor_selection_guards_hoisted_calls_and_indexed_contractions() {
    let model = dae::Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let outer = gast::Name::ident("outer");
        let enabled = gast::Name::ident("enabled");
        let shared = gast::Name::ident("shared");
        let contraction = gast::Name::ident("contraction");
        lowerer.loop_index_bounds.push(LoopIndexBound {
            name: outer.clone(),
            minimum: 1,
            maximum: 6,
        });

        let shared_call = gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(shared.clone()),
            value: gast::Expression::Real(2.0),
        });
        let contraction_loop = gast::Spanned::dummy(gast::Statement::for_loop(gast::ForLoop::new(
            Some(gast::Name::ident("inner")),
            gast::Expression::Integer(1),
            None,
            gast::Expression::Integer(3),
            vec![gast::Spanned::dummy(gast::Statement::Assignment {
                target: gast::Reference::local(contraction.clone()),
                value: gast::Expression::Ref(gast::Reference::local(outer.clone())),
            })],
        )));
        let branches = vec![
            expression_projection::SelectionBranch {
                condition_prefix: Vec::new(),
                condition: gast::Expression::Ref(gast::Reference::local(enabled)),
                value: expression_projection::SelectionValue {
                    prefix: vec![shared_call, contraction_loop],
                    expression: gast::Expression::Ref(gast::Reference::local(contraction)),
                },
            },
            expression_projection::SelectionBranch {
                condition_prefix: Vec::new(),
                condition: gast::Expression::Bool(false),
                value: expression_projection::SelectionValue {
                    prefix: Vec::new(),
                    expression: gast::Expression::Ref(gast::Reference::local(shared)),
                },
            },
        ];
        let selected = lowerer.lower_lazy_selection(
            branches,
            expression_projection::SelectionValue {
                prefix: Vec::new(),
                expression: gast::Expression::Real(0.0),
            },
            gast::ScalarType::Real,
            Span::DUMMY,
        );

        let statements = lowerer.take_prefix_statements();
        assert!(matches!(selected.expression, gast::Expression::Ref(_)));
        assert_eq!(statements.len(), 2);
        let gast::Statement::If(hoisted) = &statements[0].node else {
            panic!("loop-invariant branch work must retain its selection guard")
        };
        assert!(matches!(
            hoisted.branches[0].body[0].node,
            gast::Statement::Assignment { .. }
        ));
        let gast::Statement::If(selection) = &statements[1].node else {
            panic!("range-sensitive contraction must remain under the selection guard")
        };
        assert!(matches!(
            selection.branches[0].body[0].node,
            gast::Statement::For(_)
        ));
    });
}

mod retained_conditionals;
