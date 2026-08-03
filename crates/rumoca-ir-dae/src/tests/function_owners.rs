use super::*;

#[test]
fn functions_conditions_and_generated_runtime_nodes_use_the_same_arena() {
    let source =
        TestSource::new("function f input Real u; output Real y; end f; when x > 0 then end when;");
    let function_declaration = source.source("function f", 0);
    let literal_span = source.source("0", 0);
    let condition_owner = source.source("x > 0", 0);
    let clock_generated = DaeProvenance::generated(
        DaeGeneration::ClockLowering,
        source.span("when x > 0 then end when", 0),
    )
    .unwrap();
    let delay_generated =
        DaeProvenance::generated(DaeGeneration::DelayLowering, source.span("x > 0", 0)).unwrap();

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                function_declaration,
            )
        })?;
        let literal =
            dae.expressions(|expr| expr.at(literal_span).literal(DaeLiteral::Real(0.0)))?;
        let (function, ()) = dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_declaration),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, function_declaration)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, function_declaration)
                })?;
                let parameter_value = dae.expressions(|expr| {
                    expr.at(function_declaration).function_parameter(parameter)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_declaration))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, parameter_value, function_declaration)
                })?;
                dae.functions(|functions| functions.define(body, function_declaration))
            },
        )?;

        let condition = dae.conditions(|conditions| conditions.reserve(condition_owner))?;
        let condition_value =
            dae.expressions(|expr| expr.at(condition_owner).literal(DaeLiteral::Boolean(true)))?;
        dae.conditions(|conditions| {
            conditions.define(
                condition,
                ConditionInput::Discrete(condition_value),
                condition_owner,
            )
        })?;
        let delay_time =
            dae.expressions(|expr| expr.at(delay_generated).literal(DaeLiteral::Real(1.0)))?;
        let positive =
            dae.temporal(|temporal| temporal.positive_parameter(delay_time, 1.0, delay_generated))?;
        let delay = dae.expressions(|expr| {
            expr.at(delay_generated)
                .delay(literal, positive, delay_generated)
        })?;

        dae.expressions(|expr| {
            let _call = expr.at(function_declaration).call(function, 0, [literal])?;
            let _condition = expr
                .at(condition_owner)
                .coordinate(CoordinateInput::Condition(condition))?;
            let _clock = expr.at(clock_generated).coordinate(CoordinateInput::Time)?;
            let _delay = delay.expression();
            Ok(())
        })
    })
    .expect("all owners share one arena");

    dae.inspect(assert_function_runtime_arena);

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.expression_count(), 8);
        assert_eq!(view.variable_count(), 0);
    });
}

fn assert_function_runtime_arena(view: DaeView<'_>) {
    assert_eq!(view.expression_count(), 8);
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    assert_eq!(function.name().as_str(), "f");
    assert_eq!(function.parameter_types().len(), 1);
    assert_eq!(function.result_types().len(), 1);
    assert_eq!(function.result_values().len(), 1);
    let definition = function.result_values().get(0).unwrap();
    assert_eq!(
        view.function_definition(definition.id()).unwrap().rhs(),
        definition.rhs()
    );
    assert_eq!(
        function.result_values().iter().next().unwrap().id(),
        definition.id()
    );
    assert_eq!(
        function.result_values().rhs_iter().next(),
        Some(definition.rhs())
    );
    let result = view.expression(definition.rhs()).unwrap();
    assert_eq!(result.function_scope(), view.function_id(0));
    let condition = view.condition(view.condition_id(0).unwrap()).unwrap();
    assert!(matches!(
        condition.operation(),
        ConditionOperation::Discrete(_)
    ));
    let delay = view.delay(view.delay_id(0).unwrap()).unwrap();
    assert!(matches!(
        delay.operation(),
        DelayOperation::ParameterDelay { delay_time } if delay_time.value() == 1.0
    ));
    let expressions = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .collect::<Vec<_>>();
    let clock = expressions
        .iter()
        .find(|expression| {
            matches!(
                expression.operation(),
                ExpressionOperation::Coordinate(CoordinateView::Time)
            )
        })
        .expect("clock coordinate survives");
    assert_eq!(
        clock.provenance().origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::ClockLowering)
    );
    let delay = expressions
        .iter()
        .find(|expression| {
            matches!(
                expression.operation(),
                ExpressionOperation::Coordinate(CoordinateView::Delay(_))
            )
        })
        .expect("delay coordinate survives");
    assert_eq!(
        delay.provenance().origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::DelayLowering)
    );
}

#[test]
fn acyclic_function_rejects_self_call_at_the_exact_occurrence() {
    let source = TestSource::new("function f output Real y; algorithm y := f(); end f;");
    let declaration = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let call_at = source.source("f()", 0);
    let assignment_at = source.source("y := f()", 0);
    let call_span = call_at.span();
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), declaration))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], declaration),
            |dae, reservation| {
                let function = reservation.function();
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let value =
                    dae.expressions(|expressions| expressions.at(call_at).call(function, 0, []))?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, declaration))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, value, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, declaration))
            },
        )
        .map(|_| ())
    })
    .unwrap_err();

    assert_eq!(
        error,
        DaeConstructionError::InvalidFunctionDependency {
            function: 0,
            target: 0,
            span: call_span,
        }
    );
}

#[test]
fn recursive_group_rejects_disconnected_headers() {
    let source = TestSource::new("function f end f; function g end g;");
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let error = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let first = FunctionSignature::new(VarName::new("f"), [], [real], f_at);
        let second = FunctionSignature::new(VarName::new("g"), [], [real], g_at);
        dae.recursive_functions(first, [second], |dae, reservations| {
            for (reservation, provenance) in reservations.into_iter().zip([f_at, g_at]) {
                define_constant_result(dae, reservation, provenance)?;
            }
            Ok(())
        })
        .map(|_| ())
    })
    .unwrap_err();

    assert_eq!(
        error,
        DaeConstructionError::InvalidRecursiveFunctionGroup { span: f_at.span() }
    );
}

fn define_constant_result<'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'_, 'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let output = dae
        .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, provenance))?;
    let zero =
        dae.expressions(|expressions| expressions.at(provenance).literal(DaeLiteral::Real(0.0)))?;
    let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
    dae.functions(|functions| functions.assign(&mut body, output, zero, provenance))?;
    dae.functions(|functions| functions.define(body, provenance))
}

#[test]
fn function_parameters_cannot_cross_or_escape_semantic_owners() {
    let source = TestSource::new("function f end f; function g end g;");
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let result = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let (_, (f_output, f_value, escaped)) = dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], f_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, f_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, f_at)
                })?;
                let value = dae.expressions(|expr| expr.at(f_at).function_parameter(parameter))?;
                let mut body = dae.functions(|functions| functions.begin(reservation, f_at))?;
                dae.functions(|functions| functions.assign(&mut body, output, value, f_at))?;
                let escaped =
                    dae.functions(|functions| functions.current_definition(&body, output, f_at))?;
                dae.functions(|functions| functions.define(body, f_at))?;
                Ok((output, value, escaped))
            },
        )?;
        dae.function(
            FunctionSignature::new(VarName::new("g"), [real], [real], g_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, g_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, g_at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, g_at))?;
                let g_value =
                    dae.expressions(|expr| expr.at(g_at).function_parameter(parameter))?;
                let rejected =
                    dae.functions(|functions| functions.current_definition(&body, f_output, g_at));
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(_),
                        ..
                    })
                ));
                let error = dae.expressions(|expr| {
                    expr.at(g_at).binary(BinaryOperator::Add, f_value, g_value)
                });
                assert!(matches!(
                    error,
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(_),
                        ..
                    })
                ));
                dae.functions(|functions| functions.assign(&mut body, output, g_value, g_at))?;
                dae.functions(|functions| functions.define(body, g_at))
            },
        )?;
        dae.continuous(|continuous| continuous.value_equation(f_at, escaped))
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: None,
            ..
        })
    ));
}

#[test]
fn function_assignment_rejects_a_foreign_target_before_insertion() {
    let source = TestSource::new(
        "function f output Real y; algorithm y := 0; end f; function g output Real y; algorithm y := 0; end g;",
    );
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let rejected_at = source.source("y := 0", 0);
    let accepted_at = source.source("y := 0", 1);
    let zero_at = source.source("0", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let zero =
            dae.expressions(|expressions| expressions.at(zero_at).literal(DaeLiteral::Real(0.0)))?;
        let (g, g_output) = dae.function(
            FunctionSignature::new(VarName::new("g"), [], [real], g_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, g_at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, g_at))?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, accepted_at))?;
                dae.functions(|functions| functions.define(body, g_at))?;
                Ok(output)
            },
        )?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], f_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, f_at)
                })?;
                let f = reservation.function();
                let mut body = dae.functions(|functions| functions.begin(reservation, f_at))?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, g_output, zero, rejected_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(expected),
                        found_function,
                        span,
                    }) if expected == f.index()
                        && found_function == g.index()
                        && span == rejected_at.span()
                ));
                dae.functions(|functions| functions.assign(&mut body, output, zero, accepted_at))?;
                dae.functions(|functions| functions.define(body, f_at))
            },
        )
        .map(|_| ())
    })
    .expect("foreign-target rejection leaves both function bodies usable");

    dae.inspect(|view| {
        assert_eq!(
            view.function(view.function_id(0).unwrap())
                .unwrap()
                .definition_count(),
            1
        );
        assert_eq!(
            view.function(view.function_id(1).unwrap())
                .unwrap()
                .definition_count(),
            1
        );
    });
}

#[test]
fn function_assignment_rejects_a_wrong_typed_rhs_before_insertion() {
    let source = TestSource::new("function f output Real y; algorithm y := true; y := 0; end f;");
    let function_at = source.source("function f", 0);
    let rejected_at = source.source("y := true", 0);
    let accepted_at = source.source("y := 0", 0);
    let true_at = source.source("true", 0);
    let zero_at = source.source("0", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, function_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let (wrong, zero) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(true_at).literal(DaeLiteral::Boolean(true))?,
                        expressions.at(zero_at).literal(DaeLiteral::Real(0.0))?,
                    ))
                })?;

                let rejected = dae
                    .functions(|functions| functions.assign(&mut body, output, wrong, rejected_at));
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::ShapeMismatch { span })
                        if span == rejected_at.span()
                ));

                dae.functions(|functions| functions.assign(&mut body, output, zero, accepted_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("wrong-type rejection leaves the function body usable");

    dae.inspect(|view| {
        assert_eq!(
            view.function(view.function_id(0).unwrap())
                .unwrap()
                .definition_count(),
            1
        );
    });
}

#[test]
fn pure_functions_reject_model_runtime_coordinates_at_the_exact_use_site() {
    let source = TestSource::new(
        "function f output Real y; algorithm y := state_x; y := time; y := delay(state_x, 1); y := 0; end f;",
    );
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let state_at = source.source("state_x", 0);
    let time_at = source.source("time", 0);
    let delay_at = source.source("delay(state_x, 1)", 0);
    let delayed_state_at = source.source("state_x", 1);
    let one_at = source.source("1", 0);
    let zero_at = source.source("0", 0);
    let assignment_at = source.source("y := 0", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("state_x"),
                real,
                state_at,
                VariableAttributes::default(),
            )
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let state_use = dae.expressions(|expressions| {
                    expressions
                        .at(state_at)
                        .coordinate(CoordinateInput::State(state))
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, output, state_use, assignment_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionCoordinate {
                        coordinate: "state",
                        span,
                    }) if span == state_at.span()
                ));
                let time = dae.expressions(|expressions| {
                    expressions.at(time_at).coordinate(CoordinateInput::Time)
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, output, time, assignment_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionCoordinate {
                        coordinate: "time",
                        span,
                    }) if span == time_at.span()
                ));
                let (delayed_state, one) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(delayed_state_at)
                            .coordinate(CoordinateInput::State(state))?,
                        expressions.at(one_at).literal(DaeLiteral::Real(1.0))?,
                    ))
                })?;
                let positive =
                    dae.temporal(|temporal| temporal.positive_parameter(one, 1.0, one_at))?;
                let delay = dae.expressions(|expressions| {
                    expressions
                        .at(delay_at)
                        .delay(delayed_state, positive, delay_at)
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, output, delay.expression(), assignment_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionCoordinate {
                        coordinate: "delay",
                        span,
                    }) if span == delay_at.span()
                ));
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("rejected assignments do not mutate the function environment");
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(function.statements().count(), 1);
    });
}

#[test]
fn function_locals_keep_ordered_statements_and_exact_use_provenance() {
    let source = TestSource::new(
        "function f\n input Real u;\n output Real y;\n protected Real z;\nalgorithm\n z := u + 1;\n y := z * 2;\nend f;",
    );
    let function_at = source.source("function f", 0);
    let parameter_at = source.source("input Real u", 0);
    let output_at = source.source("output Real y", 0);
    let local_at = source.source("Real z", 0);
    let first_assignment = source.source("z := u + 1", 0);
    let second_assignment = source.source("y := z * 2", 0);
    let parameter_use = source.source("u", 1);
    let one_at = source.source("1", 0);
    let first_rhs = source.source("u + 1", 0);
    let local_use = source.source("z", 2);
    let two_at = source.source("2", 0);
    let second_rhs = source.source("z * 2", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let local = dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("z"), real, local_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let parameter = dae.expressions(|expressions| {
                    expressions.at(parameter_use).function_parameter(parameter)
                })?;
                let one = dae.expressions(|expressions| {
                    expressions.at(one_at).literal(DaeLiteral::Real(1.0))
                })?;
                let local_definition = dae.expressions(|expressions| {
                    expressions
                        .at(first_rhs)
                        .binary(BinaryOperator::Add, parameter, one)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, local, local_definition, first_assignment)
                })?;
                let local_value =
                    dae.functions(|functions| functions.read(&body, local, local_use))?;
                let two = dae.expressions(|expressions| {
                    expressions.at(two_at).literal(DaeLiteral::Real(2.0))
                })?;
                let output_definition = dae.expressions(|expressions| {
                    expressions
                        .at(second_rhs)
                        .binary(BinaryOperator::Multiply, local_value, two)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, output_definition, second_assignment)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("ordered local definitions construct a complete function");

    dae.inspect(|view| assert_function_local_body(view, local_use));
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| assert_function_local_body(view, local_use));
    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let nodes = forged["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap();
    let local_read = nodes
        .iter_mut()
        .find_map(|node| node.get_mut("function_value"))
        .expect("fixture contains a function-value read");
    local_read["definition_ordinal"] = serde_json::json!(1);
    let error = serde_json::from_value::<Dae>(forged).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("function value 1 reads definition 1, expected Some(0)"),
        "wire reconstruction must reject forged function snapshots: {error}"
    );
}

fn assert_function_local_body(view: DaeView<'_>, local_use: DaeProvenance) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let parameters = function.parameters().collect::<Vec<_>>();
    assert_eq!(parameters.len(), 1);
    assert_eq!(parameters[0].name().as_str(), "u");
    assert_eq!(
        view.source_text(parameters[0].declaration()),
        Some("input Real u"),
        "function inputs retain declaration provenance"
    );
    let values = function.values().collect::<Vec<_>>();
    assert_eq!(values.len(), 2);
    assert_eq!(values[0].name().as_str(), "y");
    assert_eq!(values[0].role(), FunctionValueRole::Output);
    assert_eq!(values[1].name().as_str(), "z");
    assert_eq!(values[1].role(), FunctionValueRole::Local);
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    let result = view
        .expression(function.result_values().rhs(0).unwrap())
        .unwrap();
    let ExpressionOperation::Binary { lhs, .. } = result.operation() else {
        panic!("final function output must retain its checked expression");
    };
    let local = view.expression(lhs).unwrap();
    assert_eq!(local.provenance(), local_use);
    let ExpressionOperation::FunctionValue { value, definition } = local.operation() else {
        panic!("local occurrence must retain its declaration and snapshot definition");
    };
    assert_eq!(value.function(), function.id());
    assert_eq!(value.ordinal(), 1);
    assert_eq!(
        view.source_text(local.provenance()),
        Some("z"),
        "the local read keeps its source occurrence"
    );
    assert!(view.expression(definition.rhs()).is_some());
}

#[test]
fn zeros_is_a_provenance_bearing_checked_array_operation() {
    let source = TestSource::new("Real z[2]; algorithm z := zeros(2);");
    let extent_at = source.source("2", 1);
    let zeros_at = source.source("zeros(2)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(2)))?;
        dae.expressions(|expressions| {
            expressions
                .at(zeros_at)
                .builtin(PureBuiltin::Zeros, [extent])
        })?;
        Ok(())
    })
    .expect("literal zeros dimensions construct a checked array");

    let assert_zeros = |view: DaeView<'_>| {
        let expression = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(expression.value_type().dimensions(), &[2]);
        assert_eq!(view.source_text(expression.provenance()), Some("zeros(2)"));
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Zeros,
                ..
            }
        ));
    };
    dae.inspect(assert_zeros);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_zeros);

    let source = TestSource::new("zeros(-1)");
    let negative_at = source.source("-1", 0);
    let invalid_zeros_at = source.source("zeros(-1)", 0);
    let error = Dae::construct(source.map, |dae| {
        let extent = dae.expressions(|expressions| {
            expressions.at(negative_at).literal(DaeLiteral::Integer(-1))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(invalid_zeros_at)
                .builtin(PureBuiltin::Zeros, [extent])
        })?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
}

#[test]
fn ones_and_fill_are_compact_typed_array_operations() {
    let source = TestSource::new("ones(2, 2); fill(0.5, 3)");
    let ones_at = source.source("ones(2, 2)", 0);
    let fill_at = source.source("fill(0.5, 3)", 0);
    let first_two_at = source.source("2", 0);
    let second_two_at = source.source("2", 1);
    let value_at = source.source("0.5", 0);
    let extent_at = source.source("3", 0);
    let dae = Dae::construct(source.map, |dae| {
        let first_two = dae.expressions(|expressions| {
            expressions.at(first_two_at).literal(DaeLiteral::Integer(2))
        })?;
        let second_two = dae.expressions(|expressions| {
            expressions
                .at(second_two_at)
                .literal(DaeLiteral::Integer(2))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(ones_at)
                .builtin(PureBuiltin::Ones, [first_two, second_two])
        })?;
        let value =
            dae.expressions(|expressions| expressions.at(value_at).literal(DaeLiteral::Real(0.5)))?;
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| {
            expressions
                .at(fill_at)
                .builtin(PureBuiltin::Fill, [value, extent])
        })?;
        Ok(())
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, dimensions, text) in [
            (2, PureBuiltin::Ones, &[2, 2][..], "ones(2, 2)"),
            (5, PureBuiltin::Fill, &[3][..], "fill(0.5, 3)"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
            assert_eq!(expression.value_type().dimensions(), dimensions);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });
}

#[test]
fn identity_is_a_checked_compact_integer_matrix_operation() {
    let source = TestSource::new("Integer i[2,2] = identity(2)");
    let extent_at = source.source("2", 2);
    let identity_at = source.source("identity(2)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(2)))?;
        dae.expressions(|expressions| {
            expressions
                .at(identity_at)
                .builtin(PureBuiltin::Identity, [extent])
        })?;
        Ok(())
    })
    .expect("one non-negative Integer extent constructs identity compactly");

    let assert_identity = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 2, "identity stores no n² payload");
        let expression = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Integer);
        assert_eq!(expression.value_type().dimensions(), &[2, 2]);
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Identity,
                arguments,
            } if arguments.len() == 1
        ));
    };
    dae.inspect(assert_identity);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_identity);
}

#[test]
fn identity_rejects_non_integer_negative_and_extra_extents() {
    let invalid = |literal: DaeLiteral, argument_count: usize| {
        let source = TestSource::new("identity(extent)");
        let at = source.source("identity(extent)", 0);
        Dae::construct(source.map, |dae| {
            let extent =
                dae.expressions(|expressions| expressions.at(at).literal(literal.clone()))?;
            let arguments = vec![extent; argument_count];
            dae.expressions(|expressions| {
                expressions.at(at).builtin(PureBuiltin::Identity, arguments)
            })?;
            Ok(())
        })
    };

    assert!(matches!(
        invalid(DaeLiteral::Real(2.0), 1),
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
    assert!(matches!(
        invalid(DaeLiteral::Integer(-1), 1),
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
    assert!(matches!(
        invalid(DaeLiteral::Integer(2), 2),
        Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 2,
            ..
        })
    ));
}

#[test]
fn vector_is_one_compact_constructor_derived_view_and_round_trips() {
    let source = TestSource::new("parameter Real p[1,3,1]; vector(p); vector(k)");
    let declaration = source.source("parameter Real p[1,3,1]", 0);
    let parameter_use = source.source("p", 1);
    let parameter_vector = source.source("vector(p)", 0);
    let binder_use = source.source("k", 0);
    let binder_vector = source.source("vector(k)", 0);
    let domain_input = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "k".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };
    let dae = Dae::construct(source.map, |dae| {
        let parameter_type = dae.types(|types| {
            types.intern(
                TypeId::new(80),
                ValueType::array(ScalarType::Real, [1, 3, 1]),
                declaration,
            )
        })?;
        let parameter = dae.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                parameter_type,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let domain = dae.domains(|domains| domains.structured(domain_input.clone(), binder_use))?;
        let binder = dae.domains(|domains| domains.binder(domain, 0, binder_use))?;
        dae.expressions(|expressions| {
            let parameter = expressions
                .at(parameter_use)
                .coordinate(CoordinateInput::Parameter(parameter))?;
            expressions
                .at(parameter_vector)
                .builtin(PureBuiltin::Vector, [parameter])?;
            let binder = expressions.at(binder_use).binder(binder)?;
            expressions
                .at(binder_vector)
                .builtin(PureBuiltin::Vector, [binder])?;
            Ok(())
        })
    })
    .expect("ARR-015 operand shapes construct vector views without scalarization");

    let assert_vector = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 4, "each vector remains one node");
        let parameter = view.expression(view.expression_id(0).unwrap()).unwrap();
        let parameter_vector = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(
            parameter_vector.value_type().scalar_type(),
            ScalarType::Real
        );
        assert_eq!(parameter_vector.value_type().dimensions(), &[3]);
        assert_eq!(parameter_vector.variability(), parameter.variability());
        assert_eq!(parameter_vector.binder_domain(), parameter.binder_domain());
        assert!(matches!(
            parameter_vector.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Vector,
                arguments,
            } if arguments.len() == 1
        ));

        let binder = view.expression(view.expression_id(2).unwrap()).unwrap();
        let binder_vector = view.expression(view.expression_id(3).unwrap()).unwrap();
        assert_eq!(
            binder_vector.value_type().scalar_type(),
            ScalarType::Integer
        );
        assert_eq!(binder_vector.value_type().dimensions(), &[1]);
        assert_eq!(binder_vector.variability(), binder.variability());
        assert_eq!(binder_vector.binder_domain(), binder.binder_domain());
    };
    dae.inspect(assert_vector);

    assert_eq!(
        bincode::serialize(&PureBuiltin::Vector).unwrap(),
        38_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 26);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"vector\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_vector);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn vector_rejects_forged_arity_and_two_non_unit_dimensions() {
    let invalid_arity = |count: usize| {
        let source = TestSource::new("vector(A)");
        let at = source.source("vector(A)", 0);
        Dae::construct(source.map, |dae| {
            let value =
                dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
            dae.expressions(|expressions| {
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Vector, vec![value; count])
            })?;
            Ok(())
        })
    };
    assert!(matches!(
        invalid_arity(0),
        Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 0,
            ..
        })
    ));
    assert!(matches!(
        invalid_arity(2),
        Err(DaeConstructionError::InvalidArity {
            expected: 1,
            found: 2,
            ..
        })
    ));

    let source = TestSource::new("vector(A)");
    let at = source.source("vector(A)", 0);
    let error = Dae::construct(source.map, |dae| {
        let matrix =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [2, 3]), at))?;
        let input = dae.variables(|variables| {
            variables.input(
                VarName::new("A"),
                matrix,
                InputVariability::Continuous,
                at,
                VariableAttributes::default(),
            )
        })?;
        let input = dae.expressions(|expressions| {
            expressions.at(at).coordinate(CoordinateInput::Input(input))
        })?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Vector, [input]))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
}

#[test]
fn vector_rejects_record_roots_until_aggregate_dimensions_are_explicit() {
    let source = TestSource::new("vector(Pair(1.0, 2))");
    let at = source.source("vector(Pair(1.0, 2))", 0);
    let error = Dae::construct(source.map, |dae| {
        let (real, integer) = dae.types(|types| {
            Ok((
                types.derived(ValueType::scalar(ScalarType::Real), at)?,
                types.derived(ValueType::scalar(ScalarType::Integer), at)?,
            ))
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("x"), real), (VarName::new("n"), integer)],
                at,
            )
        })?;
        let pair = dae.expressions(|expressions| {
            let x = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            let n = expressions.at(at).literal(DaeLiteral::Integer(2))?;
            expressions.at(at).record(pair, [x, n])
        })?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Vector, [pair]))?;
        Ok(())
    });

    assert!(matches!(
        error,
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
}

#[test]
fn transpose_swaps_only_the_first_two_axes_and_round_trips() {
    let source = TestSource::new(
        "input Real m[2,3]; input Real t[2,3,4]; input Real z[0,3]; transpose(m); transpose(t); transpose(z)",
    );
    let at = source.source("transpose(m)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let (matrix, tensor, empty) = dae.types(|types| {
            Ok((
                types.derived(ValueType::array(ScalarType::Real, [2, 3]), at)?,
                types.derived(ValueType::array(ScalarType::Real, [2, 3, 4]), at)?,
                types.derived(ValueType::array(ScalarType::Real, [0, 3]), at)?,
            ))
        })?;
        let (m, t, z) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("m"),
                    matrix,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("t"),
                    tensor,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("z"),
                    empty,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        dae.expressions(|expressions| {
            for input in [
                CoordinateInput::Input(m),
                CoordinateInput::Input(t),
                CoordinateInput::Input(z),
            ] {
                let input = expressions.at(at).coordinate(input)?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Transpose, [input])?;
            }
            Ok(())
        })
    })
    .expect("ARR-038 admits compact primitive arrays of rank two or greater");

    let assert_layout = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 6);
        for (index, expected) in [(1, &[3, 2][..]), (3, &[3, 2, 4]), (5, &[3, 0])] {
            let transpose = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(transpose.value_type().scalar_type(), ScalarType::Real);
            assert_eq!(transpose.value_type().dimensions(), expected);
            assert!(matches!(
                transpose.operation(),
                ExpressionOperation::Builtin {
                    builtin: PureBuiltin::Transpose,
                    arguments,
                } if arguments.len() == 1
            ));
        }
    };
    dae.inspect(assert_layout);

    assert_eq!(
        bincode::serialize(&PureBuiltin::Transpose).unwrap(),
        39_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 26);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"transpose\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_layout);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    decoded.inspect(assert_layout);
}

#[test]
fn transpose_rejects_forged_arity_rank_and_record_roots() {
    let invalid_arity = |count: usize| {
        let source = TestSource::new("transpose(A)");
        let at = source.source("transpose(A)", 0);
        Dae::construct(source.map, |dae| {
            let value =
                dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
            dae.expressions(|expressions| {
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Transpose, vec![value; count])
            })?;
            Ok(())
        })
    };
    for (count, found) in [(0, 0), (2, 2)] {
        assert!(matches!(
            invalid_arity(count),
            Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found: actual,
                ..
            }) if actual == found
        ));
    }

    for dimensions in [Vec::new(), vec![3]] {
        let source = TestSource::new("transpose(A)");
        let at = source.source("transpose(A)", 0);
        let error = Dae::construct(source.map, |dae| {
            let ty = dae.types(|types| {
                types.derived(ValueType::array(ScalarType::Real, dimensions.clone()), at)
            })?;
            let input = dae.variables(|variables| {
                variables.input(
                    VarName::new("A"),
                    ty,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )
            })?;
            let input = dae.expressions(|expressions| {
                expressions.at(at).coordinate(CoordinateInput::Input(input))
            })?;
            dae.expressions(|expressions| {
                expressions.at(at).builtin(PureBuiltin::Transpose, [input])
            })?;
            Ok(())
        });
        assert!(matches!(
            error,
            Err(DaeConstructionError::ShapeMismatch { .. })
        ));
    }

    let source = TestSource::new("transpose(Pair(1.0, 2.0))");
    let at = source.source("transpose(Pair(1.0, 2.0))", 0);
    let error = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("x"), real), (VarName::new("y"), real)],
                at,
            )
        })?;
        let pair = dae.expressions(|expressions| {
            let x = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
            let y = expressions.at(at).literal(DaeLiteral::Real(2.0))?;
            expressions.at(at).record(pair, [x, y])
        })?;
        dae.expressions(|expressions| expressions.at(at).builtin(PureBuiltin::Transpose, [pair]))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
}

#[test]
fn diagonal_and_outer_product_are_checked_compact_matrix_operations() {
    let source = TestSource::new(
        "input Real d[3]; input Integer lhs[2]; input Real rhs[5]; diagonal(d); outerProduct(lhs, rhs)",
    );
    let at = source.source("outerProduct(lhs, rhs)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let (real3, integer2, real5) = dae.types(|types| {
            Ok((
                types.derived(ValueType::array(ScalarType::Real, [3]), at)?,
                types.derived(ValueType::array(ScalarType::Integer, [2]), at)?,
                types.derived(ValueType::array(ScalarType::Real, [5]), at)?,
            ))
        })?;
        let (d, lhs, rhs) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("d"),
                    real3,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("lhs"),
                    integer2,
                    InputVariability::Discrete,
                    at,
                    VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("rhs"),
                    real5,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        dae.expressions(|expressions| {
            let d = expressions.at(at).coordinate(CoordinateInput::Input(d))?;
            expressions.at(at).builtin(PureBuiltin::Diagonal, [d])?;
            let lhs = expressions.at(at).coordinate(CoordinateInput::Input(lhs))?;
            let rhs = expressions.at(at).coordinate(CoordinateInput::Input(rhs))?;
            expressions
                .at(at)
                .builtin(PureBuiltin::OuterProduct, [lhs, rhs])?;
            Ok(())
        })
    })
    .expect("ARR-041/042 construct exact matrix types from compact vectors");

    let assert_layout = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 5, "neither operation scalarizes");
        let diagonal = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(diagonal.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(diagonal.value_type().dimensions(), &[3, 3]);
        assert!(matches!(
            diagonal.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Diagonal,
                arguments,
            } if arguments.len() == 1
        ));
        let outer = view.expression(view.expression_id(4).unwrap()).unwrap();
        assert_eq!(outer.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(outer.value_type().dimensions(), &[2, 5]);
        assert!(matches!(
            outer.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::OuterProduct,
                arguments,
            } if arguments.len() == 2
        ));
    };
    dae.inspect(assert_layout);

    assert_eq!(
        bincode::serialize(&PureBuiltin::Diagonal).unwrap(),
        40_u32.to_le_bytes()
    );
    assert_eq!(
        bincode::serialize(&PureBuiltin::OuterProduct).unwrap(),
        41_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 26);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"diagonal\""));
    assert!(json.contains("\"builtin\":\"outer_product\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_layout);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    decoded.inspect(assert_layout);
}

fn invalid_matrix_builtin_arity(
    builtin: PureBuiltin,
    count: usize,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("builtin(A)");
    let at = source.source("builtin(A)", 0);
    Dae::construct(source.map, |dae| {
        let value =
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
        dae.expressions(|expressions| expressions.at(at).builtin(builtin, vec![value; count]))?;
        Ok(())
    })
}

fn invalid_matrix_builtin_shape(
    builtin: PureBuiltin,
    lhs_dimensions: Vec<u32>,
    rhs_dimensions: Option<Vec<u32>>,
) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("builtin(A, B)");
    let at = source.source("builtin(A, B)", 0);
    Dae::construct(source.map, |dae| {
        let (lhs_type, rhs_type) = dae.types(|types| {
            let lhs = types.derived(ValueType::array(ScalarType::Real, lhs_dimensions), at)?;
            let rhs = match rhs_dimensions {
                Some(dimensions) => {
                    Some(types.derived(ValueType::array(ScalarType::Real, dimensions), at)?)
                }
                None => None,
            };
            Ok((lhs, rhs))
        })?;
        let (lhs, rhs) = dae.variables(|variables| {
            let lhs = variables.input(
                VarName::new("A"),
                lhs_type,
                InputVariability::Continuous,
                at,
                VariableAttributes::default(),
            )?;
            let rhs = match rhs_type {
                Some(ty) => Some(variables.input(
                    VarName::new("B"),
                    ty,
                    InputVariability::Continuous,
                    at,
                    VariableAttributes::default(),
                )?),
                None => None,
            };
            Ok((lhs, rhs))
        })?;
        dae.expressions(|expressions| {
            let lhs = expressions.at(at).coordinate(CoordinateInput::Input(lhs))?;
            let arguments = match rhs {
                Some(rhs) => vec![
                    lhs,
                    expressions.at(at).coordinate(CoordinateInput::Input(rhs))?,
                ],
                None => vec![lhs],
            };
            expressions.at(at).builtin(builtin, arguments)
        })?;
        Ok(())
    })
}

#[test]
fn diagonal_and_outer_product_reject_forged_arity_rank_and_element_type() {
    let assert_invalid_arity = |builtin, expected, count| {
        let result = invalid_matrix_builtin_arity(builtin, count);
        assert!(matches!(
            result,
            Err(DaeConstructionError::InvalidArity {
                expected: actual,
                found,
                ..
            }) if actual == expected && found == count
        ));
    };
    assert_invalid_arity(PureBuiltin::Diagonal, 1, 0);
    assert_invalid_arity(PureBuiltin::Diagonal, 1, 2);
    assert_invalid_arity(PureBuiltin::OuterProduct, 2, 0);
    assert_invalid_arity(PureBuiltin::OuterProduct, 2, 1);

    assert!(matches!(
        invalid_matrix_builtin_shape(PureBuiltin::Diagonal, vec![2, 2], None),
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
    assert!(matches!(
        invalid_matrix_builtin_shape(PureBuiltin::OuterProduct, vec![2, 2], Some(vec![3])),
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));
    assert!(matches!(
        invalid_matrix_builtin_shape(PureBuiltin::OuterProduct, vec![2], Some(vec![3, 1])),
        Err(DaeConstructionError::ShapeMismatch { .. })
    ));

    let source = TestSource::new("diagonal({true, false})");
    let at = source.source("diagonal({true, false})", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let yes = expressions.at(at).literal(DaeLiteral::Boolean(true))?;
            let no = expressions.at(at).literal(DaeLiteral::Boolean(false))?;
            let vector = expressions.at(at).array([yes, no])?;
            expressions
                .at(at)
                .builtin(PureBuiltin::Diagonal, [vector])?;
            Ok(())
        })
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::ExpectedNumeric { .. })
    ));
}

#[test]
fn skew_is_one_checked_compact_real_matrix_operation_and_round_trips() {
    let source = TestSource::new("input Real x[3]; skew(x)");
    let at = source.source("skew(x)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let vector =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [3]), at))?;
        let x = dae.variables(|variables| {
            variables.input(
                VarName::new("x"),
                vector,
                InputVariability::Continuous,
                at,
                VariableAttributes::default(),
            )
        })?;
        dae.expressions(|expressions| {
            let x = expressions.at(at).coordinate(CoordinateInput::Input(x))?;
            expressions.at(at).builtin(PureBuiltin::Skew, [x])?;
            Ok(())
        })
    })
    .expect("ARR-037 constructs skew from exactly one Real 3-vector");

    let assert_skew = |view: DaeView<'_>| {
        assert_eq!(view.expression_count(), 2, "skew stores no 3x3 payload");
        let skew = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(skew.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(skew.value_type().dimensions(), &[3, 3]);
        assert!(matches!(
            skew.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Skew,
                arguments,
            } if arguments.len() == 1
        ));
    };
    dae.inspect(assert_skew);
    assert_eq!(
        bincode::serialize(&PureBuiltin::Skew).unwrap(),
        42_u32.to_le_bytes()
    );
    assert_eq!(DAE_SCHEMA_VERSION, 26);
    let json = serde_json::to_string(&dae).unwrap();
    assert!(json.contains("\"builtin\":\"skew\""));
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(assert_skew);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    decoded.inspect(assert_skew);
}

#[test]
fn skew_rejects_forged_arity_rank_and_non_real_elements() {
    for count in [0, 2] {
        assert!(matches!(
            invalid_matrix_builtin_arity(PureBuiltin::Skew, count),
            Err(DaeConstructionError::InvalidArity {
                expected: 1,
                found,
                ..
            }) if found == count
        ));
    }
    for dimensions in [vec![2], vec![3, 1]] {
        assert!(matches!(
            invalid_matrix_builtin_shape(PureBuiltin::Skew, dimensions, None),
            Err(DaeConstructionError::ShapeMismatch { .. })
        ));
    }

    let invalid_elements = |literals: [DaeLiteral; 3]| {
        let source = TestSource::new("skew({a,b,c})");
        let at = source.source("skew({a,b,c})", 0);
        Dae::construct(source.map, |dae| {
            dae.expressions(|expressions| {
                let values = literals
                    .into_iter()
                    .map(|value| expressions.at(at).literal(value))
                    .collect::<Result<Vec<_>, _>>()?;
                let vector = expressions.at(at).array(values)?;
                expressions.at(at).builtin(PureBuiltin::Skew, [vector])?;
                Ok(())
            })
        })
    };
    assert!(matches!(
        invalid_elements([
            DaeLiteral::Integer(1),
            DaeLiteral::Integer(2),
            DaeLiteral::Integer(3),
        ]),
        Err(DaeConstructionError::TypeMismatch { .. })
    ));
    assert!(matches!(
        invalid_elements([
            DaeLiteral::Boolean(true),
            DaeLiteral::Boolean(false),
            DaeLiteral::Boolean(true),
        ]),
        Err(DaeConstructionError::ExpectedNumeric { .. })
    ));
}

#[test]
fn linspace_and_cross_are_checked_compact_vector_operations() {
    let source = TestSource::new("linspace(2.0, 4.0, 3); cross({1.0,2.0,3.0},{4.0,5.0,6.0})");
    let linspace_at = source.source("linspace(2.0, 4.0, 3)", 0);
    let cross_at = source.source("cross({1.0,2.0,3.0},{4.0,5.0,6.0})", 0);
    let provenances = [
        source.source("2.0", 0),
        source.source("4.0", 0),
        source.source("3", 0),
        source.source("1.0", 0),
        source.source("2.0", 1),
        source.source("3.0", 0),
        source.source("4.0", 1),
        source.source("5.0", 0),
        source.source("6.0", 0),
    ];
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions
                .at(provenances[0])
                .literal(DaeLiteral::Real(2.0))?;
            let stop = expressions
                .at(provenances[1])
                .literal(DaeLiteral::Real(4.0))?;
            let count = expressions
                .at(provenances[2])
                .literal(DaeLiteral::Integer(3))?;
            expressions
                .at(linspace_at)
                .builtin(PureBuiltin::Linspace, [start, stop, count])?;
            let lhs = provenances[3..6]
                .iter()
                .zip([1.0, 2.0, 3.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(cross_at).array(lhs)?;
            let rhs = provenances[6..]
                .iter()
                .zip([4.0, 5.0, 6.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let rhs = expressions.at(cross_at).array(rhs)?;
            expressions
                .at(cross_at)
                .builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, text) in [
            (3, PureBuiltin::Linspace, "linspace(2.0, 4.0, 3)"),
            (12, PureBuiltin::Cross, "cross({1.0,2.0,3.0},{4.0,5.0,6.0})"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().dimensions(), &[3]);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });

    let source = TestSource::new("cross({true,true,true},{true,true,true})");
    let at = source.source("cross({true,true,true},{true,true,true})", 0);
    let rejected = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let values = (0..6)
                .map(|_| expressions.at(at).literal(DaeLiteral::Boolean(true)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(at).array(values[..3].iter().copied())?;
            let rhs = expressions.at(at).array(values[3..].iter().copied())?;
            expressions.at(at).builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    });
    assert!(matches!(
        rejected,
        Err(DaeConstructionError::ExpectedNumeric { .. })
    ));
}

#[test]
fn enumeration_literals_are_canonical_checked_values_and_round_trip() {
    let source = TestSource::new("E.a");
    let literal_at = source.source("E.a", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| expressions.at(literal_at).enumeration_literal(1))?;
        Ok(())
    })
    .expect("positive enumeration ordinals construct");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(
            expression.value_type().scalar_type(),
            ScalarType::Enumeration
        );
        assert!(expression.value_type().dimensions().is_empty());
        assert_eq!(view.source_text(expression.provenance()), Some("E.a"));
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Literal(DaeLiteral::Enumeration(1))
        ));
    });

    let source = TestSource::new("E.invalid");
    let invalid_at = source.source("E.invalid", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| expressions.at(invalid_at).enumeration_literal(0))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::InvalidEnumerationOrdinal { ordinal: 0, .. })
    ));
}

#[test]
fn function_assertion_is_a_checked_call_scoped_statement_and_round_trips() {
    let source = TestSource::new(
        "function f\n output Real y;\nalgorithm\n assert(true, \"valid call\");\n y := 0;\nend f;",
    );
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let assertion_at = source.source("assert(true, \"valid call\")", 0);
    let condition_at = source.source("true", 0);
    let message_at = source.source("\"valid call\"", 0);
    let assignment_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .literal(DaeLiteral::Boolean(true))
                })?;
                let message = dae.expressions(|expressions| {
                    expressions
                        .at(message_at)
                        .literal(DaeLiteral::String("valid call".to_owned()))
                })?;
                dae.functions(|functions| {
                    functions.assertion(&mut body, condition, message, assertion_at)
                })?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        Ok(())
    })
    .expect("a typed top-level function assertion has a checked owner");

    let assert_owner = |view: DaeView<'_>| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        assert_eq!(statements.len(), 2);
        let FunctionStatementView::Assertion {
            condition,
            message,
            provenance,
        } = statements[0].clone()
        else {
            panic!("the first function statement retains the assertion action");
        };
        assert_eq!(
            view.source_text(provenance),
            Some("assert(true, \"valid call\")")
        );
        assert_eq!(
            view.expression(condition)
                .unwrap()
                .value_type()
                .scalar_type(),
            ScalarType::Boolean
        );
        assert_eq!(
            view.expression(message).unwrap().value_type().scalar_type(),
            ScalarType::String
        );
    };
    dae.inspect(assert_owner);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_owner);
}

#[test]
fn function_for_loop_is_a_compact_checked_transition() {
    let source = TestSource::new(
        "function sum3\n output Real y;\n protected Real scratch;\nalgorithm\n y := 0;\n for k in 1:3 loop\n  y := y + k;\n end for;\nend sum3;",
    );
    let function_at = source.source("function sum3", 0);
    let output_at = source.source("output Real y", 0);
    let scratch_at = source.source("Real scratch", 0);
    let initial_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:3 loop", 0);
    let update_at = source.source("y := y + k", 0);
    let y_use_at = source.source("y", 3);
    let k_use_at = source.source("k", 1);
    let update_value_at = source.source("y + k", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("sum3"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let scratch = dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("scratch"), real, scratch_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "k".to_string(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        loop_at,
                    )
                })?;
                let binder = DomainBinderId::from_raw(domain.index(), 0);
                let mut loop_body = dae
                    .functions(|functions| functions.begin_loop(body, domain, [output], loop_at))?;
                let current =
                    dae.functions(|functions| functions.read(loop_body.body(), output, y_use_at))?;
                let k = dae.expressions(|expressions| expressions.at(k_use_at).binder(binder))?;
                let update = dae.expressions(|expressions| {
                    expressions
                        .at(update_value_at)
                        .binary(BinaryOperator::Add, current, k)
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, scratch, update, update_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::IncompleteDefinition {
                        kind: "function loop target",
                        ..
                    })
                ));
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, output, update, update_at)
                })?;
                let body = dae.functions(|functions| functions.finish_loop(loop_body, loop_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("loop-carried function state constructs as a checked fold");

    dae.inspect(assert_sum3_loop);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_sum3_loop);
    assert_invalid_function_loop_wires(&encoded);
}

fn assert_invalid_function_loop_wires(encoded: &str) {
    let mut missing_parameter: serde_json::Value = serde_json::from_str(encoded).unwrap();
    missing_parameter["storage"]["functions"][0]["statements"][1]["for"]["targets"] =
        serde_json::json!([]);
    assert!(
        serde_json::from_value::<Dae>(missing_parameter).is_err(),
        "wire reconstruction rejects a loop operation inconsistent with generated parameters"
    );

    let mut open_initial: serde_json::Value = serde_json::from_str(encoded).unwrap();
    open_initial["storage"]["functions"][0]["statements"][1]["for"]["targets"][0] =
        serde_json::json!(1);
    assert!(
        serde_json::from_value::<Dae>(open_initial).is_err(),
        "wire reconstruction rejects an uninitialized loop-carried local"
    );

    let mut nested_fold: serde_json::Value = serde_json::from_str(encoded).unwrap();
    let outer = nested_fold["storage"]["functions"][0]["statements"][1].clone();
    nested_fold["storage"]["functions"][0]["statements"][1]["for"]["statements"] =
        serde_json::json!([outer]);
    let error = serde_json::from_value::<Dae>(nested_fold).unwrap_err();
    assert!(
        error.to_string().contains("functions.statements.nesting"),
        "wire reconstruction rejects a nested fold that normal construction cannot express: {error}"
    );
}

fn assert_sum3_loop(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    let FunctionStatementView::For {
        fold,
        statements,
        provenance,
    } = statements[1].clone()
    else {
        panic!("second function statement is the compact source loop");
    };
    assert_eq!(view.source_text(provenance), Some("for k in 1:3 loop"));
    assert_eq!(statements.count(), 1);
    let fold = view.function_fold(fold).unwrap();
    assert_eq!(fold.targets().count(), 1);
    assert_eq!(fold.initial_values().len(), 1);
    assert_eq!(fold.update_values().len(), 1);
    assert_eq!(
        view.expression(function.result_values().rhs(0).unwrap())
            .unwrap()
            .kind(),
        ExpressionKind::FunctionFoldOutput
    );
    let update = view
        .expression(fold.update_values().rhs(0).unwrap())
        .unwrap();
    assert_eq!(view.source_text(update.provenance()), Some("y + k"));
}

#[test]
fn function_loop_rejects_duplicate_carried_targets() {
    let source =
        TestSource::new("function f output Real x; algorithm x := 0; for k in 1:2 loop end for;");
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real x", 0);
    let assignment_at = source.source("x := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:2 loop", 0);
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "k".to_string(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        loop_at,
                    )
                })?;
                let _ = dae.functions(|functions| {
                    functions.begin_loop(body, domain, [output, output], loop_at)
                })?;
                Ok(())
            },
        )
        .map(|_| ())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::DuplicateDefinition {
            kind: "function loop target",
            ..
        }
    ));
}
