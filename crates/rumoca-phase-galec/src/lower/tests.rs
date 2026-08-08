use rumoca_core::{SourceMap, Span, VarName};

use super::*;

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
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
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
        let lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        let argument = view.expression_id(0).unwrap();
        let direct = lowerer
            .direct_aggregate_function_argument(argument)
            .unwrap();
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
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        lowerer.function_scope = Some(function.id());
        let direct = lowerer
            .direct_aggregate_function_argument(argument)
            .unwrap();
        assert!(matches!(
            direct,
            Some(gast::Expression::Ref(gast::Reference::Local(part)))
                if part.name.lexeme() == "y"
                    && part.subscripts.is_empty()
                    && part.span == span
        ));
    });
}

#[test]
fn single_aggregate_function_result_writes_its_checked_destination_directly() {
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

    model.inspect(|view| {
        let caller = view.function(view.function_id(1).unwrap()).unwrap();
        let call = caller
            .statements()
            .find_map(|statement| match statement {
                dae::FunctionStatementView::Assignment { definition } => {
                    matches!(
                        view.expression(definition.rhs()).unwrap().operation(),
                        dae::ExpressionOperation::Call { .. }
                    )
                    .then_some(definition.rhs())
                }
                _ => None,
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        lowerer.function_scope = Some(caller.id());
        let statement = lowerer
            .lower_direct_aggregate_call_assignment(call, gast::Name::ident("y"), span)
            .unwrap()
            .unwrap();
        assert!(lowerer.take_prefix_statements().is_empty());
        assert!(matches!(
            statement.node,
            gast::Statement::MultiAssignment { targets, .. }
                if matches!(targets.as_slice(), [gast::Reference::Local(part)] if part.name.lexeme() == "y")
        ));
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
        let classified = classify_variables(view).unwrap();
        assert!(classified.iter().any(|variable| {
            variable.class == VariableClass::Output && variable.variable.name().as_str() == "y"
        }));
        let by_id = classified
            .iter()
            .map(|variable| (variable.id.index(), variable.clone()))
            .collect::<HashMap<_, _>>();
        let mut statements = Vec::new();
        let mut locals = Vec::new();
        causal_outputs::append_causal_assignments(
            view,
            &classified,
            &by_id,
            &HashMap::new(),
            &mut locals,
            &mut statements,
        )
        .unwrap();
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
        let variables = HashMap::new();
        let previous = HashMap::new();
        let Err(rejected) = ExpressionLowerer::new(view, &variables, &previous).lower(call) else {
            panic!("an assertion needs an explicit call-scoped action sink")
        };
        assert!(matches!(
            rejected,
            GalecTargetError::UnsupportedFeature { feature, .. }
                if feature == "function-assertion"
        ));
        let mut lowerer = ExpressionLowerer::with_assertions(view, &variables, &previous);
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
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);

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
    let dependent = gast::Spanned::dummy(gast::Statement::For(gast::ForLoop {
        iterator: Some(gast::Name::ident("inner")),
        start: gast::Expression::Integer(1),
        step: None,
        stop: gast::Expression::Integer(3),
        body: vec![gast::Spanned::dummy(gast::Statement::Assignment {
            target: gast::Reference::local(accumulator),
            value: gast::Expression::Ref(gast::Reference::local(outer.clone())),
        })],
    }));
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
            condition: gast::Condition::Expression(gast::Expression::Ref(gast::Reference::local(
                accepted,
            ))),
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

    let second_loop = gast::Spanned::dummy(gast::Statement::For(gast::ForLoop {
        iterator: Some(gast::Name::ident("j")),
        start: gast::Expression::Integer(1),
        step: None,
        stop: gast::Expression::Integer(3),
        body: second_body,
    }));
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
            dae::FunctionStatementView::AssignmentGroup { definitions }
                if definitions.len() == 2
        ));
        let lowered = user_functions::lower_reachable(view, HashSet::from([caller.index()]))
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
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
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
        let contraction_loop = gast::Spanned::dummy(gast::Statement::For(gast::ForLoop {
            iterator: Some(gast::Name::ident("inner")),
            start: gast::Expression::Integer(1),
            step: None,
            stop: gast::Expression::Integer(3),
            body: vec![gast::Spanned::dummy(gast::Statement::Assignment {
                target: gast::Reference::local(contraction.clone()),
                value: gast::Expression::Ref(gast::Reference::local(outer.clone())),
            })],
        }));
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

#[test]
fn scalar_conditional_branches_do_not_share_materialized_calls() {
    let mut sources = SourceMap::new();
    let text = "function f input Real u; output Real y; algorithm y := u; end f; if true then f(1.0) else f(1.0)";
    let source = sources.add("conditional-scalar-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (function, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [real], [real], provenance),
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
        let (condition, argument) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        let call = dae
            .expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, call)], call)
        })?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let conditional = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Conditional(_)
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        lowerer.lower(conditional).unwrap();
        let prefix = lowerer.take_prefix_statements();
        let gast::Statement::If(selection) = &prefix[0].node else {
            panic!("the conditional must dominate both function calls")
        };
        let gast::Statement::MultiAssignment {
            targets: branch_targets,
            ..
        } = &selection.branches[0].body[0].node
        else {
            panic!("the selected branch must materialize its call")
        };
        let gast::Statement::MultiAssignment {
            targets: fallback_targets,
            ..
        } = &selection.else_body.as_ref().unwrap()[0].node
        else {
            panic!("the fallback branch must materialize its call")
        };
        assert_ne!(branch_targets, fallback_targets);
    });
}

#[test]
fn atomic_multi_output_group_materializes_one_guarded_call() {
    let mut sources = SourceMap::new();
    let text = "function pair input Real u; output Real a; output Real b; algorithm a := u; b := u; end pair; function caller output Real x; output Real y; algorithm (x, y) := pair(1.0); end caller";
    let source = sources.add("atomic-multi-output.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (pair, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("pair"), [real], [real, real], provenance),
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
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let value = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                dae.functions(|functions| functions.assign(&mut body, first, value, provenance))?;
                dae.functions(|functions| functions.assign(&mut body, second, value, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("caller"), [], [real, real], provenance),
            |dae, reservation| {
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, provenance)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 1, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let (condition, argument, zero) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Boolean(true))?,
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(1.0))?,
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(0.0))?,
                    ))
                })?;
                let first_call = dae.expressions(|expressions| {
                    expressions.at(provenance).call(pair, 0, [argument])
                })?;
                let second_call = dae.expressions(|expressions| {
                    expressions.at(provenance).call(pair, 1, [argument])
                })?;
                let first_value = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .conditional([(condition, first_call)], zero)
                })?;
                let second_value = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .conditional([(condition, second_call)], zero)
                })?;
                dae.functions(|functions| {
                    functions.assign_all(
                        &mut body,
                        &[(first, first_value), (second, second_value)],
                        provenance,
                    )
                })?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        Ok(())
    })
    .unwrap();

    assert_atomic_multi_output_group(&model);
}

#[test]
fn record_field_of_checked_function_call_is_projected_before_scalar_lowering() {
    let mut sources = SourceMap::new();
    let text = "record Pair Real left; Real right; end Pair; function makePair input Real u; output Pair p; algorithm p := Pair(u, u); end makePair; makePair(2.0).right";
    let source = sources.add("record-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("makePair"), [real], [pair], provenance);
        let (function, ()) = dae.function(signature, |dae, reservation| {
            let parameter = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("p"), 0, provenance)
            })?;
            dae.functions(|functions| {
                functions.local(&reservation, VarName::new("p.left"), real, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let fields = dae.expressions(|expressions| {
                Ok([
                    expressions.at(provenance).function_parameter(parameter)?,
                    expressions.at(provenance).function_parameter(parameter)?,
                ])
            })?;
            let value =
                dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
            dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        })?;
        let argument = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(2.0))
        })?;
        let call = dae
            .expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        dae.expressions(|expressions| expressions.at(provenance).field(call, 1))?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let function = view.function_id(0).unwrap();
        let lowered_functions =
            user_functions::lower_reachable(view, HashSet::from([function.index()])).unwrap();
        assert_eq!(lowered_functions.len(), 1);
        let field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { field: 1, .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let lowered = ExpressionLowerer::new(view, &variables, &previous)
            .lower(field)
            .unwrap();
        assert_eq!(lowered.scalar_type, gast::ScalarType::Real);
        assert_eq!(lowered.expression, gast::Expression::Real(2.0));

        let mut materialized = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        let selected = materialized.lower(field).unwrap().expression;
        let prefix = materialized.take_prefix_statements();
        assert_eq!(prefix.len(), 1);
        let gast::Statement::MultiAssignment { targets, .. } = &prefix[0].node else {
            panic!("one record call must become one multi-output assignment")
        };
        assert_eq!(targets.len(), 2);
        assert!(matches!(
            selected,
            gast::Expression::Ref(gast::Reference::Local(ref selected))
                if matches!(&targets[1], gast::Reference::Local(target)
                    if target.name.lexeme() == selected.name.lexeme())
        ));
    });
}

#[test]
fn function_identity_assignments_are_not_emitted() {
    let mut sources = SourceMap::new();
    let source = sources.add("identity-assignment.mo", "function makePair");
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 17)).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let signature = dae::FunctionSignature::new(
            VarName::new("makePair"),
            std::iter::empty(),
            [pair],
            provenance,
        );
        dae.function(signature, |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("result"), 0, provenance)
            })?;
            let left = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("result.left"), real, provenance)
            })?;
            let right = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("result.right"), real, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let values = dae.expressions(|expressions| {
                Ok([
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(1.0))?,
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(2.0))?,
                ])
            })?;
            dae.functions(|functions| functions.assign(&mut body, left, values[0], provenance))?;
            dae.functions(|functions| functions.assign(&mut body, right, values[1], provenance))?;
            let fields = dae.functions(|functions| {
                Ok([
                    functions.read(&body, left, provenance)?,
                    functions.read(&body, right, provenance)?,
                ])
            })?;
            let fields =
                dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
            dae.functions(|functions| functions.assign(&mut body, output, fields, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        })?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let function = view.function_id(0).unwrap();
        let lowered = user_functions::lower_reachable(view, HashSet::from([function.index()]))
            .expect("identity assignment lowers");
        assert_eq!(lowered.len(), 1);
        assert_eq!(lowered[0].statements.len(), 2);
        assert!(lowered[0].statements.iter().all(|statement| {
            !matches!(
                &statement.node,
                gast::Statement::Assignment {
                    target: gast::Reference::Local(target),
                    value: gast::Expression::Ref(gast::Reference::Local(value)),
                } if target == value
            )
        }));
    });
}

fn conditional_record_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "record Pair Real left; Real right; end Pair; function makePair input Real u; output Pair p; algorithm p := Pair(u, u); end makePair; (if true then makePair(2.0) else Pair(0.0, 0.0)).right";
    let source = sources.add("conditional-record-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("makePair"), [real], [pair], provenance);
        let (function, ()) = dae.function(signature, |dae, reservation| {
            let parameter = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("p"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let fields = dae.expressions(|expressions| {
                Ok([
                    expressions.at(provenance).function_parameter(parameter)?,
                    expressions.at(provenance).function_parameter(parameter)?,
                ])
            })?;
            let value =
                dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
            dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        })?;
        let (condition, argument, zero) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(2.0))?,
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(0.0))?,
            ))
        })?;
        let call = dae
            .expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        let fallback =
            dae.expressions(|expressions| expressions.at(provenance).record(pair, [zero, zero]))?;
        let selection = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, call)], fallback)
        })?;
        dae.expressions(|expressions| expressions.at(provenance).field(selection, 1))?;
        let shared_call_selection = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, call)], call)
        })?;
        dae.expressions(|expressions| expressions.at(provenance).field(shared_call_selection, 0))?;
        let shared_call_field =
            dae.expressions(|expressions| expressions.at(provenance).field(call, 0))?;
        dae.expressions(|expressions| {
            expressions.at(provenance).array([
                shared_call_field,
                shared_call_field,
                shared_call_field,
            ])
        })?;
        Ok(())
    })
    .unwrap()
}

fn assert_dynamic_array_record_calls_are_lazy(view: dae::DaeView<'_>) {
    let array = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .find(|id| {
            matches!(
                view.expression(*id).unwrap().operation(),
                dae::ExpressionOperation::Array(_)
            )
        })
        .unwrap();
    let index_name = gast::Name::ident("i");
    let dynamic_index = gast::Expression::Ref(gast::Reference::local(index_name.clone()));
    let variables = HashMap::new();
    let previous = HashMap::new();
    let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
    lowerer.loop_index_bounds.push(LoopIndexBound {
        name: index_name,
        minimum: 1,
        maximum: 3,
    });
    lowerer.lower_at(array, &[dynamic_index]).unwrap();
    let prefix = lowerer.take_prefix_statements();
    let gast::Statement::If(first) = &prefix[0].node else {
        panic!("the array selection must dominate its first call")
    };
    assert!(matches!(
        first.branches[0].body[0].node,
        gast::Statement::MultiAssignment { .. }
    ));
    let first_else = first.else_body.as_ref().unwrap();
    let gast::Statement::If(second) = &first_else[0].node else {
        panic!("the array selection must dominate its second call")
    };
    assert!(matches!(
        second.branches[0].body[0].node,
        gast::Statement::MultiAssignment { .. }
    ));
    assert!(matches!(
        second.else_body.as_ref().unwrap()[0].node,
        gast::Statement::MultiAssignment { .. }
    ));
}

#[test]
fn conditional_record_call_is_materialized_only_in_its_selected_branch() {
    let model = conditional_record_fixture();
    model.inspect(|view| {
        let field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { field: 1, .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        let selected = lowerer.lower(field).unwrap();
        let prefix = lowerer.take_prefix_statements();

        assert_eq!(selected.scalar_type, gast::ScalarType::Real);
        assert_eq!(prefix.len(), 1);
        let gast::Statement::If(selection) = &prefix[0].node else {
            panic!("the conditional must dominate its record call")
        };
        assert!(matches!(
            selection.branches[0].body[0].node,
            gast::Statement::MultiAssignment { .. }
        ));
        assert!(
            prefix.iter().all(|statement| !matches!(
                statement.node,
                gast::Statement::MultiAssignment { .. }
            ))
        );

        let shared_call_field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { field: 0, .. }
                )
            })
            .unwrap();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(view, &variables, &previous);
        lowerer.lower(shared_call_field).unwrap();
        let prefix = lowerer.take_prefix_statements();
        let gast::Statement::If(selection) = &prefix[0].node else {
            panic!("the conditional must dominate both uses of its shared call")
        };
        assert!(matches!(
            selection.branches[0].body[0].node,
            gast::Statement::MultiAssignment { .. }
        ));
        assert!(matches!(
            selection.else_body.as_ref().unwrap()[0].node,
            gast::Statement::MultiAssignment { .. }
        ));

        assert_dynamic_array_record_calls_are_lazy(view);
    });
}

#[test]
fn sum_reduction_projects_the_tensor_in_row_major_order() {
    let mut sources = SourceMap::new();
    let text = "sum({1.0, 2.0, 3.0})";
    let source = sources.add("sum.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let array = expressions.at(provenance).array(values)?;
            expressions
                .at(provenance)
                .builtin(dae::PureBuiltin::Sum, [array])?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let sum = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Builtin {
                        builtin: dae::PureBuiltin::Sum,
                        ..
                    }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &variables, &previous);
        let expected = gast::Expression::binary(
            gast::BinaryOp::Add,
            gast::Expression::binary(
                gast::BinaryOp::Add,
                gast::Expression::Real(1.0),
                gast::Expression::Real(2.0),
            ),
            gast::Expression::Real(3.0),
        );
        assert_eq!(lowerer.lower(sum).unwrap().expression, expected);
    });
}

#[test]
fn array_update_projects_updated_and_historical_elements() {
    let mut sources = SourceMap::new();
    let source = sources.add("array-update.mo", "x[2] := 9.0");
    let span = Span::from_offsets(source, 0, 11);
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let one = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let two = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let three = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(3.0))?;
            let values = [one, two, three];
            let base = expressions.at(provenance).array(values)?;
            let index = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(2))?;
            let value = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(9.0))?;
            expressions.at(provenance).array_update(
                base,
                value,
                [dae::Subscript::Index {
                    expression: index,
                    provenance,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let update = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::ArrayUpdate { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &variables, &previous);
        assert_eq!(
            lowerer.lower_element(update, &[1]).unwrap().expression,
            gast::Expression::Real(1.0)
        );
        assert_eq!(
            lowerer.lower_element(update, &[2]).unwrap().expression,
            gast::Expression::Real(9.0)
        );
        assert_eq!(
            lowerer.lower_element(update, &[3]).unwrap().expression,
            gast::Expression::Real(3.0)
        );
    });
}

#[test]
fn comprehension_projects_checked_binder_values() {
    let mut sources = SourceMap::new();
    let text = "{i for i in -1:2:3}";
    let source = sources.add("comprehension.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_owned(),
                        lower: -1,
                        upper: 3,
                        step: 2,
                    }],
                },
                provenance,
            )
        })?;
        let binder = dae.domains(|domains| domains.binder(domain, 0, provenance))?;
        let body = dae.expressions(|expressions| expressions.at(provenance).binder(binder))?;
        dae.expressions(|expressions| {
            expressions.at(provenance).comprehension(domain, body)?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let comprehension = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Comprehension { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &variables, &previous);
        for (ordinal, expected) in [(1, -1), (2, 1), (3, 3)] {
            assert_eq!(
                lowerer
                    .lower_element(comprehension, &[ordinal])
                    .unwrap()
                    .expression,
                gast::Expression::Integer(expected)
            );
        }
    });
}
