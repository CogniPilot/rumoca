use rumoca_core::{Reference, TypeId};

use super::super::*;
use super::support::*;

fn assert_ed007_without_borrowed_span(error: &ToDaeError, expected_context: &str) {
    let diagnostic = rumoca_core::PhaseError::to_diagnostic(error);
    assert_eq!(
        diagnostic.code.as_deref(),
        Some("ED007"),
        "unexpected error: {error:?}"
    );
    assert!(
        diagnostic.labels.is_empty(),
        "missing occurrence provenance must not borrow an enclosing source label"
    );
    assert_eq!(error.source_span(), None);
    assert!(matches!(
        error,
        ToDaeError::MissingProvenance { owner } if owner.contains(expected_context)
    ));
}

fn identity_function(
    source: &TestSource,
    input: rumoca_core::FunctionParam,
    output: rumoca_core::FunctionParam,
) -> rumoca_core::Function {
    let assignment_span = source.span("y := u", 0);
    let mut function = rumoca_core::Function::new("f", source.span("function f", 0));
    function.add_input(input);
    function.add_output(output);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", assignment_span),
        value: Expression::VarRef {
            name: Reference::new("u"),
            subscripts: Vec::new(),
            span: source.span("u", 1),
        },
        span: assignment_span,
    });
    function
}

fn add_function_call(model: &mut flat::Model, source: &TestSource, argument: Expression) {
    let call_span = source.span("f(", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![argument],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

fn assert_function_scalar_types(dae: &dae::Dae, expected: dae::ScalarType) {
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let parameter = function.parameters().next().unwrap();
        assert_eq!(
            view.value_type(parameter.value_type())
                .unwrap()
                .scalar_type(),
            expected
        );
        let output = function
            .values()
            .find(|value| value.role() == dae::FunctionValueRole::Output)
            .unwrap();
        assert_eq!(
            view.value_type(output.value_type()).unwrap().scalar_type(),
            expected
        );
    });
}

#[test]
fn real_alias_function_values_use_canonical_identity_not_display_name() {
    let source = TestSource::new(
        "function f input Voltage u; output Voltage y; algorithm y := u; end f; f(1.0);",
    );
    let alias = TypeId::new(901);
    let input = real_alias_function_param(
        "u",
        "Voltage",
        alias,
        Vec::new(),
        source.span("input Voltage u", 0),
    );
    let output = real_alias_function_param(
        "y",
        "Voltage",
        alias,
        Vec::new(),
        source.span("output Voltage y", 0),
    );
    assert_eq!(input.type_name, "Voltage");
    assert_ne!(
        input.effective_type.nominal_type(),
        input.effective_type.canonical_type()
    );
    let mut model = test_model();
    model.add_function(identity_function(&source, input, output));
    model.is_partial = true;
    add_function_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
    );

    let dae = construct(&model, source.map).expect("canonical Real alias constructs");
    assert_function_scalar_types(&dae, dae::ScalarType::Real);
}

#[test]
fn enumeration_function_values_use_registered_canonical_identity() {
    let source = TestSource::new(
        "input Color c; output Color d; function f input Color u; output Color y; algorithm y := u; end f; d = f(c);",
    );
    let enumeration = TypeId::new(902);
    let input = enumeration_function_param(
        "u",
        "Color",
        enumeration,
        Vec::new(),
        source.span("input Color u", 0),
    );
    let output = enumeration_function_param(
        "y",
        "Color",
        enumeration,
        Vec::new(),
        source.span("output Color y", 0),
    );
    let mut model = test_model();
    register_test_enumeration_type(&mut model, enumeration);
    let declaration_span = source.span("input Color c", 0);
    let mut variable = flat::Variable::empty_with_span(declaration_span);
    variable.name = VarName::new("c");
    variable.instance_id = test_instance_id("c");
    variable.component_ref = Some(test_component_reference("c", declaration_span));
    variable.type_id = enumeration;
    variable.variability = Variability::Discrete(Default::default());
    variable.causality = Causality::Input(Default::default());
    variable.is_discrete_type = true;
    variable.is_primitive = true;
    model.effective_types.insert(
        enumeration,
        rumoca_core::EffectiveType::new(enumeration, enumeration, Vec::new()).unwrap(),
    );
    model.enumeration_types.insert(enumeration);
    model.add_variable(variable.name.clone(), variable);
    let output_span = source.span("output Color d", 0);
    let mut output_variable = flat::Variable::empty_with_span(output_span);
    output_variable.name = VarName::new("d");
    output_variable.instance_id = test_instance_id("d");
    output_variable.component_ref = Some(test_component_reference("d", output_span));
    output_variable.type_id = enumeration;
    output_variable.variability = Variability::Discrete(Default::default());
    output_variable.causality = Causality::Output(Default::default());
    output_variable.is_discrete_type = true;
    output_variable.is_primitive = true;
    model.add_variable(output_variable.name.clone(), output_variable);
    model.add_function(identity_function(&source, input, output));
    model.is_partial = true;
    let equation_span = source.span("d = f(c)", 0);
    let call_span = source.span("f(c)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("d"),
                subscripts: Vec::new(),
                span: source.span("d", 1),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("f"),
                args: vec![Expression::VarRef {
                    name: test_reference("c"),
                    subscripts: Vec::new(),
                    span: source.span("c", 1),
                }],
                is_constructor: false,
                span: call_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).expect("registered enumeration constructs");
    assert_function_scalar_types(&dae, dae::ScalarType::Enumeration);
}

#[test]
fn user_class_named_real_is_not_a_predefined_scalar() {
    let source =
        TestSource::new("function f input Real u; output Real y; algorithm y := u; end f; f(1.0);");
    let user_real = TypeId::new(903);
    let input_span = source.span("input Real u", 0);
    let input = function_param("u", "Real", user_real, user_real, Vec::new(), input_span);
    let output = function_param(
        "y",
        "Real",
        user_real,
        user_real,
        Vec::new(),
        source.span("output Real y", 0),
    );
    let mut model = test_model();
    model.add_function(identity_function(&source, input, output));
    model.is_partial = true;
    add_function_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
    );

    let error = construct(&model, source.map).expect_err("display spelling cannot mint Real");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "function value type"
            && detail.contains("unsupported type `Real`")
            && span == input_span
    ));
}

#[test]
fn executable_external_object_constructor_reaches_lifecycle_boundary() {
    let source = TestSource::new(
        "function constructor\n  input Real seed;\n  output Handle handle;\n  external \"C\" handle = make_handle(seed);\nend constructor;\nHandle(1.0);",
    );
    let function_span = source.span(
        "constructor\n  input Real seed;\n  output Handle handle;\n  external \"C\" handle = make_handle(seed);\nend constructor",
        0,
    );
    let input_span = source.span("input Real seed", 0);
    let output_span = source.span("output Handle handle", 0);
    let external_arg_span = source.span("seed", 1);
    let call_span = source.span("Handle(1.0)", 0);
    let literal_span = source.span("1.0", 0);

    let mut constructor = rumoca_core::Function::new("Handle", function_span);
    constructor.add_input(real_function_param("seed", Vec::new(), input_span));
    constructor.add_output(function_param(
        "handle",
        "Handle",
        TypeId::new(900),
        TypeId::new(900),
        Vec::new(),
        output_span,
    ));
    constructor.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("make_handle".to_string()),
        output_name: Some("handle".to_string()),
        args: vec![Expression::VarRef {
            name: Reference::new("seed"),
            subscripts: Vec::new(),
            span: external_arg_span,
        }],
        annotations: Vec::new(),
    });

    let mut model = test_model();
    model.add_function(constructor);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("Handle"),
            args: vec![Expression::Literal {
                value: Literal::Real(1.0),
                span: literal_span,
            }],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    // MLS §12.9 external interfaces are now constructible, so the rejection
    // moves to the exact boundary the ExternalObject actually lacks: `Handle`
    // has no checked DAE lifecycle value type. The declaration span is the
    // output that names it, not the enclosing function.
    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "function value type"
            && detail == "`Handle.handle` has unsupported type `Handle`"
            && span == output_span
    ));
}

fn nested_assert_function_model(source: &TestSource, assertion_span: Span) -> flat::Model {
    let function_span = source.span("function f", 0);
    let output_span = source.span("output Real y", 0);
    let conditional_span = source.span("if true then assert(true, \"bad\"); end if", 0);
    let mut function = rumoca_core::Function::new("f", function_span);
    function.add_output(real_function_param("y", Vec::new(), output_span));
    function.body = vec![rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            },
            stmts: vec![rumoca_core::Statement::Assert {
                condition: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 1),
                },
                message: Box::new(Expression::Literal {
                    value: Literal::String("bad".to_string()),
                    span: source.span("\"bad\"", 0),
                }),
                level: None,
                span: assertion_span,
            }],
        }],
        else_block: None,
        span: conditional_span,
    }];

    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    let call_span = source.span("f()", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: Vec::new(),
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

#[test]
fn nested_algorithm_statement_without_span_fails_ed007() {
    let source = TestSource::new("model M algorithm if true then break; end if; end M;");
    let conditional_span = source.span("if true then break; end if", 0);
    let mut model = test_model();
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![rumoca_core::Statement::Break { span: Span::DUMMY }],
            }],
            else_block: None,
            span: conditional_span,
        }],
        source.span("algorithm if true then break; end if", 0),
        "algorithm section",
    ));
    model.is_partial = true;

    let error = construct(&model, source.map).expect_err("the nested break has no exact span");
    assert_ed007_without_borrowed_span(&error, "model algorithm");
}

#[test]
fn nested_unsupported_algorithm_statement_uses_its_exact_span() {
    let source = TestSource::new("model M algorithm if true then break; end if; end M;");
    let conditional_span = source.span("if true then break; end if", 0);
    let break_span = source.span("break", 0);
    let mut model = test_model();
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![rumoca_core::Statement::Break { span: break_span }],
            }],
            else_block: None,
            span: conditional_span,
        }],
        source.span("algorithm if true then break; end if", 0),
        "algorithm section",
    ));
    model.is_partial = true;

    let error = construct(&model, source.map).expect_err("break is not a checked DAE owner");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedAlgorithm { span, .. } if span == break_span
    ));
}

#[test]
fn nested_function_statement_without_span_fails_ed007() {
    let source = TestSource::new(
        "function f output Real y; algorithm if true then assert(true, \"bad\"); end if; end f; f();",
    );
    let model = nested_assert_function_model(&source, Span::DUMMY);

    let error =
        construct(&model, source.map).expect_err("the nested function assertion has no exact span");
    assert_ed007_without_borrowed_span(&error, "function body");
}

#[test]
fn production_lowering_preserves_function_locals_and_statement_order() {
    let source = TestSource::new(
        "function f input Real u; output Real y; protected Real z; algorithm z := u + 1.0; y := z * 2.0; end f; f(1.0);",
    );
    let function_span = source.span("function f", 0);
    let input_span = source.span("input Real u", 0);
    let output_span = source.span("output Real y", 0);
    let local_span = source.span("Real z", 0);
    let first_span = source.span("z := u + 1.0", 0);
    let second_span = source.span("y := z * 2.0", 0);
    let mut function = rumoca_core::Function::new("f", function_span);
    function.add_input(real_function_param("u", Vec::new(), input_span));
    function.add_output(real_function_param("y", Vec::new(), output_span));
    function.add_local(real_function_param("z", Vec::new(), local_span));
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("z", first_span),
            value: Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(Expression::VarRef {
                    name: Reference::new("u"),
                    subscripts: Vec::new(),
                    span: source.span("u", 1),
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Real(1.0),
                    span: source.span("1.0", 0),
                }),
                span: source.span("u + 1.0", 0),
            },
            span: first_span,
        },
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", second_span),
            value: Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(Expression::VarRef {
                    name: Reference::new("z"),
                    subscripts: Vec::new(),
                    span: source.span("z", 2),
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Real(2.0),
                    span: source.span("2.0", 0),
                }),
                span: source.span("z * 2.0", 0),
            },
            span: second_span,
        },
    ];
    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    let call_span = source.span("f(1.0)", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 1),
            }],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let values = function.values().collect::<Vec<_>>();
        assert_eq!(values.len(), 2);
        assert_eq!(values[0].name().as_str(), "y");
        assert_eq!(values[0].role(), dae::FunctionValueRole::Output);
        assert_eq!(values[1].name().as_str(), "z");
        assert_eq!(values[1].role(), dae::FunctionValueRole::Local);
        assert_eq!(function.statements().count(), 2);
        let result = view
            .expression(function.result_values().rhs(0).unwrap())
            .unwrap();
        let dae::ExpressionOperation::Binary { lhs, .. } = result.operation() else {
            panic!("output retains the second assignment expression");
        };
        let local_use = view.expression(lhs).unwrap();
        assert_eq!(view.source_text(local_use.provenance()), Some("z"));
        assert!(matches!(
            local_use.operation(),
            dae::ExpressionOperation::FunctionValue { .. }
        ));
    });
}

#[test]
fn dynamic_quotient_fails_at_its_runtime_operator_owner() {
    let source = TestSource::new("Real x; x - div(x, 2);");
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "x", "Real x", 1, Vec::new(), false);
    let quotient_span = source.span("div(x, 2)", 0);
    let equation_span = source.span("x - div(x, 2)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(variable_reference(&source, "x", "x", 1, Vec::new())),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Div,
                args: vec![
                    variable_reference(&source, "x", "x", 2, Vec::new()),
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: source.span("2", 0),
                    },
                ],
                span: quotient_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model.is_partial = true;

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedRuntimeOperator {
            operator,
            span,
            ..
        } if operator == "div" && span == quotient_span
    ));
}

#[test]
fn production_lowering_constructs_a_compact_checked_function_loop() {
    let source = TestSource::new(
        "function sum3 output Integer y; protected Integer n = 3; algorithm \
         y := 0; for k in 1:n loop y := y + k; end for; end sum3; 1.0 * sum3();",
    );
    let function_span = source.span("function sum3", 0);
    let output_span = source.span("output Integer y", 0);
    let local_span = source.span("Integer n = 3", 0);
    let initial_span = source.span("y := 0", 0);
    let loop_span = source.span("for k in 1:n loop y := y + k; end for", 0);
    let range_span = source.span("1:n", 0);
    let update_span = source.span("y := y + k", 0);
    let mut function = rumoca_core::Function::new("sum3", function_span);
    function.add_output(integer_function_param("y", Vec::new(), output_span));
    function.add_local(
        integer_function_param("n", Vec::new(), local_span).with_default(Expression::Literal {
            value: Literal::Integer(3),
            span: source.span("3", 1),
        }),
    );
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", initial_span),
            value: Expression::Literal {
                value: Literal::Integer(0),
                span: source.span("0", 0),
            },
            span: initial_span,
        },
        rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "k".to_string(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: source.span("1", 0),
                    }),
                    step: None,
                    end: Box::new(Expression::VarRef {
                        name: Reference::new("n"),
                        subscripts: Vec::new(),
                        span: source.span("n", 2),
                    }),
                    span: range_span,
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: test_component_reference("y", update_span),
                value: Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(Expression::VarRef {
                        name: Reference::new("y"),
                        subscripts: Vec::new(),
                        span: source.span("y", 3),
                    }),
                    rhs: Box::new(Expression::VarRef {
                        name: Reference::new("k"),
                        subscripts: Vec::new(),
                        span: source.span("k", 1),
                    }),
                    span: source.span("y + k", 0),
                },
                span: update_span,
            }],
            span: loop_span,
        },
    ];
    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    let call_span = source.span("sum3()", 0);
    let equation_span = source.span("1.0 * sum3()", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("sum3"),
                args: Vec::new(),
                is_constructor: false,
                span: call_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| assert_production_sum3_loop(view, loop_span));
}

fn assert_production_sum3_loop(view: dae::DaeView<'_>, loop_span: Span) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    assert_eq!(function.fold_count(), 1);
    assert_eq!(function.statements().count(), 3);
    let fold = view
        .function_fold(function.fold_id(0).unwrap())
        .expect("function owns its compact fold");
    let domain = view.domain(fold.domain()).unwrap();
    assert_eq!(domain.scalar_count(), 3);
    assert_eq!(view.source_text(domain.provenance()), Some("1:n"));
    assert_eq!(
        view.source_text(fold.provenance()),
        Some("for k in 1:n loop y := y + k; end for")
    );
    let parameter = view
        .expression(fold.parameter_values().rhs(0).unwrap())
        .unwrap();
    assert_eq!(
        parameter.provenance().origin(),
        dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::FunctionLoopLowering)
    );
    assert_eq!(parameter.provenance().span(), loop_span);
    let update = view
        .expression(fold.update_values().rhs(0).unwrap())
        .unwrap();
    assert_eq!(view.source_text(update.provenance()), Some("y + k"));
    let result = view
        .expression(function.result_values().rhs(0).unwrap())
        .unwrap();
    assert_eq!(result.kind(), dae::ExpressionKind::FunctionFoldOutput);
    assert_eq!(
        result.provenance().origin(),
        dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::FunctionLoopLowering)
    );
}

#[test]
fn reachable_function_loop_with_runtime_bound_fails_at_domain_owner() {
    let source = TestSource::new(
        "function sumN input Integer n; output Integer y; algorithm \
         y := 0; for k in 1:n loop y := y + k; end for; end sumN; \
         model M equation 0 = sumN(3); end M;",
    );
    let function_span = source.span("function sumN", 0);
    let input_span = source.span("input Integer n", 0);
    let output_span = source.span("output Integer y", 0);
    let initial_span = source.span("y := 0", 0);
    let loop_span = source.span("for k in 1:n loop y := y + k; end for", 0);
    let range_span = source.span("1:n", 0);
    let runtime_bound_span = source.span("n", 7);
    let update_span = source.span("y := y + k", 0);
    let mut function = rumoca_core::Function::new("sumN", function_span);
    function.add_input(integer_function_param("n", Vec::new(), input_span));
    function.add_output(integer_function_param("y", Vec::new(), output_span));
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", initial_span),
            value: Expression::Literal {
                value: Literal::Integer(0),
                span: source.span("0", 0),
            },
            span: initial_span,
        },
        rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "k".to_string(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: source.span("1", 0),
                    }),
                    step: None,
                    end: Box::new(Expression::VarRef {
                        name: Reference::new("n"),
                        subscripts: Vec::new(),
                        span: runtime_bound_span,
                    }),
                    span: range_span,
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: test_component_reference("y", update_span),
                value: Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(Expression::VarRef {
                        name: Reference::new("y"),
                        subscripts: Vec::new(),
                        span: source.span("y", 3),
                    }),
                    rhs: Box::new(Expression::VarRef {
                        name: Reference::new("k"),
                        subscripts: Vec::new(),
                        span: source.span("k", 1),
                    }),
                    span: source.span("y + k", 0),
                },
                span: update_span,
            }],
            span: loop_span,
        },
    ];
    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    let call_span = source.span("sumN(3)", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("sumN"),
            args: vec![Expression::Literal {
                value: Literal::Integer(3),
                span: source.span("3", 0),
            }],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "function loop domain" && span == range_span
    ));
}

fn external_random_model(
    source: &TestSource,
    pure: bool,
    annotations: Vec<rumoca_core::ExternalFunctionAnnotation>,
) -> flat::Model {
    let function_span = source.span("function f", 0);
    let input_span = source.span("input Real p0", 0);
    let output_span = source.span("output Real y0", 0);
    let state_span = source.span("output Real q0", 0);
    let mut function = rumoca_core::Function::new("f", function_span);
    function.pure = pure;
    function.add_input(real_function_param("p0", Vec::new(), input_span));
    function.add_output(real_function_param("y0", Vec::new(), output_span));
    function.add_output(real_function_param("q0", Vec::new(), state_span));
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("my_random".to_string()),
        output_name: Some("y0".to_string()),
        args: vec![
            Expression::VarRef {
                name: Reference::new("p0"),
                subscripts: Vec::new(),
                span: source.span("p0", 1),
            },
            Expression::VarRef {
                name: Reference::new("q0"),
                subscripts: Vec::new(),
                span: source.span("q0", 1),
            },
        ],
        annotations,
    });

    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    let call_span = source.span("f(2.5)", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![Expression::Literal {
                value: Literal::Real(2.5),
                span: source.span("2.5", 0),
            }],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

const EXTERNAL_SOURCE_TEXT: &str = "function f\n  input Real p0;\n  output Real y0;\n  output Real q0;\n  external \"C\" y0 = my_random(p0, q0);\nend f;\nf(2.5);";

#[test]
fn pure_external_function_lowers_as_a_purity_bearing_callable() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let annotation_span = source.span("my_random", 0);
    let model = external_random_model(
        &source,
        true,
        vec![rumoca_core::ExternalFunctionAnnotation {
            name: vec!["Library".to_string()],
            value: Expression::Literal {
                value: Literal::String("ModelicaExternalC".to_string()),
                span: annotation_span,
            },
            span: annotation_span,
        }],
    );

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert!(function.is_external());
        assert_eq!(function.statements().count(), 0);
        let external = function.external().expect("the body is external");
        assert_eq!(external.purity(), dae::FunctionPurity::Pure);
        assert_eq!(external.language(), dae::ExternalLanguage::C);
        assert_eq!(external.symbol().as_str(), "my_random");
        assert_eq!(external.linkage().libraries(), ["ModelicaExternalC"]);
        let arguments = external.arguments().collect::<Vec<_>>();
        let dae::ExternalArgumentView::Input(argument) = arguments[0] else {
            panic!("the first ABI position reads the declared formal");
        };
        let lowered = view.expression(argument).unwrap();
        assert_eq!(view.source_text(lowered.provenance()), Some("p0"));
        assert!(matches!(arguments[1], dae::ExternalArgumentView::Output(_)));
        assert!(external.result().is_some());
    });
}

#[test]
fn external_function_with_an_unproduced_output_is_rejected() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let state_span = source.span("output Real q0", 0);
    let mut model = external_random_model(&source, true, Vec::new());
    model
        .functions
        .get_mut(&VarName::new("f"))
        .unwrap()
        .external
        .as_mut()
        .unwrap()
        .args
        .truncate(1);

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "external function interface"
            && detail.contains("output `q0` that its external body never produces")
            && span == state_span
    ));
}

#[test]
fn external_function_with_an_undefined_language_is_rejected() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let function_span = source.span("function f", 0);
    let mut model = external_random_model(&source, true, Vec::new());
    model
        .functions
        .get_mut(&VarName::new("f"))
        .unwrap()
        .external
        .as_mut()
        .unwrap()
        .language = "Rust".to_string();

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "external function language" && span == function_span
    ));
}

#[test]
fn external_function_link_facts_must_be_string_literals() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let annotation_span = source.span("my_random", 0);
    let model = external_random_model(
        &source,
        true,
        vec![rumoca_core::ExternalFunctionAnnotation {
            name: vec!["Library".to_string()],
            value: Expression::Literal {
                value: Literal::Real(1.0),
                span: annotation_span,
            },
            span: annotation_span,
        }],
    );

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "external function link facts" && span == annotation_span
    ));
}

#[test]
fn external_function_with_both_bodies_is_rejected() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let function_span = source.span("function f", 0);
    let assignment_span = source.span("y0 = my_random", 0);
    let mut model = external_random_model(&source, true, Vec::new());
    model
        .functions
        .get_mut(&VarName::new("f"))
        .unwrap()
        .body
        .push(rumoca_core::Statement::Assignment {
            comp: test_component_reference("y0", assignment_span),
            value: Expression::VarRef {
                name: Reference::new("p0"),
                subscripts: Vec::new(),
                span: source.span("p0", 1),
            },
            span: assignment_span,
        });

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "function lifecycle"
            && detail.contains("both an algorithm body and an external interface")
            && span == function_span
    ));
}

#[test]
fn impure_call_from_a_continuous_equation_is_rejected() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let call_span = source.span("f(2.5)", 0);
    let model = external_random_model(&source, false, Vec::new());

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "impure call context"
            && detail.contains("called from a continuous-time equation")
            && span == call_span
    ));
}

/// MLS §12.3 permits an impure call in an initial equation. The interface
/// keeps its declared impurity there instead of being silently promoted.
#[test]
fn impure_external_function_keeps_its_declared_purity_in_an_initial_equation() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let call_span = source.span("f(2.5)", 0);
    let mut model = external_random_model(&source, false, Vec::new());
    model.equations.clear();
    model.initial_equations.push(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![Expression::Literal {
                value: Literal::Real(2.5),
                span: source.span("2.5", 0),
            }],
            is_constructor: false,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let external = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .external()
            .expect("the body is external");
        assert_eq!(external.purity(), dae::FunctionPurity::Impure);
        assert!(external.linkage().libraries().is_empty());
    });
}

/// MLS §12.9 defaults an omitted entry point to the function's simple name.
/// Flat keeps only the flattened path, so the omitted form is rejected with
/// exact provenance rather than recovered from rendered text.
#[test]
fn external_function_without_a_declared_entry_point_is_rejected() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let function_span = source.span("function f", 0);
    let mut model = external_random_model(&source, true, Vec::new());
    model
        .functions
        .get_mut(&VarName::new("f"))
        .unwrap()
        .external
        .as_mut()
        .unwrap()
        .function_name = None;

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "external function entry point" && span == function_span
    ));
}
