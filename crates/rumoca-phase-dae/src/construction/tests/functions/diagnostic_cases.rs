use super::*;

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
    let mut function =
        rumoca_core::Function::new("f", rumoca_core::DefId::new(63_115), function_span);
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.dae().inspect(|view| {
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
fn dynamic_quotient_splits_between_its_runtime_owner_and_rejection() {
    // A dynamic dividend over a time-invariant divisor now reaches the
    // checked runtime owner and constructs exactly one discontinuity root;
    // a divisor that varies during simulation keeps the ED018 rejection.
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

    let dae = construct(&model, source.map)
        .expect("a time-invariant divisor owns its checked event surface");
    dae.dae().inspect(|view| {
        assert_eq!(
            view.root_count(),
            1,
            "the runtime quotient owns exactly one discontinuity root"
        );
    });

    let source = TestSource::new("Real x; Real y; x - div(x, y);");
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "x", "Real x", 1, Vec::new(), false);
    add_primitive_variable(&mut model, &source, "y", "Real y", 1, Vec::new(), false);
    let quotient_span = source.span("div(x, y)", 0);
    let equation_span = source.span("x - div(x, y)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(variable_reference(&source, "x", "x", 1, Vec::new())),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Div,
                args: vec![
                    variable_reference(&source, "x", "x", 2, Vec::new()),
                    variable_reference(&source, "y", "y", 1, Vec::new()),
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
         y := 0; for k in 1:n loop assert(k > 0, \"positive\"); y := y + k; end for; end sum3; 1.0 * sum3();",
    );
    let (function, loop_span) = production_sum3_function(&source);
    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    add_sum3_call_equation(&mut model, &source);

    let dae = construct(&model, source.map).unwrap();
    dae.dae()
        .inspect(|view| assert_production_sum3_loop(view, loop_span));
}

fn add_sum3_call_equation(model: &mut flat::Model, source: &TestSource) {
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: call_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
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
        Some("for k in 1:n loop assert(k > 0, \"positive\"); y := y + k; end for")
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

/// A loop bound the body itself computes is not settled at translation time.
///
/// MLS §11.2.2 requires a for-statement's range to be evaluable. The output `y`
/// is written by the algorithm, so no specialization can fold it, and the
/// domain owner must still report that at the range.
#[test]
fn reachable_function_loop_with_runtime_bound_fails_at_domain_owner() {
    let source = TestSource::new(
        "function sumN input Integer n; output Integer y; algorithm \
         y := 0; for k in 1:y loop y := y + k; end for; end sumN; \
         model M equation 0 = sumN(3); end M;",
    );
    let function_span = source.span("function sumN", 0);
    let input_span = source.span("input Integer n", 0);
    let output_span = source.span("output Integer y", 0);
    let initial_span = source.span("y := 0", 0);
    let loop_span = source.span("for k in 1:y loop y := y + k; end for", 0);
    let range_span = source.span("1:y", 0);
    let runtime_bound_span = source.span("y", 2);
    let update_span = source.span("y := y + k", 0);
    let mut function =
        rumoca_core::Function::new("sumN", rumoca_core::DefId::new(63_117), function_span);
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
                        name: Reference::new("y"),
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
                        span: source.span("y", 4),
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = construct(&model, source.map).unwrap_err();
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedFlatSemantics {
                feature,
                span,
                ..
            } if feature == "function loop domain" && *span == range_span
        ),
        "unexpected error: {error:?}"
    );
}

/// A loop bound written over an input is settled by the specialization.
///
/// MLS §11.2.2 requires the range to be evaluable and MLS §12.2 lets a function
/// body be written over its inputs, so `for k in 1:n` under a call that proves
/// `n = 3` is the same three-element compact domain a literal `1:3` gives. A
/// direct accumulator owns that domain as a tensor reduction, not as a carried
/// scalar fold.
#[test]
fn reachable_function_loop_over_a_proven_input_lowers_to_a_tensor_reduction() {
    let source = TestSource::new(
        "function sumN input Integer n; output Integer y; algorithm \
         y := 0; for k in 1:n loop y := y + k; end for; end sumN; 1.0 * sumN(3);",
    );
    let function_span = source.span("function sumN", 0);
    let input_span = source.span("input Integer n", 0);
    let output_span = source.span("output Integer y", 0);
    let initial_span = source.span("y := 0", 0);
    let loop_span = source.span("for k in 1:n loop y := y + k; end for", 0);
    let range_span = source.span("1:n", 0);
    let update_span = source.span("y := y + k", 0);
    let mut function =
        rumoca_core::Function::new("sumN", rumoca_core::DefId::new(63_118), function_span);
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
                        span: source.span("n", 7),
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
    let equation_span = source.span("1.0 * sumN(3)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("sumN"),
                args: vec![Expression::Literal {
                    value: Literal::Integer(3),
                    span: source.span("3", 0),
                }],
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    dae.dae().inspect(assert_proven_input_tensor_reduction);
}

fn assert_proven_input_tensor_reduction(view: dae::DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    assert_eq!(function.fold_count(), 0);
    let mut statements = function.statements();
    let Some(dae::FunctionStatementView::Assignment { definition }) = statements.next() else {
        panic!("the accumulator must lower to one tensor assignment")
    };
    assert!(statements.next().is_none());
    let dae::ExpressionOperation::Binary { rhs, .. } =
        view.expression(definition.rhs()).unwrap().operation()
    else {
        panic!("the seed and tensor reduction must retain their source operator")
    };
    let dae::ExpressionOperation::Builtin {
        builtin: dae::PureBuiltin::Sum,
        arguments,
    } = view.expression(rhs).unwrap().operation()
    else {
        panic!("the loop must own a tensor-native sum")
    };
    let dae::ExpressionOperation::Comprehension { domain, .. } = view
        .expression(arguments.get(0).unwrap())
        .unwrap()
        .operation()
    else {
        panic!("the sum operand must retain its compact comprehension")
    };
    let domain = view.domain(domain).unwrap();
    assert_eq!(domain.scalar_count(), 3);
    assert_eq!(view.source_text(domain.provenance()), Some("1:n"));
}

/// The MLS §12.3 purity prefix the fixture's external declaration writes.
#[derive(Clone, Copy)]
enum DeclaredExternalPurity {
    /// `pure function f … external "C" …`.
    Pure,
    /// `impure function f … external "C" …`.
    Impure,
    /// `function f … external "C" …`: no prefix, the deprecated form.
    Undeclared,
}

fn external_random_model(
    source: &TestSource,
    purity: DeclaredExternalPurity,
    annotations: Vec<rumoca_core::ExternalFunctionAnnotation>,
) -> flat::Model {
    let function_span = source.span("function f", 0);
    let input_span = source.span("input Real p0", 0);
    let output_span = source.span("output Real y0", 0);
    let state_span = source.span("output Real q0", 0);
    let mut function =
        rumoca_core::Function::new("f", rumoca_core::DefId::new(63_119), function_span);
    let (pure, purity_declared) = match purity {
        DeclaredExternalPurity::Pure => (true, true),
        DeclaredExternalPurity::Impure => (false, true),
        DeclaredExternalPurity::Undeclared => (true, false),
    };
    function.pure = pure;
    function.purity_declared = purity_declared;
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
        DeclaredExternalPurity::Pure,
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
    dae.dae().inspect(|view| {
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
    let mut model = external_random_model(&source, DeclaredExternalPurity::Pure, Vec::new());
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
    let mut model = external_random_model(&source, DeclaredExternalPurity::Pure, Vec::new());
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
        DeclaredExternalPurity::Pure,
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
    let mut model = external_random_model(&source, DeclaredExternalPurity::Pure, Vec::new());
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
fn function_body_validation_retains_priority_over_later_clock_refusal() {
    let text = format!("{EXTERNAL_SOURCE_TEXT}\nClock c=Clock(0.0);");
    let source = TestSource::new(&text);
    let function_span = source.span("function f", 0);
    let assignment_span = source.span("y0 = my_random", 0);
    let mut model = external_random_model(&source, DeclaredExternalPurity::Pure, Vec::new());
    let interval_span = source.span("0.0", 0);
    let mut clock = flat::Variable::empty_with_span(source.span("Clock c=Clock(0.0)", 0));
    clock.name = VarName::new("c");
    clock.instance_id = test_instance_id("c");
    clock.component_ref = Some(test_component_reference(
        "c",
        source.span("Clock c=Clock(0.0)", 0),
    ));
    clock.type_id = TypeId::new(97);
    clock.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args: vec![Expression::Literal {
            value: Literal::Real(0.0),
            span: interval_span,
        }],
        span: source.span("Clock(0.0)", 0),
    });
    register_test_clock_type(&mut model, clock.type_id, &clock.dims);
    model.add_variable(clock.name.clone(), clock);
    model
        .variable_type_names
        .insert(VarName::new("c"), "Clock".to_string());

    let later_error = match analyze(&model) {
        Err(error) => error,
        Ok(_) => panic!(
            "analysis unexpectedly accepted the invalid zero clock interval at {interval_span:?}"
        ),
    };
    assert!(matches!(
        later_error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockLattice { span, .. },
            ..
        } if span == interval_span
    ));

    model
        .functions
        .get_mut(&VarName::new("f"))
        .expect("fixture function exists")
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

    let error = construct(&model, source.map)
        .expect_err("function validation is the established earlier refusing owner");
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
    let model = external_random_model(&source, DeclaredExternalPurity::Impure, Vec::new());

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
    let mut model = external_random_model(&source, DeclaredExternalPurity::Impure, Vec::new());
    model.equations.clear();
    model.initial_equations.push(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![Expression::Literal {
                value: Literal::Real(2.5),
                span: source.span("2.5", 0),
            }],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.dae().inspect(|view| {
        let external = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .external()
            .expect("the body is external");
        assert_eq!(external.purity(), dae::FunctionPurity::Impure);
        assert!(external.linkage().libraries().is_empty());
    });
}

/// MLS 3.7 §12.3: a function "shall be treated as impure" when "It is an
/// external function without explicit purity", and writing no prefix "is
/// deprecated" rather than illegal. MLS 3.6 §12.3 (historical; 3.7 deprecates
/// the bare form) said the same in one sentence — "assumed to be impure, but
/// without any restriction on calling them" — and both halves are proven here:
/// the stored body fact is impure, and the fixture's continuous-time call is
/// still accepted.
#[test]
fn external_function_without_a_purity_prefix_is_impure_and_callable_anywhere() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let model = external_random_model(&source, DeclaredExternalPurity::Undeclared, Vec::new());

    let dae = construct(&model, source.map)
        .expect("the deprecated form carries no restriction on calling it");
    dae.dae().inspect(|view| {
        let external = view
            .function(view.function_id(0).unwrap())
            .unwrap()
            .external()
            .expect("the body is external");
        assert_eq!(external.purity(), dae::FunctionPurity::Impure);
    });
}

/// MLS §12.9 defaults an omitted entry point to the function's simple name.
/// Flat keeps only the flattened path, so the omitted form is rejected with
/// exact provenance rather than recovered from rendered text.
#[test]
fn external_function_without_a_declared_entry_point_is_rejected() {
    let source = TestSource::new(EXTERNAL_SOURCE_TEXT);
    let function_span = source.span("function f", 0);
    let mut model = external_random_model(&source, DeclaredExternalPurity::Pure, Vec::new());
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

#[test]
fn automatic_function_vectorization_constructs_one_compact_domain() {
    let source = TestSource::new(
        "function f input Real u; output Real y; algorithm y := u; end f; \
         parameter Real result[3] = f({1.0,2.0,3.0});",
    );
    let mut model = test_model();
    let mut function = identity_function(
        &source,
        real_function_param("u", Vec::new(), source.span("input Real u", 0)),
        real_function_param("y", Vec::new(), source.span("output Real y", 0)),
    );
    function.transitively_non_replaceable = true;
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")]
        .instance_id
        .expect("Flat assigns the selected function an exact instance");
    add_primitive_variable(
        &mut model,
        &source,
        "result",
        "parameter Real result[3]",
        907,
        vec![3],
        false,
    );
    let call_span = source.span("f({1.0,2.0,3.0})", 0);
    let result = model.variables.get_mut(&VarName::new("result")).unwrap();
    result.variability = Variability::Parameter(Default::default());
    result.binding = Some(Expression::FunctionCall {
        name: Reference::new("f").with_resolved_function(ResolvedFunctionReference {
            instance_id: instance,
            base_part_count: 1,
            transitively_non_replaceable: true,
        }),
        args: vec![Expression::Array {
            elements: [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| Expression::Literal {
                    value: Literal::Real(value),
                    span: call_span,
                })
                .collect(),
            is_matrix: false,
            span: call_span,
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: call_span,
    });

    let dae = construct(&model, source.map)
        .expect("automatic vectorization has a checked compact DAE owner");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let parameter = function.parameters().next().unwrap();
        assert!(
            view.value_type(parameter.value_type())
                .unwrap()
                .dimensions()
                .is_empty(),
            "the specialization keeps the declared scalar parameter"
        );

        let result = view
            .variables()
            .map(|(_, variable)| variable)
            .find(|variable| variable.name().as_str() == "result")
            .unwrap();
        let binding = view.expression(result.binding().unwrap()).unwrap();
        assert_eq!(binding.value_type().dimensions(), [3]);
        let dae::ExpressionOperation::Comprehension { domain, body } = binding.operation() else {
            panic!("vectorization must remain one compact comprehension owner")
        };
        assert_eq!(view.domain(domain).unwrap().extents(), [3]);
        let body = view.expression(body).unwrap();
        let dae::ExpressionOperation::Call { arguments, .. } = body.operation() else {
            panic!("the compact body is one exact scalar function call")
        };
        let argument = view.expression(arguments.get(0).unwrap()).unwrap();
        assert_eq!(argument.kind(), dae::ExpressionKind::Index);
        assert!(argument.value_type().dimensions().is_empty());
        assert_eq!(body.provenance().span(), call_span);
    });
}

/// Build the `sum3` function fixture, returning it with the loop span the
/// assertion needs.
///
/// Extracted from the test body so the assembly and its assertion stay
/// adjacent; the statements and their order are unchanged.
fn production_sum3_function(source: &TestSource) -> (rumoca_core::Function, rumoca_core::Span) {
    let function_span = source.span("function sum3", 0);
    let output_span = source.span("output Integer y", 0);
    let local_span = source.span("Integer n = 3", 0);
    let initial_span = source.span("y := 0", 0);
    let loop_span = source.span(
        "for k in 1:n loop assert(k > 0, \"positive\"); y := y + k; end for",
        0,
    );
    let range_span = source.span("1:n", 0);
    let assertion_span = source.span("assert(k > 0, \"positive\")", 0);
    let update_span = source.span("y := y + k", 0);
    let mut function =
        rumoca_core::Function::new("sum3", rumoca_core::DefId::new(63_116), function_span);
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
            equations: vec![
                rumoca_core::Statement::Assert {
                    condition: Expression::Binary {
                        op: OpBinary::Gt,
                        lhs: Box::new(Expression::VarRef {
                            name: Reference::new("k"),
                            subscripts: Vec::new(),
                            span: source.span("k", 1),
                        }),
                        rhs: Box::new(Expression::Literal {
                            value: Literal::Integer(0),
                            span: source.span("0", 1),
                        }),
                        span: source.span("k > 0", 0),
                    },
                    message: Box::new(Expression::Literal {
                        value: Literal::String("positive".to_owned()),
                        span: source.span("\"positive\"", 0),
                    }),
                    level: None,
                    span: assertion_span,
                },
                rumoca_core::Statement::Assignment {
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
                            span: source.span("k", 2),
                        }),
                        span: source.span("y + k", 0),
                    },
                    span: update_span,
                },
            ],
            span: loop_span,
        },
    ];
    (function, loop_span)
}
