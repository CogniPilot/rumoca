use rumoca_core::{Reference, SourceId, TypeId};

use super::*;

struct TestSource {
    map: SourceMap,
    source: SourceId,
    text: String,
}

impl TestSource {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("direct_todae.mo", text);
        Self {
            map,
            source,
            text: text.to_string(),
        }
    }

    fn span(&self, needle: &str, occurrence: usize) -> Span {
        let start = self
            .text
            .match_indices(needle)
            .nth(occurrence)
            .map(|(start, _)| start)
            .unwrap();
        Span::from_offsets(self.source, start, start + needle.len())
    }
}

fn scalar_real_model(source: &TestSource) -> flat::Model {
    let declaration = source.span("Real x", 0);
    let use_span = source.span("x", 1);
    let literal_span = source.span("1.0", 0);
    let equation_span = source.span("x - 1.0", 0);
    let mut model = flat::Model::new();
    let mut variable = flat::Variable::empty_with_span(declaration);
    variable.name = VarName::new("x");
    variable.type_id = TypeId::new(7);
    variable.variability = Variability::Continuous(Default::default());
    variable.is_primitive = true;
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new("x"), "Real".to_string());
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: Reference::new("x"),
                subscripts: Vec::new(),
                span: use_span,
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: literal_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

fn source_priority_when_model(source: &TestSource) -> flat::Model {
    let first_condition_span = source.span("true", 0);
    let second_condition_span = source.span("true", 2);
    let first_assignment_span = source.span("m = true", 0);
    let second_assignment_span = source.span("m = false", 0);
    let mut model = flat::Model::new();
    add_primitive_variable(
        &mut model,
        source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    let mut first = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: first_condition_span,
        },
        first_condition_span,
    );
    first.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 1),
        },
        first_assignment_span,
        "first branch",
    ));
    let mut second = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: second_condition_span,
        },
        second_condition_span,
    );
    second.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(false),
            span: source.span("false", 0),
        },
        second_assignment_span,
        "second branch",
    ));
    let mut chain = flat::WhenChain::new(
        first,
        source.span(
            "when true then m = true; elsewhen true then m = false; end when",
            0,
        ),
    );
    chain.push_else_when(second);
    model.when_chains.push(chain);
    model
}

fn duplicate_when_model(source: &TestSource, nested: bool) -> flat::Model {
    let condition_span = source.span("when true", 0);
    let first_span = source.span("m = true", 0);
    let second_span = source.span("m = false", 0);
    let mut model = flat::Model::new();
    add_primitive_variable(
        &mut model,
        source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 1),
        },
        first_span,
        "first definition",
    ));
    let second = flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(false),
            span: source.span("false", 0),
        },
        second_span,
        "second definition",
    );
    if nested {
        branch.add_equation(flat::WhenEquation::conditional(
            vec![(
                Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("if true", 0),
                },
                vec![second],
            )],
            None,
            source.span("if true then m = false; end if", 0),
            "nested second definition",
        ));
    } else {
        branch.add_equation(second);
    }
    model
        .when_chains
        .push(flat::WhenChain::new(branch, source.span("when true", 0)));
    model
}

#[test]
fn production_lowering_enters_only_through_construct() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; end M;");
    let model = scalar_real_model(&source);
    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        assert_eq!(view.continuous_equation_count(), 1);
        let variable = view.variable(view.variable_id(0).unwrap()).unwrap();
        assert_eq!(variable.role(), dae::VariableRole::Algebraic);
        let equation = view.continuous_equation(0).unwrap();
        assert_eq!(equation.provenance().span(), model.equations[0].span);
        assert_eq!(
            view.expression(equation.residual())
                .unwrap()
                .provenance()
                .origin(),
            dae::DaeProvenanceOrigin::Source
        );
    });
}

#[test]
fn when_chain_lowers_source_priority_with_exact_branch_provenance() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; elsewhen true then m = false; end when; end M;",
    );
    let second_condition_span = source.span("true", 2);
    let model = source_priority_when_model(&source);
    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.event_action_count(), 2);
        let first = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        let second = view.event_action(view.event_action_id(1).unwrap()).unwrap();
        assert_eq!(view.source_text(first.provenance()), Some("m = true"));
        assert_eq!(view.source_text(second.provenance()), Some("m = false"));
        assert_eq!(first.guard(), first.trigger());
        assert_ne!(second.guard(), second.trigger());

        let guard = view.condition(second.guard()).unwrap();
        assert_eq!(guard.provenance().span(), second_condition_span);
        assert_eq!(
            guard.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::ConditionLowering)
        );
        let dae::ConditionOperation::And(branch_trigger, no_previous) = guard.operation() else {
            panic!("later branch guard must combine its trigger with source priority");
        };
        assert_eq!(branch_trigger, second.trigger());
        let negated = view.condition(no_previous).unwrap();
        assert_eq!(negated.provenance().span(), second_condition_span);
        assert_eq!(
            negated.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::ConditionLowering)
        );
        assert!(matches!(
            negated.operation(),
            dae::ConditionOperation::Not(previous) if previous == first.trigger()
        ));
    });
}

#[test]
fn malformed_flat_when_branch_rejects_direct_duplicate_at_second_definition() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; m = false; end when; end M;",
    );
    let second_span = source.span("m = false", 0);
    let model = duplicate_when_model(&source, false);
    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == second_span && detail.contains("`m`")
    ));
}

#[test]
fn malformed_flat_when_branch_rejects_nested_duplicate_at_inner_definition() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; if true then m = false; end if; end when; end M;",
    );
    let second_span = source.span("m = false", 0);
    let model = duplicate_when_model(&source, true);
    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == second_span && detail.contains("`m`")
    ));
}

#[test]
fn malformed_flat_rejects_second_independent_when_owner_for_one_target() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; end when; \
         when false then m = false; end when; end M;",
    );
    let mut model = flat::Model::new();
    add_primitive_variable(
        &mut model,
        &source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    for (condition, assignment, owner, value) in [
        (
            "when true",
            "m = true",
            "when true then m = true; end when",
            true,
        ),
        (
            "when false",
            "m = false",
            "when false then m = false; end when",
            false,
        ),
    ] {
        let condition_span = source.span(condition, 0);
        let assignment_span = source.span(assignment, 0);
        let mut branch = flat::WhenBranch::new(
            Expression::Literal {
                value: Literal::Boolean(value),
                span: condition_span,
            },
            condition_span,
        );
        branch.add_equation(flat::WhenEquation::assign(
            VarName::new("m"),
            Expression::Literal {
                value: Literal::Boolean(value),
                span: assignment_span,
            },
            assignment_span,
            assignment,
        ));
        model
            .when_chains
            .push(flat::WhenChain::new(branch, source.span(owner, 0)));
    }
    let second_owner = source.span("when false then m = false; end when", 0);
    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == second_owner && detail.contains("`m`")
    ));
}

#[test]
fn nested_duplicate_diagnostic_follows_source_insertion_order() {
    let source = TestSource::new(
        "model M discrete Boolean z; discrete Boolean a; equation \
         when true then z = true; a = true; \
         if true then z = false; a = false; end if; end when; end M;",
    );
    let mut model = flat::Model::new();
    add_primitive_variable(
        &mut model,
        &source,
        "z",
        "discrete Boolean z",
        8,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "a",
        "discrete Boolean a",
        9,
        Vec::new(),
        true,
    );
    let owner_span = source.span("when true", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: owner_span,
        },
        owner_span,
    );
    for (target, assignment) in [("z", "z = true"), ("a", "a = true")] {
        let span = source.span(assignment, 0);
        branch.add_equation(flat::WhenEquation::assign(
            VarName::new(target),
            Expression::Literal {
                value: Literal::Boolean(true),
                span,
            },
            span,
            assignment,
        ));
    }
    let z_second = source.span("z = false", 0);
    let a_second = source.span("a = false", 0);
    branch.add_equation(flat::WhenEquation::conditional(
        vec![(
            Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("if true", 0),
            },
            vec![
                flat::WhenEquation::assign(
                    VarName::new("z"),
                    Expression::Literal {
                        value: Literal::Boolean(false),
                        span: z_second,
                    },
                    z_second,
                    "z second",
                ),
                flat::WhenEquation::assign(
                    VarName::new("a"),
                    Expression::Literal {
                        value: Literal::Boolean(false),
                        span: a_second,
                    },
                    a_second,
                    "a second",
                ),
            ],
        )],
        None,
        source.span("if true then z = false; a = false; end if", 0),
        "source-order duplicates",
    ));
    model
        .when_chains
        .push(flat::WhenChain::new(branch, owner_span));
    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == z_second && detail.contains("`z`")
    ));
}

#[test]
fn when_assert_level_reaches_checked_event_action_with_exact_provenance() {
    let source = TestSource::new(
        "model M equation when true then assert(false, \"failed\", 2); end when; end M;",
    );
    let condition_span = source.span("true", 0);
    let assertion_span = source.span("assert(false, \"failed\", 2)", 0);
    let level_span = source.span("2", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assert(
        Expression::Literal {
            value: Literal::Boolean(false),
            span: source.span("false", 0),
        },
        Expression::Literal {
            value: Literal::String("failed".to_string()),
            span: source.span("\"failed\"", 0),
        },
        Some(Expression::Literal {
            value: Literal::Integer(2),
            span: level_span,
        }),
        assertion_span,
        "assert in when-clause",
    ));
    let chain = flat::WhenChain::new(
        branch,
        source.span("when true then assert(false, \"failed\", 2); end when", 0),
    );
    let mut model = flat::Model::new();
    model.when_chains.push(chain);
    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.event_action_count(), 1);
        let action = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_eq!(action.provenance().span(), assertion_span);
        assert_eq!(
            view.source_text(action.provenance()),
            Some("assert(false, \"failed\", 2)")
        );
        let dae::EventActionOperation::Assert {
            message,
            level: Some(level),
        } = action.operation()
        else {
            panic!("checked event assertion must own its optional level");
        };
        assert_eq!(
            view.source_text(view.expression(message).unwrap().provenance()),
            Some("\"failed\"")
        );
        let level = view.expression(level).unwrap();
        assert_eq!(level.provenance().span(), level_span);
        assert_eq!(view.source_text(level.provenance()), Some("2"));
        assert!(matches!(
            level.operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(2))
        ));
    });
}

#[test]
fn production_lowering_constructs_delay_with_exact_timing_evidence() {
    let source = TestSource::new("Real x; Real y; parameter Real dt = 0.5; y - delay(x, dt);");
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, &source, "x", "Real x", 1, Vec::new(), false);
    add_primitive_variable(&mut model, &source, "y", "Real y", 2, Vec::new(), false);
    add_primitive_variable(
        &mut model,
        &source,
        "dt",
        "parameter Real dt = 0.5",
        3,
        Vec::new(),
        false,
    );
    let delay_time = model.variables.get_mut(&VarName::new("dt")).unwrap();
    delay_time.variability = Variability::Parameter(Default::default());
    delay_time.binding = Some(Expression::Literal {
        value: Literal::Real(0.5),
        span: source.span("0.5", 0),
    });
    let delay_span = source.span("delay(x, dt)", 0);
    let equation_span = source.span("y - delay(x, dt)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(variable_reference(&source, "y", "y", 1, Vec::new())),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Delay,
                args: vec![
                    variable_reference(&source, "x", "x", 1, Vec::new()),
                    variable_reference(&source, "dt", "dt", 1, Vec::new()),
                ],
                span: delay_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model.is_partial = true;

    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.delay_count(), 1);
        let delay = view
            .delay(view.delay_id(0).expect("dense delay identity"))
            .expect("checked delay owner resolves");
        assert_eq!(view.source_text(delay.provenance()), Some("delay(x, dt)"));
        let dae::DelayOperation::ParameterDelay { delay_time: timing } = delay.operation() else {
            panic!("two-argument delay owns parameter timing evidence");
        };
        assert_eq!(timing.value(), 0.5);
        assert_eq!(view.source_text(timing.provenance()), Some("dt"));
        assert_eq!(
            view.source_text(
                view.expression(timing.expression())
                    .expect("delayTime expression resolves")
                    .provenance()
            ),
            Some("dt")
        );
        let delay_coordinate = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Delay(_))
                )
            })
            .expect("delay owner is consumed by one typed coordinate");
        assert_eq!(
            view.source_text(delay_coordinate.provenance()),
            Some("delay(x, dt)")
        );
    });
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
    function.add_input(rumoca_core::FunctionParam::new("u", "Real", input_span));
    function.add_output(rumoca_core::FunctionParam::new("y", "Real", output_span));
    function.add_local(rumoca_core::FunctionParam::new("z", "Real", local_span));
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference::from_flat_segments("z", first_span, None),
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
            comp: rumoca_core::ComponentReference::from_flat_segments("y", second_span, None),
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
    let mut model = flat::Model::new();
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

    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();
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
    let mut model = flat::Model::new();
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

    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();
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
    function.add_output(rumoca_core::FunctionParam::new("y", "Integer", output_span));
    function.add_local(
        rumoca_core::FunctionParam::new("n", "Integer", local_span).with_default(
            Expression::Literal {
                value: Literal::Integer(3),
                span: source.span("3", 1),
            },
        ),
    );
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference::from_flat_segments("y", initial_span, None),
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
                comp: rumoca_core::ComponentReference::from_flat_segments("y", update_span, None),
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
    let mut model = flat::Model::new();
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

    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();
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
    function.add_input(rumoca_core::FunctionParam::new("n", "Integer", input_span));
    function.add_output(rumoca_core::FunctionParam::new("y", "Integer", output_span));
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference::from_flat_segments("y", initial_span, None),
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
                comp: rumoca_core::ComponentReference::from_flat_segments("y", update_span, None),
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
    let mut model = flat::Model::new();
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

    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "function loop domain" && span == range_span
    ));
}

#[test]
fn input_ownership_requires_resolved_component_identity() {
    let source = TestSource::new("model M input Real u; end M;");
    let declaration = source.span("input Real u", 0);
    let mut model = flat::Model::new();
    let mut variable = flat::Variable::empty_with_span(declaration);
    variable.name = VarName::new("u");
    variable.type_id = TypeId::new(8);
    variable.variability = Variability::Continuous(Default::default());
    variable.causality = Causality::Input(Default::default());
    variable.is_primitive = true;
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new("u"), "Real".to_string());
    model.top_level_input_components.insert("u".to_string());

    let error = construct(&model, source.map, ToDaeOptions::default()).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "input ownership" && span == declaration
    ));
}

#[test]
fn primitive_arrays_parameters_and_discrete_values_keep_checked_owners() {
    let source = TestSource::new(
        "Real x[2]; Real y; parameter Real p[2] = {1.0,2.0}; \
         discrete Boolean m = true; equation x = p; y = x[2];",
    );
    let model = array_and_discrete_model(&source);
    let dae = construct(&model, source.map, ToDaeOptions::default()).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 4);
        assert_eq!(
            view.variable(view.variable_id(0).unwrap())
                .unwrap()
                .scalar_count(),
            2
        );
        let parameter = view.variable(view.variable_id(2).unwrap()).unwrap();
        assert_eq!(parameter.role(), dae::VariableRole::Parameter);
        assert!(parameter.binding().is_some());
        assert_eq!(
            view.variable(view.variable_id(3).unwrap()).unwrap().role(),
            dae::VariableRole::DiscreteValue
        );
        assert_eq!(view.discrete_assignment_count(), 1);
        assert_eq!(view.continuous_owner_count(), 2);
        assert!(matches!(
            view.continuous_owner(0),
            Some(dae::ContinuousOwnerView::Structured { family, .. })
                if family.scalar_rows() == 2
        ));
        assert!(matches!(
            view.continuous_owner(1),
            Some(dae::ContinuousOwnerView::Residual { .. })
        ));
        let domain = view.domain(view.domain_id(0).unwrap()).unwrap();
        assert_eq!(
            domain.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::ArrayEquationProjection)
        );
    });
}

fn array_and_discrete_model(source: &TestSource) -> flat::Model {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, source, "x", "Real x[2]", 10, vec![2], false);
    add_primitive_variable(&mut model, source, "y", "Real y", 11, Vec::new(), false);
    add_parameter_with_array_binding(&mut model, source);
    add_discrete_boolean_with_binding(&mut model, source);
    let x = variable_reference(source, "x", "x = p", 0, Vec::new());
    let p = variable_reference(source, "p", "p", 1, Vec::new());
    model.add_equation(array_equation(
        source,
        "x = p",
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(x),
            rhs: Box::new(p),
            span: source.span("x = p", 0),
        },
        2,
    ));
    let y = variable_reference(source, "y", "y = x[2]", 0, Vec::new());
    let x_indexed = variable_reference(
        source,
        "x",
        "x[2]",
        0,
        vec![Subscript::Index {
            value: 2,
            span: source.span("2", 3),
        }],
    );
    model.add_equation(array_equation(
        source,
        "y = x[2]",
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(y),
            rhs: Box::new(x_indexed),
            span: source.span("y = x[2]", 0),
        },
        1,
    ));
    model
}

fn add_primitive_variable(
    model: &mut flat::Model,
    source: &TestSource,
    name: &str,
    declaration: &str,
    type_id: u32,
    dims: Vec<i64>,
    discrete: bool,
) {
    let mut variable = flat::Variable::empty_with_span(source.span(declaration, 0));
    variable.name = VarName::new(name);
    variable.type_id = TypeId::new(type_id);
    variable.dims = dims;
    variable.variability = if discrete {
        Variability::Discrete(Default::default())
    } else {
        Variability::Continuous(Default::default())
    };
    variable.is_discrete_type = discrete;
    variable.is_primitive = true;
    model.add_variable(variable.name.clone(), variable);
    model.variable_type_names.insert(
        VarName::new(name),
        if discrete { "Boolean" } else { "Real" }.to_string(),
    );
}

fn add_parameter_with_array_binding(model: &mut flat::Model, source: &TestSource) {
    add_primitive_variable(
        model,
        source,
        "p",
        "parameter Real p[2]",
        12,
        vec![2],
        false,
    );
    let variable = model.variables.get_mut(&VarName::new("p")).unwrap();
    variable.variability = Variability::Parameter(Default::default());
    variable.binding = Some(Expression::Array {
        elements: vec![
            Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            },
            Expression::Literal {
                value: Literal::Real(2.0),
                span: source.span("2.0", 0),
            },
        ],
        is_matrix: false,
        span: source.span("{1.0,2.0}", 0),
    });
}

fn add_discrete_boolean_with_binding(model: &mut flat::Model, source: &TestSource) {
    add_primitive_variable(
        model,
        source,
        "m",
        "discrete Boolean m",
        13,
        Vec::new(),
        true,
    );
    model.variables.get_mut(&VarName::new("m")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });
}

fn variable_reference(
    source: &TestSource,
    name: &str,
    owner: &str,
    occurrence: usize,
    subscripts: Vec<Subscript>,
) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts,
        span: source.span(owner, occurrence),
    }
}

fn array_equation(
    source: &TestSource,
    owner: &str,
    residual: Expression,
    scalar_count: usize,
) -> flat::Equation {
    flat::Equation::new_array(
        residual,
        source.span(owner, 0),
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
        scalar_count,
    )
}

#[test]
fn undefined_references_fail_before_construction() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; // missing\nend M;");
    let mut model = scalar_real_model(&source);
    let span = source.span("missing", 0);
    model.equations[0].residual = Expression::VarRef {
        name: Reference::new("missing"),
        subscripts: Vec::new(),
        span,
    };

    assert!(matches!(
        construct(&model, source.map, ToDaeOptions::default()),
        Err(ToDaeError::UnresolvedReference { name, span: found })
            if name == "missing" && found == span
    ));
}

#[test]
fn missing_expression_provenance_is_not_defaulted() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; end M;");
    let mut model = scalar_real_model(&source);
    model.equations[0].residual = Expression::VarRef {
        name: Reference::new("x"),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    assert!(matches!(
        construct(&model, source.map, ToDaeOptions::default()),
        Err(ToDaeError::MissingProvenance { .. })
    ));
}
