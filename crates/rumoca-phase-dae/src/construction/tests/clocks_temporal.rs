use rumoca_core::TypeId;

use super::super::*;
use super::support::*;

struct TestClock<'a> {
    name: &'a str,
    declaration: &'a str,
    constructor_span: Span,
    interval_span: Span,
    interval: f64,
    type_id: u32,
}

#[test]
fn clock_declaration_is_not_conflated_with_a_missing_discrete_hold_coordinate() {
    let source = TestSource::new("model M Clock c = Clock(0.1); discrete Boolean m; end M;");
    let mut model = test_model();
    let mut clock = flat::Variable::empty_with_span(source.span("Clock c = Clock(0.1)", 0));
    clock.name = VarName::new("c");
    clock.instance_id = test_instance_id("c");
    clock.type_id = TypeId::new(7);
    clock.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args: vec![Expression::Literal {
            value: Literal::Real(0.1),
            span: source.span("0.1", 0),
        }],
        span: source.span("Clock(0.1)", 0),
    });
    register_test_clock_type(&mut model, clock.type_id, &clock.dims);
    model.add_variable(clock.name.clone(), clock);
    model
        .variable_type_names
        .insert(VarName::new("c"), "Clock".to_string());
    add_primitive_variable(
        &mut model,
        &source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.clock_count(), 1);
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(
            view.source_text(owner.provenance()),
            Some("discrete Boolean m")
        );
    });
}

#[test]
fn ordinary_clocked_equation_retains_exact_previous_owner_and_provenance() {
    let source = TestSource::new(
        "model M Clock clockOwner=Clock(0.1); discrete Integer boundary; \
         discrete Integer counter; equation when clockOwner then boundary=1; end when; \
         counter=previous(counter)+boundary; end M;",
    );
    let previous_span = source.span("previous(counter)", 0);
    let clock_span = source.span("Clock(0.1)", 0);
    let model = ordinary_clocked_model(&source, previous_span, clock_span);

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.clock_count(), 1);
        assert_eq!(view.previous_value_count(), 1);
        let previous = view
            .previous(view.previous_id(0).unwrap())
            .expect("one checked previous owner");
        assert_eq!(
            view.source_text(previous.provenance()),
            Some("previous(counter)")
        );
        let clock = view
            .clock(previous.clock())
            .expect("previous clock resolves");
        assert_eq!(view.source_text(clock.provenance()), Some("Clock(0.1)"));
    });
}

fn ordinary_clocked_model(
    source: &TestSource,
    previous_span: Span,
    clock_span: Span,
) -> flat::Model {
    let mut model = test_model();
    add_test_clock(
        &mut model,
        source,
        TestClock {
            name: "clockOwner",
            declaration: "Clock clockOwner=Clock(0.1)",
            constructor_span: clock_span,
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 30,
        },
    );
    add_test_integer(
        &mut model,
        source,
        "boundary",
        "discrete Integer boundary",
        31,
    );
    add_test_integer(
        &mut model,
        source,
        "counter",
        "discrete Integer counter",
        32,
    );
    let condition_span = source.span("clockOwner", 1);
    let assignment_span = source.span("boundary=1", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::VarRef {
            name: test_reference("clockOwner"),
            subscripts: Vec::new(),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("boundary"),
        Expression::Literal {
            value: Literal::Integer(1),
            span: source.span("1", 1),
        },
        assignment_span,
        "test clock boundary",
    ));
    model.when_chains.push(flat::WhenChain::new(
        branch,
        source.span("when clockOwner then boundary=1; end when", 0),
    ));
    let equation_span = source.span("counter=previous(counter)+boundary", 0);
    let previous = Expression::BuiltinCall {
        function: BuiltinFunction::Previous,
        args: vec![Expression::VarRef {
            name: test_reference("counter"),
            subscripts: Vec::new(),
            span: source.span("counter", 2),
        }],
        span: previous_span,
    };
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("counter"),
                subscripts: Vec::new(),
                span: source.span("counter", 1),
            }),
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(previous),
                rhs: Box::new(Expression::VarRef {
                    name: test_reference("boundary"),
                    subscripts: Vec::new(),
                    span: source.span("boundary", 2),
                }),
                span: source.span("previous(counter)+boundary", 0),
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

#[test]
fn interval_lowers_to_the_exact_clock_owner_with_use_site_provenance() {
    let source = TestSource::new(
        "model M Clock clockOwner=Clock(0.1); discrete Real dt; equation \
         when clockOwner then dt=interval(); end when; end M;",
    );
    let interval_span = source.span("interval()", 0);
    let mut model = test_model();
    add_test_clock(
        &mut model,
        &source,
        TestClock {
            name: "clockOwner",
            declaration: "Clock clockOwner=Clock(0.1)",
            constructor_span: source.span("Clock(0.1)", 0),
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 35,
        },
    );
    add_primitive_variable(
        &mut model,
        &source,
        "dt",
        "discrete Real dt",
        36,
        Vec::new(),
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("dt"))
        .unwrap()
        .variability = Variability::Discrete(Default::default());

    let condition_span = source.span("clockOwner", 1);
    let mut branch = flat::WhenBranch::new(
        Expression::VarRef {
            name: test_reference("clockOwner"),
            subscripts: Vec::new(),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("dt"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Interval,
            args: Vec::new(),
            span: interval_span,
        },
        source.span("dt=interval()", 0),
        "test exact interval owner",
    ));
    model.when_chains.push(flat::WhenChain::new(
        branch,
        source.span("when clockOwner then dt=interval(); end when", 0),
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let clock = view.clock_id(0).expect("one exact clock owner");
        let interval = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(
                        dae::CoordinateView::ClockInterval(owner)
                    ) if owner.index() == clock.index()
                )
            })
            .expect("interval is a typed DAE coordinate");
        assert_eq!(interval.provenance().span(), interval_span);
        assert_eq!(view.source_text(interval.provenance()), Some("interval()"));
        assert_eq!(interval.variability(), dae::ExpressionVariability::Discrete);
        assert_eq!(interval.value_type().scalar_type(), dae::ScalarType::Real);
    });
}

#[test]
fn interval_without_a_clock_owner_fails_at_the_exact_use_site() {
    let source = TestSource::new("model M Real x; equation x=interval(x); end M;");
    let interval_span = source.span("interval(x)", 0);
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "x", "Real x", 37, Vec::new(), false);
    let equation_span = source.span("x=interval(x)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("x"),
                subscripts: Vec::new(),
                span: source.span("x", 1),
            }),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Interval,
                args: vec![Expression::VarRef {
                    name: test_reference("x"),
                    subscripts: Vec::new(),
                    span: source.span("x", 2),
                }],
                span: interval_span,
            }),
            span: equation_span,
        },
        equation_span,
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
        } if feature == "clocked equation ownership proof" && span == interval_span
    ));
}

#[test]
fn disconnected_clock_domains_retain_distinct_exact_owners() {
    let source = TestSource::new(
        "model M Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.2); \
         discrete Integer x; discrete Integer y; equation \
         when leftClock then x=1; end when; \
         when rightClock then y=2; end when; end M;",
    );
    let model = distinct_clock_assignment_model(&source);
    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.clock_count(), 2);
        assert_eq!(view.clock_ownership_count(), 2);
        let mut owners = (0..view.clock_ownership_count())
            .map(|index| {
                let ownership = view
                    .clock_ownership(view.clock_ownership_id(index).unwrap())
                    .unwrap();
                let variable = view.variable(ownership.variable()).unwrap();
                let clock = view.clock(ownership.clock()).unwrap();
                (
                    variable.name().as_str().to_string(),
                    view.source_text(clock.provenance()).unwrap().to_string(),
                )
            })
            .collect::<Vec<_>>();
        owners.sort();
        assert_eq!(
            owners,
            vec![
                ("x".to_string(), "Clock(0.1)".to_string()),
                ("y".to_string(), "Clock(0.2)".to_string()),
            ]
        );
    });
}

#[test]
fn super_sample_value_constructs_an_exact_cross_clock_transfer() {
    let source = TestSource::new(
        "model M Clock baseClock=Clock(0.1); discrete Integer u; discrete Integer y; \
         equation when baseClock then u=1; end when; y=superSample(u,2); end M;",
    );
    let mut model = test_model();
    add_test_clock(
        &mut model,
        &source,
        TestClock {
            name: "baseClock",
            declaration: "Clock baseClock=Clock(0.1)",
            constructor_span: source.span("Clock(0.1)", 0),
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 44,
        },
    );
    add_test_integer(&mut model, &source, "u", "discrete Integer u", 45);
    add_test_integer(&mut model, &source, "y", "discrete Integer y", 46);
    add_test_clock_assignment(&mut model, &source, "baseClock", "u", 1);
    let conversion_span = source.span("superSample(u,2)", 0);
    let equation_span = source.span("y=superSample(u,2)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("y"),
                subscripts: Vec::new(),
                span: source.span("y", 1),
            }),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::SuperSample,
                args: vec![
                    Expression::VarRef {
                        name: test_reference("u"),
                        subscripts: Vec::new(),
                        span: source.span("u", 2),
                    },
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: source.span("2", 0),
                    },
                ],
                span: conversion_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map)
        .expect("superSample owns an exact source-to-derived-clock value transfer");
    dae.inspect(|view| {
        assert_eq!(view.clock_count(), 2);
        assert_eq!(view.clock_ownership_count(), 2);
        let transfer = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| expression.provenance().span() == conversion_span)
            .expect("the conversion is retained as an explicit DAE expression owner");
        assert_eq!(transfer.provenance().span(), conversion_span);
    });
}

#[test]
fn connected_equation_rejects_distinct_clock_owners_at_exact_second_clock_use() {
    let source = TestSource::new(
        "model M Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.2); \
         discrete Integer x; discrete Integer y; equation \
         when leftClock then x=1; end when; \
         when rightClock then y=2; end when; x=y; end M;",
    );
    let mut model = distinct_clock_assignment_model(&source);
    let right_use = source.span("rightClock", 1);
    let equation_span = source.span("x=y", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("x"),
                subscripts: Vec::new(),
                span: source.span("x", 2),
            }),
            rhs: Box::new(Expression::VarRef {
                name: test_reference("y"),
                subscripts: Vec::new(),
                span: source.span("y", 2),
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "clocked equation ownership proof"
            && detail.contains("distinct clock owners")
            && span == right_use
    ));
}

fn distinct_clock_assignment_model(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    add_test_clock(
        &mut model,
        source,
        TestClock {
            name: "leftClock",
            declaration: "Clock leftClock=Clock(0.1)",
            constructor_span: source.span("Clock(0.1)", 0),
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 40,
        },
    );
    add_test_clock(
        &mut model,
        source,
        TestClock {
            name: "rightClock",
            declaration: "Clock rightClock=Clock(0.2)",
            constructor_span: source.span("Clock(0.2)", 0),
            interval_span: source.span("0.2", 0),
            interval: 0.2,
            type_id: 41,
        },
    );
    add_test_integer(&mut model, source, "x", "discrete Integer x", 42);
    add_test_integer(&mut model, source, "y", "discrete Integer y", 43);
    add_test_clock_assignment(&mut model, source, "leftClock", "x", 1);
    add_test_clock_assignment(&mut model, source, "rightClock", "y", 2);
    model
}

fn add_test_clock_assignment(
    model: &mut flat::Model,
    source: &TestSource,
    clock: &str,
    target: &str,
    value: i64,
) {
    let condition_span = source.span(clock, 1);
    let mut branch = flat::WhenBranch::new(
        Expression::VarRef {
            name: test_reference(clock),
            subscripts: Vec::new(),
            span: condition_span,
        },
        condition_span,
    );
    let assignment = format!("{target}={value}");
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new(target),
        Expression::Literal {
            value: Literal::Integer(value),
            span: source.span(&value.to_string(), 1),
        },
        source.span(&assignment, 0),
        "distinct clock test assignment",
    ));
    let chain = format!("when {clock} then {assignment}; end when");
    model
        .when_chains
        .push(flat::WhenChain::new(branch, source.span(&chain, 0)));
}

fn add_test_clock(model: &mut flat::Model, source: &TestSource, clock: TestClock<'_>) {
    let mut variable = flat::Variable::empty_with_span(source.span(clock.declaration, 0));
    variable.name = VarName::new(clock.name);
    variable.instance_id = test_instance_id(clock.name);
    variable.type_id = TypeId::new(clock.type_id);
    variable.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args: vec![Expression::Literal {
            value: Literal::Real(clock.interval),
            span: clock.interval_span,
        }],
        span: clock.constructor_span,
    });
    register_test_clock_type(model, variable.type_id, &variable.dims);
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new(clock.name), "Clock".to_string());
}

fn add_test_integer(
    model: &mut flat::Model,
    source: &TestSource,
    name: &str,
    declaration: &str,
    type_id: u32,
) {
    let mut variable = flat::Variable::empty_with_span(source.span(declaration, 0));
    variable.name = VarName::new(name);
    variable.instance_id = test_instance_id(name);
    variable.type_id = TypeId::new(type_id);
    variable.variability = Variability::Discrete(Default::default());
    variable.is_discrete_type = true;
    variable.is_primitive = true;
    register_test_integer_type(model, variable.type_id, &variable.dims);
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new(name), "Integer".to_string());
}

#[test]
fn production_lowering_constructs_delay_with_exact_timing_evidence() {
    let source = TestSource::new("Real x; Real y; parameter Real dt = 0.5; y - delay(x, dt);");
    let mut model = test_model();
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

    let analysis = analyze(&model).unwrap();
    assert_eq!(analysis.delay_plans.len(), 1);
    let Some(DelayPlan::Fixed(timing)) = analysis.delay_plans.get(&delay_span) else {
        panic!("accepted delay occurrence owns exactly one fixed timing plan");
    };
    assert_eq!(timing.value(), 0.5);
    assert_eq!(timing.provenance(), source.span("dt", 1));

    let mut missing_provenance = model.clone();
    let Expression::Binary { rhs, .. } = &mut missing_provenance.equations[0].residual else {
        panic!("fixture owns a binary residual");
    };
    let Expression::BuiltinCall { args, .. } = rhs.as_mut() else {
        panic!("fixture owns one delay call");
    };
    let Expression::VarRef { span, .. } = &mut args[1] else {
        panic!("fixture delayTime is a parameter reference");
    };
    *span = Span::DUMMY;
    assert!(matches!(
        analyze(&missing_provenance),
        Err(ToDaeError::MissingProvenance { .. })
    ));

    let dae = construct(&model, source.map).unwrap();
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
