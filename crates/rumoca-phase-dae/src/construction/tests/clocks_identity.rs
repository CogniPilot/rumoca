use rumoca_core::TypeId;

use super::super::*;
use super::clocks_temporal::{
    TestClock, add_test_clock, add_test_clock_assignment, add_test_integer, add_test_residual,
    integer_clock_transfer, span_within, test_var_at,
};
use super::support::*;

fn cloned_clock_owner_model(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    let shared_constructor = source.span("Clock(0.1)", 0);
    let shared_interval = source.span("0.1", 0);
    for (name, declaration, type_id) in [
        ("leftClock", "Clock leftClock=Clock(0.1)", 70),
        ("rightClock", "Clock rightClock=Clock(0.1)", 71),
    ] {
        add_test_clock(
            &mut model,
            source,
            TestClock {
                name,
                declaration,
                constructor_span: shared_constructor,
                interval_span: shared_interval,
                interval: 0.1,
                type_id,
            },
        );
    }
    add_test_integer(&mut model, source, "x", "discrete Integer x", 72);
    add_test_integer(&mut model, source, "y", "discrete Integer y", 73);
    add_test_clock_assignment(&mut model, source, "leftClock", "x", 1);
    add_test_clock_assignment(&mut model, source, "rightClock", "y", 1);
    model
}

#[test]
fn cloned_constructor_spans_do_not_collapse_instance_clock_owners() {
    let source = TestSource::new(
        "Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.1); \
         discrete Integer x; discrete Integer y; \
         when leftClock then x=1; end when; \
         when rightClock then y=1; end when;",
    );
    let model = cloned_clock_owner_model(&source);
    let dae = construct(&model, source.map).expect("instance identity, not span, owns clocks");
    dae.dae().inspect(|view| assert_eq!(view.clock_count(), 2));
}

#[test]
fn cloned_constructor_spans_still_reject_cross_owner_equations() {
    let source = TestSource::new(
        "Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.1); \
         discrete Integer x; discrete Integer y; \
         when leftClock then x=1; end when; \
         when rightClock then y=1; end when; x=y;",
    );
    let mut model = cloned_clock_owner_model(&source);
    let equation_span = source.span("x=y", 0);
    let attempted = span_within(equation_span, 2, 1);
    add_test_residual(
        &mut model,
        test_var_at("x", span_within(equation_span, 0, 1)),
        test_var_at("y", attempted),
        equation_span,
    );
    let error = construct(&model, source.map).expect_err("equal schedules are not owner aliases");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "clocked equation ownership proof" && span == attempted
    ));
}

#[test]
fn cloned_when_branch_spans_do_not_overwrite_occurrence_owners() {
    let source = TestSource::new(
        "Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.1); \
         discrete Integer x; discrete Integer y; \
         when leftClock then x=1; end when; when rightClock then y=1; end when;",
    );
    let mut model = cloned_clock_owner_model(&source);
    let shared = source.span("leftClock", 1);
    let shared_condition = test_var_at("leftClock", shared);
    let mut chains = model.when_chains.iter_mut();
    let first = chains.next().expect("left fixture chain exists");
    let first = first
        .branches_mut()
        .next()
        .expect("a when chain has its required first branch");
    first.span = shared;
    first.condition = shared_condition.clone();
    let second = chains.next().expect("right fixture chain exists");
    let second = second
        .branches_mut()
        .next()
        .expect("a when chain has its required first branch");
    second.span = shared;
    // Preserve right-clock semantic identity while cloning its complete source
    // provenance onto the first occurrence.
    second.condition = Expression::VarRef {
        name: test_reference("rightClock"),
        subscripts: Vec::new(),
        span: shared,
    };
    let dae = construct(&model, source.map)
        .expect("when owner lookup is keyed by chain/branch occurrence, not span");
    dae.dae().inspect(|view| assert_eq!(view.clock_count(), 2));
}

fn two_clock_transfer_model(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    for (name, declaration, interval, type_id) in [
        ("leftClock", "Clock leftClock=Clock(0.1)", 0.1, 80),
        ("rightClock", "Clock rightClock=Clock(0.2)", 0.2, 81),
    ] {
        let constructor = format!("Clock({interval})");
        let interval_text = interval.to_string();
        add_test_clock(
            &mut model,
            source,
            TestClock {
                name,
                declaration,
                constructor_span: source.span(&constructor, 0),
                interval_span: source.span(&interval_text, 0),
                interval,
                type_id,
            },
        );
    }
    for (name, declaration, type_id) in [
        ("x", "discrete Integer x", 82),
        ("y", "discrete Integer y", 83),
        ("u", "discrete Integer u", 84),
        ("v", "discrete Integer v", 85),
    ] {
        add_test_integer(&mut model, source, name, declaration, type_id);
    }
    add_test_clock_assignment(&mut model, source, "leftClock", "x", 1);
    add_test_clock_assignment(&mut model, source, "rightClock", "y", 1);
    model
}

#[test]
fn cloned_transfer_spans_retain_distinct_occurrence_and_owner_plans() {
    let source = TestSource::new(
        "Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.2); \
         discrete Integer x; discrete Integer y; discrete Integer u; discrete Integer v; \
         when leftClock then x=1; end when; when rightClock then y=1; end when; \
         u=superSample(x,2);",
    );
    let mut model = two_clock_transfer_model(&source);
    let equation_span = source.span("u=superSample(x,2)", 0);
    let transfer_span = source.span("superSample(x,2)", 0);
    let factor_span = span_within(
        transfer_span,
        transfer_span.end.0 - transfer_span.start.0 - 2,
        1,
    );
    for (target, source_name) in [("u", "x"), ("v", "y")] {
        let transfer = integer_clock_transfer(
            BuiltinFunction::SuperSample,
            test_var_at(source_name, span_within(transfer_span, 12, 1)),
            2,
            factor_span,
            transfer_span,
        );
        add_test_residual(
            &mut model,
            test_var_at(target, span_within(equation_span, 0, 1)),
            transfer,
            equation_span,
        );
    }
    let dae = construct(&model, source.map)
        .expect("one source span may provenance multiple instantiated transfer occurrences");
    dae.dae().inspect(|view| {
        assert_eq!(view.clock_count(), 4);
        let transfers = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::ClockTransfer { .. }
                )
            })
            .count();
        assert_eq!(transfers, 2);
    });
}

#[test]
fn equal_lattice_composite_transfers_cannot_launder_distinct_lineage() {
    let source = TestSource::new(
        "Clock leftClock=Clock(0.1); Clock rightClock=Clock(0.1); \
         discrete Integer x; discrete Integer y; discrete Integer z; \
         when leftClock then x=1; end when; when rightClock then y=1; end when; \
         z=superSample(x,2)+superSample(y,2);",
    );
    let mut model = cloned_clock_owner_model(&source);
    add_test_integer(&mut model, &source, "z", "discrete Integer z", 90);
    let lhs_transfer_span = source.span("superSample(x,2)", 0);
    let rhs_transfer_span = source.span("superSample(y,2)", 0);
    let composite_span = source.span("superSample(x,2)+superSample(y,2)", 0);
    let equation_span = source.span("z=superSample(x,2)+superSample(y,2)", 0);
    let lhs = integer_clock_transfer(
        BuiltinFunction::SuperSample,
        test_var_at("x", span_within(lhs_transfer_span, 12, 1)),
        2,
        span_within(lhs_transfer_span, 14, 1),
        lhs_transfer_span,
    );
    let rhs = integer_clock_transfer(
        BuiltinFunction::SuperSample,
        test_var_at("y", span_within(rhs_transfer_span, 12, 1)),
        2,
        span_within(rhs_transfer_span, 14, 1),
        rhs_transfer_span,
    );
    add_test_residual(
        &mut model,
        test_var_at("z", span_within(equation_span, 0, 1)),
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: composite_span,
        },
        equation_span,
    );
    let error = construct(&model, source.map)
        .expect_err("compatible target lattices do not prove common source lineage");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "clocked value conversion ownership proof" && span == rhs_transfer_span
    ));
}

#[test]
fn transfer_target_conflict_reports_instead_of_indexing_parallel_provenance() {
    let source = TestSource::new(
        "Clock first=Clock(0.1); Clock second=Clock(0.2); \
         discrete Integer y; discrete Integer z; \
         when first then z=1; end when; \
         when second then y=1; z=superSample(y,2); end when;",
    );
    let mut model = test_model();
    for (name, declaration, interval, type_id) in [
        ("first", "Clock first=Clock(0.1)", 0.1, 93),
        ("second", "Clock second=Clock(0.2)", 0.2, 94),
    ] {
        let constructor = format!("Clock({interval})");
        add_test_clock(
            &mut model,
            &source,
            TestClock {
                name,
                declaration,
                constructor_span: source.span(&constructor, 0),
                interval_span: source.span(&interval.to_string(), 0),
                interval,
                type_id,
            },
        );
    }
    add_test_integer(&mut model, &source, "y", "discrete Integer y", 95);
    add_test_integer(&mut model, &source, "z", "discrete Integer z", 96);
    add_test_clock_assignment(&mut model, &source, "first", "z", 1);
    let condition_span = source.span("second", 1);
    let mut branch = flat::WhenBranch::new(test_var_at("second", condition_span), condition_span);
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal {
            value: Literal::Integer(1),
            span: source.span("1", 2),
        },
        source.span("y=1", 0),
        "dynamic target provenance fixture",
    ));
    let transfer_span = source.span("superSample(y,2)", 0);
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("z"),
        integer_clock_transfer(
            BuiltinFunction::SuperSample,
            test_var_at("y", span_within(transfer_span, 12, 1)),
            2,
            span_within(transfer_span, 14, 1),
            transfer_span,
        ),
        source.span("z=superSample(y,2)", 0),
        "dynamic target provenance fixture",
    ));
    model.when_chains.push(flat::WhenChain::new(
        branch,
        source.span("when second then y=1; z=superSample(y,2); end when", 0),
    ));
    let error = construct(&model, source.map)
        .expect_err("a dynamic transfer target joined to another owner rejects without panic");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "clocked equation ownership proof" && span == transfer_span
    ));
}

#[test]
fn duplicate_equal_schedule_definitions_for_one_coordinate_reject() {
    let source = TestSource::new("Clock c=Clock(0.1); c=Clock(0.1);");
    let mut model = test_model();
    add_test_clock(
        &mut model,
        &source,
        TestClock {
            name: "c",
            declaration: "Clock c=Clock(0.1)",
            constructor_span: source.span("Clock(0.1)", 0),
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 91,
        },
    );
    let equation_span = source.span("c=Clock(0.1)", 1);
    add_test_residual(
        &mut model,
        test_var_at("c", span_within(equation_span, 0, 1)),
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            args: vec![Expression::Literal {
                value: Literal::Real(0.1),
                span: source.span("0.1", 1),
            }],
            span: source.span("Clock(0.1)", 1),
        },
        equation_span,
    );
    let error = construct(&model, source.map)
        .expect_err("one coordinate cannot declare two semantic owners");
    assert!(
        error
            .to_string()
            .contains("more than one semantic clock owner")
    );
}

fn add_unbound_clock(
    model: &mut flat::Model,
    source: &TestSource,
    name: &str,
    declaration: &str,
    type_id: u32,
) {
    let mut clock = flat::Variable::empty_with_span(source.span(declaration, 0));
    clock.name = VarName::new(name);
    clock.instance_id = test_instance_id(name);
    clock.component_ref = Some(test_component_reference(name, source.span(declaration, 0)));
    clock.type_id = TypeId::new(type_id);
    register_test_clock_type(model, clock.type_id, &clock.dims);
    model.add_variable(clock.name.clone(), clock);
    model
        .variable_type_names
        .insert(VarName::new(name), "Clock".to_string());
}

#[test]
fn explicit_clock_alias_unifies_one_proven_owner() {
    let source = TestSource::new("Clock base=Clock(0.1); Clock alias; alias=base;");
    let mut model = test_model();
    add_test_clock(
        &mut model,
        &source,
        TestClock {
            name: "base",
            declaration: "Clock base=Clock(0.1)",
            constructor_span: source.span("Clock(0.1)", 0),
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 100,
        },
    );
    add_unbound_clock(&mut model, &source, "alias", "Clock alias", 101);
    let equation_span = source.span("alias=base", 0);
    add_test_residual(
        &mut model,
        test_var_at("alias", span_within(equation_span, 0, 5)),
        test_var_at("base", span_within(equation_span, 6, 4)),
        equation_span,
    );
    let dae = construct(&model, source.map).expect("an exact alias carries its source owner");
    dae.dae().inspect(|view| assert_eq!(view.clock_count(), 1));
}

#[test]
fn alias_between_independent_equal_lattice_owners_rejects() {
    let source = TestSource::new("Clock left=Clock(0.1); Clock right=Clock(0.1); left=right;");
    let mut model = test_model();
    for (name, declaration, occurrence, type_id) in [
        ("left", "Clock left=Clock(0.1)", 0, 102),
        ("right", "Clock right=Clock(0.1)", 1, 103),
    ] {
        add_test_clock(
            &mut model,
            &source,
            TestClock {
                name,
                declaration,
                constructor_span: source.span("Clock(0.1)", occurrence),
                interval_span: source.span("0.1", occurrence),
                interval: 0.1,
                type_id,
            },
        );
    }
    let equation_span = source.span("left=right", 0);
    add_test_residual(
        &mut model,
        test_var_at("left", span_within(equation_span, 0, 4)),
        test_var_at("right", span_within(equation_span, 5, 5)),
        equation_span,
    );
    let error = construct(&model, source.map)
        .expect_err("alias syntax cannot unify two independent constructors");
    assert!(error.to_string().contains("distinct semantic clock owners"));
}

#[test]
fn sibling_clock_definitions_receive_distinct_preorder_occurrences() {
    let source = TestSource::new("Clock(0.1)+Clock(0.2)");
    let left = Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args: vec![Expression::Literal {
            value: Literal::Real(0.1),
            span: source.span("0.1", 0),
        }],
        span: source.span("Clock(0.1)", 0),
    };
    let right = Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args: vec![Expression::Literal {
            value: Literal::Real(0.2),
            span: source.span("0.2", 0),
        }],
        span: source.span("Clock(0.2)", 0),
    };
    let root = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(left),
        rhs: Box::new(right),
        span: source.span("Clock(0.1)+Clock(0.2)", 0),
    };
    let Expression::Binary { lhs, rhs, .. } = &root else {
        return;
    };
    let left = super::super::analysis::expression_preorder_ordinal(&root, lhs)
        .expect("left sibling is in the fixture");
    let right = super::super::analysis::expression_preorder_ordinal(&root, rhs)
        .expect("right sibling is in the fixture");
    assert_ne!(left, right);
}

#[test]
fn clock_parameter_type_error_uses_exact_argument_span() {
    let source = TestSource::new("Clock c=Clock(true);");
    let mut model = test_model();
    let declaration = source.span("Clock c=Clock(true)", 0);
    let argument = source.span("true", 0);
    let mut clock = flat::Variable::empty_with_span(declaration);
    clock.name = VarName::new("c");
    clock.instance_id = test_instance_id("c");
    clock.component_ref = Some(test_component_reference("c", declaration));
    clock.type_id = TypeId::new(92);
    clock.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args: vec![Expression::Literal {
            value: Literal::Boolean(true),
            span: argument,
        }],
        span: source.span("Clock(true)", 0),
    });
    register_test_clock_type(&mut model, clock.type_id, &clock.dims);
    model.add_variable(clock.name.clone(), clock);
    model
        .variable_type_names
        .insert(VarName::new("c"), "Clock".to_string());
    let error = construct(&model, source.map).expect_err("Boolean is not a Clock interval");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockParameter {
                operator: "Clock",
                span,
                ..
            },
            ..
        } if span == argument
    ));
}

fn model_with_clock_binding(
    source: &TestSource,
    declaration: &str,
    call_span: Span,
    args: Vec<Expression>,
    type_id: u32,
) -> flat::Model {
    let mut model = test_model();
    let mut clock = flat::Variable::empty_with_span(source.span(declaration, 0));
    clock.name = VarName::new("c");
    clock.instance_id = test_instance_id("c");
    clock.component_ref = Some(test_component_reference("c", source.span(declaration, 0)));
    clock.type_id = TypeId::new(type_id);
    clock.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args,
        span: call_span,
    });
    register_test_clock_type(&mut model, clock.type_id, &clock.dims);
    model.add_variable(clock.name.clone(), clock);
    model
        .variable_type_names
        .insert(VarName::new("c"), "Clock".to_string());
    model
}

#[test]
fn nonpositive_clock_period_reports_interval_argument_span() {
    let source = TestSource::new("Clock c=Clock(0.0);");
    let argument = source.span("0.0", 0);
    let model = model_with_clock_binding(
        &source,
        "Clock c=Clock(0.0)",
        source.span("Clock(0.0)", 0),
        vec![Expression::Literal {
            value: Literal::Real(0.0),
            span: argument,
        }],
        97,
    );
    let error = construct(&model, source.map).expect_err("a Clock period must be positive");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockLattice { span, .. },
            ..
        } if span == argument
    ));
}

#[test]
fn clock_interval_counter_reports_its_exact_argument_span() {
    let source = TestSource::new("Clock c=Clock(0,1000);");
    let counter = source.span("0", 0);
    let model = model_with_clock_binding(
        &source,
        "Clock c=Clock(0,1000)",
        source.span("Clock(0,1000)", 0),
        vec![
            Expression::Literal {
                value: Literal::Integer(0),
                span: counter,
            },
            Expression::Literal {
                value: Literal::Integer(1000),
                span: source.span("1000", 0),
            },
        ],
        98,
    );
    let error = construct(&model, source.map).expect_err("interval counter must be positive");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockParameter {
                operator: "Clock",
                detail,
                span,
            },
            ..
        } if detail.contains("interval counter") && span == counter
    ));
}

#[test]
fn clock_resolution_reports_its_exact_argument_span() {
    let source = TestSource::new("Clock c=Clock(1,0);");
    let resolution = source.span("0", 0);
    let model = model_with_clock_binding(
        &source,
        "Clock c=Clock(1,0)",
        source.span("Clock(1,0)", 0),
        vec![
            Expression::Literal {
                value: Literal::Integer(1),
                span: source.span("1", 0),
            },
            Expression::Literal {
                value: Literal::Integer(0),
                span: resolution,
            },
        ],
        99,
    );
    let error = construct(&model, source.map).expect_err("Clock resolution must be positive");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockParameter {
                operator: "Clock",
                detail,
                span,
            },
            ..
        } if detail.contains("resolution") && span == resolution
    ));
}

#[test]
fn forged_real_transfer_factor_is_not_coerced_to_integer() {
    let source = TestSource::new(
        "Clock baseClock=Clock(0.1); discrete Integer v; discrete Integer y; \
         when baseClock then v=1; end when; y=superSample(v,2.0);",
    );
    let mut model = super::clocks_temporal::nested_transfer_model(&source);
    let transfer_span = source.span("superSample(v,2.0)", 0);
    let factor_span = source.span("2.0", 0);
    let equation_span = source.span("y=superSample(v,2.0)", 0);
    add_test_residual(
        &mut model,
        test_var_at("y", span_within(equation_span, 0, 1)),
        Expression::BuiltinCall {
            function: BuiltinFunction::SuperSample,
            args: vec![
                test_var_at("v", span_within(transfer_span, 12, 1)),
                Expression::Literal {
                    value: Literal::Real(2.0),
                    span: factor_span,
                },
            ],
            span: transfer_span,
        },
        equation_span,
    );
    let error = construct(&model, source.map).expect_err("Real 2.0 is not an Integer factor");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockParameter {
                operator: "superSample",
                span,
                ..
            },
            ..
        } if span == factor_span
    ));
}

#[test]
fn negative_shift_counter_uses_named_parameter_diagnostic() {
    let source = TestSource::new(
        "Clock baseClock=Clock(0.1); discrete Integer v; discrete Integer y; \
         when baseClock then v=1; end when; y=shiftSample(v,-1);",
    );
    let mut model = super::clocks_temporal::nested_transfer_model(&source);
    let transfer_span = source.span("shiftSample(v,-1)", 0);
    let counter_span = source.span("-1", 0);
    let equation_span = source.span("y=shiftSample(v,-1)", 0);
    add_test_residual(
        &mut model,
        test_var_at("y", span_within(equation_span, 0, 1)),
        Expression::BuiltinCall {
            function: BuiltinFunction::ShiftSample,
            args: vec![
                test_var_at("v", span_within(transfer_span, 12, 1)),
                Expression::Literal {
                    value: Literal::Integer(-1),
                    span: counter_span,
                },
            ],
            span: transfer_span,
        },
        equation_span,
    );
    let error = construct(&model, source.map).expect_err("negative counter is invalid");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::InvalidClockParameter {
                operator: "shiftSample",
                detail,
                span,
            },
            ..
        } if detail.contains("counter must be nonnegative") && span == counter_span
    ));
}

#[test]
fn clocked_residual_rejects_a_derivative_target_at_its_exact_use() {
    let source = TestSource::new(
        "Clock c=Clock(0.1); discrete Integer d; Real x; \
         when c then d=1; end when; d+x=0.0; der(x)=0.0;",
    );
    let mut model = state_clock_model(&source, false);
    let residual_span = source.span("d+x=0.0", 0);
    let state_use = source.span("x", 1);
    add_test_residual(
        &mut model,
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(test_var_at("d", source.span("d", 2))),
            rhs: Box::new(test_var_at("x", state_use)),
            span: source.span("d+x", 0),
        },
        Expression::Literal {
            value: Literal::Real(0.0),
            span: source.span("0.0", 0),
        },
        residual_span,
    );
    add_derivative_equation(&mut model, &source);

    let error = construct(&model, source.map)
        .expect_err("a derivative target cannot become a clocked discrete coordinate");
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
                if feature == "clocked continuous coordinate" && *span == state_use
        ),
        "unexpected clocked-residual refusal: {error:?}"
    );
}

#[test]
fn clock_transfer_rejects_a_derivative_target_at_its_exact_source_use() {
    let source = TestSource::new(
        "Clock c=Clock(0.1); discrete Integer d; discrete Integer z; Real x; \
         when c then d=1; end when; z=superSample(d+x,2); der(x)=0.0;",
    );
    let mut model = state_clock_model(&source, true);
    let state_use = source.span("x", 1);
    let transfer_span = source.span("superSample(d+x,2)", 0);
    let transfer_source = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(test_var_at("d", source.span("d", 2))),
        rhs: Box::new(test_var_at("x", state_use)),
        span: source.span("d+x", 0),
    };
    add_test_residual(
        &mut model,
        test_var_at("z", source.span("z", 1)),
        integer_clock_transfer(
            BuiltinFunction::SuperSample,
            transfer_source,
            2,
            span_within(
                transfer_span,
                transfer_span.end.0 - transfer_span.start.0 - 2,
                1,
            ),
            transfer_span,
        ),
        source.span("z=superSample(d+x,2)", 0),
    );
    add_derivative_equation(&mut model, &source);

    let error = construct(&model, source.map)
        .expect_err("a clock transfer cannot conceal a continuous state operand");
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
                if feature == "clocked continuous coordinate" && *span == state_use
        ),
        "unexpected clock-transfer refusal: {error:?}"
    );
}

#[test]
fn clock_transfer_accepts_a_parameter_sibling_of_a_clocked_coordinate() {
    let source = TestSource::new(
        "Clock c=Clock(0.1); discrete Integer d; discrete Integer z; parameter Integer p=2; \
         when c then d=1; end when; z=superSample(d+p,2);",
    );
    let mut model = test_model();
    add_identity_test_clock(&mut model, &source);
    add_test_integer(&mut model, &source, "d", "discrete Integer d", 302);
    add_test_integer(&mut model, &source, "z", "discrete Integer z", 303);
    add_test_integer(&mut model, &source, "p", "parameter Integer p=2", 304);
    let parameter = model
        .variables
        .get_mut(&VarName::new("p"))
        .expect("fixture parameter exists");
    parameter.variability = Variability::Parameter(Default::default());
    parameter.is_discrete_type = false;
    parameter.binding = Some(Expression::Literal {
        value: Literal::Integer(2),
        span: source.span("2", 0),
    });
    add_test_clock_assignment(&mut model, &source, "c", "d", 1);
    let transfer_span = source.span("superSample(d+p,2)", 0);
    add_test_residual(
        &mut model,
        test_var_at("z", source.span("z", 1)),
        integer_clock_transfer(
            BuiltinFunction::SuperSample,
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(test_var_at("d", source.span("d", 2))),
                rhs: Box::new(test_var_at("p", source.span("p", 1))),
                span: source.span("d+p", 0),
            },
            2,
            span_within(
                transfer_span,
                transfer_span.end.0 - transfer_span.start.0 - 2,
                1,
            ),
            transfer_span,
        ),
        source.span("z=superSample(d+p,2)", 0),
    );

    let _product = construct(&model, source.map)
        .expect("translation-time parameters may accompany a clocked source coordinate");
}

#[test]
fn clocked_residual_rejects_an_input_at_its_exact_use() {
    let source = TestSource::new(
        "Clock c=Clock(0.1); discrete Real clockedValue; input Real externalValue; \
         when c then clockedValue=1; end when; clockedValue+externalValue=0.0;",
    );
    let mut model = test_model();
    add_identity_test_clock(&mut model, &source);
    add_primitive_variable(
        &mut model,
        &source,
        "clockedValue",
        "discrete Real clockedValue",
        307,
        Vec::new(),
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("clockedValue"))
        .expect("clocked fixture exists")
        .variability = Variability::Discrete(Default::default());
    add_primitive_variable(
        &mut model,
        &source,
        "externalValue",
        "input Real externalValue",
        308,
        Vec::new(),
        false,
    );
    let input = model
        .variables
        .get_mut(&VarName::new("externalValue"))
        .expect("input fixture exists");
    input.causality = Causality::Input(Default::default());
    input.component_ref = Some(test_component_reference(
        "externalValue",
        source.span("input Real externalValue", 0),
    ));
    model
        .top_level_input_components
        .insert("externalValue".to_string());
    add_test_clock_assignment(&mut model, &source, "c", "clockedValue", 1);
    let input_use = source.span("externalValue", 1);
    let equation_span = source.span("clockedValue+externalValue=0.0", 0);
    add_test_residual(
        &mut model,
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(test_var_at("clockedValue", source.span("clockedValue", 2))),
            rhs: Box::new(test_var_at("externalValue", input_use)),
            span: source.span("clockedValue+externalValue", 0),
        },
        Expression::Literal {
            value: Literal::Real(0.0),
            span: source.span("0.0", 0),
        },
        equation_span,
    );

    let error = construct(&model, source.map)
        .expect_err("an external input must cross a clock boundary through sample");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "clocked continuous coordinate" && span == input_use
    ));
}

#[test]
fn duplicate_clock_instance_identity_rejects_the_second_declaration() {
    let source = TestSource::new("Clock first=Clock(0.1); Clock second=Clock(0.2);");
    let mut model = test_model();
    for (name, declaration, interval, type_id) in [
        ("first", "Clock first=Clock(0.1)", 0.1, 305),
        ("second", "Clock second=Clock(0.2)", 0.2, 306),
    ] {
        add_test_clock(
            &mut model,
            &source,
            TestClock {
                name,
                declaration,
                constructor_span: source.span(&format!("Clock({interval})"), 0),
                interval_span: source.span(&interval.to_string(), 0),
                interval,
                type_id,
            },
        );
    }
    let shared = model.variables[&VarName::new("first")].instance_id;
    model
        .variables
        .get_mut(&VarName::new("second"))
        .expect("second clock fixture exists")
        .instance_id = shared;
    let second_declaration = source.span("Clock second=Clock(0.2)", 0);

    let error = construct(&model, source.map)
        .expect_err("two clock coordinates cannot share one semantic identity");
    assert!(
        matches!(
            &error,
            ToDaeError::UnsupportedFlatSemantics { feature, detail, span }
                if feature == "Flat shape contract"
                    && detail.contains("DuplicateVariableInstanceId")
                    && *span == second_declaration
        ),
        "unexpected duplicate identity diagnostic: {error:?}"
    );
}

fn state_clock_model(source: &TestSource, with_target: bool) -> flat::Model {
    let mut model = test_model();
    add_identity_test_clock(&mut model, source);
    add_test_integer(&mut model, source, "d", "discrete Integer d", 300);
    if with_target {
        add_test_integer(&mut model, source, "z", "discrete Integer z", 301);
    }
    add_primitive_variable(&mut model, source, "x", "Real x", 302, Vec::new(), false);
    add_test_clock_assignment(&mut model, source, "c", "d", 1);
    model
}

fn add_identity_test_clock(model: &mut flat::Model, source: &TestSource) {
    add_test_clock(
        model,
        source,
        TestClock {
            name: "c",
            declaration: "Clock c=Clock(0.1)",
            constructor_span: source.span("Clock(0.1)", 0),
            interval_span: source.span("0.1", 0),
            interval: 0.1,
            type_id: 299,
        },
    );
}

fn add_derivative_equation(model: &mut flat::Model, source: &TestSource) {
    let equation_span = source.span("der(x)=0.0", 0);
    add_test_residual(
        model,
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "x",
                    test_component_reference("x", source.span("x", 2)),
                )
                .with_instance_id(test_instance_id("x")),
                subscripts: Vec::new(),
                span: source.span("x", 2),
            }],
            span: source.span("der(x)", 0),
        },
        Expression::Literal {
            value: Literal::Real(0.0),
            span: span_within(
                equation_span,
                equation_span.end.0 - equation_span.start.0 - 3,
                3,
            ),
        },
        equation_span,
    );
}
