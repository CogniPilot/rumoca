use super::*;
use rumoca_core::{ComponentRefPart, ComponentReference, DefId, Reference, SourceId};

fn span() -> Span {
    Span::from_offsets(SourceId::DUMMY, 0, 1)
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: span(),
    }
}

fn variable(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

fn assignment(name: &str, value: Expression) -> rumoca_core::Statement {
    let component = ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: Vec::new(),
            def_id: DefId::new(1),
        }],
    )
    .expect("test reference has one resolved local component");
    rumoca_core::Statement::Assignment {
        comp: component,
        value,
        span: span(),
    }
}

fn increment(name: &str) -> rumoca_core::Statement {
    assignment(
        name,
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(variable(name)),
            rhs: Box::new(integer(1)),
            span: span(),
        },
    )
}

fn for_loop(body: Vec<rumoca_core::Statement>) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: "i".to_string(),
            range: Expression::Range {
                start: Box::new(integer(1)),
                step: None,
                end: Box::new(integer(14400)),
                span: span(),
            },
        }],
        equations: body,
        span: span(),
    }
}

#[test]
fn repeated_counter_does_not_issue_one_iteration_as_a_global_bound() {
    for loop_statement in [
        for_loop(vec![increment("count")]),
        rumoca_core::Statement::While {
            block: rumoca_core::StatementBlock {
                cond: Expression::Binary {
                    op: OpBinary::Lt,
                    lhs: Box::new(variable("count")),
                    rhs: Box::new(integer(14400)),
                    span: span(),
                },
                stmts: vec![increment("count")],
            },
            span: span(),
        },
    ] {
        let mut shapes = ShapeEnvironment::with_capacity(3);
        infer_function_integer_bounds(
            &[assignment("count", integer(0)), loop_statement],
            &mut shapes,
        );
        assert_eq!(shapes.proven_integer_bounds(&variable("count")), None);
    }
}

#[test]
fn indirect_loop_recurrence_does_not_issue_a_finite_interval() {
    let statements = vec![
        assignment("a", integer(0)),
        assignment("b", integer(1)),
        for_loop(vec![assignment("a", variable("b")), increment("b")]),
    ];
    let mut shapes = ShapeEnvironment::with_capacity(3);
    infer_function_integer_bounds(&statements, &mut shapes);
    assert_eq!(shapes.proven_integer_bounds(&variable("a")), None);
    assert_eq!(shapes.proven_integer_bounds(&variable("b")), None);
}

#[test]
fn unknown_assignment_invalidates_the_old_bound_even_after_a_known_write() {
    let mut shapes = ShapeEnvironment::with_capacity(3);
    infer_function_integer_bounds(
        &[
            assignment("limit", integer(3)),
            assignment("limit", variable("input")),
            assignment("limit", integer(7)),
        ],
        &mut shapes,
    );
    assert_eq!(shapes.proven_integer_bounds(&variable("limit")), None);
}

#[test]
fn immutable_binders_and_independent_finite_definitions_keep_their_bounds() {
    let mut shapes = ShapeEnvironment::with_capacity(3);
    infer_function_integer_bounds(
        &[
            assignment("limit", integer(3)),
            for_loop(vec![assignment("scratch", integer(2))]),
        ],
        &mut shapes,
    );
    assert_eq!(
        shapes.proven_integer_bounds(&variable("limit")),
        Some((3, 3))
    );
    assert_eq!(shapes.proven_integer_bounds(&variable("i")), None);
    assert_eq!(
        shapes.proven_integer_bounds(&variable("scratch")),
        Some((2, 2))
    );
}

#[test]
fn loop_binder_shadow_does_not_replace_the_enclosing_input_fact() {
    let mut shapes = ShapeEnvironment::with_capacity(3);
    shapes.bind_scalar_value(VarName::new("i"), EvalValue::Integer(987));
    infer_function_integer_bounds(
        &[for_loop(vec![assignment("seen", variable("i"))])],
        &mut shapes,
    );
    assert_eq!(
        shapes.proven_integer_bounds(&variable("i")),
        Some((987, 987))
    );
    assert_eq!(
        shapes.proven_integer_bounds(&variable("seen")),
        Some((1, 14400))
    );
}

#[test]
fn nonlinear_integer_interval_contains_interior_values_and_rejects_overflow() {
    let mut shapes = ShapeEnvironment::with_capacity(2);
    shapes.bind_integer_bounds(VarName::new("i"), -3, 3);
    let squared = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(variable("i")),
        rhs: Box::new(variable("i")),
        span: span(),
    };
    assert_eq!(shapes.proven_integer_bounds(&squared), Some((-9, 9)));
    shapes.bind_integer_bounds(VarName::new("i"), 0, i64::MAX);
    assert_eq!(shapes.proven_integer_bounds(&squared), None);
}

#[test]
fn multi_output_call_invalidates_each_receiving_slot() {
    let rumoca_core::Statement::Assignment { comp, .. } = assignment("count", integer(0)) else {
        panic!("helper constructs an assignment")
    };
    let mut shapes = ShapeEnvironment::with_capacity(2);
    infer_function_integer_bounds(
        &[
            assignment("count", integer(3)),
            rumoca_core::Statement::FunctionCall {
                comp: Reference::new("opaque"),
                args: vec![],
                outputs: vec![None, Some(comp)],
                span: span(),
            },
        ],
        &mut shapes,
    );
    assert_eq!(shapes.proven_integer_bounds(&variable("count")), None);
}

#[test]
fn bounded_integer_builtins_cover_every_concrete_operand_and_refuse_unknown_divisors() {
    let mut shapes = ShapeEnvironment::with_capacity(2);
    shapes.bind_integer_bounds(VarName::new("i"), -5, 5);
    for (function, expected) in [
        (BuiltinFunction::Min, (-5, 2)),
        (BuiltinFunction::Max, (2, 5)),
        (BuiltinFunction::Div, (-2, 2)),
        (BuiltinFunction::Mod, (0, 1)),
    ] {
        let expression = Expression::BuiltinCall {
            function,
            args: vec![variable("i"), integer(2)],
            span: span(),
        };
        assert_eq!(shapes.proven_integer_bounds(&expression), Some(expected));
        for value in -5_i64..=5 {
            let actual = match function {
                BuiltinFunction::Min => value.min(2),
                BuiltinFunction::Max => value.max(2),
                BuiltinFunction::Div => value / 2,
                BuiltinFunction::Mod => value.rem_euclid(2),
                _ => unreachable!("the test enumerates Integer binary builtins"),
            };
            assert!((expected.0..=expected.1).contains(&actual));
        }
    }
    shapes.bind_integer_bounds(VarName::new("divisor"), 1, 2);
    let expression = Expression::BuiltinCall {
        function: BuiltinFunction::Div,
        args: vec![variable("i"), variable("divisor")],
        span: span(),
    };
    assert_eq!(shapes.proven_integer_bounds(&expression), None);
}

#[test]
fn proven_tensor_extents_bound_ranges_without_publishing_settled_local_values() {
    let mut shapes = ShapeEnvironment::with_capacity(2);
    shapes.insert(VarName::new("scores"), vec![14400]);
    let expression = Expression::BuiltinCall {
        function: BuiltinFunction::Size,
        args: vec![variable("scores"), integer(1)],
        span: span(),
    };
    assert_eq!(
        shapes.proven_integer_bounds(&expression),
        Some((14400, 14400))
    );
    infer_function_integer_bounds(&[assignment("limit", expression)], &mut shapes);
    assert_eq!(
        shapes.proven_integer_bounds(&variable("limit")),
        Some((14400, 14400))
    );
    assert_eq!(shapes.proven_extent(&variable("limit")), None);
}

#[test]
fn a_read_after_the_loop_sees_the_merged_interval_of_a_noncarried_target() {
    // `last := i` inside the loop carries nothing, so after the loop `last`
    // lies in its merged interval and `selected := last` inherits it.
    let statements = vec![
        assignment("last", integer(0)),
        assignment("selected", integer(0)),
        for_loop(vec![assignment("last", variable("i"))]),
        assignment("selected", variable("last")),
    ];
    let mut shapes = ShapeEnvironment::with_capacity(3);
    infer_function_integer_bounds(&statements, &mut shapes);
    assert_eq!(
        shapes.proven_integer_bounds(&variable("last")),
        Some((0, 14400))
    );
    assert_eq!(
        shapes.proven_integer_bounds(&variable("selected")),
        Some((0, 14400))
    );
}

#[test]
fn a_read_inside_the_writing_loop_may_see_a_carried_value() {
    // In the loop, `selected := last` may read the previous iteration's
    // `last`; that is a carried read and keeps no finite interval.
    let statements = vec![
        assignment("last", integer(0)),
        assignment("selected", integer(0)),
        for_loop(vec![
            assignment("selected", variable("last")),
            assignment("last", variable("i")),
        ]),
    ];
    let mut shapes = ShapeEnvironment::with_capacity(3);
    infer_function_integer_bounds(&statements, &mut shapes);
    assert_eq!(shapes.proven_integer_bounds(&variable("selected")), None);
}
