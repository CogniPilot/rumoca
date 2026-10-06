mod fixture;

use super::*;
use fixture::*;

#[test]
fn full_14400_conditional_counter_has_global_envelope_not_a_settled_value() {
    let source = program(
        0,
        range(1, None, 14400),
        vec![conditional(vec![increment()])],
    );
    let mut shapes = empty_shapes();
    infer_function_integer_bounds(&source, &mut shapes);
    assert_eq!(shapes.proven_integer_bounds(&count()), None);
    assert_eq!(prove(&source, &mut shapes), Some((0, 14400)));
    assert_eq!(shapes.proven_extent(&count()), None);
}

#[test]
fn induction_uses_complete_cardinality_and_preserves_initializer_and_empty_ranges() {
    for (start, step, end, initial, expected) in [
        (1, None, 14400, 7, (7, 14407)),
        (14400, Some(-1), 1, -7, (-7, 14393)),
        (1, Some(3), 14400, 0, (0, 4800)),
        (9, Some(-2), 1, 0, (0, 5)),
        (2, None, 1, 4, (4, 4)),
        (1, Some(-1), 2, 4, (4, 4)),
    ] {
        let source = program(initial, range(start, step, end), vec![increment()]);
        assert_eq!(prove(&source, &mut empty_shapes()), Some(expected));
    }
}

#[test]
fn canonical_size_domain_uses_immutable_checked_shape_without_enumerating_cells() {
    let scores = variable("scores", DefId::new(8103));
    let end = Expression::BuiltinCall {
        function: BuiltinFunction::Size,
        args: vec![scores, integer(1)],
        span: span(),
    };
    let source = program(
        0,
        Expression::Range {
            start: Box::new(integer(1)),
            step: None,
            end: Box::new(end),
            span: span(),
        },
        vec![conditional(vec![increment()])],
    );
    let mut shapes = empty_shapes();
    shapes.insert(VarName::new("scores"), vec![14400]);
    assert_eq!(prove(&source, &mut shapes), Some((0, 14400)));
}

#[test]
fn unknown_decrement_multiple_writes_and_nested_unproved_loops_keep_refusal() {
    let bodies = [
        vec![update(variable("input", DefId::new(8104)))],
        vec![update(binary(OpBinary::Sub, count(), integer(1)))],
        vec![update(binary(OpBinary::Add, count(), integer(2)))],
        vec![increment(), increment()],
        vec![
            conditional(vec![increment()]),
            conditional(vec![increment()]),
        ],
        vec![for_range(range(1, None, 2), vec![increment()])],
        vec![Statement::While {
            block: rumoca_core::StatementBlock {
                cond: variable("accept", DefId::new(8102)),
                stmts: vec![increment()],
            },
            span: span(),
        }],
    ];
    for body in bodies {
        let source = program(0, range(1, None, 14400), body);
        assert_eq!(prove(&source, &mut empty_shapes()), None);
    }
}

#[test]
fn extra_writes_prefix_reads_shadowing_and_wrong_declaration_are_not_evidence() {
    let base = program(0, range(1, None, 14400), vec![increment()]);
    let mut suffix = base.clone();
    suffix.push(update(integer(9)));
    let mut prefix = base.clone();
    prefix.insert(0, assignment("seen", DefId::new(8105), count()));
    let mut shadow = base.clone();
    let Statement::For { indices, .. } = &mut shadow[1] else {
        panic!("fixture For")
    };
    indices[0].ident = "count".to_owned();
    for source in [suffix, prefix, shadow] {
        assert_eq!(prove(&source, &mut empty_shapes()), None);
    }
    let mut wrong = base;
    wrong[1] = for_range(
        range(1, None, 14400),
        vec![assignment("count", DefId::new(999), integer(1))],
    );
    assert_eq!(prove(&wrong, &mut empty_shapes()), None);
}

#[test]
fn arithmetic_overflow_zero_or_unproved_range_and_opaque_receivers_keep_refusal() {
    for (initial, domain) in [
        (i64::MAX, range(1, None, 1)),
        (0, range(i64::MIN, None, i64::MAX)),
        (0, range(1, Some(0), 14400)),
    ] {
        assert_eq!(
            prove(
                &program(initial, domain, vec![increment()]),
                &mut empty_shapes()
            ),
            None
        );
    }
    let mut source = program(0, range(1, None, 14400), vec![increment()]);
    source.push(Statement::FunctionCall {
        comp: rumoca_core::Reference::new("opaque"),
        args: vec![],
        outputs: vec![Some(component("count", COUNTER))],
        span: span(),
    });
    assert_eq!(prove(&source, &mut empty_shapes()), None);
    let domain = Expression::Range {
        start: Box::new(integer(1)),
        step: None,
        end: Box::new(variable("input", DefId::new(8104))),
        span: span(),
    };
    assert_eq!(
        prove(&program(0, domain, vec![increment()]), &mut empty_shapes()),
        None
    );
}

#[test]
fn range_operand_mutation_does_not_reuse_a_shape_environment_point_fact() {
    let limit_id = DefId::new(8110);
    let domain = Expression::Range {
        start: Box::new(integer(1)),
        step: None,
        end: Box::new(variable("limit", limit_id)),
        span: span(),
    };
    let mut source = program(0, domain, vec![increment()]);
    source.push(assignment("limit", limit_id, integer(1)));
    let mut shapes = empty_shapes();
    shapes.bind_integer_bounds(VarName::new("limit"), 14400, 14400);
    assert_eq!(prove(&source, &mut shapes), None);
}

#[test]
fn nested_conditionals_allow_one_optional_site_but_not_alternative_write_sites() {
    let nested = program(
        0,
        range(1, None, 14400),
        vec![conditional(vec![conditional(vec![increment()])])],
    );
    assert_eq!(prove(&nested, &mut empty_shapes()), Some((0, 14400)));
    let mut duplicate = conditional(vec![increment()]);
    let Statement::If { else_block, .. } = &mut duplicate else {
        panic!("fixture If")
    };
    *else_block = Some(vec![increment()]);
    assert_eq!(
        prove(
            &program(0, range(1, None, 14400), vec![duplicate]),
            &mut empty_shapes()
        ),
        None
    );
}

#[test]
fn missing_or_nonliteral_initializer_and_partial_targets_are_not_certificates() {
    let mut missing = program(0, range(1, None, 14400), vec![increment()]);
    missing.remove(0);
    assert_eq!(prove(&missing, &mut empty_shapes()), None);
    let mut dynamic = program(0, range(1, None, 14400), vec![increment()]);
    dynamic[0] = update(variable("input", DefId::new(8104)));
    assert_eq!(prove(&dynamic, &mut empty_shapes()), None);
    let mut partial = program(0, range(1, None, 14400), vec![increment()]);
    let part = rumoca_core::ComponentRefPart {
        ident: "count".to_owned(),
        span: span(),
        subs: vec![Subscript::Expr {
            expr: Box::new(integer(1)),
            span: span(),
        }],
        def_id: COUNTER,
    };
    let comp = rumoca_core::ComponentReference::construct(false, span(), vec![part])
        .expect("resolved partial reference");
    partial[0] = Statement::Assignment {
        comp,
        value: integer(0),
        span: span(),
    };
    assert_eq!(prove(&partial, &mut empty_shapes()), None);
}

#[test]
fn range_identity_cannot_borrow_an_unrelated_cached_spelling_or_definition() {
    let limit = component("limit", DefId::new(8110));
    let renamed = rumoca_core::Reference::from_component_reference(limit)
        .with_var_name(VarName::new("other"));
    let expressions = [
        Expression::VarRef {
            name: renamed,
            subscripts: vec![],
            span: span(),
        },
        variable("limit", DefId::new(999)),
        Expression::VarRef {
            name: rumoca_core::Reference::new("limit"),
            subscripts: vec![],
            span: span(),
        },
    ];
    for end in expressions {
        let mut shapes = empty_shapes();
        shapes.bind_scalar_value(VarName::new("limit"), EvalValue::Integer(14400));
        shapes.bind_scalar_value(VarName::new("other"), EvalValue::Integer(14400));
        let domain = Expression::Range {
            start: Box::new(integer(1)),
            step: None,
            end: Box::new(end),
            span: span(),
        };
        assert_eq!(
            prove(&program(0, domain, vec![increment()]), &mut shapes),
            None
        );
    }
}

#[test]
fn range_input_binder_collision_and_same_name_foreign_write_cannot_borrow_point_facts() {
    let end = variable("limit", DefId::new(8110));
    let domain = Expression::Range {
        start: Box::new(integer(1)),
        step: None,
        end: Box::new(end),
        span: span(),
    };
    let base = program(0, domain, vec![increment()]);
    let mut shadow = base.clone();
    let Statement::For { indices, .. } = &mut shadow[1] else {
        panic!("fixture For")
    };
    indices[0].ident = "limit".to_owned();
    let mut foreign = base;
    foreign.insert(0, assignment("limit", DefId::new(999), integer(17)));
    for source in [shadow, foreign] {
        let mut shapes = empty_shapes();
        shapes.bind_scalar_value(VarName::new("limit"), EvalValue::Integer(14400));
        assert_eq!(prove(&source, &mut shapes), None);
    }
}

#[test]
fn range_fact_requires_owning_input_metadata_not_only_a_resolved_component() {
    let end = variable("limit", DefId::new(8110));
    let domain = Expression::Range {
        start: Box::new(integer(1)),
        step: None,
        end: Box::new(end),
        span: span(),
    };
    let source = program(0, domain, vec![increment()]);
    for inputs in [vec![], vec![(VarName::new("limit"), DefId::new(999))]] {
        let mut shapes = empty_shapes();
        shapes.bind_scalar_value(VarName::new("limit"), EvalValue::Integer(14400));
        infer_finite_for_counter_bounds(
            &source,
            &mut shapes,
            &[(VarName::new("count"), COUNTER)],
            &inputs,
        );
        assert_eq!(shapes.proven_integer_bounds(&count()), None);
    }
}
