use super::super::preservation_corpus::{
    ENTRY_VALUES, assign, assign_element, binary, element, for_loop, integer, real, span, var,
};
use super::super::preservation_interpreter::interpret;
use super::*;

fn dynamic_loop(
    start: Expression,
    step: Option<i64>,
    end: Expression,
    body: Vec<rumoca_core::Statement>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            binder: rumoca_core::IndexBinderProvenance::Generated,
            ident: "j".to_string(),
            range: Expression::Range {
                start: Box::new(start),
                step: step.map(|value| Box::new(integer(value))),
                end: Box::new(end),
                span: span(),
            },
        }],
        equations: body,
        span: span(),
    }
}

fn ordered_update() -> rumoca_core::Statement {
    assign(
        "y",
        binary(
            OpBinary::Add,
            binary(OpBinary::Mul, var("y"), real(10.0)),
            var("j"),
        ),
    )
}

#[test]
fn bounded_dynamic_strides_preserve_point_order_empty_domains_and_array_aliases() {
    for step in [1, 2, -1, -2] {
        let (start, end) = if step > 0 {
            (var("i"), integer(3))
        } else {
            (var("i"), integer(1))
        };
        let body = vec![
            assign("y", real(0.0)),
            assign("t", var("r")),
            for_loop(
                "i",
                3,
                vec![dynamic_loop(
                    start,
                    Some(step),
                    end,
                    vec![
                        assign_element(
                            "t",
                            var("j"),
                            binary(OpBinary::Add, element("t", var("j")), var("y")),
                        ),
                        ordered_update(),
                    ],
                )],
            ),
            assign("w", var("t")),
        ];
        let compacted = rectangularize_dependent_loops(
            &body,
            &HashMap::new(),
            &ShapeEnvironment::default(),
            &ValueKinds::default(),
        )
        .expect("immutable bounded strided domains have a compact owner");
        assert_ne!(format!("{body:?}"), format!("{compacted:?}"));
        for entry in ENTRY_VALUES {
            let environment = entry.environment();
            let before =
                interpret(&body, &environment).expect("source fixture is finite and in bounds");
            let after = interpret(&compacted, &environment)
                .expect("compact fixture executes the same points");
            for name in ["y", "w"] {
                assert_eq!(
                    before.get(&VarName::new(name)),
                    after.get(&VarName::new(name)),
                    "step{step} {name}"
                );
            }
        }
    }
    let body = vec![
        assign("y", real(0.0)),
        for_loop(
            "i",
            3,
            vec![dynamic_loop(
                var("i"),
                Some(-1),
                integer(4),
                vec![ordered_update()],
            )],
        ),
    ];
    let compacted = rectangularize_dependent_loops(
        &body,
        &HashMap::new(),
        &ShapeEnvironment::default(),
        &ValueKinds::default(),
    )
    .expect("the empty descending envelope remains compact");
    for entry in ENTRY_VALUES {
        assert_eq!(
            interpret(&body, &entry.environment()),
            interpret(&compacted, &entry.environment())
        );
    }
}

#[test]
fn mutable_range_operand_requires_entry_snapshot_instead_of_live_membership_reads() {
    let body = vec![dynamic_loop(
        integer(1),
        None,
        var("k"),
        vec![ordered_update(), assign("k", integer(0))],
    )];
    let mut shapes = ShapeEnvironment::default();
    shapes.bind_integer_bounds(VarName::new("k"), 0, 3);
    let error =
        rectangularize_dependent_loops(&body, &HashMap::new(), &shapes, &ValueKinds::default())
            .expect_err("a body write cannot change the entry range");
    assert!(format!("{error:?}").contains("entry snapshot"));
}

#[test]
fn shape_query_range_operand_needs_no_entry_snapshot_but_element_read_does() {
    let body = vec![ordered_update(), assign("x", integer(0))];
    let size_end = Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("x"), integer(1)],
        span: span(),
    };
    require_entry_invariant_range(&size_end, &body, span())
        .expect("a declared-shape query is not a value read");
    let error = require_entry_invariant_range(&element("x", integer(1)), &body, span())
        .expect_err("an element read of a written operand needs the snapshot");
    assert!(format!("{error:?}").contains("entry snapshot"));
}

#[test]
fn full14400_parent_domain_does_not_expand_statement_storage() {
    let body = vec![for_loop(
        "i",
        14400,
        vec![dynamic_loop(
            var("i"),
            Some(-2),
            integer(1),
            vec![assign("y", binary(OpBinary::Add, var("y"), var("j")))],
        )],
    )];
    let compacted = rectangularize_dependent_loops(
        &body,
        &HashMap::new(),
        &ShapeEnvironment::default(),
        &ValueKinds::default(),
    )
    .expect("the full finite parent keeps one bounded nested domain");
    let [rumoca_core::Statement::For { equations, .. }] = compacted.as_slice() else {
        panic!("outer source loop remains one owner")
    };
    let [
        rumoca_core::Statement::For {
            indices, equations, ..
        },
    ] = equations.as_slice()
    else {
        panic!("inner source loop remains one owner")
    };
    assert_eq!(indices.len(), 1);
    assert_eq!(equations.len(), 1);
    assert!(matches!(&equations[0], rumoca_core::Statement::If { .. }));
}

#[test]
fn nonlinear_range_start_keeps_interior_points_in_its_conservative_envelope() {
    let outer = rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            binder: rumoca_core::IndexBinderProvenance::Generated,
            ident: "i".to_string(),
            range: Expression::Range {
                start: Box::new(integer(-3)),
                step: None,
                end: Box::new(integer(3)),
                span: span(),
            },
        }],
        equations: vec![dynamic_loop(
            binary(OpBinary::Mul, var("i"), var("i")),
            None,
            integer(9),
            vec![assign("y", binary(OpBinary::Add, var("y"), var("j")))],
        )],
        span: span(),
    };
    let body = vec![assign("y", real(0.0)), outer];
    let compacted = rectangularize_dependent_loops(
        &body,
        &HashMap::new(),
        &ShapeEnvironment::default(),
        &ValueKinds::default(),
    )
    .expect("checked interval arithmetic bounds the nonlinear start");
    for entry in ENTRY_VALUES {
        let before = interpret(&body, &entry.environment()).expect("source is finite");
        let after = interpret(&compacted, &entry.environment())
            .expect("compact mask preserves all interior points");
        assert_eq!(
            before.get(&VarName::new("y")),
            after.get(&VarName::new("y"))
        );
    }
}

#[test]
fn empty_dynamic_domain_preserves_the_initial_signed_zero() {
    use super::super::preservation_values::Value;
    let body = vec![
        assign("y", real(-0.0)),
        for_loop(
            "i",
            3,
            vec![dynamic_loop(
                var("i"),
                None,
                integer(0),
                vec![assign("y", var("j"))],
            )],
        ),
    ];
    let compacted = rectangularize_dependent_loops(
        &body,
        &HashMap::new(),
        &ShapeEnvironment::default(),
        &ValueKinds::default(),
    )
    .expect("a proven empty envelope has a compact owner");
    assert_ne!(format!("{body:?}"), format!("{compacted:?}"));
    for entry in ENTRY_VALUES {
        for statements in [&body, &compacted] {
            let values =
                interpret(statements, &entry.environment()).expect("the empty domain terminates");
            let Some(Value::Real(value)) = values.get(&VarName::new("y")) else {
                panic!("the Real output retains its initial type")
            };
            assert_eq!(value.to_bits(), (-0.0_f64).to_bits());
        }
    }
}

#[test]
fn enclosing_binder_shadows_settled_outer_values_during_envelope_recognition() {
    let body = vec![
        assign("y", real(0.0)),
        for_loop(
            "i",
            3,
            vec![dynamic_loop(
                var("i"),
                Some(2),
                integer(3),
                vec![ordered_update()],
            )],
        ),
    ];
    let mut shapes = ShapeEnvironment::default();
    shapes.bind_scalar_value(VarName::new("i"), EvalValue::Integer(987));
    let static_integers = HashMap::from([(VarName::new("i"), 987)]);
    let compacted =
        rectangularize_dependent_loops(&body, &static_integers, &shapes, &ValueKinds::default())
            .expect("the inner range reads the lexical binder");
    assert_ne!(format!("{body:?}"), format!("{compacted:?}"));
    for entry in ENTRY_VALUES {
        let before = interpret(&body, &entry.environment()).expect("source is finite");
        let after = interpret(&compacted, &entry.environment()).expect("compact source is finite");
        assert_eq!(
            before.get(&VarName::new("y")),
            after.get(&VarName::new("y"))
        );
    }
}

#[test]
fn generated_stride_difference_requires_checked_integer_bounds() {
    let body = vec![dynamic_loop(
        var("limit"),
        Some(2),
        integer(0),
        vec![ordered_update()],
    )];
    let mut shapes = ShapeEnvironment::default();
    shapes.bind_integer_bounds(VarName::new("limit"), i64::MIN, i64::MIN + 1);
    let compacted =
        rectangularize_dependent_loops(&body, &HashMap::new(), &shapes, &ValueKinds::default())
            .expect("a missing arithmetic proof leaves the source for typed refusal");
    assert_eq!(
        format!("{body:?}"),
        format!("{compacted:?}"),
        "an overflowing generated difference cannot gain an owner"
    );
}

#[test]
fn membership_guard_uses_issued_source_id_with_unused_or_shadowed_body() {
    let issued = rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(42));
    let inner = rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(43));
    let nested = rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            binder: inner,
            ident: "j".into(),
            range: integer_range(1, 2, span()),
        }],
        equations: vec![assign(
            "y",
            Expression::VarRef {
                name: inner.reference("j", span()).unwrap(),
                subscripts: Vec::new(),
                span: span(),
            },
        )],
        span: span(),
    };
    for body in [vec![assign("y", real(7.0))], vec![nested]] {
        let mut statement = dynamic_loop(integer(1), None, var("k"), body);
        let rumoca_core::Statement::For { indices, .. } = &mut statement else {
            unreachable!()
        };
        indices[0].binder = issued;
        let mut shapes = ShapeEnvironment::default();
        shapes.bind_integer_bounds(VarName::new("k"), 1, 3);
        let compacted = rectangularize_dependent_loops(
            &[statement],
            &HashMap::new(),
            &shapes,
            &ValueKinds::default(),
        )
        .unwrap();
        let rumoca_core::Statement::For { equations, .. } = &compacted[0] else {
            panic!("compact loop")
        };
        let rumoca_core::Statement::If { cond_blocks, .. } = &equations[0] else {
            panic!("membership guard")
        };
        assert_guard_identity(&cond_blocks[0].cond, issued);
    }
}

fn assert_guard_identity(guard: &Expression, binder: rumoca_core::IndexBinderProvenance) {
    struct References(Vec<Reference>);
    impl rumoca_core::ExpressionVisitor for References {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            if reference.as_str() == "j" {
                self.0.push(reference.clone());
            }
            self.walk_var_ref(reference, subscripts);
        }
    }
    let mut references = References(Vec::new());
    rumoca_core::ExpressionVisitor::visit_expression(&mut references, guard);
    assert!(!references.0.is_empty());
    for reference in references.0 {
        match binder {
            rumoca_core::IndexBinderProvenance::Source(id) => {
                assert_eq!(reference.target_def_id(), Some(id))
            }
            rumoca_core::IndexBinderProvenance::Generated => assert!(reference.is_generated()),
        }
    }
}

#[test]
fn membership_guard_constructor_preserves_generated_and_refuses_reserved_source_id() {
    let range_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("membership_guard.mo"),
        2,
        6,
    );
    let range = integer_range(1, 3, range_span);
    let guard = range_membership_guard(
        rumoca_core::IndexBinderProvenance::Generated,
        "j",
        &range,
        1,
        span(),
    )
    .unwrap();
    assert_guard_identity(&guard, rumoca_core::IndexBinderProvenance::Generated);
    let error = range_membership_guard(
        rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(0)),
        "j",
        &range,
        1,
        span(),
    )
    .unwrap_err();
    assert!(
        matches!(error, ToDaeError::UnsupportedFlatSemantics { feature, detail, span: actual } if feature == "function loop iterator" && detail.contains("reserved unresolved DefId(0)") && actual == range_span)
    );
}

#[test]
fn membership_guard_comprehension_preserves_body_read_admission_and_issued_identity() {
    let issued = rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(42));
    let foreign = rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(41));
    let mut shapes = ShapeEnvironment::default();
    shapes.bind_integer_bounds(VarName::new("k"), 1, 3);
    let bounds = HashMap::from([(VarName::new("k"), (1, 3))]);
    let integers = HashMap::new();
    for (binder, body, admitted) in [
        (issued, integer(7), false),
        (
            issued,
            Expression::VarRef {
                name: foreign.reference("j", span()).unwrap(),
                subscripts: Vec::new(),
                span: span(),
            },
            false,
        ),
        (
            rumoca_core::IndexBinderProvenance::Generated,
            Expression::VarRef {
                name: foreign.reference("j", span()).unwrap(),
                subscripts: Vec::new(),
                span: span(),
            },
            false,
        ),
        (
            issued,
            Expression::VarRef {
                name: issued.reference("j", span()).unwrap(),
                subscripts: Vec::new(),
                span: span(),
            },
            true,
        ),
    ] {
        let expression = Expression::ArrayComprehension {
            expr: Box::new(body),
            indices: vec![rumoca_core::ComprehensionIndex {
                binder,
                name: "j".into(),
                range: Expression::Range {
                    start: Box::new(integer(1)),
                    step: None,
                    end: Box::new(var("k")),
                    span: span(),
                },
            }],
            filter: None,
            span: span(),
        };
        let mut rewriter = MaskedComprehensionRewriter {
            static_integers: &integers,
            shapes: &shapes,
            bounds: &bounds,
            error: None,
        };
        let rewritten = rewriter.rewrite_expression(&expression);
        assert!(rewriter.error.is_none());
        if !admitted {
            assert_eq!(rewritten, expression);
            continue;
        }
        let Expression::ArrayComprehension { expr, .. } = rewritten else {
            panic!("constructor")
        };
        let Expression::If { branches, .. } = *expr else {
            panic!("admitted membership guard")
        };
        assert_guard_identity(&branches[0].0, issued);
    }
}
