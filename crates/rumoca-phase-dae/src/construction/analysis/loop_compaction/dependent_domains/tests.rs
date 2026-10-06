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
        let compacted =
            rectangularize_dependent_loops(&body, &HashMap::new(), &ShapeEnvironment::default())
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
    let compacted =
        rectangularize_dependent_loops(&body, &HashMap::new(), &ShapeEnvironment::default())
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
    let error = rectangularize_dependent_loops(&body, &HashMap::new(), &shapes)
        .expect_err("a body write cannot change the entry range");
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
    let compacted =
        rectangularize_dependent_loops(&body, &HashMap::new(), &ShapeEnvironment::default())
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
    let compacted =
        rectangularize_dependent_loops(&body, &HashMap::new(), &ShapeEnvironment::default())
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
    let compacted =
        rectangularize_dependent_loops(&body, &HashMap::new(), &ShapeEnvironment::default())
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
    let compacted = rectangularize_dependent_loops(&body, &static_integers, &shapes)
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
    let compacted = rectangularize_dependent_loops(&body, &HashMap::new(), &shapes)
        .expect("a missing arithmetic proof leaves the source for typed refusal");
    assert_eq!(
        format!("{body:?}"),
        format!("{compacted:?}"),
        "an overflowing generated difference cannot gain an owner"
    );
}
