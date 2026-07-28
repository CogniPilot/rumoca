use super::*;

fn literal(value: rumoca_core::Literal) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value,
        span: test_span(),
    }
}

fn assertion_statement(condition: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assert {
        condition,
        message: Box::new(literal(rumoca_core::Literal::String(
            "algorithm assertion failed".to_string(),
        ))),
        level: None,
        span: test_span(),
    }
}

fn assert_action_condition(dae: &rumoca_ir_dae::Dae) -> &rumoca_core::Expression {
    let action = dae
        .events
        .event_actions
        .first()
        .expect("algorithm assertion must produce an event action");
    assert!(
        matches!(
            action.kind,
            rumoca_ir_dae::DaeEventActionKind::Assert { .. }
        ),
        "algorithm assertion must preserve its action kind"
    );
    &action.condition
}

#[test]
fn assignment_before_assert_uses_algorithm_current_value() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    flat.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("x"),
                value: literal(rumoca_core::Literal::Real(2.0)),
                span: test_span(),
            },
            assertion_statement(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Eq,
                lhs: Box::new(make_var_ref("x")),
                rhs: Box::new(literal(rumoca_core::Literal::Real(2.0))),
                span: test_span(),
            }),
        ],
        test_span(),
        "ordered algorithm assertion",
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("ordered algorithm assertion should lower");

    assert!(
        matches!(
            assert_action_condition(&dae),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                ..
            }
        ),
        "the successful assertion must use x := 2 rather than x at algorithm entry"
    );
}

#[test]
fn parser_shaped_assert_call_is_not_dropped() {
    let mut flat = Model::new();
    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::FunctionCall {
            comp: make_comp_ref("assert"),
            args: vec![
                literal(rumoca_core::Literal::Boolean(false)),
                literal(rumoca_core::Literal::String("failed".to_string())),
            ],
            outputs: Vec::new(),
            span: test_span(),
        }],
        test_span(),
        "parser-shaped assert call",
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("parser-shaped assert call should lower");

    assert!(
        matches!(
            assert_action_condition(&dae),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                ..
            }
        ),
        "assert(false, ...) must produce an active failure action"
    );
}

#[test]
fn nested_algorithm_assert_is_rejected_instead_of_dropped() {
    let mut flat = Model::new();
    flat.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: literal(rumoca_core::Literal::Boolean(true)),
                stmts: vec![assertion_statement(literal(rumoca_core::Literal::Boolean(
                    false,
                )))],
            }],
            else_block: None,
            span: test_span(),
        }],
        test_span(),
        "nested algorithm assertion",
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("unsupported nested assertion ordering must fail");

    assert!(
        matches!(
            err,
            ToDaeError::UnsupportedAlgorithm {
                ref section,
                ref origin,
                ..
            } if section == "model" && origin.contains("Assert")
        ),
        "nested assertion must report ED013, got {err:?}"
    );
}

#[test]
fn assert_before_assignment_is_rejected_instead_of_using_final_value() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    flat.algorithms.push(flat::Algorithm::new(
        vec![
            assertion_statement(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: Box::new(make_var_ref("x")),
                rhs: Box::new(literal(rumoca_core::Literal::Real(0.0))),
                span: test_span(),
            }),
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("x"),
                value: literal(rumoca_core::Literal::Real(2.0)),
                span: test_span(),
            },
        ],
        test_span(),
        "assert before assignment",
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("DAE equations cannot represent this algorithm-entry read");

    assert!(
        matches!(
            err,
            ToDaeError::UnsupportedAlgorithm {
                ref section,
                ref origin,
                ..
            } if section == "model" && origin.contains("AssertReadsTargetBeforeAssignment")
        ),
        "unsupported assertion ordering must report ED013, got {err:?}"
    );
}
