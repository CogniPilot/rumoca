use super::*;

#[test]
fn test_todae_if_reassignment_uses_current_step_false_branch() {
    let mut flat = Model::new();
    add_discrete_valued(&mut flat, "cond");
    add_discrete_real(&mut flat, "raw");
    add_discrete_real(&mut flat, "x");
    add_discrete_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("x"),
                value: make_var_ref("raw"),
                span: test_span(),
            },
            rumoca_core::Statement::If {
                cond_blocks: vec![rumoca_core::StatementBlock {
                    cond: make_var_ref("cond"),
                    stmts: vec![rumoca_core::Statement::Assignment {
                        comp: make_comp_ref("x"),
                        value: rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: test_span(),
                        },
                        span: test_span(),
                    }],
                }],
                else_block: None,
                span: test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("x"),
                span: test_span(),
            },
        ],
        test_span(),
        "if reassignment after current value".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("conditional reassignment should lower");

    let updates = dae
        .discrete
        .real_updates
        .iter()
        .filter_map(|eq| {
            Some((
                eq.lhs.as_ref()?.as_str().to_string(),
                format!("{:?}", eq.rhs),
            ))
        })
        .collect::<std::collections::HashMap<_, _>>();

    for target in ["x", "y"] {
        let rhs = updates
            .get(target)
            .unwrap_or_else(|| panic!("missing lowered update for {target}"));
        assert!(
            rhs.contains("VarName(\"raw\")"),
            "{target} should retain the current-step raw value in the false branch, got {rhs}"
        );
        assert!(
            !rhs.contains("VarName(\"__pre__.x\")"),
            "{target} must not read pre(x) for an if reassignment false branch, got {rhs}"
        );
    }
}

#[test]
fn test_todae_if_subscript_reassignment_uses_current_step_array_value() {
    let mut flat = Model::new();
    add_discrete_valued(&mut flat, "cond");
    add_discrete_real_vector(&mut flat, "raw", 2);
    add_discrete_real_vector(&mut flat, "y", 2);

    flat.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("raw"),
                span: test_span(),
            },
            rumoca_core::Statement::If {
                cond_blocks: vec![rumoca_core::StatementBlock {
                    cond: make_var_ref("cond"),
                    stmts: vec![rumoca_core::Statement::Assignment {
                        comp: make_subscripted_comp_ref(
                            "y",
                            rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: test_span(),
                            },
                        ),
                        value: rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.0),
                            span: test_span(),
                        },
                        span: test_span(),
                    }],
                }],
                else_block: None,
                span: test_span(),
            },
        ],
        test_span(),
        "if subscript reassignment after current array value".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("conditional subscript reassignment should lower");

    let rhs = dae
        .discrete
        .real_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y[1]"))
        .map(|eq| format!("{:?}", eq.rhs))
        .expect("missing lowered update for y[1]");

    assert!(
        rhs.contains("VarName(\"raw\")"),
        "y[1] should retain raw[1] in the false branch, got {rhs}"
    );
    assert!(
        !rhs.contains("VarName(\"__pre__.y\")"),
        "y[1] must not read pre(y) for an if reassignment false branch, got {rhs}"
    );
}
