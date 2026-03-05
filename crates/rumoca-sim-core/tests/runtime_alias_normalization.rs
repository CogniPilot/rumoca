use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_sim_core::normalize_runtime_aliases_collect;

fn var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn int(v: i64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Integer(v))
}

fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn lt(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Lt(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn eq(rhs: dae::Expression) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: "equation from ".to_string(),
        scalar_count: 1,
    }
}

fn expr_contains_var_ref(expr: &dae::Expression, var_name: &dae::VarName) -> bool {
    let mut refs = std::collections::HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.contains(var_name)
}

#[test]
fn test_normalize_runtime_aliases_rewrites_event_surfaces_to_core_states() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.states.insert(
        dae::VarName::new("v"),
        dae::Variable::new(dae::VarName::new("v")),
    );
    dae.algebraics.insert(
        dae::VarName::new("d"),
        dae::Variable::new(dae::VarName::new("d")),
    );
    dae.parameters.insert(
        dae::VarName::new("r"),
        dae::Variable::new(dae::VarName::new("r")),
    );

    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("v"),
    )));
    dae.f_x.push(eq(sub(var("d"), sub(var("x"), var("r")))));
    dae.f_x.push(eq(sub(
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("v")],
        },
        dae::Expression::If {
            branches: vec![(
                lt(var("d"), int(0)),
                dae::Expression::Unary {
                    op: dae::OpUnary::Minus(Default::default()),
                    rhs: Box::new(int(1)),
                },
            )],
            else_branch: Box::new(dae::Expression::Unary {
                op: dae::OpUnary::Minus(Default::default()),
                rhs: Box::new(int(2)),
            }),
        },
    )));

    let cond = lt(var("d"), int(0));
    dae.relation.push(cond.clone());
    dae.f_c.push(eq(cond));

    let normalized = normalize_runtime_aliases_collect(&mut dae).0;
    assert_eq!(normalized, 1, "expected one alias variable normalized");
    assert!(
        !dae.algebraics.contains_key(&dae::VarName::new("d")),
        "alias variable should be removed from algebraics after rewrite"
    );
    assert_eq!(
        dae.f_x.len(),
        2,
        "alias defining equation should be removed after substitution"
    );

    assert!(
        !dae.relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, &dae::VarName::new("d"))),
        "relation expressions should not reference eliminated alias"
    );
    assert!(
        dae.relation
            .iter()
            .any(|expr| expr_contains_var_ref(expr, &dae::VarName::new("x"))),
        "relation expressions should be rewritten to core-state variables"
    );
    assert!(
        !dae.f_c
            .iter()
            .any(|eq| expr_contains_var_ref(&eq.rhs, &dae::VarName::new("d"))),
        "f_c expressions should not reference eliminated alias"
    );
}
