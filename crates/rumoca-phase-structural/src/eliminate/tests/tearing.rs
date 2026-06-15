use super::*;

#[test]
fn test_eliminate_trivial_tears_symbolic_algebraic_loop() {
    let mut dae = Dae::new();
    for name in ["a", "b", "c"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), dae::Variable::new(VarName::new(name)));
    }
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("b")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "loop a = b".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("b")),
            rhs: Box::new(var_ref("c")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "loop b = c".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("c")),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Sin,
                args: vec![var_ref("a")],
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "loop residual c = sin(a)".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 2);
    assert_eq!(result.substitutions.len(), 2);
    assert_eq!(dae.variables.algebraics.len(), 1);
    assert_eq!(dae.continuous.equations.len(), 1);
    for substitution in &result.substitutions {
        assert!(
            !dae.variables
                .algebraics
                .contains_key(&substitution.var_name)
        );
        assert!(!expr_contains_var(
            &dae.continuous.equations[0].rhs,
            &substitution.var_name
        ));
    }
    let remaining_name = dae.variables.algebraics.keys().next().unwrap();
    assert!(expr_contains_var(
        &dae.continuous.equations[0].rhs,
        remaining_name
    ));
}
