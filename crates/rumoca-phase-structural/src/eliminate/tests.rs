use super::*;
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

type BuiltinFunction = flat::BuiltinFunction;
type Literal = flat::Literal;

fn sub_op() -> OpBinary {
    OpBinary::Sub(Default::default())
}

fn minus_op() -> OpUnary {
    OpUnary::Minus(Default::default())
}

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}

fn var_ref_idx(name: &str, idx: i64) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![flat::Subscript::Index(idx)],
    }
}

fn lit(v: f64) -> Expression {
    Expression::Literal(Literal::Real(v))
}

fn add_op() -> OpBinary {
    OpBinary::Add(Default::default())
}

fn mul_op() -> OpBinary {
    OpBinary::Mul(Default::default())
}

// ── try_solve_for_unknown ─────────────────────────────────────────

#[test]
fn test_try_solve_sub_lhs() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("z")),
        rhs: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(lit(1.0)),
        }),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(matches!(result.unwrap(), Expression::Binary { .. }));
}

#[test]
fn test_try_solve_sub_rhs() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(var_ref("z")),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(matches!(result.unwrap(), Expression::VarRef { .. }));
}

#[test]
fn test_try_solve_negated() {
    let inner = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("z")),
        rhs: Box::new(lit(5.0)),
    };
    let rhs = Expression::Unary {
        op: minus_op(),
        rhs: Box::new(inner),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(matches!(result.unwrap(), Expression::Literal(Literal::Real(v)) if v == 5.0));
}

#[test]
fn test_try_solve_sub_lhs_with_unity_subscript_alias_matches() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref_idx("z", 1)),
        rhs: Box::new(lit(3.0)),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_some());
    assert!(matches!(result.unwrap(), Expression::Literal(Literal::Real(v)) if v == 3.0));
}

#[test]
fn test_try_solve_sub_lhs_with_non_unity_subscript_fails() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref_idx("z", 2)),
        rhs: Box::new(lit(3.0)),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_none());
}

#[test]
fn test_try_solve_complex_fails() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(Expression::Binary {
            op: mul_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("z")),
        }),
        rhs: Box::new(lit(4.0)),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("z"));
    assert!(result.is_none());
}

#[test]
fn test_try_solve_does_not_match_complex_base_for_field_unknown() {
    let rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("transferFunction.aSum")),
        rhs: Box::new(lit(1.0)),
    };
    let result = try_solve_for_unknown(&rhs, &VarName::new("transferFunction.aSum.re"));
    assert!(result.is_none());
}

#[test]
fn test_var_ref_matches_unknown_allows_complex_base_to_field_alias() {
    assert!(var_ref_matches_unknown(
        &VarName::new("transferFunction.aSum"),
        &[],
        &VarName::new("transferFunction.aSum.re")
    ));
    assert!(var_ref_matches_unknown(
        &VarName::new("transferFunction.aSum"),
        &[],
        &VarName::new("transferFunction.aSum.im")
    ));
}

#[test]
fn test_expr_contains_var_matches_complex_base_alias() {
    let expr = var_ref("transferFunction.aSum");
    assert!(expr_contains_var(
        &expr,
        &VarName::new("transferFunction.aSum.re")
    ));
    assert!(expr_contains_var(
        &expr,
        &VarName::new("transferFunction.aSum.im")
    ));
}

#[test]
fn test_expr_contains_der_of_matches_indexed_subscript_form() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref_idx("x", 2)],
    };
    assert!(expr_contains_der_of(&expr, &VarName::new("x")));
}

#[test]
fn test_expr_contains_der_of_matches_embedded_subscript_mid_path() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref("support[2].phi")],
    };
    assert!(expr_contains_der_of(&expr, &VarName::new("support.phi")));
}

// ── substitute_var ────────────────────────────────────────────────

#[test]
fn test_substitute_var_simple() {
    let expr = Expression::Binary {
        op: mul_op(),
        lhs: Box::new(var_ref("z")),
        rhs: Box::new(lit(2.0)),
    };
    let replacement = Expression::Binary {
        op: add_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(lit(1.0)),
    };
    let result = substitute_var(&expr, &VarName::new("z"), &replacement);
    if let Expression::Binary { lhs, .. } = &result {
        assert!(matches!(lhs.as_ref(), Expression::Binary { .. }));
    } else {
        panic!("expected Binary");
    }
}

#[test]
fn test_substitute_var_keeps_complex_base_when_substituting_field_unknown() {
    let expr = Expression::FieldAccess {
        base: Box::new(var_ref("transferFunction.aSum")),
        field: "im".to_string(),
    };
    let result = substitute_var(&expr, &VarName::new("transferFunction.aSum.re"), &lit(99.0));
    match result {
        Expression::FieldAccess { base, field } => {
            assert_eq!(field, "im");
            match base.as_ref() {
                Expression::VarRef { name, subscripts } => {
                    assert_eq!(name.as_str(), "transferFunction.aSum");
                    assert!(subscripts.is_empty());
                }
                _ => panic!("expected base VarRef to remain unchanged"),
            }
        }
        _ => panic!("expected FieldAccess to remain unchanged"),
    }
}

#[test]
fn test_substitute_var_nested() {
    let expr = Expression::Unary {
        op: minus_op(),
        rhs: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(lit(1.0)),
        }),
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(42.0));
    if let Expression::Unary { rhs, .. } = &result {
        if let Expression::Binary { lhs, .. } = rhs.as_ref() {
            assert!(matches!(lhs.as_ref(), Expression::Literal(Literal::Real(v)) if *v == 42.0));
        } else {
            panic!("expected Binary inside Unary");
        }
    } else {
        panic!("expected Unary");
    }
}

#[test]
fn test_substitute_var_in_if() {
    let expr = Expression::If {
        branches: vec![(Expression::Literal(Literal::Boolean(true)), var_ref("z"))],
        else_branch: Box::new(lit(0.0)),
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(99.0));
    if let Expression::If { branches, .. } = &result {
        assert!(matches!(&branches[0].1, Expression::Literal(Literal::Real(v)) if *v == 99.0));
    } else {
        panic!("expected If");
    }
}

#[test]
fn test_substitute_var_skips_pre_edge_change_arguments() {
    let expr = Expression::Binary {
        op: add_op(),
        lhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("z")],
        }),
        rhs: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Edge,
                args: vec![var_ref("z")],
            }),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Change,
                args: vec![var_ref("z")],
            }),
        }),
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(99.0));
    let Expression::Binary { lhs, rhs, .. } = result else {
        panic!("expected binary expression");
    };
    assert!(
        matches!(
            lhs.as_ref(),
            Expression::BuiltinCall { function: BuiltinFunction::Pre, args }
                if matches!(
                    args.as_slice(),
                    [Expression::VarRef { name, subscripts }]
                        if name.as_str() == "z" && subscripts.is_empty()
                )
        ),
        "pre() argument should remain unchanged"
    );
    let Expression::Binary { lhs, rhs, .. } = rhs.as_ref() else {
        panic!("expected nested binary expression");
    };
    assert!(
        matches!(
            lhs.as_ref(),
            Expression::BuiltinCall { function: BuiltinFunction::Edge, args }
                if matches!(
                    args.as_slice(),
                    [Expression::VarRef { name, subscripts }]
                        if name.as_str() == "z" && subscripts.is_empty()
                )
        ),
        "edge() argument should remain unchanged"
    );
    assert!(
        matches!(
            rhs.as_ref(),
            Expression::BuiltinCall { function: BuiltinFunction::Change, args }
                if matches!(
                    args.as_slice(),
                    [Expression::VarRef { name, subscripts }]
                        if name.as_str() == "z" && subscripts.is_empty()
                )
        ),
        "change() argument should remain unchanged"
    );
}

#[test]
fn test_substitute_var_rewrites_regular_builtin_arguments() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var_ref("z")],
    };
    let result = substitute_var(&expr, &VarName::new("z"), &lit(7.0));
    assert!(
        matches!(
            result,
            Expression::BuiltinCall { function: BuiltinFunction::Sin, args }
                if matches!(
                    args.as_slice(),
                    [Expression::Literal(Literal::Real(v))] if *v == 7.0
                )
        ),
        "regular builtins should still be substituted"
    );
}

// ── expr_contains_var ─────────────────────────────────────────────

#[test]
fn test_expr_contains_var_true() {
    let expr = Expression::Binary {
        op: add_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(var_ref("z")),
    };
    assert!(expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_false() {
    let expr = Expression::Binary {
        op: add_op(),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(lit(1.0)),
    };
    assert!(!expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_in_builtin() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var_ref("z")],
    };
    assert!(expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_accepts_embedded_unity_subscript_alias() {
    let expr = var_ref("z[1]");
    assert!(expr_contains_var(&expr, &VarName::new("z")));
}

#[test]
fn test_expr_contains_var_rejects_embedded_non_unity_subscript() {
    let expr = var_ref("z[2]");
    assert!(!expr_contains_var(&expr, &VarName::new("z")));
}

// ── eliminate_trivial ─────────────────────────────────────────────

fn build_test_dae_3eq() -> Dae {
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal(Literal::Real(1.0)));
    dae.states.insert(VarName::new("x"), var_x);

    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    // ODE: 0 = der(x) - z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // Algebraic: 0 = z - x
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("x")),
        },
        span: Span::DUMMY,
        origin: "alg".to_string(),
        scalar_count: 1,
    });

    dae
}

#[test]
fn test_eliminate_trivial_simple() {
    let mut dae = build_test_dae_3eq();
    let result = eliminate_trivial(&mut dae);

    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert_eq!(dae.f_x.len(), 1);
    assert!(!dae.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_chain() {
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal(Literal::Real(1.0)));
    dae.states.insert(VarName::new("x"), var_x);
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

    // ODE: 0 = der(x) - b
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(var_ref("b")),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // 0 = a - x  (a = x)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("x")),
        },
        span: Span::DUMMY,
        origin: "alg1".to_string(),
        scalar_count: 1,
    });

    // 0 = b - a  (b = a)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("b")),
            rhs: Box::new(var_ref("a")),
        },
        span: Span::DUMMY,
        origin: "alg2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);

    assert_eq!(result.n_eliminated, 2);
    assert_eq!(dae.f_x.len(), 1);
    assert!(dae.algebraics.is_empty());
}

#[test]
fn test_eliminate_trivial_alias_pair_two_unknowns() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("b")),
        },
        span: Span::DUMMY,
        origin: "alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(dae.f_x.len(), 0);
    assert_eq!(dae.algebraics.len(), 1);
    assert!(
        dae.algebraics.contains_key(&VarName::new("a"))
            || dae.algebraics.contains_key(&VarName::new("b"))
    );
}

#[test]
fn test_eliminate_trivial_alias_pair_prefers_output_elimination() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "output_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "y");
    assert!(!dae.outputs.contains_key(&VarName::new("y")));
    assert!(dae.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_keeps_fixed_alias_unknown() {
    let mut dae = Dae::new();
    let mut fixed = dae::Variable::new(VarName::new("y"));
    fixed.fixed = Some(true);
    fixed.start = Some(lit(0.0));
    dae.algebraics.insert(VarName::new("y"), fixed);
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "fixed_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert!(dae.algebraics.contains_key(&VarName::new("y")));
    assert!(!dae.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_keeps_runtime_partition_defined_output() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "output_alias".to_string(),
        scalar_count: 1,
    });

    dae.f_z.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "runtime_partition".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert!(
        result.substitutions.is_empty(),
        "runtime partition dependencies should block trivial elimination"
    );
    assert!(dae.outputs.contains_key(&VarName::new("y")));
    assert!(dae.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_blt_keeps_fixed_alias_unknown_against_state() {
    let mut dae = Dae::new();

    let mut state = dae::Variable::new(VarName::new("x"));
    state.start = Some(lit(0.0));
    dae.states.insert(VarName::new("x"), state);

    let mut fixed = dae::Variable::new(VarName::new("y"));
    fixed.fixed = Some(true);
    fixed.start = Some(lit(0.0));
    dae.algebraics.insert(VarName::new("y"), fixed);

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("x")),
        },
        span: Span::DUMMY,
        origin: "fixed_alias_to_state".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "state_dynamics".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);

    assert!(
        dae.algebraics.contains_key(&VarName::new("y")),
        "fixed alias unknown must not be eliminated by BLT"
    );
    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "y"),
        "BLT should not create substitution for fixed unknown y"
    );
}

#[test]
fn test_eliminate_trivial_direct_assignment_with_multiple_live_unknowns() {
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(lit(0.0));
    dae.states.insert(VarName::new("x"), var_x);
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    // Keep `a` coupled to dynamics so it is not trivially removable.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(var_ref("a")),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // `z` can still be eliminated because this row is a direct assignment.
    // Another live unknown (`a`) remains in the row expression.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(Expression::Binary {
                op: mul_op(),
                lhs: Box::new(var_ref("a")),
                rhs: Box::new(var_ref("a")),
            }),
        },
        span: Span::DUMMY,
        origin: "leaf".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert_eq!(dae.f_x.len(), 1);
    assert!(!dae.algebraics.contains_key(&VarName::new("z")));
    assert!(dae.algebraics.contains_key(&VarName::new("a")));
}

#[test]
fn test_eliminate_trivial_allows_output_if_assignment() {
    let mut dae = Dae::new();

    dae.algebraics
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    // 0 = x - 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "x_assign".to_string(),
        scalar_count: 1,
    });

    // 0 = y - if x > 0 then x else 0
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt(Default::default()),
                        lhs: Box::new(var_ref("x")),
                        rhs: Box::new(lit(0.0)),
                    },
                    var_ref("x"),
                )],
                else_branch: Box::new(lit(0.0)),
            }),
        },
        span: Span::DUMMY,
        origin: "y_if_assign".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "y"),
        "output assignment with if-expression should be eliminable"
    );
    assert!(
        !dae.outputs.contains_key(&VarName::new("y")),
        "output y should be removed from live unknowns after elimination"
    );
}

#[test]
fn test_eliminate_trivial_handles_singleton_array_alias_equation() {
    let mut dae = Dae::new();
    let mut aux = dae::Variable::new(VarName::new("aux"));
    aux.dims = vec![1];
    dae.algebraics.insert(VarName::new("aux"), aux);
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    let mut p = dae::Variable::new(VarName::new("p"));
    p.start = Some(lit(2.0));
    dae.parameters.insert(VarName::new("p"), p);

    // 0 = aux[1] - p
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("aux[1]")),
            rhs: Box::new(var_ref("p")),
        },
        span: Span::DUMMY,
        origin: "aux_alias".to_string(),
        scalar_count: 1,
    });
    // 0 = z - aux
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("aux")),
        },
        span: Span::DUMMY,
        origin: "z_aux".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 2);
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "aux"),
        "expected canonical aux substitution in elimination result"
    );
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "z"),
        "expected z substitution in elimination result"
    );
    assert!(
        dae.f_x.is_empty(),
        "all trivial equations should be eliminated"
    );
    assert!(
        !dae.algebraics.contains_key(&VarName::new("aux")),
        "singleton array alias variable should be removed from unknowns"
    );
    assert!(
        !dae.algebraics.contains_key(&VarName::new("z")),
        "dependent alias variable should be removed from unknowns"
    );
}

#[test]
fn test_eliminate_trivial_derstate_kept() {
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal(Literal::Real(1.0)));
    dae.states.insert(VarName::new("x"), var_x);

    // ODE: 0 = der(x) - 1.0
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.f_x.len(), 1);
}

#[test]
fn test_eliminate_trivial_keeps_array_alias_equations() {
    let mut dae = Dae::new();

    let mut arr = dae::Variable::new(VarName::new("arr"));
    arr.dims = vec![3];
    dae.algebraics.insert(VarName::new("arr"), arr);

    let mut pin = dae::Variable::new(VarName::new("plug.pin.i"));
    pin.dims = vec![3];
    dae.algebraics.insert(VarName::new("plug.pin.i"), pin);

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("arr")),
            rhs: Box::new(var_ref("plug.pin.i")),
        },
        span: Span::DUMMY,
        origin: "array_alias".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.f_x.len(), 1);
    assert!(dae.algebraics.contains_key(&VarName::new("arr")));
    assert!(dae.algebraics.contains_key(&VarName::new("plug.pin.i")));
}

#[test]
fn test_eliminate_trivial_preserves_indexed_flow_reference() {
    let mut dae = Dae::new();

    dae.algebraics.insert(
        VarName::new("sineVoltage.sineVoltage[1].p.i"),
        dae::Variable::new(VarName::new("sineVoltage.sineVoltage[1].p.i")),
    );

    let mut array_alias = dae::Variable::new(VarName::new("sineVoltage.i"));
    array_alias.dims = vec![3];
    dae.algebraics
        .insert(VarName::new("sineVoltage.i"), array_alias);

    let mut pin_alias = dae::Variable::new(VarName::new("sineVoltage.plug_p.pin.i"));
    pin_alias.dims = vec![3];
    dae.algebraics
        .insert(VarName::new("sineVoltage.plug_p.pin.i"), pin_alias);

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("sineVoltage.i")),
            rhs: Box::new(var_ref("sineVoltage.plug_p.pin.i")),
        },
        span: Span::DUMMY,
        origin: "array_alias".to_string(),
        scalar_count: 3,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("sineVoltage.sineVoltage[1].p.i")),
            rhs: Box::new(var_ref("sineVoltage.plug_p.pin[1].i")),
        },
        span: Span::DUMMY,
        origin: "flow".to_string(),
        scalar_count: 1,
    });

    let _ = eliminate_trivial(&mut dae);
    let flow_eq = dae
        .f_x
        .iter()
        .find(|eq| eq.origin == "flow")
        .expect("flow equation must be preserved");
    let Expression::Binary { rhs, .. } = &flow_eq.rhs else {
        panic!("flow equation should remain binary");
    };
    let Expression::VarRef {
        name: rhs_name,
        subscripts,
    } = rhs.as_ref()
    else {
        panic!("flow rhs should remain a VarRef");
    };
    assert_eq!(rhs_name.as_str(), "sineVoltage.plug_p.pin[1].i");
    assert!(subscripts.is_empty());
}

#[test]
fn test_eliminate_trivial_skips_substitution_to_unsliced_multiscalar_solution() {
    let mut dae = Dae::new();

    dae.algebraics.insert(
        VarName::new("source.pin[1].i"),
        dae::Variable::new(VarName::new("source.pin[1].i")),
    );
    dae.algebraics.insert(
        VarName::new("branch.pin.i"),
        dae::Variable::new(VarName::new("branch.pin.i")),
    );
    let mut source_pin = dae::Variable::new(VarName::new("source.pin.i"));
    source_pin.dims = vec![3];
    dae.algebraics
        .insert(VarName::new("source.pin.i"), source_pin);

    // Scalar-to-vector alias row that must not be used for elimination.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("source.pin[1].i")),
            rhs: Box::new(var_ref("source.pin.i")),
        },
        span: Span::DUMMY,
        origin: "scalar_vector_alias".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("branch.pin.i")),
            rhs: Box::new(var_ref("source.pin[1].i")),
        },
        span: Span::DUMMY,
        origin: "flow".to_string(),
        scalar_count: 1,
    });

    let _ = eliminate_trivial(&mut dae);
    let flow_eq = dae
        .f_x
        .iter()
        .find(|eq| eq.origin == "flow")
        .expect("flow equation must be preserved");
    let Expression::Binary { rhs, .. } = &flow_eq.rhs else {
        panic!("flow equation should remain binary");
    };
    let Expression::VarRef {
        name: rhs_name,
        subscripts,
    } = rhs.as_ref()
    else {
        panic!("flow rhs should remain a VarRef");
    };
    assert_eq!(rhs_name.as_str(), "source.pin[1].i");
    assert!(subscripts.is_empty());
}

#[test]
fn test_eliminate_structurally_singular_boundary_resolution() {
    // 2 equations both referencing only `a`, `b` unmatched.
    // Phase A resolves a=1.0, then eq2 becomes zero-unknown and is removed.
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(lit(2.0)),
        },
        span: Span::DUMMY,
        origin: "eq2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    // Phase A: eq1 solves a=1.0 (1 unknown), eq2 becomes 0-unknown → removed.
    assert_eq!(result.n_eliminated, 2);
    assert_eq!(dae.f_x.len(), 0);
}

#[test]
fn test_eliminate_bare_varref() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: var_ref("z"),
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert!(dae.f_x.is_empty());
}

// ── Boundary resolution specific tests ──────────────────────────

#[test]
fn test_boundary_zero_unknown_removed() {
    // dae::Equation with no unknowns (parameter-only) should be removed.
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    // eq1: 0 = z - 1.0  (1 unknown, solvable)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    // eq2: 0 = 3.0 - 3.0  (0 unknowns, redundant)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(lit(3.0)),
            rhs: Box::new(lit(3.0)),
        },
        span: Span::DUMMY,
        origin: "eq2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    // Both removed: eq2 has 0 unknowns, eq1 has 1 unknown (z=1.0).
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.f_x.is_empty());
}

#[test]
fn test_boundary_zero_unknown_alias_equation_becomes_substitution() {
    let mut dae = Dae::new();

    let mut state = dae::Variable::new(VarName::new("x"));
    state.start = Some(lit(0.0));
    dae.states.insert(VarName::new("x"), state);
    dae.algebraics
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.inputs.insert(
        VarName::new("alias_local"),
        dae::Variable::new(VarName::new("alias_local")),
    );
    dae.discrete_valued.insert(
        VarName::new("local"),
        dae::Variable::new(VarName::new("local")),
    );

    // 0 = der(x) - y
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(var_ref("y")),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // 0 = y - if alias_local then 1 else 0
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(var_ref("alias_local"), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
            }),
        },
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });

    // 0 = alias_local - local (no continuous unknowns)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("alias_local")),
            rhs: Box::new(var_ref("local")),
        },
        span: Span::DUMMY,
        origin: "discrete_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    let alias_sub = result
        .substitutions
        .iter()
        .find(|sub| sub.var_name.as_str() == "alias_local")
        .expect("discrete alias equation should be converted to substitution");

    assert!(
        matches!(
            alias_sub.expr,
            Expression::VarRef { ref name, ref subscripts }
                if name.as_str() == "local" && subscripts.is_empty()
        ),
        "alias_local should resolve to local, got {:?}",
        alias_sub.expr
    );
    assert!(
        dae.f_x
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &VarName::new("alias_local"))),
        "remaining equations should no longer reference alias_local"
    );
}

#[test]
fn test_boundary_cascade_resolution() {
    // a=1 (1 unknown), b=a (2 unknowns initially, 1 after a resolved).
    // Phase A should cascade: resolve a first, then b becomes 1-unknown.
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

    // eq1: 0 = a - 1.0  (1 unknown)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    // eq2: 0 = b - a  (2 unknowns initially)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("b")),
            rhs: Box::new(var_ref("a")),
        },
        span: Span::DUMMY,
        origin: "eq2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.f_x.is_empty());
    assert!(dae.algebraics.is_empty());
}

#[test]
fn test_boundary_skips_ode_equations() {
    // ODE equation should never be eliminated by boundary resolution.
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal(Literal::Real(0.0)));
    dae.states.insert(VarName::new("x"), var_x);

    // ODE: 0 = der(x) - 1.0
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.f_x.len(), 1);
}

#[test]
fn test_boundary_eliminates_derivative_dependent_output_alias() {
    // Keep true ODE equation and eliminate derivative-dependent output alias.
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal(Literal::Real(0.0)));
    dae.states.insert(VarName::new("x"), var_x);
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    // ODE: 0 = der(x) - 1.0
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(lit(1.0)),
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // Alias output: 0 = y - der(x)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
        },
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "y");
    assert_eq!(dae.f_x.len(), 1);
    assert!(!dae.outputs.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_eliminates_control_flow_solution_equation() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(Expression::Literal(Literal::Boolean(true)), lit(1.0))],
                else_branch: Box::new(lit(2.0)),
            }),
        },
        span: Span::DUMMY,
        origin: "if_expr".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.f_x.len(), 0);
    assert!(!dae.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_eliminates_single_unknown_connection_after_substitution() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));

    // Source-like equation: y = if time < 0.2 then 1 else 2.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(lit(0.2)),
                    },
                    lit(1.0),
                )],
                else_branch: Box::new(lit(2.0)),
            }),
        },
        span: Span::DUMMY,
        origin: "source".to_string(),
        scalar_count: 1,
    });

    // Connection equation reduced to one unknown after y substitution.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("u")),
        },
        span: Span::DUMMY,
        origin: "connection equation: y = u".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.f_x.is_empty());
    assert!(!dae.outputs.contains_key(&VarName::new("y")));
    assert!(!dae.algebraics.contains_key(&VarName::new("u")));
    assert!(
        result
            .substitutions
            .iter()
            .any(|s| s.var_name.as_str() == "y"),
        "expected y substitution"
    );
    assert!(
        result
            .substitutions
            .iter()
            .any(|s| s.var_name.as_str() == "u"),
        "expected u substitution from reduced connection equation"
    );
}

#[test]
fn test_boundary_keeps_connection_eq_touching_runtime_discrete_target() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.inputs
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));

    // Runtime-discrete partition assignment target (f_m/f_z lhs) marks `y`
    // as a runtime-discrete target that must not lose alias edges.
    dae.f_m.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: Expression::Literal(Literal::Boolean(false)),
        span: Span::DUMMY,
        origin: "runtime discrete assignment".to_string(),
        scalar_count: 1,
    });

    // Connection equation that would normally be single-live-unknown.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("u")),
        },
        span: Span::DUMMY,
        origin: "connection equation: y = u".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.f_x.len(), 1);
    assert!(dae.outputs.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_keeps_state_only_algebraic_constraint() {
    let mut dae = Dae::new();

    let mut x = dae::Variable::new(VarName::new("x"));
    x.start = Some(lit(0.0));
    dae.states.insert(VarName::new("x"), x);
    let mut y = dae::Variable::new(VarName::new("y"));
    y.start = Some(lit(0.0));
    dae.states.insert(VarName::new("y"), y);

    // ODE rows.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            }),
            rhs: Box::new(lit(0.0)),
        },
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("y")],
            }),
            rhs: Box::new(lit(0.0)),
        },
        span: Span::DUMMY,
        origin: "ode_y".to_string(),
        scalar_count: 1,
    });
    // Algebraic state coupling: x = y.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("y")),
        },
        span: Span::DUMMY,
        origin: "state_coupling".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert!(
        dae.f_x.iter().any(|eq| eq.origin == "state_coupling"),
        "state-only algebraic constraint must be preserved"
    );
    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "x" && sub.var_name.as_str() != "y"),
        "state variables should not be eliminated by boundary stage"
    );
}

#[test]
fn test_boundary_preserves_indexed_array_connection_constraints() {
    let mut dae = Dae::new();

    let mut add_u = dae::Variable::new(VarName::new("add.u"));
    add_u.dims = vec![2];
    dae.algebraics.insert(VarName::new("add.u"), add_u);

    let mut product_u = dae::Variable::new(VarName::new("product.u"));
    product_u.dims = vec![2];
    dae.algebraics.insert(VarName::new("product.u"), product_u);

    dae.outputs.insert(
        VarName::new("integerStep.y"),
        dae::Variable::new(VarName::new("integerStep.y")),
    );

    // Source-like assignment for integerStep.y.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("integerStep.y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(lit(2.0)),
                    },
                    lit(0.0),
                )],
                else_branch: Box::new(lit(3.0)),
            }),
        },
        span: Span::DUMMY,
        origin: "source".to_string(),
        scalar_count: 1,
    });

    // Connection equations from RealNetwork-style indexed array inputs.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("integerStep.y")),
            rhs: Box::new(var_ref("add.u[2]")),
        },
        span: Span::DUMMY,
        origin: "connection equation: integerStep.y = add.u[2]".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("integerStep.y")),
            rhs: Box::new(var_ref("product.u[1]")),
        },
        span: Span::DUMMY,
        origin: "connection equation: integerStep.y = product.u[1]".to_string(),
        scalar_count: 1,
    });

    let _ = eliminate_trivial(&mut dae);

    let mut refs = std::collections::HashSet::new();
    for eq in &dae.f_x {
        eq.rhs.collect_var_refs(&mut refs);
    }
    assert!(
        refs.contains(&VarName::new("add.u[2]")),
        "indexed array constraint add.u[2] must remain live after elimination"
    );
    assert!(
        refs.contains(&VarName::new("product.u[1]")),
        "indexed array constraint product.u[1] must remain live after elimination"
    );
}
