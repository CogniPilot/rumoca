use super::*;

#[test]
fn test_boundary_keeps_state_only_algebraic_constraint() {
    let mut dae = Dae::new();

    let mut x = dae::Variable::new(VarName::new("x"));
    x.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), x);
    let mut y = dae::Variable::new(VarName::new("y"));
    y.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("y"), y);

    // ODE rows.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("y")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode_y".to_string(),
        scalar_count: 1,
    });
    // Algebraic state coupling: x = y.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("y")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "state_coupling".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin == "state_coupling"),
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
    dae.variables
        .algebraics
        .insert(VarName::new("add.u"), add_u);

    let mut product_u = dae::Variable::new(VarName::new("product.u"));
    product_u.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("product.u"), product_u);

    dae.variables.outputs.insert(
        VarName::new("integerStep.y"),
        dae::Variable::new(VarName::new("integerStep.y")),
    );

    // Source-like assignment for integerStep.y.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("integerStep.y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt,
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(lit(2.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    lit(0.0),
                )],
                else_branch: Box::new(lit(3.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "source".to_string(),
        scalar_count: 1,
    });

    // Connection equations from RealNetwork-style indexed array inputs.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("integerStep.y")),
            rhs: Box::new(var_ref("add.u[2]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: integerStep.y = add.u[2]".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("integerStep.y")),
            rhs: Box::new(var_ref("product.u[1]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: integerStep.y = product.u[1]".to_string(),
        scalar_count: 1,
    });

    let _ = eliminate_trivial(&mut dae);

    let mut refs = std::collections::HashSet::new();
    for eq in &dae.continuous.equations {
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

#[test]
fn test_boundary_keeps_internal_discrete_connection_chain_for_runtime_alias_paths() {
    let mut dae = Dae::new();
    for name in [
        "src.y",
        "adder.b",
        "adder.xor.x[1]",
        "adder.xor.g1.x[1]",
        "adder.xor.g1.auxiliary[1]",
    ] {
        dae.variables
            .discrete_valued
            .insert(VarName::new(name), dae::Variable::new(VarName::new(name)));
    }

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("src.y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt,
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(lit(0.2)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    lit(3.0),
                )],
                else_branch: Box::new(lit(4.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "digital source".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("src.y")),
            rhs: Box::new(var_ref("adder.b")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: src.y = adder.b".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("adder.b")),
            rhs: Box::new(var_ref("adder.xor.x[1]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: adder.b = adder.xor.x[1]".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("adder.xor.x[1]")),
            rhs: Box::new(var_ref("adder.xor.g1.x[1]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: adder.xor.x[1] = adder.xor.g1.x[1]".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("adder.xor.g1.auxiliary[1]")),
            rhs: Box::new(var_ref("adder.xor.g1.x[1]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "gate auxiliary".to_string(),
        scalar_count: 1,
    });

    let _ = eliminate_trivial(&mut dae);

    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin == "connection equation: adder.xor.x[1] = adder.xor.g1.x[1]"),
        "internal discrete connector aliases must remain live after boundary elimination"
    );
}
