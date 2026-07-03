use super::*;

#[test]
fn test_boundary_keeps_state_only_algebraic_constraint() {
    let mut dae = Dae::new();

    let mut x = test_dae_variable("x");
    x.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), x);
    let mut y = test_dae_variable("y");
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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
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

    let mut add_u = component_var("add.u");
    add_u.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("add.u"), add_u);
    dae.variables
        .algebraics
        .insert(VarName::new("add.u[2]"), component_var("add.u[2]"));

    let mut product_u = component_var("product.u");
    product_u.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("product.u"), product_u);
    dae.variables
        .algebraics
        .insert(VarName::new("product.u[1]"), component_var("product.u[1]"));

    dae.variables.outputs.insert(
        VarName::new("integerStep.y"),
        component_var("integerStep.y"),
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

    eliminate_trivial(&mut dae).expect("structural elimination should succeed");

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
fn test_boundary_eliminates_indexed_scalar_algebraic_connection_alias() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("plug.pin[1].v.re"),
        component_var("plug.pin[1].v.re"),
    );
    dae.variables.algebraics.insert(
        VarName::new("adapter.pin[1].v.re"),
        component_var("adapter.pin[1].v.re"),
    );

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("plug.pin[1].v.re")),
            rhs: Box::new(var_ref("adapter.pin[1].v.re")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: plug.pin[1].v.re = adapter.pin[1].v.re".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "plug.pin[1].v.re"
                || sub.var_name.as_str() == "adapter.pin[1].v.re"),
        "indexed scalar algebraic connection alias should produce a substitution"
    );
}

#[test]
fn test_boundary_eliminates_nested_index_field_connection_alias() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("plug.pin[1].v.re"),
        component_var("plug.pin[1].v.re"),
    );
    dae.variables.algebraics.insert(
        VarName::new("adapter.pin[1].v.re"),
        component_var("adapter.pin[1].v.re"),
    );

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(field_access(
                field_access(index_access(field_access(var_ref("plug"), "pin"), 1), "v"),
                "re",
            )),
            rhs: Box::new(field_access(
                field_access(
                    index_access(field_access(var_ref("adapter"), "pin"), 1),
                    "v",
                ),
                "re",
            )),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: plug.pin[1].v.re = adapter.pin[1].v.re".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "plug.pin[1].v.re"
                || sub.var_name.as_str() == "adapter.pin[1].v.re"),
        "nested index/field connection alias should produce a scalar substitution"
    );
}

#[test]
fn test_orphan_drop_does_not_keep_scalarized_unknown_by_base_alias_only() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("resistor.plug_p.pin[2].v.im"),
        test_dae_variable("resistor.plug_p.pin[2].v.im"),
    );
    dae.variables.parameters.insert(
        VarName::new("resistor.plug_p.pin"),
        test_dae_variable("resistor.plug_p.pin"),
    );
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: var_ref("resistor.plug_p.pin"),
        span: Span::DUMMY,
        origin: "metadata-only aggregate reference".to_string(),
        scalar_count: 1,
    });

    drop_unreferenced_continuous_unknowns(&mut dae);

    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("resistor.plug_p.pin[2].v.im")),
        "scalarized algebraic unknowns require an exact live reference"
    );
}

#[test]
fn test_orphan_drop_keeps_exact_scalarized_unknown_reference() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("resistor.plug_p.pin[2].v.im"),
        test_dae_variable("resistor.plug_p.pin[2].v.im"),
    );
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: var_ref("resistor.plug_p.pin[2].v.im"),
        span: Span::DUMMY,
        origin: "exact scalarized reference".to_string(),
        scalar_count: 1,
    });

    drop_unreferenced_continuous_unknowns(&mut dae);

    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("resistor.plug_p.pin[2].v.im")),
        "exact scalarized algebraic references must keep the unknown live"
    );
}

#[test]
fn test_boundary_eliminates_single_live_indexed_flow_alias() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("star.plugToPin[2].pin_p.i.im"),
        test_dae_variable("star.plugToPin[2].pin_p.i.im"),
    );
    dae.variables.parameters.insert(
        VarName::new("star.pin_p[2].i.im"),
        test_dae_variable("star.pin_p[2].i.im"),
    );
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref("star.plugToPin[2].pin_p.i.im")),
            rhs: Box::new(Expression::Unary {
                op: OpUnary::Minus,
                rhs: Box::new(var_ref("star.pin_p[2].i.im")),
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "flow sum equation: star.plugToPin[2].pin_p.i.im + -star.pin_p[2].i.im = 0"
            .to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 1);
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "star.plugToPin[2].pin_p.i.im"),
        "single-live indexed flow aliases should be eliminated"
    );
}

#[test]
fn test_boundary_eliminates_pairwise_indexed_flow_alias() {
    let mut dae = Dae::new();

    for name in [
        "star.plugToPin[2].pin_p.i.im",
        "star.pin_p[2].i.im",
        "star.pin_n.i.im",
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref("star.plugToPin[2].pin_p.i.im")),
            rhs: Box::new(Expression::Unary {
                op: OpUnary::Minus,
                rhs: Box::new(var_ref("star.pin_p[2].i.im")),
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "flow sum equation: star.plugToPin[2].pin_p.i.im + -star.pin_p[2].i.im = 0"
            .to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref("star.pin_p[2].i.im")),
            rhs: Box::new(var_ref("star.pin_n.i.im")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "flow sum equation: star.pin_p[2].i.im + star.pin_n.i.im = 0".to_string(),
        scalar_count: 1,
    });

    let all_unknowns = collect_boundary_unknowns(&dae).expect("boundary unknown collection works");
    let unknown_index =
        BoundaryUnknownIndex::build(&dae, &all_unknowns).expect("boundary index builds");
    let live = find_live_scalar_unknowns(
        &dae.continuous.equations[0].rhs,
        &unknown_index,
        &std::collections::HashSet::new(),
    )
    .expect("live unknown scan works");
    assert_eq!(
        live,
        vec![
            VarName::new("star.plugToPin[2].pin_p.i.im"),
            VarName::new("star.pin_p[2].i.im"),
        ],
        "pairwise flow alias must expose only its two scalar current unknowns"
    );

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "star.plugToPin[2].pin_p.i.im"
                || sub.var_name.as_str() == "star.pin_p[2].i.im"),
        "pairwise indexed flow aliases should be eliminated before KCL rows"
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
            .insert(VarName::new(name), test_dae_variable(name));
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

    eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin == "connection equation: adder.xor.x[1] = adder.xor.g1.x[1]"),
        "internal discrete connector aliases must remain live after boundary elimination"
    );
}

fn index_access(base: Expression, idx: i64) -> Expression {
    Expression::Index {
        base: Box::new(base),
        subscripts: vec![rumoca_core::Subscript::generated_index(idx, test_span())],
        span: test_span(),
    }
}

fn field_access(base: Expression, field: &str) -> Expression {
    Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span: test_span(),
    }
}
