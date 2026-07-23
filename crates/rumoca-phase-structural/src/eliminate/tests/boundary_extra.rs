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
    dae.variables.inputs.insert(
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
fn test_orphan_drop_keeps_exact_scalarized_lhs_owner() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("resistor.plug_p.pin[2].v.im"),
        test_dae_variable("resistor.plug_p.pin[2].v.im"),
    );
    dae.continuous
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            VarName::new("resistor.plug_p.pin[2].v.im"),
            lit(0.0),
            Span::DUMMY,
            "exact scalarized lhs",
            1,
        ));

    drop_unreferenced_continuous_unknowns(&mut dae);

    let sorted = crate::sort_dae(&dae)
        .expect("the retained explicit scalarized lhs must remain structurally matchable");
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("resistor.plug_p.pin[2].v.im")),
        "an exact scalarized lhs must keep its owning unknown live"
    );
    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(
        sorted.matching.len(),
        1,
        "the retained equation must match its one exact scalarized unknown"
    );
}

#[test]
fn test_orphan_drop_keeps_exact_scalarized_slice_lhs_owners() {
    let mut dae = Dae::new();
    let span = test_span();

    let mut aggregate_metadata = component_var("leg_force_w");
    aggregate_metadata.dims = vec![3, 2];
    dae.variables
        .inputs
        .insert(VarName::new("leg_force_w"), aggregate_metadata);
    for row in 1..=3 {
        let name = format!("leg_force_w[{row},1]");
        dae.variables
            .algebraics
            .insert(VarName::new(&name), component_var(&name));
    }
    dae.variables.algebraics.insert(
        VarName::new("leg_force_w[1,2]"),
        component_var("leg_force_w[1,2]"),
    );

    let lhs = Reference::with_component_reference(
        "leg_force_w",
        rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "leg_force_w".to_string(),
                span,
                subs: vec![
                    rumoca_core::Subscript::Colon { span },
                    rumoca_core::Subscript::Index { value: 1, span },
                ],
            }],
            def_id: None,
        },
    );
    dae.continuous
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            lhs,
            array(vec![lit(0.0), lit(0.0), lit(0.0)]),
            span,
            "three-row slice lhs",
            3,
        ));

    drop_unreferenced_continuous_unknowns(&mut dae);

    for row in 1..=3 {
        let name = VarName::new(format!("leg_force_w[{row},1]"));
        assert!(
            dae.variables.algebraics.contains_key(&name),
            "slice lhs must keep exact owner `{}` live",
            name.as_str()
        );
    }
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("leg_force_w[1,2]")),
        "slice lhs must not keep an unrelated scalar leaf by base alias"
    );
    let mut mismatched = dae.clone();
    mismatched.continuous.equations[0].scalar_count = 2;
    drop_unreferenced_continuous_unknowns(&mut mismatched);
    assert!(
        mismatched.variables.algebraics.is_empty(),
        "a slice whose DAE shape disagrees with scalar_count must fail closed"
    );
    let resolver = crate::incidence::ScalarUnknownResolver::from_entries(
        (1..=3).map(|row| (format!("leg_force_w[{row},1]"), row - 1)),
    );
    let mut lhs_columns = std::collections::HashSet::new();
    crate::incidence::collect_equation_lhs_unknown(
        dae.continuous.equations[0].lhs.as_ref(),
        &resolver,
        &mut lhs_columns,
    );
    assert_eq!(
        lhs_columns.len(),
        3,
        "the retained slice lhs must expose all three exact owners to structural incidence"
    );
}

#[test]
fn test_orphan_drop_keeps_structured_fixed_singleton_lhs_owner() {
    let mut dae = Dae::new();
    let span = test_span();

    let mut aggregate_metadata = component_var("fixed_target");
    aggregate_metadata.dims = vec![2, 2];
    dae.variables
        .inputs
        .insert(VarName::new("fixed_target"), aggregate_metadata);
    for name in ["fixed_target[1,1]", "fixed_target[2,1]"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), component_var(name));
    }

    let lhs = Reference::with_component_reference(
        "fixed_target",
        rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "fixed_target".to_string(),
                span,
                subs: vec![
                    rumoca_core::Subscript::Index { value: 1, span },
                    rumoca_core::Subscript::Index { value: 1, span },
                ],
            }],
            def_id: None,
        },
    );
    dae.continuous
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            lhs,
            lit(0.0),
            span,
            "one exact structured lhs owner",
            1,
        ));

    drop_unreferenced_continuous_unknowns(&mut dae);

    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("fixed_target[1,1]")),
        "a structured fixed lhs with scalar_count=1 must keep its exact leaf owner"
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("fixed_target[2,1]")),
        "a structured fixed lhs must not keep an unrelated leaf"
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
        result.substitutions.iter().any(|sub| sub.var_name.as_str()
            == "star.plugToPin[2].pin_p.i.im"
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

fn boundary_alias_fixture() -> Dae {
    let mut dae = Dae::new();
    for name in ["alias", "preexisting_orphan"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.variables.parameters.insert(
        VarName::new("external_source"),
        test_dae_variable("external_source"),
    );
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("external_source")),
            rhs: Box::new(var_ref("alias")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "alias definition".to_string(),
        scalar_count: 1,
    });
    dae
}

fn runtime_equation(origin: &str, rhs: Expression) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: origin.to_string(),
        scalar_count: 1,
    }
}

fn builtin(function: BuiltinFunction, arg: Expression) -> Expression {
    Expression::BuiltinCall {
        function,
        args: vec![arg],
        span: Span::DUMMY,
    }
}

fn event_message(action: &dae::DaeEventAction) -> &Expression {
    match &action.kind {
        dae::DaeEventActionKind::Assert { message }
        | dae::DaeEventActionKind::Terminate { message } => message,
    }
}

#[test]
fn boundary_fixpoint_rewrites_every_plain_dae_surface_before_retiring_target() {
    let mut dae = boundary_alias_fixture();
    dae.initialization
        .equations
        .push(runtime_equation("initial", var_ref("alias")));
    dae.events.synthetic_root_conditions.push(var_ref("alias"));
    dae.clocks.constructor_exprs.push(var_ref("alias"));
    dae.clocks.triggered_conditions.push(var_ref("alias"));
    for kind in [
        dae::DaeEventActionKind::Assert {
            message: var_ref("alias"),
        },
        dae::DaeEventActionKind::Terminate {
            message: var_ref("alias"),
        },
    ] {
        dae.events.event_actions.push(dae::DaeEventAction {
            condition: var_ref("alias"),
            kind,
            span: Span::DUMMY,
            origin: "event action".to_string(),
        });
    }

    let result = resolve_boundary_equations_to_fixpoint(&mut dae).unwrap();
    let alias = VarName::new("alias");

    assert!(
        result.substitutions.iter().any(|sub| sub.var_name == alias),
        "substitutions: {:?}",
        result
            .substitutions
            .iter()
            .map(|sub| sub.var_name.as_str())
            .collect::<Vec<_>>()
    );
    assert!(!dae.variables.algebraics.contains_key(&alias));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("preexisting_orphan"))
    );
    assert!(
        dae.initialization
            .equations
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &alias))
    );
    assert!(
        dae.events
            .synthetic_root_conditions
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(
        dae.clocks
            .constructor_exprs
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(
        dae.clocks
            .triggered_conditions
            .iter()
            .all(|expr| !expr_contains_var(expr, &alias))
    );
    assert!(dae.events.event_actions.iter().all(|action| {
        !expr_contains_var(&action.condition, &alias)
            && !expr_contains_var(event_message(action), &alias)
    }));
}

#[test]
fn boundary_fixpoint_keeps_target_referenced_by_pre_edge_change_surfaces() {
    let mut dae = boundary_alias_fixture();
    dae.initialization.equations.push(runtime_equation(
        "initial pre",
        builtin(BuiltinFunction::Pre, var_ref("alias")),
    ));
    dae.clocks
        .triggered_conditions
        .push(builtin(BuiltinFunction::Edge, var_ref("alias")));
    dae.events.event_actions.push(dae::DaeEventAction {
        condition: lit(1.0),
        kind: dae::DaeEventActionKind::Terminate {
            message: builtin(BuiltinFunction::Change, var_ref("alias")),
        },
        span: Span::DUMMY,
        origin: "terminate".to_string(),
    });

    let result = resolve_boundary_equations_to_fixpoint(&mut dae).unwrap();
    let alias = VarName::new("alias");

    assert!(result.substitutions.iter().any(|sub| sub.var_name == alias));
    assert!(dae.variables.algebraics.contains_key(&alias));
    assert!(expr_contains_var(
        &dae.initialization.equations[0].rhs,
        &alias
    ));
    assert!(expr_contains_var(
        &dae.clocks.triggered_conditions[0],
        &alias
    ));
    assert!(expr_contains_var(
        event_message(&dae.events.event_actions[0]),
        &alias
    ));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("preexisting_orphan"))
    );
}
