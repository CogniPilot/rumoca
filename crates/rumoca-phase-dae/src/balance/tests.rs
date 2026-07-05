use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("balance_fixture.mo"),
        1,
        2,
    )
}

fn scalar_eq(count: usize) -> dae::Equation {
    scalar_eq_with_lhs("x", count)
}

fn scalar_eq_with_lhs(lhs_name: &str, count: usize) -> dae::Equation {
    dae::Equation {
        lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(lhs_name).into(),
                subscripts: vec![],
                span: test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                span: test_span(),
            }),
            span: test_span(),
        },
        span: test_span(),
        origin: "test".to_string(),
        scalar_count: count,
    }
}

fn scalar_assignment_with_rhs_ref(lhs_name: &str, rhs_name: &str) -> dae::Equation {
    dae::Equation {
        lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(rhs_name).into(),
            subscripts: vec![],
            span: test_span(),
        },
        span: test_span(),
        origin: "test".to_string(),
        scalar_count: 1,
    }
}

fn connection_assignment_with_rhs_ref(lhs_name: &str, rhs_name: &str) -> dae::Equation {
    dae::Equation {
        origin: "explicit connection equation: a = b".to_string(),
        ..scalar_assignment_with_rhs_ref(lhs_name, rhs_name)
    }
}

fn connection_assignment_with_count(
    lhs_name: &str,
    rhs_name: &str,
    scalar_count: usize,
) -> dae::Equation {
    dae::Equation {
        scalar_count,
        ..connection_assignment_with_rhs_ref(lhs_name, rhs_name)
    }
}

fn binary_residual_eq_with_count(
    lhs_name: &str,
    rhs_name: &str,
    origin: &str,
    scalar_count: usize,
) -> dae::Equation {
    dae::Equation {
        lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(lhs_name).into(),
                subscripts: vec![],
                span: test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new(rhs_name).into(),
                subscripts: vec![],
                span: test_span(),
            }),
            span: test_span(),
        },
        span: test_span(),
        origin: origin.to_string(),
        scalar_count,
    }
}

fn connection_assignment_with_rhs_index(
    lhs_name: &str,
    rhs_name: &str,
    rhs_index: i64,
) -> dae::Equation {
    dae::Equation {
        lhs: Some(rumoca_core::VarName::new(lhs_name).into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(rhs_name).into(),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                rhs_index,
                test_span(),
            )],
            span: test_span(),
        },
        span: test_span(),
        origin: "explicit connection equation: a = b[1]".to_string(),
        scalar_count: 1,
    }
}

fn dae_with_unknown_scalars(unknown_scalars: i64) -> dae::Dae {
    let mut dae = dae::Dae::default();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            name: rumoca_core::VarName::new("x"),
            dims: vec![unknown_scalars],
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae
}

fn discrete_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn discrete_vector_var(name: &str, size: i64) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        dims: vec![size],
        ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    }
}

fn algebraic_vector_var(name: &str, size: i64) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        dims: vec![size],
        ..rumoca_ir_dae::Variable::empty_with_span(test_span())
    }
}

#[test]
fn test_balance_clamps_overconstrained_interface_to_deficit() {
    let mut dae = dae_with_unknown_scalars(4);
    dae.continuous.equations.push(scalar_eq(4));
    dae.metadata.overconstrained_interface_count = 9;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn admission_accepts_exact_initial_deficit_closure() {
    let mut dae = dae_with_unknown_scalars(3);
    dae.continuous.equations.push(scalar_eq(1));
    dae.initialization
        .equations
        .push(scalar_eq_with_lhs("x", 2));

    let detail = initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -2);
    assert_eq!(detail.deficit_before, 2);
    assert_eq!(detail.initial_equation_scalars, 2);
    assert_eq!(detail.closure_used, 2);
    assert_eq!(detail.deficit_after, 0);
    assert!(detail.is_admissible());
    assert!(is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
}

#[test]
fn admission_rejects_partial_initial_deficit_closure() {
    let mut dae = dae_with_unknown_scalars(4);
    dae.continuous.equations.push(scalar_eq(1));
    dae.initialization
        .equations
        .push(scalar_eq_with_lhs("x", 2));

    let detail = initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

    assert_eq!(detail.deficit_before, 3);
    assert_eq!(detail.closure_used, 2);
    assert_eq!(detail.deficit_after, 1);
    assert!(!detail.is_admissible());
    assert!(!is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
}

#[test]
fn admission_uses_overconstrained_break_edges_as_deficit_closure() {
    let mut dae = dae_with_unknown_scalars(4);
    dae.continuous.equations.push(scalar_eq(2));
    dae.metadata.oc_break_edge_scalar_count = 2;
    let detail = initial_closure_balance_detail(&dae).expect("valid DAE fixture");

    assert_eq!(
        (
            balance(&dae).expect("valid DAE fixture"),
            detail.deficit_before,
            detail.overconstrained_break_edge_scalars,
            detail.closure_used,
            detail.deficit_after,
            is_balanced_for_admission(&dae).expect("valid DAE fixture"),
        ),
        (-2, 2, 2, 2, 0, true)
    );
}

#[test]
fn admission_rejects_missing_initial_deficit_closure() {
    let mut dae = dae_with_unknown_scalars(2);
    dae.continuous.equations.push(scalar_eq(1));

    let detail = initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

    assert_eq!(detail.deficit_before, 1);
    assert_eq!(detail.initial_equation_scalars, 0);
    assert_eq!(detail.deficit_after, 1);
    assert!(!is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
}

#[test]
fn admission_does_not_mask_overdetermined_balance_with_initial_equations() {
    let mut dae = dae_with_unknown_scalars(1);
    dae.continuous.equations.push(scalar_eq(2));
    dae.initialization
        .equations
        .push(scalar_eq_with_lhs("x", 3));

    let detail = initial_closure_balance_detail(&dae).expect("valid DAE initial balance fixture");

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 1);
    assert_eq!(detail.deficit_before, 0);
    assert_eq!(detail.closure_used, 0);
    assert!(!detail.is_admissible());
    assert!(!is_balanced_for_admission(&dae).expect("valid DAE balance fixture"));
}

#[test]
fn test_balance_uses_only_needed_overconstrained_interface() {
    let mut dae = dae_with_unknown_scalars(4);
    dae.continuous.equations.push(scalar_eq(3));
    dae.metadata.overconstrained_interface_count = 9;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_applies_oc_interface_even_with_break_edges() {
    let mut dae = dae_with_unknown_scalars(10);
    dae.continuous.equations.push(scalar_eq(1));
    dae.metadata.overconstrained_interface_count = 9;
    dae.metadata.oc_break_edge_scalar_count = 12;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_clamps_interface_flow_to_remaining_deficit() {
    let mut dae = dae_with_unknown_scalars(4);
    dae.continuous.equations.push(scalar_eq(4));
    dae.metadata.interface_flow_count = 3;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_uses_interface_flow_to_close_deficit_only() {
    let mut dae = dae_with_unknown_scalars(5);
    dae.continuous.equations.push(scalar_eq(3));
    dae.metadata.interface_flow_count = 9;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_clamps_stream_interface_equations_to_remaining_deficit() {
    let mut dae = dae_with_unknown_scalars(4);
    dae.continuous.equations.push(scalar_eq(4));
    dae.metadata.stream_interface_equation_count = 3;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_uses_stream_interface_equations_to_close_deficit_only() {
    let mut dae = dae_with_unknown_scalars(5);
    dae.continuous.equations.push(scalar_eq(3));
    dae.metadata.stream_interface_equation_count = 9;
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_ignores_unconstrained_discrete_real_declaration() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_discrete_updates_against_discrete_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("m"), discrete_var("m"));
    dae.discrete.valued_updates.push(scalar_eq_with_lhs("m", 1));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_ignores_solved_discrete_update_rhs_refs_as_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("guard"), discrete_var("guard"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("m"), discrete_var("m"));
    dae.discrete
        .valued_updates
        .push(scalar_assignment_with_rhs_ref("m", "guard"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_discrete_connection_update_rhs_refs_as_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("a"), discrete_var("a"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), discrete_var("b"));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("a", "b"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -1);
}

#[test]
fn balance_rejects_missing_discrete_input_metadata_variable() {
    let mut dae = dae::Dae::default();
    dae.metadata
        .discrete_input_names
        .push("missing_input".to_string());

    let err = balance(&dae).expect_err("missing discrete metadata should fail");

    assert!(matches!(
        err,
        BalanceError::MissingDiscreteVariableMetadata { ref name, .. }
            if name.as_str() == "missing_input"
    ));
}

#[test]
fn test_balance_ignores_partial_vector_connection_rhs_refs_as_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("a"), discrete_var("a"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), discrete_vector_var("b", 2));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("a", "b"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_expands_scalarized_discrete_connection_targets() {
    let mut dae = dae::Dae::default();
    for name in ["a[1]", "a[2]", "b[1]", "b[2]"] {
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), discrete_var(name));
    }
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_count("a", "b", 2));

    assert_eq!(
        balance(&dae).expect("scalarized vector connection should expand to children"),
        2
    );
}

#[test]
fn test_balance_anchors_size_one_discrete_input_vectors_by_index() {
    let mut dae = dae::Dae::default();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("source"),
        discrete_vector_var("source", 1),
    );
    for name in ["a", "b"] {
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), discrete_var(name));
    }
    dae.metadata.discrete_input_names.push("source".to_string());
    dae.discrete.valued_updates.push(scalar_eq_with_lhs("b", 1));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_index("a", "source", 1));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("a", "b"));

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_discrete_connection_cycles_by_graph_rank() {
    let mut dae = dae::Dae::default();
    for name in ["a", "b", "c"] {
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), discrete_var(name));
    }
    dae.discrete
        .valued_updates
        .push(scalar_assignment_with_rhs_ref("a", "source"));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("a", "b"));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("b", "c"));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("c", "a"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_skips_discrete_connection_between_component_defined_targets() {
    let mut dae = dae::Dae::default();
    for name in ["a", "b"] {
        dae.variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), discrete_var(name));
        dae.discrete
            .valued_updates
            .push(scalar_assignment_with_rhs_ref(name, "source"));
    }
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_ref("a", "b"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_matches_subscripted_connection_rhs_to_vector_anchor() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("a"), discrete_var("a"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), discrete_vector_var("b", 2));
    dae.discrete
        .valued_updates
        .push(scalar_assignment_with_rhs_ref("a", "source"));
    dae.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("b").into()),
        scalar_count: 2,
        ..scalar_assignment_with_rhs_ref("b", "source")
    });
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_rhs_index("a", "b", 1));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_vector_discrete_connection_cycles_by_scalar_rank() {
    let mut dae = dae::Dae::default();
    for name in ["a", "b", "c"] {
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            discrete_vector_var(name, 2),
        );
    }
    dae.discrete.valued_updates.push(dae::Equation {
        scalar_count: 2,
        ..scalar_assignment_with_rhs_ref("a", "source")
    });
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_count("a", "b", 2));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_count("b", "c", 2));
    dae.discrete
        .valued_updates
        .push(connection_assignment_with_count("c", "a", 2));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_discrete_real_updates_against_discrete_real_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
    dae.discrete.real_updates.push(scalar_eq_with_lhs("z", 1));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_residual_discrete_real_updates() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), discrete_var("z"));
    let mut eq = scalar_eq_with_lhs("z", 1);
    eq.lhs = None;
    dae.discrete.real_updates.push(eq);
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_ignores_state_reinit_updates_for_static_balance() {
    let mut dae = dae::Dae::default();
    dae.variables
        .states
        .insert(rumoca_core::VarName::new("x"), discrete_var("x"));
    dae.continuous.equations.push(scalar_eq(1));
    dae.discrete.real_updates.push(scalar_eq(1));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn test_balance_counts_condition_equations_against_condition_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), discrete_var("c"));
    dae.conditions.equations.push(scalar_eq_with_lhs("c", 1));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}

#[test]
fn balance_spends_component_vector_aliases_only_against_surplus() {
    let mut dae = dae::Dae::default();
    for name in ["a", "b", "c"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "a",
        "source",
        "component equation",
        3,
    ));
    dae.continuous.equations.push(dae::Equation {
        scalar_count: 6,
        ..scalar_eq_with_lhs("b", 6)
    });
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "a",
        "c",
        "connection equation: a = c",
        3,
    ));

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);

    for name in ["d", "e"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -3);
}

#[test]
fn balance_spends_component_vector_forwarding_only_against_surplus() {
    let mut dae = dae::Dae::default();
    for name in ["source", "alias"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "source",
        "source_driver",
        "component equation",
        3,
    ));
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "alias",
        "alias_driver",
        "component equation",
        3,
    ));
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "alias",
        "source",
        "equation from adaptor",
        3,
    ));

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);

    for name in ["d", "e"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -3);
}

#[test]
fn balance_spends_scalarized_vector_bindings_only_against_surplus() {
    let mut dae = dae::Dae::default();
    dae.variables.outputs.insert(
        rumoca_core::VarName::new("alias"),
        algebraic_vector_var("alias", 3),
    );
    for name in ["source", "extra"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "source",
        "source_driver",
        "component equation",
        3,
    ));
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "extra",
        "extra_driver",
        "component equation",
        6,
    ));
    for idx in 1..=3 {
        dae.continuous.equations.push(binary_residual_eq_with_count(
            "alias",
            "source",
            &format!("binding equation for alias [scalarized {idx}]"),
            1,
        ));
    }

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);

    for name in ["d", "e"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -3);
}

#[test]
fn balance_spends_scalarized_vector_forwarding_only_against_surplus() {
    let mut dae = dae::Dae::default();
    dae.variables.outputs.insert(
        rumoca_core::VarName::new("alias"),
        algebraic_vector_var("alias", 3),
    );
    for name in ["source", "extra"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "source",
        "source_driver",
        "component equation",
        3,
    ));
    dae.continuous.equations.push(binary_residual_eq_with_count(
        "extra",
        "extra_driver",
        "component equation",
        6,
    ));
    for idx in 1..=3 {
        dae.continuous.equations.push(binary_residual_eq_with_count(
            "alias",
            "source",
            &format!("equation from adaptor [scalarized {idx}]"),
            1,
        ));
    }

    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);

    for name in ["d", "e"] {
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(name),
            algebraic_vector_var(name, 3),
        );
    }
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), -3);
}

#[test]
fn test_balance_ignores_condition_rhs_refs_as_unknowns() {
    let mut dae = dae::Dae::default();
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("guard"), discrete_var("guard"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), discrete_var("c"));
    dae.conditions
        .equations
        .push(scalar_assignment_with_rhs_ref("c", "guard"));
    assert_eq!(balance(&dae).expect("valid DAE balance fixture"), 0);
}
