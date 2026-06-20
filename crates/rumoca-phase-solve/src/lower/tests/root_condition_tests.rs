use super::*;

#[test]
fn lower_discrete_rhs_skips_untargeted_condition_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    let condition = binary(
        rumoca_core::OpBinary::Gt,
        var("y"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        },
    );
    dae_model.conditions.relations.push(condition.clone());
    dae_model.conditions.equations.push(dae::Equation::residual(
        condition,
        rumoca_core::Span::DUMMY,
        // MLS Appendix B: a targetless f_c row is a relation/root condition,
        // not a condition-memory assignment row for discrete event updates.
        "targetless relation row",
    ));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("targetless relation rows should not be discrete assignments");

    assert!(rows.is_empty());
}

#[test]
fn lower_root_conditions_emit_unbiased_relation_surface() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("h"), scalar_var("h"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c[1]"), scalar_var("c[1]"));
    let relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("h")),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span,
        }),
        span,
    };
    dae_model.conditions.relations.push(relation.clone());
    dae_model.conditions.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("c[1]"),
        relation,
        rumoca_core::Span::DUMMY,
        "condition memory",
    ));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_root_conditions(&dae_model, &layout).expect("root lowering should succeed");

    assert_eq!(rows.len(), 1);
    let (_, active_output) = eval_linear_ops(&rows[0], &[0.0], &[1.0], 0.0);
    let (_, inactive_output) = eval_linear_ops(&rows[0], &[0.0], &[0.0], 0.0);
    let (_, cleared_output) = eval_linear_ops(&rows[0], &[1.0e-5], &[1.0], 0.0);

    assert_eq!(
        active_output.expect("active condition memory should produce a root value"),
        0.0,
        "condition memory must not bias the relation root surface"
    );
    assert_eq!(
        inactive_output.expect("inactive condition memory should produce a root value"),
        0.0,
        "condition memory must not bias the relation root surface"
    );
    assert!(
        cleared_output.expect("cleared relation surface should stay positive") > 0.0,
        "the raw relation surface should expose cleared-side residuals"
    );
}

#[test]
fn lower_root_conditions_emit_unbiased_aggregate_relation_surfaces() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("h1"), scalar_var("h1"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("h2"), scalar_var("h2"));
    let mut c = scalar_var("c");
    c.dims = vec![2];
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), c);

    let first_relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("h1")),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span,
        }),
        span,
    };
    let second_relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("h2")),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span,
        }),
        span,
    };
    dae_model.conditions.relations.push(first_relation.clone());
    dae_model.conditions.relations.push(second_relation.clone());
    dae_model
        .conditions
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            rumoca_core::VarName::new("c"),
            rumoca_core::Expression::Array {
                elements: vec![first_relation, second_relation],
                is_matrix: false,
                span,
            },
            rumoca_core::Span::DUMMY,
            "MLS Appendix B aggregate condition memories",
            2,
        ));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_root_conditions(&dae_model, &layout).expect("root lowering should succeed");
    assert_eq!(rows.len(), 2);

    let y = [0.0, 0.0];
    let first_active = [1.0, 0.0];
    let second_active = [0.0, 1.0];
    let (_, first_row_active) = eval_linear_ops(&rows[0], &y, &first_active, 0.0);
    let (_, second_row_inactive) = eval_linear_ops(&rows[1], &y, &first_active, 0.0);
    let (_, first_row_inactive) = eval_linear_ops(&rows[0], &y, &second_active, 0.0);
    let (_, second_row_active) = eval_linear_ops(&rows[1], &y, &second_active, 0.0);

    assert_eq!(
        first_row_active.expect("first row should evaluate"),
        0.0,
        "c[1] must not bias the first relation surface"
    );
    assert_eq!(
        second_row_inactive.expect("second row should evaluate"),
        0.0,
        "c[2] must not bias the second relation surface"
    );
    assert_eq!(
        first_row_inactive.expect("first row should evaluate"),
        0.0,
        "c[1] must not bias the first relation surface"
    );
    assert_eq!(
        second_row_active.expect("second row should evaluate"),
        0.0,
        "c[2] must not bias the second relation surface"
    );
}

#[test]
fn lower_root_conditions_keep_enum_literal_relations_active() {
    let mut dae_model = dae::Dae::default();
    let span = lower_test_span();
    dae_model.symbols.enum_literal_ordinals.extend([
        ("LimiterHomotopy.NoHomotopy".to_string(), 1),
        ("LimiterHomotopy.Linear".to_string(), 2),
        ("LimiterHomotopy.UpperLimit".to_string(), 3),
        ("LimiterHomotopy.LowerLimit".to_string(), 4),
    ]);
    let mut mode = scalar_var("mode");
    mode.start = Some(var("LimiterHomotopy.LowerLimit"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("mode"), mode);

    dae_model
        .conditions
        .relations
        .push(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("mode")),
            rhs: Box::new(var("LimiterHomotopy.LowerLimit")),
            span,
        });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_root_conditions(&dae_model, &layout)
        .expect("enum-literal relation should lower as an active root");

    let (_, true_output) = eval_linear_ops(&rows[0], &[], &[4.0], 0.0);
    let (_, false_output) = eval_linear_ops(&rows[0], &[], &[1.0], 0.0);

    assert!(
        true_output.expect("matching enum relation should evaluate") < 0.0,
        "matching enum literal equality must be an active true root"
    );
    assert!(
        false_output.expect("non-matching enum relation should evaluate") > 0.0,
        "non-matching enum literal equality must be an active false root"
    );
}
