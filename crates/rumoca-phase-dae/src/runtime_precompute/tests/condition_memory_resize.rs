use super::*;

#[test]
fn test_runtime_precompute_resizes_condition_memory_variables_after_prune() {
    let time_only = time_gt(2.5);
    let root_cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Gt,
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(root_cond.clone());
    dae_model.conditions.relations = vec![time_only.clone(), root_cond];
    dae_model.conditions.equations = vec![
        dae::Equation::explicit(
            condition_lhs("c", 1),
            time_only,
            test_span(60, 65),
            "condition equation from test",
        ),
        dae::Equation::explicit(
            condition_lhs("c", 2),
            dae_model.conditions.relations[1].clone(),
            test_span(60, 65),
            "condition equation from test",
        ),
    ];
    let mut condition = dae::Variable::new(rumoca_core::VarName::new("c"), test_span(60, 65));
    condition.dims = vec![2];
    condition.start = Some(rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: test_span(60, 65),
            };
            2
        ],
        is_matrix: false,
        span: test_span(60, 65),
    });
    let mut pre_condition =
        dae::Variable::new(rumoca_core::VarName::new("__pre__.c"), test_span(60, 65));
    pre_condition.dims = vec![2];
    pre_condition.start = condition.start.clone();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), condition);
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("__pre__.c"), pre_condition);

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(
        dae_model
            .variables
            .discrete_valued
            .get(&rumoca_core::VarName::new("c"))
            .map(|variable| variable.dims.as_slice()),
        Some(&[1][..])
    );
    assert_eq!(
        dae_model
            .variables
            .parameters
            .get(&rumoca_core::VarName::new("__pre__.c"))
            .map(|variable| variable.dims.as_slice()),
        Some(&[1][..])
    );
    assert!(
        matches!(
            &dae_model
                .variables
                .discrete_valued
                .get(&rumoca_core::VarName::new("c"))
                .and_then(|variable| variable.start.as_ref()),
            Some(rumoca_core::Expression::Array { elements, .. }) if elements.len() == 1
        ),
        "condition memory start vector must match the pruned relation count"
    );
}
