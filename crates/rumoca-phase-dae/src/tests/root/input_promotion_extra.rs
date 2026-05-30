use super::*;

#[test]
fn test_rhs_intra_component_alias_with_multilayer_connected_lhs_does_not_promote_input() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("test.p"),
        flat::Variable {
            name: VarName::new("test.p"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("test.conn.field"),
        flat::Variable {
            name: VarName::new("test.conn.field"),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            connected: true,
            dims: vec![2, 3],
            ..Default::default()
        },
    );

    add_component_equation(&mut flat, "test.conn[1].field[2]", make_var_ref("test.p"));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for multi-layer connected LHS alias");

    let input = rumoca_core::VarName::new("test.p");
    assert!(
        dae.variables.inputs.contains_key(&input),
        "RHS input should remain an input when aliased from a connected multi-layer LHS"
    );
    assert!(
        !dae.variables.algebraics.contains_key(&input),
        "RHS input should not be promoted to algebraic when LHS is connected"
    );
}

#[test]
fn test_model_description_propagation() {
    let mut flat = flat::Model::new();
    flat.model_description = Some("Test model description".to_string());

    // Add a simple variable to make it valid
    let var = rumoca_ir_flat::Variable {
        name: "x".into(),
        variability: rumoca_core::Variability::Parameter(Default::default()),
        ..Default::default()
    };
    flat.add_variable("x".into(), var);

    let dae = to_dae(&flat).unwrap();
    assert_eq!(
        dae.metadata.model_description,
        Some("Test model description".to_string())
    );
}
