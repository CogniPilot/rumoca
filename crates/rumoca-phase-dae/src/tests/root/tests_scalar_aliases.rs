use super::*;

#[test]
fn test_todae_preserves_primitive_parameter_binding_with_matching_root_leaf() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("m"),
        flat::Variable {
            name: VarName::new("m"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(3),
                span: rumoca_core::Span::DUMMY,
            }),
            evaluate: true,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("multiStarResistance.mBasic"),
        flat::Variable {
            name: VarName::new("multiStarResistance.mBasic"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }),
            evaluate: true,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("multiStarResistance.resistor.m"),
        flat::Variable {
            name: VarName::new("multiStarResistance.resistor.m"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(make_var_ref("multiStarResistance.mBasic")),
            evaluate: true,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should preserve primitive parameter modifier bindings");

    let nested = dae
        .variables
        .parameters
        .get(&rumoca_core::VarName::new("multiStarResistance.resistor.m"))
        .expect("missing nested resistor phase parameter");
    let start = nested
        .start
        .as_ref()
        .expect("nested resistor phase parameter should keep its modifier binding");

    match start {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert!(
                subscripts.is_empty(),
                "expected scalar primitive parameter reference"
            );
            assert_eq!(name.as_str(), "multiStarResistance.mBasic");
        }
        other => panic!("expected VarRef start expression, got {other:?}"),
    }
}

#[test]
fn test_todae_preserves_enum_literal_parameter_binding_with_matching_root_leaf() {
    let mut flat = Model::new();
    flat.enum_literal_ordinals
        .insert("Modelica.Blocks.Types.FilterType.LowPass".to_string(), 1);

    flat.add_variable(
        VarName::new("filterType"),
        flat::Variable {
            name: VarName::new("filterType"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(make_var_ref("Modelica.Blocks.Types.FilterType.LowPass")),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should preserve enum literal parameter bindings");

    let filter_type = dae
        .variables
        .parameters
        .get(&rumoca_core::VarName::new("filterType"))
        .expect("missing enum parameter");
    let start = filter_type
        .start
        .as_ref()
        .expect("enum parameter should keep its literal binding");

    match start {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert!(subscripts.is_empty(), "expected scalar enum literal");
            assert_eq!(name.as_str(), "Modelica.Blocks.Types.FilterType.LowPass");
        }
        other => panic!("expected enum VarRef start expression, got {other:?}"),
    }
}
