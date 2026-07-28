use super::*;

fn enum_literal_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: Vec::new(),
        span: crate::test_support::test_span(),
    }
}

fn add_enum_bound_variable(
    flat: &mut Model,
    name: &str,
    literal_name: &str,
    variability: rumoca_core::Variability,
) {
    let name = VarName::new(name);
    flat.add_variable(
        name.clone(),
        crate::test_support::with_component_ref(flat::Variable {
            name,
            variability,
            binding: Some(enum_literal_ref(literal_name)),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(crate::test_support::test_span())
        }),
    );
}

fn assert_ordinal_start(variable: &rumoca_ir_dae::Variable, expected: i64) {
    assert!(
        matches!(
            variable.start,
            Some(Expression::Literal {
                value: Literal::Integer(value),
                ..
            }) if value == expected
        ) || matches!(
            variable.start,
            Some(Expression::Literal {
                value: Literal::Real(value),
                ..
            }) if value == expected as f64
        ),
        "enum binding must survive Flat→DAE as ordinal {expected}, got {:?}",
        variable.start
    );
}

fn assert_ordinal_expression(expression: Option<&Expression>, expected: i64) {
    assert!(
        matches!(
            expression,
            Some(Expression::Literal {
                value: Literal::Integer(value),
                ..
            }) if *value == expected
        ) || matches!(
            expression,
            Some(Expression::Literal {
                value: Literal::Real(value),
                ..
            }) if *value == expected as f64
        ),
        "enum metadata must preserve ordinal {expected}, got {expression:?}"
    );
}

#[test]
fn enum_parameter_and_constant_bindings_survive_flat_to_dae() {
    let mut flat = Model::new();
    flat.enum_literal_ordinals
        .insert("Color.green".to_string(), 2);
    flat.enum_literal_ordinals
        .insert("Pkg.Types.Mode.active".to_string(), 3);
    add_enum_bound_variable(
        &mut flat,
        "c",
        "Color.green",
        rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
    );
    add_enum_bound_variable(
        &mut flat,
        "mode",
        "Pkg.Types.Mode.active",
        rumoca_core::Variability::Constant(rumoca_core::Token::default()),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("enum-bound parameter and constant should lower");

    assert_ordinal_start(
        dae.variables
            .parameters
            .get(&VarName::new("c"))
            .expect("missing enum parameter"),
        2,
    );
    assert_ordinal_start(
        dae.variables
            .constants
            .get(&VarName::new("mode"))
            .expect("missing enum constant"),
        3,
    );
    assert_ordinal_expression(dae.metadata.variable_starts.get("c"), 2);
    assert_ordinal_expression(dae.metadata.variable_starts.get("mode"), 3);
}
