use super::*;

/// A scalar parameter binding that references a known variable must keep that
/// reference instead of suffix-resolving to an unrelated variable.
#[test]
fn test_scalar_binding_to_known_variable_keeps_reference() {
    let mut flat = Model::new();
    for (name, value) in [("m", 3), ("multiStar.mBasic", 1)] {
        let var_name = VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            crate::test_support::with_component_ref(flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    span: crate::test_support::test_span(),
                }),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let target = VarName::new("multiStar.resistor.m");
    flat.add_variable(
        target.clone(),
        crate::test_support::with_component_ref(flat::Variable {
            name: target.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(
                    rumoca_core::component_reference_from_flat_name(
                        &VarName::new("multiStar.mBasic"),
                        crate::test_support::test_span(),
                    )
                    .expect("fixture name must form a component reference"),
                ),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("scalar parameter bindings should convert");
    let start = dae
        .variables
        .parameters
        .get(&target)
        .expect("target parameter should exist in DAE")
        .start
        .as_ref()
        .expect("binding becomes parameter start");
    let Expression::VarRef { name, .. } = start else {
        panic!("expected a variable reference start, got {start:?}");
    };
    assert_eq!(name.var_name().as_str(), "multiStar.mBasic");
}
