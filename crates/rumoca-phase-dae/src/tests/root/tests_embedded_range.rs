use super::*;

#[test]
fn test_classify_equations_embedded_range_zero_dimension_is_elided() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("ports_mXi_flow"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("ports_mXi_flow"),
            dims: vec![1, 0],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("ports_mXi_flow[1,:]")),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Integer(0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
        // Flatten may conservatively carry scalar_count=1 for this form.
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("ports_mXi_flow"),
        Variable::new(
            rumoca_core::VarName::new("ports_mXi_flow"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert!(
        dae.continuous.equations.is_empty(),
        "embedded range over a zero-sized dimension must contribute zero scalar equations"
    );
}
