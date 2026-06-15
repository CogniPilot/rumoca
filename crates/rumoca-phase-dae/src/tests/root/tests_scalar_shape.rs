use super::*;

#[test]
fn test_classify_equations_prefers_lhs_scalar_shape_over_flat_scalar_count() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x"),
            is_primitive: true,
            ..Default::default()
        }),
    );
    flat.add_variable(
        VarName::new("a"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("a"),
            dims: vec![4],
            is_primitive: true,
            ..Default::default()
        }),
    );
    flat.add_variable(
        VarName::new("b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("b"),
            dims: vec![4],
            is_primitive: true,
            ..Default::default()
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("x")),
            rhs: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(make_var_ref("a")),
                rhs: Box::new(make_var_ref("b")),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
        // Simulate conservative flatten scalarization from array varrefs.
        scalar_count: 4,
    });

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("x"),
        Variable::new(rumoca_core::VarName::new("x")),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(rumoca_core::VarName::new("a")),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(rumoca_core::VarName::new("b")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(
        dae.continuous.equations[0].scalar_count, 1,
        "scalar LHS should force a single scalar equation even when flat carried array-sized scalar_count"
    );
}
