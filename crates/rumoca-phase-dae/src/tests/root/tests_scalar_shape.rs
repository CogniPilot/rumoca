use super::*;

#[test]
fn test_classify_equations_prefers_lhs_scalar_shape_over_flat_scalar_count() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("a"),
        flat::Variable {
            name: VarName::new("a"),
            dims: vec![4],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("b"),
        flat::Variable {
            name: VarName::new("b"),
            dims: vec![4],
            is_primitive: true,
            ..Default::default()
        },
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

fn indexed_field_access(base: &str, index: i64, fields: &[&str]) -> rumoca_core::Expression {
    let mut expr = rumoca_core::Expression::Index {
        base: Box::new(make_var_ref(base)),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            index,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };
    for field in fields {
        expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(expr),
            field: (*field).to_string(),
            span: rumoca_core::Span::DUMMY,
        };
    }
    expr
}

fn arithmetic_indexed_field_access(base: &str, fields: &[&str]) -> rumoca_core::Expression {
    let mut expr = rumoca_core::Expression::Index {
        base: Box::new(make_var_ref(base)),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(2),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };
    for field in fields {
        expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(expr),
            field: (*field).to_string(),
            span: rumoca_core::Span::DUMMY,
        };
    }
    expr
}

fn add_complex_record_prefix(flat: &mut Model, prefix: &str) {
    for suffix in [".re", ".im"] {
        flat.add_variable(
            VarName::new(format!("{prefix}{suffix}")),
            flat::Variable {
                name: VarName::new(format!("{prefix}{suffix}")),
                is_primitive: true,
                ..Default::default()
            },
        );
    }
}

#[test]
fn test_extract_lhs_var_size_preserves_indexed_record_field_path() {
    let mut flat = Model::new();
    for phase in 1..=3 {
        add_complex_record_prefix(
            &mut flat,
            &format!(
                "converter_m.singlePhaseElectroMagneticConverter[{phase}].port_p.Phi"
            ),
        );
    }

    let lhs = indexed_field_access(
        "converter_m.singlePhaseElectroMagneticConverter",
        1,
        &["port_p", "Phi"],
    );
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(make_var_ref("rhs")),
        span: rumoca_core::Span::DUMMY,
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = extract_lhs_var_size(&residual, &flat, &prefix_counts);

    assert_eq!(
        scalar_count,
        Some(2),
        "indexed Complex field should size to one element's re/im pair, not the full array"
    );
}

#[test]
fn test_infer_scalar_count_from_varrefs_preserves_indexed_record_field_path() {
    let mut flat = Model::new();
    for phase in 1..=3 {
        add_complex_record_prefix(
            &mut flat,
            &format!(
                "converter_m.singlePhaseElectroMagneticConverter[{phase}].port_p.Phi"
            ),
        );
    }

    let expr = indexed_field_access(
        "converter_m.singlePhaseElectroMagneticConverter",
        1,
        &["port_p", "Phi"],
    );
    let prefix_counts = build_prefix_counts(&flat);

    assert_eq!(
        infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts),
        Some(2),
        "var-ref fallback should keep the element index and infer one Complex field"
    );
}

#[test]
fn test_infer_scalar_count_from_varrefs_rejects_unrenderable_indexed_field_guess() {
    let mut flat = Model::new();
    for phase in 1..=3 {
        add_complex_record_prefix(
            &mut flat,
            &format!(
                "converter_m.singlePhaseElectroMagneticConverter[{phase}].port_p.Phi"
            ),
        );
    }

    let expr = arithmetic_indexed_field_access(
        "converter_m.singlePhaseElectroMagneticConverter",
        &["port_p", "Phi"],
    );
    let prefix_counts = build_prefix_counts(&flat);

    assert_eq!(
        infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts),
        None,
        "unrenderable indexed field access should not silently fall back to the whole-array prefix size"
    );
}
