use super::*;

fn declare_array(flat: &mut Model, name: &str, dims: &[i64]) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            dims: dims.to_vec(),
            is_primitive: true,
            ..flat::Variable::empty_with_span(crate::test_support::test_span())
        }),
    );
}

fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    }
}

fn literal_subscripts(expr: &Expression) -> Option<(&str, Vec<i64>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => Some(*value),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some((name.as_str(), indices))
}

fn assert_compound_dot_term(expr: &Expression, row: i64, inner: i64) {
    let Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs,
        rhs,
        ..
    } = expr
    else {
        panic!("expected compound dot term, got {expr:?}");
    };
    let Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: lhs_a,
        rhs: lhs_b,
        ..
    } = lhs.as_ref()
    else {
        panic!("expected matrix inner sum, got {lhs:?}");
    };
    assert_eq!(literal_subscripts(lhs_a), Some(("A", vec![row, inner])));
    assert_eq!(literal_subscripts(lhs_b), Some(("B", vec![row, inner])));
    let Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: rhs_c,
        rhs: rhs_d,
        ..
    } = rhs.as_ref()
    else {
        panic!("expected vector inner sum, got {rhs:?}");
    };
    assert_eq!(literal_subscripts(rhs_c), Some(("C", vec![inner])));
    assert_eq!(literal_subscripts(rhs_d), Some(("D", vec![inner])));
}

#[test]
fn test_todae_projects_bare_compound_array_operands_as_complete_dots() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("A", [2, 2].as_slice()),
        ("B", [2, 2].as_slice()),
        ("C", [2].as_slice()),
        ("D", [2].as_slice()),
        ("Y", [2].as_slice()),
    ] {
        declare_array(&mut flat, name, dims);
    }
    let add = |lhs, rhs| binary(rumoca_core::OpBinary::Add, lhs, rhs);
    let product = binary(
        rumoca_core::OpBinary::Mul,
        add(make_structured_var_ref("A"), make_structured_var_ref("B")),
        add(make_structured_var_ref("C"), make_structured_var_ref("D")),
    );
    flat.add_equation(flat::Equation {
        residual: binary(
            rumoca_core::OpBinary::Sub,
            Expression::Index {
                base: Box::new(make_structured_var_ref("Y")),
                subscripts: vec![rumoca_core::Subscript::Colon {
                    span: crate::test_support::test_span(),
                }],
                span: crate::test_support::test_span(),
            },
            product,
        ),
        span: crate::test_support::test_span(),
        origin: flat::EquationOrigin::ComponentEquation {
            component: "CompoundArrayProduct".to_string(),
        },
        scalar_count: 2,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("bare compound array operands must project as a matrix-vector product");

    assert_eq!(dae.continuous.equations.len(), 2);
    for (lane, equation) in dae.continuous.equations.iter().enumerate() {
        let row = i64::try_from(lane + 1).expect("two lanes fit i64");
        let Expression::Binary { lhs, rhs, .. } = &equation.rhs else {
            panic!("expected scalar residual");
        };
        assert_eq!(literal_subscripts(lhs), Some(("Y", vec![row])));
        let Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: first,
            rhs: second,
            ..
        } = rhs.as_ref()
        else {
            panic!("lane {row} must contain the complete two-term dot, got {rhs:?}");
        };
        assert_compound_dot_term(first, row, 1);
        assert_compound_dot_term(second, row, 2);
    }
}
