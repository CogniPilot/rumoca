//! Regression coverage for subscript stripping, embedded-subscript counting, and variable-reference scalar sizing.

use super::*;

#[test]
fn test_has_evaluable_arithmetic_subscript() {
    // Evaluable integer arithmetic: should return true
    assert!(has_evaluable_arithmetic_subscript("pc[((2 * 1) - 1)].i"));
    assert!(has_evaluable_arithmetic_subscript("pc[(2 * 1)].i"));
    assert!(has_evaluable_arithmetic_subscript("x[(1 + 1)]"));
    assert!(has_evaluable_arithmetic_subscript("z[(2 - 1)]"));

    // Simple integer subscripts: should return false
    assert!(!has_evaluable_arithmetic_subscript("pc[1].i"));
    assert!(!has_evaluable_arithmetic_subscript("x[2]"));
    assert!(!has_evaluable_arithmetic_subscript("T[1,2]"));

    // No subscripts: should return false
    assert!(!has_evaluable_arithmetic_subscript("x"));
    assert!(!has_evaluable_arithmetic_subscript("a.b.c"));

    // Unresolved variable names in subscripts: should return false
    assert!(!has_evaluable_arithmetic_subscript("suspend[i]"));
    assert!(!has_evaluable_arithmetic_subscript("x[n]"));
    assert!(!has_evaluable_arithmetic_subscript("port_a[m].h"));
}

#[test]
fn test_strip_subscript_handles_nested_brackets_in_subscript_expression() {
    assert_eq!(
        strip_subscript("medium_T[1].X[medium_T[1].nX]").map(|v| v.to_string()),
        Some("medium_T[1].X".to_string())
    );
}

#[test]
fn test_count_embedded_subscripts_ignores_nested_component_indices() {
    assert_eq!(
        count_embedded_subscripts("medium_T[1].X[medium_T[1].nX]"),
        2
    );
}

#[test]
fn test_strip_subscript_preserves_field_suffix() {
    assert_eq!(
        strip_subscript("pc[1].i").map(|v| v.to_string()),
        Some("pc.i".to_string())
    );
    assert_eq!(
        strip_subscript("sum.u[1]").map(|v| v.to_string()),
        Some("sum.u".to_string())
    );
}

#[test]
fn test_infer_scalar_count_arithmetic_subscript_does_not_inflate() {
    let mut flat = Model::new();
    for name in ["pc[1].i", "pc[2].i"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let expr = Expression::VarRef {
        name: VarName::new("pc[((2 * 1) - 1)].i").into(),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    };
    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, None,
        "unevaluated arithmetic subscripts should not be mapped to base-array scalar size"
    );
}

#[test]
fn test_infer_scalar_count_varref_subscripts_use_element_size() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("line.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("line.i"),
            dims: vec![4],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Reproduces Electrical M_OLine-style references where scalarized element access
    // must not inherit the full base-array size.
    let expr = Expression::VarRef {
        name: VarName::new("line.i").into(),
        subscripts: vec![Subscript::generated_index(
            1,
            crate::test_support::test_span(),
        )],
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(
        scalar_count,
        Some(1),
        "subscripted varrefs should infer scalar element size, not full array size"
    );
}

#[test]
fn test_infer_scalar_count_varref_subscripts_zero_sized_dim_is_zero() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("line.i"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("line.i"),
            dims: vec![0],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let expr = Expression::VarRef {
        name: VarName::new("line.i").into(),
        subscripts: vec![Subscript::generated_index(
            1,
            crate::test_support::test_span(),
        )],
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(
        scalar_count,
        Some(0),
        "indexing a zero-sized dimension should produce zero scalar equations"
    );
}

#[test]
fn test_infer_varref_form_multilayer_embedded_subscript_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let prefix_counts = build_prefix_counts(&flat);
    let form = infer_varref_form("bus[1].signal[2]", &[], &flat, &prefix_counts);
    assert_eq!(form, ExpressionForm::Scalar);
}

#[test]
fn test_infer_scalar_count_multilayer_embedded_subscript_varref_is_scalar() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let expr = Expression::VarRef {
        name: VarName::new("bus[1].signal[2]").into(),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    };
    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_scalar_count_from_varrefs(&expr, &flat, &prefix_counts);
    assert_eq!(scalar_count, Some(1));
}

#[test]
fn test_build_prefix_counts_normalizes_embedded_subscripts() {
    let mut flat = Model::new();
    for name in [
        "r1.v[1].re",
        "r1.v[1].im",
        "r1.v[2].re",
        "r1.v[2].im",
        "r1.v[3].re",
        "r1.v[3].im",
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        prefix_counts.get("r1.v").copied(),
        Some(6),
        "normalized prefix should aggregate scalarized element fields"
    );
}
