//! Regression coverage for scalar-count inference of function-call left-hand sides.

use super::*;

#[test]
fn test_infer_scalar_count_function_lhs_uses_function_output_dims() {
    let mut flat = Model::new();
    // Record-like argument prefix with scalar size 12 (9 + 3).
    flat.add_variable(
        VarName::new("R_b.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("R_b.w"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.w"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("w_rel_b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("w_rel_b"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let mut f =
        rumoca_core::Function::new("Frames.angularVelocity2", crate::test_support::test_span());
    f.add_input(rumoca_core::FunctionParam::new(
        "R",
        "Orientation",
        crate::test_support::test_span(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new("w", "Real", crate::test_support::test_span())
            .with_dims(vec![3]),
    );
    flat.add_function(f);

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.angularVelocity2").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("R_b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "function-call LHS should use function output dims, not record argument size"
    );
}

#[test]
fn test_infer_scalar_count_function_lhs_supports_alias_suffix_lookup() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("R_b.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("R_b.w"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.w"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("w_rel_b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("w_rel_b"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Store function under a fully-qualified name, but call through a short alias.
    let mut f = rumoca_core::Function::new(
        "Modelica.Mechanics.MultiBody.Frames.angularVelocity2",
        crate::test_support::test_span(),
    );
    f.add_input(rumoca_core::FunctionParam::new(
        "R",
        "Orientation",
        crate::test_support::test_span(),
    ));
    f.add_output(
        rumoca_core::FunctionParam::new("w", "Real", crate::test_support::test_span())
            .with_dims(vec![3]),
    );
    flat.add_function(f);

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.angularVelocity2").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("R_b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "alias function call should resolve to unique fully-qualified function output size"
    );
}

#[test]
fn test_infer_scalar_count_function_lhs_uses_rhs_unknown_size_when_signature_is_unavailable() {
    let mut flat = Model::new();
    // Record-like argument prefix with scalar size 12.
    flat.add_variable(
        VarName::new("R_b.T"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.T"),
            dims: vec![3, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("R_b.w"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("R_b.w"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("w_rel_b"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("w_rel_b"),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // No function definition added on purpose: scalar inference should use the
    // opposite side's declared unknown size (w_rel_b:3) instead of treating the
    // function argument record size (12) as the function result shape.
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.angularVelocity2").into(),
            args: vec![Expression::VarRef {
                name: VarName::new("R_b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "when function signature is unavailable, LHS function-call equations should infer from RHS unknown size"
    );
}

#[test]
fn test_infer_scalar_count_function_lhs_skips_rhs_function_arg_records() {
    let mut flat = Model::new();
    for (name, dims) in [
        ("R_a.T", vec![3, 3]),
        ("R_a.w", vec![3]),
        ("R_b.T", vec![3, 3]),
        ("R_b.w", vec![3]),
        ("w_rel_b", vec![3]),
    ] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                dims,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // angularVelocity2(R_b) = resolve2(R_b, angularVelocity1(R_a)) + w_rel_b
    // All function names are intentionally unresolved in flat.functions.
    let lhs = Expression::FunctionCall {
        name: VarName::new("Frames.angularVelocity2").into(),
        args: vec![Expression::VarRef {
            name: VarName::new("R_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }],
        is_constructor: false,
        span: crate::test_support::test_span(),
    };
    let rhs = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Frames.resolve2").into(),
            args: vec![
                Expression::VarRef {
                    name: VarName::new("R_b").into(),
                    subscripts: vec![],
                    span: crate::test_support::test_span(),
                },
                Expression::FunctionCall {
                    name: VarName::new("Frames.angularVelocity1").into(),
                    args: vec![Expression::VarRef {
                        name: VarName::new("R_a").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }],
                    is_constructor: false,
                    span: crate::test_support::test_span(),
                },
            ],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("w_rel_b").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "record-typed arguments inside RHS function calls must not inflate scalar count"
    );
}
