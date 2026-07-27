//! Regression coverage for equation scalar-count inference over record arrays and range subscripts.

use super::*;

#[test]
fn test_infer_equation_scalar_count_connector_field_array_alias() {
    let mut flat = Model::new();
    for name in [
        "pin_n[1].v",
        "pin_n[2].v",
        "pin_n[3].v",
        "plug_n.pin[1].v",
        "plug_n.pin[2].v",
        "plug_n.pin[3].v",
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

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pin_n.v").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::VarRef {
            name: VarName::new("plug_n.pin.v").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "connector-field array alias equation should infer phase count"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_prefix_uses_scalarized_children() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("state"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("state"),
            is_primitive: false,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    for name in ["state.p", "state.T"] {
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

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("state").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::FunctionCall {
            name: VarName::new("Modelica.Media.Common.smoothStep").into(),
            args: vec![],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 2,
        "record prefix equations should count scalarized primitive child fields"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_lhs_uses_full_slice_size() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: Literal::Integer(2),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Record array fields for an array of 2 state records with 5 scalar members.
    for field in ["T", "d", "h", "p", "phase"] {
        flat.add_variable(
            VarName::new(format!("pipe.statesFM.{field}")),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(format!("pipe.statesFM.{field}")),
                dims: vec![2],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // LHS is a range slice over the record array. This should count both
    // selected elements: 2 records * 5 scalars each = 10 equations.
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[1:pipe.n]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 10,
        "record-array range LHS should scale by selected elements, not per-element size"
    );
}

#[test]
fn test_infer_equation_scalar_count_structured_range_subscript_uses_slice_size() {
    let mut flat = Model::new();
    let transformer_i_ref = rumoca_core::ComponentReference {
        local: false,
        span: crate::test_support::test_span(),
        parts: vec![
            rumoca_core::ComponentRefPart {
                ident: "transformerL".to_string(),
                span: crate::test_support::test_span(),
                subs: vec![],
            },
            rumoca_core::ComponentRefPart {
                ident: "i".to_string(),
                span: crate::test_support::test_span(),
                subs: vec![],
            },
        ],
        def_id: None,
    };

    flat.add_variable(
        VarName::new("m"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("m"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(3),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("transformerL.i"),
        flat::Variable {
            name: VarName::new("transformerL.i"),
            component_ref: Some(transformer_i_ref.clone()),
            dims: vec![3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "transformerL.i",
                transformer_i_ref,
            ),
            subscripts: vec![rumoca_core::Subscript::expr(
                Box::new(Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: crate::test_support::test_span(),
                    }),
                    step: None,
                    end: Box::new(Expression::VarRef {
                        name: VarName::new("m").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }),
                crate::test_support::test_span(),
            )],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            args: vec![Expression::VarRef {
                name: VarName::new("m").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }],
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 3,
        "structured range subscripts should count as vector slices"
    );
}

#[test]
fn test_infer_equation_scalar_count_structured_record_range_uses_effective_fields() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(2),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    for index in 1..=2 {
        for field in ["T", "p"] {
            let name = VarName::new(format!("pipe.statesFM[{index}].{field}"));
            flat.add_variable(
                name.clone(),
                crate::test_support::with_component_ref(flat::Variable {
                    name,
                    is_primitive: true,
                    ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ))
                }),
            );
        }
    }

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::Index {
            base: Box::new(Expression::VarRef {
                name: VarName::new("pipe.statesFM").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            subscripts: vec![rumoca_core::Subscript::expr(
                Box::new(Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: crate::test_support::test_span(),
                    }),
                    step: None,
                    end: Box::new(Expression::VarRef {
                        name: VarName::new("pipe.n").into(),
                        subscripts: vec![],
                        span: crate::test_support::test_span(),
                    }),
                    span: crate::test_support::test_span(),
                }),
                crate::test_support::test_span(),
            )],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    assert_eq!(
        infer_equation_scalar_count(&residual, &flat, &prefix_counts),
        4,
        "record-array slices count only effective primitive fields"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_uses_parameter_start_fallback() {
    let mut flat = Model::new();

    // Parameter without explicit binding (value available via start).
    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            start: Some(Expression::Literal {
                value: Literal::Integer(1),
                span: crate::test_support::test_span(),
            }),
            binding: None,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Record array fields for an array of 2 state records with 5 scalar members.
    for field in ["T", "d", "h", "p", "phase"] {
        flat.add_variable(
            VarName::new(format!("pipe.statesFM.{field}")),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(format!("pipe.statesFM.{field}")),
                dims: vec![2],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // 2:(pipe.n + 1) with pipe.n=1 should select one record element.
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[2:(pipe.n + 1)]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 5,
        "record-array range LHS should use evaluable parameter start values for range bounds"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_uses_known_lower_bound_when_upper_is_unknown()
 {
    let mut flat = Model::new();

    // Record array fields for an array of 2 state records with 5 scalar members.
    for field in ["T", "d", "h", "p", "phase"] {
        flat.add_variable(
            VarName::new(format!("pipe.statesFM.{field}")),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(format!("pipe.statesFM.{field}")),
                dims: vec![2],
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    // End bound depends on an unknown symbol. Use the known lower bound and
    // declared dimension to avoid over-counting (2:dim -> one element).
    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[2:(pipe.n + 1)]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 5,
        "record-array range LHS should clamp unknown upper bounds using known lower bounds"
    );
}

#[test]
fn test_infer_equation_scalar_count_record_array_range_with_scalarized_field_indices() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("pipe.n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("pipe.n"),
            is_primitive: true,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(1),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    // Record fields already scalarized into indexed names (dims = []).
    for idx in [1, 2] {
        for field in ["T", "d", "h", "p", "phase"] {
            let name = format!("pipe.statesFM[{idx}].{field}");
            flat.add_variable(
                VarName::new(name.clone()),
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
    }

    let residual = Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: VarName::new("pipe.statesFM[2:(pipe.n + 1)]").into(),
            subscripts: vec![],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };

    let prefix_counts = build_prefix_counts(&flat);
    let scalar_count = infer_equation_scalar_count(&residual, &flat, &prefix_counts);
    assert_eq!(
        scalar_count, 5,
        "record-array range LHS should infer array length from indexed scalarized fields"
    );
}
