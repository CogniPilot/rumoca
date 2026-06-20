use super::{
    BuiltinFunction, ComponentPath, ComponentRefPart, ComponentReference, DefId, Expression,
    Function, FunctionParam, FunctionParamShapeContractError, FunctionShapeContractError, Literal,
    OpBinary, Reference, SourceId, Span, Subscript, VarName, component_path_base_name,
    expressions_semantically_equal, parse_scalar_name, scoped_component_path_candidates,
    split_trailing_subscript_suffix, strip_trailing_subscript_suffix,
};
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};

static INTERNER_STRESS_SEQUENCE: AtomicUsize = AtomicUsize::new(0);

fn test_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ir_primitives_test.mo"), 1, 2)
}

#[test]
fn expression_require_span_accepts_real_span() {
    let span = Span::from_offsets(SourceId(7), 3, 7);
    let expr = Expression::Literal {
        value: Literal::Integer(1),
        span,
    };

    assert_eq!(
        expr.require_span("literal expression")
            .expect("literal span should be accepted")
            .span(),
        span
    );
}

#[test]
fn expression_require_span_rejects_dummy_span() {
    let expr = Expression::Literal {
        value: Literal::Integer(1),
        span: Span::DUMMY,
    };
    let err = expr
        .require_span("literal expression")
        .expect_err("dummy span should be rejected");

    assert_eq!(err.context(), "literal expression");
}

#[test]
fn builtin_function_all_entries_round_trip_by_name() {
    for builtin in BuiltinFunction::ALL {
        assert_eq!(
            BuiltinFunction::from_name(builtin.name()),
            Some(*builtin),
            "{} should parse back to its builtin variant",
            builtin.name()
        );
    }
}

#[test]
fn var_name_reuses_interned_identity_for_equal_text() {
    let first = VarName::new("body.position.x");
    let second = VarName::from(String::from("body.position.x"));
    let third = VarName::new("body.position.y");

    assert_eq!(first, second);
    assert_ne!(first, third);
    assert_eq!(first.id(), second.id());
    assert_ne!(first.id(), third.id());
    assert_eq!(first.as_str(), "body.position.x");
}

#[test]
fn var_name_hashes_by_interned_identity() {
    let first = VarName::new("body.position.x");
    let second = VarName::new("body.position.x");

    let mut first_hash = DefaultHasher::new();
    let mut second_hash = DefaultHasher::new();
    first.hash(&mut first_hash);
    second.hash(&mut second_hash);

    assert_eq!(first_hash.finish(), second_hash.finish());
}

#[test]
fn reference_carries_component_ref_and_target_def_id_without_owning_def_id() {
    let component_ref = ComponentReference {
        local: false,
        span: Span::DUMMY,
        parts: vec![
            ComponentRefPart {
                ident: "body".to_string(),
                span: Span::DUMMY,
                subs: vec![Subscript::generated_index(2, Span::DUMMY)],
            },
            ComponentRefPart {
                ident: "r".to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
            },
        ],
        def_id: Some(DefId::new(42)),
    };
    let reference = Reference::with_component_reference("body[2].r", component_ref.clone());

    assert_eq!(reference.as_str(), "body[2].r");
    assert_eq!(reference.target_def_id(), Some(DefId::new(42)));
    assert_eq!(reference.component_ref(), Some(&component_ref));
    assert_eq!(reference.parts(), component_ref.parts.as_slice());
}

#[test]
fn reference_appended_index_uses_required_owner_provenance() {
    let owner_span = Span::from_offsets(SourceId::from_source_name("append_ref.mo"), 20, 28);
    let component_ref = ComponentReference {
        local: false,
        span: Span::DUMMY,
        parts: vec![ComponentRefPart {
            ident: "body".to_string(),
            span: Span::DUMMY,
            subs: Vec::new(),
        }],
        def_id: Some(DefId::new(42)),
    };
    let reference = Reference::with_component_reference("body", component_ref);

    let indexed = reference.with_appended_index(
        2,
        owner_span
            .require_provenance("test appended index")
            .expect("test span is real"),
    );

    let component_ref = indexed
        .component_ref()
        .expect("appended structured reference keeps component metadata");
    assert_eq!(component_ref.span, Span::DUMMY);
    assert_eq!(component_ref.parts[0].span, Span::DUMMY);
    assert_eq!(
        component_ref.parts[0].subs,
        vec![Subscript::generated_index_with_provenance(
            2,
            owner_span
                .require_provenance("test appended index")
                .expect("test span is real"),
        )]
    );
}

#[test]
fn expression_span_recovers_reference_component_span() {
    let span = Span::from_offsets(SourceId::from_source_name("ref_span.mo"), 12, 18);
    let component_ref = ComponentReference {
        local: false,
        span,
        parts: vec![ComponentRefPart {
            ident: "z".to_string(),
            span,
            subs: Vec::new(),
        }],
        def_id: Some(DefId::new(7)),
    };
    let expr = Expression::VarRef {
        name: Reference::from_component_reference(component_ref),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    assert_eq!(expr.span(), Some(span));
}

#[test]
fn function_param_shape_contract_accepts_zero_dynamic_sentinel() {
    let param = FunctionParam::new("x", "Real", test_span()).with_dims(vec![0, 3]);

    assert_eq!(param.validate_shape_contract(), Ok(()));
}

#[test]
fn function_param_shape_contract_rejects_negative_dims() {
    let span = test_span();
    let param = FunctionParam::new("x", "Real", span).with_dims(vec![2, -1]);

    assert_eq!(
        param.validate_shape_contract(),
        Err(FunctionParamShapeContractError::NegativeDimension {
            param: "x".to_string(),
            dimension: -1,
            span,
        })
    );
}

#[test]
fn function_param_shape_contract_rejects_missing_type() {
    let span = test_span();
    let param = FunctionParam::new("x", "", span);

    assert_eq!(
        param.validate_shape_contract(),
        Err(FunctionParamShapeContractError::EmptyTypeName {
            param: "x".to_string(),
            span,
        })
    );
}

#[test]
fn function_param_shape_contract_rejects_mismatched_shape_expr() {
    let span = test_span();
    let param = FunctionParam::new("x", "Real", span)
        .with_dims(vec![0, 3])
        .with_shape_expr(vec![Subscript::colon(Span::DUMMY)]);

    assert_eq!(
        param.validate_shape_contract(),
        Err(FunctionParamShapeContractError::ShapeExprLengthMismatch {
            param: "x".to_string(),
            dims: 2,
            shape_expr: 1,
            span,
        })
    );
}

#[test]
fn function_param_shape_contract_rejects_negative_shape_index() {
    let span = test_span();
    let param = FunctionParam::new("x", "Real", span)
        .with_dims(vec![0])
        .with_shape_expr(vec![Subscript::generated_index(-1, Span::DUMMY)]);

    assert_eq!(
        param.validate_shape_contract(),
        Err(FunctionParamShapeContractError::NegativeShapeIndex {
            param: "x".to_string(),
            index: -1,
            span,
        })
    );
}

#[test]
fn function_shape_contract_reports_bad_local_param() {
    let span = test_span();
    let mut function = Function::new("Pkg.f", Span::DUMMY);
    function.add_local(FunctionParam::new("tmp", "Real", span).with_dims(vec![-1]));

    assert_eq!(
        function.validate_shape_contract(),
        Err(FunctionShapeContractError::Param {
            function: VarName::new("Pkg.f"),
            source: FunctionParamShapeContractError::NegativeDimension {
                param: "tmp".to_string(),
                dimension: -1,
                span,
            },
        })
    );
}

#[test]
fn function_param_shape_contract_error_displays_reason() {
    let error = FunctionParamShapeContractError::NegativeDimension {
        param: "tmp".to_string(),
        dimension: -1,
        span: Span::DUMMY,
    };

    assert_eq!(
        error.to_string(),
        "function parameter `tmp` has negative dimension -1"
    );
}

#[test]
fn function_shape_contract_error_displays_nested_reason() {
    let error = FunctionShapeContractError::Param {
        function: VarName::new("Pkg.f"),
        source: FunctionParamShapeContractError::NegativeShapeIndex {
            param: "tmp".to_string(),
            index: -1,
            span: Span::DUMMY,
        },
    };

    assert_eq!(
        error.to_string(),
        "function `Pkg.f` parameter shape contract failed: \
         function parameter `tmp` has negative shape index -1"
    );
    assert_eq!(
        std::error::Error::source(&error).map(ToString::to_string),
        Some("function parameter `tmp` has negative shape index -1".to_string())
    );
}

#[test]
fn component_path_candidates_walk_parent_scopes_without_subscript_dot_split() {
    let name = ComponentPath::from_flat_path("value");
    let scope = ComponentPath::from_flat_path("pkg.arr[data.medium]");

    assert_eq!(
        scoped_component_path_candidates(&name, &scope),
        vec![
            "pkg.arr[data.medium].value".to_string(),
            "pkg.value".to_string(),
            "value".to_string(),
        ]
    );
}

#[test]
fn component_path_preserves_component_reference_subscripts() {
    let component_ref = ComponentReference {
        local: false,
        span: Span::DUMMY,
        parts: vec![
            ComponentRefPart {
                ident: "body".to_string(),
                span: Span::DUMMY,
                subs: vec![Subscript::generated_index(2, Span::DUMMY)],
            },
            ComponentRefPart {
                ident: "r".to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
            },
        ],
        def_id: Some(DefId::new(42)),
    };

    assert_eq!(
        ComponentPath::from_component_reference(&component_ref).to_flat_string(),
        "body[2].r"
    );
    assert_eq!(component_ref.to_var_name().as_str(), "body[2].r");
}

#[test]
fn component_path_from_parts_preserves_presegmented_subscripts() {
    let path = ComponentPath::from_parts(["stackData", "cellData[1,2]", "nRC"]);

    assert_eq!(path.to_flat_string(), "stackData.cellData[1,2].nRC");
    assert_eq!(path.parts(), &["stackData", "cellData[1,2]", "nRC"]);
}

#[test]
fn component_path_supports_borrowed_part_slice_lookup() {
    let mut paths = HashMap::new();
    paths.insert(
        ComponentPath::from_flat_path("stack.cellData"),
        ComponentPath::from_flat_path("cellData"),
    );

    let current = ComponentPath::from_flat_path("stack.cellData.nRC");

    assert_eq!(
        paths
            .get(&current.parts()[..2])
            .map(ComponentPath::to_flat_string),
        Some("cellData".to_string())
    );
}

#[test]
fn component_path_joins_part_slice_without_intermediate_path() {
    let target = ComponentPath::from_flat_path("cellData");
    let current = ComponentPath::from_flat_path("stack.cellData.nRC");

    assert_eq!(
        target
            .join_part_slice(&current.parts()[2..])
            .to_flat_string(),
        "cellData.nRC"
    );
}

#[test]
fn var_name_interner_deduplicates_repeated_workspace_reopens() {
    let names = [
        "Workspace.Model.x",
        "Workspace.Model.der_x",
        "Workspace.Model.subsystem.y",
        "Workspace.Model.subsystem.parameter",
    ];
    let expected_ids = names.map(|name| VarName::new(name).id());

    for _ in 0..1_000 {
        for (name, expected_id) in names.iter().copied().zip(expected_ids) {
            assert_eq!(VarName::new(name).id(), expected_id);
        }
    }
}

#[test]
fn var_name_interner_retains_unique_names_for_process_lifetime() {
    let sequence = INTERNER_STRESS_SEQUENCE.fetch_add(1, Ordering::Relaxed);
    let prefix = format!("__rumoca_interner_lifecycle_{sequence}_");
    let names = (0..64)
        .map(|idx| format!("{prefix}{idx}"))
        .collect::<Vec<_>>();
    let ids = names
        .iter()
        .map(|name| VarName::new(name.clone()).id())
        .collect::<Vec<_>>();

    for (name, expected_id) in names.iter().zip(ids) {
        assert_eq!(
            VarName::new(name.clone()).id(),
            expected_id,
            "unique VarName text should retain its process-local id"
        );
    }
}

#[test]
fn expression_semantic_equality_ignores_spans() {
    let lhs = Expression::Binary {
        op: OpBinary::Gt,
        lhs: Box::new(Expression::VarRef {
            name: Reference::new("x"),
            subscripts: vec![],
            span: Span::DUMMY,
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Real(0.0),
            span: Span::DUMMY,
        }),
        span: Span::from_offsets(super::SourceId(1), 1, 2),
    };
    let rhs = lhs
        .clone()
        .with_span(Span::from_offsets(super::SourceId(1), 3, 4));

    assert!(expressions_semantically_equal(&lhs, &rhs));
    assert!(lhs.semantically_eq_ignoring_spans(&rhs));
}

#[test]
fn scalar_name_parser_rejects_non_integer_indices() {
    let parsed = parse_scalar_name("x[1, 2]").expect("valid scalar name");
    assert_eq!(parsed.base, "x");
    assert_eq!(parsed.indices, vec![1, 2]);
    assert!(parse_scalar_name("x[1.0]").is_none());
    assert!(parse_scalar_name("x[i]").is_none());
    assert!(parse_scalar_name("x").is_none());
    assert!(parse_scalar_name("x][1]").is_none());
    assert!(parse_scalar_name("x[1]][2]").is_none());
    assert!(parse_scalar_name("record_array[1].field[2]").is_some());
}

#[test]
fn trailing_subscript_strip_accepts_symbolic_range_suffixes() {
    assert_eq!(strip_trailing_subscript_suffix("x[2:n]"), Some("x"));
    assert_eq!(strip_trailing_subscript_suffix("x[1:(nx - 1)]"), Some("x"));
    assert_eq!(strip_trailing_subscript_suffix("rmsvM[1].mean.x"), None);
    assert_eq!(
        strip_trailing_subscript_suffix("record_array[1].field[2]"),
        Some("record_array[1].field")
    );
    assert_eq!(strip_trailing_subscript_suffix("x[]"), None);
}

#[test]
fn trailing_subscript_split_uses_balanced_final_group() {
    assert_eq!(
        split_trailing_subscript_suffix("a.b[2]"),
        Some(("a.b", "2"))
    );
    assert_eq!(
        split_trailing_subscript_suffix("a[b.c].d[1, 2]"),
        Some(("a[b.c].d", "1, 2"))
    );
    assert_eq!(
        split_trailing_subscript_suffix("a[f(b[1])]"),
        Some(("a", "f(b[1])"))
    );
    assert_eq!(split_trailing_subscript_suffix("a.b"), None);
    assert_eq!(split_trailing_subscript_suffix("a.b[2"), None);
    assert_eq!(split_trailing_subscript_suffix("[2]"), None);
    assert_eq!(split_trailing_subscript_suffix("x][1]"), None);
}

#[test]
fn component_path_base_name_strips_balanced_subscripts_only() {
    assert_eq!(
        component_path_base_name("bus[data.medium].pin[1].v"),
        Some("bus.pin.v".to_string())
    );
    assert_eq!(
        component_path_base_name("record_array[1].field[2]"),
        Some("record_array.field".to_string())
    );
    assert_eq!(component_path_base_name("a.b"), Some("a.b".to_string()));
    assert_eq!(component_path_base_name("a..b"), None);
    assert_eq!(component_path_base_name(".a"), None);
    assert_eq!(component_path_base_name("a."), None);
    assert_eq!(component_path_base_name("a[1"), None);
    assert_eq!(component_path_base_name("a]"), None);
    assert_eq!(component_path_base_name("[1].a"), None);
}
