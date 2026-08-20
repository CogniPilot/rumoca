use super::{
    BuiltinFunction, ComponentPath, ComponentRefPart, ComponentReference, ComponentReferenceError,
    DefId, Expression, Function, FunctionInstanceId, FunctionParam,
    FunctionParamShapeContractError, FunctionShapeContractError, InstanceId, Literal, OpBinary,
    PRE_SLOT_NAMESPACE, Reference, ResolvedFunctionReference, SourceId, Span, Subscript, TypeId,
    VarName, component_path_base_name, component_path_trailing_index,
    expression_semantic_fingerprint, expressions_semantically_equal,
    flat_expression_component_path, is_pre_slot, parse_scalar_name, pre_slot_base, pre_slot_name,
    scoped_component_path_candidates, split_trailing_subscript_suffix,
    strip_trailing_subscript_suffix,
};
use crate::{EffectiveType, EffectiveTypeError};
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicUsize, Ordering};

static INTERNER_STRESS_SEQUENCE: AtomicUsize = AtomicUsize::new(0);

fn test_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ir_primitives_test.mo"), 1, 2)
}

fn real_value_type(dimensions: Vec<i64>) -> EffectiveType {
    EffectiveType::new(TypeId::new(11), TypeId::new(1), dimensions)
        .expect("fixture function type is resolved")
}

#[test]
fn default_instance_identity_is_the_reserved_unset_value() {
    assert_eq!(InstanceId::default(), InstanceId::UNSET);
    assert!(InstanceId::default().is_unset());
    assert!(!InstanceId::new(1).is_unset());
    assert_eq!(InstanceId::UNSET.index(), 0);
}

#[test]
fn expression_require_span_accepts_real_span() {
    let span = Span::from_offsets(
        SourceId::from_source_name("core_ir_primitives_source_7.mo"),
        3,
        7,
    );
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
fn flat_expression_component_path_preserves_projected_indices() {
    let span = test_span();
    let expression = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(Expression::FieldAccess {
                base: Box::new(Expression::VarRef {
                    name: Reference::new("stack"),
                    subscripts: Vec::new(),
                    span,
                }),
                field: "cellData".to_string(),
                field_def_id: DefId::new(2),
                span,
            }),
            subscripts: vec![Subscript::index(1, span), Subscript::index(2, span)],
            span,
        }),
        field: "nRC".to_string(),
        field_def_id: DefId::new(3),
        span,
    };

    assert_eq!(
        flat_expression_component_path(&expression)
            .expect("projected expression should be path-shaped")
            .to_flat_string(),
        "stack.cellData[1,2].nRC"
    );
}

#[test]
fn builtin_function_all_entries_round_trip_by_name() {
    for builtin in BuiltinFunction::ALL {
        if builtin.requires_predefined_identity() {
            assert_eq!(BuiltinFunction::from_name(builtin.name()), None);
            continue;
        }
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
    let component_ref = ComponentReference::construct(
        false,
        Span::DUMMY,
        vec![
            ComponentRefPart {
                ident: "body".to_string(),
                span: Span::DUMMY,
                subs: vec![Subscript::generated_index(2, Span::DUMMY)],
                def_id: DefId::new(7),
            },
            ComponentRefPart {
                ident: "r".to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
                def_id: DefId::new(42),
            },
        ],
    )
    .expect("test reference is nonempty");
    let reference = Reference::with_component_reference("body[2].r", component_ref.clone());

    assert_eq!(reference.as_str(), "body[2].r");
    assert_eq!(reference.root_def_id(), Some(DefId::new(7)));
    assert_eq!(reference.target_def_id(), Some(DefId::new(42)));
    assert_eq!(reference.component_ref(), Some(&component_ref));
    assert_eq!(reference.parts(), component_ref.parts());
}

#[test]
fn reference_appended_index_uses_required_owner_provenance() {
    let owner_span = Span::from_offsets(SourceId::from_source_name("append_ref.mo"), 20, 28);
    let component_ref = ComponentReference::construct(
        false,
        Span::DUMMY,
        vec![ComponentRefPart {
            ident: "body".to_string(),
            span: Span::DUMMY,
            subs: Vec::new(),
            def_id: DefId::new(42),
        }],
    )
    .expect("test reference is nonempty");
    let function = ResolvedFunctionReference {
        instance_id: FunctionInstanceId::new(8),
        base_part_count: 1,
        transitively_non_replaceable: false,
    };
    let reference = Reference::with_component_reference("body", component_ref)
        .with_instance_id(InstanceId::new(9))
        .with_resolved_function(function);

    let indexed = reference.with_appended_index(
        2,
        owner_span
            .require_provenance("test appended index")
            .expect("test span is real"),
    );

    let component_ref = indexed
        .component_ref()
        .expect("appended structured reference keeps component metadata");
    assert_eq!(component_ref.span(), Span::DUMMY);
    assert_eq!(component_ref.parts()[0].span, Span::DUMMY);
    assert_eq!(
        component_ref.parts()[0].subs,
        vec![Subscript::generated_index_with_provenance(
            2,
            owner_span
                .require_provenance("test appended index")
                .expect("test span is real"),
        )]
    );
    assert_eq!(indexed.target_def_id(), Some(DefId::new(42)));
    assert_eq!(indexed.instance_id(), Some(InstanceId::new(9)));
    assert_eq!(indexed.resolved_function(), Some(function));
}

#[test]
fn appended_field_cannot_inherit_the_base_exact_target() {
    let root_span = Span::from_offsets(SourceId::from_source_name("field.mo"), 1, 5);
    let base_span = Span::from_offsets(SourceId::from_source_name("field.mo"), 6, 12);
    let field_span = Span::from_offsets(SourceId::from_source_name("field.mo"), 13, 18);
    let function = ResolvedFunctionReference {
        instance_id: FunctionInstanceId::new(8),
        base_part_count: 2,
        transitively_non_replaceable: false,
    };
    let reference = Reference::from_component_reference(
        ComponentReference::construct(
            false,
            root_span,
            vec![
                ComponentRefPart {
                    ident: "owner".to_string(),
                    span: root_span,
                    subs: Vec::new(),
                    def_id: DefId::new(7),
                },
                ComponentRefPart {
                    ident: "record".to_string(),
                    span: base_span,
                    subs: Vec::new(),
                    def_id: DefId::new(8),
                },
            ],
        )
        .expect("test reference is nonempty"),
    )
    .with_instance_id(InstanceId::new(9))
    .with_resolved_function(function);

    let field = reference
        .with_appended_field(
            "value",
            DefId::new(42),
            field_span
                .require_provenance("test record field")
                .expect("test span is real"),
        )
        .expect("structured field projection is valid");

    assert_eq!(field.root_def_id(), Some(DefId::new(7)));
    assert_eq!(
        field.target_def_id(),
        Some(DefId::new(42)),
        "the appended member carries its own exact declaration identity"
    );
    let parts = field
        .component_ref()
        .expect("projection remains structured")
        .parts();
    assert_eq!(parts[0].span, root_span);
    assert_eq!(parts[1].span, base_span);
    assert_eq!(parts[2].span, field_span);
    assert_eq!(field.instance_id(), Some(InstanceId::new(9)));
    assert_eq!(field.resolved_function(), None);

    assert_eq!(
        Reference::new("record")
            .with_appended_field(
                "value",
                DefId::new(42),
                field_span
                    .require_provenance("test record field")
                    .expect("test span is real"),
            )
            .expect_err("an exact projection cannot degrade to a rendered reference"),
        ComponentReferenceError::MissingStructuredBase,
    );
    assert_eq!(
        reference
            .with_appended_field(
                "value",
                DefId::new(0),
                field_span
                    .require_provenance("test record field")
                    .expect("test span is real"),
            )
            .expect_err("an unresolved field identity must fail"),
        ComponentReferenceError::MissingPartIdentity { part_index: 2 },
    );
}

#[test]
fn expression_span_recovers_reference_component_span() {
    let span = Span::from_offsets(SourceId::from_source_name("ref_span.mo"), 12, 18);
    let component_ref = ComponentReference::construct(
        false,
        span,
        vec![ComponentRefPart {
            ident: "z".to_string(),
            span,
            subs: Vec::new(),
            def_id: DefId::new(7),
        }],
    )
    .expect("test reference is nonempty");
    let expr = Expression::VarRef {
        name: Reference::from_component_reference(component_ref),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    assert_eq!(expr.span(), Some(span));
}

#[test]
fn function_param_shape_contract_accepts_zero_dynamic_sentinel() {
    let param = FunctionParam::new("x", "Real", real_value_type(vec![0, 3]), test_span());

    assert_eq!(param.validate_shape_contract(), Ok(()));
}

#[test]
fn effective_function_type_rejects_negative_dims_at_construction() {
    assert_eq!(
        EffectiveType::new(TypeId::new(11), TypeId::new(1), vec![2, -1]),
        Err(EffectiveTypeError::NegativeExtent)
    );
}

#[test]
fn function_param_shape_contract_rejects_missing_type() {
    let span = test_span();
    let param = FunctionParam::new("x", "", real_value_type(Vec::new()), span);

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
    let param = FunctionParam::new("x", "Real", real_value_type(vec![0, 3]), span)
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
    let param = FunctionParam::new("x", "Real", real_value_type(vec![0]), span)
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
    function.add_local(
        FunctionParam::new("tmp", "Real", real_value_type(vec![0]), span)
            .with_shape_expr(vec![Subscript::generated_index(-1, Span::DUMMY)]),
    );

    assert_eq!(
        function.validate_shape_contract(),
        Err(FunctionShapeContractError::Param {
            function: VarName::new("Pkg.f"),
            source: FunctionParamShapeContractError::NegativeShapeIndex {
                param: "tmp".to_string(),
                index: -1,
                span,
            },
        })
    );
}

#[test]
fn function_param_shape_contract_error_displays_reason() {
    let error = FunctionParamShapeContractError::NegativeShapeIndex {
        param: "tmp".to_string(),
        index: -1,
        span: Span::DUMMY,
    };

    assert_eq!(
        error.to_string(),
        "function parameter `tmp` has negative shape index -1"
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
    let component_ref = ComponentReference::construct(
        false,
        Span::DUMMY,
        vec![
            ComponentRefPart {
                ident: "body".to_string(),
                span: Span::DUMMY,
                subs: vec![Subscript::generated_index(2, Span::DUMMY)],
                def_id: DefId::new(42),
            },
            ComponentRefPart {
                ident: "r".to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
                def_id: DefId::new(43),
            },
        ],
    )
    .expect("test reference is nonempty");

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
fn component_path_supports_prefix_identity_lookup() {
    let mut paths = HashMap::new();
    paths.insert(
        ComponentPath::from_flat_path("stack.cellData"),
        ComponentPath::from_flat_path("cellData"),
    );

    let current = ComponentPath::from_flat_path("stack.cellData.nRC");
    let prefix = current.prefix(2).expect("two-segment prefix should exist");

    assert_eq!(
        paths.get(&prefix).map(ComponentPath::to_flat_string),
        Some("cellData".to_string())
    );
}

/// `ComponentPath` is a live `HashMap`/`HashSet` key, so its `Hash` must not
/// reach the `Vec<String>` payload. There is no way to observe "which bytes
/// went into the hasher" from outside, so this pins the mechanism: the path's
/// hash is exactly the hash of the interned name it already carries.
#[test]
fn component_path_hashes_its_interned_identity_not_its_parts() {
    let path = ComponentPath::from_flat_path("stack.cellData[2].nRC");

    let mut from_path = DefaultHasher::new();
    path.hash(&mut from_path);
    let mut from_identity = DefaultHasher::new();
    VarName::new("stack.cellData[2].nRC").hash(&mut from_identity);

    assert_eq!(
        from_path.finish(),
        from_identity.finish(),
        "ComponentPath must hash its interned identity; hashing `parts` walks \
         every segment of a flattened path on every map probe"
    );
    assert_ne!(
        path.parts(),
        &[path.as_str().to_string()],
        "the payload this test guards against hashing must really be segmented"
    );
}

/// Equal paths must still land in the same bucket after the switch from
/// `parts`-hashing to identity-hashing.
#[test]
fn component_path_equal_paths_agree_on_hash() {
    let from_flat = ComponentPath::from_flat_path("stack.cellData.nRC");
    let from_parts = ComponentPath::from_parts(["stack", "cellData", "nRC"]);

    assert_eq!(from_flat, from_parts);

    let mut flat_hash = DefaultHasher::new();
    from_flat.hash(&mut flat_hash);
    let mut parts_hash = DefaultHasher::new();
    from_parts.hash(&mut parts_hash);
    assert_eq!(flat_hash.finish(), parts_hash.finish());
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
        span: Span::from_offsets(
            super::SourceId::from_source_name("core_ir_primitives_source_1.mo"),
            1,
            2,
        ),
    };
    let rhs = lhs.clone().with_span(Span::from_offsets(
        super::SourceId::from_source_name("core_ir_primitives_source_1.mo"),
        3,
        4,
    ));

    assert!(expressions_semantically_equal(&lhs, &rhs));
    assert!(lhs.semantically_eq_ignoring_spans(&rhs));
    assert_eq!(
        expression_semantic_fingerprint(&lhs),
        expression_semantic_fingerprint(&rhs),
        "semantic fingerprints must ignore source-only spans like equality does"
    );
}

/// A structured reference to `resistor.v` carrying the given declaration id.
fn declaration_reference(def_id: DefId, span: Span) -> Reference {
    let part = |ident: &str, part_def_id: DefId| ComponentRefPart {
        ident: ident.to_string(),
        span,
        subs: Vec::new(),
        def_id: part_def_id,
    };
    Reference::with_component_reference(
        "resistor.v",
        ComponentReference::construct(
            false,
            span,
            vec![part("resistor", DefId::new(10)), part("v", def_id)],
        )
        .expect("test reference is nonempty"),
    )
}

fn var_ref(name: Reference) -> Expression {
    Expression::VarRef {
        name,
        subscripts: vec![],
        span: Span::DUMMY,
    }
}

/// Two references that denote the same flat variable fingerprint equal however
/// much declaration provenance they carry. A `DefId` names a *declaration*,
/// and one declaration
/// backs many flat variables (`phase-flatten`'s `DefIdVarRefIndex` keeps a
/// `Vec` per `DefId` precisely because the mapping is one-to-many), so a
/// def-id-keyed fingerprint would equate `resistor1.v` with `resistor2.v` and
/// contradict `expressions_semantically_equal`. The identity that *is*
/// one-to-one with a flat variable is the interned `VarName`, and that is what
/// both equality and this fingerprint use.
#[test]
fn fingerprint_is_stable_across_declaration_identity() {
    let span = test_span();
    let resolved = var_ref(declaration_reference(DefId::new(11), span));
    let other_declaration = var_ref(declaration_reference(DefId::new(12), span));
    let bare = var_ref(Reference::new("resistor.v"));

    for candidate in [&other_declaration, &bare] {
        assert!(
            expressions_semantically_equal(&resolved, candidate),
            "same flat variable must stay semantically equal: {candidate:?}"
        );
        assert_eq!(
            expression_semantic_fingerprint(&resolved),
            expression_semantic_fingerprint(candidate),
            "equal expressions must fingerprint equal, or fingerprint-bucketed \
duplicate-equation removal would drop a real duplicate: {candidate:?}"
        );
    }
}

/// The `VarRef` arm hashes the interned identity, not the rendered spelling.
///
/// The property above cannot catch a revert to `name.as_str().hash(hasher)`:
/// the interner is a bijection, so text-hashing and id-hashing induce the same
/// partition and every equality-agreement property holds under both. What
/// differs is the mechanism — id-hashing writes the `VarNameId` a `VarName`
/// already carries, text-hashing walks the flattened path byte by byte on every
/// fingerprint — and a mechanism is only pinned by recomputing it.
///
/// So: rebuild the expected value from the interned id alone. Any arm that
/// reaches past the id to the bytes (or to `component_ref`, or to `def_id`)
/// produces a different digest and fails here. Nothing platform- or
/// order-dependent is asserted: no hash value is written down, the expected
/// value is derived from the same interner in the same process.
#[test]
fn var_ref_fingerprint_hashes_the_interned_id_not_the_spelling() {
    let name = "resistor.v";
    let expr = Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    };

    let mut expected = DefaultHasher::new();
    std::mem::discriminant(&expr).hash(&mut expected);
    VarName::new(name).id().hash(&mut expected);
    0usize.hash(&mut expected);

    assert_eq!(
        expression_semantic_fingerprint(&expr),
        expected.finish(),
        "the VarRef fingerprint must be the interned `VarNameId`, the empty \
subscript list and nothing else; hashing `name.as_str()` reaches past the id \
the interner exists to provide, and hashing `component_ref`/`def_id` would \
disagree with `expressions_semantically_equal`, which ignores both"
    );
}

/// Two calls that *render* identically but resolve to different function
/// instances fingerprint differently.
///
/// `FunctionInstanceId` distinguishes inherited and redeclared instances of one
/// declaration, so these two calls are not equal — yet hashing the rendered
/// name collapsed them into one bucket. Hashing the resolved instance id splits
/// them, which is safe in the direction that matters: equality compares the
/// whole `Reference`, so anything that is equal still hashes equal.
#[test]
fn fingerprint_separates_distinct_function_instances_that_render_alike() {
    let callee = |instance: u32| {
        Reference::new("Medium.density").with_resolved_function(super::ResolvedFunctionReference {
            instance_id: super::FunctionInstanceId::new(instance),
            base_part_count: 1,
            transitively_non_replaceable: false,
        })
    };
    let call = |instance: u32| Expression::FunctionCall {
        name: callee(instance),
        args: vec![],
        is_constructor: false,
        span: Span::DUMMY,
    };
    let inherited = call(1);
    let redeclared = call(2);

    assert_eq!(
        callee(1).as_str(),
        callee(2).as_str(),
        "the two calls must render alike for this test to mean anything"
    );
    assert!(!expressions_semantically_equal(&inherited, &redeclared));
    assert_ne!(
        expression_semantic_fingerprint(&inherited),
        expression_semantic_fingerprint(&redeclared),
        "distinct resolved function instances must not share a fingerprint bucket"
    );
    assert_eq!(
        expression_semantic_fingerprint(&inherited),
        expression_semantic_fingerprint(&call(1)),
        "the same resolved instance must fingerprint stably"
    );
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

#[test]
fn component_path_trailing_index_accepts_single_trailing_literal_index() {
    assert_eq!(
        component_path_trailing_index("c[3]"),
        Some(("c".to_string(), 3))
    );
    assert_eq!(
        component_path_trailing_index("a.b[2]"),
        Some(("a.b".to_string(), 2))
    );
    assert_eq!(
        component_path_trailing_index("__pre__.c[1]"),
        Some(("__pre__.c".to_string(), 1))
    );
    assert_eq!(
        component_path_trailing_index("Modelica.X.'1'[4]"),
        Some(("Modelica.X.'1'".to_string(), 4))
    );
}

#[test]
fn component_path_trailing_index_rejects_non_positive_and_non_numeric_indices() {
    assert_eq!(component_path_trailing_index("c[0]"), None);
    assert_eq!(component_path_trailing_index("c[-1]"), None);
    assert_eq!(component_path_trailing_index("c[i]"), None);
}

#[test]
fn component_path_trailing_index_rejects_mid_path_indices() {
    assert_eq!(component_path_trailing_index("x[2].y"), None);
    assert_eq!(component_path_trailing_index("x[2].y[3]"), None);
    assert_eq!(
        component_path_trailing_index("bus[data.medium].pin[1]"),
        None
    );
}

#[test]
fn component_path_trailing_index_rejects_multi_index_and_missing_subscript() {
    assert_eq!(component_path_trailing_index("c[1,2]"), None);
    assert_eq!(component_path_trailing_index("c[1][2]"), None);
    assert_eq!(component_path_trailing_index("c"), None);
    assert_eq!(component_path_trailing_index("a..b[2]"), None);
    assert_eq!(component_path_trailing_index(".a[2]"), None);
    assert_eq!(component_path_trailing_index("[1]"), None);
}

#[test]
fn pre_slot_name_round_trips_through_pre_slot_base() {
    for base in ["x", "sampled.u", "Modelica.X.'1'", "c[3]"] {
        let slot = pre_slot_name(base);
        assert_eq!(pre_slot_base(slot.as_str()), Some(base), "{base}");
        assert!(is_pre_slot(slot.as_str()), "{base}");
    }
}

#[test]
fn pre_slot_name_pins_rendered_convention() {
    assert_eq!(PRE_SLOT_NAMESPACE, "__pre__");
    assert_eq!(pre_slot_name("x").as_str(), "__pre__.x");
    assert_eq!(pre_slot_name("sampled.u").as_str(), "__pre__.sampled.u");
}

#[test]
fn pre_slot_base_strips_exactly_one_namespace_level() {
    let nested = pre_slot_name("__pre__.x");
    assert_eq!(nested.as_str(), "__pre__.__pre__.x");
    assert_eq!(pre_slot_base(nested.as_str()), Some("__pre__.x"));
}

#[test]
fn pre_slot_detection_rejects_non_pre_names() {
    for name in ["x", "pre.x", "__pre__", "__pre__x", "prefix.__pre__.x", ""] {
        assert_eq!(pre_slot_base(name), None, "{name}");
        assert!(!is_pre_slot(name), "{name}");
    }
}

// ---------------------------------------------------------------------------
// Location source identity (P4 parser allocation work)
// ---------------------------------------------------------------------------

#[test]
fn location_is_allocation_free() {
    // `Location` must stay a plain POD struct: six `u32` plus a `SourceId`.
    // Re-introducing an owned `String`/`Vec` field would put a heap allocation
    // back into every parser token, which is exactly what P4 removed.
    assert_eq!(std::mem::size_of::<super::Location>(), 32);
    assert_eq!(std::mem::size_of::<super::Token>(), 56);
    assert!(!std::mem::needs_drop::<super::Location>());
}

#[test]
fn location_span_matches_manual_offsets() {
    let source = SourceId::from_source_name("A.mo");
    let location = super::Location {
        start: 4,
        end: 9,
        source,
        ..Default::default()
    };
    assert_eq!(location.span(), Span::from_offsets(source, 4, 9));
    assert!(location.has_source());
}

#[test]
fn default_location_has_no_source() {
    let location = super::Location::default();
    assert_eq!(location.source, SourceId::DUMMY);
    assert!(!location.has_source());
}

#[test]
fn location_with_empty_range_has_no_source() {
    let location = super::Location {
        start: 7,
        end: 7,
        source: SourceId::from_source_name("A.mo"),
        ..Default::default()
    };
    assert!(!location.has_source());
}

#[test]
fn merged_location_takes_start_from_self_and_end_from_other() {
    let source = SourceId::from_source_name("Merge.mo");
    let start = super::Location {
        start_line: 3,
        start_column: 5,
        end_line: 3,
        end_column: 7,
        start: 40,
        end: 42,
        source,
    };
    let end = super::Location {
        start_line: 9,
        start_column: 1,
        end_line: 9,
        end_column: 4,
        start: 100,
        end: 103,
        source,
    };
    let merged = start.merged_with(&end);
    assert_eq!(merged.start_line, 3);
    assert_eq!(merged.start_column, 5);
    assert_eq!(merged.end_line, 9);
    assert_eq!(merged.end_column, 4);
    assert_eq!(merged.start, 40);
    assert_eq!(merged.end, 103);
    assert_eq!(merged.source, source);
}
