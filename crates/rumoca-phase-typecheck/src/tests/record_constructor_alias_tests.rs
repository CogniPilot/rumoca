use super::*;

#[test]
fn test_user_defined_record_constructor_mismatch_detection() {
    let source = r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;

        model Test
            LeftPayload lhs;
        equation
            lhs = RightPayload(x = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_err(),
        "record constructor of different type should be rejected"
    );

    let diags = result.expect_err("expected diagnostics");
    assert!(
        diags
            .iter()
            .any(|d| d.code.as_deref() == Some("ET002") && d.message.contains("type mismatch")),
        "expected ET002 diagnostic for mismatched record constructor assignment"
    );
}

#[test]
fn test_user_defined_record_constructor_compatibility() {
    let source = r#"
        record Payload
            Real x;
        end Payload;

        model Test
            Payload lhs;
        equation
            lhs = Payload(x = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "record constructor with matching type should remain compatible"
    );
}

#[test]
fn test_record_wrapper_constructor_assignment_is_compatible() {
    let source = r#"
        record BasePayload
            Real x;
        end BasePayload;

        record WrappedPayload = BasePayload;

        model Test
            WrappedPayload lhs;
        equation
            lhs = BasePayload(x = 1.0);
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let result = typecheck(resolved);
    assert!(
        result.is_ok(),
        "record wrapper should be assignment-compatible with its base constructor"
    );
}

#[test]
fn test_alias_field_key_range_matches_only_target_prefix() {
    let mut sorted_keys = vec![
        "root.src.fieldA".to_string(),
        "root.src.fieldB".to_string(),
        "root.src2.fieldC".to_string(),
        "root.target.fieldD".to_string(),
    ];
    sorted_keys.sort_unstable();

    let matched = TypeChecker::alias_field_key_range(&sorted_keys, "root.src.");
    let matched: Vec<&str> = matched.iter().map(String::as_str).collect();

    assert_eq!(matched, vec!["root.src.fieldA", "root.src.fieldB"]);
}

#[test]
fn test_propagate_alias_map_copies_root_and_prefixed_fields() {
    let aliases = vec![("dst".to_string(), "src".to_string())];
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("src".to_string(), 1);
    values.insert("src.nX".to_string(), 2);
    values.insert("src.nXi".to_string(), 3);
    values.insert("src2.nX".to_string(), 99);

    let progress = TypeChecker::propagate_alias_map(&aliases, &mut values);

    assert!(progress);
    assert_eq!(values.get("dst"), Some(&1));
    assert_eq!(values.get("dst.nX"), Some(&2));
    assert_eq!(values.get("dst.nXi"), Some(&3));
    assert_eq!(values.get("dst2.nX"), None);
}

#[test]
fn test_extract_simple_path_preserves_subscripted_component_refs() {
    let expr = Expression::ComponentReference(ComponentReference {
        local: false,
        qualified_display_name: None,
        parts: vec![
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("stackData"),
                    location: Default::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
                def_id: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("cellData"),
                    location: Default::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: Some(vec![
                    Subscript::Expression(Expression::Terminal {
                        terminal_type: TerminalType::UnsignedInteger,
                        token: Token {
                            text: Arc::from("1"),
                            location: Default::default(),
                            token_number: 0,
                            token_type: 0,
                        },
                        span: rumoca_core::Span::DUMMY,
                    }),
                    Subscript::Expression(Expression::Terminal {
                        terminal_type: TerminalType::UnsignedInteger,
                        token: Token {
                            text: Arc::from("2"),
                            location: Default::default(),
                            token_number: 0,
                            token_type: 0,
                        },
                        span: rumoca_core::Span::DUMMY,
                    }),
                ]),
                def_id: None,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    });

    assert_eq!(
        TypeChecker::extract_simple_path(&expr).map(|path| path.to_flat_string()),
        Some("stackData.cellData[1,2]".to_string())
    );
}

#[test]
fn test_propagate_alias_map_copies_indexed_record_fields() {
    let aliases = vec![(
        "dst.cell[1].cellData".to_string(),
        "src.stackData.cellData[1,1]".to_string(),
    )];
    let mut values: rustc_hash::FxHashMap<String, Vec<usize>> = rustc_hash::FxHashMap::default();
    values.insert(
        "src.stackData.cellData[1,1].OCV_SOC".to_string(),
        vec![29, 2],
    );

    let progress = TypeChecker::propagate_alias_map(&aliases, &mut values);

    assert!(progress);
    assert_eq!(
        values.get("dst.cell[1].cellData.OCV_SOC"),
        Some(&vec![29, 2])
    );
}

#[test]
fn instance_identity_scope_keeps_subscript_dot_single_segment() {
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        InstanceData {
            instance_id: InstanceId::new(1),
            qualified_name: QualifiedName {
                parts: vec![("plug[data.medium]".to_string(), vec![])],
            },
            type_id: TypeId::new(11),
            ..Default::default()
        },
    );

    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let reference = make_comp_ref("plug[data.medium]");
    assert!(
        matches!(
            scope.lookup_reference(&reference, 1, None, None),
            SemanticLookup::Found(ComponentSemantics {
                type_id,
                ..
            }) if type_id == TypeId::new(11)
        ),
        "dot inside subscript content must not block top-level instanced scope aliases"
    );
}

#[test]
fn instance_identity_scope_uses_typed_component_family_domain() {
    let source = r#"
        model Cell
            Real member[2];
        end Cell;
        model Test
            Cell cells[3];
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("resolve should succeed");
    let cells_def_id = resolved.definitions.classes["Test"].components["cells"]
        .def_id
        .expect("cells declaration identity");
    let member_def_id = resolved.definitions.classes["Cell"].components["member"]
        .def_id
        .expect("member declaration identity");
    let instanced = rumoca_phase_instantiate::instantiate(resolved, "Test")
        .expect("instantiate should succeed");
    let root_class_id = instanced
        .overlay
        .classes
        .values()
        .find(|class| class.owner_component_id.is_none())
        .map(|class| class.instance_id)
        .expect("root class occurrence");
    let scope = InstanceSemanticScope::from_overlay(&instanced.overlay);
    let cells = exact_component_reference(&[("cells", None)], cells_def_id, cells_def_id);
    let selected_cell =
        exact_component_reference(&[("cells", Some(1))], cells_def_id, cells_def_id);
    let selected_member = exact_component_reference(
        &[("cells", Some(1)), ("member", None)],
        cells_def_id,
        member_def_id,
    );

    assert!(
        matches!(
            scope.lookup_reference_shape(&cells, 1, Some(root_class_id), None),
            SemanticLookup::Found(Some(shape)) if shape == vec![3]
        ),
        "the exact family owner and declaration must retain the root array domain"
    );
    assert!(
        matches!(
            scope.lookup_reference(&selected_cell, 1, Some(root_class_id), None),
            SemanticLookup::Found(ComponentSemantics { shape: Some(shape), .. })
                if shape.is_empty()
        ),
        "selecting one expanded family occurrence must produce a scalar component"
    );
    assert!(
        matches!(
            scope.lookup_reference_shape(&selected_member, 2, Some(root_class_id), None),
            SemanticLookup::Found(Some(shape)) if shape == vec![2]
        ),
        "the exact nested member declaration must retain its own typed family domain"
    );
}

#[test]
fn exact_local_scalar_shape_precedes_same_named_parent_array_domain() {
    let source = r#"
        connector Pin
            Real value;
        end Pin;
        model Adapter
            Pin pin;
        end Adapter;
        model Owner
            Pin pin[3];
            Adapter adapters[1];
        end Owner;
        model Test
            Owner owner;
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("resolve should succeed");
    let owner_class_def_id = resolved.definitions.classes["Owner"]
        .def_id
        .expect("Owner class identity");
    let adapter_class_def_id = resolved.definitions.classes["Adapter"]
        .def_id
        .expect("Adapter class identity");
    let owner_pin_def_id = resolved.definitions.classes["Owner"].components["pin"]
        .def_id
        .expect("Owner.pin declaration identity");
    let adapter_pin_def_id = resolved.definitions.classes["Adapter"].components["pin"]
        .def_id
        .expect("Adapter.pin declaration identity");
    let instanced = rumoca_phase_instantiate::instantiate(resolved, "Test")
        .expect("instantiate should succeed");
    let owner_class_id = instanced
        .overlay
        .classes
        .values()
        .find(|class| class.class_def_id == Some(owner_class_def_id))
        .map(|class| class.instance_id)
        .expect("Owner class occurrence");
    let adapter_class_id = instanced
        .overlay
        .classes
        .values()
        .find(|class| class.class_def_id == Some(adapter_class_def_id))
        .map(|class| class.instance_id)
        .expect("Adapter class occurrence");
    let scope = InstanceSemanticScope::from_overlay(&instanced.overlay);
    let owner_pin = exact_component_reference(&[("pin", None)], owner_pin_def_id, owner_pin_def_id);
    let adapter_pin =
        exact_component_reference(&[("pin", None)], adapter_pin_def_id, adapter_pin_def_id);

    assert!(
        matches!(
            scope.lookup_reference_shape(&owner_pin, 1, Some(owner_class_id), None),
            SemanticLookup::Found(Some(shape)) if shape == vec![3]
        ),
        "the enclosing same-named declaration must retain its typed array domain"
    );
    assert!(
        matches!(
            scope.lookup_reference_shape(&adapter_pin, 1, Some(adapter_class_id), None),
            SemanticLookup::Found(Some(shape)) if shape.is_empty()
        ),
        "the exact local scalar declaration must not inherit the enclosing array domain"
    );
}

fn exact_component_reference(
    parts: &[(&str, Option<u64>)],
    root_def_id: DefId,
    target_def_id: DefId,
) -> ComponentReference {
    ComponentReference {
        local: false,
        qualified_display_name: None,
        parts: parts
            .iter()
            .enumerate()
            .map(|(index, (name, subscript))| ComponentRefPart {
                ident: Token {
                    text: Arc::from(*name),
                    ..Default::default()
                },
                subs: subscript.map(|value| {
                    vec![Subscript::Expression(Expression::Terminal {
                        terminal_type: TerminalType::UnsignedInteger,
                        token: Token {
                            text: Arc::from(value.to_string()),
                            ..Default::default()
                        },
                        span: rumoca_core::Span::DUMMY,
                    })]
                }),
                def_id: Some(if index == 0 {
                    root_def_id
                } else {
                    target_def_id
                }),
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_type_scope_hint_fallback_keeps_subscript_dot_single_segment() {
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        InstanceData {
            qualified_name: QualifiedName {
                parts: vec![
                    ("sys".to_string(), vec![]),
                    ("arr[data.medium]".to_string(), vec![]),
                    ("state".to_string(), vec![]),
                ],
            },
            type_name: "Medium.ThermodynamicState".to_string(),
            ..Default::default()
        },
    );

    let hints = TypeChecker::build_type_scope_hints(&overlay);
    let state_path = rumoca_core::ComponentPath::from_parts(["sys", "arr[data.medium]", "state"]);
    assert_eq!(
        hints.get(&state_path),
        Some(&vec![
            "Medium.ThermodynamicState".to_string(),
            "Medium".to_string(),
        ])
    );

    let subscript = Subscript::Expression(Expression::ComponentReference(ComponentReference {
        local: false,
        qualified_display_name: None,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: Arc::from("nX"),
                location: Default::default(),
                token_number: 0,
                token_type: 0,
            },
            subs: None,
            def_id: None,
        }],
        span: rumoca_core::Span::DUMMY,
    }));
    let mut ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::new();
    ctx.add_integer("Medium.nX", 4);

    assert_eq!(
        TypeChecker::eval_dimension_with_type_scope_fallback(
            &subscript,
            &rumoca_core::ComponentPath::from_parts(["sys", "arr[data.medium]", "state", "X",]),
            &hints,
            &ctx,
        ),
        Some(4)
    );
}
