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
fn test_alias_head_ignores_dots_inside_subscripts() {
    assert_eq!(TypeChecker::alias_head("src.stackData.cellData"), "src");
    assert_eq!(
        TypeChecker::alias_head("plug[data.medium].port"),
        "plug[data.medium]"
    );
    assert_eq!(TypeChecker::alias_head("ch[1].chi.vol2"), "ch[1]");
    assert_eq!(TypeChecker::alias_head("standalone"), "standalone");
}

#[test]
fn test_extract_simple_path_preserves_subscripted_component_refs() {
    let expr = Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("stackData"),
                    location: Default::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
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
            },
        ],
        def_id: None,
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
fn test_insert_instanced_aliases_ignores_dot_inside_subscript_expression() {
    let mut out = HashMap::new();
    TypeChecker::insert_instanced_aliases(
        &mut out,
        "plug[data.medium]",
        TypeId::new(7),
        Some("Top"),
    );

    assert_eq!(out.get("plug[data.medium]"), Some(&TypeId::new(7)));
    assert_eq!(out.get("Top.plug[data.medium]"), Some(&TypeId::new(7)));
}

#[test]
fn test_build_instanced_component_type_scope_keeps_subscript_dot_single_segment() {
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        InstanceData {
            qualified_name: QualifiedName {
                parts: vec![("plug[data.medium]".to_string(), vec![])],
            },
            type_id: TypeId::new(11),
            ..Default::default()
        },
    );

    let (full_prefix, short_model) = TypeChecker::instanced_scope_prefixes("Top.Model");
    let scope_map =
        TypeChecker::build_instanced_component_type_scope(&overlay, &full_prefix, &short_model);
    assert_eq!(
        scope_map.get("plug[data.medium]"),
        Some(&TypeId::new(11)),
        "dot inside subscript content must not block top-level instanced scope aliases"
    );
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
        parts: vec![ComponentRefPart {
            ident: Token {
                text: Arc::from("nX"),
                location: Default::default(),
                token_number: 0,
                token_type: 0,
            },
            subs: None,
        }],
        def_id: None,
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
