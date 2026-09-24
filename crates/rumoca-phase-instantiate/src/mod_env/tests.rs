use super::*;
use std::sync::Arc;

const TEST_FILE: &str = "mod_env.mo";

fn test_location() -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 1,
        start_column: 1,
        end_line: 1,
        end_column: 2,
        start: 0,
        end: 1,
        source: rumoca_core::SourceId::from_source_name(TEST_FILE),
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_instantiate_mod_env_source_7.mo"),
        0,
        1,
    )
}

fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn make_int_expr_with_span(value: i64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(&value.to_string()),
        span,
    }
}

fn make_int_expr(value: i64) -> ast::Expression {
    make_int_expr_with_span(value, rumoca_core::Span::DUMMY)
}

fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    })
}

fn make_resolved_comp_ref_expr(parts: &[(&str, u32)]) -> ast::Expression {
    let mut expr = make_comp_ref_expr(&parts.iter().map(|(name, _)| *name).collect::<Vec<_>>());
    let ast::Expression::ComponentReference(comp_ref) = &mut expr else {
        unreachable!("component-reference helper must return a component reference");
    };
    for (part, (_, def_id)) in comp_ref.parts.iter_mut().zip(parts) {
        part.def_id = Some(rumoca_core::DefId::new(*def_id));
    }
    expr
}

fn resolved_component(def_id: u32) -> ast::Component {
    let mut component = ast::Component::empty_with_span(test_span());
    component.def_id = Some(rumoca_core::DefId::new(def_id));
    component
}

fn make_named_arg(name: &str, value: ast::Expression) -> ast::Expression {
    ast::Expression::NamedArgument {
        name: make_token(name),
        value: Arc::new(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_function_call(
    name: &str,
    def_id: rumoca_core::DefId,
    args: Vec<ast::Expression>,
) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
                def_id: Some(def_id),
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        },
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn active_mod_env_keys(ctx: &InstantiateContext) -> Vec<String> {
    ctx.mod_env()
        .active
        .keys()
        .map(ToString::to_string)
        .collect()
}

#[test]
fn indexed_modifier_resolution_rejects_unsupported_selections() {
    let array = ast::Expression::Array {
        elements: vec![make_int_expr(1)],
        kind: rumoca_core::ArrayConstructor::Array,
        span: rumoca_core::Span::DUMMY,
    };
    let subscript = |expr| ast::Subscript::Expression(expr);
    for invalid in [
        make_int_expr(0),
        make_int_expr(2),
        make_comp_ref_expr(&["i"]),
    ] {
        assert_eq!(select_array_value(&array, &[subscript(invalid)]), None);
    }
    assert_eq!(
        select_array_value(&make_int_expr(1), &[subscript(make_int_expr(1))]),
        None
    );
}

fn make_name(name: &str) -> ast::Name {
    ast::Name {
        name: vec![make_token(name)],
        def_id: None,
    }
}

#[test]
fn string_modifier_type_check_requires_segment_boundary() {
    assert!(component_type_allows_string_modifier("String"));
    assert!(component_type_allows_string_modifier("Modelica.String"));
    assert!(component_type_allows_string_modifier("Pkg.Types.String"));
    assert!(!component_type_allows_string_modifier("MyString"));
    assert!(!component_type_allows_string_modifier("Pkg.StringAlias"));
}

#[test]
fn test_resolve_sibling_modification_keeps_class_modification_reference() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut data = ast::Component {
        name: "aimcData".to_string(),
        ..ast::Component::empty_with_span(test_span())
    };
    data.modifications.insert(
        "statorCoreParameters".to_string(),
        ast::Expression::ClassModification {
            target: ast::ComponentReference {
                local: false,
                parts: vec![
                    ast::ComponentRefPart {
                        ident: make_token("Modelica"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Electrical"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Machines"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Losses"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("CoreParameters"),
                        subs: None,
                        def_id: None,
                    },
                ],
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
            },
            modifications: vec![
                make_named_arg("PRef", make_int_expr(410)),
                make_named_arg("VRef", make_int_expr(388)),
            ],
            each_flags: vec![false, false],
            final_flags: vec![false, false],
            redeclare_flags: vec![false, false],
            span: rumoca_core::Span::DUMMY,
        },
    );
    effective_components.insert("aimcData".to_string(), data);

    let expr = make_comp_ref_expr(&["aimcData", "statorCoreParameters"]);
    let (resolved, _) = resolve_modification_expr(
        &expr,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &ast::ClassTree::default(),
            imports: &[],
        },
        false,
        None,
    )
    .expect("resolution should succeed");

    assert_eq!(
        resolved, expr,
        "record bindings should stay as references so declaration defaults are preserved"
    );
}

#[test]
fn test_resolve_sibling_modification_still_resolves_scalar_field_override() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut data = ast::Component {
        name: "stackData".to_string(),
        ..ast::Component::empty_with_span(test_span())
    };
    data.modifications
        .insert("mSystems".to_string(), make_int_expr(2));
    effective_components.insert("stackData".to_string(), data);

    let expr = make_comp_ref_expr(&["stackData", "mSystems"]);
    let (resolved, _) = resolve_modification_expr(
        &expr,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &ast::ClassTree::default(),
            imports: &[],
        },
        false,
        None,
    )
    .expect("resolution should succeed");

    assert_eq!(
        resolved,
        make_int_expr(2),
        "scalar sibling field overrides should keep existing behavior"
    );
}

#[test]
fn test_declaration_binding_preserves_component_reference_identity() {
    let mut mod_env = ast::ModificationEnvironment::default();
    mod_env.add(
        ast::QualifiedName::from_ident("pathLengths"),
        ast::ModificationValue::with_source_scope(
            make_comp_ref_expr(&["length"]),
            Some(make_comp_ref_expr(&["length"])),
            Some(ast::QualifiedName::from_ident("pipe")),
        ),
    );

    let expr = make_comp_ref_expr(&["pathLengths"]);
    let resolved = resolve_declaration_binding_expr(
        &expr,
        &mod_env,
        &IndexMap::default(),
        &ast::ClassTree::default(),
    )
    .expect("declaration binding resolution should succeed");

    assert_eq!(
        resolved, expr,
        "declaration bindings must keep sibling component references instead of inlining modifier values"
    );
}

#[test]
fn test_resolve_sibling_modification_keeps_function_call_record_like_binding() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut data = ast::Component {
        name: "aimcData".to_string(),
        ..ast::Component::empty_with_span(test_span())
    };
    data.modifications.insert(
        "statorCoreParameters".to_string(),
        ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![
                    ast::ComponentRefPart {
                        ident: make_token("Modelica"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Electrical"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Machines"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Losses"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("CoreParameters"),
                        subs: None,
                        def_id: None,
                    },
                ],
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
            },
            args: vec![make_int_expr(410), make_int_expr(388)],
            is_partial_application: false,
            span: rumoca_core::Span::DUMMY,
        },
    );
    effective_components.insert("aimcData".to_string(), data);

    let expr = make_comp_ref_expr(&["aimcData", "statorCoreParameters"]);
    let (resolved, _) = resolve_modification_expr(
        &expr,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &ast::ClassTree::default(),
            imports: &[],
        },
        false,
        None,
    )
    .expect("resolution should succeed");

    assert_eq!(
        resolved, expr,
        "function-call record-like overrides should stay as references"
    );
}

#[test]
fn test_insert_scoped_modifier_binding_reorders_non_shifted_parent_key() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    let sibling = ast::QualifiedName::from_ident("a");

    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    ctx.mod_env_mut().add(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    parent_snapshot.insert(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(9),
            source: None,
            value_scope: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &IndexMap::default(),
    )
    .expect("non-final parent key can be replaced");

    assert_eq!(
        active_mod_env_keys(&ctx),
        vec!["a".to_string(), "k".to_string()],
        "non-shifted parent key should be replaced as a new local binding"
    );
    assert_eq!(
        ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
        Some(make_int_expr(9))
    );
}

#[test]
fn test_insert_scoped_modifier_binding_keeps_shifted_parent_key_position() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    let sibling = ast::QualifiedName::from_ident("a");

    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    ctx.mod_env_mut().add(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    parent_snapshot.insert(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let mut shifted_parent_keys = IndexMap::default();
    shifted_parent_keys.insert(key.clone(), ());

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(11),
            source: None,
            value_scope: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &shifted_parent_keys,
    )
    .expect("shifted non-final parent key can be preserved");

    assert_eq!(
        active_mod_env_keys(&ctx),
        vec!["k".to_string(), "a".to_string()],
        "shifted parent key should remain in place"
    );
    assert_eq!(
        ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
        Some(make_int_expr(1)),
        "outer/shifted parent modifier must keep precedence (MLS §7.2.4)"
    );
}

#[test]
fn test_insert_scoped_modifier_binding_keeps_shifted_final_parent_key() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("R");
    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr(10), false, true),
    );

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr(10), false, true),
    );

    let mut shifted_parent_keys = IndexMap::default();
    shifted_parent_keys.insert(key.clone(), ());

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(20),
            source: None,
            value_scope: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &shifted_parent_keys,
    )
    .expect("shifted final outer modifier keeps precedence over inner default");

    assert_eq!(
        ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
        Some(make_int_expr(10))
    );
}

#[test]
fn test_apply_component_modifier_rejects_inherited_final_component() {
    let base_id = rumoca_core::DefId::new(10);
    let mut base = ast::ClassDef {
        def_id: Some(base_id),
        name: make_token("Base"),
        ..Default::default()
    };
    base.components.insert(
        "p".to_string(),
        ast::Component {
            name: "p".to_string(),
            type_name: make_name("Real"),
            is_final: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut derived = ast::ClassDef {
        name: make_token("Derived"),
        ..Default::default()
    };
    derived.extends.push(ast::Extend {
        base_name: make_name("Base"),
        base_def_id: Some(base_id),
        location: test_location(),
        ..Default::default()
    });

    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base;");
    tree.definitions.classes.insert("Base".to_string(), base);
    tree.definitions
        .classes
        .insert("Derived".to_string(), derived.clone());
    tree.def_map.insert(base_id, "Base".to_string());
    tree.name_map.insert("Base".to_string(), base_id);

    let mut ctx = InstantiateContext::new();
    let parent_snapshot = IndexMap::default();
    let shifted_parent_keys = IndexMap::default();
    let type_overrides = TypeOverrideMap::default();
    let eval_ctx = ModifierEvalContext {
        tree: &tree,
        effective_components: &IndexMap::default(),
        type_overrides: &type_overrides,
        target_class: Some(&derived),
        insert_ctx: ScopedInsertContext {
            parent_snapshot: &parent_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            source_scope: None,
            imports: &[],
        },
    };

    let err = apply_component_modifier(
        &mut ctx,
        "p",
        &make_int_expr_with_span(2, test_span()),
        ModifierPrefixes::default(),
        &eval_ctx,
    )
    .expect_err("inherited final components must reject modification");

    let message = err.to_string();
    assert!(message.contains("final"), "{message}");
    assert!(matches!(*err, InstantiateError::RedeclareFinal { .. }));
}

#[test]
fn test_apply_component_modifier_requires_span_for_final_modifier_error() {
    let mut class = ast::ClassDef {
        name: make_token("C"),
        ..Default::default()
    };
    class.components.insert(
        "p".to_string(),
        ast::Component {
            name: "p".to_string(),
            type_name: make_name("Real"),
            is_final: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let parent_snapshot = IndexMap::default();
    let shifted_parent_keys = IndexMap::default();
    let type_overrides = TypeOverrideMap::default();
    let tree = ast::ClassTree::default();
    let eval_ctx = ModifierEvalContext {
        tree: &tree,
        effective_components: &IndexMap::default(),
        type_overrides: &type_overrides,
        target_class: Some(&class),
        insert_ctx: ScopedInsertContext {
            parent_snapshot: &parent_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            source_scope: None,
            imports: &[],
        },
    };

    let err = apply_component_modifier(
        &mut ctx,
        "p",
        &make_int_expr(2),
        ModifierPrefixes::default(),
        &eval_ctx,
    )
    .expect_err("unspanned final modifier error should fail fast");

    assert!(matches!(
        *err,
        InstantiateError::MissingSourceContext { .. }
    ));
}

#[test]
fn test_insert_scoped_modifier_binding_reports_final_collision_at_source() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr_with_span(1, test_span()), false, true),
    );

    let err = insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key,
            value: make_int_expr_with_span(2, test_span()),
            source: Some(make_int_expr_with_span(3, test_span())),
            value_scope: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &IndexMap::default(),
    )
    .expect_err("local modification must not override final binding");

    assert!(matches!(*err, InstantiateError::RedeclareFinal { .. }));
}

#[test]
fn test_insert_scoped_modifier_binding_requires_span_for_final_collision() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr_with_span(1, test_span()), false, true),
    );

    let err = insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key,
            value: make_int_expr(2),
            source: None,
            value_scope: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &IndexMap::default(),
    )
    .expect_err("unspanned final binding error should fail fast");

    assert!(matches!(
        *err,
        InstantiateError::MissingSourceContext { .. }
    ));
}

#[test]
fn nested_modifier_keeps_evaluated_and_written_occurrence_scopes() {
    let root_scope = ast::QualifiedName::new();
    let machine_scope = ast::QualifiedName::from_ident("machine");
    let mut selected_member = make_comp_ref_expr(&["driveData", "motorData", "wNominal"]);
    let ast::Expression::ComponentReference(reference) = &mut selected_member else {
        unreachable!()
    };
    reference.set_root_def_id(Some(rumoca_core::DefId::new(42)));

    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("wNominal"),
        ast::ModificationValue::with_source_scope(
            selected_member.clone(),
            Some(selected_member.clone()),
            Some(root_scope.clone()),
        ),
    );
    let ast::Expression::ComponentReference(target) = make_comp_ref_expr(&["wRef"]) else {
        unreachable!()
    };
    let written_value = make_resolved_comp_ref_expr(&[("wNominal", 901)]);
    let modifier = ast::Expression::Modification {
        target,
        value: Arc::new(written_value.clone()),
        span: test_span(),
    };
    let components = IndexMap::default();
    let tree = ast::ClassTree::default();
    process_nested_modifications_recursive(
        &mut ctx,
        &ast::QualifiedName::from_ident("coreParameters"),
        &[modifier],
        &NestedModificationContext {
            effective_components: &components,
            tree: &tree,
            source_scope: Some(machine_scope.clone()),
            imports: &[],
        },
        NestedModificationFlags {
            prefixes: ModifierPrefixes::default(),
            each_flags: &[],
            final_flags: &[],
        },
    )
    .expect("nested modifier must retain both occurrence scopes");

    let stored = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_dotted("coreParameters.wRef"))
        .expect("nested field modifier");
    assert_eq!(stored.value, selected_member);
    assert_eq!(stored.source.as_ref(), Some(&written_value));
    let ast::Expression::ComponentReference(value_ref) = &stored.value else {
        panic!("substituted binding must keep the selected member reference")
    };
    let ast::Expression::ComponentReference(source_ref) = stored.source.as_ref().unwrap() else {
        panic!("written source must keep the default member reference")
    };
    assert_eq!(value_ref.root_def_id(), Some(rumoca_core::DefId::new(42)));
    assert_eq!(source_ref.root_def_id(), Some(rumoca_core::DefId::new(901)));
    assert_eq!(stored.value_scope.as_ref(), Some(&root_scope));
    assert_eq!(stored.source_scope.as_ref(), Some(&machine_scope));
}

#[test]
fn record_projection_keeps_evaluated_and_written_occurrence_scopes() {
    let root_scope = ast::QualifiedName::new();
    let machine_scope = ast::QualifiedName::from_ident("machine");
    let value = make_resolved_comp_ref_expr(&[
        ("driveData", 42),
        ("motorData", 503),
        ("coreParameters", 504),
    ]);
    let source = make_resolved_comp_ref_expr(&[("coreParameters", 601)]);
    let mut record = ast::ClassDef {
        class_type: rumoca_core::ClassType::Record,
        ..Default::default()
    };
    record
        .components
        .insert("wRef".to_string(), resolved_component(700));
    let mut ctx = InstantiateContext::new();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &value,
            source: Some(&source),
            value_scope: Some(root_scope.clone()),
            source_scope: Some(machine_scope.clone()),
            each: false,
        },
        &record,
        &IndexMap::default(),
    )
    .expect("record projection must preserve both occurrence scopes");
    let stored = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_ident("wRef"))
        .expect("projected record field");
    assert_eq!(stored.value_scope.as_ref(), Some(&root_scope));
    assert_eq!(stored.source_scope.as_ref(), Some(&machine_scope));
    let ast::Expression::FieldAccess {
        base, field_def_id, ..
    } = &stored.value
    else {
        panic!("evaluated record field must retain its structured base")
    };
    assert_eq!(base.as_ref(), &value);
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(700)));
    let ast::Expression::FieldAccess { base, .. } = stored.source.as_ref().unwrap() else {
        panic!("written record field must retain its structured base")
    };
    assert_eq!(base.as_ref(), &source);
}

#[test]
fn test_forwarded_modifier_keeps_forwarded_source_scope() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("frictionParameters");
    let forwarded_value = make_comp_ref_expr(&["aimcData", "frictionParameters"]);
    let forwarded_scope = Some(ast::QualifiedName::new());

    ctx.mod_env_mut().active.insert(
        key.clone(),
        ast::ModificationValue::with_source_scope(
            forwarded_value.clone(),
            Some(forwarded_value.clone()),
            forwarded_scope.clone(),
        ),
    );

    let parent_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    let insert_ctx = ScopedInsertContext {
        parent_snapshot: &parent_snapshot,
        shifted_parent_keys: &shifted_parent_keys,
        source_scope: Some(ast::QualifiedName::from_ident("aimc")),
        imports: &[],
    };

    insert_modifier_value_with_structural_overrides(
        &mut ctx,
        "frictionParameters",
        &make_comp_ref_expr(&["frictionParameters"]),
        ModifierInsertOptions {
            allow_string_eval: false,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &ast::ClassTree::default(),
        &insert_ctx,
    )
    .expect("forwarded modifier insertion should succeed");

    let stored = ctx
        .mod_env()
        .get(&key)
        .expect("forwarded modifier binding should exist");
    assert_eq!(
        stored.value, forwarded_value,
        "forwarded binding should preserve resolved parent expression"
    );
    assert_eq!(
        stored.source_scope, forwarded_scope,
        "forwarded binding should preserve original lexical source scope"
    );
    assert_eq!(
        stored.source.as_ref(),
        Some(&forwarded_value),
        "forwarded binding should preserve symbolic source expression"
    );
}

#[test]
fn test_sibling_modifier_reference_keeps_local_source_scope() {
    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().active.insert(
        ast::QualifiedName::from_ident("pathLengths"),
        ast::ModificationValue::with_source_scope(
            make_comp_ref_expr(&["length"]),
            Some(make_comp_ref_expr(&["length"])),
            Some(ast::QualifiedName::from_ident("pipe")),
        ),
    );

    let parent_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    let local_scope = Some(ast::QualifiedName::from_ident("flowModel"));
    let insert_ctx = ScopedInsertContext {
        parent_snapshot: &parent_snapshot,
        shifted_parent_keys: &shifted_parent_keys,
        source_scope: local_scope.clone(),
        imports: &[],
    };

    insert_modifier_value_with_structural_overrides(
        &mut ctx,
        "pathLengths_internal",
        &make_comp_ref_expr(&["pathLengths"]),
        ModifierInsertOptions {
            allow_string_eval: false,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &ast::ClassTree::default(),
        &insert_ctx,
    )
    .expect("sibling modifier insertion should succeed");

    let stored = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_ident("pathLengths_internal"))
        .expect("sibling modifier binding should exist");
    assert_eq!(
        stored.source.as_ref(),
        Some(&make_comp_ref_expr(&["pathLengths"]))
    );
    assert_eq!(stored.source_scope, local_scope);
}

#[test]
fn test_modifier_with_same_resolved_value_keeps_existing_source_scope() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("frictionParameters");
    let forwarded_value = make_comp_ref_expr(&["aimcData", "frictionParameters"]);
    let forwarded_scope = Some(ast::QualifiedName::new());

    ctx.mod_env_mut().active.insert(
        key.clone(),
        ast::ModificationValue::with_source_scope(
            forwarded_value.clone(),
            Some(forwarded_value.clone()),
            forwarded_scope.clone(),
        ),
    );

    let parent_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    let insert_ctx = ScopedInsertContext {
        parent_snapshot: &parent_snapshot,
        shifted_parent_keys: &shifted_parent_keys,
        source_scope: Some(ast::QualifiedName::from_ident("aimc")),
        imports: &[],
    };

    insert_modifier_value_with_structural_overrides(
        &mut ctx,
        "frictionParameters",
        &forwarded_value,
        ModifierInsertOptions {
            allow_string_eval: false,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &ast::ClassTree::default(),
        &insert_ctx,
    )
    .expect("same-value modifier insertion should succeed");

    let stored = ctx
        .mod_env()
        .get(&key)
        .expect("modifier binding should exist");
    assert_eq!(
        stored.source_scope, forwarded_scope,
        "resolved multi-part modifier should inherit source scope from existing parent binding"
    );
}

#[test]
fn test_propagate_record_binding_overrides_non_targeted_field_values() {
    let mut nested_record = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(800)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("phase".to_string(), resolved_component(801));
    nested_record
        .components
        .insert("p".to_string(), resolved_component(802));

    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("phase"),
        ast::ModificationValue::simple(make_int_expr(7)),
    );

    let binding_expr = make_comp_ref_expr(&["state_in"]);
    let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &targeted_keys,
    )
    .expect("record field projection should succeed");

    let phase_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("phase"))
        .expect("phase field binding should be present");
    match &phase_mod.value {
        ast::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            assert_eq!(field, "phase");
            assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(801)));
            match base.as_ref() {
                ast::Expression::ComponentReference(cref) => {
                    assert_eq!(cref.parts.len(), 1);
                    assert_eq!(cref.parts[0].ident.text.as_ref(), "state_in");
                }
                _ => panic!("field binding should project from record binding expression"),
            }
        }
        _ => panic!("phase field should be rebound from record binding"),
    }
}

#[derive(Clone, Copy)]
struct RecordProjectionIds {
    state: rumoca_core::DefId,
    phase: rumoca_core::DefId,
    pressure: rumoca_core::DefId,
    make_state: rumoca_core::DefId,
    make_state_p: rumoca_core::DefId,
}

struct RecordProjectionFixture {
    tree: ast::ClassTree,
    state: ast::ClassDef,
    constructor: ast::Expression,
    ordinary_function: ast::Expression,
}

fn record_projection_fixture() -> RecordProjectionFixture {
    let ids = RecordProjectionIds {
        state: rumoca_core::DefId::new(1500),
        phase: rumoca_core::DefId::new(1501),
        pressure: rumoca_core::DefId::new(1502),
        make_state: rumoca_core::DefId::new(1503),
        make_state_p: rumoca_core::DefId::new(1504),
    };
    let state = record_projection_state(ids);
    let make_state = record_projection_function(ids);
    let tree = record_projection_tree(&state, make_state, ids);
    let pressure = make_comp_ref_expr(&["pressure"]);

    RecordProjectionFixture {
        tree,
        state,
        constructor: make_function_call("State", ids.state, vec![pressure.clone()]),
        ordinary_function: make_function_call("makeState", ids.make_state, vec![pressure]),
    }
}

fn record_projection_state(ids: RecordProjectionIds) -> ast::ClassDef {
    let mut state = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(ids.state),
        ..Default::default()
    };
    state
        .components
        .insert("phase".to_string(), resolved_component(ids.phase.index()));
    state
        .components
        .insert("p".to_string(), resolved_component(ids.pressure.index()));
    state
}

fn record_projection_function(ids: RecordProjectionIds) -> ast::ClassDef {
    let mut function = ast::ClassDef {
        name: make_token("makeState"),
        class_type: rumoca_core::ClassType::Function,
        def_id: Some(ids.make_state),
        ..Default::default()
    };
    function.components.insert(
        "phase".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(1505)),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    function.components.insert(
        "p".to_string(),
        ast::Component {
            def_id: Some(ids.make_state_p),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    function.components.insert(
        "state".to_string(),
        ast::Component {
            type_name: make_name("State"),
            type_def_id: Some(ids.state),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    function
}

fn record_projection_tree(
    state: &ast::ClassDef,
    make_state: ast::ClassDef,
    ids: RecordProjectionIds,
) -> ast::ClassTree {
    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("State".to_string(), state.clone());
    tree.definitions
        .classes
        .insert("makeState".to_string(), make_state);

    let mut package = ast::ClassDef {
        name: make_token("Pkg"),
        class_type: rumoca_core::ClassType::Package,
        def_id: Some(rumoca_core::DefId::new(1506)),
        ..Default::default()
    };
    let mut scoped_record = state.clone();
    scoped_record.name = make_token("makeState");
    scoped_record.def_id = Some(rumoca_core::DefId::new(1507));
    package
        .classes
        .insert("makeState".to_string(), scoped_record);
    tree.definitions.classes.insert("Pkg".to_string(), package);

    for (def_id, name) in [
        (ids.state, "State"),
        (ids.make_state, "makeState"),
        (rumoca_core::DefId::new(1506), "Pkg"),
        (rumoca_core::DefId::new(1507), "Pkg.makeState"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
    }
    tree
}

#[test]
fn test_record_projection_only_positional_maps_actuals_for_record_constructors() {
    let fixture = record_projection_fixture();
    let targeted_keys = IndexMap::default();

    let mut constructor_ctx = InstantiateContext::new();
    propagate_record_binding_to_fields(
        &fixture.tree,
        &mut constructor_ctx,
        RecordBindingProjection {
            value: &fixture.constructor,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &fixture.state,
        &targeted_keys,
    )
    .expect("record constructor projection should succeed");
    let phase_binding = &constructor_ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("phase"))
        .expect("record constructor should project the phase field")
        .value;
    let ast::Expression::ComponentReference(phase_reference) = phase_binding else {
        panic!("record constructor positional input should bind the matching field");
    };
    assert_eq!(phase_reference.to_string(), "pressure");

    let mut function_ctx = InstantiateContext::new();
    propagate_record_binding_to_fields(
        &fixture.tree,
        &mut function_ctx,
        RecordBindingProjection {
            value: &fixture.ordinary_function,
            source: None,
            value_scope: Some(ast::QualifiedName::from_ident("Pkg")),
            source_scope: Some(ast::QualifiedName::from_ident("Pkg")),
            each: false,
        },
        &fixture.state,
        &IndexMap::default(),
    )
    .expect("ordinary record-returning function projection should succeed");
    let phase_binding = &function_ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("phase"))
        .expect("ordinary function result should still project the result field")
        .value;
    assert!(
        matches!(phase_binding, ast::Expression::FieldAccess { base, field, .. }
            if field == "phase" && base.as_ref() == &fixture.ordinary_function),
        "ordinary function result must remain a field access of the call"
    );
}

#[test]
fn source_record_returning_function_keeps_call_as_field_access() {
    let source = r#"
package P
  record State
    Integer phase;
    Real p;
  end State;

  function makeState
    input Integer phase;
    input Real p;
    output State state;
  algorithm
    state.phase := phase + 1;
    state.p := p;
  end makeState;

  model Use
    Real pressure;
    State state = makeState(2, pressure);
  end Use;
end P;
"#;
    let file_name = "record_returning_function_projection.mo";
    let parsed = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source should parse");
    let mut tree = ast::ClassTree::from_parsed(parsed);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source should resolve");
    let instanced = crate::instantiate(resolved, "P.Use")
        .expect("record-returning function binding should instantiate");

    let phase = instanced
        .overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "state.phase")
        .expect("record projection should materialize state.phase");
    let binding = phase
        .binding
        .as_ref()
        .expect("state.phase should retain a binding");
    let ast::Expression::FieldAccess { base, field, .. } = binding else {
        panic!("record-returning function must not be treated as a constructor");
    };
    assert_eq!(field, "phase");
    assert!(
        matches!(base.as_ref(), ast::Expression::FunctionCall { .. }),
        "state.phase must project from the function call result"
    );
}

#[test]
fn test_record_projection_preserves_written_source_and_exact_field_identity() {
    let mut core_parameters = ast::ClassDef {
        name: make_token("CoreParameters"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(408)),
        ..Default::default()
    };
    core_parameters
        .components
        .insert("PRef".to_string(), resolved_component(414));

    let resolved = make_resolved_comp_ref_expr(&[("aimcData", 305), ("statorCoreParameters", 519)]);
    let written = make_resolved_comp_ref_expr(&[("statorCoreParameters", 519)]);
    let source_scope = ast::QualifiedName::from_ident("aimc");
    let mut ctx = InstantiateContext::new();

    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &resolved,
            source: Some(&written),
            value_scope: Some(source_scope.clone()),
            source_scope: Some(source_scope.clone()),
            each: false,
        },
        &core_parameters,
        &IndexMap::default(),
    )
    .expect("record field projection should preserve value and source provenance");

    let projected = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_ident("PRef"))
        .expect("projected field binding should exist");
    assert_eq!(projected.source_scope, Some(source_scope));
    assert_projected_ref(&projected.value, &[305, 519], 414);
    assert_projected_ref(
        projected
            .source
            .as_ref()
            .expect("projected binding should retain its written source"),
        &[519],
        414,
    );
}

fn assert_projected_ref(expr: &ast::Expression, base_def_ids: &[u32], field_def_id: u32) {
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id: actual_field_def_id,
        ..
    } = expr
    else {
        panic!("expected projected field access, got {expr:?}");
    };
    let ast::Expression::ComponentReference(comp_ref) = base.as_ref() else {
        panic!("expected component-reference projection base, got {base:?}");
    };
    assert_eq!(field, "PRef");
    assert_eq!(
        *actual_field_def_id,
        Some(rumoca_core::DefId::new(field_def_id))
    );
    assert_eq!(
        comp_ref
            .parts
            .iter()
            .map(|part| part.def_id.expect("base segment must have exact identity"))
            .collect::<Vec<_>>(),
        base_def_ids
            .iter()
            .map(|def_id| rumoca_core::DefId::new(*def_id))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_propagate_record_binding_preserves_each_prefix_for_fields() {
    let mut nested_record = ast::ClassDef {
        name: make_token("Orientation"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(810)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("T".to_string(), resolved_component(811));

    let mut ctx = InstantiateContext::new();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &make_comp_ref_expr(&["R"]),
            source: None,
            value_scope: None,
            source_scope: None,
            each: true,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("T"))
        .expect("projected field binding should exist");
    assert!(field.each);
}

#[test]
fn test_propagate_record_binding_does_not_treat_start_as_field_default() {
    let mut nested_record = ast::ClassDef {
        name: make_token("Orientation"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(820)),
        ..Default::default()
    };
    nested_record.components.insert(
        "T".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(821)),
            start: make_int_expr(0),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = make_comp_ref_expr(&["source"]);
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("T"))
        .expect("start-only fields must inherit the record binding");
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = &field_mod.value
    else {
        panic!("field binding should project from the record binding");
    };
    assert_eq!(field, "T");
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(821)));
    assert_eq!(base.as_ref(), &binding_expr);
}

#[test]
fn test_propagate_record_binding_preserves_targeted_field_modifiers() {
    let mut nested_record = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(830)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("phase".to_string(), resolved_component(831));

    let mut ctx = InstantiateContext::new();
    let phase_qn = ast::QualifiedName::from_ident("phase");
    ctx.mod_env_mut().add(
        phase_qn.clone(),
        ast::ModificationValue::simple(make_int_expr(42)),
    );

    let mut targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    targeted_keys.insert(phase_qn.clone(), ());
    let binding_expr = make_comp_ref_expr(&["state_in"]);
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: true,
        },
        &nested_record,
        &targeted_keys,
    )
    .expect("record field projection should succeed");

    let phase_mod = ctx
        .mod_env()
        .active
        .get(&phase_qn)
        .expect("targeted phase modifier should still be present");
    match &phase_mod.value {
        ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "42"),
        _ => panic!("targeted field modifier should not be replaced"),
    }
    assert!(
        !phase_mod.each,
        "the winning field modifier must not inherit `each` from the record binding"
    );
}

#[test]
fn test_propagate_record_binding_projects_if_expression_branches_per_field() {
    let mut nested_record = ast::ClassDef {
        name: make_token("CellData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(840)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("OCV_SOC".to_string(), resolved_component(841));

    let mut ctx = InstantiateContext::new();
    let binding_expr = ast::Expression::If {
        branches: vec![(
            make_comp_ref_expr(&["isDegraded"]),
            make_comp_ref_expr(&["cellDataDegraded"]),
        )],
        else_branch: Arc::new(make_comp_ref_expr(&["cellDataOriginal"])),
        span: rumoca_core::Span::DUMMY,
    };
    let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &targeted_keys,
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("OCV_SOC"))
        .expect("OCV_SOC field binding should be present");
    let ast::Expression::If {
        branches,
        else_branch,
        ..
    } = &field_mod.value
    else {
        panic!("field projection should preserve if-expression structure");
    };
    assert_eq!(branches.len(), 1);
    let (_cond, then_expr) = &branches[0];
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = then_expr
    else {
        panic!("then-branch should project field access");
    };
    assert_eq!(field, "OCV_SOC");
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(841)));
    assert_eq!(*base.as_ref(), make_comp_ref_expr(&["cellDataDegraded"]));

    let ast::Expression::FieldAccess {
        base: else_base,
        field: else_field,
        ..
    } = else_branch.as_ref()
    else {
        panic!("else-branch should project field access");
    };
    assert_eq!(else_field, "OCV_SOC");
    assert_eq!(
        *else_base.as_ref(),
        make_comp_ref_expr(&["cellDataOriginal"])
    );
}

#[test]
fn test_record_alias_from_outer_scope_projects_declared_default_field() {
    let mut nested_record = ast::ClassDef {
        name: make_token("CellData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(1200)),
        ..Default::default()
    };
    nested_record.components.insert(
        "nRC".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(1201)),
            binding: Some(make_int_expr(1)),
            start: make_int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = make_comp_ref_expr(&["cellDataOriginal"]);
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: Some(ast::QualifiedName::new()),
            source_scope: Some(ast::QualifiedName::new()),
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("outer record alias should project fields");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("nRC"))
        .expect("the outer record's nRC must replace the target default");
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = &field_mod.value
    else {
        panic!("outer record field should be projected");
    };
    assert_eq!(field, "nRC");
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(1201)));
    assert_eq!(base.as_ref(), &binding_expr);
}

#[test]
fn test_propagate_record_binding_preserves_matching_default_record_constructor() {
    let mut nested_record = ast::ClassDef {
        name: make_token("BaseData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(850)),
        ..Default::default()
    };
    nested_record.components.insert(
        "mu_i".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(851)),
            binding: Some(make_int_expr(1)),
            start: make_int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("BaseData"),
                subs: None,
                def_id: Some(rumoca_core::DefId::new(850)),
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        },
        args: Vec::new(),
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };

    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    assert!(
        ctx.mod_env().active.is_empty(),
        "matching zero-argument record constructors should preserve declared defaults"
    );
}

#[test]
fn test_propagate_record_binding_projects_subtype_default_record_constructor_fields() {
    let mut nested_record = ast::ClassDef {
        name: make_token("BaseData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(860)),
        ..Default::default()
    };
    nested_record.components.insert(
        "mu_i".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(861)),
            binding: Some(make_int_expr(1)),
            start: make_int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("M350_50A"),
                subs: None,
                def_id: Some(rumoca_core::DefId::new(862)),
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        },
        args: Vec::new(),
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };

    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("mu_i"))
        .expect("subtype default record constructor should project field binding");
    let ast::Expression::FieldAccess { base, field, .. } = &field_mod.value else {
        panic!("subtype constructor field should be projected");
    };
    assert_eq!(field, "mu_i");
    assert_eq!(base.as_ref(), &binding_expr);
}

#[test]
fn test_propagate_record_binding_projects_through_unique_constructor_record_field() {
    let inner_def_id = rumoca_core::DefId::new(1001);
    let outer_def_id = rumoca_core::DefId::new(1002);
    let inner_x_def_id = rumoca_core::DefId::new(1003);
    let outer_inner_params_def_id = rumoca_core::DefId::new(1004);
    let mut inner_record = ast::ClassDef {
        name: make_token("Inner"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(inner_def_id),
        ..Default::default()
    };
    inner_record
        .components
        .insert("x".to_string(), resolved_component(inner_x_def_id.index()));

    let mut outer_record = ast::ClassDef {
        name: make_token("Outer"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(outer_def_id),
        ..Default::default()
    };
    outer_record.components.insert(
        "innerParams".to_string(),
        ast::Component {
            def_id: Some(outer_inner_params_def_id),
            type_name: ast::Name {
                name: vec![make_token("Pkg"), make_token("Inner")],
                def_id: Some(inner_def_id),
            },
            type_def_id: Some(inner_def_id),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut package = ast::ClassDef {
        name: make_token("Pkg"),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    package
        .classes
        .insert("Inner".to_string(), inner_record.clone());
    package.classes.insert("Outer".to_string(), outer_record);
    let mut tree = ast::ClassTree::default();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    tree.def_map.insert(inner_def_id, "Pkg.Inner".to_string());
    tree.def_map.insert(outer_def_id, "Pkg.Outer".to_string());

    let mut ctx = InstantiateContext::new();
    let binding_expr = ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("Outer"),
                subs: None,
                def_id: Some(outer_def_id),
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        },
        args: Vec::new(),
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };

    propagate_record_binding_to_fields(
        &tree,
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: Some(ast::QualifiedName::from_ident("Pkg")),
            source_scope: Some(ast::QualifiedName::from_ident("Pkg")),
            each: false,
        },
        &inner_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("x"))
        .expect("inner field binding should be present");
    let ast::Expression::FieldAccess {
        base,
        field: inner_field,
        ..
    } = &field_mod.value
    else {
        panic!("inner field should be projected");
    };
    assert_eq!(inner_field, "x");
    let ast::Expression::FieldAccess {
        base: constructor,
        field: outer_field,
        ..
    } = base.as_ref()
    else {
        panic!("projection should first select the unique compatible record field");
    };
    assert_eq!(outer_field, "innerParams");
    assert_eq!(constructor.as_ref(), &binding_expr);
}

#[test]
fn test_propagate_record_binding_skips_non_record_classes() {
    let mut nested_block = ast::ClassDef {
        name: make_token("UniformNoise"),
        class_type: rumoca_core::ClassType::Block,
        ..Default::default()
    };
    nested_block.components.insert(
        "y".to_string(),
        ast::Component::empty_with_span(test_span()),
    );
    nested_block.components.insert(
        "seedState".to_string(),
        ast::Component::empty_with_span(test_span()),
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = make_comp_ref_expr(&["noise"]);
    let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            value_scope: None,
            source_scope: None,
            each: false,
        },
        &nested_block,
        &targeted_keys,
    )
    .expect("non-record projection should succeed without mutation");

    assert!(
        ctx.mod_env().active.is_empty(),
        "non-record class modifiers must not synthesize per-field record bindings"
    );
}
