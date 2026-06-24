use super::*;
use rumoca_core::DefId;

fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("mod_env_tests.mo"),
        1,
        2,
    )
}

fn make_int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(&value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_name(text: &str) -> ast::Name {
    ast::Name {
        name: vec![make_token(text)],
        def_id: None,
    }
}

fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    })
}

fn make_component(name: &str, type_name: &str, type_def_id: Option<DefId>) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        name_token: make_token(name),
        type_name: make_name(type_name),
        type_def_id,
        ..ast::Component::empty_with_span(test_span())
    }
}

fn make_start_modification(value: ast::Expression) -> ast::Expression {
    ast::Expression::Modification {
        target: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("start"),
                subs: None,
            }],
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        },
        value: std::sync::Arc::new(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn process_nested_test_modifications(
    ctx: &mut InstantiateContext,
    prefix: &ast::QualifiedName,
    modifications: &[ast::Expression],
    source_scope: Option<ast::QualifiedName>,
    flags: NestedModificationFlags<'_>,
) -> InstantiateResult<()> {
    let effective_components = IndexMap::default();
    let tree = ast::ClassTree::default();
    let nested_ctx = NestedModificationContext {
        effective_components: &effective_components,
        tree: &tree,
        source_scope,
    };
    process_nested_modifications_recursive(ctx, prefix, modifications, &nested_ctx, flags)
}

#[test]
fn local_modifier_replaces_unrelated_parent_final_key() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("m");
    let parent_value = ast::ModificationValue::with_prefixes(make_int_expr(3), false, true);
    ctx.mod_env_mut().add(key.clone(), parent_value.clone());

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(key.clone(), parent_value);

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(5),
            source: Some(make_comp_ref_expr(&["outer_m"])),
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &IndexMap::default(),
    )
    .expect("local modifier should replace a same-name parent-scope key");

    assert!(
        ctx.mod_env().get(&key).is_some_and(|value| !value.final_),
        "replacement should not inherit the unrelated parent final prefix"
    );
}

#[test]
fn test_nested_modification_preserves_final_prefix() {
    let mut ctx = InstantiateContext::new();
    let prefix = ast::QualifiedName::from_ident("x");
    let modifications = vec![make_start_modification(make_int_expr(1))];

    process_nested_test_modifications(
        &mut ctx,
        &prefix,
        &modifications,
        None,
        NestedModificationFlags {
            prefixes: ModifierPrefixes {
                final_: true,
                each: false,
            },
            each_flags: &[],
            final_flags: &[],
        },
    )
    .expect("nested modification should be stored");

    assert!(
        ctx.mod_env()
            .get(&ast::QualifiedName::from_dotted("x.start"))
            .is_some_and(|mv| mv.final_)
    );
}

#[test]
fn test_nested_attribute_final_flag_is_preserved() {
    let mut ctx = InstantiateContext::new();
    let prefix = ast::QualifiedName::from_ident("x");
    let modifications = vec![make_start_modification(make_int_expr(1))];

    process_nested_test_modifications(
        &mut ctx,
        &prefix,
        &modifications,
        None,
        NestedModificationFlags {
            prefixes: ModifierPrefixes::default(),
            each_flags: &[false],
            final_flags: &[true],
        },
    )
    .expect("nested final attribute modification should be stored");

    assert!(
        ctx.mod_env()
            .get(&ast::QualifiedName::from_dotted("x.start"))
            .is_some_and(|mv| mv.final_)
    );
}

#[test]
fn nested_start_attribute_preserves_local_parameter_reference() {
    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("h_start"),
        ast::ModificationValue::with_source_scope(
            make_comp_ref_expr(&["h_start_1"]),
            Some(make_comp_ref_expr(&["h_start_1"])),
            Some(ast::QualifiedName::from_ident("HEX")),
        ),
    );

    let prefix = ast::QualifiedName::from_ident("h");
    let source_scope = Some(ast::QualifiedName::from_dotted("HEX.pipe_1"));
    let modifications = vec![make_start_modification(make_comp_ref_expr(&["h_start"]))];

    process_nested_test_modifications(
        &mut ctx,
        &prefix,
        &modifications,
        source_scope.clone(),
        NestedModificationFlags {
            prefixes: ModifierPrefixes::default(),
            each_flags: &[],
            final_flags: &[],
        },
    )
    .expect("nested start modification should be recorded");

    let stored = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_dotted("h.start"))
        .expect("start attribute modifier should exist");
    assert_eq!(stored.value, make_comp_ref_expr(&["h_start"]));
    assert_eq!(stored.source_scope, source_scope);
}

#[test]
fn sibling_primitive_parameter_reference_does_not_project_attributes() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut m = make_component("m", "Integer", None);
    m.modifications.insert("min".to_string(), make_int_expr(1));
    effective_components.insert("m".to_string(), m);

    let fields = collect_structural_integer_fields_from_sibling_reference(
        &make_comp_ref_expr(&["m"]),
        &ast::ModificationEnvironment::default(),
        &effective_components,
        &ast::ClassTree::default(),
    );

    assert!(
        fields.is_empty(),
        "primitive attributes like m.min are not record fields"
    );
}

#[test]
fn test_collect_structural_integer_fields_from_sibling_reference() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let record_def = DefId::new(100);
    let mut stack_data = ast::Component {
        name: "stackData".to_string(),
        type_def_id: Some(record_def),
        ..ast::Component::empty_with_span(test_span())
    };
    stack_data
        .modifications
        .insert("Ns".to_string(), make_int_expr(3));
    stack_data
        .modifications
        .insert("Np".to_string(), make_int_expr(2));
    stack_data.modifications.insert(
        "cellDataOriginal".to_string(),
        make_comp_ref_expr(&["cellDataOriginal"]),
    );
    effective_components.insert("stackData".to_string(), stack_data);
    let mut tree = ast::ClassTree::default();
    tree.def_map.insert(record_def, "StackData".to_string());
    tree.definitions.classes.insert(
        "StackData".to_string(),
        ast::ClassDef {
            name: make_token("StackData"),
            def_id: Some(record_def),
            class_type: rumoca_core::ClassType::Record,
            ..Default::default()
        },
    );

    let expr = make_comp_ref_expr(&["stackData"]);
    let resolved = collect_structural_integer_fields_from_sibling_reference(
        &expr,
        &ast::ModificationEnvironment::default(),
        &effective_components,
        &tree,
    );
    assert_eq!(resolved.len(), 2);

    let mut seen: IndexMap<String, i64> = IndexMap::default();
    for (name, value) in resolved {
        let ast::Expression::Terminal { token, .. } = value else {
            panic!("expected terminal integer");
        };
        seen.insert(
            name,
            token
                .text
                .parse::<i64>()
                .expect("integer terminal should parse"),
        );
    }

    assert_eq!(seen.get("Ns"), Some(&3));
    assert_eq!(seen.get("Np"), Some(&2));
}

#[cfg(test)]
mod moved_inline_tests {
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
            file_name: TEST_FILE.to_string(),
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
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_named_arg(name: &str, value: ast::Expression) -> ast::Expression {
        ast::Expression::NamedArgument {
            name: make_token(name),
            value: Arc::new(value),
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
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Electrical"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Machines"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Losses"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("CoreParameters"),
                            subs: None,
                        },
                    ],
                    def_id: None,
                    span: rumoca_core::Span::DUMMY,
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
        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
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
        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
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
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Electrical"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Machines"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Losses"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("CoreParameters"),
                            subs: None,
                        },
                    ],
                    def_id: None,
                    span: rumoca_core::Span::DUMMY,
                },
                args: vec![make_int_expr(410), make_int_expr(388)],
                span: rumoca_core::Span::DUMMY,
            },
        );
        effective_components.insert("aimcData".to_string(), data);

        let expr = make_comp_ref_expr(&["aimcData", "statorCoreParameters"]);
        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
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
            ast::ModificationValue::with_prefixes(
                make_int_expr_with_span(1, test_span()),
                false,
                true,
            ),
        );

        let err = insert_scoped_modifier_binding(
            &mut ctx,
            ScopedModifierBinding {
                key,
                value: make_int_expr_with_span(2, test_span()),
                source: Some(make_int_expr_with_span(3, test_span())),
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
            ast::ModificationValue::with_prefixes(
                make_int_expr_with_span(1, test_span()),
                false,
                true,
            ),
        );

        let err = insert_scoped_modifier_binding(
            &mut ctx,
            ScopedModifierBinding {
                key,
                value: make_int_expr(2),
                source: None,
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
    fn test_enum_if_modifier_resolves_in_written_scope() {
        let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
        let mut tau2 = ast::Component {
            name: "tau2".to_string(),
            type_name: make_name("Real"),
            ..ast::Component::empty_with_span(test_span())
        };
        tau2.binding = Some(make_int_expr(30));
        effective_components.insert("tau2".to_string(), tau2);

        let mut eps = ast::Component {
            name: "eps".to_string(),
            type_name: make_name("Real"),
            ..ast::Component::empty_with_span(test_span())
        };
        eps.binding = Some(make_int_expr(0));
        effective_components.insert("eps".to_string(), eps);

        let mut energy_dynamics = ast::Component {
            name: "energyDynamics".to_string(),
            type_name: make_name("Dynamics"),
            ..ast::Component::empty_with_span(test_span())
        };
        energy_dynamics.binding = Some(make_comp_ref_expr(&[
            "Modelica",
            "Fluid",
            "Types",
            "Dynamics",
            "DynamicFreeInitial",
        ]));
        effective_components.insert("energyDynamics".to_string(), energy_dynamics);

        let expr = ast::Expression::If {
            branches: vec![(
                ast::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Arc::new(make_comp_ref_expr(&["tau2"])),
                    rhs: Arc::new(make_comp_ref_expr(&["eps"])),
                    span: rumoca_core::Span::DUMMY,
                },
                make_comp_ref_expr(&["energyDynamics"]),
            )],
            else_branch: Arc::new(make_comp_ref_expr(&[
                "Modelica",
                "Fluid",
                "Types",
                "Dynamics",
                "SteadyState",
            ])),
            span: rumoca_core::Span::DUMMY,
        };

        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
        )
        .expect("enum modifier should resolve in the written scope");

        assert_eq!(
            resolved,
            make_comp_ref_expr(&[
                "Modelica",
                "Fluid",
                "Types",
                "Dynamics",
                "DynamicFreeInitial",
            ])
        );
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
            ..Default::default()
        };
        nested_record.components.insert(
            "phase".to_string(),
            ast::Component::empty_with_span(test_span()),
        );
        nested_record.components.insert(
            "p".to_string(),
            ast::Component::empty_with_span(test_span()),
        );

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
            &binding_expr,
            None,
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
            ast::Expression::FieldAccess { base, field, .. } => {
                assert_eq!(field, "phase");
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

    #[test]
    fn test_propagate_record_binding_preserves_targeted_field_modifiers() {
        let mut nested_record = ast::ClassDef {
            name: make_token("State"),
            class_type: rumoca_core::ClassType::Record,
            ..Default::default()
        };
        nested_record.components.insert(
            "phase".to_string(),
            ast::Component::empty_with_span(test_span()),
        );

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
            &binding_expr,
            None,
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
    }

    #[test]
    fn test_propagate_record_binding_projects_if_expression_branches_per_field() {
        let mut nested_record = ast::ClassDef {
            name: make_token("CellData"),
            class_type: rumoca_core::ClassType::Record,
            ..Default::default()
        };
        nested_record.components.insert(
            "OCV_SOC".to_string(),
            ast::Component::empty_with_span(test_span()),
        );

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
            &binding_expr,
            None,
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
        let ast::Expression::FieldAccess { base, field, .. } = then_expr else {
            panic!("then-branch should project field access");
        };
        assert_eq!(field, "OCV_SOC");
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
    fn test_propagate_record_binding_preserves_matching_default_record_constructor() {
        let mut nested_record = ast::ClassDef {
            name: make_token("BaseData"),
            class_type: rumoca_core::ClassType::Record,
            ..Default::default()
        };
        nested_record.components.insert(
            "mu_i".to_string(),
            ast::Component {
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
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
            args: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
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
            ..Default::default()
        };
        nested_record.components.insert(
            "mu_i".to_string(),
            ast::Component {
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
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
            args: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
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
        let mut inner_record = ast::ClassDef {
            name: make_token("Inner"),
            class_type: rumoca_core::ClassType::Record,
            def_id: Some(inner_def_id),
            ..Default::default()
        };
        inner_record.components.insert(
            "x".to_string(),
            ast::Component::empty_with_span(test_span()),
        );

        let mut outer_record = ast::ClassDef {
            name: make_token("Outer"),
            class_type: rumoca_core::ClassType::Record,
            def_id: Some(outer_def_id),
            ..Default::default()
        };
        outer_record.components.insert(
            "innerParams".to_string(),
            ast::Component {
                type_name: ast::Name {
                    name: vec![make_token("Pkg"), make_token("Inner")],
                    def_id: Some(inner_def_id),
                },
                type_def_id: Some(inner_def_id),
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let mut tree = ast::ClassTree::default();
        tree.definitions
            .classes
            .insert("Pkg.Outer".to_string(), outer_record);
        tree.def_map.insert(inner_def_id, "Pkg.Inner".to_string());
        tree.def_map.insert(outer_def_id, "Pkg.Outer".to_string());

        let mut ctx = InstantiateContext::new();
        let binding_expr = ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("Outer"),
                    subs: None,
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
            args: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        propagate_record_binding_to_fields(
            &tree,
            &mut ctx,
            &binding_expr,
            Some(ast::QualifiedName::from_ident("Pkg")),
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
            &binding_expr,
            None,
            &nested_block,
            &targeted_keys,
        )
        .expect("non-record projection should succeed without mutation");

        assert!(
            ctx.mod_env().active.is_empty(),
            "non-record class modifiers must not synthesize per-field record bindings"
        );
    }
}
