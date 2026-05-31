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
        ..Default::default()
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
        ..Default::default()
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
