use super::*;
use rumoca_ir_ast as ast;

/// Helper to create a token with text for testing.
fn make_token(text: &str) -> rumoca_ir_core::Token {
    rumoca_ir_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_ir_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

/// Helper to create a Name for testing.
fn make_name(text: &str) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: vec![make_token(text)],
        def_id: None,
    }
}

fn make_int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(&value.to_string()),
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
    })
}

#[test]
fn test_equations_to_instance_without_connections_filters_connect_equations() {
    let equations = vec![
        ast::Equation::Connect {
            lhs: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("a"),
                    subs: None,
                }],
                def_id: None,
            },
            rhs: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("b"),
                    subs: None,
                }],
                def_id: None,
            },
        },
        ast::Equation::Simple {
            lhs: make_comp_ref_expr(&["x"]),
            rhs: make_int_expr(1),
        },
    ];

    let source_map = rumoca_core::SourceMap::new();
    let origin = ast::QualifiedName::from_ident("M");
    let converted = equations_to_instance_without_connections(&equations, &origin, &source_map);

    assert_eq!(converted.len(), 1);
    assert!(matches!(
        converted[0].equation,
        ast::Equation::Simple { .. }
    ));
    assert_eq!(converted[0].origin, origin);
}

#[test]
fn test_context_path() {
    let mut ctx = InstantiateContext::new();
    assert!(ctx.current_path().is_empty());

    ctx.push_path("model");
    ctx.push_path("component");
    assert_eq!(ctx.current_path().to_flat_string(), "model.component");

    ctx.pop_path();
    assert_eq!(ctx.current_path().to_flat_string(), "model");
}

#[test]
fn test_extract_int_params_record_alias_prefers_rebound_field_values() {
    // Reproduces CellRCStack pattern:
    // parameter Data cellData; parameter Data cellData2(nRC=2); ... cellData=cellData2
    // For-loop ranges over cellData.nRC must use 2, not cellData's default 1.
    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_dotted("cellData.nRC"),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    mod_env.add(
        ast::QualifiedName::from_dotted("cellData2.nRC"),
        ast::ModificationValue::simple(make_int_expr(2)),
    );
    mod_env.add(
        ast::QualifiedName::from_ident("cellData"),
        ast::ModificationValue::simple(make_comp_ref_expr(&["cellData2"])),
    );

    let effective_components = IndexMap::new();
    let tree = ast::ClassTree::default();
    let int_params = extract_int_params_with_mods(&effective_components, &mod_env, &tree);

    assert_eq!(
        int_params.get("cellData2.nRC"),
        Some(&2),
        "target record field should be present"
    );
    assert_eq!(
        int_params.get("cellData.nRC"),
        Some(&2),
        "aliased record field should override stale/default value"
    );
}

// -------------------------------------------------------------------------
// Inner/Outer tests (MLS §5.4)
// -------------------------------------------------------------------------

#[test]
fn test_register_and_find_inner() {
    let mut ctx = InstantiateContext::new();

    // Register an inner declaration
    let qn = ast::QualifiedName::from_dotted("system.world");
    ctx.register_inner("world", qn, "World", None);

    // Should be able to find it
    let inner = ctx.find_inner("world");
    assert!(inner.is_some());
    assert_eq!(
        inner.unwrap().qualified_name.to_flat_string(),
        "system.world"
    );
    assert_eq!(inner.unwrap().type_name, "World");

    // Non-existent inner should not be found
    assert!(ctx.find_inner("nonexistent").is_none());
}

#[test]
fn test_inner_scope_visibility() {
    let mut ctx = InstantiateContext::new();

    // Register an inner in the root scope
    let qn = ast::QualifiedName::from_dotted("root.g");
    ctx.register_inner("g", qn, "Real", None);

    // Push a new scope (entering a nested component)
    ctx.push_inner_scope();

    // Inner from parent scope should still be visible
    assert!(ctx.find_inner("g").is_some());

    // Register a different inner in the nested scope
    let qn2 = ast::QualifiedName::from_dotted("root.nested.x");
    ctx.register_inner("x", qn2, "Real", None);
    assert!(ctx.find_inner("x").is_some());

    // Pop the scope
    ctx.pop_inner_scope();

    // Inner from root scope should still be visible
    assert!(ctx.find_inner("g").is_some());

    // Inner from nested scope should NOT be visible anymore
    assert!(ctx.find_inner("x").is_none());
}

#[test]
fn test_inner_shadowing() {
    // MLS §5.4: An outer element references the closest inner element
    let mut ctx = InstantiateContext::new();

    // Register "g" in root scope
    let qn_outer = ast::QualifiedName::from_dotted("root.g");
    ctx.register_inner("g", qn_outer, "Real", None);

    // Push a new scope and register another "g"
    ctx.push_inner_scope();
    let qn_inner = ast::QualifiedName::from_dotted("root.nested.g");
    ctx.register_inner("g", qn_inner, "Real", None);

    // Should find the inner (closer) "g", not the outer one
    let inner = ctx.find_inner("g").unwrap();
    assert_eq!(inner.qualified_name.to_flat_string(), "root.nested.g");

    // Pop the scope
    ctx.pop_inner_scope();

    // Now should find the outer "g"
    let inner = ctx.find_inner("g").unwrap();
    assert_eq!(inner.qualified_name.to_flat_string(), "root.g");
}

// -------------------------------------------------------------------------
// Type compatibility tests (MLS §5.4)
// -------------------------------------------------------------------------

#[test]
fn test_type_compatible_exact_match() {
    // Exact type name match is always compatible
    let tree = ast::ClassTree::default();
    assert!(is_type_compatible(&tree, "Real", "Real"));
    assert!(is_type_compatible(&tree, "MyConnector", "MyConnector"));
}

#[test]
fn test_type_compatible_builtin_mismatch() {
    // Built-in types must match exactly
    let tree = ast::ClassTree::default();
    assert!(!is_type_compatible(&tree, "Real", "Integer"));
    assert!(!is_type_compatible(&tree, "Boolean", "String"));
    assert!(!is_type_compatible(&tree, "Real", "Boolean"));
}

#[test]
fn test_type_compatible_class_inheritance() {
    // Test that a derived class is compatible with its base
    // Create a simple class hierarchy: DerivedConnector extends BaseConnector
    let mut tree = ast::ClassTree::default();

    // Base class
    let base = ast::ClassDef {
        name: make_token("BaseConnector"),
        ..Default::default()
    };

    // Derived class that extends Base
    let derived = ast::ClassDef {
        name: make_token("DerivedConnector"),
        extends: vec![ast::Extend {
            base_name: make_name("BaseConnector"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("BaseConnector".to_string(), base);
    tree.definitions
        .classes
        .insert("DerivedConnector".to_string(), derived);

    // DerivedConnector should be compatible with BaseConnector (subtype)
    assert!(is_type_compatible(
        &tree,
        "BaseConnector",
        "DerivedConnector"
    ));

    // BaseConnector is NOT compatible with DerivedConnector (not a subtype)
    assert!(!is_type_compatible(
        &tree,
        "DerivedConnector",
        "BaseConnector"
    ));
}

#[test]
fn test_class_extends_direct() {
    let tree = ast::ClassTree::default();

    // Create a class that directly extends BaseConnector
    let derived = ast::ClassDef {
        name: make_token("Derived"),
        extends: vec![ast::Extend {
            base_name: make_name("BaseConnector"),
            ..Default::default()
        }],
        ..Default::default()
    };

    assert!(class_extends(&tree, &derived, "BaseConnector"));
    assert!(!class_extends(&tree, &derived, "OtherClass"));
}

#[test]
fn test_class_extends_transitive() {
    // Create hierarchy: C extends B extends A
    let mut tree = ast::ClassTree::default();

    let class_a = ast::ClassDef {
        name: make_token("A"),
        ..Default::default()
    };

    let class_b = ast::ClassDef {
        name: make_token("B"),
        extends: vec![ast::Extend {
            base_name: make_name("A"),
            ..Default::default()
        }],
        ..Default::default()
    };

    let class_c = ast::ClassDef {
        name: make_token("C"),
        extends: vec![ast::Extend {
            base_name: make_name("B"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions.classes.insert("A".to_string(), class_a);
    tree.definitions.classes.insert("B".to_string(), class_b);
    tree.definitions
        .classes
        .insert("C".to_string(), class_c.clone());

    // C extends B directly
    assert!(class_extends(&tree, &class_c, "B"));
    // C extends A transitively (through B)
    assert!(class_extends(&tree, &class_c, "A"));
    // C does not extend D
    assert!(!class_extends(&tree, &class_c, "D"));
}
