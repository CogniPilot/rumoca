use super::*;
use std::sync::Arc;

/// Create a minimal component for testing.
fn make_component(name: &str, is_replaceable: bool, is_final: bool) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        is_replaceable,
        is_final,
        ..Default::default()
    }
}

/// Create a component reference for testing.
fn make_component_ref(name: &str) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_ir_core::Token {
                text: std::sync::Arc::from(name),
                location: rumoca_ir_core::Location::default(),
                token_number: 0,
                token_type: 0,
            },
            subs: None,
        }],
        def_id: None,
    }
}

/// Create a token for testing.
fn make_token(text: &str) -> rumoca_ir_core::Token {
    rumoca_ir_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_ir_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

/// Create a Name for testing.
fn make_name(text: &str) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: vec![make_token(text)],
        def_id: None,
    }
}

#[test]
fn test_validate_redeclaration_non_replaceable() {
    // A non-replaceable component should fail redeclaration
    let tree = ast::ClassTree::default();
    let comp = make_component("x", false, false);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("not replaceable"));
}

#[test]
fn test_validate_redeclaration_final() {
    // A final component should fail redeclaration (even if replaceable)
    let tree = ast::ClassTree::default();
    let comp = make_component("x", true, true);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("final"));
}

#[test]
fn test_validate_redeclaration_replaceable() {
    // A replaceable, non-final component should succeed
    let tree = ast::ClassTree::default();
    let comp = make_component("x", true, false);
    let result = validate_redeclaration(&tree, &comp, "x", None, Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_validate_redeclaration_constant_rejected() {
    let tree = ast::ClassTree::default();
    let mut comp = make_component("x", true, false);
    comp.variability = rumoca_ir_core::Variability::Constant(make_token("constant"));
    let result = validate_redeclaration(&tree, &comp, "x", Some("Real"), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.to_string()
            .contains("constant elements cannot be redeclared")
    );
}

#[test]
fn test_classes_are_compatible_for_equivalent_declarations() {
    let component = ast::Component {
        name: "k".to_string(),
        name_token: make_token("k"),
        type_name: make_name("Real"),
        variability: rumoca_ir_core::Variability::Parameter(make_token("parameter")),
        start: ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
        },
        has_explicit_binding: true,
        binding: Some(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
        }),
        ..Default::default()
    };

    let helper_a = ast::ClassDef {
        name: make_token("Helper"),
        class_type: ast::ClassType::Model,
        components: IndexMap::from([("k".to_string(), component.clone())]),
        ..Default::default()
    };
    let helper_b = ast::ClassDef {
        name: make_token("Helper"),
        class_type: ast::ClassType::Model,
        components: IndexMap::from([("k".to_string(), component)]),
        ..Default::default()
    };

    assert!(classes_are_compatible(&helper_a, &helper_b));
}

// -------------------------------------------------------------------------
// Constrainedby validation tests (MLS §7.3.2)
// -------------------------------------------------------------------------

/// Create a component with constrainedby for testing.
fn make_constrained_component(
    name: &str,
    type_name: &str,
    constrainedby: Option<&str>,
) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        type_name: make_name(type_name),
        is_replaceable: true,
        is_final: false,
        constrainedby: constrainedby.map(make_name),
        ..Default::default()
    }
}

#[test]
fn test_constrainedby_exact_match() {
    // Redeclaring with exact same type as constraint should succeed
    let tree = ast::ClassTree::default();
    let comp = make_constrained_component("x", "Real", Some("Real"));
    let result = validate_redeclaration(&tree, &comp, "x", Some("Real"), Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_constrainedby_violation_builtin() {
    // Redeclaring Real constrained component to Integer should fail
    let tree = ast::ClassTree::default();
    let comp = make_constrained_component("x", "Real", Some("Real"));
    let result = validate_redeclaration(&tree, &comp, "x", Some("Integer"), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("violates constrainedby"));
}

#[test]
fn test_constrainedby_default_uses_original_type() {
    // When no constrainedby is specified, the original type is the constraint
    let tree = ast::ClassTree::default();
    let comp = make_constrained_component("x", "Real", None);
    // Redeclaring to Integer should fail (Real is implicit constraint)
    let result = validate_redeclaration(&tree, &comp, "x", Some("Integer"), Span::DUMMY);
    assert!(result.is_err());
}

#[test]
fn test_constrainedby_subtype_allowed() {
    // Redeclaring to a subtype of the constraint should succeed

    let mut tree = ast::ClassTree::default();

    // Create base class
    let base = ast::ClassDef {
        name: make_token("BaseConnector"),
        ..Default::default()
    };

    // Create derived class that extends base
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

    // ast::Component constrained to BaseConnector
    let comp = make_constrained_component("c", "BaseConnector", Some("BaseConnector"));

    // Redeclaring to DerivedConnector (a subtype) should succeed
    let result = validate_redeclaration(&tree, &comp, "c", Some("DerivedConnector"), Span::DUMMY);
    assert!(result.is_ok());
}

#[test]
fn test_constrainedby_non_subtype_rejected() {
    // Redeclaring to a non-subtype should fail
    let mut tree = ast::ClassTree::default();

    // Create two unrelated classes
    let class_a = ast::ClassDef {
        name: make_token("ClassA"),
        ..Default::default()
    };
    let class_b = ast::ClassDef {
        name: make_token("ClassB"),
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("ClassA".to_string(), class_a);
    tree.definitions
        .classes
        .insert("ClassB".to_string(), class_b);

    // ast::Component constrained to ClassA
    let comp = make_constrained_component("c", "ClassA", Some("ClassA"));

    // Redeclaring to ClassB (not a subtype) should fail
    let result = validate_redeclaration(&tree, &comp, "c", Some("ClassB"), Span::DUMMY);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("violates constrainedby"));
}

// -------------------------------------------------------------------------
// is_type_subtype tests
// -------------------------------------------------------------------------

#[test]
fn test_is_type_subtype_exact_match() {
    let tree = ast::ClassTree::default();
    assert!(is_type_subtype(&tree, "Real", "Real"));
    assert!(is_type_subtype(&tree, "MyClass", "MyClass"));
}

#[test]
fn test_is_type_subtype_builtin_mismatch() {
    let tree = ast::ClassTree::default();
    assert!(!is_type_subtype(&tree, "Real", "Integer"));
    assert!(!is_type_subtype(&tree, "Boolean", "String"));
}

#[test]
fn test_is_type_subtype_via_extends() {
    let mut tree = ast::ClassTree::default();

    // A extends nothing
    let class_a = ast::ClassDef {
        name: make_token("A"),
        ..Default::default()
    };

    // B extends A
    let class_b = ast::ClassDef {
        name: make_token("B"),
        extends: vec![ast::Extend {
            base_name: make_name("A"),
            ..Default::default()
        }],
        ..Default::default()
    };

    // C extends B (transitive: C -> B -> A)
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
    tree.definitions.classes.insert("C".to_string(), class_c);

    // B is subtype of A
    assert!(is_type_subtype(&tree, "B", "A"));
    // C is subtype of B
    assert!(is_type_subtype(&tree, "C", "B"));
    // C is subtype of A (transitive)
    assert!(is_type_subtype(&tree, "C", "A"));
    // A is NOT subtype of B
    assert!(!is_type_subtype(&tree, "A", "B"));
}

#[test]
fn test_class_extends_cached_matches_base_def_id_for_relative_extends_name() {
    use rumoca_core::DefId;

    let mut tree = ast::ClassTree::default();

    let c_id = DefId::new(1);
    let interfaces_id = DefId::new(2);
    let d_id = DefId::new(3);
    let pkg_id = DefId::new(4);
    let root_id = DefId::new(5);

    let class_c = ast::ClassDef {
        def_id: Some(c_id),
        name: make_token("C"),
        ..Default::default()
    };

    let mut class_interfaces = ast::ClassDef {
        def_id: Some(interfaces_id),
        name: make_token("Interfaces"),
        class_type: ast::ClassType::Package,
        ..Default::default()
    };
    class_interfaces
        .classes
        .insert("C".to_string(), class_c.clone());

    let class_d = ast::ClassDef {
        def_id: Some(d_id),
        name: make_token("D"),
        extends: vec![ast::Extend {
            // Relative name intentionally omits top-level prefix.
            base_name: make_name("Pkg.Interfaces.C"),
            base_def_id: Some(c_id),
            ..Default::default()
        }],
        ..Default::default()
    };

    let mut class_pkg = ast::ClassDef {
        def_id: Some(pkg_id),
        name: make_token("Pkg"),
        class_type: ast::ClassType::Package,
        ..Default::default()
    };
    class_pkg
        .classes
        .insert("Interfaces".to_string(), class_interfaces);
    class_pkg.classes.insert("D".to_string(), class_d);

    let mut class_root = ast::ClassDef {
        def_id: Some(root_id),
        name: make_token("Root"),
        class_type: ast::ClassType::Package,
        ..Default::default()
    };
    class_root.classes.insert("Pkg".to_string(), class_pkg);
    tree.definitions
        .classes
        .insert("Root".to_string(), class_root);

    tree.def_map
        .insert(c_id, "Root.Pkg.Interfaces.C".to_string());
    tree.def_map
        .insert(interfaces_id, "Root.Pkg.Interfaces".to_string());
    tree.def_map.insert(d_id, "Root.Pkg.D".to_string());
    tree.def_map.insert(pkg_id, "Root.Pkg".to_string());
    tree.def_map.insert(root_id, "Root".to_string());

    tree.name_map
        .insert("Root.Pkg.Interfaces.C".to_string(), c_id);
    tree.name_map
        .insert("Root.Pkg.Interfaces".to_string(), interfaces_id);
    tree.name_map.insert("Root.Pkg.D".to_string(), d_id);
    tree.name_map.insert("Root.Pkg".to_string(), pkg_id);
    tree.name_map.insert("Root".to_string(), root_id);

    let d_class = tree
        .get_class_by_qualified_name("Root.Pkg.D")
        .expect("Root.Pkg.D class should exist");
    let mut cache = SubtypeCache::new();
    assert!(
        class_extends_cached(&tree, d_class, "Interfaces.C", &mut cache),
        "relative extends with base_def_id should match short queried supertype"
    );
}

#[test]
fn test_extract_modification_target_modification() {
    // Test extracting target from ast::Expression::Modification
    let expr = ast::Expression::Modification {
        target: make_component_ref("myVar"),
        value: Arc::new(ast::Expression::Empty),
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("myVar".to_string())
    );
}

#[test]
fn test_extract_modification_target_class_modification() {
    // Test extracting target from ast::Expression::ClassModification
    let expr = ast::Expression::ClassModification {
        target: make_component_ref("myClass"),
        modifications: vec![],
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("myClass".to_string())
    );
}

#[test]
fn test_extract_modification_target_named_argument() {
    // Test extracting target from ast::Expression::NamedArgument
    let expr = ast::Expression::NamedArgument {
        name: make_token("param"),
        value: Arc::new(ast::Expression::Empty),
    };
    assert_eq!(
        extract_modification_target(&expr),
        Some("param".to_string())
    );
}

#[test]
fn test_is_effectively_primitive_transitive_enumeration_chain() {
    // Test that type alias chains leading to enumerations are detected as primitive
    // type Logic = enumeration(...)
    // connector DigitalSignal = Logic
    // connector DigitalInput = input DigitalSignal

    // Create a tree with these classes
    let mut tree = ast::ClassTree::new();

    // Logic enumeration
    let mut logic = ast::ClassDef {
        name: make_token("Logic"),
        ..Default::default()
    };
    logic.enum_literals.push(ast::EnumLiteral {
        ident: make_token("U"),
        description: vec![],
    });
    logic.enum_literals.push(ast::EnumLiteral {
        ident: make_token("X"),
        description: vec![],
    });

    // DigitalSignal = Logic
    let digital_signal = ast::ClassDef {
        name: make_token("DigitalSignal"),
        extends: vec![ast::Extend {
            base_name: make_name("Logic"),
            ..Default::default()
        }],
        ..Default::default()
    };

    // DigitalInput = input DigitalSignal
    let digital_input = ast::ClassDef {
        name: make_token("DigitalInput"),
        extends: vec![ast::Extend {
            base_name: make_name("DigitalSignal"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions.classes.insert("Logic".to_string(), logic);
    tree.definitions
        .classes
        .insert("DigitalSignal".to_string(), digital_signal);
    tree.definitions
        .classes
        .insert("DigitalInput".to_string(), digital_input.clone());

    // DigitalInput should be effectively primitive because it chains to Logic (enumeration)
    assert!(is_effectively_primitive_transitive(&tree, &digital_input));
}
