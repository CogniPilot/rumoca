//! Resolution of user-defined types (aliases, enumerations, records) to
//! concrete type ids on both the class tree and the instanced overlay.

use super::*;

#[test]
fn test_user_defined_type_resolution() {
    let source = r#"
        type Voltage = Real;
        type Mode = enumeration(Off, On);
        record Payload
            Real x;
        end Payload;

        model Test
            Voltage v;
            Mode m;
            Payload p;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");
    let tree = typed.into_inner();

    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let v_type_id = test
        .components
        .get("v")
        .and_then(|c| c.type_id)
        .expect("v type id");
    let m_type_id = test
        .components
        .get("m")
        .and_then(|c| c.type_id)
        .expect("m type id");
    let p_type_id = test
        .components
        .get("p")
        .and_then(|c| c.type_id)
        .expect("p type id");

    assert!(!v_type_id.is_unknown(), "alias type should resolve");
    assert!(!m_type_id.is_unknown(), "enum type should resolve");
    assert!(!p_type_id.is_unknown(), "record type should resolve");

    assert!(
        matches!(tree.type_table.get(v_type_id), Some(Type::Alias(_))),
        "Voltage should be represented as a Type::Alias"
    );
    assert!(
        matches!(tree.type_table.get(m_type_id), Some(Type::Enumeration(_))),
        "Mode should be represented as a Type::Enumeration"
    );
    assert!(
        matches!(
            tree.type_table.get(p_type_id),
            Some(Type::Class(cls)) if cls.kind == ClassKind::Record
        ),
        "Payload should be represented as a record class type"
    );
}

#[test]
fn test_typecheck_instanced_populates_user_defined_type_ids() {
    let source = r#"
        type Voltage = Real;
        type Mode = enumeration(Off, On);
        model Test
            Voltage v;
            Mode m;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    let v_decl = test.components.get("v").expect("v declaration");
    let m_decl = test.components.get("m").expect("m declaration");

    let mut overlay = InstanceOverlay::new();
    let v_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: v_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_ident("v"),
        // Seed with builtin Real id to verify instanced typecheck rewrites
        // to declared alias identity (Voltage), not just UNKNOWN placeholders.
        type_id: tree.type_table.real(),
        type_name: "Voltage".to_string(),
        type_def_id: v_decl.type_def_id,
        is_primitive: true,
        ..Default::default()
    });
    let m_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: m_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_ident("m"),
        type_id: TypeId::UNKNOWN,
        type_name: "Mode".to_string(),
        type_def_id: m_decl.type_def_id,
        is_primitive: true,
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Test").expect("typecheck_instanced should pass");

    let v_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "v")
        .expect("v instance");
    let m_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "m")
        .expect("m instance");

    assert!(
        !v_inst.type_id.is_unknown(),
        "instanced alias type should resolve"
    );
    assert_ne!(
        v_inst.type_id,
        tree.type_table.real(),
        "alias type should preserve declared identity, not collapse to builtin Real"
    );
    assert!(
        !m_inst.type_id.is_unknown(),
        "instanced enum type should resolve"
    );
}
