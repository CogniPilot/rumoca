//! Type-name resolution and binding scopes in the instanced pipeline: suffix
//! and dotted-anchor lookup, projected field types, and modifier source
//! scopes.

use super::*;

#[test]
fn test_typecheck_instanced_resolves_unique_suffix_type_name() {
    let source = r#"
        package A
            package Units
                type Reluctance = Real;
            end Units;
        end A;

        model Test
            A.Units.Reluctance r;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .get_class_by_qualified_name("Test")
        .expect("Test class should resolve");
    let r_decl = test.components.get("r").expect("r declaration");
    let mut overlay = InstanceOverlay::new();
    let id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: id,
        qualified_name: QualifiedName::from_dotted("Test.r"),
        type_id: TypeId::UNKNOWN,
        // Simulate an instanced relative/imported type path.
        type_name: "Units.Reluctance".to_string(),
        type_def_id: None,
        is_primitive: true,
        source_location: r_decl.location.clone(),
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("instanced typecheck should resolve unique suffix type names");

    let r_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "Test.r")
        .expect("r instance");
    assert!(
        !r_inst.type_id.is_unknown(),
        "unique suffix type should resolve"
    );
}

#[test]
fn test_typecheck_instanced_rejects_ambiguous_suffix_type_name() {
    let source = r#"
        package A
            package Units
                type Reluctance = Real;
            end Units;
        end A;

        package B
            package Units
                type Reluctance = Real;
            end Units;
        end B;

        model Test
            A.Units.Reluctance r;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .get_class_by_qualified_name("Test")
        .expect("Test class should resolve");
    let r_decl = test.components.get("r").expect("r declaration");
    let mut overlay = InstanceOverlay::new();
    let id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: id,
        qualified_name: QualifiedName::from_dotted("Test.r"),
        type_id: TypeId::UNKNOWN,
        // Ambiguous between A.Units.Reluctance and B.Units.Reluctance.
        type_name: "Units.Reluctance".to_string(),
        type_def_id: None,
        is_primitive: true,
        source_location: r_decl.location.clone(),
        ..Default::default()
    });

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("ambiguous suffix type names should remain unresolved");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET001")
            && d.message.contains("undefined type 'Units.Reluctance'")),
        "expected unresolved-type diagnostic for ambiguous suffix, got: {:?}",
        err
    );
}

#[test]
fn test_typecheck_instanced_resolves_dotted_type_via_anchor_def_id() {
    let source = r#"
        package Outer
            package Medium
                type AbsolutePressure = Real;
            end Medium;

            model Test
                Medium.AbsolutePressure p;
            end Test;
        end Outer;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");
    let tree = typed.into_inner();

    let medium_def_id = tree
        .name_map
        .get("Outer.Medium")
        .copied()
        .expect("Outer.Medium should resolve");
    let medium_package_type = tree
        .type_table
        .lookup("Outer.Medium")
        .expect("Outer.Medium package type should exist");
    let test = tree
        .get_class_by_qualified_name("Outer.Test")
        .expect("Outer.Test class should resolve");
    let p_decl = test.components.get("p").expect("p declaration");

    let mut overlay = InstanceOverlay::new();
    let id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: id,
        qualified_name: QualifiedName::from_dotted("Outer.Test.p"),
        type_id: TypeId::UNKNOWN,
        type_name: "Medium.AbsolutePressure".to_string(),
        // Anchor only the first segment (`Medium`) and require dotted-tail resolution.
        type_def_id: Some(medium_def_id),
        is_primitive: true,
        source_location: p_decl.location.clone(),
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Outer.Test")
        .expect("instanced typecheck should resolve anchored dotted type names");

    let p_inst = overlay
        .components
        .values()
        .find(|d| d.qualified_name.to_flat_string() == "Outer.Test.p")
        .expect("p instance");
    assert!(
        !p_inst.type_id.is_unknown(),
        "anchored dotted type should resolve"
    );
    assert_ne!(
        p_inst.type_id, medium_package_type,
        "dotted type must not collapse to anchor package type"
    );
}

#[test]
fn test_typecheck_instanced_detects_user_defined_equation_mismatch() {
    let source = r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;
        model Test
            LeftPayload lhs;
            RightPayload rhs;
        equation
            lhs = rhs;
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

    let lhs_decl = test.components.get("lhs").expect("lhs declaration");
    let rhs_decl = test.components.get("rhs").expect("rhs declaration");

    let mut overlay = InstanceOverlay::new();
    let lhs_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: lhs_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_dotted("Test.lhs"),
        type_id: TypeId::UNKNOWN,
        type_name: "LeftPayload".to_string(),
        type_def_id: lhs_decl.type_def_id,
        is_primitive: false,
        ..Default::default()
    });
    let rhs_id = overlay.alloc_id();
    overlay.add_component(rumoca_ir_ast::InstanceData {
        instance_id: rhs_id,
        qualified_name: rumoca_ir_ast::QualifiedName::from_dotted("Test.rhs"),
        type_id: TypeId::UNKNOWN,
        type_name: "RightPayload".to_string(),
        type_def_id: rhs_decl.type_def_id,
        is_primitive: false,
        ..Default::default()
    });

    let err = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("instanced mismatch should fail typecheck");
    assert!(
        err.iter().any(|d| d.code.as_deref() == Some("ET002")),
        "expected ET002 diagnostics for instanced user-defined type mismatch"
    );
}

#[test]
fn test_typecheck_instanced_uses_effective_projected_field_type() {
    let source = r#"
        record BasePayload
            Real x;
        end BasePayload;
        record ExtendedPayload
            extends BasePayload;
            Real extra;
        end ExtendedPayload;
        record Holder
            BasePayload payload;
        end Holder;
        model Test
            Holder holder;
            ExtendedPayload projected;
        end Test;
    "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .get_class_by_qualified_name("Test")
        .expect("Test class");
    let extended = tree
        .get_class_by_qualified_name("ExtendedPayload")
        .expect("extended record");
    let extra = extended.components.get("extra").expect("extra field");

    let mut overlay = InstanceOverlay::new();
    add_instanced_component(
        &mut overlay,
        "holder",
        test.components.get("holder").expect("holder component"),
        false,
    );
    let payload_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: payload_id,
        qualified_name: QualifiedName::from_dotted("holder.payload"),
        type_name: "ExtendedPayload".to_string(),
        type_def_id: extended.def_id,
        is_primitive: false,
        ..Default::default()
    });
    add_instanced_component(&mut overlay, "holder.payload.extra", extra, true);

    let projected = test
        .components
        .get("projected")
        .expect("projected component");
    let projected_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: projected_id,
        qualified_name: QualifiedName::from_ident("projected"),
        source_location: projected.location.clone(),
        type_name: projected.type_name.to_string(),
        type_def_id: projected.type_def_id,
        binding: Some(Expression::FieldAccess {
            base: Arc::new(Expression::ComponentReference(make_comp_ref("holder"))),
            field: "payload".to_string(),
            span: rumoca_core::Span::DUMMY,
        }),
        is_primitive: false,
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("effective projected record type should be preserved");
}

#[test]
fn test_typecheck_instanced_uses_modifier_source_scope_for_bindings() {
    let source = r#"
        record LeftPayload
            Real x;
        end LeftPayload;
        record RightPayload
            Real x;
        end RightPayload;
        model Holder
            LeftPayload payload;
        end Holder;
        model Test
            RightPayload payload;
            Holder holder;
        end Test;
    "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .get_class_by_qualified_name("Test")
        .expect("Test class");
    let holder = tree
        .get_class_by_qualified_name("Holder")
        .expect("Holder class");

    let mut overlay = InstanceOverlay::new();
    add_instanced_component(
        &mut overlay,
        "payload",
        test.components.get("payload").expect("outer payload"),
        false,
    );
    add_instanced_component(
        &mut overlay,
        "holder",
        test.components.get("holder").expect("holder component"),
        false,
    );
    let nested = holder.components.get("payload").expect("nested payload");
    let mut outer_payload_ref = make_comp_ref("payload");
    outer_payload_ref.parts[0].ident.location = nested.location.clone();
    let nested_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: nested_id,
        qualified_name: QualifiedName::from_dotted("holder.payload"),
        source_location: nested.location.clone(),
        type_name: nested.type_name.to_string(),
        type_def_id: nested.type_def_id,
        binding: Some(Expression::ComponentReference(outer_payload_ref)),
        binding_source_scope: Some(QualifiedName::new()),
        binding_from_modification: true,
        is_primitive: false,
        ..Default::default()
    });

    let diagnostics = typecheck_instanced(&tree, &mut overlay, "Test")
        .expect_err("outer RightPayload must not capture nested LeftPayload");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ET002")),
        "expected source-scoped binding mismatch, got: {diagnostics:?}"
    );
}

#[test]
fn test_typecheck_instanced_uses_modifier_source_scope_for_structural_values() {
    let source = r#"
        model Holder
            parameter Integer n = 0;
            Real x[n];
        end Holder;
        model Test
            parameter Integer n = 2;
            Holder holder;
        end Test;
    "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.into_inner();
    let test = tree
        .get_class_by_qualified_name("Test")
        .expect("Test class");
    let holder = tree
        .get_class_by_qualified_name("Holder")
        .expect("Holder class");

    let mut overlay = InstanceOverlay::new();
    add_instanced_component(
        &mut overlay,
        "n",
        test.components.get("n").expect("outer n"),
        true,
    );
    add_instanced_component(
        &mut overlay,
        "holder",
        test.components.get("holder").expect("holder component"),
        false,
    );

    let nested_n = holder.components.get("n").expect("nested n");
    let nested_n_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: nested_n_id,
        qualified_name: QualifiedName::from_dotted("holder.n"),
        source_location: nested_n.location.clone(),
        type_name: nested_n.type_name.to_string(),
        type_def_id: nested_n.type_def_id,
        variability: nested_n.variability.clone(),
        binding: Some(Expression::ComponentReference(make_comp_ref("n"))),
        binding_source_scope: Some(QualifiedName::new()),
        binding_from_modification: true,
        start: Some(Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: "0".into(),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }),
        is_primitive: true,
        ..Default::default()
    });

    let nested_x = holder.components.get("x").expect("nested x");
    let nested_x_id = overlay.alloc_id();
    overlay.add_component(InstanceData {
        instance_id: nested_x_id,
        qualified_name: QualifiedName::from_dotted("holder.x"),
        source_location: nested_x.location.clone(),
        type_name: nested_x.type_name.to_string(),
        type_def_id: nested_x.type_def_id,
        dims_expr: nested_x.shape_expr.clone(),
        is_primitive: true,
        ..Default::default()
    });

    typecheck_instanced(&tree, &mut overlay, "Test")
        .expect("source-scoped structural parameter should typecheck");
    let nested_x = overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "holder.x")
        .expect("holder.x instance");
    assert_eq!(nested_x.dims, vec![2]);
}
