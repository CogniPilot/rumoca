//! Collision regressions for DefId/InstanceId-backed typecheck scopes.

use super::*;

fn identity_instance(
    overlay: &mut InstanceOverlay,
    path: &str,
    source_def_id: DefId,
    type_id: TypeId,
) -> InstanceId {
    semantic_instance(
        overlay,
        path,
        source_def_id,
        type_id,
        rumoca_core::Variability::Continuous(Token::default()),
        Vec::new(),
        Vec::new(),
    )
}

fn semantic_instance(
    overlay: &mut InstanceOverlay,
    path: &str,
    source_def_id: DefId,
    type_id: TypeId,
    variability: rumoca_core::Variability,
    instance_subscripts: Vec<i64>,
    dims: Vec<i64>,
) -> InstanceId {
    let instance_id = overlay.alloc_id();
    let mut qualified_name = QualifiedName::from_dotted(path);
    let declared_part = qualified_name
        .parts
        .last_mut()
        .expect("test instance path must not be empty");
    // The declaration reference names the innermost segment of the instance
    // path; take it from the structured qualified name, not from the text.
    let declared_ident = declared_part.0.clone();
    declared_part.1 = instance_subscripts;
    overlay.add_component(InstanceData {
        instance_id,
        component_ref: Some(
            rumoca_core::ComponentReference::construct(
                false,
                rumoca_core::Span::DUMMY,
                vec![rumoca_core::ComponentRefPart {
                    ident: declared_ident,
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                    def_id: source_def_id,
                }],
            )
            .expect("test declaration reference has exact identity"),
        ),
        qualified_name,
        type_id,
        variability,
        dims,
        ..Default::default()
    });
    instance_id
}

fn set_instance_owner(overlay: &mut InstanceOverlay, path: &str, owner: InstanceId) {
    let data = overlay
        .components
        .values_mut()
        .find(|data| data.qualified_name.to_flat_string() == path)
        .expect("test instance path must exist");
    data.owner_class_id = Some(owner);
}

fn add_owner_class(overlay: &mut InstanceOverlay, owner: InstanceId, component: InstanceId) {
    overlay.add_class(rumoca_ir_ast::ClassInstanceData {
        instance_id: owner,
        owner_component_id: Some(component),
        ..Default::default()
    });
}

fn add_outer_member_case(
    overlay: &mut InstanceOverlay,
    owner: InstanceId,
    scenario: (&str, &str, &str, &str, u32),
    system_def_id: DefId,
    value_def_id: DefId,
) {
    let (_, outer_path, inner_path, member_path, member_type_id) = scenario;
    let outer_id = identity_instance(overlay, outer_path, system_def_id, TypeId::new(20));
    let inner_id = identity_instance(overlay, inner_path, system_def_id, TypeId::new(21));
    identity_instance(
        overlay,
        member_path,
        value_def_id,
        TypeId::new(member_type_id),
    );
    set_instance_owner(overlay, outer_path, owner);
    add_owner_class(overlay, owner, outer_id);
    let inner_owner = InstanceId(owner.0 + 1000);
    set_instance_owner(overlay, member_path, inner_owner);
    add_owner_class(overlay, inner_owner, inner_id);
    overlay.outer_prefix_to_inner.insert(
        ComponentPath::from_flat_path(outer_path),
        ComponentPath::from_flat_path(inner_path),
    );
}

fn add_ambiguous_outer_case(
    overlay: &mut InstanceOverlay,
    system_def_id: DefId,
    value_def_id: DefId,
) -> InstanceId {
    let ambiguous_owner = InstanceId(999);
    let ambiguous_outer = identity_instance(
        overlay,
        "Root.ambiguous.outer.system",
        system_def_id,
        TypeId::new(22),
    );
    let ambiguous_inner = identity_instance(
        overlay,
        "Root.ambiguous.system",
        system_def_id,
        TypeId::new(23),
    );
    identity_instance(
        overlay,
        "Root.ambiguous.system.allowFlowReversal",
        value_def_id,
        TypeId::new(24),
    );
    let ambiguous_value_2 = identity_instance(
        overlay,
        "Root.ambiguous.system.allowFlowReversal",
        value_def_id,
        TypeId::new(25),
    );
    set_instance_owner(overlay, "Root.ambiguous.outer.system", ambiguous_owner);
    add_owner_class(overlay, ambiguous_owner, ambiguous_outer);
    let ambiguous_inner_owner = InstanceId(1999);
    set_instance_owner(
        overlay,
        "Root.ambiguous.system.allowFlowReversal",
        ambiguous_inner_owner,
    );
    // Give both colliding member occurrences the same inner owner so the
    // identity walk reaches both candidates and reports ambiguity.
    overlay
        .components
        .get_mut(&ambiguous_value_2)
        .expect("ambiguous second member exists")
        .owner_class_id = Some(ambiguous_inner_owner);
    add_owner_class(overlay, ambiguous_inner_owner, ambiguous_inner);
    overlay.outer_prefix_to_inner.insert(
        ComponentPath::from_flat_path("Root.ambiguous.outer.system"),
        ComponentPath::from_flat_path("Root.ambiguous.system"),
    );
    ambiguous_owner
}

fn resolved_member_reference(root_def_id: DefId, member_def_id: DefId) -> ComponentReference {
    let mut reference = resolved_reference("system", root_def_id);
    reference.parts.push(ComponentRefPart {
        ident: Token {
            text: Arc::from("allowFlowReversal"),
            ..Default::default()
        },
        subs: None,
        def_id: Some(member_def_id),
    });
    reference
}

fn resolved_reference(name: &str, def_id: DefId) -> ComponentReference {
    let mut reference = make_comp_ref(name);
    reference.set_root_def_id(Some(def_id));
    reference
}

#[test]
fn partially_evaluated_declaration_shape_remains_unknown() {
    let source = r#"
        model Test
            input Real lines[:, 2, 2];
        end Test;
    "#;
    let tree = resolve(parse(source))
        .expect("resolve should succeed")
        .into_inner();
    let lines = &tree.definitions.classes["Test"].components["lines"];
    let semantics = ComponentSemantics::from_declaration_with_type(lines, TypeId::new(91));

    assert_eq!(
        semantics.shape, None,
        "the literal suffix cannot masquerade as the complete declared rank"
    );
}

fn indexed_reference(name: &str, index: i64, def_id: DefId) -> ComponentReference {
    let mut reference = resolved_reference(name, def_id);
    reference.parts[0].subs = Some(vec![Subscript::Expression(Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: Arc::from(index.to_string()),
            ..Default::default()
        },
        span: rumoca_core::Span::DUMMY,
    })]);
    reference
}

#[test]
fn nested_instances_with_one_source_def_keep_distinct_types() {
    let member_def_id = DefId::new(41);
    let mut overlay = InstanceOverlay::new();
    identity_instance(
        &mut overlay,
        "Pkg.Root.left.value",
        member_def_id,
        TypeId::new(11),
    );
    identity_instance(
        &mut overlay,
        "Pkg.Root.right.value",
        member_def_id,
        TypeId::new(12),
    );
    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let reference = resolved_reference("value", member_def_id);

    for (instance_scope, expected) in [
        ("Pkg.Root.left", TypeId::new(11)),
        ("Pkg.Root.right", TypeId::new(12)),
    ] {
        assert!(matches!(
            scope.lookup_reference(
                &reference,
                1,
                None,
                Some(&ComponentPath::from_flat_path(instance_scope)),
            ),
            SemanticLookup::Found(ComponentSemantics { type_id, .. }) if type_id == expected
        ));
    }
}

#[test]
fn qualified_scope_does_not_capture_short_prefix_sibling() {
    let value_def_id = DefId::new(42);
    let mut overlay = InstanceOverlay::new();
    identity_instance(
        &mut overlay,
        "Pkg.Root.value",
        value_def_id,
        TypeId::new(21),
    );
    identity_instance(&mut overlay, "Root.value", value_def_id, TypeId::new(22));
    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let reference = resolved_reference("value", value_def_id);

    assert!(matches!(
        scope.lookup_reference(
            &reference,
            1,
            None,
            Some(&ComponentPath::from_flat_path("Pkg.Root")),
        ),
        SemanticLookup::Found(ComponentSemantics { type_id, .. })
            if type_id == TypeId::new(21)
    ));
}

#[test]
fn outer_redirects_keep_sibling_and_nested_inner_identity() {
    let system_def_id = DefId::new(46);
    let value_def_id = DefId::new(47);
    let mut overlay = InstanceOverlay::new();
    for scenario in [
        (
            "Root.left.outer",
            "Root.left.outer.system",
            "Root.left.system",
            "Root.left.system.allowFlowReversal",
            11,
        ),
        (
            "Root.left.nested.outer",
            "Root.left.nested.outer.system",
            "Root.left.nested.system",
            "Root.left.nested.system.allowFlowReversal",
            13,
        ),
        (
            "Root.right.outer",
            "Root.right.outer.system",
            "Root.system",
            "Root.system.allowFlowReversal",
            15,
        ),
    ] {
        let (_, _, _, _, expected) = scenario;
        let owner = InstanceId(900 + expected);
        add_outer_member_case(&mut overlay, owner, scenario, system_def_id, value_def_id);
    }

    let ambiguous_owner = add_ambiguous_outer_case(&mut overlay, system_def_id, value_def_id);

    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let mut reference = resolved_reference("system", system_def_id);
    reference.parts.push(ComponentRefPart {
        ident: Token {
            text: Arc::from("allowFlowReversal"),
            ..Default::default()
        },
        subs: None,
        def_id: Some(value_def_id),
    });
    assert!(matches!(
        scope.lookup_reference(
            &resolved_reference("system", system_def_id),
            1,
            Some(InstanceId(911)),
            Some(&ComponentPath::from_flat_path("Root.left.outer")),
        ),
        SemanticLookup::Found(ComponentSemantics { type_id, .. })
            if type_id == TypeId::new(21)
    ));
    for (instance_scope, owner, expected) in [
        ("Root.left.outer", InstanceId(911), 11),
        ("Root.left.nested.outer", InstanceId(913), 13),
        ("Root.right.outer", InstanceId(915), 15),
    ] {
        let actual = scope.lookup_reference(
            &reference,
            2,
            Some(owner),
            Some(&ComponentPath::from_flat_path(instance_scope)),
        );
        let correct = matches!(
            &actual,
            SemanticLookup::Found(ComponentSemantics { type_id, .. })
                if *type_id == TypeId::new(expected)
        );
        assert!(
            correct,
            "{instance_scope}: expected type {expected}, got {actual:?}"
        );
    }
    assert_eq!(
        scope.lookup_reference(
            &reference,
            2,
            Some(ambiguous_owner),
            Some(&ComponentPath::from_flat_path("Root.ambiguous.outer")),
        ),
        SemanticLookup::Ambiguous,
        "an ambiguous inner occurrence must remain ambiguous after redirection"
    );

    let wrong_tail = resolved_member_reference(system_def_id, DefId::new(999));
    assert_eq!(
        scope.lookup_reference(
            &wrong_tail,
            2,
            Some(InstanceId(911)),
            Some(&ComponentPath::from_flat_path("Root.left.outer")),
        ),
        SemanticLookup::Missing,
        "a resolved tail with the wrong DefId must not borrow the textual inner member"
    );
}

#[test]
fn missing_outer_redirect_target_stays_unresolved() {
    let system_def_id = DefId::new(47);
    let value_def_id = DefId::new(48);
    let mut overlay = InstanceOverlay::new();
    let outer_id = identity_instance(
        &mut overlay,
        "Root.bad.outer.system",
        system_def_id,
        TypeId::new(16),
    );
    let owner = InstanceId(999);
    set_instance_owner(&mut overlay, "Root.bad.outer.system", owner);
    add_owner_class(&mut overlay, owner, outer_id);
    overlay.outer_prefix_to_inner.insert(
        ComponentPath::from_flat_path("Root.bad.outer.system"),
        ComponentPath::from_flat_path("Root.bad.missing.system"),
    );

    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let mut unresolved = resolved_reference("system", system_def_id);
    unresolved.parts.push(ComponentRefPart {
        ident: Token {
            text: Arc::from("value"),
            ..Default::default()
        },
        subs: None,
        def_id: Some(value_def_id),
    });
    assert_eq!(
        scope.lookup_reference(
            &unresolved,
            2,
            Some(owner),
            Some(&ComponentPath::from_flat_path("Root.bad.outer")),
        ),
        SemanticLookup::Missing,
        "an outer redirect with no inner instance must not fall back to the outer occurrence"
    );
}

#[test]
fn resolved_same_spelling_with_wrong_def_id_cannot_use_outer_redirect() {
    let system_def_id = DefId::new(48);
    let wrong_system_def_id = DefId::new(49);
    let owner = InstanceId(999);
    let mut overlay = InstanceOverlay::new();
    identity_instance(&mut overlay, "Root.system", system_def_id, TypeId::new(19));
    identity_instance(
        &mut overlay,
        "Root.child.system",
        system_def_id,
        TypeId::new(20),
    );
    set_instance_owner(&mut overlay, "Root.child.system", owner);
    overlay.outer_prefix_to_inner.insert(
        ComponentPath::from_flat_path("Root.child.system"),
        ComponentPath::from_flat_path("Root.system"),
    );

    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let wrong_reference = resolved_reference("system", wrong_system_def_id);
    assert_eq!(
        scope.lookup_reference(
            &wrong_reference,
            1,
            Some(owner),
            Some(&ComponentPath::from_flat_path("Root.child")),
        ),
        SemanticLookup::Missing,
        "resolved identity must reject a same-spelled but unrelated outer endpoint"
    );
}

#[test]
fn inherited_def_id_remaps_are_scoped_to_the_owning_instance() {
    let retained_system_def_id = DefId::new(50);
    let stale_system_def_id = DefId::new(51);
    let owner = InstanceId(1000);
    let other_owner = InstanceId(2000);
    let mut overlay = InstanceOverlay::new();
    let retained = identity_instance(
        &mut overlay,
        "Root.left.system",
        retained_system_def_id,
        TypeId::new(30),
    );
    let other = identity_instance(
        &mut overlay,
        "Root.right.system",
        retained_system_def_id,
        TypeId::new(31),
    );
    set_instance_owner(&mut overlay, "Root.left.system", owner);
    set_instance_owner(&mut overlay, "Root.right.system", other_owner);
    add_owner_class(&mut overlay, owner, retained);
    add_owner_class(&mut overlay, other_owner, other);
    overlay.inherited_def_id_remaps.insert(
        owner,
        [(stale_system_def_id, retained_system_def_id)]
            .into_iter()
            .collect(),
    );

    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let stale_reference = resolved_reference("system", stale_system_def_id);
    assert!(matches!(
        scope.lookup_reference(
            &stale_reference,
            1,
            Some(owner),
            Some(&ComponentPath::from_flat_path("Root.left")),
        ),
        SemanticLookup::Found(ComponentSemantics { type_id, .. })
            if type_id == TypeId::new(30)
    ));
    assert_eq!(
        scope.lookup_reference(
            &stale_reference,
            1,
            Some(other_owner),
            Some(&ComponentPath::from_flat_path("Root.right")),
        ),
        SemanticLookup::Missing,
        "a remap from one class occurrence must not cross into another occurrence"
    );
}

#[test]
fn literal_subscripts_select_instances_and_symbolic_family_conflicts_are_ambiguous() {
    let array_def_id = DefId::new(43);
    let mut overlay = InstanceOverlay::new();
    semantic_instance(
        &mut overlay,
        "Root.values",
        array_def_id,
        TypeId::new(31),
        rumoca_core::Variability::Continuous(Token::default()),
        vec![1],
        Vec::new(),
    );
    semantic_instance(
        &mut overlay,
        "Root.values",
        array_def_id,
        TypeId::new(32),
        rumoca_core::Variability::Continuous(Token::default()),
        vec![2],
        Vec::new(),
    );
    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let root_scope = ComponentPath::from_flat_path("Root");

    for (index, expected) in [(1, TypeId::new(31)), (2, TypeId::new(32))] {
        let reference = indexed_reference("values", index, array_def_id);
        assert!(matches!(
            scope.lookup_reference(&reference, 1, None, Some(&root_scope)),
            SemanticLookup::Found(ComponentSemantics { type_id, .. }) if type_id == expected
        ));
    }

    let family = resolved_reference("values", array_def_id);
    assert_eq!(
        scope.lookup_reference(&family, 1, None, Some(&root_scope)),
        SemanticLookup::Ambiguous,
        "an unresolved family with disagreeing instance metadata must not select the first path"
    );
}

#[test]
fn variability_only_family_collision_is_ambiguous() {
    let array_def_id = DefId::new(44);
    let mut overlay = InstanceOverlay::new();
    semantic_instance(
        &mut overlay,
        "Root.mode",
        array_def_id,
        TypeId::new(33),
        rumoca_core::Variability::Parameter(Token::default()),
        vec![1],
        Vec::new(),
    );
    semantic_instance(
        &mut overlay,
        "Root.mode",
        array_def_id,
        TypeId::new(33),
        rumoca_core::Variability::Continuous(Token::default()),
        vec![2],
        Vec::new(),
    );
    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let family = resolved_reference("mode", array_def_id);

    assert_eq!(
        scope.lookup_reference(
            &family,
            1,
            None,
            Some(&ComponentPath::from_flat_path("Root")),
        ),
        SemanticLookup::Ambiguous,
        "variability aliases must not silently select one colliding instance"
    );
}

#[test]
fn shape_only_family_collision_is_ambiguous() {
    let array_def_id = DefId::new(45);
    let mut overlay = InstanceOverlay::new();
    for (index, dims) in [(1, vec![2]), (2, vec![3])] {
        semantic_instance(
            &mut overlay,
            "Root.values",
            array_def_id,
            TypeId::new(34),
            rumoca_core::Variability::Continuous(Token::default()),
            vec![index],
            dims,
        );
    }
    let scope = InstanceSemanticScope::from_overlay(&overlay);
    let family = resolved_reference("values", array_def_id);

    assert_eq!(
        scope.lookup_reference(
            &family,
            1,
            None,
            Some(&ComponentPath::from_flat_path("Root")),
        ),
        SemanticLookup::Ambiguous,
        "shape aliases must not silently select one colliding instance"
    );
}

/// A dotted reference resolves its later parts within the owning instance.
#[test]
fn a_dotted_reference_resolves_its_tail_as_a_named_child() {
    let body_def_id = DefId::new(51);
    let mut overlay = InstanceOverlay::new();
    identity_instance(&mut overlay, "Pkg.Root.body", body_def_id, TypeId::new(31));
    identity_instance(
        &mut overlay,
        "Pkg.Root.body.value",
        DefId::new(52),
        TypeId::new(32),
    );
    let scope = InstanceSemanticScope::from_overlay(&overlay);

    let mut reference = resolved_reference("body", body_def_id);
    reference.parts.push(ComponentRefPart {
        ident: Token {
            text: Arc::from("value"),
            ..Default::default()
        },
        subs: None,
        def_id: None,
    });

    assert!(
        matches!(
            scope.lookup_reference(
                &reference,
                2,
                None,
                Some(&ComponentPath::from_flat_path("Pkg.Root")),
            ),
            SemanticLookup::Found(ComponentSemantics { type_id, .. })
                if type_id == TypeId::new(32)
        ),
        "the tail part names a child of the instance the head part resolved to"
    );

    let mut absent = resolved_reference("body", body_def_id);
    absent.parts.push(ComponentRefPart {
        ident: Token {
            text: Arc::from("missing"),
            ..Default::default()
        },
        subs: None,
        def_id: None,
    });
    assert!(
        !matches!(
            scope.lookup_reference(
                &absent,
                2,
                None,
                Some(&ComponentPath::from_flat_path("Pkg.Root")),
            ),
            SemanticLookup::Found(ComponentSemantics { type_id, .. })
                if type_id == TypeId::new(32)
        ),
        "a tail part that names no child must not borrow a sibling's semantics"
    );
}

#[test]
fn projected_unresolved_members_keep_the_owning_instances_semantics() {
    let body_def_id = DefId::new(61);
    let mut overlay = InstanceOverlay::new();
    identity_instance(&mut overlay, "Root.body", body_def_id, TypeId::new(41));
    for (path, declaration, type_id) in [
        ("Root.body.value", 62, 42),
        ("Root.body.other", 63, 43),
        ("Root.sibling", 64, 44),
        ("Root.sibling.value", 65, 45),
    ] {
        identity_instance(
            &mut overlay,
            path,
            DefId::new(declaration),
            TypeId::new(type_id),
        );
    }
    let scope = InstanceSemanticScope::from_overlay(&overlay);
    for (field, expected) in [("value", Some(42)), ("other", Some(43)), ("absent", None)] {
        let expression = Expression::FieldAccess {
            base: Arc::new(Expression::ComponentReference(resolved_reference(
                "body",
                body_def_id,
            ))),
            field: field.into(),
            field_def_id: None,
            span: rumoca_core::Span::DUMMY,
        };
        let actual = scope.lookup_expression(
            &expression,
            None,
            Some(&ComponentPath::from_flat_path("Root")),
        );
        match expected {
            Some(expected) => assert!(
                matches!(actual,
                SemanticLookup::Found(ComponentSemantics { type_id, .. })
                    if type_id == TypeId::new(expected)),
                "{field}: {actual:?}"
            ),
            None => assert_eq!(actual, SemanticLookup::Missing),
        }
    }
}
