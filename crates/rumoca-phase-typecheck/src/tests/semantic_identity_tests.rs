//! Collision regressions for DefId/InstanceId-backed typecheck scopes.

use super::*;

fn identity_instance(
    overlay: &mut InstanceOverlay,
    path: &str,
    source_def_id: DefId,
    type_id: TypeId,
) {
    semantic_instance(
        overlay,
        path,
        source_def_id,
        type_id,
        rumoca_core::Variability::Continuous(Token::default()),
        Vec::new(),
        Vec::new(),
    );
}

fn semantic_instance(
    overlay: &mut InstanceOverlay,
    path: &str,
    source_def_id: DefId,
    type_id: TypeId,
    variability: rumoca_core::Variability,
    instance_subscripts: Vec<i64>,
    dims: Vec<i64>,
) {
    let instance_id = overlay.alloc_id();
    let mut qualified_name = QualifiedName::from_dotted(path);
    qualified_name
        .parts
        .last_mut()
        .expect("test instance path must not be empty")
        .1 = instance_subscripts;
    overlay.add_component(InstanceData {
        instance_id,
        component_ref: Some(rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: Vec::new(),
            def_id: Some(source_def_id),
        }),
        qualified_name,
        type_id,
        variability,
        dims,
        ..Default::default()
    });
}

fn resolved_reference(name: &str, def_id: DefId) -> ComponentReference {
    let mut reference = make_comp_ref(name);
    reference.def_id = Some(def_id);
    reference
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
