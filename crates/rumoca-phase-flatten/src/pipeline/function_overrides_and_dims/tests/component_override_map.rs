//! Component override map construction: inherited defaults, active
//! redeclares, and receiver types.

use super::*;

struct RootReceiverTypeFixture {
    tree: ClassTree,
    overlay: InstanceOverlay,
    world_component_def: DefId,
    gravity_def: DefId,
}

fn root_receiver_type_fixture() -> RootReceiverTypeFixture {
    let world_def = DefId::new(1);
    let gravity_def = DefId::new(2);
    let base_def = DefId::new(3);
    let alias_def = DefId::new(4);
    let world_component_def = DefId::new(5);

    let mut tree = ClassTree::new();
    let world_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ScopeKind::Class);
    let base_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ScopeKind::Class);
    let alias_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ScopeKind::Class);
    let gravity_scope = tree
        .scope_tree
        .create_scope(world_scope, ScopeKind::Function);

    tree.scope_tree.add_member(
        tree.scope_tree.global(),
        ComponentPath::from_flat_path("World"),
        world_def,
    );
    tree.scope_tree.add_member(
        tree.scope_tree.global(),
        ComponentPath::from_flat_path("BasePointMass"),
        base_def,
    );
    tree.scope_tree.add_member(
        tree.scope_tree.global(),
        ComponentPath::from_flat_path("AliasPointMass"),
        alias_def,
    );
    tree.scope_tree.add_member(
        world_scope,
        ComponentPath::from_flat_path("gravityAcceleration"),
        gravity_def,
    );

    let mut world = class("World", ClassType::Model);
    world.def_id = Some(world_def);
    world.scope_id = Some(world_scope);
    world.classes.insert(
        "gravityAcceleration".to_string(),
        ClassDef {
            def_id: Some(gravity_def),
            scope_id: Some(gravity_scope),
            ..class("gravityAcceleration", ClassType::Function)
        },
    );

    let mut base_point_mass = class("BasePointMass", ClassType::Model);
    base_point_mass.def_id = Some(base_def);
    base_point_mass.scope_id = Some(base_scope);
    let mut world_component = component("world", "World", world_def);
    world_component.def_id = Some(world_component_def);
    base_point_mass
        .components
        .insert("world".to_string(), world_component);

    let mut alias_point_mass = class("AliasPointMass", ClassType::Model);
    alias_point_mass.def_id = Some(alias_def);
    alias_point_mass.scope_id = Some(alias_scope);
    alias_point_mass.extends.push(Extend {
        base_name: Name::from_string("BasePointMass"),
        base_def_id: Some(base_def),
        ..Extend::default()
    });

    tree.definitions.classes.insert("World".to_string(), world);
    tree.definitions
        .classes
        .insert("BasePointMass".to_string(), base_point_mass);
    tree.definitions
        .classes
        .insert("AliasPointMass".to_string(), alias_point_mass);
    tree.def_map.insert(world_def, "World".to_string());
    tree.def_map.insert(base_def, "BasePointMass".to_string());
    tree.def_map.insert(alias_def, "AliasPointMass".to_string());
    tree.name_map
        .insert("World.gravityAcceleration".to_string(), gravity_def);

    let mut overlay = InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: QualifiedName::new(),
            source_scope: Some(QualifiedName::from_ident("AliasPointMass")),
            source_scope_id: Some(alias_scope),
            ..ClassInstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    RootReceiverTypeFixture {
        tree,
        overlay,
        world_component_def,
        gravity_def,
    }
}

#[test]
fn root_class_scope_inherits_member_function_receiver_types() {
    let RootReceiverTypeFixture {
        tree,
        overlay,
        world_component_def,
        gravity_def,
    } = root_receiver_type_fixture();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let override_map =
        build_component_override_map(&overlay, &tree, &class_index, "AliasPointMass")
            .expect("component override map");
    let (_, override_functions) = override_context_for_scope("", &override_map);
    let marker = MemberFunctionCallMarker {
        tree: &tree,
        class_index: &class_index,
        override_functions: &override_functions,
    };

    assert_eq!(
        marked_target_def_id(
            &marker,
            deferred_member_ref(("world", world_component_def), "gravityAcceleration"),
        ),
        Some(gravity_def)
    );
}

#[test]
fn active_redeclare_overrides_inherited_default_alias_for_same_slot() {
    let medium_slot = DefId::new(9);
    let concrete_medium = override_target_with_slot(
        "ConcreteMedium",
        medium_slot,
        DefId::new(1),
        ClassType::Package,
        true,
    );
    let inherited_default = override_target_with_slot(
        "BaseClass.Medium",
        medium_slot,
        DefId::new(2),
        ClassType::Package,
        false,
    );
    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(
        ComponentPath::root(),
        [(medium_slot, concrete_medium.clone())]
            .into_iter()
            .collect(),
    );
    component_override_map.insert(
        ComponentPath::from_flat_path("pipe"),
        [(medium_slot, inherited_default)].into_iter().collect(),
    );

    let (override_packages, _) = override_context_for_scope("pipe", &component_override_map);

    assert_eq!(override_packages.len(), 1);
    assert_eq!(override_packages[0].name, concrete_medium.name);
    assert!(override_packages[0].active);
}

/// Two same-spelled aliases at different nesting depths are different slots:
/// the outer slot's active selection must not replace or merge with the
/// inner slot, so an inner redeclare is never governed by an outer alias of
/// the same leaf name.
#[test]
fn same_spelled_aliases_at_different_depths_stay_distinct_slots() {
    let outer_slot = DefId::new(21);
    let inner_slot = DefId::new(22);
    let mut outer_selection = override_target_with_slot(
        "OuterConcrete",
        outer_slot,
        DefId::new(31),
        ClassType::Package,
        true,
    );
    outer_selection.alias = "Medium".to_string();
    let mut inner_selection = override_target_with_slot(
        "InnerConcrete",
        inner_slot,
        DefId::new(32),
        ClassType::Package,
        false,
    );
    inner_selection.alias = "Medium".to_string();
    assert_eq!(outer_selection.alias, inner_selection.alias);

    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(
        ComponentPath::root(),
        [(outer_slot, outer_selection)].into_iter().collect(),
    );
    component_override_map.insert(
        ComponentPath::from_flat_path("pipe"),
        [(inner_slot, inner_selection.clone())]
            .into_iter()
            .collect(),
    );

    let (override_packages, override_functions) =
        override_context_for_scope("pipe", &component_override_map);

    assert_eq!(
        override_packages.len(),
        2,
        "distinct slots must both stay visible"
    );
    let inner_entry = override_functions
        .get(&inner_slot)
        .expect("the inner slot answers by its own identity");
    assert_eq!(
        inner_entry.def_id, inner_selection.def_id,
        "the outer active same-spelled alias must not capture the inner slot"
    );
}

#[test]
fn component_class_override_activity_uses_source_redeclare_reference() {
    let medium_def = DefId::new(10);
    let partial_def = DefId::new(1);
    let concrete_def = DefId::new(2);
    let explicit_def = DefId::new(3);
    let modelica_def = DefId::new(4);
    let media_def = DefId::new(5);
    let water_def = DefId::new(6);
    let forwarding_to_default = rumoca_ir_ast::ClassOverride::new(
        "Medium",
        medium_def,
        partial_def,
        Some(resolved_comp_ref(&[("Medium", medium_def)])),
    );
    let forwarding_to_concrete = rumoca_ir_ast::ClassOverride::new(
        "Medium",
        medium_def,
        concrete_def,
        Some(resolved_comp_ref(&[("Medium", medium_def)])),
    );
    let explicit_concrete = rumoca_ir_ast::ClassOverride::new(
        "Medium",
        medium_def,
        explicit_def,
        Some(resolved_comp_ref(&[
            ("Modelica", modelica_def),
            ("Media", media_def),
            ("Water", water_def),
            ("StandardWaterOnePhase", explicit_def),
        ])),
    );
    let inherited_default =
        override_target_with_active("BaseClass.Medium", partial_def, ClassType::Package, false);
    let concrete_class = class("StandardWaterOnePhase", ClassType::Package);
    let explicit_class = class("StandardWaterOnePhase", ClassType::Package);
    let default_class = class("Medium", ClassType::Package);
    let concrete_target = ResolvedClassRef {
        name: "Modelica.Media.Water.StandardWaterOnePhase".to_string(),
        def_id: concrete_def,
        class_def: &concrete_class,
    };
    let explicit_target = ResolvedClassRef {
        name: "Modelica.Media.Water.StandardWaterOnePhase".to_string(),
        def_id: explicit_def,
        class_def: &explicit_class,
    };
    let default_target = ResolvedClassRef {
        name: "Modelica.Fluid.Interfaces.PartialTwoPort.Medium".to_string(),
        def_id: partial_def,
        class_def: &default_class,
    };

    let tree = ClassTree::new();
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    assert!(!component_class_override_is_active(
        &tree,
        &class_index,
        &forwarding_to_default,
        Some(&inherited_default),
        &default_target,
    ));
    assert!(component_class_override_is_active(
        &tree,
        &class_index,
        &forwarding_to_concrete,
        Some(&inherited_default),
        &concrete_target,
    ));
    assert!(component_class_override_is_active(
        &tree,
        &class_index,
        &explicit_concrete,
        Some(&inherited_default),
        &explicit_target,
    ));
}
