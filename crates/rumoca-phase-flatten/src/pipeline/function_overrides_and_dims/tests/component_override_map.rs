//! Component override map construction: inherited defaults, active
//! redeclares, and receiver types.

use super::*;

#[test]
fn root_class_scope_inherits_member_function_receiver_types() {
    let world_def = DefId::new(1);
    let gravity_def = DefId::new(2);
    let base_def = DefId::new(3);
    let alias_def = DefId::new(4);

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
    base_point_mass
        .components
        .insert("world".to_string(), component("world", "World", world_def));

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
    overlay.add_class(ClassInstanceData {
        qualified_name: QualifiedName::new(),
        source_scope: Some(QualifiedName::from_ident("AliasPointMass")),
        source_scope_id: Some(alias_scope),
        ..ClassInstanceData::default()
    });

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
        marker
            .mark_component_function_call(deferred_member_ref(
                ("world", world_def),
                "gravityAcceleration",
            ))
            .target_def_id(),
        Some(gravity_def)
    );
}

#[test]
fn inherited_same_named_package_defaults_keep_class_def_id_identity() {
    let base_a_def = DefId::new(30);
    let base_b_def = DefId::new(31);
    let medium_a_def = DefId::new(32);
    let medium_b_def = DefId::new(33);
    let target_a_def = DefId::new(34);
    let target_b_def = DefId::new(35);
    let derived_def = DefId::new(36);

    let mut target_a = class("TargetA", ClassType::Package);
    target_a.def_id = Some(target_a_def);
    let mut target_b = class("TargetB", ClassType::Package);
    target_b.def_id = Some(target_b_def);
    let mut medium_a = class("Medium", ClassType::Package);
    medium_a.def_id = Some(medium_a_def);
    medium_a.extends.push(Extend {
        base_name: Name::from_string("TargetA"),
        base_def_id: Some(target_a_def),
        ..Extend::default()
    });
    let mut medium_b = class("Medium", ClassType::Package);
    medium_b.def_id = Some(medium_b_def);
    medium_b.extends.push(Extend {
        base_name: Name::from_string("TargetB"),
        base_def_id: Some(target_b_def),
        ..Extend::default()
    });
    let mut base_a = class("BaseA", ClassType::Model);
    base_a.def_id = Some(base_a_def);
    base_a.classes.insert("Medium".to_string(), medium_a);
    let mut base_b = class("BaseB", ClassType::Model);
    base_b.def_id = Some(base_b_def);
    base_b.classes.insert("Medium".to_string(), medium_b);
    let mut derived = class("Derived", ClassType::Model);
    derived.def_id = Some(derived_def);
    derived.extends.extend([
        Extend {
            base_name: Name::from_string("BaseA"),
            base_def_id: Some(base_a_def),
            ..Extend::default()
        },
        Extend {
            base_name: Name::from_string("BaseB"),
            base_def_id: Some(base_b_def),
            ..Extend::default()
        },
    ]);

    let mut tree = ClassTree::new();
    for (name, class_def) in [
        ("TargetA", target_a),
        ("TargetB", target_b),
        ("BaseA", base_a),
        ("BaseB", base_b),
        ("Derived", derived),
    ] {
        tree.definitions.classes.insert(name.to_string(), class_def);
    }
    for (def_id, name) in [
        (base_a_def, "BaseA"),
        (base_b_def, "BaseB"),
        (medium_a_def, "BaseA.Medium"),
        (medium_b_def, "BaseB.Medium"),
        (target_a_def, "TargetA"),
        (target_b_def, "TargetB"),
        (derived_def, "Derived"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
    }
    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let derived = class_index.get(derived_def).expect("derived class");
    let mut overrides = OverrideEntries::default();
    collect_component_constructor_aliases_for_class(
        &tree,
        &class_index,
        derived,
        "Derived",
        false,
        &mut FxHashSet::default(),
        &mut overrides,
    );

    let slots = overrides
        .exact_packages
        .iter()
        .filter_map(|target| target.alias_def_id)
        .collect::<Vec<_>>();
    assert_eq!(slots.len(), 2);
    assert!(slots.contains(&medium_a_def));
    assert!(slots.contains(&medium_b_def));
}

#[test]
fn package_default_without_resolved_base_does_not_mint_exact_slot() {
    let target_def = DefId::new(40);
    let alias_def = DefId::new(41);
    let holder_def = DefId::new(42);

    let mut target = class("Target", ClassType::Package);
    target.def_id = Some(target_def);
    let mut alias = class("Medium", ClassType::Package);
    alias.def_id = Some(alias_def);
    alias.extends.push(Extend {
        base_name: Name::from_string("Target"),
        base_def_id: None,
        ..Extend::default()
    });
    let mut holder = class("Holder", ClassType::Model);
    holder.def_id = Some(holder_def);
    holder.classes.insert("Medium".to_string(), alias);

    let mut tree = ClassTree::new();
    tree.definitions
        .classes
        .insert("Target".to_string(), target);
    tree.definitions
        .classes
        .insert("Holder".to_string(), holder);
    for (def_id, name) in [
        (target_def, "Target"),
        (holder_def, "Holder"),
        (alias_def, "Holder.Medium"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
    }

    let class_index = rumoca_ir_ast::ClassDefIndex::from_tree(&tree);
    let holder = class_index.get(holder_def).expect("holder class");
    let mut overrides = OverrideEntries::default();
    collect_component_constructor_aliases_for_class(
        &tree,
        &class_index,
        holder,
        "Holder",
        false,
        &mut FxHashSet::default(),
        &mut overrides,
    );

    assert!(overrides.exact_packages.is_empty());
    assert!(overrides.by_alias.is_empty());
}

#[test]
fn same_named_exact_package_slots_remain_distinct_across_scopes() {
    let mut concrete_medium = override_target("ConcreteMedium", DefId::new(1), ClassType::Package);
    concrete_medium.alias = "Medium".to_string();
    concrete_medium.alias_def_id = Some(DefId::new(3));
    let mut inherited_default =
        override_target_with_active("BaseClass.Medium", DefId::new(2), ClassType::Package, false);
    inherited_default.alias = "Medium".to_string();
    inherited_default.alias_def_id = Some(DefId::new(4));
    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(
        ComponentPath::root(),
        override_entries([concrete_medium.clone()]),
    );
    component_override_map.insert(
        ComponentPath::from_flat_path("pipe"),
        override_entries([inherited_default.clone()]),
    );

    let (override_packages, _) = override_context_for_scope("pipe", &component_override_map);

    assert_eq!(override_packages.len(), 2);
    assert!(override_packages.iter().any(|target| {
        target.alias_def_id == concrete_medium.alias_def_id
            && target.name == concrete_medium.name
            && target.active
    }));
    assert!(override_packages.iter().any(|target| {
        target.alias_def_id == inherited_default.alias_def_id
            && target.name == inherited_default.name
            && !target.active
    }));
}

#[test]
fn unknown_package_defaults_with_different_targets_remain_ambiguous() {
    let mut first = override_target("FirstMedium", DefId::new(11), ClassType::Package);
    first.alias = "Medium".to_string();
    let mut second = override_target("SecondMedium", DefId::new(12), ClassType::Package);
    second.alias = "Medium".to_string();
    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(ComponentPath::root(), override_entries([first.clone()]));
    component_override_map.insert(
        ComponentPath::from_flat_path("pipe"),
        override_entries([second.clone()]),
    );

    let (override_packages, _) = override_context_for_scope("pipe", &component_override_map);

    assert_eq!(override_packages.len(), 2);
    assert!(
        override_packages
            .iter()
            .any(|target| target.def_id == first.def_id)
    );
    assert!(
        override_packages
            .iter()
            .any(|target| target.def_id == second.def_id)
    );
}

#[test]
fn unknown_default_survives_same_named_exact_package_slot() {
    let mut unknown_default =
        override_target_with_active("DefaultMedium", DefId::new(21), ClassType::Package, false);
    unknown_default.alias = "Medium".to_string();
    let mut exact_override =
        override_target_with_active("SelectedMedium", DefId::new(22), ClassType::Package, true);
    exact_override.alias = "Medium".to_string();
    exact_override.alias_def_id = Some(DefId::new(23));
    let mut component_override_map = ComponentOverrideMap::default();
    component_override_map.insert(
        ComponentPath::root(),
        override_entries([unknown_default.clone(), exact_override.clone()]),
    );

    let (override_packages, _) = override_context_for_scope("", &component_override_map);

    assert_eq!(override_packages.len(), 2);
    assert!(override_packages.iter().any(|target| {
        target.def_id == unknown_default.def_id && target.alias_def_id.is_none()
    }));
    assert!(override_packages.iter().any(|target| {
        target.def_id == exact_override.def_id && target.alias_def_id == exact_override.alias_def_id
    }));
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

    assert!(!component_class_override_is_active(
        &forwarding_to_default,
        Some(&inherited_default),
        &default_target,
    ));
    assert!(component_class_override_is_active(
        &forwarding_to_concrete,
        Some(&inherited_default),
        &concrete_target,
    ));
    assert!(component_class_override_is_active(
        &explicit_concrete,
        Some(&inherited_default),
        &explicit_target,
    ));
}
