use super::*;
use rumoca_core::EffectiveType;

#[test]
fn alias_roots_complete_beyond_sixteen_edges() {
    let mut source = String::new();
    for index in 0..20 {
        let base = if index == 0 {
            "Real".to_string()
        } else {
            format!("Alias{}", index - 1)
        };
        source.push_str(&format!("type Alias{index} = {base};\n"));
    }
    source.push_str("model Test\n  Alias19 value;\nend Test;\n");

    let standalone = resolve(parse(&source)).expect("long finite alias chain resolves");
    let typed = typecheck(standalone).expect("the standalone public entry accepts a finite chain");
    for index in 0..20 {
        let name = format!("Alias{index}");
        let type_id = typed
            .type_table
            .lookup(&name)
            .unwrap_or_else(|| panic!("standalone publication retains {name}"));
        let Some(Type::Alias(alias)) = typed.type_table.get(type_id) else {
            panic!("{name} must remain an alias type");
        };
        assert!(
            !alias.aliased.is_unknown(),
            "a successfully published alias cannot retain a placeholder edge",
        );
    }

    let instanced = resolve(parse(&source)).expect("the paired instanced source resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(instanced.inner(), "Test") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    let typed = typecheck_instanced_tree(&instanced, overlay, "Test")
        .expect("the instanced public entry accepts the same finite chain");
    let overlay = typed.overlay();
    let value = overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "value")
        .expect("typed long-alias occurrence");
    let effective = &overlay.effective_types[&value.type_id];
    assert_eq!(effective.canonical_type(), instanced.type_table.real(),);
}

#[test]
fn used_unknown_and_cyclic_aliases_fail_before_flattening() {
    let unknown = r#"
        type Broken = MissingLibrary.Value;
        model Test
            Broken value;
        end Test;
    "#;
    let diagnostics = resolve(parse(unknown))
        .expect_err("an absent extends target must fail at its resolution owner");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER003")),
        "an absent alias base must retain the exact resolution diagnostic: {diagnostics:?}",
    );

    let cyclic = r#"
        type First = Second;
        type Second = First;
        model Test
            First value;
        end Test;
    "#;
    let diagnostics =
        resolve(parse(cyclic)).expect_err("a cyclic alias graph must fail at resolution");
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("ER004")),
        "a cyclic alias graph must retain the cycle diagnostic: {diagnostics:?}",
    );
}

#[test]
fn equality_constraint_exposures_require_exact_effective_record_identity() {
    let source = r#"
        record R
            Real x;
            replaceable function equalityConstraint
                input R a;
                input R b;
                output Real residue[2];
            end equalityConstraint;
        end R;
        function AlternateConstraint
            extends R.equalityConstraint;
        end AlternateConstraint;
        model Test
            R ordinary;
            R alternate(redeclare function equalityConstraint = AlternateConstraint);
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("source resolves");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    assert_eq!(
        instanced.overlay.finalized_overconstrained().err(),
        Some(rumoca_ir_ast::EqualityConstraintOccurrenceError::EffectiveTypeCatalogNotFinalized,),
    );

    typecheck_instanced_test_projection(&instanced.tree, &mut instanced.overlay, "Test")
        .expect("effective record exposure typechecks");
    let occurrence = |name: &str| {
        instanced
            .overlay
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == name)
            .expect("record occurrence")
    };
    let ordinary = occurrence("ordinary");
    let alternate = occurrence("alternate");
    let ordinary_x = occurrence("ordinary.x");
    let alternate_x = occurrence("alternate.x");
    let finalized = instanced
        .overlay
        .finalized_overconstrained()
        .expect("owner and effective-type proofs finalized");
    let ordinary_exposure = finalized
        .exposure(ordinary.instance_id)
        .expect("ordinary exposure");
    let alternate_exposure = finalized
        .exposure(alternate.instance_id)
        .expect("alternate exposure");

    assert_eq!(
        ordinary_exposure
            .effective_record_identity()
            .effective_type_id(),
        ordinary.type_id,
    );
    assert_eq!(
        alternate_exposure
            .effective_record_identity()
            .effective_type_id(),
        alternate.type_id,
    );
    assert!(
        instanced.overlay.effective_types[&ordinary.type_id]
            .dimensions()
            .is_empty(),
    );
    assert_eq!(ordinary_exposure.cardinality().scalar_count(), 2);
    assert_eq!(alternate_exposure.cardinality().scalar_count(), 2,);
    assert_ne!(
        ordinary_exposure.selected_function_def_id(),
        alternate_exposure.selected_function_def_id(),
    );
    assert_eq!(
        finalized.record_owner(ordinary_x.instance_id),
        Some(ordinary.instance_id),
    );
    assert_eq!(
        finalized.record_owner(alternate_x.instance_id),
        Some(alternate.instance_id),
    );
}

#[test]
fn concrete_shape_and_nominal_type_define_effective_identity() {
    let source = r#"
        type Count = Integer;
        model Test
            Integer scalar;
            Integer matrix[2, 3];
            Integer sameShape[2, 3];
            Count aliasedScalar;
        end Test;
    "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("source resolves");
    let tree = resolved.inner().clone();
    let model = tree
        .get_class_by_qualified_name("Test")
        .expect("Test model");
    let mut overlay = InstanceOverlay::new();
    for name in ["scalar", "matrix", "sameShape", "aliasedScalar"] {
        add_test_instance(
            &mut overlay,
            name,
            model.components.get(name).expect("component declaration"),
            None,
        );
    }
    overlay
        .finalize_overconstrained_record_owners()
        .expect("the manually constructed fixture must carry instantiate's owner proof");

    typecheck_instanced_test_projection(&tree, &mut overlay, "Test")
        .expect("instance types resolve");

    let type_id = |name: &str| {
        overlay
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == name)
            .map(|data| data.type_id)
            .expect("typed instance")
    };
    let scalar = type_id("scalar");
    let matrix = type_id("matrix");
    let same_shape = type_id("sameShape");
    let aliased_scalar = type_id("aliasedScalar");

    assert_ne!(scalar, matrix, "shape is part of effective identity");
    assert_eq!(
        matrix, same_shape,
        "equal nominal types and shapes share one canonical identity"
    );
    assert_ne!(
        scalar, aliased_scalar,
        "a declared alias retains its nominal identity"
    );

    let matrix_type = &overlay.effective_types[&matrix];
    assert_eq!(matrix_type.dimensions(), [2, 3]);
    assert_eq!(matrix_type.canonical_type(), tree.type_table.integer());
    let alias_type = &overlay.effective_types[&aliased_scalar];
    assert_ne!(alias_type.nominal_type(), tree.type_table.integer());
    assert_eq!(alias_type.canonical_type(), tree.type_table.integer());
}

#[test]
fn sibling_redeclare_occurrences_keep_distinct_exact_types() {
    let source = r#"
        package P
            type ValueA = Real;
            type ValueB = Real;
            partial package MediumBase
                replaceable type Value = ValueA constrainedby Real;
            end MediumBase;
            package MediumA
                extends MediumBase(redeclare type Value = ValueA);
            end MediumA;
            package MediumB
                extends MediumBase(redeclare type Value = ValueB);
            end MediumB;
            model Cell
                replaceable package Medium = MediumBase constrainedby MediumBase;
                Medium.Value value;
            end Cell;
            model Test
                Cell a(redeclare package Medium = MediumA);
                Cell b(redeclare package Medium = MediumB);
            end Test;
        end P;
    "#;
    let resolved = resolve(parse(source)).expect("source resolves");
    let source_value_def_id = resolved.definitions.classes["P"].classes["Cell"].components["value"]
        .def_id
        .expect("source value declaration identity");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay =
            match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "P.Test") {
                rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
                rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                    missing_inners,
                    ..
                } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
                rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                    panic!("fixture instantiation failed: {error}")
                }
            };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    typecheck_instanced_test_projection(&instanced.tree, &mut instanced.overlay, "P.Test")
        .expect("both specialized siblings typecheck");

    let occurrence = |name: &str| {
        instanced
            .overlay
            .components
            .values()
            .find(|data| data.qualified_name.to_flat_string() == name)
            .expect("typed specialized occurrence")
    };
    let left = occurrence("a.value");
    let right = occurrence("b.value");
    assert_eq!(
        left.component_ref
            .as_ref()
            .map(|reference| reference.target_def_id()),
        Some(source_value_def_id)
    );
    assert_eq!(
        right
            .component_ref
            .as_ref()
            .map(|reference| reference.target_def_id()),
        Some(source_value_def_id)
    );
    assert_ne!(
        left.type_id, right.type_id,
        "one source declaration must retain distinct per-owner specialized identities"
    );
}

#[test]
fn enumeration_effective_identity_is_classified_before_flattening() {
    let source = r#"
        package P
            type L = enumeration(U, X, Z, ZERO, ONE);
            model Test
                L a(start = L.U);
            end Test;
        end P;
    "#;
    let resolved = resolve(parse(source)).expect("source resolves");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay =
            match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "P.Test") {
                rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
                rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                    missing_inners,
                    ..
                } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
                rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                    panic!("fixture instantiation failed: {error}")
                }
            };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };

    typecheck_instanced_test_projection(&instanced.tree, &mut instanced.overlay, "P.Test")
        .expect("enumeration component typechecks");

    let coordinate = instanced
        .overlay
        .components
        .values()
        .find(|data| data.qualified_name.to_flat_string() == "a")
        .expect("typed enumeration coordinate");
    let effective = &instanced.overlay.effective_types[&coordinate.type_id];
    assert!(
        instanced
            .overlay
            .enumeration_types
            .contains(&coordinate.type_id),
        "the exact effective identity must carry enumeration classification"
    );
    assert_ne!(
        effective.canonical_type(),
        coordinate.type_id,
        "the regression requires distinct nominal and effective identity arenas"
    );
}

#[test]
fn missing_canonical_root_leaves_effective_publication_unchanged() {
    let source = r#"
        model Test
            Real value;
        end Test;
    "#;
    let (mut checker, _type_table, mut instanced) = effective_finalization_fixture(source, "Test");
    let occurrence = instanced
        .overlay
        .components
        .values()
        .next()
        .expect("fixture has one component");
    let nominal = occurrence.type_id;
    instanced.overlay.type_roots.shift_remove(&nominal);
    let before = instanced.overlay.clone();

    let semantic_catalogs = test_semantic_catalog_projection(&instanced.tree);
    assert!(!checker.finalize_effective_types(&mut instanced.overlay, semantic_catalogs));
    assert!(checker.diagnostics().iter().any(|diagnostic| {
        diagnostic.code.as_deref() == Some("ET000")
            && diagnostic
                .message
                .contains("complete acyclic canonical root")
    }));
    assert_effective_publication_unchanged(&instanced.overlay, &before);
}

#[test]
fn invalid_effective_shape_leaves_effective_publication_unchanged() {
    let source = r#"
        model Test
            Real value;
        end Test;
    "#;
    let (mut checker, _type_table, mut instanced) = effective_finalization_fixture(source, "Test");
    instanced
        .overlay
        .components
        .values_mut()
        .next()
        .expect("fixture has one component")
        .dims = vec![-1];
    let before = instanced.overlay.clone();

    let semantic_catalogs = test_semantic_catalog_projection(&instanced.tree);
    assert!(!checker.finalize_effective_types(&mut instanced.overlay, semantic_catalogs));
    assert!(checker.diagnostics().iter().any(|diagnostic| {
        diagnostic.code.as_deref() == Some("ET000") && diagnostic.message.contains("effective type")
    }));
    assert_effective_publication_unchanged(&instanced.overlay, &before);
}

#[test]
fn equality_constraint_failure_leaves_all_effective_state_unpublished() {
    let source = r#"
        record R
            Real x;
            replaceable function equalityConstraint
                input R a;
                input R b;
                output Real residue[2];
            end equalityConstraint;
        end R;
        model Test
            R value;
        end Test;
    "#;
    let (mut checker, _type_table, mut instanced) = effective_finalization_fixture(source, "Test");
    let record = instanced
        .tree
        .get_def_id_by_name("R")
        .expect("overconstrained record declaration exists");
    instanced.overlay.type_ids_by_def_id.shift_remove(&record);
    let before = instanced.overlay.clone();

    let semantic_catalogs = test_semantic_catalog_projection(&instanced.tree);
    assert!(!checker.finalize_effective_types(&mut instanced.overlay, semantic_catalogs));
    assert!(checker.diagnostics().iter().any(|diagnostic| {
        diagnostic.code.as_deref() == Some("ET013")
            && diagnostic.message.contains("equalityConstraint")
    }));
    assert_effective_publication_unchanged(&instanced.overlay, &before);
}

#[test]
fn missing_instantiate_owner_proof_is_not_repaired_by_typecheck() {
    let source = r#"
        model Test
            Real value;
        end Test;
    "#;
    let resolved = resolve(parse(source)).expect("owner-proof fixture resolves");
    let tree = resolved.inner().clone();
    let component = tree
        .get_class_by_qualified_name("Test")
        .and_then(|model| model.components.get("value"))
        .expect("owner-proof fixture component exists");
    let mut overlay = InstanceOverlay::new();
    add_test_instance(&mut overlay, "value", component, None);
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&tree)
        .expect("owner-proof fixture has a complete nominal context");
    checker
        .populate_overlay_type_roots(&tree, &mut overlay, &type_table)
        .expect("owner-proof fixture publishes checked nominal roots");
    checker.resolve_overlay_component_types(&tree, &mut overlay, &type_table);
    seed_stale_effective_publication(&mut overlay, type_table.real());
    let before = overlay.clone();

    let semantic_catalogs = test_semantic_catalog_projection(&tree);
    assert!(!checker.finalize_effective_types(&mut overlay, semantic_catalogs));
    assert!(checker.diagnostics().iter().any(|diagnostic| {
        diagnostic.code.as_deref() == Some("ET013")
            && diagnostic
                .message
                .contains("descendant ownership is not finalized")
    }));
    assert_effective_publication_unchanged(&overlay, &before);
    assert_eq!(
        overlay.finalized_overconstrained().err(),
        Some(rumoca_ir_ast::EqualityConstraintOccurrenceError::OwnerCatalogNotFinalized),
        "typecheck must not mint instantiate's owner-finalized proof"
    );
}

fn effective_finalization_fixture(
    source: &str,
    model: &str,
) -> (TypeChecker, TypeTable, rumoca_ir_ast::InstancedTree) {
    let resolved = resolve(parse(source)).expect("effective finalization fixture resolves");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, model) {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    let mut checker = TypeChecker::new();
    let (type_table, _type_root_catalog) = checker
        .initialize_instanced_context(&instanced.tree)
        .expect("effective finalization fixture has a complete nominal context");
    checker
        .populate_overlay_type_roots(&instanced.tree, &mut instanced.overlay, &type_table)
        .expect("effective finalization fixture publishes checked nominal roots");
    checker.resolve_overlay_component_types(&instanced.tree, &mut instanced.overlay, &type_table);
    assert!(
        !checker.has_errors(),
        "fixture setup must not emit semantic diagnostics: {:?}",
        checker.diagnostics()
    );
    (checker, type_table, instanced)
}

fn seed_stale_effective_publication(overlay: &mut InstanceOverlay, canonical: TypeId) {
    let stale = TypeId::new(u32::MAX - 2);
    overlay.effective_types.insert(
        stale,
        EffectiveType::new(canonical, canonical, []).expect("stale fixture descriptor is valid"),
    );
    overlay.enumeration_types.insert(stale);
    overlay.type_roots.insert(stale, canonical);
}

fn assert_effective_publication_unchanged(actual: &InstanceOverlay, before: &InstanceOverlay) {
    let component_types = |overlay: &InstanceOverlay| {
        overlay
            .components
            .iter()
            .map(|(instance, data)| (*instance, data.type_id))
            .collect::<Vec<_>>()
    };
    assert_eq!(component_types(actual), component_types(before));
    assert_eq!(actual.type_roots, before.type_roots);
    assert_eq!(actual.effective_types, before.effective_types);
    assert_eq!(actual.enumeration_types, before.enumeration_types);
    assert_eq!(
        actual.overconstrained_construction_counts(),
        before.overconstrained_construction_counts()
    );
    assert_eq!(
        format!("{:?}", actual.finalized_overconstrained().err()),
        format!("{:?}", before.finalized_overconstrained().err()),
    );
}
