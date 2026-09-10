use super::*;

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
    let tree = resolved.into_inner();
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

    typecheck_instanced(&tree, &mut overlay, "Test").expect("instance types resolve");

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
    let mut instanced = rumoca_phase_instantiate::instantiate(resolved, "P.Test")
        .expect("both specialized siblings instantiate");
    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "P.Test")
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
    let mut instanced = rumoca_phase_instantiate::instantiate(resolved, "P.Test")
        .expect("enumeration component instantiates");

    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "P.Test")
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
