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
