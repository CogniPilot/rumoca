use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn typed_flat_model(source: &str, model: &str) -> flat::Model {
    let file_name = "<effective_type_identity>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model)
        .expect("instanced model typechecks");
    rumoca_phase_flatten::flatten_ref(&tree, &overlay, model).expect("typed model flattens")
}

fn variable<'model>(model: &'model flat::Model, name: &str) -> &'model flat::Variable {
    model
        .variables
        .iter()
        .find(|(candidate, _)| candidate.as_str() == name)
        .map(|(_, variable)| variable)
        .unwrap_or_else(|| panic!("missing flat variable `{name}`"))
}

#[test]
fn flat_preserves_scalar_array_and_alias_effective_types() {
    let model = typed_flat_model(
        r#"
        package P
            type Count = Integer;
            model M
                Integer scalar;
                Integer matrix[2, 3];
                Integer sameShape[2, 3];
                Count aliasedScalar;
            end M;
        end P;
        "#,
        "P.M",
    );

    let scalar = variable(&model, "scalar");
    let matrix = variable(&model, "matrix");
    let same_shape = variable(&model, "sameShape");
    let aliased_scalar = variable(&model, "aliasedScalar");

    assert_ne!(scalar.type_id, matrix.type_id);
    assert_eq!(matrix.type_id, same_shape.type_id);
    assert_ne!(scalar.type_id, aliased_scalar.type_id);
    assert_eq!(model.effective_types[&matrix.type_id].dimensions(), [2, 3]);
    assert_eq!(
        model.effective_types[&aliased_scalar.type_id].dimensions(),
        []
    );
}

#[test]
fn flat_preserves_redeclared_nominal_type_identity() {
    let model = typed_flat_model(
        r#"
        package P
            type ValueA = Real;
            type ValueB = Real;
            package Medium
                replaceable type Value = ValueA
                    constrainedby Real;
            end Medium;
            package MediumA
                extends Medium(redeclare type Value = ValueA);
            end MediumA;
            package MediumB
                extends Medium(redeclare type Value = ValueB);
            end MediumB;
            model Holder
                replaceable package Selected = Medium
                    constrainedby Medium;
                Selected.Value value;
            end Holder;
            model M
                Holder base(redeclare package Selected = MediumA);
                Holder changed(redeclare package Selected = MediumB);
            end M;
        end P;
        "#,
        "P.M",
    );

    let base = variable(&model, "base.value");
    let changed = variable(&model, "changed.value");
    assert_ne!(
        base.type_id, changed.type_id,
        "a concrete redeclare selects a distinct nominal effective type"
    );
    let base_type = &model.effective_types[&base.type_id];
    let changed_type = &model.effective_types[&changed.type_id];
    assert_ne!(base_type.nominal_type(), changed_type.nominal_type());
    assert_eq!(base_type.canonical_type(), changed_type.canonical_type());
}

#[test]
fn enum_indexed_matrix_and_scalar_integer_have_distinct_types() {
    let model = typed_flat_model(
        r#"
        package P
            type L = enumeration(U, X, Z, ZERO, ONE);
            model M
                parameter Integer map[L, L] = [1,1,1,1,1;
                                               1,2,2,2,2;
                                               1,2,3,3,3;
                                               1,2,3,4,4;
                                               1,2,3,4,5];
                L a(start=L.U);
                L b(start=L.U);
                Integer f(start=1);
            algorithm
                if change(a) or change(b) then
                    f := map[a, b];
                end if;
            end M;
        end P;
        "#,
        "P.M",
    );

    let map = variable(&model, "map");
    let scalar = variable(&model, "f");
    assert_eq!(map.dims, [5, 5]);
    assert!(scalar.dims.is_empty());
    assert_ne!(map.type_id, scalar.type_id);
    assert_eq!(model.effective_types[&map.type_id].dimensions(), [5, 5]);
}
