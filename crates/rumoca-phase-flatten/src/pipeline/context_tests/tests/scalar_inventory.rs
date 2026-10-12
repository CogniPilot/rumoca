use super::*;

const NAMES: [&str; 13] = [
    "dimensions",
    "dimensionAlias",
    "singleton",
    "empty",
    "label",
    "labelAlias",
    "n",
    "gain",
    "enabled",
    "emptyCount",
    "parent.n",
    "parent.child.n",
    "parent.child.chosen",
];

fn declaration_parts(name: &str) -> Vec<(&str, DefId)> {
    let parts: &[(&str, u32)] = match name {
        "dimensions" => &[("dimensions", 400)],
        "dimensionAlias" => &[("dimensionAlias", 401)],
        "singleton" => &[("singleton", 402)],
        "empty" => &[("empty", 403)],
        "label" => &[("label", 404)],
        "labelAlias" => &[("labelAlias", 405)],
        "n" => &[("n", 406)],
        "gain" => &[("gain", 407)],
        "enabled" => &[("enabled", 408)],
        "emptyCount" => &[("emptyCount", 409)],
        "parent.n" => &[("parent", 490), ("n", 410)],
        "parent.child.n" => &[("parent", 490), ("child", 491), ("n", 411)],
        "parent.child.chosen" => &[("parent", 490), ("child", 491), ("chosen", 412)],
        _ => panic!("unknown fixture declaration: {name}"),
    };
    parts
        .iter()
        .map(|&(part, id)| (part, DefId::new(id)))
        .collect()
}

fn reference(name: &str, index: Option<i64>) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            name,
            core_component_ref(&declaration_parts(name)),
        ),
        subscripts: index
            .map(|value| rumoca_core::Subscript::Index {
                value,
                span: test_span(),
            })
            .into_iter()
            .collect(),
        span: test_span(),
    }
}

fn add_parameter(
    model: &mut flat::Model,
    name: &str,
    dims: &[i64],
    binding: Expression,
    canonical: rumoca_core::TypeId,
    modified: bool,
) {
    let index = NAMES
        .iter()
        .position(|candidate| *candidate == name)
        .unwrap() as u32;
    let type_id = rumoca_core::TypeId::new(500 + index);
    model.effective_types.insert(
        type_id,
        rumoca_core::EffectiveType::new(canonical, canonical, dims).unwrap(),
    );
    let var_name = rumoca_core::VarName::new(name);
    model.add_variable(
        var_name.clone(),
        flat::Variable {
            name: var_name,
            component_ref: Some(core_component_ref(&declaration_parts(name))),
            instance_id: InstanceId::new(index + 1),
            dims: dims.to_vec(),
            binding: Some(binding),
            type_id,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding_from_modification: modified,
            is_primitive: true,
            is_discrete_type: canonical != model.predefined_types.real,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
}

fn add_aggregate_parameters(model: &mut flat::Model) {
    let integer = model.predefined_types.integer;
    for (name, dims, binding) in [
        ("dimensions", vec![2], int_array(&[3, 2])),
        ("dimensionAlias", vec![2], reference("dimensions", None)),
        ("singleton", vec![1], int_array(&[3])),
        ("empty", vec![0], fill_expr(0, &[0])),
    ] {
        add_parameter(model, name, &dims, binding, integer, false);
    }
    let string = model.predefined_types.string;
    add_parameter(
        model,
        "label",
        &[],
        Expression::Literal {
            value: rumoca_core::Literal::String("wide".to_string()),
            span: test_span(),
        },
        string,
        false,
    );
    add_parameter(
        model,
        "labelAlias",
        &[],
        reference("label", None),
        string,
        false,
    );
}

fn add_scalar_parameters(model: &mut flat::Model) {
    let integer = model.predefined_types.integer;
    for (name, binding, modified) in [
        ("n", reference("dimensionAlias", Some(1)), false),
        (
            "emptyCount",
            Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![reference("empty", None), int_lit(1)],
                span: test_span(),
            },
            false,
        ),
        ("parent.n", int_lit(7), false),
        ("parent.child.n", int_lit(2), false),
        ("parent.child.chosen", reference("parent.n", None), true),
    ] {
        add_parameter(model, name, &[], binding, integer, modified);
    }
    let real = model.predefined_types.real;
    let boolean = model.predefined_types.boolean;
    add_parameter(
        model,
        "gain",
        &[],
        mul_expr(reference("dimensionAlias", Some(2)), real_lit(2.5)),
        real,
        false,
    );
    add_parameter(
        model,
        "enabled",
        &[],
        Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(reference("n", None)),
            rhs: Box::new(int_lit(2)),
            span: test_span(),
        },
        boolean,
        false,
    );
}

fn inventory_model() -> flat::Model {
    let mut model = flat::Model {
        predefined_types: flat::PredefinedTypeIds {
            real: rumoca_core::TypeId::new(1),
            integer: rumoca_core::TypeId::new(2),
            boolean: rumoca_core::TypeId::new(3),
            string: rumoca_core::TypeId::new(4),
            clock: rumoca_core::TypeId::new(5),
        },
        ..flat::Model::default()
    };
    assert!(model.predefined_types.is_complete());
    add_aggregate_parameters(&mut model);
    add_scalar_parameters(&mut model);
    model
}

#[test]
fn scalar_inventory_preserves_aggregate_index_string_and_modifier_dependencies() {
    let mut ctx = Context::new();
    ctx.build_parameter_lookup(&inventory_model(), &source_backed_tree());
    assert_eq!(ctx.parameter_values.get("n"), Some(&3));
    assert_eq!(ctx.real_parameter_values.get("gain"), Some(&5.0));
    assert_eq!(ctx.boolean_parameter_values.get("enabled"), Some(&true));
    assert_eq!(ctx.parameter_values.get("emptyCount"), Some(&0));
    assert_eq!(ctx.parameter_values.get("parent.child.chosen"), Some(&7));
    for name in [
        "dimensions",
        "dimensionAlias",
        "singleton",
        "empty",
        "label",
        "labelAlias",
    ] {
        assert!(ctx.aggregate_parameter_values.contains_key(name), "{name}");
        assert!(!ctx.parameter_values.contains_key(name), "{name}");
        assert!(!ctx.real_parameter_values.contains_key(name), "{name}");
        assert!(!ctx.boolean_parameter_values.contains_key(name), "{name}");
    }
    assert_eq!(ctx.array_dimensions.get("singleton"), Some(&vec![1]));
    assert_eq!(ctx.array_dimensions.get("empty"), Some(&vec![0]));
}
