use super::*;

fn var_ref_with_target_def(name: &str, target_def_id: DefId) -> Expression {
    let parts = crate::path_utils::segments(name)
        .into_iter()
        .map(|segment| rumoca_core::ComponentRefPart {
            ident: segment.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        })
        .collect();
    let component_ref = rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts,
        def_id: Some(target_def_id),
    };
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(name, component_ref),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn modified_binding_dimensions_replace_stale_declared_shape() {
    let mut ctx = Context::new();
    let tree = source_backed_tree();
    let mut flat = flat::Model::default();
    let ambiguous_mr_def = DefId::new(42);
    ctx.target_def_names
        .insert(ambiguous_mr_def, "mr".to_string());

    for (name, binding, binding_from_modification) in [
        ("mr", int_lit(3), false),
        ("aimsM.mr", int_lit(5), true),
        (
            "aimsM.rotor.m",
            var_ref_with_target_def("aimsM.mr", ambiguous_mr_def),
            true,
        ),
    ] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(binding),
                binding_from_modification,
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let r_name = rumoca_core::VarName::new("aimsM.rotor.resistor.R");
    flat.add_variable(
        r_name.clone(),
        flat::Variable {
            name: r_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            dims: vec![3],
            binding: Some(Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args: vec![int_lit(1), var_ref("aimsM.rotor.m")],
                span: rumoca_core::Span::DUMMY,
            }),
            binding_from_modification: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let local_m = rumoca_core::VarName::new("aimsM.rotor.resistor.m");
    flat.add_variable(
        local_m.clone(),
        flat::Variable {
            name: local_m,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(var_ref_with_target_def("aimsM.mr", ambiguous_mr_def)),
            binding_from_modification: true,
            is_discrete_type: true,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut overlay = InstanceOverlay::default();
    overlay.components.insert(
        InstanceId::new(1),
        symbolic_instance(
            InstanceId::new(1),
            "aimsM.rotor.resistor.R",
            vec![ast::Subscript::Expression(component_ref_expr(
                "aimsM.rotor.resistor.m",
            ))],
        ),
    );

    ctx.build_parameter_lookup(&flat, &tree);
    assert_eq!(ctx.get_integer_param("aimsM.rotor.m"), Some(5));
    assert_eq!(ctx.get_integer_param("aimsM.rotor.resistor.m"), Some(5));
    assert_eq!(
        ctx.array_dimensions.get("aimsM.rotor.resistor.R"),
        Some(&vec![3])
    );

    assert!(ctx.reconcile_modified_binding_dimensions(&mut flat));
    assert_eq!(
        flat.variables.get(&r_name).expect("R variable").dims,
        vec![5]
    );

    flat.variables.get_mut(&r_name).expect("R variable").dims = vec![3];
    assert!(
        ctx.recompute_symbolic_component_dimensions(&mut flat, &overlay, &tree)
            .expect("modified local m dimension is available to symbolic dimensions")
    );
    assert_eq!(
        flat.variables.get(&r_name).expect("R variable").dims,
        vec![5]
    );

    ctx.build_parameter_lookup(&flat, &tree);
    assert_eq!(
        ctx.array_dimensions.get("aimsM.rotor.resistor.R"),
        Some(&vec![5])
    );
}
