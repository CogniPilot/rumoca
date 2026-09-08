use super::*;

#[test]
fn preserves_relative_package_call_spelling_with_exact_target() {
    let (tree, rename_def) = spice3_relative_function_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    assert_relative_package_function_call_is_exact(
        &tree,
        &class_index,
        "Modelica.Electrical.Spice3.Internal.MOS",
        rename_def,
    );
    assert_relative_package_function_call_is_exact(
        &tree,
        &class_index,
        "Modelica.Electrical.Spice3.Semiconductors.M_PMOS",
        rename_def,
    );
}

fn spice3_relative_function_tree() -> (ast::ClassTree, rumoca_core::DefId) {
    let modelica_def = rumoca_core::DefId::new(1);
    let electrical_def = rumoca_core::DefId::new(2);
    let spice3_def = rumoca_core::DefId::new(3);
    let internal_def = rumoca_core::DefId::new(4);
    let mos_def = rumoca_core::DefId::new(5);
    let mos1_def = rumoca_core::DefId::new(6);
    let rename_def = rumoca_core::DefId::new(7);
    let semiconductors_def = rumoca_core::DefId::new(8);
    let pmos_def = rumoca_core::DefId::new(9);

    let rename = class(
        "mos1RenameParameters",
        rumoca_core::ClassType::Function,
        rename_def,
    );
    let mut mos1 = class("Mos1", rumoca_core::ClassType::Package, mos1_def);
    mos1.classes
        .insert("mos1RenameParameters".to_string(), rename);
    let mos = class("MOS", rumoca_core::ClassType::Model, mos_def);
    let mut internal = class("Internal", rumoca_core::ClassType::Package, internal_def);
    internal.classes.insert("MOS".to_string(), mos);
    internal.classes.insert("Mos1".to_string(), mos1);
    let mut spice3 = class("Spice3", rumoca_core::ClassType::Package, spice3_def);
    spice3.classes.insert("Internal".to_string(), internal);
    let mut pmos = class("M_PMOS", rumoca_core::ClassType::Model, pmos_def);
    pmos.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Modelica.Electrical.Spice3.Internal.MOS"),
        base_def_id: Some(mos_def),
        location: rumoca_core::Location::default(),
        modifications: Vec::new(),
        break_names: Vec::new(),
        is_protected: false,
        annotation: Vec::new(),
    });
    let mut semiconductors = class(
        "Semiconductors",
        rumoca_core::ClassType::Package,
        semiconductors_def,
    );
    semiconductors.classes.insert("M_PMOS".to_string(), pmos);
    spice3
        .classes
        .insert("Semiconductors".to_string(), semiconductors);
    let mut electrical = class(
        "Electrical",
        rumoca_core::ClassType::Package,
        electrical_def,
    );
    electrical.classes.insert("Spice3".to_string(), spice3);
    let mut modelica = class("Modelica", rumoca_core::ClassType::Package, modelica_def);
    modelica
        .classes
        .insert("Electrical".to_string(), electrical);

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    (tree, rename_def)
}

fn assert_relative_package_function_call_is_exact(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_scope: &str,
    rename_def: rumoca_core::DefId,
) {
    let mut expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Mos1.mos1RenameParameters",
            core_comp_ref(&["Mos1", "mos1RenameParameters"], rename_def),
        ),
        args: Vec::new(),
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };

    canonicalize_function_calls_in_expression_with_scope(
        &mut expr,
        tree,
        class_index,
        Some(source_scope),
    );

    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(
        name.as_str(),
        "Mos1.mos1RenameParameters",
        "canonical resolution must not replace the readable source occurrence spelling"
    );
    assert_eq!(name.target_def_id(), Some(rename_def));
    assert!(name.component_ref().is_some());
}

#[test]
fn test_collect_nested_function_calls() {
    let mut flat = flat::Model::new();

    // Create: 0 = outer(inner(x)) - y
    let inner_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("inner"),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![],
            span: test_span(),
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };
    let outer_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("outer"),
        args: vec![inner_call],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(outer_call),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("y"),
            subscripts: vec![],
            span: test_span(),
        }),
        span: test_span(),
    };
    flat.add_equation(flat::Equation::new(
        residual,
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let calls = collect_function_calls(&flat);
    assert!(calls.contains("inner"));
    assert!(calls.contains("outer"));
    assert_eq!(calls.len(), 2);
}

#[test]
fn validates_flat_boundary_rejects_non_executable_functions() {
    let mut flat = flat::Model::new();
    flat.add_function(rumoca_core::Function::new(
        "Modelica.Media.Interfaces.PartialMedium.setState_phX",
        rumoca_core::DefId::new(62_015),
        test_span(),
    ));

    let err = validate_flat_function_bindings(&flat)
        .expect_err("flat boundary must reject abstract function bindings");

    assert!(matches!(
        err,
        FlattenError::FunctionWithoutBody { ref name, .. }
            if name == "Modelica.Media.Interfaces.PartialMedium.setState_phX"
    ));
}

#[test]
fn validates_flat_boundary_allows_constructors_without_body() {
    let mut flat = flat::Model::new();
    let mut constructor =
        rumoca_core::Function::new("RecordType", rumoca_core::DefId::new(62_016), test_span());
    constructor.is_constructor = true;
    flat.add_function(constructor);

    validate_flat_function_bindings(&flat).expect("record constructors are structural");
}

#[test]
fn record_function_signature_keeps_constructor_as_structural_dependency() {
    let record_def_id = rumoca_core::DefId::new(41);
    let mut function =
        rumoca_core::Function::new("Pkg.makePose", rumoca_core::DefId::new(62_017), test_span());
    function.add_output(
        crate::test_support::aggregate_param("pose", "Pkg.Pose", Vec::new(), test_span())
            .with_type_class(rumoca_core::ClassType::Record)
            .with_type_def_id(record_def_id),
    );

    let dependencies = collect_function_dep_requests(&function);
    let constructor = dependencies
        .iter()
        .find(|dependency| dependency.name == "Pkg.Pose")
        .expect("record constructor should remain a structural dependency");
    assert_eq!(
        constructor.target_def_id,
        Some(record_def_id),
        "record constructor dependencies must preserve declaration identity"
    );
}

#[test]
fn contextualized_record_parameter_updates_declaration_identity() {
    let package_def = rumoca_core::DefId::new(1);
    let inherited_state_def = rumoca_core::DefId::new(2);
    let concrete_state_def = rumoca_core::DefId::new(3);
    let mut package = class("Pkg", rumoca_core::ClassType::Package, package_def);
    package.classes.insert(
        "State".to_string(),
        class("State", rumoca_core::ClassType::Record, concrete_state_def),
    );
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(62_018), test_span());
    function.add_input(
        crate::test_support::aggregate_param("state", "Pkg.State", Vec::new(), test_span())
            .with_type_class(rumoca_core::ClassType::Record)
            .with_type_def_id(inherited_state_def),
    );

    contextualize_record_param_type_names(&tree, &class_index, "Pkg.f", &mut function).unwrap();

    assert_eq!(function.inputs[0].type_name, "Pkg.State");
    assert_eq!(function.inputs[0].type_def_id, Some(concrete_state_def));
}

#[test]
fn contextualized_record_parameter_follows_replaceable_type_alias() {
    let package_def = rumoca_core::DefId::new(11);
    let record_def = rumoca_core::DefId::new(12);
    let alias_def = rumoca_core::DefId::new(13);
    let mut package = class("Pkg", rumoca_core::ClassType::Package, package_def);
    package.classes.insert(
        "Quaternion".to_string(),
        class("Quaternion", rumoca_core::ClassType::Record, record_def),
    );
    let mut alias = class("Orientation", rumoca_core::ClassType::Type, alias_def);
    alias.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Pkg.Quaternion"),
        base_def_id: Some(record_def),
        ..Default::default()
    });
    package.classes.insert("Orientation".to_string(), alias);

    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(62_019), test_span());
    function.add_input(
        crate::test_support::aggregate_param("orientation", "Orientation", Vec::new(), test_span())
            .with_type_class(rumoca_core::ClassType::Record)
            .with_type_def_id(record_def),
    );

    contextualize_record_param_type_names(&tree, &class_index, "Pkg.f", &mut function).unwrap();

    assert_eq!(function.inputs[0].type_name, "Pkg.Orientation");
    assert_eq!(function.inputs[0].type_def_id, Some(alias_def));
}

#[test]
fn contextualized_record_parameter_uses_resolved_identity_for_lexical_alias() {
    let package_def = rumoca_core::DefId::new(21);
    let record_def = rumoca_core::DefId::new(22);
    let mut package = class("Pkg", rumoca_core::ClassType::Package, package_def);
    package.classes.insert(
        "ComplexVoltage".to_string(),
        class("ComplexVoltage", rumoca_core::ClassType::Record, record_def),
    );
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(62_020), test_span());
    function.add_input(
        crate::test_support::aggregate_param(
            "voltage",
            "SI.ComplexVoltage",
            Vec::new(),
            test_span(),
        )
        .with_type_class(rumoca_core::ClassType::Record)
        .with_type_def_id(record_def),
    );

    contextualize_record_param_type_names(&tree, &class_index, "Pkg.f", &mut function).unwrap();

    assert_eq!(function.inputs[0].type_name, "Pkg.ComplexVoltage");
    assert_eq!(function.inputs[0].type_def_id, Some(record_def));
}

#[test]
fn validates_flat_boundary_allows_output_binding_functions() {
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new(
        "Modelica.Math.BooleanVectors.anyTrue",
        rumoca_core::DefId::new(62_021),
        test_span(),
    );
    function.add_output(
        crate::test_support::boolean_param("result", Vec::new(), test_span()).with_default(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: test_span(),
            },
        ),
    );
    flat.add_function(function);

    validate_flat_function_bindings(&flat)
        .expect("functions with output declaration bindings are executable");
}

#[test]
fn test_convert_component_to_param_prefers_binding_over_start_default() {
    let mut component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        location: test_location(9, 15),
        has_explicit_binding: true,
        start: ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: "0".into(),
                ..Default::default()
            },
            span: test_span(),
        },
        binding: Some(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: "3".into(),
                ..Default::default()
            },
            span: test_span(),
        }),
        ..ast::Component::empty_with_span(test_span())
    };

    let source_map = test_source_map();
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    component.type_name.def_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Integer"));
    component.type_def_id = component.type_name.def_id;
    let type_overlay = crate::test_support::type_overlay(&tree);
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "m",
        &component,
        &source_map,
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();
    assert!(matches!(
        param.default,
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            ..
        })
    ));
}

pub(super) fn integer_subscript(value: i64) -> ast::Subscript {
    ast::Subscript::Expression(ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..Default::default()
        },
        span: test_span(),
    })
}

fn size_subscript(
    reference: &str,
    reference_def_id: rumoca_core::DefId,
    dimension: i64,
) -> ast::Subscript {
    let predefined_size_def_id = rumoca_core::DefId::new(4200);
    ast::Subscript::Expression(ast::Expression::FunctionCall {
        comp: ast_comp_ref(&["size"], predefined_size_def_id),
        args: vec![
            ast::Expression::ComponentReference(ast_comp_ref(&[reference], reference_def_id)),
            ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: dimension.to_string().into(),
                    ..Default::default()
                },
                span: test_span(),
            },
        ],
        is_partial_application: false,
        span: test_span(),
    })
}

#[test]
fn test_convert_component_to_param_preserves_mixed_dynamic_rank() {
    let c0_def_id = rumoca_core::DefId::new(4201);
    let mut component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        location: test_location(9, 15),
        shape: vec![2],
        shape_expr: vec![size_subscript("c0", c0_def_id, 1), integer_subscript(2)],
        ..ast::Component::empty_with_span(test_span())
    };

    let source_map = test_source_map();
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    component.type_name.def_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"));
    component.type_def_id = component.type_name.def_id;
    let type_overlay = crate::test_support::type_overlay(&tree);
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "den2",
        &component,
        &source_map,
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dimensions(), [0, 2]);
    assert_eq!(param.shape_expr.len(), 2);
}

#[test]
fn test_convert_component_to_param_resolves_constant_shape_expr() {
    let package_def = rumoca_core::DefId::new(10);
    let n_state_def = rumoca_core::DefId::new(11);
    let mut package = class(
        "Xorshift64star",
        rumoca_core::ClassType::Package,
        package_def,
    );
    package.components.insert(
        "nState".to_string(),
        ast::Component {
            def_id: Some(n_state_def),
            type_name: ast::Name::from_string("Integer"),
            variability: rumoca_core::Variability::Constant(token("constant")),
            has_explicit_binding: true,
            binding: Some(ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: token("2"),
                span: test_span(),
            }),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.definitions
        .classes
        .insert("Xorshift64star".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();
    let n_state_ref = ast_comp_ref(&["nState"], n_state_def);
    let mut component = ast::Component {
        type_name: ast::Name::from_string("Integer"),
        location: test_location(9, 15),
        shape_expr: vec![ast::Subscript::Expression(
            ast::Expression::ComponentReference(n_state_ref),
        )],
        ..ast::Component::empty_with_span(test_span())
    };
    component.type_name.def_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Integer"));
    component.type_def_id = component.type_name.def_id;
    let param = convert_component_to_param(
        &class_index,
        "state",
        &component,
        &test_source_map(),
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dimensions(), [2]);
    assert!(matches!(
        param.shape_expr.as_slice(),
        [rumoca_core::Subscript::Index { value: 2, .. }]
    ));
}

#[test]
fn test_convert_component_to_param_inherits_type_alias_dims() {
    let orientation_id = rumoca_core::DefId::new(1);
    let matrix_id = rumoca_core::DefId::new(2);
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let real_id = crate::test_support::predefined_type_def_id(&tree, "Real");

    let mut matrix = ast::ClassDef {
        def_id: Some(matrix_id),
        name: rumoca_core::Token {
            text: "TransformationMatrix".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Type,
        array_subscripts: vec![integer_subscript(3), integer_subscript(3)],
        ..Default::default()
    };
    matrix.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Real"),
        base_def_id: Some(real_id),
        ..Default::default()
    });

    let mut orientation = ast::ClassDef {
        def_id: Some(orientation_id),
        name: rumoca_core::Token {
            text: "Orientation".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Type,
        ..Default::default()
    };
    orientation.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Pkg.Internal.TransformationMatrix"),
        base_def_id: Some(matrix_id),
        ..Default::default()
    });

    let mut package = ast::ClassDef {
        name: rumoca_core::Token {
            text: "Pkg".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    let mut internal = ast::ClassDef {
        name: rumoca_core::Token {
            text: "Internal".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    internal
        .classes
        .insert("TransformationMatrix".to_string(), matrix);
    package.classes.insert("Internal".to_string(), internal);
    package
        .classes
        .insert("Orientation".to_string(), orientation);

    tree.definitions.classes.insert("Pkg".to_string(), package);
    tree.def_map
        .insert(orientation_id, "Pkg.Orientation".to_string());
    tree.def_map
        .insert(matrix_id, "Pkg.Internal.TransformationMatrix".to_string());

    let mut type_name = ast::Name::from_string("Pkg.Orientation");
    type_name.def_id = Some(orientation_id);
    let component = ast::Component {
        type_name,
        location: test_location(9, 15),
        ..ast::Component::empty_with_span(test_span())
    };
    let source_map = test_source_map();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();
    let param = convert_component_to_param(
        &class_index,
        "T",
        &component,
        &source_map,
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dimensions(), [3, 3]);
    assert_eq!(param.type_name, "Real");
}

#[test]
fn test_constructor_signature_preserves_local_default_references() {
    let record_def = rumoca_core::DefId::new(1);
    let orientation_def = rumoca_core::DefId::new(2);
    let n_def = rumoca_core::DefId::new(3);
    let mut class_def = ast::ClassDef {
        name: rumoca_core::Token {
            text: "C".into(),
            ..Default::default()
        },
        def_id: Some(record_def),
        location: test_location(0, 8),
        class_type: rumoca_core::ClassType::Record,
        components: ast::AstIndexMap::from_iter([
            (
                "orientation".to_string(),
                ast::Component {
                    name: "orientation".to_string(),
                    def_id: Some(orientation_def),
                    type_name: ast::Name::from_string("Real"),
                    location: test_location(11, 27),
                    ..ast::Component::empty_with_span(test_span())
                },
            ),
            (
                "N".to_string(),
                ast::Component {
                    name: "N".to_string(),
                    def_id: Some(n_def),
                    type_name: ast::Name::from_string("Real"),
                    location: test_location(30, 37),
                    has_explicit_binding: true,
                    binding: Some(ast::Expression::ComponentReference(ast_comp_ref(
                        &["orientation"],
                        orientation_def,
                    ))),
                    ..ast::Component::empty_with_span(test_span())
                },
            ),
        ]),
        ..Default::default()
    };
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.def_map.insert(record_def, "Pkg.C".to_string());
    tree.def_map
        .insert(orientation_def, "Pkg.C.orientation".to_string());
    tree.def_map.insert(n_def, "Pkg.C.N".to_string());
    let real_def = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"))
        .expect("fixture tree owns predefined Real");
    for component in class_def.components.values_mut() {
        component.type_name.def_id = Some(real_def);
        component.type_def_id = Some(real_def);
    }
    let source_map = test_source_map();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();

    let constructor = convert_constructor_signature(
        &class_index,
        &class_def,
        "Pkg.C",
        &source_map,
        ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
        FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
    )
    .unwrap();

    assert_eq!(
        constructor.exposure_def_id, record_def,
        "the constructor signature carries the record declaration as its exposure"
    );
    let n_param = constructor
        .inputs
        .iter()
        .find(|param| param.name == "N")
        .expect("N constructor input");
    assert!(matches!(
        n_param.default,
        Some(rumoca_core::Expression::VarRef { ref name, .. }) if name.as_str() == "orientation"
    ));
}

#[test]
fn record_type_fields_preserve_short_operator_record_base_fields() {
    let complex_def = rumoca_core::DefId::new(101);
    let re_def = rumoca_core::DefId::new(102);
    let im_def = rumoca_core::DefId::new(103);
    let flux_def = rumoca_core::DefId::new(104);
    let mut complex = class("Complex", rumoca_core::ClassType::Record, complex_def);
    complex.operator_record = true;
    complex.location = test_location(0, 8);
    for (name, def_id, location) in [
        ("re", re_def, test_location(11, 27)),
        ("im", im_def, test_location(30, 37)),
    ] {
        complex.components.insert(
            name.to_string(),
            ast::Component {
                name: name.to_string(),
                def_id: Some(def_id),
                type_name: ast::Name::from_string("Real"),
                location,
                ..ast::Component::empty_with_span(test_span())
            },
        );
    }
    let mut flux = class(
        "ComplexMagneticFlux",
        rumoca_core::ClassType::Record,
        flux_def,
    );
    flux.operator_record = true;
    flux.location = test_location(0, 8);
    flux.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Complex"),
        base_def_id: Some(complex_def),
        ..Default::default()
    });

    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.source_map = test_source_map();
    tree.definitions
        .classes
        .insert("Complex".to_string(), complex);
    tree.definitions
        .classes
        .insert("ComplexMagneticFlux".to_string(), flux);
    let real_def = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"))
        .expect("fixture tree owns predefined Real");
    for class in tree.definitions.classes.values_mut() {
        for component in class.components.values_mut() {
            component.type_name.def_id = Some(real_def);
            component.type_def_id = Some(real_def);
        }
    }
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();
    let flux = class_index
        .get(flux_def)
        .expect("derived operator record class");

    let fields = record_type_fields(
        &class_index,
        flux,
        "ComplexMagneticFlux",
        &tree,
        FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
    )
    .expect("resolved record field metadata");

    assert_eq!(
        fields
            .iter()
            .map(|field| (field.name.as_str(), field.def_id))
            .collect::<Vec<_>>(),
        vec![("re", re_def), ("im", im_def)]
    );
}

#[test]
fn test_function_local_normalization_rewrites_self_qualified_default() {
    let orientation_def = rumoca_core::DefId::new(14_003);
    let mut function =
        rumoca_core::Function::new("Pkg.C", rumoca_core::DefId::new(62_022), test_span());
    function.add_input(
        crate::test_support::real_param("orientation", Vec::new(), test_span())
            .with_def_id(orientation_def),
    );
    function.add_input(
        crate::test_support::real_param("N", Vec::new(), test_span())
            .with_def_id(rumoca_core::DefId::new(14_004))
            .with_default(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "Pkg.C.orientation",
                    core_structured_comp_ref(&[
                        ("Pkg", rumoca_core::DefId::new(14_001)),
                        ("C", rumoca_core::DefId::new(14_002)),
                        ("orientation", orientation_def),
                    ]),
                ),
                subscripts: vec![],
                span: test_span(),
            }),
    );

    normalize_function_local_references(&mut function);

    let Some(rumoca_core::Expression::VarRef { name, .. }) = &function.inputs[1].default else {
        panic!("expected normalized reference default");
    };
    assert_eq!(name.as_str(), "orientation");
    assert_eq!(name.target_def_id(), Some(orientation_def));
    assert!(
        name.parts()
            .iter()
            .all(|part| part.def_id.index() != 0 && !part.span.is_dummy())
    );
}
