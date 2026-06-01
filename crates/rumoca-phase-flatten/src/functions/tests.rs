use super::*;
use std::sync::Arc;

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        ..rumoca_core::Token::default()
    }
}

fn class(
    name: &str,
    class_type: rumoca_core::ClassType,
    def_id: rumoca_core::DefId,
) -> ast::ClassDef {
    ast::ClassDef {
        name: token(name),
        class_type,
        def_id: Some(def_id),
        ..ast::ClassDef::default()
    }
}

fn core_comp_ref(parts: &[&str]) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: Span::DUMMY,
        parts: parts
            .iter()
            .map(|part| rumoca_core::ComponentRefPart {
                ident: part.to_string(),
                span: Span::DUMMY,
                subs: Vec::new(),
            })
            .collect(),
        def_id: None,
    }
}

fn core_comp_ref_with_def_id(
    parts: &[&str],
    def_id: rumoca_core::DefId,
) -> rumoca_core::ComponentReference {
    let mut reference = core_comp_ref(parts);
    reference.def_id = Some(def_id);
    reference
}

fn ast_comp_ref(parts: &[&str]) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: parts
            .iter()
            .map(|part| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
            })
            .collect(),
        span: Span::DUMMY,
        def_id: None,
    }
}

#[test]
fn canonicalize_collected_function_calls_uses_unique_suffix_match() {
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new("Modelica.Math.Polynomials.fitting", Span::DUMMY);
    function
        .body
        .push(rumoca_core::Statement::Return { span: Span::DUMMY });
    flat.add_function(function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Polynomials.fitting"),
            args: vec![],
            is_constructor: false,
            span: Span::DUMMY,
        },
        Span::DUMMY,
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls(&mut flat);

    let rumoca_core::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call residual");
    };
    assert_eq!(name.as_str(), "Modelica.Math.Polynomials.fitting");
}

#[test]
fn canonicalize_collected_function_calls_visits_when_clauses() {
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.Events.trip", Span::DUMMY);
    function
        .body
        .push(rumoca_core::Statement::Return { span: Span::DUMMY });
    flat.add_function(function);

    let mut when = flat::WhenClause::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Events.trip"),
            args: vec![],
            is_constructor: false,
            span: Span::DUMMY,
        },
        Span::DUMMY,
    );
    when.add_equation(flat::WhenEquation::Conditional {
        branches: vec![(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Events.trip"),
                args: vec![],
                is_constructor: false,
                span: Span::DUMMY,
            },
            vec![flat::WhenEquation::FunctionCallOutputs {
                outputs: vec![rumoca_core::VarName::new("y")],
                function: rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Events.trip"),
                    args: vec![],
                    is_constructor: false,
                    span: Span::DUMMY,
                },
                span: Span::DUMMY,
                origin: "when function call".to_string(),
            }],
        )],
        else_branch: vec![flat::WhenEquation::Assign {
            target: rumoca_core::VarName::new("y"),
            value: rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Events.trip"),
                args: vec![],
                is_constructor: false,
                span: Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: "when assignment".to_string(),
        }],
        span: Span::DUMMY,
        origin: "nested when branch".to_string(),
    });
    flat.when_clauses.push(when);

    canonicalize_collected_function_calls(&mut flat);

    assert_function_call_name(&flat.when_clauses[0].condition, "Pkg.Events.trip");
    let flat::WhenEquation::Conditional {
        branches,
        else_branch,
        ..
    } = &flat.when_clauses[0].equations[0]
    else {
        panic!("expected conditional when equation");
    };
    assert_function_call_name(&branches[0].0, "Pkg.Events.trip");
    let flat::WhenEquation::FunctionCallOutputs { function, .. } = &branches[0].1[0] else {
        panic!("expected function-call output when equation");
    };
    assert_function_call_name(function, "Pkg.Events.trip");
    let flat::WhenEquation::Assign { value, .. } = &else_branch[0] else {
        panic!("expected assignment when equation");
    };
    assert_function_call_name(value, "Pkg.Events.trip");
}

fn assert_function_call_name(expr: &rumoca_core::Expression, expected: &str) {
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call expression, got {expr:?}");
    };
    assert_eq!(name.as_str(), expected);
}

#[test]
fn canonicalize_collected_function_calls_leaves_ambiguous_suffix() {
    let mut flat = flat::Model::new();
    let mut math_function =
        rumoca_core::Function::new("Modelica.Math.Polynomials.fitting", Span::DUMMY);
    math_function
        .body
        .push(rumoca_core::Statement::Return { span: Span::DUMMY });
    flat.add_function(math_function);
    let mut user_function = rumoca_core::Function::new("User.Polynomials.fitting", Span::DUMMY);
    user_function
        .body
        .push(rumoca_core::Statement::Return { span: Span::DUMMY });
    flat.add_function(user_function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Polynomials.fitting"),
            args: vec![],
            is_constructor: false,
            span: Span::DUMMY,
        },
        Span::DUMMY,
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls(&mut flat);

    let rumoca_core::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call residual");
    };
    assert_eq!(name.as_str(), "Polynomials.fitting");
}

#[test]
fn validates_function_output_assignment_before_return() {
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"]),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });

    validate_function_outputs_assigned(&function).expect("output assignment is valid");
}

#[test]
fn rejects_function_output_only_assigned_after_return() {
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    function
        .body
        .push(rumoca_core::Statement::Return { span: Span::DUMMY });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"]),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });

    let err = validate_function_outputs_assigned(&function).expect_err("output is unassigned");
    assert!(matches!(
        err,
        FlattenError::FunctionOutputUnassigned {
            ref function,
            ref output,
            ..
        } if function == "Pkg.f" && output == "y"
    ));
}

#[test]
fn test_collect_no_function_calls() {
    let flat = flat::Model::new();
    let calls = collect_function_calls(&flat);
    assert!(calls.is_empty());
}

#[test]
fn test_collect_function_call_in_equation() {
    let mut flat = flat::Model::new();

    // Create an equation with a function call: 0 = myFunc(x) - y
    let func_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("MyPackage.myFunc"),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(func_call),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("y"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    flat.add_equation(flat::Equation::new(
        residual,
        Span::DUMMY,
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let calls = collect_function_calls(&flat);
    assert!(calls.contains("MyPackage.myFunc"));
    assert_eq!(calls.len(), 1);
}

#[test]
fn function_request_collection_deduplicates_by_def_id() {
    let target_def_id = rumoca_core::DefId::new(42);
    let mut function = rumoca_core::Function::new("Pkg.wrapper", Span::DUMMY);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"]),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "Alias.f",
                core_comp_ref_with_def_id(&["Alias", "f"], target_def_id),
            ),
            args: Vec::new(),
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["z"]),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "Pkg.f",
                core_comp_ref_with_def_id(&["Alias", "f"], target_def_id),
            ),
            args: Vec::new(),
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    });

    let calls = collect_function_dep_requests(&function);

    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].target_def_id, Some(target_def_id));
}

#[test]
fn target_def_id_request_keeps_concrete_exposed_package() {
    let partial_pkg_def = rumoca_core::DefId::new(1);
    let inherited_fn_def = rumoca_core::DefId::new(2);
    let concrete_pkg_def = rumoca_core::DefId::new(3);

    let mut inherited_fn = class(
        "specificEnthalpy_pTX",
        rumoca_core::ClassType::Function,
        inherited_fn_def,
    );
    inherited_fn.algorithms.push(vec![ast::Statement::Return {
        token: token("return"),
    }]);

    let mut partial_pkg = class(
        "PartialMedium",
        rumoca_core::ClassType::Package,
        partial_pkg_def,
    );
    partial_pkg
        .classes
        .insert("specificEnthalpy_pTX".to_string(), inherited_fn);

    let mut concrete_pkg = class(
        "ReferenceMoistAir",
        rumoca_core::ClassType::Package,
        concrete_pkg_def,
    );
    concrete_pkg.extends.push(ast::Extend {
        base_name: ast::Name {
            def_id: Some(partial_pkg_def),
            ..ast::Name::from_string("PartialMedium")
        },
        base_def_id: Some(partial_pkg_def),
        ..ast::Extend::default()
    });

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_pkg);
    tree.definitions
        .classes
        .insert("ReferenceMoistAir".to_string(), concrete_pkg);
    tree.def_map
        .insert(partial_pkg_def, "PartialMedium".to_string());
    tree.def_map.insert(
        inherited_fn_def,
        "PartialMedium.specificEnthalpy_pTX".to_string(),
    );
    tree.def_map
        .insert(concrete_pkg_def, "ReferenceMoistAir".to_string());

    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let request = FunctionRequest {
        name: "ReferenceMoistAir.specificEnthalpy_pTX".to_string(),
        target_def_id: Some(inherited_fn_def),
        component_ref: Some(core_comp_ref_with_def_id(
            &["ReferenceMoistAir", "specificEnthalpy_pTX"],
            inherited_fn_def,
        )),
    };

    let (resolved_name, function) = lookup_function_request(&tree, &class_index, &request)
        .expect("lookup should not error")
        .expect("inherited function should resolve through concrete package");

    assert_eq!(resolved_name, "ReferenceMoistAir.specificEnthalpy_pTX");
    assert_eq!(
        function.name.as_str(),
        "ReferenceMoistAir.specificEnthalpy_pTX"
    );
    assert_eq!(function.def_id, Some(inherited_fn_def));
}

#[test]
fn canonicalizes_relative_package_function_call_from_source_scope() {
    let (tree, rename_def) = spice3_relative_function_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    assert_relative_package_function_call_canonicalizes(
        &tree,
        &class_index,
        "Modelica.Electrical.Spice3.Internal.MOS",
        rename_def,
    );
    assert_relative_package_function_call_canonicalizes(
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

fn assert_relative_package_function_call_canonicalizes(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_scope: &str,
    rename_def: rumoca_core::DefId,
) {
    let mut expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Mos1.mos1RenameParameters",
            core_comp_ref(&["Mos1", "mos1RenameParameters"]),
        ),
        args: Vec::new(),
        is_constructor: false,
        span: Span::DUMMY,
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
        "Modelica.Electrical.Spice3.Internal.Mos1.mos1RenameParameters"
    );
    assert_eq!(name.target_def_id(), Some(rename_def));
    assert!(name.has_structure());
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
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let outer_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("outer"),
        args: vec![inner_call],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(outer_call),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("y"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    flat.add_equation(flat::Equation::new(
        residual,
        Span::DUMMY,
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
        Span::DUMMY,
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
    let mut constructor = rumoca_core::Function::new("RecordType", Span::DUMMY);
    constructor.is_constructor = true;
    flat.add_function(constructor);

    validate_flat_function_bindings(&flat).expect("record constructors are structural");
}

#[test]
fn validates_flat_boundary_allows_output_binding_functions() {
    let mut flat = flat::Model::new();
    let mut function =
        rumoca_core::Function::new("Modelica.Math.BooleanVectors.anyTrue", Span::DUMMY);
    function.add_output(
        rumoca_core::FunctionParam::new("result", "Boolean").with_default(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: Span::DUMMY,
            },
        ),
    );
    flat.add_function(function);

    validate_flat_function_bindings(&flat)
        .expect("functions with output declaration bindings are executable");
}

#[test]
fn test_convert_component_to_param_prefers_binding_over_start_default() {
    let component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        has_explicit_binding: true,
        start: ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: "0".into(),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        },
        binding: Some(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: "3".into(),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }),
        ..Default::default()
    };

    let def_map = crate::ResolveDefMap::default();
    let source_map = rumoca_core::SourceMap::new();
    let tree = ast::ClassTree::new();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "m",
        &component,
        &source_map,
        &def_map,
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

fn integer_subscript(value: i64) -> ast::Subscript {
    ast::Subscript::Expression(ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..Default::default()
        },
        span: rumoca_core::Span::DUMMY,
    })
}

fn size_subscript(reference: &str, dimension: i64) -> ast::Subscript {
    ast::Subscript::Expression(ast::Expression::FunctionCall {
        comp: ast_comp_ref(&["size"]),
        args: vec![
            ast::Expression::ComponentReference(ast_comp_ref(&[reference])),
            ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: dimension.to_string().into(),
                    ..Default::default()
                },
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    })
}

#[test]
fn test_convert_component_to_param_preserves_mixed_dynamic_rank() {
    let component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        shape: vec![2],
        shape_expr: vec![size_subscript("c0", 1), integer_subscript(2)],
        ..Default::default()
    };

    let def_map = crate::ResolveDefMap::default();
    let source_map = rumoca_core::SourceMap::new();
    let tree = ast::ClassTree::new();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "den2",
        &component,
        &source_map,
        &def_map,
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dims, vec![0, 2]);
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
                span: Span::DUMMY,
            }),
            ..Default::default()
        },
    );

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("Xorshift64star".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut n_state_ref = ast_comp_ref(&["nState"]);
    n_state_ref.def_id = Some(n_state_def);
    let component = ast::Component {
        type_name: ast::Name::from_string("Integer"),
        shape_expr: vec![ast::Subscript::Expression(
            ast::Expression::ComponentReference(n_state_ref),
        )],
        ..Default::default()
    };

    let param = convert_component_to_param(
        &class_index,
        "state",
        &component,
        &rumoca_core::SourceMap::new(),
        &crate::ResolveDefMap::default(),
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dims, vec![2]);
    assert!(matches!(
        param.shape_expr.as_slice(),
        [rumoca_core::Subscript::Index { value: 2, .. }]
    ));
}

#[test]
fn test_convert_component_to_param_inherits_type_alias_dims() {
    let orientation_id = rumoca_core::DefId::new(1);
    let matrix_id = rumoca_core::DefId::new(2);

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

    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    tree.def_map
        .insert(orientation_id, "Pkg.Orientation".to_string());
    tree.def_map
        .insert(matrix_id, "Pkg.Internal.TransformationMatrix".to_string());

    let mut type_name = ast::Name::from_string("Pkg.Orientation");
    type_name.def_id = Some(orientation_id);
    let component = ast::Component {
        type_name,
        ..Default::default()
    };
    let source_map = rumoca_core::SourceMap::new();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "T",
        &component,
        &source_map,
        &crate::ResolveDefMap::default(),
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dims, vec![3, 3]);
}

#[test]
fn test_constructor_signature_preserves_local_default_references() {
    let orientation_def = rumoca_core::DefId::new(1);
    let n_def = rumoca_core::DefId::new(2);
    let class_def = ast::ClassDef {
        name: rumoca_core::Token {
            text: "C".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Record,
        components: ast::AstIndexMap::from_iter([
            (
                "orientation".to_string(),
                ast::Component {
                    name: "orientation".to_string(),
                    def_id: Some(orientation_def),
                    type_name: ast::Name::from_string("Real"),
                    ..Default::default()
                },
            ),
            (
                "N".to_string(),
                ast::Component {
                    name: "N".to_string(),
                    def_id: Some(n_def),
                    type_name: ast::Name::from_string("Real"),
                    has_explicit_binding: true,
                    binding: Some(ast::Expression::ComponentReference(
                        ast::ComponentReference {
                            local: false,
                            parts: vec![],
                            def_id: Some(orientation_def),
                            span: rumoca_core::Span::DUMMY,
                        },
                    )),
                    ..Default::default()
                },
            ),
        ]),
        ..Default::default()
    };
    let mut tree = ast::ClassTree::default();
    tree.def_map
        .insert(orientation_def, "Pkg.C.orientation".to_string());
    tree.def_map.insert(n_def, "Pkg.C.N".to_string());
    let source_map = rumoca_core::SourceMap::new();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    let constructor = convert_constructor_signature(
        &class_index,
        &class_def,
        "Pkg.C",
        &source_map,
        &tree.def_map,
    )
    .unwrap();

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
fn test_function_local_normalization_rewrites_self_qualified_default() {
    let mut function = rumoca_core::Function::new("Pkg.C", Span::DUMMY);
    function.add_input(rumoca_core::FunctionParam::new("orientation", "Real"));
    function.add_input(rumoca_core::FunctionParam::new("N", "Real").with_default(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Pkg.C.orientation"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
    ));

    normalize_function_local_references(&mut function);

    assert!(matches!(
        function.inputs[1].default,
        Some(rumoca_core::Expression::VarRef { ref name, .. }) if name.as_str() == "orientation"
    ));
}

#[test]
fn test_function_context_inherits_base_lexical_imports() {
    let (tree, derived_function) = function_context_inheritance_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut member_cache = qualify::MemberDefIdCache::default();
    let context =
        collect_function_context(&tree, &class_index, &derived_function, &mut member_cache);

    assert_eq!(
        context.imports.get("pi").map(String::as_str),
        Some("Modelica.Constants.pi")
    );
    assert!(context.components.contains_key("crossArea"));
}

fn function_context_inheritance_tree() -> (ast::ClassTree, ast::ClassDef) {
    let base_def = rumoca_core::DefId::new(1);
    let derived_def = rumoca_core::DefId::new(2);
    let root_package_def = rumoca_core::DefId::new(3);
    let base_package_def = rumoca_core::DefId::new(4);
    let derived_package_def = rumoca_core::DefId::new(5);

    let base_function = base_pressure_loss_function(base_def);
    let derived_function = ast::ClassDef {
        def_id: Some(derived_def),
        name: rumoca_core::Token {
            text: "pressureLoss".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Function,
        extends: vec![ast::Extend {
            base_name: ast::Name::from_string("P.Base.pressureLoss"),
            base_def_id: Some(base_def),
            ..Default::default()
        }],
        ..Default::default()
    };
    let base_package = ast::ClassDef {
        def_id: Some(base_package_def),
        name: rumoca_core::Token {
            text: "Base".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        imports: vec![ast::Import::Qualified {
            path: ast::Name::from_string("Modelica.Constants.pi"),
            location: rumoca_core::Location::default(),
            global_scope: false,
        }],
        classes: ast::AstIndexMap::from_iter([("pressureLoss".to_string(), base_function)]),
        ..Default::default()
    };
    let derived_package = ast::ClassDef {
        def_id: Some(derived_package_def),
        name: rumoca_core::Token {
            text: "Derived".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        classes: ast::AstIndexMap::from_iter([(
            "pressureLoss".to_string(),
            derived_function.clone(),
        )]),
        ..Default::default()
    };
    let root_package = ast::ClassDef {
        def_id: Some(root_package_def),
        name: rumoca_core::Token {
            text: "P".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        classes: ast::AstIndexMap::from_iter([
            ("Base".to_string(), base_package),
            ("Derived".to_string(), derived_package),
        ]),
        ..Default::default()
    };

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("P".to_string(), root_package);
    register_function_context_inheritance_names(
        &mut tree,
        [
            (root_package_def, "P"),
            (base_package_def, "P.Base"),
            (derived_package_def, "P.Derived"),
            (base_def, "P.Base.pressureLoss"),
            (derived_def, "P.Derived.pressureLoss"),
        ],
    );
    (tree, derived_function)
}

fn base_pressure_loss_function(base_def: rumoca_core::DefId) -> ast::ClassDef {
    ast::ClassDef {
        def_id: Some(base_def),
        name: rumoca_core::Token {
            text: "pressureLoss".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Function,
        components: ast::AstIndexMap::from_iter([("crossArea".to_string(), pi_component())]),
        ..Default::default()
    }
}

fn pi_component() -> ast::Component {
    ast::Component {
        name: "crossArea".to_string(),
        type_name: ast::Name::from_string("Real"),
        binding: Some(ast::Expression::ComponentReference(
            ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: rumoca_core::Token {
                        text: "pi".into(),
                        ..Default::default()
                    },
                    subs: None,
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
        )),
        ..Default::default()
    }
}

fn register_function_context_inheritance_names(
    tree: &mut ast::ClassTree,
    names: [(rumoca_core::DefId, &str); 5],
) {
    for (def_id, name) in names {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
}

#[test]
fn test_extract_derivative_annotation_simple() {
    use rumoca_core::Token;
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference};
    use std::sync::Arc;

    // Test: annotation(derivative = myFunc_der)
    let annotations = vec![ast::Expression::NamedArgument {
        name: Token {
            text: Arc::from("derivative"),
            ..Default::default()
        },
        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("myFunc_der"),
                    ..Default::default()
                },
                subs: None,
            }],
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert!(derivs[0].zero_derivative.is_empty());
    assert!(derivs[0].no_derivative.is_empty());
}

#[test]
fn test_extract_derivative_annotation_with_modification() {
    use rumoca_core::Token;
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference};
    use std::sync::Arc;

    // Test: annotation(derivative(order=2) = myFunc_der2)
    // This is represented as a Modification with target having subscripts
    let annotations = vec![ast::Expression::Modification {
        target: ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("derivative"),
                    ..Default::default()
                },
                subs: Some(vec![ast::Subscript::Expression(
                    ast::Expression::NamedArgument {
                        name: Token {
                            text: Arc::from("order"),
                            ..Default::default()
                        },
                        value: Arc::new(ast::Expression::Terminal {
                            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                            token: Token {
                                text: Arc::from("2"),
                                ..Default::default()
                            },
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                )]),
            }],
            span: rumoca_core::Span::DUMMY,
        },
        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("myFunc_der2"),
                    ..Default::default()
                },
                subs: None,
            }],
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der2");
    assert_eq!(derivs[0].order, 2);
}

#[test]
fn test_extract_derivative_annotation_with_zero_derivative() {
    use rumoca_core::Token;
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference};
    use std::sync::Arc;

    // Test: annotation(derivative(zeroDerivative=k) = myFunc_der)
    let annotations = vec![ast::Expression::Modification {
        target: ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("derivative"),
                    ..Default::default()
                },
                subs: Some(vec![ast::Subscript::Expression(
                    ast::Expression::NamedArgument {
                        name: Token {
                            text: Arc::from("zeroDerivative"),
                            ..Default::default()
                        },
                        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
                            local: false,
                            def_id: None,
                            parts: vec![ComponentRefPart {
                                ident: Token {
                                    text: Arc::from("k"),
                                    ..Default::default()
                                },
                                subs: None,
                            }],
                            span: rumoca_core::Span::DUMMY,
                        })),
                        span: rumoca_core::Span::DUMMY,
                    },
                )]),
            }],
            span: rumoca_core::Span::DUMMY,
        },
        value: Arc::new(ast::Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: Arc::from("myFunc_der"),
                    ..Default::default()
                },
                subs: None,
            }],
            span: rumoca_core::Span::DUMMY,
        })),
        span: rumoca_core::Span::DUMMY,
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert_eq!(derivs[0].zero_derivative, vec!["k"]);
}
