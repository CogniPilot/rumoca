use super::*;
use indexmap::IndexMap;
use rumoca_core::TypeId;
use rumoca_ir_ast as ast;

fn create_test_model() -> flat::Model {
    let mut flat = flat::Model::new();

    // Add Pin.v (non-flow)
    let pin_v = flat::Variable {
        name: flat::VarName::new("pin.v"),
        flow: false,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("pin.v"), pin_v);

    // Add Pin.i (flow)
    let pin_i = flat::Variable {
        name: flat::VarName::new("pin.i"),
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("pin.i"), pin_i);

    flat
}

#[test]
fn test_connection_involves_disabled_handles_dot_inside_bracket_expression() {
    let conn = ast::InstanceConnection {
        a: ast::QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
            ],
        },
        b: ast::QualifiedName::from_ident("sink"),
        connector_type: None,
        span: Span::DUMMY,
        scope: String::new(),
    };

    let mut disabled = std::collections::HashSet::new();
    disabled.insert("bus[data.medium].pin".to_string());
    assert!(connection_involves_disabled(&conn, &disabled));
}

#[test]
fn test_connection_involves_disabled_ignores_non_matching_bracket_expression() {
    let conn = ast::InstanceConnection {
        a: ast::QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
            ],
        },
        b: ast::QualifiedName::from_ident("sink"),
        connector_type: None,
        span: Span::DUMMY,
        scope: String::new(),
    };

    let mut disabled = std::collections::HashSet::new();
    disabled.insert("bus[data.other].pin".to_string());
    assert!(!connection_involves_disabled(&conn, &disabled));
}

#[test]
fn test_is_flow_variable() {
    let flat = create_test_model();

    // Pin.i is flow
    assert!(is_flow_variable(&flat, &flat::VarName::new("pin.i")));

    // Pin.v is not flow
    assert!(!is_flow_variable(&flat, &flat::VarName::new("pin.v")));

    // Unknown variable returns false
    assert!(!is_flow_variable(&flat, &flat::VarName::new("unknown")));
}

#[test]
fn test_is_stream_variable() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("pin.h_outflow"),
        flat::Variable {
            stream: true,
            ..Default::default()
        },
    );

    assert!(is_stream_variable(
        &flat,
        &flat::VarName::new("pin.h_outflow")
    ));
    assert!(!is_stream_variable(&flat, &flat::VarName::new("pin.v")));
}

#[test]
fn test_connect_primitive_vars_routes_streams_to_stream_set() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("a.h_outflow"),
        flat::Variable {
            stream: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("b.h_outflow"),
        flat::Variable {
            stream: true,
            ..Default::default()
        },
    );

    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    connect_primitive_vars(
        &flat::VarName::new("a.h_outflow"),
        &flat::VarName::new("b.h_outflow"),
        &flat,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
    );

    assert!(flow_pairs.is_empty());
    assert!(
        potential_uf.get_sets().is_empty(),
        "stream connect() must not generate potential equality sets"
    );
    assert_eq!(stream_uf.get_sets().len(), 1);
}

#[test]
fn test_is_flow_variable_subscripted_element_of_array_field() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..Default::default()
        },
    );
    assert!(is_flow_variable(&flat, &flat::VarName::new("arr.n.i[2]")));
}

#[test]
fn test_union_find() {
    let mut uf = UnionFind::new();

    let a = flat::VarName::new("a");
    let b = flat::VarName::new("b");
    let c = flat::VarName::new("c");

    // Initially, each is its own set
    assert_eq!(uf.find(&a), a);
    assert_eq!(uf.find(&b), b);

    // Union a and b
    uf.union(&a, &b);
    assert_eq!(uf.find(&a), uf.find(&b));

    // Union b and c
    uf.union(&b, &c);
    assert_eq!(uf.find(&a), uf.find(&c));

    // Should have one set with all three
    let sets = uf.get_sets();
    assert_eq!(sets.len(), 1);
    assert_eq!(sets.values().next().unwrap().len(), 3);
}

#[test]
fn test_create_equality_residual() {
    let lhs = var_to_expr(&flat::VarName::new("a"));
    let rhs = var_to_expr(&flat::VarName::new("b"));
    let residual = create_equality_residual(lhs, rhs);

    // Should be Binary { op: Sub, lhs: a, rhs: b }
    match residual {
        flat::Expression::Binary { op, .. } => {
            assert!(matches!(op, flat::OpBinary::Sub(_)));
        }
        _ => panic!("Expected Binary expression"),
    }
}

#[test]
fn test_create_sum() {
    let exprs = vec![
        var_to_expr(&flat::VarName::new("a")),
        var_to_expr(&flat::VarName::new("b")),
        var_to_expr(&flat::VarName::new("c")),
    ];

    let sum = create_sum(exprs);

    // Should be ((a + b) + c)
    match sum {
        flat::Expression::Binary { op, .. } => {
            assert!(matches!(op, flat::OpBinary::Add(_)));
        }
        _ => panic!("Expected Binary expression"),
    }
}

#[test]
fn test_generate_equality_equations() {
    let mut flat = flat::Model::new();

    // Add variables
    flat.add_variable(flat::VarName::new("r1.n.v"), flat::Variable::default());
    flat.add_variable(flat::VarName::new("r2.p.v"), flat::Variable::default());
    flat.add_variable(flat::VarName::new("r3.p.v"), flat::Variable::default());

    let vars = vec![
        flat::VarName::new("r1.n.v"),
        flat::VarName::new("r2.p.v"),
        flat::VarName::new("r3.p.v"),
    ];

    generate_equality_equations(&mut flat, &vars).unwrap();

    // Should generate 2 equations (n-1 for n=3)
    assert_eq!(flat.equations.len(), 2);

    // All variables should be marked as connected
    assert!(
        flat.variables
            .get(&flat::VarName::new("r1.n.v"))
            .unwrap()
            .connected
    );
    assert!(
        flat.variables
            .get(&flat::VarName::new("r2.p.v"))
            .unwrap()
            .connected
    );
    assert!(
        flat.variables
            .get(&flat::VarName::new("r3.p.v"))
            .unwrap()
            .connected
    );
}

#[test]
fn test_generate_flow_equation() {
    let mut flat = flat::Model::new();

    // Add flow variables
    let v1 = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("r1.n.i"), v1);

    let v2 = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("r2.p.i"), v2);

    let vars = vec![flat::VarName::new("r1.n.i"), flat::VarName::new("r2.p.i")];

    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, std::collections::HashSet<flat::VarName>>::new(),
    )
    .unwrap();

    // Should generate 1 equation (sum = 0)
    assert_eq!(flat.equations.len(), 1);

    // Variables should be marked as connected
    assert!(
        flat.variables
            .get(&flat::VarName::new("r1.n.i"))
            .unwrap()
            .connected
    );
    assert!(
        flat.variables
            .get(&flat::VarName::new("r2.p.i"))
            .unwrap()
            .connected
    );
}

#[test]
fn test_generate_flow_equation_marks_base_connected_for_subscripted_var() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("a.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("b.n.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );

    let vars = vec![flat::VarName::new("a.n.i[2]"), flat::VarName::new("b.n.i")];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, std::collections::HashSet<flat::VarName>>::new(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
    assert!(
        flat.variables
            .get(&flat::VarName::new("a.n.i"))
            .unwrap()
            .connected
    );
}

#[test]
fn test_generate_flow_equation_subscripted_unknown_dims_is_scalar() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("a.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("b.n.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );

    let vars = vec![flat::VarName::new("a.n.i[2]"), flat::VarName::new("b.n.i")];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, std::collections::HashSet<flat::VarName>>::new(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
}

#[test]
fn test_generate_flow_equation_mixed_scalar_and_array_is_scalar_sum() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("s.n.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );

    let vars = vec![flat::VarName::new("arr.n.i"), flat::VarName::new("s.n.i")];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, std::collections::HashSet<flat::VarName>>::new(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 1,
        "scalar+array flow connection sets should contribute one scalar flow-sum equation"
    );
}

#[test]
fn test_generate_flow_equation_two_arrays_and_scalar_keeps_array_scalar_count() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("arr1.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("arr2.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![2],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("s.n.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );

    let vars = vec![
        flat::VarName::new("arr1.n.i"),
        flat::VarName::new("arr2.n.i"),
        flat::VarName::new("s.n.i"),
    ];
    generate_flow_equation(
        &mut flat,
        &vars,
        "",
        &IndexMap::<String, std::collections::HashSet<flat::VarName>>::new(),
    )
    .unwrap();

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 2,
        "flow sum with multiple array terms should preserve array scalarization"
    );
}

#[test]
fn test_generate_flow_equation_sign_convention() {
    let mut flat = flat::Model::new();

    // Add inside connector (3 parts: component.connector.variable)
    let v_inside = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("r.p.i"), v_inside);

    // Add outside connector (2 parts: connector.variable)
    let v_outside = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("p.i"), v_outside);

    let vars = vec![flat::VarName::new("r.p.i"), flat::VarName::new("p.i")];

    let mut interface_flow_vars_by_scope = IndexMap::new();
    interface_flow_vars_by_scope.insert(
        String::new(),
        std::collections::HashSet::from([flat::VarName::new("p.i")]),
    );
    generate_flow_equation(&mut flat, &vars, "", &interface_flow_vars_by_scope).unwrap();

    // Should generate 1 equation
    assert_eq!(flat.equations.len(), 1);

    // Check the origin shows correct signs:
    // r.p.i is inside (+), p.i is outside (-)
    let origin = &flat.equations[0].origin;
    let origin_str = origin.to_string();
    assert!(
        origin_str.contains("r.p.i") && origin_str.contains("-p.i"),
        "Expected 'r.p.i + -p.i = 0', got: {}",
        origin_str
    );
}

#[test]
fn test_generate_flow_equation_sign_for_nested_outside_connector_member() {
    let mut flat = flat::Model::new();

    let outside_nested = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("port.frame.f"), outside_nested);

    let inside = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("comp.port.f"), inside);

    let vars = vec![
        flat::VarName::new("port.frame.f"),
        flat::VarName::new("comp.port.f"),
    ];
    let mut interface_flow_vars_by_scope = IndexMap::new();
    interface_flow_vars_by_scope.insert(
        String::new(),
        std::collections::HashSet::from([flat::VarName::new("port.frame.f")]),
    );
    generate_flow_equation(&mut flat, &vars, "", &interface_flow_vars_by_scope).unwrap();

    let origin = &flat.equations[0].origin;
    let origin_str = origin.to_string();
    assert!(
        origin_str.contains("-port.frame.f") && origin_str.contains("comp.port.f"),
        "Expected outside nested connector member to be negated, got: {}",
        origin_str
    );
}

#[test]
fn test_generate_flow_equation_uses_scope_specific_interface_flows() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("cell.p.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("cell.multiSensor.pc.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );

    let vars = vec![
        flat::VarName::new("cell.p.i"),
        flat::VarName::new("cell.multiSensor.pc.i"),
    ];
    let mut interface_flow_vars_by_scope = IndexMap::new();
    interface_flow_vars_by_scope.insert(
        "cell".to_string(),
        std::collections::HashSet::from([flat::VarName::new("cell.p.i")]),
    );
    generate_flow_equation(&mut flat, &vars, "cell", &interface_flow_vars_by_scope).unwrap();

    let origin_str = flat.equations[0].origin.to_string();
    assert!(
        origin_str.contains("-cell.p.i") && origin_str.contains("cell.multiSensor.pc.i"),
        "Expected nested scope outside connector to be negated, got: {}",
        origin_str
    );
}

#[test]
fn test_validate_flow_consistency_ok() {
    let mut flat = flat::Model::new();

    // Both flow
    let v1 = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a.i"), v1);

    let v2 = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b.i"), v2);

    // Should succeed
    let result = validate_flow_consistency(
        &flat,
        &flat::VarName::new("a.i"),
        &flat::VarName::new("b.i"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_flow_consistency_mismatch() {
    let mut flat = flat::Model::new();

    // One flow, one non-flow
    let v1 = flat::Variable {
        flow: true,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a.i"), v1);

    let v2 = flat::Variable {
        flow: false,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b.v"), v2);

    // Should fail
    let result = validate_flow_consistency(
        &flat,
        &flat::VarName::new("a.i"),
        &flat::VarName::new("b.v"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_dimension_compatibility_ok() {
    let mut flat = flat::Model::new();

    // Same dimensions
    let v1 = flat::Variable {
        dims: vec![3],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), v1);

    let v2 = flat::Variable {
        dims: vec![3],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), v2);

    // Should succeed
    let result = validate_dimension_compatibility(
        &flat,
        &flat::VarName::new("a"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_dimension_compatibility_mismatch() {
    let mut flat = flat::Model::new();

    // Different dimensions
    let v1 = flat::Variable {
        dims: vec![3],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), v1);

    let v2 = flat::Variable {
        dims: vec![5],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), v2);

    // Mismatched dimensions should fail
    let result = validate_dimension_compatibility(
        &flat,
        &flat::VarName::new("a"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_dimension_compatibility_io_mismatch_still_fails() {
    let mut flat = flat::Model::new();

    let v1 = flat::Variable {
        dims: vec![2],
        causality: flat::Causality::Input(flat::Token::default()),
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("u"), v1);

    let v2 = flat::Variable {
        dims: vec![3],
        causality: flat::Causality::Output(flat::Token::default()),
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("y"), v2);

    let result = validate_dimension_compatibility(
        &flat,
        &flat::VarName::new("u"),
        &flat::VarName::new("y"),
        Span::DUMMY,
    );
    assert!(
        result.is_err(),
        "MLS §9.2 requires connect() array dimensions to match even for input/output pairs"
    );
}

#[test]
fn test_validate_dimension_compatibility_partial_subscript_projects_remaining_dims() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![2, 3],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), lhs);

    let rhs = flat::Variable {
        dims: vec![3],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &flat::VarName::new("a[1]"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(
        result.is_ok(),
        "A[1] for A[2,3] should preserve trailing dimension [3]"
    );
}

#[test]
fn test_validate_dimension_compatibility_partial_subscript_mismatch_fails() {
    let mut flat = flat::Model::new();

    let lhs = flat::Variable {
        dims: vec![2, 3],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), lhs);

    let rhs = flat::Variable {
        dims: vec![4],
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), rhs);

    let result = validate_dimension_compatibility(
        &flat,
        &flat::VarName::new("a[1]"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(
        result.is_err(),
        "A[1] for A[2,3] has projected dims [3], so it must reject [4]"
    );
}

#[test]
fn test_split_trailing_index_groups_multi_index() {
    let (base, groups) =
        split_trailing_index_groups("connector.field[2][3]").expect("should parse");
    assert_eq!(base, "connector.field");
    assert_eq!(groups, vec!["[2]".to_string(), "[3]".to_string()]);
}

#[test]
fn test_validate_type_compatibility_ok() {
    let mut flat = flat::Model::new();
    let type_roots = IndexMap::new();

    // Both same type (type_id = 1 for both)
    let v1 = flat::Variable {
        type_id: TypeId(1), // Same type
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(1), // Same type
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), v2);

    // Should succeed
    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &flat::VarName::new("a"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_type_compatibility_mismatch() {
    let mut flat = flat::Model::new();
    let type_roots = IndexMap::new();

    // Different types (type_id = 1 vs 2)
    let v1 = flat::Variable {
        type_id: TypeId(1), // e.g., Real
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(2), // e.g., Integer
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), v2);

    // Should fail
    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &flat::VarName::new("a"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_err());
}

#[test]
fn test_validate_type_compatibility_unknown_allowed() {
    let mut flat = flat::Model::new();
    let type_roots = IndexMap::new();

    let v1 = flat::Variable {
        type_id: TypeId::UNKNOWN,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: TypeId(1),
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), v2);

    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &flat::VarName::new("a"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

#[test]
fn test_validate_type_compatibility_alias_root_allowed() {
    let mut flat = flat::Model::new();
    let mut type_roots = IndexMap::new();

    let alias = TypeId(9);
    let root = TypeId(1);
    type_roots.insert(alias, root);

    let v1 = flat::Variable {
        type_id: alias,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("a"), v1);

    let v2 = flat::Variable {
        type_id: root,
        ..Default::default()
    };
    flat.add_variable(flat::VarName::new("b"), v2);

    let result = validate_type_compatibility(
        &flat,
        &type_roots,
        &flat::VarName::new("a"),
        &flat::VarName::new("b"),
        Span::DUMMY,
    );
    assert!(result.is_ok());
}

// =============================================================================
// Array Index Matching Tests
// =============================================================================

#[test]
fn test_split_path_with_indices() {
    // Simple path
    assert_eq!(split_path_with_indices("a.b.c"), vec!["a", "b", "c"]);

    // Path with array indices
    assert_eq!(
        split_path_with_indices("resistor[1].p.v"),
        vec!["resistor[1]", "p", "v"]
    );

    // Multiple array indices
    assert_eq!(
        split_path_with_indices("plug_p.pin[2].v"),
        vec!["plug_p", "pin[2]", "v"]
    );

    // Complex indices
    assert_eq!(
        split_path_with_indices("a[1,2].b[3].c"),
        vec!["a[1,2]", "b[3]", "c"]
    );
}

#[test]
fn test_strip_array_index() {
    assert_eq!(strip_array_index("resistor[1]"), "resistor");
    assert_eq!(strip_array_index("pin[2]"), "pin");
    assert_eq!(strip_array_index("p"), "p");
    assert_eq!(strip_array_index("a[1,2,3]"), "a");
}

#[test]
fn test_strip_embedded_array_indices() {
    assert_eq!(
        strip_embedded_array_indices("battery.resistor[2].p.i"),
        Some("battery.resistor.p.i".to_string())
    );
    assert_eq!(
        strip_embedded_array_indices("battery.capacitor.p.i[2]"),
        Some("battery.capacitor.p.i".to_string())
    );
    assert_eq!(strip_embedded_array_indices("battery.p.i"), None);
}

#[test]
fn test_extract_array_index() {
    assert_eq!(extract_array_index("resistor[1]"), Some("[1]".to_string()));
    assert_eq!(extract_array_index("pin[2]"), Some("[2]".to_string()));
    assert_eq!(extract_array_index("p"), None);
    assert_eq!(extract_array_index("a[1,2,3]"), Some("[1,2,3]".to_string()));
}

#[test]
fn test_parse_array_element_ref_simple_and_indexed_prefix() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("x"),
        flat::Variable {
            dims: vec![3],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("cell[2].v"),
        flat::Variable {
            dims: vec![4],
            ..Default::default()
        },
    );

    assert_eq!(
        parse_array_element_ref("x[1]", &flat),
        Some((flat::VarName::new("x"), 1))
    );
    assert_eq!(
        parse_array_element_ref("cell[2].v[3]", &flat),
        Some((flat::VarName::new("cell[2].v"), 3))
    );
}

#[test]
fn test_parse_array_element_ref_rejects_non_scalar_or_non_terminal_subscripts() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("x"),
        flat::Variable {
            dims: vec![2, 2],
            ..Default::default()
        },
    );

    assert_eq!(parse_array_element_ref("x[1,2]", &flat), None);
    assert_eq!(parse_array_element_ref("x[1].y", &flat), None);
}

#[test]
fn test_scalarize_collapsed_connector_element() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("s[1].inductance.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..Default::default()
        },
    );

    let scalarized = scalarize_collapsed_connector_element(
        &flat::VarName::new("s[1].inductance.n.i"),
        "s[1].inductance.n[2]",
        &flat,
    );
    assert_eq!(scalarized, flat::VarName::new("s[1].inductance.n.i[2]"));
}

#[test]
fn test_scalarize_collapsed_connector_element_without_dims_still_scalarizes() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("cell.cell.resistor.p.i"),
        flat::Variable {
            flow: true,
            // Collapsed connector-array fields can reach flatten with dims=[].
            dims: vec![],
            ..Default::default()
        },
    );

    let scalarized = scalarize_collapsed_connector_element(
        &flat::VarName::new("cell.cell.resistor.p.i"),
        "cell.cell.resistor[2].p",
        &flat,
    );
    assert_eq!(scalarized, flat::VarName::new("cell.cell.resistor.p.i[2]"));
}

#[test]
fn test_is_flow_variable_subscripted_with_unknown_dims() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            // Unknown dims in flat::Variable must still allow element flow handling.
            dims: vec![],
            ..Default::default()
        },
    );

    assert!(is_flow_variable(&flat, &flat::VarName::new("arr.n.i[2]")));
}

#[test]
fn test_matches_with_array_indices() {
    let matches = |name: &str, segments: &[&str]| {
        let parsed = split_path_with_indices(name)
            .into_iter()
            .map(std::borrow::ToOwned::to_owned)
            .collect::<Vec<_>>();
        matches_with_array_indices_cached(&parsed, segments)
    };

    // resistor.p should match resistor[1].p.v
    assert!(matches("resistor[1].p.v", &["resistor", "p"]));
    assert!(matches("resistor[2].p.i", &["resistor", "p"]));

    // plug_p.pin should match plug_p.pin[1].v
    assert!(matches("plug_p.pin[1].v", &["plug_p", "pin"]));

    // Exact match (no array indices)
    assert!(matches("r1.n.v", &["r1", "n"]));

    // No match - wrong base name
    assert!(!matches("resistor[1].p.v", &["capacitor", "p"]));

    // No match - not enough parts (no suffix)
    assert!(!matches("resistor[1].p", &["resistor", "p"]));
}

#[test]
fn test_find_exact_match_with_array_expansion_handles_dot_inside_subscript() {
    let var_names = [
        flat::VarName::new("bus[data.medium].pin"),
        flat::VarName::new("bus[data.other].pin"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(var_names.iter());

    let matches = find_exact_match_with_array_expansion("bus[data.medium].pin", &var_index);
    assert_eq!(matches, vec![flat::VarName::new("bus[data.medium].pin")]);
}

#[test]
fn test_find_sub_variables_with_array_expansion_handles_dot_inside_subscript() {
    let var_names = [
        flat::VarName::new("bus[data.medium].pin.i"),
        flat::VarName::new("bus[data.medium].pin.v"),
        flat::VarName::new("bus[data.other].pin.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(var_names.iter());

    let matches =
        find_sub_variables_with_array_expansion_indexed("bus[data.medium].pin", &var_index);
    assert_eq!(matches.len(), 2);
    assert!(matches.contains(&flat::VarName::new("bus[data.medium].pin.i")));
    assert!(matches.contains(&flat::VarName::new("bus[data.medium].pin.v")));
}

#[test]
fn test_extract_suffix_exact() {
    // Exact prefix match
    let result = extract_suffix("r1.n.v", "r1.n");
    assert_eq!(result, Some(("v".to_string(), "".to_string())));

    let result = extract_suffix("a.b.c.d", "a.b.c");
    assert_eq!(result, Some(("d".to_string(), "".to_string())));
}

#[test]
fn test_extract_suffix_with_array_indices() {
    // Array prefix match
    let result = extract_suffix("resistor[1].p.v", "resistor.p");
    assert_eq!(result, Some(("v".to_string(), "[1]".to_string())));

    let result = extract_suffix("plug_p.pin[2].i", "plug_p.pin");
    assert_eq!(result, Some(("i".to_string(), "[2]".to_string())));

    // Multiple segments with indices
    let result = extract_suffix("resistor[1].p.v", "resistor.p");
    assert_eq!(result, Some(("v".to_string(), "[1]".to_string())));
}

#[test]
fn test_extract_suffix_preserves_segment_index_when_name_is_collapsed_array_field() {
    let result = extract_suffix("s[1].inductance.n.i", "s[1].inductance.n[2]");
    assert_eq!(result, Some(("i".to_string(), "[1][2]".to_string())));
}

#[test]
fn test_extract_suffix_handles_dot_inside_subscript_expression() {
    let result = extract_suffix("bus[data.medium].pin.i", "bus[data.medium].pin[2]");
    assert_eq!(
        result,
        Some(("i".to_string(), "[data.medium][2]".to_string()))
    );
}

#[test]
fn test_find_matching_var_b_exact() {
    let subs_b = vec![
        flat::VarName::new("plug_p.pin.v"),
        flat::VarName::new("plug_p.pin.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    // Exact match
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("plug_p.pin.v")));

    let result = find_matching_var_b_indexed("i", "", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("plug_p.pin.i")));

    // No match
    let result = find_matching_var_b_indexed("x", "", &sub_match_index);
    assert_eq!(result, None);
}

#[test]
fn test_find_matching_var_b_exact_with_dotted_suffix() {
    let subs_b = vec![
        flat::VarName::new("plug_p.pin.inner.v"),
        flat::VarName::new("plug_p.pin.inner.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    let result = find_matching_var_b_indexed("inner.v", "", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("plug_p.pin.inner.v")));
}

#[test]
fn test_find_matching_var_b_with_array_indices() {
    let subs_b = vec![
        flat::VarName::new("plug_p.pin[1].v"),
        flat::VarName::new("plug_p.pin[1].i"),
        flat::VarName::new("plug_p.pin[2].v"),
        flat::VarName::new("plug_p.pin[2].i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    // Should find matching indexed variable
    let result = find_matching_var_b_indexed("v", "[1]", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("plug_p.pin[1].v")));

    let result = find_matching_var_b_indexed("i", "[2]", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("plug_p.pin[2].i")));

    // Wrong index
    let result = find_matching_var_b_indexed("v", "[3]", &sub_match_index);
    assert_eq!(result, None);
}

#[test]
fn test_find_matching_var_b_with_collapsed_indexed_connector_path() {
    let subs_b = vec![
        flat::VarName::new("s[1].n.i"),
        flat::VarName::new("s[1].n.v"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("s[1].n[2]", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("i", "", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("s[1].n.i")));
}

#[test]
fn test_find_matching_var_b_preserves_explicit_cross_index_path() {
    // Connects like resistor[1].n <-> resistor[2].p should use B's explicit
    // index even when A/B indices differ.
    let subs_b = vec![
        flat::VarName::new("cell.cell.resistor.p.v"),
        flat::VarName::new("cell.cell.resistor.p.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index =
        ConnectionSubMatchIndex::new("cell.cell.resistor[2].p", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("cell.cell.resistor.p.v")));
}

#[test]
fn test_find_matching_var_b_does_not_cross_match_connector_member_name() {
    let subs_b = vec![
        flat::VarName::new("resistor.p.v"),
        flat::VarName::new("resistor.n.v"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("resistor[1].n", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("resistor.n.v")));
}

#[test]
fn test_find_matching_var_b_allows_indexed_b_when_a_has_no_indices() {
    // Reproduces scalar-to-indexed connector matches like:
    // connect(internalHeatPort, resistor[1].heatPort)
    // where A suffix extraction yields empty indices.
    let subs_b = vec![
        flat::VarName::new("battery.resistor.heatPort.T"),
        flat::VarName::new("battery.resistor.heatPort.Q_flow"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index =
        ConnectionSubMatchIndex::new("battery.resistor[1].heatPort", &subs_b, &var_index);

    let t_match = find_matching_var_b_indexed("T", "", &sub_match_index);
    let q_match = find_matching_var_b_indexed("Q_flow", "", &sub_match_index);

    assert_eq!(
        t_match,
        Some(flat::VarName::new("battery.resistor.heatPort.T"))
    );
    assert_eq!(
        q_match,
        Some(flat::VarName::new("battery.resistor.heatPort.Q_flow"))
    );
}

#[test]
fn test_strip_explicit_path_indices() {
    assert_eq!(
        strip_explicit_path_indices("[1][2]", "s[1].inductance.n"),
        "[2]"
    );
    assert_eq!(
        strip_explicit_path_indices("[1][2]", "s[1].inductance.n[2]"),
        ""
    );
    assert_eq!(strip_explicit_path_indices("[2]", "plug_p.pin"), "[2]");
}

#[test]
fn test_find_matching_var_b_keeps_trailing_connector_index_with_explicit_prefix() {
    let subs_b = vec![
        flat::VarName::new("s[1].p[1].i"),
        flat::VarName::new("s[1].p[2].i"),
        flat::VarName::new("s[1].p[3].i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("s[1].p", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("i", "[2]", &sub_match_index);
    assert_eq!(result, Some(flat::VarName::new("s[1].p[2].i")));
}

#[test]
fn test_connect_sub_variable_indexes_collapsed_b_array_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("plug_p.pin[2].i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("plugs_n.pin.i"),
        flat::Variable {
            flow: true,
            dims: vec![3],
            ..Default::default()
        },
    );

    let sub_a = flat::VarName::new("plug_p.pin[2].i");
    let subs_b = vec![flat::VarName::new("plugs_n.pin.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("plugs_n.pin", &subs_b, &var_index);

    connect_sub_variable(
        &sub_a,
        "plug_p.pin",
        "plugs_n.pin",
        &sub_match_index,
        &mut ctx,
    );

    assert_eq!(
        flow_pairs,
        vec![(
            flat::VarName::new("plug_p.pin[2].i"),
            flat::VarName::new("plugs_n.pin.i[2]")
        )]
    );
}

#[test]
fn test_connect_sub_variable_does_not_index_scalar_b_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("resistor[1].p.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("r0.n.i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );

    let sub_a = flat::VarName::new("resistor[1].p.i");
    let subs_b = vec![flat::VarName::new("r0.n.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("r0.n", &subs_b, &var_index);

    connect_sub_variable(&sub_a, "resistor.p", "r0.n", &sub_match_index, &mut ctx);

    assert_eq!(
        flow_pairs,
        vec![(
            flat::VarName::new("resistor[1].p.i"),
            flat::VarName::new("r0.n.i")
        )]
    );
}

#[test]
fn test_connect_sub_variable_does_not_index_single_element_b_array_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("plug_p.pin[1].i"),
        flat::Variable {
            flow: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("starpoints.pin.i"),
        flat::Variable {
            flow: true,
            dims: vec![1],
            ..Default::default()
        },
    );

    let sub_a = flat::VarName::new("plug_p.pin[1].i");
    let subs_b = vec![flat::VarName::new("starpoints.pin.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("starpoints.pin", &subs_b, &var_index);

    connect_sub_variable(
        &sub_a,
        "plug_p.pin",
        "starpoints.pin",
        &sub_match_index,
        &mut ctx,
    );

    assert_eq!(
        flow_pairs,
        vec![(
            flat::VarName::new("plug_p.pin[1].i"),
            flat::VarName::new("starpoints.pin.i")
        )]
    );
}

#[test]
fn test_find_sub_variables_with_array_expansion() {
    let mut flat = flat::Model::new();

    // Add variables for resistor[1-3].p.v and resistor[1-3].p.i
    for i in 1..=3 {
        flat.add_variable(
            flat::VarName::new(format!("resistor[{}].p.v", i)),
            flat::Variable::default(),
        );
        flat.add_variable(
            flat::VarName::new(format!("resistor[{}].p.i", i)),
            flat::Variable {
                flow: true,
                ..Default::default()
            },
        );
    }

    // Searching for "resistor.p" should find all resistor[*].p.* variables
    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("resistor.p", &pc, &var_index);
    assert_eq!(subs.len(), 6);

    // Verify all expected variables are found
    for i in 1..=3 {
        assert!(subs.contains(&flat::VarName::new(format!("resistor[{}].p.v", i))));
        assert!(subs.contains(&flat::VarName::new(format!("resistor[{}].p.i", i))));
    }
}

#[test]
fn test_find_sub_variables_indexed_prefix_matches_collapsed_connector_array_fields() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        flat::VarName::new("s[1].inductance.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..Default::default()
        },
    );
    flat.add_variable(
        flat::VarName::new("s[1].inductance.n.v"),
        flat::Variable {
            dims: vec![4],
            ..Default::default()
        },
    );

    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("s[1].inductance.n[2]", &pc, &var_index);
    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&flat::VarName::new("s[1].inductance.n.i")));
    assert!(subs.contains(&flat::VarName::new("s[1].inductance.n.v")));
}

#[test]
fn test_find_sub_variables_exact_match_preferred() {
    let mut flat = flat::Model::new();

    // Add both exact and indexed variables
    flat.add_variable(flat::VarName::new("r1.n.v"), flat::Variable::default());
    flat.add_variable(flat::VarName::new("r1.n.i"), flat::Variable::default());
    flat.add_variable(flat::VarName::new("r1[1].n.v"), flat::Variable::default());
    flat.add_variable(flat::VarName::new("r1[1].n.i"), flat::Variable::default());

    // Searching for "r1.n" should find exact matches
    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("r1.n", &pc, &var_index);
    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&flat::VarName::new("r1.n.v")));
    assert!(subs.contains(&flat::VarName::new("r1.n.i")));
}

#[test]
fn test_find_sub_variables_indexed_prefix_does_not_cross_match_connector_members() {
    let mut flat = flat::Model::new();
    // Collapsed connector-array fields commonly appear as indexless members with
    // array dims kept on the primitive variable itself.
    for name in [
        "resistor.p.v",
        "resistor.p.i",
        "resistor.n.v",
        "resistor.n.i",
    ] {
        flat.add_variable(
            flat::VarName::new(name),
            flat::Variable {
                dims: vec![1],
                flow: name.ends_with(".i"),
                ..Default::default()
            },
        );
    }

    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("resistor[1].n", &pc, &var_index);

    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&flat::VarName::new("resistor.n.v")));
    assert!(subs.contains(&flat::VarName::new("resistor.n.i")));
    assert!(!subs.contains(&flat::VarName::new("resistor.p.v")));
    assert!(!subs.contains(&flat::VarName::new("resistor.p.i")));
}
