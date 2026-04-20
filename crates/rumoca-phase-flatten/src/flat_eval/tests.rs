use super::*;
use rumoca_ir_flat as flat;

fn var(name: &str) -> flat::Expression {
    flat::Expression::VarRef {
        name: flat::VarName::new(name),
        subscripts: vec![],
    }
}

fn field(base: flat::Expression, name: &str) -> flat::Expression {
    flat::Expression::FieldAccess {
        base: Box::new(base),
        field: name.to_string(),
    }
}

fn int(value: i64) -> flat::Expression {
    flat::Expression::Literal(flat::Literal::Integer(value))
}

#[test]
fn eval_integer_div_operator_requires_exact_quotient() {
    let expr = flat::Expression::Binary {
        op: flat::OpBinary::Div(flat::Token::default()),
        lhs: Box::new(int(7)),
        rhs: Box::new(int(2)),
    };
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &known_reals,
        known_bools: &known_bools,
        known_enums: &known_enums,
        array_dims: &array_dims,
        functions: &functions,
        var_context: None,
    };
    assert_eq!(try_eval_integer_with_context(&expr, &ctx), None);
}

#[test]
fn eval_integer_div_builtin_remains_truncating() {
    let expr = flat::Expression::BuiltinCall {
        function: flat::BuiltinFunction::Div,
        args: vec![int(7), int(2)],
    };
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &known_reals,
        known_bools: &known_bools,
        known_enums: &known_enums,
        array_dims: &array_dims,
        functions: &functions,
        var_context: None,
    };
    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(3));
}

#[test]
fn eval_boolean_enum_eq_accepts_different_qualification_paths() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.modelStructure".to_string(),
        "Modelica.Fluid.Types.ModelStructure.a_vb".to_string(),
    );

    let expr = flat::Expression::Binary {
        op: flat::OpBinary::Eq(flat::Token::default()),
        lhs: Box::new(var("pipe.modelStructure")),
        rhs: Box::new(var("pipe.Types.ModelStructure.a_vb")),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );

    assert_eq!(value, Some(true));
}

#[test]
fn eval_boolean_enum_eq_accepts_shared_type_literal_tail() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "frameResolve".to_string(),
        "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
    );

    let expr = flat::Expression::Binary {
        op: flat::OpBinary::Eq(flat::Token::default()),
        lhs: Box::new(var("frameResolve")),
        rhs: Box::new(var(
            "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve",
        )),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );
    assert_eq!(value, Some(true));
}

#[test]
fn eval_boolean_enum_eq_rejects_different_enum_type() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "mode".to_string(),
        "Modelica.Blocks.Types.Init.PI".to_string(),
    );

    let expr = flat::Expression::Binary {
        op: flat::OpBinary::Eq(flat::Token::default()),
        lhs: Box::new(var("mode")),
        rhs: Box::new(var("Modelica.Blocks.Types.SimpleController.PI")),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );
    assert_eq!(value, Some(false));
}

#[test]
fn eval_integer_if_uses_canonicalized_enum_condition() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("pipe.n".to_string(), 1);

    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.modelStructure".to_string(),
        "Modelica.Fluid.Types.ModelStructure.a_vb".to_string(),
    );

    let cond = flat::Expression::Binary {
        op: flat::OpBinary::Eq(flat::Token::default()),
        lhs: Box::new(var("pipe.modelStructure")),
        rhs: Box::new(var("pipe.Types.ModelStructure.a_vb")),
    };

    let expr = flat::Expression::If {
        branches: vec![(
            cond,
            flat::Expression::Binary {
                op: flat::OpBinary::Add(flat::Token::default()),
                lhs: Box::new(var("pipe.n")),
                rhs: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
            },
        )],
        else_branch: Box::new(flat::Expression::Literal(flat::Literal::Integer(0))),
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: Some("pipe.nFMDistributed"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(2));
}

#[test]
fn eval_integer_if_resolves_unqualified_enum_condition_with_var_context() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("Bessel.order".to_string(), 3);

    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "Bessel.filterType".to_string(),
        "Modelica.Blocks.Types.FilterType.LowPass".to_string(),
    );

    let cond = flat::Expression::Binary {
        op: flat::OpBinary::Or(flat::Token::default()),
        lhs: Box::new(flat::Expression::Binary {
            op: flat::OpBinary::Eq(flat::Token::default()),
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandPass")),
        }),
        rhs: Box::new(flat::Expression::Binary {
            op: flat::OpBinary::Eq(flat::Token::default()),
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandStop")),
        }),
    };

    let expr = flat::Expression::If {
        branches: vec![(
            cond,
            flat::Expression::Binary {
                op: flat::OpBinary::Mul(flat::Token::default()),
                lhs: Box::new(int(2)),
                rhs: Box::new(var("order")),
            },
        )],
        else_branch: Box::new(var("order")),
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: Some("Bessel.na"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(3));
}

#[test]
fn eval_integer_if_handles_integer_builtin_with_scoped_enum_conditions() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("Bessel.order".to_string(), 3);

    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "Bessel.filterType".to_string(),
        "Modelica.Blocks.Types.FilterType.LowPass".to_string(),
    );
    known_enums.insert(
        "Bessel.analogFilter".to_string(),
        "Modelica.Blocks.Types.AnalogFilter.Bessel".to_string(),
    );

    let filter_is_band = flat::Expression::Binary {
        op: flat::OpBinary::Or(flat::Token::default()),
        lhs: Box::new(flat::Expression::Binary {
            op: flat::OpBinary::Eq(flat::Token::default()),
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandPass")),
        }),
        rhs: Box::new(flat::Expression::Binary {
            op: flat::OpBinary::Eq(flat::Token::default()),
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandStop")),
        }),
    };

    let analog_is_cd = flat::Expression::Binary {
        op: flat::OpBinary::Eq(flat::Token::default()),
        lhs: Box::new(var("analogFilter")),
        rhs: Box::new(var("Modelica.Blocks.Types.AnalogFilter.CriticalDamping")),
    };

    let expr = flat::Expression::If {
        branches: vec![(filter_is_band, var("order")), (analog_is_cd, int(0))],
        else_branch: Box::new(flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Integer,
            args: vec![flat::Expression::Binary {
                op: flat::OpBinary::Div(flat::Token::default()),
                lhs: Box::new(var("order")),
                rhs: Box::new(int(2)),
            }],
        }),
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: Some("Bessel.na"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(1));
}

#[test]
fn extract_enum_value_ignores_dotted_parameter_refs() {
    let extracted = try_extract_enum_value(&var("pipe1.system.energyDynamics"));
    assert_eq!(extracted, None);
}

#[test]
fn extract_enum_value_accepts_scoped_enum_literal_paths() {
    let extracted = try_extract_enum_value(&var("pipe.Types.ModelStructure.a_v_b"));
    assert_eq!(
        extracted,
        Some("pipe.Types.ModelStructure.a_v_b".to_string())
    );
}

#[test]
fn extract_enum_value_ignores_uppercase_name_with_dot_only_inside_subscript() {
    let extracted = try_extract_enum_value(&var("TypeAlias[data.medium]"));
    assert_eq!(extracted, None);
}

#[test]
fn eval_boolean_enum_eq_does_not_guess_dotted_parameter_ref_literal() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.energyDynamics".to_string(),
        "Modelica.Fluid.Types.Dynamics.SteadyStateInitial".to_string(),
    );

    let expr = flat::Expression::Binary {
        op: flat::OpBinary::Eq(flat::Token::default()),
        lhs: Box::new(var("pipe.energyDynamics")),
        rhs: Box::new(var("pipe1.system.energyDynamics")),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );
    assert_eq!(value, None);
}

#[test]
fn eval_integer_field_access_resolves_overqualified_suffix() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("stackData.cellData[1,1].nRC".to_string(), 2);

    let expr = field(var("stack.cell[1,1].cell.stackData.cellData[1,1]"), "nRC");
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: Some("stack.cell[1,1].cell.cellData.nRC"),
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn eval_size_resolves_dotted_field_access_in_active_component_scope() {
    let mut array_dims = FxHashMap::default();
    array_dims.insert("mover.per.motorEfficiency.V_flow".to_string(), vec![3]);

    let expr = flat::Expression::BuiltinCall {
        function: flat::BuiltinFunction::Size,
        args: vec![
            field(field(var("per"), "motorEfficiency"), "V_flow"),
            int(1),
        ],
    };
    let ctx = ParamEvalContext {
        known_ints: &FxHashMap::default(),
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims: &array_dims,
        functions: &FxHashMap::default(),
        var_context: Some("mover.eff"),
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(3));
}

#[test]
fn eval_integer_if_returns_common_value_when_condition_unknown() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("left".to_string(), 2);
    known_ints.insert("right".to_string(), 2);

    let expr = flat::Expression::If {
        branches: vec![(var("cond"), var("left"))],
        else_branch: Box::new(var("right")),
    };
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: None,
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn get_parent_scope_ignores_dot_inside_subscript_expression() {
    assert_eq!(get_parent_scope("arr[data.medium]"), None);
    assert_eq!(get_parent_scope("pkg.arr[data.medium]"), Some("pkg"));
    assert_eq!(
        get_parent_scope("pkg.arr[data.medium].field"),
        Some("pkg.arr[data.medium]")
    );
}

#[test]
fn resolve_by_suffix_stripping_ignores_dot_inside_subscript_expression() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("medium].x".to_string(), 99);
    known_ints.insert("x".to_string(), 1);

    assert_eq!(
        resolve_by_suffix_stripping("pkg.arr[data.medium].x", &known_ints),
        Some(1),
        "only top-level dotted segments should be stripped"
    );
}

#[test]
fn eval_enum_if_resolves_selected_branch_with_known_bool_condition() {
    let mut known_bools = FxHashMap::default();
    known_bools.insert("Medium.singleState".to_string(), true);

    let expr = flat::Expression::If {
        branches: vec![(var("Medium.singleState"), var("Dynamics.SteadyState"))],
        else_branch: Box::new(var("Dynamics.SteadyStateInitial")),
    };

    let value = try_eval_flat_expr_enum(
        &expr,
        &FxHashMap::default(),
        &known_bools,
        &FxHashMap::default(),
    );
    assert_eq!(value, Some("Dynamics.SteadyState".to_string()));
}

#[test]
fn eval_enum_if_returns_common_value_when_condition_unknown() {
    let expr = flat::Expression::If {
        branches: vec![(var("cond"), var("Dynamics.SteadyState"))],
        else_branch: Box::new(var("Dynamics.SteadyState")),
    };

    let value = try_eval_flat_expr_enum(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    );
    assert_eq!(value, Some("Dynamics.SteadyState".to_string()));
}

#[test]
fn infer_array_dims_from_comprehension_range_and_body() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("n".to_string(), 4);

    let expr = flat::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![flat::ComprehensionIndex {
            name: "i".to_string(),
            range: flat::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
            },
        }],
        filter: None,
    };

    let dims = infer_array_dimensions_full_with_conds(
        &expr,
        &known_ints,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    );
    assert_eq!(dims, Some(vec![4]));
}

#[test]
fn infer_array_dims_from_nested_comprehension_body_shape() {
    let expr = flat::Expression::ArrayComprehension {
        expr: Box::new(flat::Expression::Array {
            elements: vec![var("i"), var("i")],
            is_matrix: false,
        }),
        indices: vec![flat::ComprehensionIndex {
            name: "i".to_string(),
            range: flat::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
            },
        }],
        filter: None,
    };

    let dims = infer_array_dimensions(&expr);
    assert_eq!(dims, Some(vec![3, 2]));
}

#[test]
fn infer_array_dims_from_comprehension_returns_none_with_filter() {
    let expr = flat::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![flat::ComprehensionIndex {
            name: "i".to_string(),
            range: flat::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
            },
        }],
        filter: Some(Box::new(flat::Expression::Binary {
            op: flat::OpBinary::Gt(flat::Token::default()),
            lhs: Box::new(var("i")),
            rhs: Box::new(int(1)),
        })),
    };

    let dims = infer_array_dimensions(&expr);
    assert_eq!(dims, None);
}
